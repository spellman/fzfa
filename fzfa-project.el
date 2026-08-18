;;; fzfa-project.el --- Project.el integration for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; project.el integration for fzfa: project-scoped file, directory,
;; buffer, recentf, and project-switch commands.
;;
;; Loaded automatically when `project' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-project-find-file'      Find a file in the current project
;;   `fzfa-project-find-dir'       Open a directory in the current project
;;   `fzfa-project-buffer'         Switch to a buffer of the current project
;;   `fzfa-project-recentf'        Open a recent file from the current project
;;   `fzfa-project-switch-project' Switch to a project root on disk
;;
;; File-search and grep commands shipped in sibling extensions
;; (e.g. `fzfa-rg', `fzfa-fd', `fzfa-find', `fzfa-git-ls-files') already
;; honor the project root via `fzfa-project-backend' / `fzfa--default-dir'.
;; This extension covers operations that are project-aware in a way the
;; shell-driven commands are not: candidate sets derived from
;; `project-files', `project-buffers', and `fzfa-project-roots-dirs'.

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-files "project")
(declare-function project-buffers "project")
(declare-function project-switch-project "project")
(defvar recentf-list)

(defcustom fzfa-project-roots-dirs (list (expand-file-name "~/Projects/"))
  "Directories whose immediate children are project roots.

`fzfa-project-switch-project' lists depth-1 subdirectories of each
entry instead of consulting `project-known-project-roots', so a
project shows up as soon as it exists on disk rather than after it
has been visited once."
  :type '(repeat directory)
  :group 'fzfa)

(defun fzfa-project--current ()
  "Return the current project, or signal a user-error."
  (or (project-current)
      (user-error "Not in a project")))

(defun fzfa-project--label (root)
  "Return a short label for project ROOT for use in a prompt."
  (file-name-nondirectory (directory-file-name root)))

;;;###autoload
(defun fzfa-project-find-file ()
  "Find a file in the current project.

Candidate set comes from `project-files', so membership respects
`project-vc-*' and `project-find-functions'."
  (interactive)
  (require 'project)
  (let* ((pr (fzfa-project--current))
         (root (expand-file-name (project-root pr)))
         (files (project-files pr)))
    (unless files
      (user-error "No files in current project"))
    (when-let* ((sel (fzfa-completing-read
                      :candidates (mapcar
                                   (lambda (f) (file-relative-name f root))
                                   files)
                      :prompt (format "project file [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-file)))
      (fzfa-visit-file (expand-file-name sel root)))))

;;;###autoload
(defun fzfa-project-find-dir ()
  "Open a directory contained in the current project, in Dired.

Candidates are the unique parent directories of `project-files', plus
the project root itself."
  (interactive)
  (let* ((pr (fzfa-project--current))
         (root (expand-file-name (project-root pr)))
         (seen (make-hash-table :test 'equal))
         (dirs nil))
    (dolist (f (project-files pr))
      (let ((d (file-name-directory f)))
        (unless (gethash d seen)
          (puthash d t seen)
          (push (file-relative-name d root) dirs))))
    (unless (member "./" dirs) (push "./" dirs))
    (when-let* ((sel (fzfa-completing-read
                      :candidates (sort dirs #'string<)
                      :prompt (format "project dir [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-file)))
      (dired (expand-file-name sel root)))))

;;;###autoload
(defun fzfa-project-buffer ()
  "Switch to a buffer of the current project."
  (interactive)
  (require 'project)
  (let* ((pr (fzfa-project--current))
         (root (expand-file-name (project-root pr)))
         (names (cl-loop for b in (project-buffers pr)
                         for name = (buffer-name b)
                         unless (or (minibufferp b)
                                    (string-prefix-p " " name))
                         collect name)))
    (unless names
      (user-error "No buffers in current project"))
    (when-let* ((sel (fzfa-completing-read
                      :candidates names
                      :prompt (format "project buffer [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-buffer)))
      (fzfa-with-visit (switch-to-buffer sel)))))

;;;###autoload
(defun fzfa-project-recentf ()
  "Open a recently visited file under the current project.

Filters `recentf-list' to entries whose expanded path is under the
current project's root."
  (interactive)
  (require 'project)
  (require 'recentf)
  (recentf-mode 1)
  (let* ((pr (fzfa-project--current))
         (root (file-name-as-directory (expand-file-name (project-root pr))))
         (files (cl-loop for f in recentf-list
                         for ef = (expand-file-name f)
                         when (string-prefix-p root ef)
                         collect (file-relative-name ef root))))
    (unless files
      (user-error "No recent files under %s" (abbreviate-file-name root)))
    (when-let* ((sel (fzfa-completing-read
                      :candidates files
                      :prompt (format "project recentf [%s]: "
                                      (fzfa-project--label root))
                      :category 'fzfa-file)))
      (fzfa-visit-file (expand-file-name sel root)))))

(defun fzfa-project--discover-roots ()
  "Return depth-1 subdirectories of each dir in `fzfa-project-roots-dirs'."
  (cl-loop for parent in fzfa-project-roots-dirs
           when (file-directory-p parent)
           nconc (cl-loop for entry in (directory-files parent t)
                          when (and (file-directory-p entry)
                                    (not (member (file-name-nondirectory entry)
                                                 '("." ".."))))
                          collect (file-name-as-directory entry))))

;;;###autoload
(defun fzfa-project-switch-project ()
  "Switch to a project root via fzf.

Candidates are the immediate subdirectories of each directory in
`fzfa-project-roots-dirs' — every project on disk, not only the ones
`project-known-project-roots' has recorded.  After selection,
dispatches through `project-switch-project' so the user's
`project-switch-commands' menu kicks in."
  (interactive)
  (require 'project)
  (let ((roots (fzfa-project--discover-roots)))
    (unless roots
      (user-error "No projects found under %s"
                  (mapconcat #'abbreviate-file-name
                             fzfa-project-roots-dirs ", ")))
    (when-let* ((sel (fzfa-completing-read
                      :candidates (mapcar #'abbreviate-file-name roots)
                      :prompt "switch project: "
                      :category 'fzfa-file)))
      (project-switch-project (expand-file-name sel)))))

(provide 'fzfa-project)
;;; fzfa-project.el ends here
