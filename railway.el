;;; railway.el - a minor mode for rails projects to hang things off of

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: rails
;; Version: 1

;;; Commentary:

;; Simple minor mode/map/and hook that will alway be there in
;; rails projects, but will go away for anything else

;;; Code

;; stolen from rinari
(defun railway-root (&optional dir home)
  (or dir (setq dir default-directory))
  (if (file-exists-p (expand-file-name
                      "environment.rb" (expand-file-name "config" dir)))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" dir)
        (railway-root new-dir)))))

(defvar railway-minor-mode-map (make-sparse-keymap))
(defvar railway-minor-mode-hook nil)
(defvar railway-project-changed-hook nil)
(defvar railway-current-project "")

(define-minor-mode railway-minor-mode
  "Railway Mode"
  :lighter " rails"
  :keymap railway-minor-mode-map
  :group 'railway)
  

(defun railway-maybe-launch ()
  (interactive)
  (let ((root (railway-root)))
    (if root
        (progn
          (when (not (string= railway-current-project root))
            (run-hooks 'railway-project-changed-hook)
            (setq railway-current-project root))
          (railway-minor-mode t)
          (run-hooks 'railway-minor-mode-hook))
      (if railway-minor-mode (railway-minor-mode)))))

(defadvice cd (after railway-cd activate)
  (railway-maybe-launch))

(add-hook 'after-change-major-mode-hook 'railway-maybe-launch)


(defun railway-create-migration ()
  (interactive)
  (let* ((migration-path (concat (railway-root) "/db/migrate/"))
         (name (read-from-minibuffer "Migration name: "))
         (class (replace-regexp-in-string "_" "" (capitalize name)))
         (timestamp (format-time-string "%Y%m%d%H%S" (current-time)))
         (file-name (concat timestamp "_" name ".rb")))

    (find-file (concat migration-path file-name))
    (insert (concat "class " class))
    (newline)

    (insert "def change")
    (indent-for-tab-command)
    (newline)

    (insert "end")
    (indent-for-tab-command)
    (newline)

    (insert "end")
    (indent-for-tab-command)))


(provide 'railway)
