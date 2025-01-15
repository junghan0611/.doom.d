;;; autoload/dired.el -*- lexical-binding: t; -*-

;;;###autoload
(defun buffer-with-dired-item ()
  "Creates buffer with current item of dired or direx buffer"
  (eq major-mode 'dired-mode) (find-file-noselect (dired-get-file-for-visit)))

;;;###autoload
(defmacro dired-split-action (split-type)
  `(defun ,(intern (concat "dired--" (symbol-name split-type))) ()
     (interactive)
     (let ((buf (buffer-with-dired-item)))
       (funcall #',split-type)
       (switch-to-buffer buf))))

;;;###autoload
(defun dired-ace-action ()
  (interactive)
  (with-demoted-errors "%s"
    (require 'ace-window)
    (let ((buf (buffer-with-dired-item))
          (aw-dispatch-always t))
      (aw-switch-to-window (aw-select nil))
      (switch-to-buffer buf))))

;;; 2024-07-16 추가

;; ("C-c C-n" . my/dired-find-file-ace)
;; ("C-c C-l" . my/dired-limit-regexp)
;; ("M-j" . my/dired-file-jump-from-here)
;; ("M-u" . dired-up-directory)
;; ("C-c C-q" . my/dired-kill-all-buffers)

;;;###autoload
(defun my/dired-find-file-ace ()
  (interactive)
  (let ((find-file-run-dired t)
        (fname (dired-get-file-for-visit)))
    (if (ace-select-window)
        (find-file fname))))

;;;###autoload
(defun my/dired-kill-all-buffers ()
  (interactive)
  (mapc (lambda (buf)
          (when (eq 'dired-mode
                    (buffer-local-value 'major-mode buf))
            (kill-buffer buf)))
        (buffer-list)))
