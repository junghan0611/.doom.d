;;; autoload/utils.el -*- lexical-binding: t; -*-

;;; open-file pop-to-buffer

;;;###autoload
(defun cashpw/open-file (file-path)
  "Open file at FILE-PATH in another window."
  (let ((buffer (find-file-other-window file-path)))
    (with-current-buffer buffer
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;;; window-dedication

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/

;;;###autoload
(defun spacemacs/toggle-current-window-dedication ()
  "Toggle dedication state of a window. Commands that change the buffer that a
window is displaying will not typically change the buffer displayed by
a dedicated window."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated
                 "no longer "
               "")
             (buffer-name))))

;;; toggle-maximize-buffer

;;;###autoload
(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;;; window-layout-toggle

;;;###autoload
(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;;; spacemacs/delete-window

; SPC w d
;;;###autoload
(defun spacemacs/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))
