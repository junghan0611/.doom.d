;;; lisp/eww-load.el -*- lexical-binding: t; -*-

;; a bunch of these things were borrowed from: Protesilaos Stavrou and remodeled for my
;; own needs https://protesilaos.com/codelog/2021-03-25-emacs-eww

;;;###autoload
(defun +process-external-url (url)
  "To be called when an external process sends a URL to Emacs."
  (pcase url
    ((pred (string-match-p "https\\:\\/\\/www.youtube.com\\/watch"))
     (youtube-sub-extractor-extract-subs url))
    ((pred (and (string-match-p "https\\:\\/\\/github.com.*" url)
                (modulep! :custom git)))
     (forge-visit-topic-via-url url))
    (_
     (+eww-open-in-other-window url))))

;;;###autoload
(defun +default-browse-url (url &rest args)
  "Always use default (external) browser"
  (interactive (browse-url-interactive-arg "URL: "))
  ;; eww resets browse-url-function, I don't want that
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (funcall-interactively #'browse-url url args)))

;;;###autoload
(defun +eww-browse-url (url &rest args)
  "Always use eww browser"
  (interactive (browse-url-interactive-arg "URL: "))
  ;; eww resets browse-url-function, I don't want that
  (funcall-interactively #'eww-browse-url url args))

(defmacro eww-act-visible-window (&rest body)
  "Run BODY within narrowed-region.
If region is active run BODY within active region instead.
Return the value of the last form of BODY."
  `(save-restriction
     (if (use-region-p)
         (narrow-to-region (region-beginning) (region-end))
       (narrow-to-region (window-start) (window-end)))
     ,@body))

(defun +eww/open-in-other-window2 (url &rest args)
  "Use `eww-open-in-new-buffer' in another window."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; (interactive (list (car (eww-suggested-uris))))
  (other-window-prefix)  ; For emacs28 -- it's a hack, but why not?
  (eww url :new-buffer))

(require 'transient)
;;;###autoload
(transient-define-prefix eww-zoom-transient ()
  "EWW"
  ["Fonts"
   [("j" "decrease" +eww/decrease-font-size :transient t)
    ("k" "increase" +eww/increase-font-size :transient t)]])

;;; provide

(provide 'eww-load)
