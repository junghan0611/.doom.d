;;; lisp/my-themes.el -*- lexical-binding: t; -*-

;;; theme framework

;;;; my/doom-p modus-p ef-p

(defun remove-items (x y)
  (setq y (cl-remove-if (lambda (item) (memq item x)) y))
  y)

(defun my/doom-p ()
  (seq-find
   (lambda (x) (string-match-p (rx bos "doom") (symbol-name x)))
   custom-enabled-themes))

;; (defun my/doom-homage-white-p ()
;;   (seq-find
;;    (lambda (x)
;;      (string-match-p (rx bos "doom-homage-white") (symbol-name x)))
;;    custom-enabled-themes))

(defun my/modus-p ()
  (seq-find
   (lambda (x) (string-match-p (rx bos "modus") (symbol-name x)))
   custom-enabled-themes))

(defun my/ef-p ()
  (seq-find
   (lambda (x) (string-match-p (rx bos "ef") (symbol-name x)))
   custom-enabled-themes))

;;;; config per doom-themes

(progn
  (setq doom-one-comment-bg nil)
  (setq doom-one-brighter-comments t)
  (setq doom-opera-light-brighter-comments t)
  (setq doom-zenburn-brighter-comments t)
  (setq doom-acario-light-brighter-comments t)
  (setq doom-dracula-brighter-comments t))

;;;; my/load-custom-set-faces

;;;###autoload
(defun my/load-custom-set-faces ()
  (interactive)

  (when (my/doom-p)
    (custom-set-faces
     ;; `(ekg-notes-mode-title ((t :inherit outline-1 :weight bold :height 1.0)))
     ;; `(ekg-title ((t :inherit outline-2 :weight semibold :height 1.0 :underline t)))
     ;; `(ekg-tag ((t :background ,(doom-blend 'yellow 'bg 0.1) :box (:line-width 1 :color ,(doom-color 'bg-alt)) :foreground ,(doom-color 'fg) :style nil))) ; prose-tag
     ;; `(ekg-resource ((t :inherit outline-7 :weight regular :height 1.0 :underline t)))
     ;; `(ekg-metadata ((t :inherit outline-1 :weight regular :height 1.0)))

     ;; `(corfu-default ((t (:inherit tooltip))))
     ;; `(corfu-current ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'fg)))))

     `(tab-bar ((t (:background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg) :weight semibold))))
     `(tab-bar-tab ((t (:inherit tab-bar-tab :background ,(doom-color 'bg) :box (:color ,(doom-color 'bg-alt)) :foreground ,(doom-color 'yellow) :underline ,(doom-color 'yellow) :weight bold))))
     `(tab-bar-tab-inactive ((t (:background ,(doom-color 'bg) :box (:color ,(doom-color 'bg-alt)) :foreground ,(doom-color 'fg) :weight semibold))))
     `(tab-bar-tab-group-active ((t (:inherit tab-bar-tab))))
     `(tab-bar-tab-ungrouped ((t (:inherit tab-bar-tab-inactive))))
     `(tab-bar-tab-group-inactive ((t (:inherit tab-bar-tab-inactive))))

     ;; `(tab-bar-tab-inactive ((t (:inherit tab-bar-tab-inactive :background ,(doom-color 'bg) :box (:color ,(doom-color 'bg-alt)) :foreground ,(doom-color 'fg) :weight semibold))))
; :box (:line-width 1 :color ,(doom-color 'bg) :style nil)

     ;; `(outline-1 ((t (:background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg)))))
     ;; `(outline-2 ((t (:background ,(doom-color 'yellow-alt) :foreground ,(doom-color 'fg)))))
     ;; `(outline-3 ((t (:background ,(doom-blend 'blue 'bg 0.1) :foreground ,(doom-color 'fg)))))
     ;; `(outline-4 ((t (:background ,(doom-blend 'green 'bg 0.1) :foreground ,(doom-color 'fg)))))

     `(org-mode-line-clock ((t :inherit doom-modeline-info :weight semibold)))
     `(org-mode-line-clock-overrun ((t :inherit doom-modeline-urgent :weight semibold)))
     `(jinx-misspelled ((t :underline (:style wave :color ,(doom-color 'magenta)))))
     ;; `(ten-id-face ((t :inherit font-lock-keyword-face :underline (:style double-line :color ,(doom-color 'cyan)) :weight semibold)))

     ;; `(fringe ((t (:background ,(doom-darken 'bg 0.05)))))
     ;; `(org-agenda-diary ((t (:inherit org-agenda-calendar-sexp :foreground ,(doom-color 'fg) :weight semibold))))

     `(consult-separator ((t (:inherit default :foreground ,(doom-color 'yellow)))))
     `(consult-notes-time ((t (:inherit default :foreground ,(doom-color 'cyan)))))
     ;; `(consult-notes-name ((t (:inherit default :foreground ,(doom-color 'fg)))))

     `(org-tag ((t (:foreground ,(doom-color 'fg) :background ,(doom-blend 'yellow 'bg 0.1) :box (:line-width 1 :color ,(doom-color 'base5))))))

     `(diredp-file-name ((t (:foregorund ,(doom-color 'base8) ))))
     )

    (when (locate-library "spacious-padding")
      (spacious-padding-mode +1))
    ) ; end-of doom-themes

  ;; useless
  ;; (when (my/modus-p) (message "load-custom-set-faces->modus-themes") (my/modus-themes-custom-faces))
  ;; (when (my/ef-p) (message "load-custom-set-faces->ef-themes") (my/ef-themes-hl-todo-faces))
  ) ; end-of defun

;;;; load-theme-randomly

(defun load-theme-randomly (appearance)
  (let* ((day
          (random 20)
          ;; (string-to-number (format-time-string "%j" (current-time)))
          )
         (themes
          (pcase appearance
            ('light my/light-themes)
            ('dark my/dark-themes)))
         (themes-length (length themes))
         ;; (theme (nth (% (* day 7) themes-length) themes)))
         (theme (nth (% day themes-length) themes)))
    (message "Loading theme: %s" (format "%s" theme))
    (load-theme theme t)))

(defun my/doom-themes-random-light ()
  (interactive)
  (load-theme-randomly 'light))

(defun my/doom-themes-random-dark ()
  (interactive)
  (load-theme-randomly 'dark))

(setq my/themes-blacklisted
      '(leuven
        leuven-dark
        manoj-dark
        tango
        wombat
        adwaita
        tsdh-dark
        dichromacy
        light-blue
        misterioso
        tango-dark
        tsdh-light
        wheatgrass
        whiteboard
        deeper-blue

        ;; doom-one
        ;; doom-one-light
        ;; doom-homage-white
        ;; doom-homage-black
        ;; doom-gruvbox
        ;; doom-gruvbox-light

        modus-operandi
        modus-operandi-tinted
        modus-operandi-tritanopia
        modus-operandi-deuteranopia

        ;; ef-light
        ;; ef-eagle
        ;; ef-duo-light
        ;; ef-trio-light
        ;; ef-melissa-light
        ;; ef-elea-light
        ;; ef-frost
        ;; ef-maris-light
        ;; ef-day
        ;; ef-kassio
        ;; ef-spring
        ;; ef-summer
        ;; ef-arbutus
        ;; ef-cyprus
        ;; ef-tritanopia-light
        ;; ef-deuteranopia-light

        modus-vivendi-tinted
        modus-vivendi
        modus-vivendi-deuteranopia
        modus-vivendi-tritanopia

        ;; ef-owl
        ;; ef-dark
        ;; ef-bio
        ;; ef-rosa
        ;; ef-night
        ;; ef-autumn
        ;; ef-cherie
        ;; ef-winter
        ;; ef-duo-dark
        ;; ef-elea-dark
        ;; ef-symbiosis
        ;; ef-trio-dark
        ;; ef-maris-dark
        ;; ef-melissa-dark
        ;; ef-deuteranopia-dark
        ;; ef-tritanopia-dark
        ))

(setq consult-themes
      (remove-items my/themes-blacklisted (custom-available-themes)))

;;;; doom my/light-themes

(defvar my/light-themes
  '(
    doom-homage-white
    doom-flatwhite
    ;; doom-acario-light
    ;; doom-opera-light
    ;; doom-tomorrow-day
    ;; doom-gruvbox-light
    ))

(defvar my/dark-themes
  '(
    ;; doom-one ;; Grayish theme with great colors.
    doom-dracula
    doom-zenburn
    ;; doom-gruvbox
    ))

;;; provide

(provide 'my-themes)
