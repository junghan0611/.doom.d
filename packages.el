;; -*- no-byte-compile: t; -*-

;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;;; doom-disabled-packages

;; (disable-packages! flyspell
;;                    ccls                 ; I use clangd.
;;                    compat               ; I use the latest version of Emacs.
;;                    ace-window
;;                    volatile-highlights
;;                    hl-line
;;                    elfeed-goodies
;;                    mu4e-alert)

(unpin! consult) ; (void-function consult--process-collection)

(package! diredfl :disable t) ; conflict with denote
(package! dirvish :disable t)
;; (package! paredit :disable t) ; clojure module
;; (package! code-review :disable t) ; not working
(package! nose :disable t) ; python module
(package! lsp-python-ms :disable t)
(package! flyspell-lazy :disable t)
(package! flymake-popon :disable t)

(package! undo-fu-session :disable t)
(package! elfeed-goodies :disable t)
(package! org-superstar :disable t)
(package! org-fancy-priorities :disable t)
(package! solaire-mode :disable t)
(package! ace-window :disable t)

(package! lsp-mode :disable t)
(package! flycheck-popup-tip :disable t) ; conflict
;; (package! flycheck-plantuml :disable t)

(package! emojify :disable t) ; from mastodon

;; disable nerd-icons for performance issues
(package! treemacs-nerd-icons :disable t)

;; (package! corfu-popupinfo :disable t)

(package! evil-snipe :disable t)
;; (package! evil-mc :disable t)

;; Disable tty module
(package! evil-terminal-cursor-changer :disable t) ; conflict on kitty
(package! kkp :disable t) ; conflict on term-keys

;;; DONT org-mode for latex-preview-auto-mode

;; cd doom-emacs-dir
;; rm -Rf eln-cache
;; cd .local/straight
;; rm -Rf build-30.0.93/org or build-29.4.50/org
;; rm -Rf repos/org
;; doom sync -u

;; (package! org :recipe
;;   (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
;;    (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
;;    :files
;;    (:defaults "etc")
;;    :build t :pre-build
;;    (with-temp-file "org-version.el"
;;      (require 'lisp-mnt)
;;      (let
;;          ((version
;;            (with-temp-buffer
;;              (insert-file-contents "lisp/org.el")
;;              (lm-header "version")))
;;           (git-version
;;            (string-trim
;;             (with-temp-buffer
;;               (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
;;               (buffer-string)))))
;;        (insert
;;         (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
;;         (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
;;         "(provide 'org-version)\n"))))
;;   :pin nil)
;; (unpin! org)

;;; additional packages

;;;; :os tty

(package! term-keys :recipe (:host github :repo "junghan0611/term-keys"))

;;;; :ui - visual

(package! transpose-frame)
(package! hydra)
(package! pretty-hydra)
(package! major-mode-hydra)

(package! ef-themes)
(package! modus-themes)
(package! show-font)
;; (package! hammy)

(package! info+)
(package! list-unicode-display)
(package! spacious-padding)

(unpin! doom-themes)
;; https://github.com/junghan0611/doom-themes/commit/9baa0ae4cef9a2b46eea0101964ac0d04c1c31c1
(package! doom-themes :recipe (:host github :repo "junghan0611/doom-themes" :branch "ko"))

(package! keycast)
(package! outli :recipe (:host github :repo "jdtsmith/outli" :files ("*.el")))
(package! fontaine)
(package! golden-ratio)
(package! mode-minder :recipe (:host github :repo "jdtsmith/mode-minder"))
;; (package! breadcrumb)
(package! celestial-mode-line)
(package! lin)
(package! nerd-icons-dired)
(package! nerd-icons-completion) ; vertico +icons

(package! dired-preview)

(package! dired+)
;; (package! bookmark+)

;;;; :editor

(package! copy-as-format)
(package! expand-region)
;; (package! evil-matchit)
(package! evil-owl) ;; register
(package! tempel)
(package! tempel-collection)
(package! imenu-list)
;; (package! titlecase)
(package! deadgrep)
(package! rg) ; ripgrep
(package! affe)
(package! fzf)
(package! ace-link)
(package! unfill)
(package! translate-mode)
(package! separedit :recipe (:host github :repo "twlz0ne/separedit.el"))


(package! consult-omni :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
(package! goto-last-change)

;;;; :lang org-mode

(package! org-fragtog)          ;; interactive toggling of inline latex formulas
(package! org-appear)
(package! orgabilize :recipe (:host github :repo "akirak/orgabilize.el"))
(package! org-glossary :recipe (:host github :repo "tecosaur/org-glossary" :files ("*.el" "*.org" "*.texi")))
(package! org-pandoc-import :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "*.org" "filters" "preprocessors")))
(package! org-web-tools)
(package! org-index) ;; 색인 지원
(package! corg :recipe (:host github :repo "isamert/corg.el"))

(package! org-download)
(package! ox-epub)

(package! org-rainbow-tags)
(package! org-drill)

(package! org-rich-yank)
(package! ox-reveal)
(package! org-transclusion)
(package! org-remark)
(package! org-bookmarks)

(package! ox-leanpub) ;; https://github.com/junghan0611/ox-leanpub

(package! mermaid-mode)
(package! ob-mermaid)

(package! org-ql)
(package! org-kanban)

(package! org-modern)
(package! org-modern-indent :recipe (:host github :repo "jdtsmith/org-modern-indent"))

;; (package! parse-csv :recipe (:host github :repo "junghan0611/el-csv")) ; for om-dash
;; (package! om-dash :recipe (:host github :repo "gavv/om-dash" :files ("*.el" "*.org"))) ; org-based dashboards

;; (package! org-bookmark-heading)

;; (package! d2-mode)
;; (package! ob-d2 :recipe (:host github :repo "dmacvicar/ob-d2"))

;;;; :tools

;;;;; :tools biblio

(package! citar)
(package! citar-embark)
(package! parsebib)
(package! citeproc)

;;;; :tools writing

(package! hypothesis :recipe (:host github :repo "EFLS/hypothesis"))
(package! side-notes)
(package! redacted)
(package! centered-cursor-mode)

(package! google-translate)
(package! jinx)
(package! logos)
(package! olivetti)
(package! palimpsest)
(package! immersive-translate)
(package! focus)

;; (package! quarto-mode :recipe (:host github :repo "quarto-dev/quarto-emacs" )) ; require polymode
(package! quarto-mode :pin "a7b974f7d22ef939eaed8b9919434bcf20b1438f")
(package! ox-quarto :recipe (:host github :repo "jrgant/ox-quarto"))

(package! math-preview)

(package! guess-language :recipe (:host github :repo "junghan0611/guess-language.el" :branch "master" :files ("*.el" "trigrams/*")))
(package! txl :recipe (:host github :repo "junghan0611/txl.el" :branch "ko"))
;; (package! flymake-vale :recipe (:host github :repo "tpeacock19/flymake-vale"))

;;;; :pkm

;;;;; :pkm denote

(package! denote)

(package! denote-explore)
(package! consult-notes)

(package! consult-denote)

(package! citar-denote)
(package! tmr) ; timer

;; (package! ekg)

(package! binder)
(package! ten :recipe (:host sourcehut :repo "nobiot/ten")) ;; https://git.sr.ht/~nobiot/ten
;; (package! obsidian)

;; (package! org-fc
;;   :recipe (:host github
;;            ;; :repo "l3kn/org-fc"
;;            :repo "cashpw/org-fc"
;;            :branch "feat/classes"
;;            :files (:defaults "awk" "demo.org")))

;;;; AI

(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el" "aider-doom.el")))

;; git@github.com:ziova/wolfram.el.git
;; (package! wolfram :recipe (:host github :repo "ziova/wolfram.el"))

(package! llm)
;; (package! kagi)
;; (package! org-ai)

(package! chatgpt-shell)
(package! ob-chatgpt-shell)
(package! pcsv)

(package! gptel)
;; (package! gptel :recipe (:host github :repo "karthink/gptel" :branch "master"))
;; (package! gptel :recipe (:host github :repo "karthink/gptel" :branch "feature-tool-use"))
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

(package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))) ; github copilot
(package! copilot-chat :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el"))) ; github copilot

;; gptel plugins
;; (package! yap :recipe (:host github :repo "meain/yap"))
;; (package! elysium :recipe (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
;; (package! evedel :recipe (:host github :repo "daedsidog/evedel"))

(package! whisper :recipe (:host github :repo "natrys/whisper.el"))

(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

;; (package! ellama)
;; (package! openai :recipe (:host github :repo "emacs-openai/openai" :files ("*.el")))
;; (package! dall-e :recipe (:host github :repo "emacs-openai/dall-e"))
;; (package! elisa :recipe (:host github :repo "s-kostyaev/elisa" :files ("*.el" "*.org")))

;; ~/sync/man/dotsamples/doom/lemon-dot-doom/modules/cae/ai/config.el
;; (package! greader)

;;;; :lang

;;;;; TODO latex snippets

;; (package! aas)
(package! laas)
(package! math-symbol-lists)

;;;; Coding

(package! auto-highlight-symbol)
(package! symbol-overlay)

(when (modulep! :lang clojure)
  ;; (package! kaocha-runner) ; Koacha test runner in Emacs
  (package! vega-view)
  (package! clj-deps-new)
  (package! clay))
  ;; (package! clojure-essential-ref-nov)


(unpin! elisp-demos)
(package! elisp-demos :recipe (:host github :repo "junghan0611/elisp-demos" :branch "ko"))

(package! exercism)
(package! bats-mode)

;; Use the latest available packages for Clojure
;; - cider, clojure-mode
;; (unpin! (:lang clojure))
;; (unpin! conda)
;; (package! conda)

(package! devdocs-browser)
(package! aggressive-indent)

;; (package! bats-mode) ; shell-scripts
;; (package! code-cells)

;; (package! hy-mode :recipe (:host github :repo "jethack23/hy-mode"))
;; (package! geiser)
;; (package! geiser-mit :recipe (:host github :repo "emacsmirror/geiser-mit"))

;;;; Git

(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :files ("*.el")))
(package! git-link :recipe (:host github :repo "sshaw/git-link"))

(package! git-cliff)
(package! gist)
(package! consult-git-log-grep)
(package! magit-todos)

;;;; Reading

(package! eww :built-in t)
(package! tp)
;; (package! mastodon)
(package! adoc-mode)

(package! yeetube :recipe (:host github :repo "Boruch-Baum/emacs-yeetube.el"))

(package! elfeed-tube-mpv)
;; (package! elfeed-webkit) ; not working on ubuntu

;; (package! browser-hist :recipe (:host github :repo "agzam/browser-hist.el"))

(package! subed :recipe (:host github :repo "sachac/subed" :files ("subed/*.el")))
;; (package! dwim-shell-command)
;; (package! bm) ; visible bookmark

;; filter marked text out
(package! org-marked-text-overview :recipe (:host github :repo "lijigang/org-marked-text-overview"))
;; (package! org-zettel-ref-mode :recipe (:host github :repo "junghan0611/org-zettel-ref-mode" :branch "ko"))

(package! org-supertag :recipe (:host github :repo "yibie/org-supertag"))
(package! docsim)

;;;; Workspace


;;;; Misc

(package! atomic-chrome)
(package! empv) ;; TODO mpv frontend
(package! djvu)
(package! calibredb)
(package! nov)
(package! osm) ; OpenStreetMaps
(package! gif-screencast)
(package! lorem-ipsum)
;;  (package! go-translate :recipe (:host github :repo "lorniu/go-translate" :files ("*.el" "*.org" )))
(package! go-translate)
(package! youtube-sub-extractor)

;; (package! ready-player)

;; Very large files mode loads large files in chunks to open ridiculously large files.
;; (package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
;;   :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

;; (package! zoxide)
;; (package! telega) ; telegram
;;;; Transient

(package! ccmenu :recipe (:host github :repo "junghan0611/ccmenu"))
;; (package! casual-suite)
(package! casual-suite :recipe (:host github :repo "kickingvegas/casual-suite"))
(package! recent-rgrep :recipe (:host github :repo "kickingvegas/recent-rgrep"))

(package! p-search :recipe (:host github :repo "zkry/p-search"))

(package! git-grep-transient)

;;;; Forked PKGs

(unless IS-TERMUX
  ;; This is an emacs package which supports OWL Manchester Notation https://www.w3.org/TR/owl2-manchester-syntax/
  ;; (package! omn-mode)
  ;; (package! elot
  ;;   :recipe (:host github :repo "junghan0611/elot"  :branch "ko" :files("elot-package/*")))
  ;; "johanwk/elot"
  (package! elot :recipe (:local-repo "local/elot" :branch "ko" :files("elot-package/*")))

  (package! pylookup :recipe (:local-repo "local/pylookup")))

  ;; (package! paw :recipe (:local-repo "local/paw" :branch "ko" :files ("*")))
  ;; (package! paw :recipe (:host github :repo "junghan0611/paw" :branch "ko" :files ("*")))

  ;; (package! trekker
  ;;   :recipe (:host github :repo "junghan0611/trekker" :branch "ko" :files("*.md" "*.el" "*.py")))


;;; Applications
;;;; Calculator

(package! literate-calc-mode)
(package! calctex :recipe (:host github
                           :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

;;;; TODO waiting

(package! default-text-scale :recipe (:host github :repo "purcell/default-text-scale"))
(package! moc :recipe (:host github :repo "positron-solutions/moc"))
(package! dslide :recipe (:host github :repo "positron-solutions/dslide"))

(package! anddo :recipe (:host github :repo "junghan0611/anddo.el"))

;; A method for blocking access to emacs commands based on time.
;; https://git.sr.ht/~swflint/time-block-command
;; randomly ask yourself a question to collect productivity data. The primary
;; entry point is the macro define-asker. It may be called as follows
;; https://git.sr.ht/~swflint/random-ask

;;;; choi

;; (package! google-this)
(package! webpaste)
;; (package! password-store-menu)
;; (package! fireplace)
;; (package! snow)

(package! image-slicing :recipe (:host github :repo "ginqi7/image-slicing"))
(package! org-sliced-images)
(package! org-linenote)

;; (package! consult-projectile) ; ?!

;;; end-of file
