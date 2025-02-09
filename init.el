;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

;;; Pre-init
;;;; Variables

;; This is so that I don't accidentally start Emacs as a daemon.
(when (daemonp) (kill-emacs))

;;;; Path

(setq user-dotemacs-dir doom-user-dir)
(setq emacs-type 'doomemacs)

;; delete insert keymap
(setq evil-org-key-theme '(navigation textobjects additional calendar todo))

(defun my/org-emacs-config-file () (expand-file-name "README.org" user-dotemacs-dir))

;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; (add-to-list 'load-path (concat dotspacemacs-directory "lisp"))
(dolist (dir '( "lisp" "site-lisp")) ;; "ccmenu"
  (push (expand-file-name dir user-dotemacs-dir) load-path))

;;;; Termux

(setq-default root-path "/")

(defvar IS-TERMUX
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(when IS-TERMUX
  (setq root-path "/data/data/com.termux/files/"))

(setq my/slow-ssh
      (or
       (string= (getenv "IS_TRAMP") "true")))

(setq my/remote-server
      (or (string= (getenv "IS_REMOTE") "true")
          ;; (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
          (string= (system-name) "server1")
          (string= (system-name) "server2")
          (string= (system-name) "server3"))) ; for test

(setenv "IS_EMACS" "true")

;;;; PGTK

;; You should be able to use input methods since GtkIMContext is enabled by
;; default. If you don't like GtkIMContext, you can disable it by writing as
;; follows in ~/.emacs: pgtk-use-im-context disable gtk im modules for
;; emacs-pgtk, add "Emacs*UseXIM: false" to ~/.Xresources to disable xim
(if (eq window-system 'pgtk)
    (pgtk-use-im-context nil))

(when (boundp 'pgtk-use-im-context-on-new-connection)
  (setq pgtk-use-im-context-on-new-connection nil))

;;; Doom Modules

;; :input
(doom!
 :completion
 (corfu +orderless +icons +dabbrev)
 ;; (vertico +icons)
 vertico ;; search engine of the future

 :ui
 doom              ; what makes DOOM look the way it does
 doom-dashboard    ; a nifty splash screen for Emacs
 ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
 hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW/XXX/BUG
 ;; indent-guides ; highlighted indent columns
 ;; (ligatures +extra)         ; ligatures and symbols to make your code pretty again
 modeline ; snazzy, Atom-inspired modeline, plus API
 neotree ; a project drawer, like NERDTree for vim
 ophints           ; highlight the region an operation acts on
 popup    ; tame sudden yet inevitable temporary windows

 treemacs;  a project drawer, like neotree but cooler
 vc-gutter ;; +pretty
 (window-select +numbers) ; visually switch windows
 workspaces        ; tab emulation, persistence & separate workspaces

 :editor
 (evil +everywhere); come to the dark side, we have cookies
 file-templates    ; auto-snippets for empty files
 fold              ; (nigh) universal code folding
 format            ; automated prettiness
 multiple-cursors  ; editing in many places at once
 rotate-text       ; cycle region at point between text candidates
 snippets          ; my elves. They type so I don't have to
 ;;word-wrap         ; soft wrapping with language-aware indent

 :emacs
 electric          ; smarter, keyword-based electric-indent
 dired             ; making dired pretty [functional]
 (ibuffer +icons)  ; interactive buffer management

 undo              ; persistent, smarter undo for your inevitable mistakes
 vc                ; version-control and Emacs, sitting in a tree
 eww               ; Emacs' built-in web browser

 :term
 (:unless IS-TERMUX (eshell)) ; the elisp shell that works everywhere
 (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm) ; the best terminal emulation in Emacs

 :checkers
 (syntax -flymake) ;;  tasing you for every semicolon you forget

 (:unless IS-TERMUX (spell +flyspell)) ; +hunspell - tasing you for misspelling mispelling
 ;; grammar           ; tasing grammar mistake every you make

 :tools
 ;; ansible
 biblio ;; Writes a PhD for you (citation needed)
 debugger ; FIXME stepping through code, to help you add bugs
 direnv

 ;; (docker +lsp)
 ;; editorconfig      ; let someone else argue about tabs vs spaces

 (eval +overlay)     ; run code, run (also, repls)
 lookup ; only dumb-jump
 (lsp +eglot)

 (magit +forge) ; a git porcelain for Emacs

 make              ; run make tasks from Emacs
 ;;pass              ; password manager for nerds
 (:unless IS-TERMUX (pdf)) ; pdf enhancements
 ;; (:unless IS-TERMUX (prodigy)) ;; FIXME managing external services & code builders
 ;;terraform         ; infrastructure as code
 ;;tmux              ; an API for interacting with tmux
 ;; tree-sitter ;; syntax and parsing, sitting in a tree...
 upload            ; map local to remote projects via ssh/ftp

 :os
 (:if IS-MAC macos)  ; improve compatibility with macOS
 tty                 ; improve the terminal Emacs experience

 :lang
 ;;agda              ; types of types of types of types...
 beancount         ; mind the GAAP
 ;;(cc +lsp)         ; C > C++ == 1
 (clojure +lsp) ; java with a lisp
 common-lisp ; if you've seen one lisp, you've seen them all
 ;; (:unless IS-TERMUX (coq)) ; proofs-as-programs
 ;;crystal           ; ruby at the speed of c
 ;;csharp            ; unity, .NET, and mono shenanigans
 data              ; config/data formats
 ;;(dart +flutter)   ; paint ui and not much else
 ;;dhall
 ;; (:unless IS-TERMUX (elixir +lsp)) ; +tree-sitter ; erlang done right
 ;;elm               ; care for a cup of TEA?
 emacs-lisp        ; drown in parentheses
 ;;erlang            ; an elegant language for a more civilized age

 ess
 ;; (:unless IS-TERMUX (ess +tree-sitter +lsp)) ; speaks statistics
 ;;factor
 ;;faust             ; dsp, but you get to keep your soul
 ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
 ;;fsharp            ; ML stands for Microsoft's Language
 ;;fstar             ; (dependent) types and (monadic) effects and Z3
 ;;gdscript          ; the language you waited for
 ;;(go +lsp)         ; the hipster dialect
 ;;(graphql +lsp)    ; Give queries a REST

 ;; (haskell +lsp) ; a language that's lazier than I am

 ;; ;; hy ; custom - readability of scheme w/ speed of python
 ;;idris             ; a language you can depend on

 json  ; At least it ain't XML
 (javascript +lsp) ;; all(hope(abandon(ye(who(enter(here))))))
 ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
 ;;julia             ; a better, faster MATLAB
 ;;kotlin            ; a better, slicker Java(Script)
 (latex +latexmk +cdlatex) ; writing papers in Emacs has never been so fun
 ;;lean              ; for folks with too much to prove
 ledger            ; be audit you can be
 ;;lua               ; one-based indices? one-based indices
 markdown          ; writing docs for people to ignore
 ;;nim               ; python + lisp at the speed of c
 ;;nix               ; I hereby declare "nix geht mehr!"
 ;;ocaml             ; an objective camel
 (org                         ; organize your plain life in plain text
  ;; +dragndrop                  ; drag & drop files/images into org buffers
  +hugo                     ; use Emacs for hugo blogging
  +noter                      ; enhanced PDF notetaking
  +jupyter                    ; ipython/jupyter support for babel
  +pandoc                     ; export-with-pandoc support
  +gnuplot                    ; who doesn't like pretty pictures
  +present                    ; using org-mode for presentations
  +contacts
  +journal
  ;; +pretty
  ;; +passwords
  ;; +pomodoro                 ; be fruitful with the tomato technique
  )                     ; wander around notes

 ;;php               ; perl's insecure younger brother
 ;; plantuml ; diagrams for confusing people more
 graphviz
 ;;purescript        ; javascript, but functional
 (python +lsp) ; +conda beautiful is better than ugly
 ;;qt                ; the 'cutest' gui framework ever
 racket ; a DSL for DSLs
 ;;raku              ; the artist formerly known as perl6
 ;;rest              ; Emacs as a REST client
 rst                 ; ReST in peace
 ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
 ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 ;;scala             ; java, but good
 (scheme +mit) ;; +racket ; a fully conniving family of lisps
 sh ; she sells {ba,z,fi}sh shells on the C xor
 ;;sml
 ;;solidity          ; do you need a blockchain? No.
 ;;swift             ; who asked for emoji variables?
 ;;terra             ; Earth and Moon in alignment for performance.
 (web +lsp) ; the tubes
 yaml ; JSON, but readable
 ;;zig               ; C, but simpler

 :email
 ;; (mu4e +org +gmail)
 (notmuch +org)
 ;;(wanderlust +gmail)

 :app
 calendar
 emms
 (rss +org +youtube)        ; emacs as an RSS reader
 everywhere        ; *leave* Emacs!? You must be joking
 ;;irc               ; how neckbeards socialize

 ;; 2024-05-31 현재 사용자 설정은 다 커스텀 모듈로 옮기는게 좋겠다.
 ;; 2025-01-15 아니. 커스텀 다 빼.
 ;; :custom
 ;; completion
 ;; ui
 ;; emacs
 ;; editor
 ;; writing
 ;; tab-bar
 ;; (:unless IS-TERMUX extra)
 ;; eaf

 :config
 ;;literate
 (default +bindings +smartparens)
 )

;;; init.el ends here
