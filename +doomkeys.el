;;; +doomkeys.el -*- lexical-binding: t; -*-

(message "load +doomkeys.el")

;;; Global Leader Keybindings

;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/
;; NOTE: use `map!' macro for convienience
;;
;; Bind key onto Evil Normal state
;; (map! :after evil
;;       :map evil-normal-state-map
;;       "/" #'+default/search-buffer)

;; ------------------------------------------------
;; Over-ride or add to Doom Emacs default key bindings

;;;; Top-menu M-x

(map! :leader
      "SPC" nil
      "." nil
      "," nil
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Find file in project" "." #'projectile-find-file
      :desc "Find file in cwd" "," #'my/consult-fd
      :desc "consult-buffer" "`" #'consult-buffer
      :desc "Eval expression" "M-;" #'pp-eval-expression
      :desc "Search for symbol in cwd" "(" #'my/search-cwd-symbol-at-point
      )

;;;; SPC

;; (:prefix-map ("TAB" . nil))
;; :desc "Last Buffer" "TAB" #'evil-switch-to-windows-last-buffer

(progn
  ;; unset doom keybidings
  (map! (:when (modulep! :ui workspaces)
          :n "C-t"   #'nil
          :n "C-S-t" #'nil
          :g "M-0"   #'switch-to-minibuffer
          ))
  (global-set-key (kbd "C-s-[") '+workspace/switch-left)
  (global-set-key (kbd "C-s-]") '+workspace/switch-right)
  )

;; Doom's Default
;; (:when (modulep! :ui workspaces)
;;   :n "C-t"   #'+workspace/new
;;   :n "C-S-t" #'+workspace/display
;;   :g "M-1"   #'+workspace/switch-to-0
;;   :g "M-2"   #'+workspace/switch-to-1
;;   :g "M-3"   #'+workspace/switch-to-2
;;   :g "M-4"   #'+workspace/switch-to-3
;;   :g "M-5"   #'+workspace/switch-to-4
;;   :g "M-6"   #'+workspace/switch-to-5
;;   :g "M-7"   #'+workspace/switch-to-6
;;   :g "M-8"   #'+workspace/switch-to-7
;;   :g "M-9"   #'+workspace/switch-to-8
;;   ;; :g "M-0"   #'+workspace/switch-to-final
;;   :g "M-0"   #'+workspace/switch-to-final
;;   )

;;;; DONT 'l' Layout keys - disable `SPC TAB' workspace prefix

;; spacemacs's style
(map! :leader
      (:prefix ("l". "layout/workspace")
       :desc "+workspace/other" "<tab>" #'+workspace/other
       :desc "+workspace/display" "d" #'+workspace/display
       :desc "+workspace/delete" "D" #'+workspace/delete
       :desc "+workspace/switch-to" "l" #'+workspace/switch-to
       :desc "+workspace/load" "L" #'+workspace/load
       :desc "+workspace/new" "n" #'+workspace/new
       :desc "+workspace/rename " "r" #'+workspace/rename
       :desc "+workspace/restore-last-session" "R" #'+workspace/restore-last-session
       :desc "+workspace/save" "s" #'+workspace/save
       :desc "+workspace/kill-session" "x" #'+workspace/kill-session
       :desc "+workspace/swtich-to-0" "0" #'+workspace/switch-to-0
       :desc "+workspace/switch-to-1" "1" #'+workspace/switch-to-1
       :desc "+workspace/switch-to-2" "2" #'+workspace/switch-to-2
       :desc "+workspace/switch-to-3" "3" #'+workspace/switch-to-3
       :desc "+workspace/switch-to-4" "4" #'+workspace/switch-to-4
       :desc "+workspace/switch-to-5" "5" #'+workspace/switch-to-5
       ;; :desc "Switch to 6" "6" #'+workspace/switch-to-6
       ;; :desc "Switch to 7" "7" #'+workspace/switch-to-7
       ;; :desc "Switch to 8" "8" #'+workspace/switch-to-8
       ;; :desc "Switch to 9" "9" #'+workspace/switch-to-9
       ))

;;;; popup

(map! :leader
      ":" nil
      ";" nil
      ;; "`" nil
      ;; :desc "popup/toggle" "`" #'+popup/toggle
      :desc "popup/toggle" ";" #'+popup/toggle ; #'popper-toggle
      :desc "popup/close-all" ":" #'+popup/close-all
      )

;; doom /modules/config/default/+evil-bindings.el
(map! (:when (modulep! :ui popup)
        "C-`"   #'+popup/toggle
        "C-~"   #'+popup/raise
        "C-x p" #'+popup/other))

(global-set-key (kbd "C-`") #'+vterm/toggle) ;; vscode style

;;;; Replace Doom `/' highlight with buffer-search

(map! :after evil
      :map evil-normal-state-map
      "." #'+default/search-buffer) ;; / -> .

;;;; 's' search

(map! :leader
      "/" nil
      "s f" #'+vertico/consult-fd-or-find
      :desc "my/consult-fd" "s F" #'my/consult-fd
      :desc "consult-locate" "s M-l" #'consult-locate
      ;; :desc "Search project" "/" #'+default/search-project
      ;; :desc "Search cwd" "/" #'+default/search-cwd
      :desc "Search for symbol in cwd" "s SPC" #'my/search-cwd-symbol-at-point)

;;;; 'v' er/expand-region

(map! :leader
      :desc "er/expand-region" "v" #'er/expand-region
      :desc "expand-menu" "V" #'expand-transient
      )

;;;; '=' Format

(map! :leader
      (:prefix ("=" . "format")
       :desc "buffer" "=" #'+format/buffer
       :desc "buffer" "b" #'+format/buffer
       :desc "region" "r" #'+format/region
       :desc "whitespace" "w" #'delete-trailing-whitespace))

;;;; 'b' buffer

(map! :leader
      (:prefix "b"
       "b" nil
       "s" nil
       "S" nil
       "k" nil
       "K" nil
       "h" nil
       :desc "Ash-Goto-Agenda" "A" #'ash-goto-org-agenda
       ;; :desc "Dashboard" "h" #'+doom-dashboard/open
       :desc "switch-workspace-buffer" "b" #'+vertico/switch-workspace-buffer ; default
       :desc "consult-buffer" "." #'consult-buffer
       :desc "consult-buffer" "B" #'consult-buffer
       :desc "Kill all Dired buffers" "D" #'my/dired-kill-all-buffers
       :desc "Jump to Bookmark" "RET" #'consult-bookmark
       :desc "Kill buffer and window" "K" #'kill-buffer-and-window
       ;; :desc "Kill buffer and return previous" "K" #'kill-buffer-and-return-previous
       :desc "Kill all buffers" "M-k" #'doom/kill-all-buffers
       :desc "Stitch to Scratch" "s" #'bh/switch-to-scratch
       :desc "Switch to Agenda" "a" #'bh/switch-to-agenda
       :desc "Save all *Org* buffers" "S" #'org-save-all-org-buffers
       :desc "*evil* Write all buffers" "w" #'evil-write-all
       ;; :desc "Toggle Last" "TAB" #'evil-switch-to-windows-last-buffer)
       ))

;; move/ swap buffer
;; (map! :leader
;;       (:prefix "b"
;;        :desc "Move buffer to window 1" "1" #'buffer-to-window-1
;;        :desc "Move buffer to window 2" "2" #'buffer-to-window-2
;;        :desc "Move buffer to window 3" "3" #'buffer-to-window-3
;;        :desc "Move buffer to window 4" "4" #'buffer-to-window-4
;;        :desc "Swap buffer to window 1" "M-1" #'swap-buffer-window-no-follow-1
;;        :desc "Swap buffer to window 2" "M-2" #'swap-buffer-window-no-follow-2
;;        :desc "Swap buffer to window 3" "M-3" #'swap-buffer-window-no-follow-3
;;        :desc "Swap buffer to window 4" "M-4" #'swap-buffer-window-no-follow-4
;;        ))

;;;; 'f' file

;; (map! :leader
;;       (:prefix "f"
;;        :desc "Org-save-all-org-buffers" "S" #'org-save-all-org-buffers
;;        )
;;       )

;;;; 'p' project

;; Toggle treemacs project browser from project menu
;; (map! :leader
;;       (:prefix "p"
;;        "t" nil  ; disable project todos key binding
;;        :desc "Project browser" "t" #'+treemacs/toggle))

;;;; 'g' Git

;; Change SPC g s to call Magit Status, rather than stage hunk at point
;; (map! :leader
;;       (:prefix "g"
;;        :desc "" "s" nil  ; remove existing binding
;;        :desc "Magit Status" "s" #'magit-status))

;; ~/sync/man/dotsamples/doom/agzam-dot-doom/config.el
(map! :leader
      (:prefix ("g" . "git")
       "h" nil
       :desc "magit-file" "F" #'magit-file-dispatch
       :desc "jump-list" "j" #'evil-show-jumps
       ;; :desc "git status" "s" #'magit-status
       :desc "git-log-grep" "K" #'consult-git-log-grep
       :desc "gh/search-repos" "SPC" #'consult-gh-search-repos
       :desc "gh/search-find-file" "1" #'consult-gh-find-file
       (:prefix ("h" . "consult-gh")
        :desc "gh/repo-clone" "c" #'consult-gh-repo-clone
        :desc "gh/default-repos" "d" #'consult-gh-default-repos
        :desc "gh/find-file" "f" #'consult-gh-find-file
        :desc "gh/search-repos" "s" #'consult-gh-search-repos
        :desc "gh-search-code" "S" #'consult-gh-search-code
        :desc "gh-search-prs" "p" #'consult-gh-search-prs
        :desc "gh-pr-list" "P" #'consult-gh-pr-list
        :desc "gh-search-issues" "i" #'consult-gh-search-issues
        :desc "gh-issue-list" "I" #'consult-gh-issue-list
        :desc "gh-fork-current-repo" "k" #'consult-gh-fork-current-repo)
       (:prefix (";" . "git link")
        :desc "git-link-blame" "b" #'git-link-blame
        :desc "git-link-kill/copy" "l" #'git-link-kill
        :desc "git-link-main-branch" "m" #'git-link-main-branch)))

;;;; 'h' help

(map! :leader
      (:prefix "h"
       "a" #'helpful-at-point
       "f" #'helpful-function
       "h" #'helpful-symbol
       "t" nil ; consult-theme
       :desc "themes-map" "t" ews-modus-themes-map
       "p" nil
       (:prefix ("p" . "packages")
                "l" #'list-packages
                "f" #'find-library-other-window
                "p" #'doom/help-packages
                "d" #'doom/describe-package)
       "1" #'find-function-other-window
       "v" #'helpful-variable
       "j" #'info-display-manual))

;;;; 'k' lisp

;; /john-dot-doom/+smartparens.el
(when (locate-library "smartparens")
  ;; A Spacemacs like Lisp state menu (without the transient state)
  (map! :leader
        (:prefix ("k". "Smartparens")
         :desc "Delete Pair" "D" #'delete-pair
         :desc "Slurp forward" "s" #'sp-forward-slurp-sexp
         :desc "Slurp backward" "S" #'sp-backward-slurp-sexp
         :desc "" "$"   #'sp-end-of-sexp
         (:prefix ("`" . "Hybrid")
          :desc "Kill" "k" #'sp-kill-hybrid-sexp
          :desc "Push" "p" #'sp-push-hybrid-sexp
          :desc "Slurp" "s" #'sp-slurp-hybrid-sexp
          :desc "Transpose" "t" #'sp-transpose-hybrid-sexp
          :desc "Absorb" "a" #'sp-absorb-sexp
          :desc "Barf forward" "b" #'sp-forward-barf-sexp
          :desc "Barf backward" "B" #'sp-backward-barf-sexp
          :desc "Convoluted" "c" #'sp-convolute-sexp)
         (:prefix ("d" . "Delete")
          :desc "Delete Pair" "d" #'delete-pair
          :desc "Symbol" "s" #'sp-kill-symbol
          :desc "Symbol Backward" "S" #'sp-backward-kill-symbol
          :desc "Word" "w" #'sp-kill-word
          :desc "Word Backward" "W" #'sp-backward-kill-word
          :desc "Kill" "x" #'sp-kill-sexp
          :desc "Kill Backward" "X" #'sp-backward-kill-sexp)
         :desc "Splice" "e" #'sp-splice-sexp-killing-forward
         :desc "Splice Backward" "E" #'sp-splice-sexp-killing-backward
         :desc "Symbol Backward" "h" #'sp-backward-symbol
         :desc "Sexp Backward" "H" #'sp-backward-sexp
         :desc "Join" "j" #'sp-join-sexp
         :desc "Sexp Forward" "l" #'sp-forward-sexp
         :desc "Sexp Forward" "L" #'sp-forward-sexp
         :desc "Raise" "r" #'sp-raise-sexp
         :desc "Slurp" "s" #'sp-forward-slurp-sexp
         :desc "Slurp Backward" "S" #'sp-backward-slurp-sexp
         :desc "Transpose" "t" #'sp-transpose-sexp
         :desc "Up Backward" "U" #'sp-backward-up-sexp
         (:prefix ("w" . "Wrap")
          :desc "()" "(" #'sp-wrap-round
          :desc "{}" "{" #'sp-wrap-curly
          :desc "[]" "[" #'sp-wrap-square
          :desc "Round" "w" #'sp-wrap-round
          :desc "Curly" "c" #'sp-wrap-curly
          :desc "Square" "s" #'sp-wrap-square
          :desc "Unwrap" "u" #'sp-unwrap-sexp)
         :desc "Copy sexp" "y" #'sp-copy-sexp))
  )

;; (when (locate-library "puni")
;;   (map! :leader
;;         (:prefix ("k". "paredit")
;;          :desc "Delete Pair" "D" #'delete-pair
;;          :desc "Slurp forward" "s" #'puni-slurp-forward
;;          :desc "Slurp backward" "S" #'puni-slurp-backward
;;          :desc "" "$"   #'puni-end-of-sexp
;;          (:prefix ("d" . "Delete")
;;           :desc "Delete Pair" "d" #'delete-pair
;;           ;; :desc "Symbol" "s" #'sp-kill-symbol
;;           ;; :desc "Symbol Backward" "S" #'sp-backward-kill-symbol
;;           ;; :desc "Word" "w" #'sp-kill-word
;;           ;; :desc "Word Backward" "W" #'sp-backward-kill-word
;;           :desc "Kill" "x" #'puni-kill-sexp
;;           ;; :desc "Kill Backward" "X" #'sp-backward-kill-sexp
;;           )
;;          :desc "Splice" "e" #'puni-splice-killing-forward
;;          :desc "Splice Backward" "E" #'sp-splice-killing-backward
;;          ;; :desc "Symbol Backward" "h" #'sp-backward-symbol
;;          :desc "Sexp Backward" "H" #'puni-backward-sexp
;;          ;; :desc "Join" "j" #'sp-join-sexp
;;          :desc "Sexp Forward" "l" #'puni-forward-sexp
;;          ;; :desc "Sexp Forward" "L" #'puni-forward-sexp

;;          ;; :desc "Raise" "r" #'sp-raise-sexp

;;          :desc "Slurp" "s" #'puni-forward-sexp
;;          ;; :desc "Slurp Backward" "S" #'puni-backward-sexp
;;          ;; :desc "Transpose" "t" #'puni-transpose-sexp
;;          ;; :desc "Up Backward" "U" #'sp-backward-up-sexp
;;          (:prefix ("w" . "Wrap")
;;           :desc "()" "(" #'puni-wrap-round
;;           :desc "{}" "{" #'puni-wrap-curly
;;           :desc "[]" "[" #'puni-wrap-square
;;           ;; :desc "Round" "w" #'puni-wrap-round
;;           ;; :desc "Curly" "c" #'puni-wrap-curly
;;           ;; :desc "Square" "s" #'puni-wrap-square
;;           :desc "Unwrap" "u" #'puni-splice)
;;          ;; :desc "Copy sexp" "y" #'sp-copy-sexp
;;          )
;;         )
;;   )

;;;; 'C' Capture

;; Add C for Capture
(map! :leader
      (:prefix ("C" . "Capture")
       :desc "orgabilize: url-to-org" "a" #'orgabilize-org-archive
       "n" #'org-capture
       :desc "orgabilize: insert-org-link" "l" #'orgabilize-insert-org-link
       :desc "orgabilize: find-archived-files" "f" #'orgabilize-org-find-archived-file
       :desc "org-web-tools: read-url-as-org" "t" #'org-web-tools-read-url-as-org
       :desc "org-web-tools: read-url-as-org" "T" #'org-web-tools-convert-links-to-page-entries
       :desc "jao-eww-to-org" "e" #'jao-eww-to-org
       :desc "wikinforg" "w" #'wikinforg
       :desc "wiki-summary" "s" #'wiki-summary-insert
       :desc "remember" "r" #'remember
       :desc "remember-notes" "R" #'remember-notes
       "i" #'bh/insert-inactive-timestamp
       "o" #'org-pandoc-import-as-org
       :desc "org-remark-mark-line" "m" #'org-remark-mark-line
       :desc "hypothesis: to archive" "h" #'hypothesis-to-archive
       ))

;;;; 'D' Diff/Compare

;; 둠에 어디 있는지 모르겠다만 참고해서 넣어놓고 사용하라
;; Diff of files
(map! :leader
      (:prefix ("D" . "Diff/Compare")
               (:prefix ("d" . "diff")
                :desc "3 files" "3" #'ediff3
                :desc "ediff" "d" #'diff
                :desc "ediff" "e" #'ediff
                :desc "version" "r" #'vc-root-diff
                :desc "version" "v" #'vc-ediff)))

;; spacemacs : layers/+spacemacs/spacemacs-defaults/keybindings.el
;; ("D" "Diff/Compare"
;;    ("b"  "Buffers"
;;     ("3" ediff-buffers3 "Between 3 buffers...")
;;     ("b" ediff-buffers "Between 2 buffers...")
;;     ("B" ediff-backup "With backup file...")
;;     ("p" ediff-patch-buffer "With a patch..."))
;;    ("d" "Directories"
;;     ("3" ediff-directories3 "Between 3 directories...")
;;     ("d" ediff-directories "Between 2 directories...")
;;     ("r" ediff-directory-revisions "Using SCM revisions..."))
;;    ("f" "Files"
;;     ("." spacemacs/ediff-dotfile-and-template "With Spacemacs dotfile")
;;     ("3" ediff-files3 "Between 3 files...")
;;     ("f" ediff-files "Between 2 files...")
;;     ("p" ediff-patch-file "With a patch...")
;;     ("v" ediff-revision "Between file revisions..."))
;;    ("m" "Merge"
;;     ("b" "Buffers"
;;      ("3" ediff-merge-buffers-with-ancestor "3-way merge...")
;;      ("b" ediff-merge-buffers "2-way merge..."))
;;     ("d" "Directories"
;;      ("3" ediff-merge-directories-with-ancestor "3-way merge...")
;;      ("d" ediff-merge-directories "2-way merge..."))
;;     ("f" "Files"
;;      ("3" ediff-merge-files-with-ancestor "3-way merge...")
;;      ("f" ediff-merge-files "2-way merge..."))
;;     ("r" "Revisions"
;;      ("3" ediff-merge-revisions-with-ancestor "3-way merge...")
;;      ("r" ediff-merge-revisions "2-way merge...")))
;;    ("r" "Regions"
;;     ("l" ediff-regions-linewise "Between 2 regions (linewise)...")
;;     ("w" ediff-regions-wordwise "Between 2 regions (wordwise)..."))
;;    ("w" "Windows"
;;     ("l" ediff-windows-linewise "Linewise between visible text...")
;;     ("w" ediff-windows-wordwise "Wordwise between visible text..."))
;;    ("s" ediff-show-registry "Show registry")
;;    ("h" ediff-documentation "Documentation"))

;;;; 'w' window

(map! :leader
      :prefix "w"
      "1" nil "2" nil "3" nil "4" nil "5" nil "6" nil "7" nil "8" nil "9" nil "0" nil "-" nil "b" nil "d" nil "r" nil "R" nil "m" nil "<" nil ">" nil "_" nil "|" nil
      "C-=" nil "C-_" nil "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-k" nil "C-l" nil "C-w" nil "C-n" nil "C-o" nil "C-p" nil "C-q" nil "C-r" nil "C-s" nil "C-t" nil "C-u" nil "C-v" nil "C-x" nil "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil "C-<down>" nil "C-<left>" nil "C-<right>" nil "C-<up>" nil
      "TAB" #'evil-window-prev
      "." #'window-transient
      "c" #'window-cleanup+
      "g" #'golden-ratio
      :desc "delete-window" "d" #'spacemacs/delete-window
      ;; "D" #'delete-window ; block delete workspace
      "M" #'ace-swap-window
      ;; "W" #'ace-window
      "m" #'toggle-maximize-buffer
      "=" #'balance-windows-area
      :desc "window-vsplit" "/" #'evil-window-vsplit
      ;; :desc "window-vsplit" "v" #'evil-window-vsplit
      ;; :desc "window-vsplit-follow" "V" #'+evil/window-vsplit-and-follow
      :desc "window-layout-toggle" "-" 'spacemacs/window-layout-toggle
      :desc "delete-other-window" "O" 'delete-other-windows)

;;;; 'i' insert/translate

(map! :leader
      (:prefix "i"
       :desc "time-stamp" "0" #'time-stamp
       :desc "hl-todo-insert" "t" #'hl-todo-insert
       :desc "add-global-abbrev" "a" #'add-global-abbrev
       :desc "list-unicode-display" "U" #'list-unicode-display
       ;; :desc "txl-translate-insert" "i" #'txl-translate-insert
       :desc "wiki-summary-insert" "w" #'wiki-summary-insert
       :desc "immersive-translate-paragraph" "m" #'immersive-translate-paragraph
       :desc "immersive-translate-auto-mode" "M" #'immersive-translate-auto-mode
       :desc "gt-do-translate" "d" #'gt-do-translate
       :desc "consult-register" "r" #'consult-register
       :desc "consult-yasnippet" "s" #'consult-yasnippet
       :desc "+default/yank-pop" "y" #'+default/yank-pop
       ))

;;;; 'x' text

(map! :leader
      "x" nil
      (:prefix ("x" ."text")
               "x" #'jinx-correct-word
               ;; (:prefix ("l" . "language")
               ;;  :desc "define" "d" #'define-it-at-point
               ;;  :desc "grammarly check" "g" #'lsp-grammarly-check-grammar
               ;;  :desc "sdcv" "l" #'sdcv-search-pointer
               ;;  :desc "Merriam Webster" "m" #'mw-thesaurus-lookup-dwim
               ;;  :desc "wiktionary" "w" #'wiktionary-bro-dwim)
               (:prefix ("g" . "google-translate")
                :desc "en->ko" "k" #'google-translate-query-translate-reverse
                :desc "en->ko2" "K" #'+google-translate-en->ko
                :desc "ko->en" "e" #'google-translate-query-translate
                :desc "ko->en2" "E" #'+google-translate-ko->en
                :desc "translate-at-point" "g" #'google-translate-at-point)
               (:prefix ("c" . "chatgpt")
                        "c" #'gptel+
                        "e" #'+gptel-improve-text-transient
                        "p" #'gptel-save-as-org-with-denote-metadata
                        "s" #'gptel-send))
      )

;;;; 's' search/symbol

(map! :leader
      (:prefix ("s" . "search/symbol")
       :desc "consult-omni-transient" "/" #'consult-omni-transient
       :desc "eww-search-words" "1" #'eww-search-words
       :desc "find-name-dired" "2" #'find-name-dired
       :desc "search-github-with-lang" "g" #'+search-github-with-lang
       :desc "imenu" "j" #'imenu)
      )

;;;; 'S' Search Plus : Custom Search

(require 'my-search)

(map! :leader
      (:prefix-map ("S" . "Search+")
       :desc "engine-mode >>" "s"  #'engine-mode-prefixed-map
       :desc "google"           "g"     #'my/search-google
       :desc "naver"            "N"     #'my/search-naver
       :desc "naver > terms"             "n"     #'my/search-terms-naver
       :desc "daum > dict"             "d"     #'my/search-dict-daum
       ;; :desc "dotnet"           "D"     #'my/search-dotnet
       ;; :desc "onelook"          "e"     #'my/search-onelook
       :desc "thesaurus"        "t"     #'my/search-thesaurus
       ;; :desc "elixir"           "x"     #'my/search-elixir
       ;; :desc "flutter"          "f"     #'my/search-flutter
       :desc "blogs"         "b"     #'my/search-blogs
       ;; :desc "sdcv > at-point" "/" 'sdcv-search-pointer
       ;; :desc "sdcv > input" "?" 'sdcv-search-input
       ;; :desc "sdcv > at-point posframe" "." 'sdcv-search-pointer+ ; posframe
       :desc "wordreference > ko->en" "\[" 'my/wr-koen
       :desc "wordreference > en->ko" "\]" 'my/wr-enko

       ;; :desc "lexic > search" "l" 'lexic-search
       ;; :desc "external-dict > search" "e" 'external-dict-dwim
       ;; :desc "mw-thesaurus > lookup" "X" 'mw-thesaurus-lookup-dwim
       ;; :desc "powerthesaurus > transient" "P" 'powerthesaurus-transient

       (:prefix ("w" . "wiktionary")
                "e" 'wiktionary-lookup-word-en
                "k" 'wiktionary-lookup-word-ko)
       (:prefix ("q" . "wikiquote")
                "e" 'wikiquote-lookup-quote-en
                "k" 'wikiquote-lookup-quote-ko)
       ))

;;;; 't' toggle

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "v-line nav" "w" #'+toggle-visual-line-navigation
       :desc "consult-minor-mode" "m" #'consult-minor-mode-menu
       :desc "tab-line mode" "T" #'tab-line-mode
       ;; "y" #'consult-yank-from-kill-ring
       :desc "Copilot" "c" #'copilot-mode
       :desc "CamelCase" "C" #'subword-mode
       :desc "Column Indicator" "I" #'display-fill-column-indicator-mode
       ;; :desc "Window dedication" "d" #'spacemacs/toggle-current-window-dedication
       :desc "toggle-window-dedicated" "d" #'toggle-window-dedicated)
      )

;;;; 'o' open

(map! :leader
      (:prefix ("o" . "open")
       ;; :desc "Side-journal-toggle" :n "TAB" #'side-journal-toggle-notes
       :desc "open journal today" :n "SPC" #'org-journal-open-current-journal-file
       :desc "elfeed" "l" #'elfeed
       ))

;;;; 'j' junghanacs hotkey

(map! :leader
      (:prefix ("j" . "junghanacs")
       :desc "Mastodon" "m" #'mastodon
       :desc "Tmr" "t" #'tmr
       :desc "Tmr-view" "v" #'tmr-tabulated-view
       :desc "Anddo: Todos" "d" #'anddo
       ))

;;;; 'r' remote / register

(map! :leader
      :desc "jump to register" "rr" #'jump-to-register)

;; (set-register ?l '(file . "~/org/ledger/ledger-2024.dat"))
(set-register ?b '(file . "~/sync/org/blog/20240104T061355--blog.org"))
(set-register ?c '(file . "~/.doom.d/README.org"))

;;;; 'n' +notes denote

(map! :leader
      (:prefix ("n" . "notes")
       ;; "a" nil
       "d" nil
       "n" nil
       "S" nil
       ;; default keybindings
       ;; :desc "ews-annotate-map"             "a" ews-annotate-map
       :desc "ash-goto-agenda"               "A" 'ash-goto-org-agenda
       ;; :desc "consult-org-agenda" "A" #'consult-org-agenda ; M-g a

       :desc "ews-bibliography-map"          "b" ews-bibliography-map
       :desc "org-capture"                   "c" #'org-capture
       :desc "+org/toggle-last-clock"        "C" #'+org/toggle-last-clock
       :desc "ews-denote-map"                "d" ews-denote-map

       :desc "+default/find-in-notes"        "f" #'+default/find-in-notes ; find-files
       :desc "+default/browse-notes"         "F" #'+default/browse-notes

       :desc "org-store-link"                "l" #'org-store-link
       :desc "org-store-link-id-optional"    "L" #'my/org-store-link-id-optional
       :desc "org-tags-view"                 "m" #'org-tags-view

       :desc "org-clock-cancel"              "M-c" #'org-clock-cancel
       ;; :desc "org-capture-goto-target"       "N" #'org-capture-goto-target

       :desc "org-clock-goto"                "o" #'org-clock-goto
       :desc "org-todo-list"                 "t" #'org-todo-list

       :desc "+default/org-notes-search"     "g" #'+default/org-notes-search ; grep
       ;; :desc "+default/org-notes-headlines"  "S" #'+default/org-notes-headlines

       :desc "org-search-veiw"               "v" #'org-search-view
       "u" #'org-transclusion-mode

       :desc "+org/export-to-clipboard"      "y" #'+org/export-to-clipboard

       :desc "org-journal-open-today" "SPC" #'org-journal-open-current-journal-file
       ;; :desc "side-journal-toggle" "TAB" #'side-journal-toggle-notes

       :desc "org-drill"                     "D" #'org-drill

       :desc "consult-org-all"               "'" #'my/consult-org-all

       ;; :desc "ews-note-map"                  "n" ews-note-map
       :desc "ews-denote-map"                  "n" ews-denote-map

       ;; :desc "my/denote-random-note"        "?" #'my/denote-random-note
       ;; :desc "org-roam-random-no-dates"        "?" #'ash/org-roam-node-random-no-dates
       ))

;;;; 'N' consult-notes

(after! consult-notes
  (map! :leader
        :desc "consult-notes" "N" 'consult-notes))

;;;; DONT SPC 1-4 window

;; (map! :leader
;;       :desc "select-window 1" "1" #'winum-select-window-1
;;       :desc "select-window 2" "2" #'winum-select-window-2
;;       :desc "select-window 3" "3" #'winum-select-window-3
;;       :desc "select-window 4" "4" #'winum-select-window-4)

;;;; TODO '0' LLM - gptel

(map! :leader
      "0" nil
      (:prefix ("0" . "llm/gptel")
       ;; "0" #'+gpt-dwim-current-buffer
       "0" #'gptel-mode
       "t" #'gptel-org-toggle-branching-context
       :desc "gptel-send: default" :n "l" (cmd! (cashpw/gptel-send (alist-get 'default gptel-directives)))
       :desc "gptel-send: chain-of-thought" :n "c" (cmd! (cashpw/gptel-send (alist-get 'chain-of-thought gptel-directives)))
       :desc "gptel-send: follow-up" :n "f" (cmd! (cashpw/gptel-send (alist-get 'follow-up gptel-directives))))
      )

;;; Custom EVIL Keys

;; agzam : /agzam-dot-doom/config.el
(map! :i "M-l" #'sp-forward-slurp-sexp ; downcase-word
      :i "M-h" #'sp-forward-barf-sexp  ; mark-paragraph
      ;; :v "s" #'evil-surround-region
      ;; "s-b" #'consult-buffer
      ;; "s-=" #'text-scale-increase
      ;; "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter)) ; nop
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter)) ; nop
      :n "zk" #'text-scale-increase ; fold
      :n "zj" #'text-scale-decrease
      ;; :n "DEL" #'previous-buffer
      :n "DEL" #'evil-switch-to-windows-last-buffer
      ;; :n "s-e" #'+scroll-line-down-other-window
      ;; :n "s-y" #'+scroll-line-up-other-window
      :i "M-/" #'hippie-expand
      ;; :n "g9" #'ibuffer-sidebar-jump ; 'gi' evil-insert-resume
      :n "g SPC" #'evil-jump-to-tag
      :i "C-v" #'evil-paste-after ; evil-quoted-insert : 'C-q'
      ;; :i "TAB" #'completion-at-point ; jump out of
      ;; (:when (featurep :system 'linux)
      ;;   :i "C-M-S-s-y" #'nerd-dictation-toggle)
      ;; SPC g [ / ]
      :n "[ g" #'+vc-gutter/previous-hunk ; remap diff-hl-previous-hunk
      :n "] g" #'+vc-gutter/next-hunk ; remap diff-hl-next-hunk

      :m "8" #'evil-ex-search-word-forward ; default *
      :m "3" #'evil-ex-search-word-backward ; default #
      :m "4" #'evil-end-of-line ; default $
      :m "0" #'evil-beginning-of-line

      ;; :m "C-i" #'evil-jump-forward ;; evil-want-C-i-jump - evil-maps.el
      :n "g ]" #'evil-jump-forward
      :n "g [" #'evil-jump-backward
      :n "g RET" #'tabgo

      )

;;; Major-Mode Leader Keybindings

;;;; minibuffer-mode-map

(map! (:map minibuffer-mode-map
            "M-l" #'sp-forward-slurp-sexp
            "M-h" #'sp-forward-barf-sexp)
      (:map minibuffer-local-map
            "C-c C-s" #'embark-collect))

;;;; Doom's org-mode-map

;;;;; after! evil-org

(after! evil-org
  ;; (message "after evil-org - doomkeys")

  ;; doom에서 =C-RET= 키는 아래에 추가 =C-S-RET= 키는 위로 추가로 바인딩을
  ;; 변경한다. 새로 함수를 추가해서 해당 함수에 바인딩하는데, 해당 함수에
  ;; =org-blank-before-new-entry= 심볼 값이 반영이 안 되어 있어서 org mode의
  ;; 디폴트 함수로 바인딩을 했다.

  (map! :map evil-org-agenda-mode-map "P" 'org-procrastinate)
  (map! :map evil-org-mode-map
        ;; :ni [C-return]   #'org-insert-heading-respect-content
        ;; :ni [C-S-return] #'org-insert-todo-heading-respect-content
        :ni "C-c C-RET"      #'my/org-open-at-point-other-window
        :ni "C-c C-<return>" #'my/org-open-at-point-other-window))

(after! embark
  (map!
   (:map embark-org-link-map
    :desc "open-at-point-other-window" "o" #'my/org-open-at-point-other-window
    )))

;;;;; after! org

(after! org
  (message "after org - doomkeys")

;;;;; org-mode-map

  (map! :map org-mode-map
        ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
        ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
        ;; that, and should probably be PRed to org.
        ;; [tab]        #'org-cycle

        "M-8" #'tempel-insert
        "<f12>" #'org-transclusion-mode
        "C-M-y" #'org-rich-yank
        ;; "C-M-Y" #'cae-org-rich-yank
        "s-p" #'org-hugo-export-to-md ;; "M-p"
        "M-n" #'org-next-link
        "M-p" #'org-previous-link
        :n "]b"  #'org-next-block
        :n "[b"  #'org-previous-block

        ;; "C-c C-S-l"  #'+org/remove-link
        ;; "C-c C-i"    #'org-toggle-inline-images
        ;; ;; textmate-esque newline insertion
        ;; "S-RET"      #'+org/shift-return
        ;; "C-RET"      #'+org/insert-item-below
        ;; "C-S-RET"    #'+org/insert-item-above
        ;; "C-M-RET"    #'org-insert-subheading
        ;; [C-return]   #'+org/insert-item-below
        ;; [C-S-return] #'+org/insert-item-above
        ;; [C-M-return] #'org-insert-subheading
        ;; (:when (featurep :system 'macos)
        ;;   [s-return]   #'+org/insert-item-below
        ;;   [s-S-return] #'+org/insert-item-above
        ;;   [s-M-return] #'org-insert-subheading)
        ;; ;; Org-aware C-a/C-e
        ;; [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
        ;; [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line
        )

;;;;; localleader 1

  (map! :map org-mode-map
        :localleader
        "RET" #'gptel-mode
        "#" #'org-update-statistics-cookies
        "'" #'org-edit-special
        "*" #'org-ctrl-c-star
        "+" #'org-ctrl-c-minus
        "," #'org-switchb
        "." #'org-goto
        "@" #'org-cite-insert
        (:when (modulep! :completion vertico)
          "." #'consult-org-heading
          "/" #'consult-org-agenda)
        "A" #'org-archive-subtree-default
        "e" #'org-export-dispatch
        "f" #'org-footnote-action
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-id-get-create
        "k" #'org-babel-remove-result
        "K" #'+org/remove-result-blocks
        "n" #'org-store-link
        "o" #'org-set-property
        "q" #'org-set-tags-command
        "t" #'org-todo
        "T" #'org-todo-list
        "x" #'org-toggle-checkbox
        "V" #'org-marked-text-overview-mode
        (:prefix ("a" . "attachments")
                 "a" #'org-attach
                 "d" #'org-attach-delete-one
                 "D" #'org-attach-delete-all
                 "l" #'+org/attach-file-and-insert-link
                 "f" #'my/consult-org-screenshot
                 "F" #'+org/find-file-in-attachments
                 "n" #'org-attach-new
                 "o" #'org-attach-open
                 "O" #'org-attach-open-in-emacs
                 "r" #'org-attach-reveal
                 "R" #'org-attach-reveal-in-emacs
                 "u" #'org-attach-url
                 "s" #'org-attach-set-directory
                 "S" #'org-attach-sync
                 (:when (modulep! +dragndrop)
                   "c" #'org-download-screenshot
                   "p" #'org-download-clipboard
                   "P" #'org-download-yank))
        (:prefix ("b" . "tables")
                 "-" #'org-table-insert-hline
                 "a" #'org-table-align
                 "b" #'org-table-blank-field
                 "c" #'org-table-create-or-convert-from-region
                 "e" #'org-table-edit-field
                 "f" #'org-table-edit-formulas
                 "h" #'org-table-field-info
                 "s" #'org-table-sort-lines
                 "r" #'org-table-recalculate
                 "R" #'org-table-recalculate-buffer-tables
                 (:prefix ("d" . "delete")
                          "c" #'org-table-delete-column
                          "r" #'org-table-kill-row)
                 (:prefix ("i" . "insert")
                          "c" #'org-table-insert-column
                          "h" #'org-table-insert-hline
                          "r" #'org-table-insert-row
                          "H" #'org-table-hline-and-move)
                 (:prefix ("t" . "toggle")
                          "f" #'org-table-toggle-formula-debugger
                          "o" #'org-table-toggle-coordinate-overlays)
                 (:when (modulep! +gnuplot)
                   "p" #'org-plot/gnuplot))
        (:prefix ("c" . "clock")
                 "c" #'org-clock-cancel
                 "d" #'org-clock-mark-default-task
                 "e" #'org-clock-modify-effort-estimate
                 "E" #'org-set-effort
                 "g" #'org-clock-goto
                 "G" (cmd! (org-clock-goto 'select))
                 "l" #'+org/toggle-last-clock
                 "i" #'org-clock-in
                 "I" #'org-clock-in-last
                 "o" #'org-clock-out
                 "r" #'org-resolve-clocks
                 "R" #'org-clock-report
                 "t" #'org-evaluate-time-range
                 "=" #'org-clock-timestamps-up
                 "-" #'org-clock-timestamps-down)
        (:prefix ("d" . "date/deadline")
                 "d" #'org-deadline
                 "s" #'org-schedule
                 "t" #'org-time-stamp
                 "T" #'org-time-stamp-inactive)
        (:prefix ("g" . "goto")
                 "g" #'org-goto
                 (:when (modulep! :completion ivy)
                   "g" #'counsel-org-goto
                   "G" #'counsel-org-goto-all)
                 (:when (modulep! :completion helm)
                   "g" #'helm-org-in-buffer-headings
                   "G" #'helm-org-agenda-files-headings)
                 (:when (modulep! :completion vertico)
                   "g" #'consult-org-heading
                   "G" #'consult-org-agenda)
                 "c" #'org-clock-goto
                 "C" (cmd! (org-clock-goto 'select))
                 "i" #'org-id-goto
                 "r" #'org-refile-goto-last-stored
                 "v" #'+org/goto-visible
                 "x" #'org-capture-goto-last-stored)
        (:prefix ("l" . "links")
                 "c" #'org-cliplink
                 "d" #'+org/remove-link
                 "i" #'org-id-store-link
                 "l" #'org-insert-link
                 "L" #'org-insert-all-links
                 "s" #'org-store-link
                 "S" #'org-insert-last-stored-link
                 "t" #'org-toggle-link-display
                 (:when (modulep! :os macos)
                   "g" #'org-mac-link-get-link))
        (:prefix ("P" . "publish")
                 "a" #'org-publish-all
                 "f" #'org-publish-current-file
                 "p" #'org-publish
                 "P" #'org-publish-current-project
                 "s" #'org-publish-sitemap)
        (:prefix ("r" . "refile")
                 "." #'+org/refile-to-current-file
                 "c" #'+org/refile-to-running-clock
                 "l" #'+org/refile-to-last-location
                 "f" #'+org/refile-to-file
                 "o" #'+org/refile-to-other-window
                 "O" #'+org/refile-to-other-buffer
                 "v" #'+org/refile-to-visible
                 "r" #'org-refile
                 "R" #'org-refile-reverse) ; to all `org-refile-targets'
        (:prefix ("s" . "tree/subtree")
                 "a" #'org-toggle-archive-tag
                 "b" #'org-tree-to-indirect-buffer
                 "c" #'org-clone-subtree-with-time-shift
                 "d" #'org-cut-subtree
                 "h" #'org-promote-subtree
                 "j" #'org-move-subtree-down
                 "k" #'org-move-subtree-up
                 "l" #'org-demote-subtree
                 "n" #'org-narrow-to-subtree
                 "r" #'org-refile
                 "s" #'org-sparse-tree
                 "A" #'org-archive-subtree-default
                 "N" #'widen
                 "S" #'org-sort)
        (:prefix ("p" . "priority")
                 "d" #'org-priority-down
                 "p" #'org-priority
                 "u" #'org-priority-up))

;;;;; localleader 2

  (map! :map org-mode-map
        :localleader
        "o" nil
        ;; :desc "@note-map" "n" ews-note-map
        :desc "@denote-map" "n" ews-denote-map
        :desc "@org-transclusion-map" "u" ews-org-transclusion-map
        :desc "@org-noter-map" "o" ews-org-noter-map

        :desc "org-set-effot" "E" #'org-set-effort
        :desc "time-stamp" "!" #'time-stamp
        :desc "org-appear-mode" "8" #'org-appear-mode
        :desc "insert-inactive-timestamp" "9" #'bh/insert-inactive-timestamp

        :desc "insert checkbox\|bracket" "]" #'cae-org-insert-checkbox-or-bracket
        :desc "convert syntax to lower" "L" #'cae-org-syntax-convert-keyword-case-to-lower

        ;; l links
        :desc "cae-org-insert-file-link" "l f" #'cae-org-insert-file-link
        :desc "my/org-store-link-id-optional" "l I" #'my/org-store-link-id-optional

        :desc "org-paste-subtree" "s p" #'org-paste-subtree
        :desc "org-rich-yank" "l y" #'org-rich-yank
        ;; :desc "cae-org-rich-yank" "l Y" #'cae-org-rich-yank
        :desc "update statistics cookies" "#" #'org-update-statistics-cookies
        :desc "rename-file-and-buffer" "R" #'my/rename-file-and-buffer

        :desc "ox-reveal: export > html" "PR" #'org-reveal-export-to-html
        :desc "ox-re-reveal: export > html" "Pr" #'org-re-reveal-export-to-html
        :desc "ox-hugo: export > md" "Ph" #'org-hugo-export-to-md

        ;; math
        ;; :desc "math-preview-at-point" "/" #'math-preview-at-point
        ;; :desc "math-preview-all" "M-/" #'math-preview-all
        ;; :desc "math-preview-clear-all" "C-M-/" #'math-preview-clear-all

        :desc "math-symbol-list" "C-0" #'math-symbol-list
        (:prefix ("0" . "@custom")
                 "c" 'my/genfile-timestamp
                 "b" 'palimpsest-move-region-to-bottom
                 "B" 'palimpsest-move-region-to-top
                 "d" 'my/get-file-line
                 "e" 'my/get-file-link
                 "f" 'my/encode-buffer-to-utf8
                 "g" 'my/copy-word
                 "h" 'my/copy-line
                 "i" 'my/copy-paragraph
                 "j" 'my/copy-buffer
                 "k" 'my/backward-last-edit
                 "t" 'my/org-titlecase-level-1
                 "l" 'my/buffer-edit-hook
                 "R" 'my/rename-file-and-buffer
                 "n" 'my/grep-find
                 "o" 'my/open-external
                 "p" 'my/open-external-pdf
                 "q" 'my/unfill-paragraph-or-region
                 "0" 'cc-todo-item
                 )
        (:prefix ("-" . "translate-mode")
                 "t" 'translate-mode
                 "p" 'translate/translate-current-reference-paragraph
                 "w" 'translate/translate-word-at-point
                 "f" 'translate-open-reference-file
                 "b" 'translate-select-reference-buffer
                 "h" 'translate-toggle-highlight
                 )
        )

;;;;; after! org-journal

  (message "after org-journal - doomkeys")
  (require 'org-journal)

  (map! (:map org-journal-mode-map
         :n "]f"  #'org-journal-next-entry
         :n "[f"  #'org-journal-previous-entry
         :n "]b"  #'org-next-block
         :n "[b"  #'org-previous-block
         :n "C-n" #'org-next-visible-heading
         :n "C-p" #'org-previous-visible-heading)
        ;; :n "C-n" #'org-journal-next-entry
        ;; :n "C-p" #'org-journal-previous-entry
        (:map org-journal-search-mode-map
              "C-n" #'org-journal-search-next
              "C-p" #'org-journal-search-previous)
        :localleader
        (:map org-journal-mode-map
              (:prefix ("a" . "attachments"))
              (:prefix ("b" . "tables"))
              (:prefix ("c" . "clock"))
              (:prefix ("d" . "date/deadline"))
              (:prefix ("p" . "priority"))
              (:prefix ("g" . "goto"))
              (:prefix ("s" . "tree/subtree"))
              (:prefix ("r" . "refile"))
              (:prefix ("P" . "publish"))
              (:prefix ("l" . "links"))
              (:prefix ("0" . "@custom"))
              (:prefix ("-" . "translate-mode"))
              (:prefix ("u" . "xxxx"))
              (:prefix ("o" . "xxxx"))
              (:prefix ("n" . "xxxx"))
              (:prefix ("p" . "xxxx"))
              (:prefix ("j" . "journal")
                       "c" #'org-journal-new-entry
                       "d" #'org-journal-new-date-entry
                       "n" #'org-journal-next-entry
                       "p" #'org-journal-previous-entry)
              (:prefix ("S" . "journal-search")
                       "s" #'org-journal-search
                       "f" #'org-journal-search-forever
                       "F" #'org-journal-search-future
                       "w" #'org-journal-search-calendar-week
                       "m" #'org-journal-search-calendar-month
                       "y" #'org-journal-search-calendar-year))
        (:map org-journal-search-mode-map
              "n" #'org-journal-search-next
              "p" #'org-journal-search-prev)
        )
  )

;;;; markdown-mode-map

;; Changes
;; - move toggle prefix from `t' to `T'
;; - add table prefix `t'

(with-eval-after-load 'markdown-mode
  (map! :map markdown-mode-map
        :localleader
        "y" #'yank-as-org
        ;; (:prefix ("t" . "Table")
        ;;  :desc "Header" "h" #'markdown-table-hline-at-point-p
        ;;  :desc "Sort" "s" #'markdown-table-sort-lines
        ;;  :desc "Region to table" "r" #'markdown-table-convert-region
        ;;  :desc "Table insert" "t" #'markdown-insert-table
        ;;  (:prefix ("d" . "Delete")
        ;;   :desc "column" "c" #'markdown-table-delete-column
        ;;   :desc "row" "r" #'markdown-table-delete-row)
        ;;  (:prefix ("i" . "Insert")
        ;;   :desc "Column" "c" #'markdown-table-insert-column
        ;;   :desc "Row" "r" #'markdown-table-insert-row))

        ;; (:prefix ("T" . "toggle")
        ;;  :desc "Inline LaTeX"      "e" #'markdown-toggle-math
        ;;  :desc "Code highlights"   "f" #'markdown-toggle-fontify-code-blocks-natively
        ;;  :desc "Inline images"     "i" #'markdown-toggle-inline-images
        ;;  :desc "URL hiding"        "l" #'markdown-toggle-url-hiding
        ;;  :desc "Markup hiding"     "m" #'markdown-toggle-markup-hiding
        ;;  :desc "Wiki links"        "w" #'markdown-toggle-wiki-links
        ;;  :desc "GFM checkbox"      "x" #'markdown-toggle-gfm-checkbox)
        )
  )

;;;; eww-mode-map

(with-eval-after-load 'eww
  (with-eval-after-load "org" (require 'ol-eww nil t))

  (map! :map eww-mode-map
        "C-c C-o" #'eww-browse-with-external-browser
        :n "C-j" (cmd! () (pixel-scroll-precision-scroll-down 50))
        :n "C-k" (cmd! () (pixel-scroll-precision-scroll-up 50))
        :n "j" #'evil-next-visual-line
        :n "k" #'evil-previous-visual-line
        :n "q" #'kill-buffer-and-window

        (:localleader
         :desc "zoom" "z" #'eww-zoom-transient
         :desc "open-in-other-window" "e" #'+eww/open-in-other-window
         :desc "external browser" "E" #'eww-browse-with-external-browser
         "r" 'eww-readable
         "R" 'eww-reload
         "p" 'eww-previous-url
         "n" 'eww-next-url
         "h" 'eww-list-histories
         "d" 'eww-download
         "a" 'eww-add-bookmark
         "s" 'ace-link-eww
         ;; "c" 'eww-copy-page-url
         (:prefix ("v" . "view")
                  "x" 'eww-browse-with-external-browser
                  "f" 'eww-toggle-fonts
                  "r" 'eww-readable
                  "s" 'eww-view-source)
         (:prefix ("l" . "list")
                  "b" 'eww-list-buffers
                  "o" 'eww-list-bookmarks)
         ))
  )

;;;; TODO clojure-mode-map

;; Lookup functions in Clojure - The Essentail Reference book
;; https://github.com/p3r7/clojure-essential-ref

;; TODO: review evaluation key bindings from Spacemacs
(map! :after cider
      :map clojure-mode-map
      :localleader
      :desc "REPL session" "'" #'sesman-start

      (:prefix ("h" . "help")
               "r" #'clojure-essential-ref))

;;       ;; Debug Clojure
;;       (:prefix ("d" . "debug/inspect")
;;        :desc "debug" "d" #'cider-debug-defun-at-point
;;        (:prefix ("i" . "inspect")
;;         :desc "last expression" "e" #'cider-inspect-last-sexp
;;         :desc "expression" "f" #'cider-inspect-defun-at-point
;;         :desc "inspector" "i" #'cider-inspect
;;         :desc "last result" "l" #'cider-inspect-last-result
;;         (:prefix ("p" . "portal")
;;          :desc "Clear" "c" #'portal.api/open
;;          :desc "Clear" "D" #'portal.api/close
;;          :desc "Open" "p" #'portal.api/open)
;;         :desc "value" "v" #'cider-inspect-expr))

;;       ;; Evaluation
;;       (:prefix "e"
;;        :desc "Expression to comment" ";" #'cider-eval-defun-to-comment
;;        ;; :desc "" "e$" #'spacemacs/cider-eval-sexp-end-of-line
;;        :desc "at point" "(" #'cider-eval-list-at-point
;;        :desc "buffer" "b" #'cider-eval-buffer
;;        "D" nil  ; Doom: send to repl
;;        :desc "prev expression" "e" #'cider-eval-last-sexp
;;        :desc "expresion" "f" #'cider-eval-defun-at-point
;;        :desc "interupt" "i" #'cider-interrupt
;;        ;; :desc "" "el" #'spacemacs/cider-eval-sexp-end-of-line
;;        :desc "macroexpand" "m" #'cider-macroexpand-1
;;        :desc "macroexpand all" "M" #'cider-macroexpand-all
;;        :desc "region" "r" #'cider-eval-region
;;        :desc "undefine" "u" #'cider-undef
;;        :desc "undefine" "U" #'cider-undef-all
;;        :desc "expresion at point" "v" #'cider-eval-sexp-at-point
;;        :desc "expresion upto point" "V" #'cider-eval-sexp-up-to-point
;;        :desc "replace with result" "w" #'cider-eval-last-sexp-and-replace)

;;       ;; Format Clojure
;;       (:prefix ("=" . "format")
;;        :desc "buffer" "=" #'cider-format-buffer
;;        :desc "region" "r" #'cider-format-region
;;        :desc "expression" "f" #'cider-format-defun
;;        (:prefix ("e" . "edn")
;;         :desc "expression" "b" #'cider-format-edn-buffer
;;         :desc "prev expression" "e" #'cider-format-edn-last-sexp
;;         :desc "expression" "r" #'cider-format-edn-region))

;;       ;; Goto / jump
;;       (:prefix ("g" . "goto/jump")
;;        :desc "pop back" "b" #'cider-pop-back
;;        :desc "classpath" "c" #'cider-classpath
;;        ;; :desc "Find var" "c" #'spacemacs/clj-find-var
;;        :desc "find ns" "n" #'cider-find-ns
;;        :desc "error" "e" #'cider-jump-to-compilation-error
;;        :desc "resource" "r" #'cider-find-resource
;;        :desc "spec" "s" #'cider-browse-spec
;;        :desc "spec All" "S" #'cider-browse-spec-all)

;;       ;; Help & Documentation
;;       (:prefix ("h" . "help")
;;        :desc "apropos" "a" #'cider-apropos
;;        :desc "cheetsheet" "c" #'cider-cheatsheet
;;        :desc "clojure docs" "d" #'cider-clojuredocs
;;        :desc "javadoc" "j" #'cider-javadoc
;;        :desc "browse ns" "n" #'cider-browse-ns
;;        :desc "browse all ns" "N" #'cider-browse-ns-all
;;        :desc "browse spec" "s" #'cider-browse-spec
;;        :desc "browse all spe" "S" #'cider-browse-spec-all)

;;       ;; Evaluation - Namespaces
;;       (:prefix ("n" . "namespace")
;;        :desc "reload all" "a" #'cider-ns-reload-all
;;        :desc "" "n" #'cider-eval-ns-form
;;        :desc "" "r" #'cider-ns-refresh
;;        :desc "" "l" #'cider-ns-reload
;;        :desc "" "L" #'cider-ns-reload-all)

;;       ;; Evaluation - Pretty print
;;       (:prefix ("n" . "Pretty print")
;;        :desc "Expression comment" ";" #'cider-pprint-eval-defun-to-comment
;;        :desc "Preceeding expresion comment" ":" #'cider-pprint-eval-last-sexp-to-comment
;;        :desc "Expression" "f" #'cider-pprint-eval-defun-at-point
;;        :desc "Preceeding Expression" "e" #'cider-pprint-eval-last-sexp)

;;       ;; Refactor - Doom clj-refactor hydra menu
;;       (:prefix-map ("R" . nil))

;;       ;; REPL Sesison management
;;       (:prefix ("s" . "REPL Session")
;;        ;; :desc "toggle buffer" "a" (if (eq m 'cider-repl-mode) 'cider-switch-to-last-clojure-buffer 'cider-switch-to-repl-buffer)
;;        :desc "Browse Session" "b" #'sesman-browser
;;        :desc "Goto Session" "g" #'sesman-goto
;;        :desc "Session Info" "i" #'sesman-info
;;        :desc "quit" "q" #'sesman-quit
;;        :desc "quit session" "Q" #'sesman-quit-session
;;        :desc "restart" "r" #'sesman-restart
;;        :desc "start Session" "s" #'sesman-start

;;        (:prefix ("l" . "Link Sessions")
;;         :desc "buffer" "b" #'sesman-link-with-buffer
;;         :desc "directory" "d" #'sesman-link-with-directory
;;         :desc "project" "p" #'sesman-link-with-project
;;         :desc "project" "s" #'cider-connect-sibling-clj
;;         :desc "project" "S" #'cider-connect-sibling-cljs
;;         :desc "unlink" "u" #'sesman-unlink))

;;       ;; Testing
;;       (:prefix ("t" . "Testing")
;;        :desc "loaded" "l" #'cider-test-run-loaded-tests
;;        :desc "namespace" "n" #'cider-test-run-ns-tests
;;        :desc "project" "p" #'cider-test-run-project-tests
;;        :desc "filters" "s" #'cider-test-run-ns-tests-with-filters
;;        :desc "show report" "S" #'cider-test-show-report
;;        :desc "filters" "r" #'cider-test-rerun-failed-tests
;;        :desc "filters" "R" #'cider-test-rerun-test
;;        :desc "test" "t" #'cider-test-run-test)

;;       (:prefix ("T" . "Toggle")
;;        :desc "auto-test" "a" #'cider-auto-test-mode
;;        :desc "enlightenment" "e" #'cider-enlighten-mode
;;        :desc "namespace" "n" #'cider-test-run-ns-tests
;;        :desc "project" "p" #'cider-test-run-project-tests
;;        :desc "filters" "s" #'cider-test-run-ns-tests-with-filters
;;        :desc "show report" "S" #'cider-test-show-report
;;        :desc "filters" "r" #'cider-test-rerun-failed-tests
;;        :desc "filters" "R" #'cider-test-rerun-test
;;        :desc "test" "t" #'cider-test-run-test))

;; Kaocha test runner from CIDER - Requires running REPL
;; next prefix expressions for key sequence, i.e. `SPC t k'
;; (map! :after kaocha-runner
;;       :map clojure-mode-map
;;       :localleader
;;       (:prefix "t"
;;                (:prefix ("k". "Kaocha")
;;                 :desc "Run current test" "t" #'kaocha-runner-run-test-at-point
;;                 :desc "Run test" "r" #'kaocha-runner-run-tests
;;                 :desc "Run all tests" "a" #'kaocha-runner-run-all-tests
;;                 :desc "Runner Warnings" "w" #'kaocha-runner-show-warnings
;;                 :desc "Kaocha Runner" "h" #'kaocha-runner-hide-windows)))

;;;; global map

(map!
 (:after
  consult
  ;; C-c bindings (mode-specific-map
  "C-c m" #'consult-mode-command
  "C-c b" #'consult-bookmark
  "C-c k" #'consult-kmacro
  ;; C-x bindings (ctl-x-map
  "C-x M-:" #'consult-complex-command ;; orig#'repeat-complex-command
  "C-x b" #'consult-buffer ;; orig#'switch-to-buffer
  "C-x 4 b" #'consult-buffer-other-window ;; orig#'switch-to-buffer-other-window
  "C-x 5 b" #'consult-buffer-other-frame ;; orig#'switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  "M-#" #'consult-register-load
  "M-'" #'consult-register-store ;; orig#'abbrev-prefix-mark (unrelated
  ;; ("C-M-#" #'consult-register ; ugly
  "M-`" #'consult-register ; default tmm-menubar
  ;; Other custom bindings
  "M-y" #'consult-yank-pop ;; orig#'yank-pop
  ;; M-g bindings (goto-map
  "M-g E" #'consult-compile-error
  "M-g f" #'consult-flymake ;; Alternative: consult-flycheck
  "M-g g" #'consult-goto-line ;; orig#'goto-line
  ;; ("M-g M-g" #'consult-goto-line           ;; orig#'goto-line
  "M-g o" #'consult-outline ;; Alternative: consult-org-heading
  "M-g m" #'consult-mark
  "M-g k" #'consult-global-mark
  "M-g i" #'consult-imenu
  "M-g I" #'consult-imenu-multi
  ;; M-s bindings (search-map
  "M-s b" #'consult-buffer
  "M-s f" #'consult-find
  "M-s F" #'my/consult-fd
  "M-s L" #'consult-locate
  ;; "M-s g" #'consult-grep
  "M-s G" #'consult-git-grep
  "M-s K" #'consult-git-log-grep
  ;; "M-s r" #'consult-ripgrep
  "M-s l" #'consult-line
  "M-s m" #'consult-line-multi
  "M-s k" #'consult-keep-lines
  "M-s u" #'consult-focus-lines
  ;; Isearch integration
  "M-s e" #'consult-isearch-history
  ;; :map minibuffer-local-map ("M-r" #''consult-history ; doom's default C-s
  ;; :map read-expression-map ("M-r" #''consult-history
  (:map isearch-mode-map
        "M-e" #'consult-isearch-history ;; orig#'isearch-edit-string
        "M-s e" #'consult-isearch-history ;; orig#'isearch-edit-string
        "M-s l" #'consult-line))
 )

;;;; smartparens-mode-map

;; Doom's Default - /modules/config/default/+emacs-bindings.el
(map!
 (:after smartparens
  :map smartparens-mode-map
  "C-M-a"           #'sp-beginning-of-sexp
  "C-M-e"           #'sp-end-of-sexp
  "C-M-f"           #'sp-forward-sexp
  "C-M-b"           #'sp-backward-sexp
  "C-M-n"           #'sp-next-sexp
  "C-M-p"           #'sp-previous-sexp
  "C-M-u"           #'sp-up-sexp
  "C-M-d"           #'sp-down-sexp
  "C-M-k"           #'sp-kill-sexp
  "C-M-t"           #'sp-transpose-sexp
  "C-M-<backspace>" #'sp-splice-sexp

  "C-<right>" #'sp-forward-slurp-sexp
  "C-<left>" #'sp-forward-barf-sexp
  "M-<left>" #'sp-backward-slurp-sexp
  "M-<right>" #'sp-backward-barf-sexp

  "M-<up>"  #'sp-splice-sexp-killing-backward
  "M-<down>" #'sp-splice-sexp-killing-forward

  "C-c (" #'sp-wrap-round
  ;; "C-c [" #'sp-wrap-square ; conflict org-mode-map
  ;; "C-c {" #'sp-wrap-curly
  ))

;; ;;;###autoload
;; (defun bk/improve-last-parens ()
;;   (interactive)
;;   (evil-normal-state)
;;   (evil-append-line 1))

;; (map! :i "C-j" #'bk/improve-last-parens
;;       :i "C-l" #'sp-forward-sexp)

;;;; treemacs - f9

(map!
 (:when (modulep! :ui treemacs)
   "<f9>"   #'+treemacs/toggle
   "<C-f9>" #'treemacs-find-file
   "<M-f9>" #'treemacs-select-window
   ))

;;;; dired-mode-map

(map! :map dired-mode-map
      :localleader
      "h" #'dired-omit-mode
      "H" #'dired-hide-details-mode
      "p" #'dired-preview-mode
      :desc "sort-modified-date" "o" #'dired-sort-toggle-or-edit

      "m" #'my/dired-attach-to-mastodon

      :desc "*denote-insert* marked-notes" "i" #'my/denote-link-dired-marked-notes
      "g" #'prot-dired-grep-marked-files
      "l" #'prot-dired-limit-regexp

      :desc "*denote-rename* files" "r" #'denote-dired-rename-files
      :desc "*denote-rename* using front-matter" "R" #'denote-dired-rename-marked-files-using-front-matter
      :desc "*denote-rename* with keywords" "w" #'denote-dired-rename-marked-files-with-keywords
      :desc "*denote-rename* add keywords" "k" #'denote-dired-rename-marked-files-add-keywords
      :desc "*denote-rename* remove keywords" "K" #'denote-dired-rename-marked-files-remove-keywords

      :desc "*casual-dired* menu" ";" #'casual-dired-tmenu
      "-" #'nerd-icons-dired-mode

      :desc "denote-map" "n" ews-denote-map
      ;; (:prefix ("y" . "copy")
      ;;          )
      )

;;;; python-mode-map

(map! :after python
      :localleader
      :map python-mode-map
      (:prefix ("t" . "test")
               "a" #'python-pytest
               "f" #'python-pytest-file-dwim
               "F" #'python-pytest-file
               "t" #'python-pytest-function-dwim
               "T" #'python-pytest-function
               "r" #'python-pytest-repeat
               "p" #'python-pytest-dispatch)
      (:prefix ("h" . "help")
               "l" #'pylookup-lookup
               "h" #'pylookup-lookup-at-point)
      (:prefix ("e" . "pipenv")
       :desc "activate"    "a" #'pipenv-activate
       :desc "deactivate"  "d" #'pipenv-deactivate
       :desc "install"     "i" #'pipenv-install
       :desc "lock"        "l" #'pipenv-lock
       :desc "open module" "o" #'pipenv-open
       :desc "run"         "r" #'pipenv-run
       :desc "shell"       "s" #'pipenv-shell
       :desc "uninstall"   "u" #'pipenv-uninstall)
      (:prefix ("i" . "imports")
       :desc "Insert missing imports" "i" #'pyimport-insert-missing
       :desc "Remove unused imports"  "R" #'pyimport-remove-unused
       :desc "Optimize imports"       "o" #'+python/optimize-imports)
      ;; (:prefix ("g" . "conda")
      ;;          "a" #'conda-env-activate
      ;;          "d" #'conda-env-deactivate
      ;;          "l" #'conda-env-list
      ;;          "t" #'conda-env-autoactivate-mode)
      )

;;;; csv-mode-map

(map! :after csv-mode
      :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "d" #'csv-kill-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose
      "h"  #'csv-header-line
      "i"  #'csv-toggle-invisibility
      "n"  #'csv-forward-field
      "p"  #'csv-backward-field
      "r"  #'csv-reverse-region
      "y" #'csv-yank-fields
      "Y" #'csv-yank-as-new-table
      )

;;;; Info-mode-map

(map! :map Info-mode-map
      :after info
      :n "M-," #'Info-history-back
      :n "M-." #'Info-history-forward
      :n "^" #'Info-up
      :n "C-n" #'Info-forward-node
      :n "C-p" #'Info-backward-node
      :n ">" #'Info-next
      :n "<" #'Info-prev
      :n "]" #'Info-next-reference
      :n "[" #'Info-prev-reference
      :n "H" #'Info-top-node
      :n "~" #'Info-directory [remap consult-imenu] #'Info-toc)

;;;; chatgpt-shell

(map!
 :map chatgpt-shell-mode-map
 :i "RET"
 #'+default/newline
 :i
 "M-<return>" #'shell-maker-submit
 :i "M-RET"
 #'shell-maker-submit
 :i
 "M-." #'dictionary-lookup-definition
 :i "C-c C-l"
 #'chatgpt-shell-clear-buffer
 (:localleader
  "p"
  #'chatgpt-shell-swap-system-prompt
  "m"
  #'chatgpt-shell-swap-model-version)
 :map comint-mode-map
 "C-c C-l" #'comint-clear-buffer)

;;;; osm-mode-map

(map! :map osm-mode-map
      :n
      ;; Navigation
      "h" #'osm-left
      "l" #'osm-right
      "j" #'osm-down
      "k" #'osm-up
      "H" #'osm-left-left
      "L" #'osm-right-right
      "J" #'osm-down-down
      "K" #'osm-up-up
      ;; Positioning
      "+" #'osm-zoom-in
      "-" #'osm-zoom-out
      "c" #'osm-center
      "g"  #'osm-home
      "r" #'revert-buffer
      ;; Url
      "u" #'osm-save-url
      "y" #'org-store-link
      ;; Other
      "s" #'osm-search
      "X" #'osm-gpx-hide
      "q" #'quit-window)

;;;; cdlatex

; use cdlatex completion instead of yasnippet
(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)

(map! :after latex
      :map cdlatex-mode-map
      :localleader
      :desc "Insert math symbol"
      "i" #'cdlatex-math-symbol
      :desc "Begin environment"
      "e" #'cdlatex-environment)

;;;; anddo

(after! anddo
  (map! :map anddo-mode-map
        :n "n"   'anddo-new-item
        :n "e"   'anddo-edit-item
        :n "s"   'anddo-change-status
        :n "r"   'anddo-toggle-listing-mode
        :n "D"   'anddo-delete-item
        :n "l"   'anddo-show-body
        :n "<RET>" 'anddo-show-body
        :n "q" 'casual-anddo-tmenu
        :n "Q" 'kill-buffer-and-window
        :localleader
        "n"   'anddo-new-item
        "e"   'anddo-edit-item
        "s"   'anddo-change-status
        "r"   'anddo-toggle-listing-mode
        "D"   'anddo-delete-item
        "l"   'anddo-show-body
        "<RET>" 'anddo-show-body))

;;;; citar-denote embark-citation-map

(map!
 :after (embark citar-denote)
 (:map citar-embark-citation-map
       "1" #'citar-denote-find-citation ; really useful
       ;; "2" #'citar-denote-open-note
       ))


;;;; vertico-map

(map! :map vertico-map
      ;; "C-'" #'vertico-quick-insert
      ;; "C-h" #'vertico-directory-delete-word
      ;; "C-c C-g" #'vertico-grid-mode
      ;; "M-h" #'vertico-grid-left
      ;; "M-l" #'vertico-grid-right

      "M-j" #'vertico-next
      "M-k" #'vertico-previous
      "M-v" #'toggle-input-method
      "M-g" #'toggle-input-method
      "`"   #'toggle-input-method
      "M-8" #'tempel-insert
      "M-*" #'tempel-insert
      ;; "M-S-j" #'vertico-scroll-up
      ;; "M-S-k" #'vertico-scroll-down

      ;; "C-e" #'vertico-scroll-up
      ;; "C-y" #'vertico-scroll-down
      ;; "]" #'vertico-next-group
      ;; "[" #'vertico-previous-group
      ;; "~" #'vertico-jump-to-home-dir-on~
      ;; "C-/" #'vertico-jump-root
      ;; "C-?" #'vertico-jump-sudo
      ;; "M-m" #'embark-select
      ;; "C-S-SPC" #'embark-preview+
      )

;;;; gptel - fix transient menu

(after! gptel
  (transient-append-suffix 'gptel-menu "k"
    '("q" "quit" transient-quit-one))

  ;; Doom binds ~RET~ in Org mode to =+org/dwim-at-point=, which appears to conflict with gptel's transient menu bindings for some reason.
  ;; Two solutions:
  ;; - Press ~C-m~ instead of the return key. evil-ret
  ;; - Change the send key from return to a key of your choice:
  (transient-suffix-put 'gptel-menu (kbd "RET") :key "M-RET")
  )


;;; TODO ctl-x maps

;; /home/junghan/sync/man/dotsamples/doom/yqdotfiles-dot-doom-clj/.doom.d/map.el
;; (map!
;;  (:map ctl-x-map
;;        "8" 'ctl-x-8-map
;;        ;; "9" 'ctl-x-9-map
;;        )
;;  )

;;; end-of-func
