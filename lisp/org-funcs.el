;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghanacs
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

;;; fixing files at once

;; 매우 중요한 개념이다.
;; 인덴트, 이름, 포멧, 형식 등 한번에 일괄 수정하는 방법

;;;; Indentation

;; https://stackoverflow.com/questions/2551632/how-to-format-all-files-under-a-dir-in-emacs

;; Next, open a Dired buffer at the top level of the directory under which you
;; want to change all of the files. Give the dired command a numeric prefix so
;; that it will ask for the switches to give to the ls command, and add the R
;; (recurse) switch: C-u C-x d R RET your-directory RET. 그런 다음, 모든 파일을
;; 변경하려는 디렉터리의 최상위 수준에서 Dired 버퍼를 엽니다. dired 명령에 숫자
;; 접두사를 지정하여 ls 명령에 전달할 스위치를 요청하고 R 을 추가합니다. (재귀)
;; 스위치를 추가합니다: C-u C-x d R RET your-directory RET .

;; Next, mark all of the regular files in the recursive directory listing: first
;; * / to mark all the directories, then * t to toggle the selection. 그런 다음
;; 재귀적 디렉터리 목록에 있는 모든 일반 파일을 표시합니다. 먼저 * / 을 눌러
;; 모든 디렉터리를 표시한 다음 * t 을 눌러 선택 항목을 전환합니다.

;; Finally, run the above command: M-x indent-marked-files.

;; Be aware that if you already have any buffers visiting any of the target
;; files, they'll be killed by indent-marked-files. Also be aware that none of
;; the file changes will be undoable; use with caution! I tested it in a simple
;; case and it seems to work as described, but I make no guarantees. 대상 파일을
;; 방문하는 버퍼가 이미 있는 경우 indent-marked-files 에 의해 버퍼가 종료된다는
;; 점에 유의하세요. . 또한 어떤 파일 변경도 되돌릴 수 없으므로 주의해서
;; 사용하세요! 간단한 케이스에서 테스트해 본 결과 설명대로 작동하는 것 같지만,
;; 보장할 수는 없습니다.

(defun my/indent-marked-files ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;; Usage :: '~/.spacemacs.d/layers/' and then '.el'
;; https://stackoverflow.com/questions/2551632/how-to-format-all-files-under-a-dir-in-emacs
(defun my/indent-files (directory extension)
  (interactive (list (read-directory-name "Directory: ")
                     (read-string "File extension: ")))
  (dolist (file (directory-files-recursively directory extension))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;;; my/org-store-link-id-optional for org-transclusion

;; agzam-dot-spacemacs/layers/ag-org/funcs.el
;;;###autoload
(defun my/org-store-link-id-optional (&optional arg)
  "Stores a link, reversing the value of `org-id-link-to-org-use-id'.
If it's globally set to create the ID property, then it wouldn't,
and if it is set to nil, then it would forcefully create the ID."
  (interactive "P")
  (let ((org-id-link-to-org-use-id (not org-id-link-to-org-use-id)))
    (org-store-link arg :interactive)))

;;; org-rich-yank : format paste

;; https://github.com/unhammer/org-rich-yank If you want to change how the
;; source block or link is formatted, you can do so by setting
;; org-rich-yank-format-paste to a function. For example, links to local files
;; might be useful in your org document but not so useful in exported content,
;; so you may want to make such a link a comment line.

;; 소스 블록 또는 링크의 서식을 변경하려면 org-rich-yank-format-paste 을 함수로
;; 설정하여 변경할 수 있습니다. 예를 들어 로컬 파일에 대한 링크는 조직
;; 문서에서는 유용하지만 내보낸 콘텐츠에서는 유용하지 않을 수 있으므로 이러한
;; 링크를 주석 줄로 만들 수 있습니다.

(when (locate-library "org-rich-yank")
  (defun my/org-rich-yank-format-paste (language contents link)
    "Based on `org-rich-yank--format-paste-default'."
    (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n#+comment: %s"
            language
            (org-rich-yank--trim-nl contents)
            link))
  (customize-set-variable 'org-rich-yank-format-paste #'my/org-rich-yank-format-paste))

;;; doom : cae functions

;; ;;;###autoload
;; (defun cae-org-rich-yank ()
;;   (interactive)
;;   (require 'org-rich-yank)
;;   (let* ((source-mode
;;           (or (and (buffer-live-p org-rich-yank--buffer)
;;                    (let ((mode (buffer-local-value 'major-mode org-rich-yank--buffer)))
;;                      (parent-mode-is-derived-p
;;                       (buffer-local-value 'major-mode org-rich-yank--buffer)
;;                       'prog-mode))
;;                    (buffer-local-value 'major-mode org-rich-yank--buffer))
;;               (pcase (language-detection-string (current-kill 0))
;;                 ('ada 'ada-mode) ('c 'c-mode) ('cpp 'c++-mode)
;;                 ('clojure 'clojure-mode) ('csharp 'csharp-mode)
;;                 ('css 'css-mode) ('dart 'dart-mode)
;;                 ('delphi 'delphi-mode) ('emacslisp 'emacs-lisp-mode)
;;                 ('erlang 'erlang-mode) ('fortran 'fortran-mode)
;;                 ('fsharp 'fsharp-mode) ('go 'go-mode)
;;                 ('groovy 'groovy-mode) ('haskell 'haskell-mode)
;;                 ('html 'html-mode) ('java 'java-mode)
;;                 ('javascript 'javascript-mode) ('json 'js-json-mode)
;;                 ('latex 'latex-mode) ('lisp 'lisp-mode)
;;                 ('lua 'lua-mode) ('matlab 'matlab-mode)
;;                 ('objc 'objc-mode) ('perl 'perl-mode)
;;                 ('php 'php-mode) ('prolog 'prolog-mode)
;;                 ('python 'python-mode) ('r 'r-mode)
;;                 ('ruby 'ruby-mode) ('rust 'rust-mode)
;;                 ('scala 'scala-mode) ('shell 'shell-script-mode)
;;                 ('smalltalk 'smalltalk-mode) ('sql 'sql-mode)
;;                 ('swift 'swift-mode) ('visualbasic 'visual-basic-mode)
;;                 ('xml 'nxml-mode)
;;                 (_ 'text-mode))))
;;          (paste
;;           (concat
;;            (format "#+begin_src %s\n"
;;                    (replace-regexp-in-string
;;                     "-mode$" ""
;;                     (symbol-name source-mode)))
;;            (thread-last (current-kill 0)
;;                         (org-rich-yank--trim-nl)
;;                         (funcall (lambda (s)
;;                                    (with-temp-buffer
;;                                      (insert s)
;;                                      (funcall source-mode)
;;                                      (indent-region (point-min) (point-max))
;;                                      (buffer-substring-no-properties (point-min) (point-max))))))
;;            (format "\n#+end_src\n"))))
;;     (insert paste)))

;;;###autoload
(defun cae-org-insert-checkbox-or-bracket (arg)
  (interactive "p")
  (if (and (= arg 1)
           (ignore-errors
             (<= (point)
                 (save-excursion
                   (beginning-of-line)
                   (re-search-forward
                    (rx bol
                        (or (+ "*") "-")
                        (* whitespace)
                        (? "[" (group any) "]")
                        (* whitespace))
                    (pos-eol))
                   (point)))))
      (progn (if (match-string 1)
                 (delete-region (1- (match-beginning 1))
                                (progn (goto-char (1+ (match-end 1)))
                                       (skip-chars-forward "\s\t")
                                       (point)))
               (insert "[ ] "))
             (goto-char (pos-eol)))
    (org-self-insert-command arg)))


;;;###autoload
(defun cae-org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))

;;;###autoload
(defun cae-org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (s-matches-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
      (message "Replaced %d occurances" count))))

;;; eliies functions
;;;; my/genfile-timestamp

;; 중국어 문장 부호를 인식하도록 문장 끝을 설정합니다. 채우기에서 마침표 뒤에 두
;; 개의 공백을 삽입할 필요가 없습니다.
;; (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; generate timestamp such as 2016_1031_ for file name
(defun my/genfile-timestamp()
  (concat (format-time-string "%Y%m%d")
          (char-to-string (+ 65 (random 26)))
          (char-to-string (+ 65 (random 26)))
          "_"))

;; self define functions
;; (defun my/imenu-default-goto-function-advice (orig-fun &rest args)
;;   (apply orig-fun args)
;;   (recenter))

;; https://www.reddit.com/r/emacs/comments/yjobc2/comment/iur16c7/
(defun my/parse-headline (x)
  (plist-get (cadr x) :raw-value))

(defun my/get-headlines ()
  (org-element-map (org-element-parse-buffer) 'headline #'my/parse-headline))

(defun my/link-to-headline ()
  "Insert an internal link to a headline."
  (interactive)
  (let* ((headlines (my/get-headlines))
         (choice (completing-read "Headings: " headlines nil t))
         (desc (read-string "Description: " choice)))
    (org-insert-link buffer-file-name (concat "*" choice) desc)))

(defun my/get-file-line ()
  "Show (and set kill-ring) current file and line"
  (interactive)
  (unless (buffer-file-name)
    (error "No file for buffer %s" (buffer-name)))
  (let ((msg (format "%s::%d"
                     (file-truename (buffer-file-name))
                     (line-number-at-pos))))
    (kill-new msg)
    (message msg)))

(defun my/get-file-link ()
  "Show (and set kill-ring) current file"
  (interactive)
  (unless (buffer-file-name)
    (error "No file for buffer %s" (buffer-name)))
  (let ((msg (format "file:\\\\%s"
                     (replace-regexp-in-string "/" "\\\\"
                                               (file-truename (buffer-file-name))))))
    (kill-new msg)
    (message msg)))

(defun my/encode-buffer-to-utf8 ()
  "Sets the buffer-file-coding-system to UTF8."
  (interactive)
  (set-buffer-file-coding-system 'utf-8 nil))

(defun my-blink(begin end)
  "blink a region. used for copy and delete"
  (interactive)
  (let* ((rh (make-overlay begin end)))
    (progn
      (overlay-put rh 'face '(:background "DodgerBlue" :foreground "White"))
      (sit-for 0.2 t)
      (delete-overlay rh)
      )))

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring. Remove leading and
    trailing whitespace while we're at it. Also, remove whitespace before
    column, if any. Also, font-lock will be removed, if any. Also, the
    copied region will be highlighted shortly (it 'blinks')."
  (save-excursion
    (let* ((beg (get-point begin-of-thing 1))
           (end (get-point end-of-thing arg)))
      (progn
        (copy-region-as-kill beg end)
        (with-temp-buffer
          (yank)
          (goto-char 1)
          (while (looking-at "[ \t\n\r]")
            (delete-char 1))
          (delete-trailing-whitespace)
          (delete-whitespace-rectangle (point-min) (point-max)) ;; del column \s, hehe
          (font-lock-unfontify-buffer) ;; reset font lock
          (kill-region (point-min) (point-max))
          )
        ))))

(defun my/copy-word (&optional arg)
  "Copy word at point into kill-ring"
  (interactive "P")
  (my-blink (get-point 'backward-word 1) (get-point 'forward-word 1))
  (copy-thing 'backward-word 'forward-word arg)
  (message "word at point copied"))

(defun my/copy-line (&optional arg)
  "Copy line at point into kill-ring, truncated"
  (interactive "P")
  (my-blink (get-point 'beginning-of-line 1) (get-point 'end-of-line 1))
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (message "line at point copied"))

(defun my/copy-paragraph (&optional arg)
  "Copy paragraph at point into kill-ring, truncated"
  (interactive "P")
  (my-blink (get-point 'backward-paragraph 1) (get-point 'forward-paragraph 1))
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  (message "paragraph at point copied"))

(defun my/copy-buffer(&optional arg)
  "Copy the whole buffer into kill-ring, as-is"
  (interactive "P")
  (progn
    (my-blink (point-min) (point-max))
    (copy-region-as-kill (point-min) (point-max))
    (message "buffer copied")))

;;;; my/rename-file-and-buffer

;; copy from http://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; many thanks to Bozhidar Batsov (https://github.com/bbatsov)
(defun my/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting. Binded to
  key C-c r"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)
(setq grep-find-command
      "find . -type f -not -name \"*.svn-base\" -and -not -name \"*#\" -and -not -name \"*.tmp\" -and -not -name \"*.obj\" -and -not -name \"*.386\" -and -not -name \"*.img\" -and -not -name \"*.LNK\" -and -not -name GTAGS -print0 | xargs -0 grep -n -e ")

(defun my/grep-find()
  (interactive)
  (grep-find (concat grep-find-command (buffer-substring-no-properties (region-beginning) (region-end)))))

;;;; my/open-external

(defun my/open-external (&optional file)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
copy from xah lee: http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html"
  (interactive)
  (let (doit
        (flist
         (cond
          ((or (string-equal major-mode "dired-mode")
               (string-equal major-mode "sr-mode"))
           (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))

    (setq doit (if (<= (length flist) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ")))
    (when doit
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (path) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)) ) flist))
       ((string-equal system-type "darwin")
        (mapc (lambda (path) (shell-command (format "open \"%s\"" path)))  flist))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path)) ) flist))
       ((string-equal system-type "cygwin")
        (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path)) ) flist))))))

;;;; my/open-external-pdf

(defun my/open-external-pdf ()
  (interactive)
  (my/open-external
   (concat
    (file-name-sans-extension (buffer-file-name))
    ".pdf")))

;;; gr-functions
;;;; DONT Dily Notes with org-journal

;;;###autoload
;; (defun gr/daily-notes (p)
;;   "Pop up dailynotes.org."
;;   (interactive "P")
;;   (let ((buffer (find-file-noselect
;;                  (concat org-directory "/dailynotes.org"))))
;;     (cond ((equal p '(4))
;;            (select-frame (make-frame-command))
;;            (find-file (concat org-directory "/dailynotes.org"))
;;            (set-frame-position (selected-frame) 845 20)
;;            (delete-other-windows))
;;           ((eq (current-buffer) buffer)
;;            nil)
;;           (t (pop-to-buffer-same-window buffer)))
;;     (gr/daily-notes-new-headline)))

;; (defun gr/daily-notes-new-headline ()
;;   (interactive)
;;   (org-cycle-set-startup-visibility)
;;   (let ((date (concat "** " (format-time-string "%Y-%m-%d %A")))
;;         (month (concat "* " (format-time-string "%B %Y")))
;;         (last-month (format-time-string "%B"
;;                                         (time-subtract
;;                                          (current-time)
;;                                          (days-to-time 30)))))
;;     (goto-char (point-min))
;;     (unless (re-search-forward month nil t)
;;       (re-search-forward (concat "* " last-month))
;;       (forward-line 1)
;;       (kill-whole-line)
;;       (forward-line -1)
;;       (org-cycle)
;;       (insert month "\n\n")
;;       (forward-line -2)
;;       (org-set-property "VISIBILITY" "all"))
;;     (unless (re-search-forward date nil t)
;;       (forward-line 4)
;;       (insert "\n" date "\n- |\n")
;;       (search-backward "|")
;;       (delete-char 1))))

;; (defun gr/org-journal-new-entry ()
;;   (interactive)
;;   (select-frame (make-frame-command))
;;   (delete-other-windows)
;;   (org-journal-new-entry nil nil))

;; (defun gr/org-journal-new-entry ()
;;   (interactive)
;;   (split-window-below)
;;   (windmove-down)
;;   (org-journal-new-entry nil nil)
;;   (goto-char (point-max)))

;;;; word count

(defun gr/word-count-subtree ()
  "Count words in org subtree at point."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let ((wc (org-word-count-aux (point-min) (point-max))))
      (kill-new (format "%d" wc))
      (message (format "%d words in subtree." wc)))))


(defun gr/lookup-word-at-point ()
  "Lookup word at point in OSX Dictionary."
  (interactive)
  (call-process-shell-command (format "open dict:///%s/" (word-at-point))))

;; (defun gr/open-mu4e ()
;;   (interactive)
;;   (select-frame (make-frame))
;;   (set-frame-size (selected-frame) 125 45)
;;   (mu4e)
;;   (delete-other-windows))
;;   (display-buffer-full-frame " *mu4e-main*" '(nil)))
;;   ;; (switch-to-buffer " *mu4e-main*")
;;   (pop-to-buffer-same-window " *mu4e-main*")
;;   (delete-other-windows))
;; ;; use display-buffer-full-frame ?

(defun gr/open-fragments-file ()
  (interactive)
  (find-file "~/Dropbox/org/fragments.org"))

;; (defun gr/open-fragments-file-other-frame ()
;;   (interactive)
;;   (find-file-other-frame "~/Dropbox/org/fragments.org"))

;; (defun switch-to-minibuffer-window ()
;;   "Switch to minibuffer window (if active)."
;;   (interactive)
;;   (when (active-minibuffer-window)
;;     (select-window (active-minibuffer-window))))

;; load ~/.emacs.d/init.el
;; (defun refresh-emacs ()
;;   (interactive)
;;   (load "~/.emacs.d/init.el"))

;; (global-set-key (kbd "C-c I") 'refresh-emacs)

;; (defun gr/open-tasks-file ()
;;   (interactive)
;;   (find-file "~/Dropbox/org/tasks.org"))

;; (defun gr/open-tasks-upcoming-agenda ()
;;   (interactive)
;;   (find-file "~/Dropbox/org/tasks.org")
;;   (delete-other-windows)
;;   (set-frame-size (selected-frame) 80 43)
;;   (split-window-below)
;;   (org-agenda nil "y")
;;   (other-window 1)
;;   (enlarge-window 5))

;; (defun gr/open-inbox-below ()
;;   (interactive)
;;   (let ((buffer (find-file-noselect org-refile-file)))
;;     (pop-to-buffer buffer
;;                    '(display-buffer-at-bottom))))

;; (defun gr/open-init-file (p)
;;   "Open myinit.org in new frame. With universal argument, open in current window."
;;   (interactive "P")
;;   (cond ((equal p '(4))
;;           (gr/make-frame)
;;           (find-file (concat user-emacs-directory "init.el")))
;;     (t (find-file (concat user-emacs-directory "init.el")))))

(defun gr/insert-line (p)
  (interactive "P")
  (cond ((equal p '(4)) (save-excursion
                          (end-of-line 0)
                          (open-line 1)))
        (t (save-excursion
             (end-of-line)
             (open-line 1)))))

;; (global-set-key (kbd "C-o") 'gr/insert-line)

(defun gr/comment-and-copy ()
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (kill-ring-save beg end t)
    (comment-region beg end)
    (goto-char end)
    (forward-line 2)
    (save-excursion
      (yank)
      (newline 2))))

;; (bind-key* (kbd "C-M-;") 'gr/comment-and-copy)

(defun gr/copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))

(defun gr/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;; capitalize, upcase, downcase dwim

(defun title-case-region ()
  "Render string in region in title case."
  (interactive)
  (let* ((input (buffer-substring (region-beginning)
                                  (region-end)))
         (words (split-string input))
         (first (pop words))
         (last (car(last words)))
         (do-not-capitalize '("a" "an" "and" "as" "at" "but" "by" "en" "for" "if" "in" "of" "on" "or" "the" "to" "via"))
         (output (concat (capitalize first)
                         " "
                         (mapconcat (lambda (w)
                                      (if (not(member (downcase w) do-not-capitalize))
                                          (capitalize w)(downcase w)))
                                    (butlast words) " ")
                         " " (capitalize last))))
    (replace-string input output nil (region-beginning)(region-end))))


(defun ct/word-boundary-at-point-or-region (&optional callback)
  "Return the boundary (beginning and end) of the word at point, or region, if any.
Forwards the points to CALLBACK as (CALLBACK p1 p2), if present.

URL: https://christiantietze.de/posts/2021/03/change-case-of-word-at-point/"
  (let ((deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when callback
      (funcall callback $p1 $p2))
    (list $p1 $p2)))

;; (defun ct/capitalize-word-at-point ()
;;   (interactive)
;;   (ct/word-boundary-at-point-or-region #'upcase-initials-region))

(defun ct/downcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'downcase-region))

(defun ct/upcase-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'upcase-region))

(defun ct/capitalize-region (p1 p2)
  (downcase-region p1 p2)
  (upcase-initials-region p1 p2))

(defun ct/capitalize-word-at-point ()
  (interactive)
  (ct/word-boundary-at-point-or-region #'ct/capitalize-region))

;; Set global shortcuts
;; (global-set-key (kbd "M-c") #'ct/capitalize-word-at-point)
;; (global-set-key (kbd "M-u") #'ct/upcase-word-at-point)
;; (global-set-key (kbd "M-l") #'ct/downcase-word-at-point)
;; (global-set-key (kbd "M-U") #'title-case-region)



;;;; convert docx to org

(defun gr/flush-properties-drawers ()
  (interactive)
  (goto-line 2)
  (flush-lines ":PROPERTIES:")
  (flush-lines ":CUSTOM_ID:")
  (flush-lines ":END:")
  )

(defun gr/convert-pandoc-docx-org ()
  "Use pandoc via shell command to convert a docx file to an org file.
Navigate to files in dired, mark files, and execute command."
  (interactive)
  (dired-do-async-shell-command
   "pandoc -f docx -t org --wrap=none" current-prefix-arg
   (dired-get-marked-files t current-prefix-arg))
  (switch-to-buffer-other-window "*Async Shell Command*")
  (run-with-idle-timer 1 nil
                       'gr/flush-properties-drawers)
  (goto-line 2)
  (run-with-idle-timer 1 nil
                       'gr/flush-properties-drawers)
  )

;;;; my/pandoc--convert-buffer-from-markdown-to-org-in-place

;; from cashpw
(defun my/pandoc--convert-buffer-from-markdown-to-org-in-place ()
  "Converts the current buffer to org-mode in place."
  (interactive)
  (let ((buffer-content
         (buffer-string))
        (tmp-file
         (format
          "/tmp/%s.md"
          (format-time-string
           "%s" (current-time)))))
    (with-temp-buffer
      (insert
       buffer-content)
      (write-file
       tmp-file))
    (erase-buffer)
    (insert
     (shell-command-to-string
      (concat
       (format
        "pandoc --wrap=none -f markdown -t org %s"
        tmp-file)
       ;; Remove :PROPERTIES: drawers beneath headings
       " | sed -E '/^[[:space:]]*:/d'")))
    (org-mode)))

;;;; "Better Return" edited

;; a better return; inserts list item with RET instead of M-RET

;; (defun scimax/org-return (&optional ignore)
;;   "Add new list item, heading or table row with RET.
;; A double return on an empty element deletes it.
;; Use a prefix arg to get regular RET. "
;;   (interactive "P")
;;   (if ignore
;;       (org-return)
;;     (cond

;;      ((eq 'line-break (car (org-element-context)))
;;       (org-return t))

;;      ;; Open links like usual, unless point is at the end of a line.
;;      ;; and if at beginning of line, just press enter.
;;      ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
;;           (bolp))
;;       (org-return))

;;      ;; checkboxes - add new or delete empty
;;      ((org-at-item-checkbox-p)
;;       (cond
;;        ;; at the end of a line.
;;        ((and (eolp)
;;              (not (eq 'item (car (org-element-context)))))
;;         (org-insert-todo-heading nil))
;;        ;; no content, delete
;;        ((and (eolp) (eq 'item (car (org-element-context))))
;;         (delete-region (line-beginning-position) (line-end-position)))
;;        ((eq 'paragraph (car (org-element-context)))
;;         (goto-char (org-element-property :end (org-element-context)))
;;         (org-insert-todo-heading nil))
;;        (t
;;         (org-return))))

;;      ;; lists end with two blank lines, so we need to make sure we are also not
;;      ;; at the beginning of a line to avoid a loop where a new entry gets
;;      ;; created with only one blank line.
;;      ((org-in-item-p)
;;       (cond
;;        ;; empty definition list
;;        ((and (looking-at " ::")
;;              (looking-back "- " 3))
;;         (beginning-of-line)
;;         (delete-region (line-beginning-position) (line-end-position)))
;;        ;; empty item
;;        ((and (looking-at "$")
;;              (or
;;               (looking-back "- " 3)
;;               (looking-back "+ " 3)
;;               (looking-back " \\* " 3)))
;;         (beginning-of-line)
;;         (delete-region (line-beginning-position) (line-end-position)))
;;        ;; numbered list
;;        ((and (looking-at "$")
;;              (looking-back "^[0-9]+. " (line-beginning-position)))
;;         (beginning-of-line)
;;         (delete-region (line-beginning-position) (line-end-position)))
;;        ;; insert new item
;;        (t
;;         (if (not (looking-at "$"))
;;             (org-return)
;;           (end-of-line)
;;           (org-insert-item)))))

;;      ;; org-heading
;;      ((org-at-heading-p)
;;       (if (not (string= "" (org-element-property :title (org-element-context))))
;;           (if (not (looking-at "$"))
;;               (org-return)
;;             (progn
;;               ;; Go to end of subtree suggested by Pablo GG on Disqus post.
;;               ;;(org-end-of-subtree)
;;               (org-meta-return)
;;               ;;(org-metaright)
;;               ;;(org-insert-heading-respect-content)
;;               (outline-show-entry)
;;               ))
;;         ;; The heading was empty, so we delete it
;;         (beginning-of-line)
;;         (delete-region (line-beginning-position) (line-end-position))))

;;      ;; tables
;;      ((org-at-table-p)
;;       (if (-any?
;;            (lambda (x) (not (string= "" x)))
;;            (nth
;;             (- (org-table-current-dline) 1)
;;             (remove 'hline (org-table-to-lisp))))
;;           (org-return)
;;         ;; empty row
;;         (beginning-of-line)
;;         (delete-region (line-beginning-position) (line-end-position))
;;         (org-return)))

;;      ;; footnotes
;;      ((or (org-footnote-at-reference-p)
;;           (org-footnote-at-definition-p))
;;       (org-footnote-action))

;;      ;; fall-through case
;;      (t
;;       (org-return)))))

;;; karthink-dotfiles-popper/lisp/utilities.el

;;;; COUNT-WORDS-REGION: USING `while'

;; ;;;###autoload
(defun my/count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
  ;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
      ;; 2. Run the while loop.
      (while (and (< (point) end)
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))
      ;; 3. Send a message to the user.
      (cond ((zerop count)
	     (message
	      "The region does NOT have any words."))
	    ((= 1 count)
	     (message
	      "The region has 1 word."))
	    (t
	     (message
	      "The region has %d words." count))))))

;; count words in region
;; (global-set-key (kbd "C-=") 'my/count-words-region)

;; ;;;###autoload
(defun my/count-words-buffer ()
  "Print number of words in the region."
  ;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0)
          (beginning (point-min))
          (end (point-max)))
      (goto-char beginning)
      ;; 2. Run the while loop.
      (while (and (< (point) end)
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))
      ;; 3. Send a message to the user.
      (cond ((zerop count) (message "No words"))
            ((= 1 count) (message "1 word"))
            (t  (message  (format "%d words" count)))))))

;;;; PRINT ASCII TABLE

;; ;;;###autoload
(defun ascii-table ()
  "Display basic ASCII table (0 thru 127)"
  (interactive)
  (pop-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96)))))
  (special-mode))

;;;; INSERT FUNCTION DEFINITION AT POINT

;; ;;;###autoload
(defun insert-definition-at-point ()
  "Function to find the definition of the defun at point and insert it there."
  (interactive)
  (save-excursion
    (imenu (thing-at-point 'symbol))
    (mark-defun)
    (kill-ring-save (region-beginning)
                    (region-end)))
  (with-temp-buffer
    (yank)
    (beginning-of-buffer)
    (delete-blank-lines)
    (kill-new (buffer-substring-no-properties
               (point-min)
               (point-max))
              t))
  (beginning-of-line)
  (yank)
  (kill-whole-line)
  (beginning-of-defun))
;; (global-set-key (kbd "C-x C-M-y") 'insert-definition-at-point)

;;; org funcs


;;;; org-open-at-point-other-window

;; ohyecloudy-dot-doom/doom.d/config.org
;;;###autoload
(defun my/org-open-at-point-other-window ()
  (interactive)
  (let ((org-link-frame-setup (cons (cons 'file 'find-file-other-window) org-link-frame-setup)))
    (org-open-at-point)))

;;;; repeated words

(defun my/the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

;;   ;; Bind 'the-the' to  C-c \
;;   (bind-key "C-c \\" 'the-the)

;;;; add-newlines-between-paragraphs

(defun my/add-newlines-between-paragraphs ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (move-end-of-line nil)
      (newline)
      (next-line))))
;;;; org-capture and org-id

;;;;; my/org-insert-magic-link

(defun my/org-insert-magic-link (url)
  "Auto create org links to Wikipedia URL."
  (interactive "sLink to? ")
  (require 'url-util)
  (let ((title))
    (cond
     ((string-prefix-p "https://en.wikipedia.org/wiki/" url)
      (setq title (decode-coding-string (url-unhex-string (substring url 30)) 'utf-8)))
     (t (error "I have no idea what to do with this")))
    (org-insert-link nil url title)))

;;;;; my/org-capture-goto-link

;; /home/junghan/.emacs.tshu/lisp/lang-org.el
(defun my/org-capture-goto-link ()
  (let ((file (nth 1 (org-capture-get :target)))
        (headline (plist-get org-store-link-plist :description))
        (link (plist-get org-store-link-plist :link)))
    (org-capture-put :target (list 'file+headline file headline))
    (widen)
    (goto-char (point-min))
    (let (case-fold-search)
      (if (re-search-forward
           (format org-complex-heading-regexp-format
                   (regexp-quote headline)) nil t)
          (org-end-of-subtree)
        (org-capture-put :flag t)
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* TODO " headline "\n")
        (insert "[[" link "]]\n")
        (point)))))

;;;;; my/get-id-to-clipboard

(defun my/get-id-to-clipboard() "Copy an ID link with the
  headline to killring, if no ID is there then create a new unique
  ID.  This function works only in org-mode or org-agenda buffers.
  The purpose of this function is to easily construct id:-links to
  org-mode items. If its assigned to a key it saves you marking the
  text and copying to the killring."
       (interactive)
       (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
         (org-agenda-show)
         (org-agenda-goto))
       (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
         (setq mytmphead (nth 4 (org-heading-components)))
         (setq mytmpid (funcall 'org-id-get-create))
         (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
         (kill-new mytmplink)
         (message "Copied %s to killring (clipboard)" mytmplink)
         ))

;; (let ((map global-map))
;;     (define-key map (kbd "C-c j g") 'my/get-id-to-clipboard))

;;;; split and indirect orgtree

;; copy from writers-dot-spacemaccs
(defun my/split-and-indirect-orgtree ()
  "Splits window to the right and opens an org tree section in it"
  (interactive)
  (split-window-right)
  (org-tree-to-indirect-buffer)
  (windmove-right))

(defun my/kill-and-unsplit-orgtree ()
  "Kills the cloned buffer and deletes the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;;;; org-toggle-emphasis-markers

(defun my/org-toggle-emphasis-markers ()
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-fontify-buffer :interactively))

;;;; align-comments

(defun my/align-comments (beginning end)
  "Align comments in region."
  (interactive "*r")
  (align-regexp beginning end (concat "\\(\\s-*\\)"
                                      (regexp-quote comment-start)) nil 2))

;;;; indent-buffer

(defun my/indent-buffer ()
  "Indent buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;; comment-or-uncomment-region

(defun my/comment-or-uncomment-region ()
  "Comment or uncomment region with just a character (e.g. '/'). If a region is
active call comment-or-uncomment-region, otherwise just insert the given char."
  (interactive)
  (call-interactively (if (region-active-p)
                          'comment-or-uncomment-region
                        'self-insert-command)))

;;;; org-indent-src-block

(defun my/org-modern-indent-src-block ()
  (interactive)
  (org-edit-special)
  (my/indent-buffer)
  (org-edit-src-exit))

;;;; org-sort-by-priority

(defun my/org-modern-sort-by-priority ()
  "Sort entries in level=2 by priority."
  (interactive)
  (org-map-entries (lambda () (condition-case nil
                                  (org-sort-entries nil ?p)
                                (error nil)))
                   "LEVEL=1")
  (org-set-startup-visibility))

;;;; org-delete-link

(defun org-delete-link ()
  "Remove the link part of an org-mode link at point and keep
  only the description"
  (interactive)
  (let ((elem (org-element-context)))
    (when (eq (car elem) 'link)
      (let* ((content-begin (org-element-property :contents-begin elem))
             (content-end  (org-element-property :contents-end elem))
             (link-begin (org-element-property :begin elem))
             (link-end (org-element-property :end elem)))
        (when (and content-begin content-end)
          (let ((content (buffer-substring-no-properties content-begin content-end)))
            (delete-region link-begin link-end)
            (insert (concat content " "))))))))

;;;; org-insert-file-link

(defun org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))

;;;; org-random-heading

(defun my/org-random-heading ()
  "Jump to a random org heading in the current org file."
  (interactive)
  (goto-char (point-min))
  (let ((headings '()))
    (while (re-search-forward "^\\*+ " nil t)
      (push (point) headings))
    (when headings
      (goto-char (nth (random (length headings)) headings))
      (org-reveal))))

;;;; org-insert-link-dwim

(defun org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "Title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

;;;; org-show-level

(defun org-show-level-1 ()
  (interactive)
  (org-content 1))

(defun org-show-level-2 ()
  (interactive)
  (org-content 2))

(defun org-show-level-3 ()
  (interactive)
  (org-content 3))

(defun org-show-level-4 ()
  (interactive)
  (org-content 4))

;;;; show-duplicate-lines

(defun show-duplicate-lines ()
  "Display all duplicate lines in the current buffer."
  (interactive)
  (let ((lines (split-string (buffer-string) "\n" t))
        (seen-lines '())
        (dup-lines '()))
    (dolist (line lines)
      (if (member line seen-lines)
          (setq dup-lines (cons line dup-lines))
        (setq seen-lines (cons line seen-lines))))
    (if dup-lines
        (with-output-to-temp-buffer "*Duplicate Lines*"
          (dolist (line (reverse dup-lines))
            (princ (concat line "\n"))))
      (message "No duplicate lines found."))))

;;;; get/insert heading-title / category

;; (defun cc-todo-item () ; from choi
;;   (interactive)
;;   (org-insert-heading)
;;   (org-schedule nil)
;;   (org-shiftup)
;;   (org-shiftright)
;;   (end-of-line))

;; http://article.gmane.org/gmane.emacs.orgmode/10256
(defun my/org-get-heading-title ()
  "Returns the heading of the current entry as a string, without the leading stars, the TODO keyword or the tags."
  (let ((title-with-props (org-get-heading t))
        (keyword (org-get-todo-state)))
    (substring-no-properties title-with-props (if keyword (1+ (length keyword))))))

(defun my/org-insert-heading-category ()
  "Insert a :CATEGORY: property and it's value to the PROPERTY drawer at point."
  (interactive)
  (let ((point (point)))
    (org-entry-put point "CATEGORY" (org-get-heading-title))))

(defun my/org-insert-heading-categories-all ()
  "Insert :CATEGORY: properties to each headlines indented level 2."
  (interactive)
  (org-map-entries
   (lambda ()
     (if (eq 2 (org-current-level))
         (org-insert-heading-category)))))

(defun my/insert-header-from-note-name ()
  (interactive)
  (let ((date (encode-time (org-parse-time-string (substring (buffer-name) 5 -4)))))
    (save-excursion
      (goto-char (point-min))
      (insert (concat "#+TITLE: " (org-format-time-string "%Y-%m-%d " date nil)  "\n"
                      "#+date: " (format-time-string "[%Y-%m-%d]" date) "\n\n")
              )))
  )

;;;; org-narrow-to-item

;; https://emacs-china.org/t/org-mode-narrow-to-sublist/24682/5
(defun my/org-narrow-to-item ()
  "Narrow buffer to the current item.
Throw an error when not in a list."
  (interactive)
  (save-excursion
    (narrow-to-region
     (progn (org-beginning-of-item) (point))
     (progn (org-end-of-item) (1- (point))))))

;;;; my/view-text-file-as-info-manual

;; View ‘info’, ‘texi’, ‘org’ and ‘md’ files as ‘Info’ manual
(defun my/view-text-file-as-info-manual ()
  (interactive)
  (require 'ox-texinfo)
  (let ((org-export-with-broken-links 'mark))
    (pcase (file-name-extension (buffer-file-name))
      (`"info"
       (info (buffer-file-name)))
      (`"texi"
       (info (org-texinfo-compile (buffer-file-name))))
      (`"org"
       (info (org-texinfo-export-to-info)))
      (`"md"
       (let ((org-file-name (concat (file-name-sans-extension (buffer-file-name)) ".org")))
         (apply #'call-process "pandoc" nil standard-output nil
                `("-f" "markdown"
                  "-t" "org"
                  "-o" , org-file-name
                  , (buffer-file-name)))
         (with-current-buffer (find-file-noselect org-file-name)
           (info (org-texinfo-export-to-info)))))
      (_ (user-error "Don't know how to convert `%s' to an `info' file"
                     (file-name-extension (buffer-file-name)))))))

;;;; my/org-outdent-or-promote

(defun my/org-outdent-or-promote ()
  "Run either org-outdent-item-tree or org-promote-subtree,
depending on which one is appropriate based on the context."
  (interactive)
  (cond
   ;; If the cursor is on a plain list item, run org-outdent-item-tree
   ((org-at-item-p) (org-outdent-item-tree))
   ;; If the cursor is on a headline, run org-promote-subtree
   ((org-at-heading-p) (org-promote-subtree))
   ;; Otherwise, do nothing and show a message
   (t (message "Not at an item or a headline"))))

(defun my/org-indent-or-demote ()
  "Run either org-indent-item-tree or org-demote-subtree,
depending on which one is appropriate based on the context."
  (interactive)
  (cond
   ;; If the cursor is on a plain list item, run org-indent-item-tree
   ((org-at-item-p) (org-indent-item-tree))
   ;; If the cursor is on a headline, run org-demote-subtree
   ((org-at-heading-p) (org-demote-subtree))
   ;; Otherwise, do nothing and show a message
   (t (message "Not at an item or a headline"))))

;;;; my/unfill-paragraph-or-region

;; unfill paragraph: the opposite of fill-paragraph
(defun my/unfill-paragraph-or-region (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my/unfill-paragraph-keep-formatting (start end)
  "Unfill the region, but preserve plain-text lists and org-mode SCHEDULED tasks."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (unfill-paragraph start end)))

(defun should-unfill-p (start end)
  "Determine whether the region should be unfilled."
  (save-excursion
    (goto-char start)
    (not (or (looking-at "^\\s-*\\([-*+]\\|[0-9]+[.)]\\)\\s-+") ;; plain-text list
             (looking-at "^\\s-*SCHEDULED:")           ;; org-mode SCHEDULED task
             (looking-at "^\\s-*DEADLINE:")            ;; org-mode DEADLINE
             ))))

(defun my/unfill-region-smart (start end)
  "Unfill the region, but preserve plain-text lists and org-mode SCHEDULED tasks."
  (interactive "*r")
  (let ((pos start))
    (while (< pos end)
      (let ((next-pos (or (next-single-property-change pos 'hard) end)))
        (when (should-unfill-p pos next-pos)
          (unfill-region-keep-formatting pos next-pos))
        (setq pos next-pos)))))

;;;; my/region-to-numbered-list

(defun my/region-to-numbered-list (start end)
  "Turn a region into a numbered list."
  (interactive "r")
  (let* ((s (buffer-substring-no-properties start end))
         (split-strings (split-string s "\\([0-9]+\\. \\)" t))
         (trimmed-strings (mapcar 'string-trim split-strings))
         (filtered-strings (cl-remove-if (lambda (x) (string= x "")) trimmed-strings))
         (numbered-strings
          (cl-loop for str in filtered-strings and i from 1
                   collect (concatenate 'string (number-to-string i) ". " str))))
    (delete-region start end)
    (insert (mapconcat 'identity numbered-strings "\n"))))

;;;; my/extract-hyperlinks-from-file

(defun my/extract-hyperlinks-from-file (file)
  "Extracts all hyperlinks from the text file FILE."
  (interactive "fEnter file to extract hyperlinks from: ")
  (let ((hyperlinks '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "\\(http\\|https\\|id\\)://[^[:space:]]+" nil t)
        (push (match-string-no-properties 0) hyperlinks)))
    (message "Hyperlinks extracted: %s" hyperlinks)
    hyperlinks))

;;;; my/extract-hyperlinks-from-buffer

(defun my/extract-hyperlinks-from-buffer ()
  "Extracts all hyperlinks from the current buffer."
  (interactive)
  (let ((hyperlinks '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:[^]]+\\]\\]" nil t)
        (push (match-string-no-properties 0) hyperlinks)))
    (message "Hyperlinks extracted: %s" hyperlinks)
    hyperlinks))

;;;; Capitalize level 1 headings use correct rules of capitalizing titles

(defun my/org-titlecase-level-1 ()
  "Convert all Level 1 org-mode headings to title case."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (titlecase-line))))

;;;; my/iloveyou

(defun my/iloveyou (args)
  (interactive "P")
  (message "%s" (propertize "I love you!" 'Face '(:foreground "red")))
  )

;;;; my/daily-log

(defun my/daily-log ()
  "Insert a new daily log entry with the current date."
  (interactive)
  (goto-char (point-max))
  (org-insert-heading-respect-content)
  (insert (format-time-string "[%Y-%m-%d %a]") "\n")
  (insert "+ Accomplishments:\n")
  (insert "  - Task 1\n")
  (insert "  - Task 2\n")
  (insert "+ Challenges:\n")
  (insert "  - Issue 1\n")
  (insert "  - Issue 2\n")
  (insert "+ Learnings:\n")
  (insert "  - Insight 1\n")
  (insert "  - Insight 2\n")
  (insert "+ Plans for Tomorrow:\n")
  (insert "  - Task 1\n")
  (insert "  - Task 2\n"))

;;;; my/consult-org-screenshot

;;;###autoload
(defun my/consult-org-screenshot ()
  (interactive)
  (consult-fd org-screenshot-path))

;;;; org-procratinate

;;;###autoload
(defun org-procrastinate ()
  "Set the scheduled date on an Org agenda item to tomorrow."
  (interactive)
  (org-agenda-schedule nil "+1d"))

;;;; add-to-org-user-agenda-files

;; (setq org-agenda-files (append org-user-agenda-files org-agenda-files))
;;;###autoload
(defun add-to-org-user-agenda-files (file)
  "다음 파일을 org-agenda-files에 추가합니다: FILE"
  (unless (member file org-user-agenda-files)
    (add-to-list 'org-user-agenda-files file)))

;;; pkm func

;; /injae-dotfiles/module/+util.el
;; text random
(defun randomize-region (beg end)
  (interactive "r")
  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty
    ;; line; it is probably not the case that the line should be
    ;; included in the reversal; it isn't difficult to add it
    ;; afterward.
    (or (and (eolp) (not (bolp)))
        (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))
    (let ((strs (shuffle-list
                 (split-string (buffer-substring-no-properties beg end)
                               "\n"))))
      (delete-region beg end)
      (dolist (str strs)
        (insert (concat str "\n"))))))

(defun describe-last-function ()
  (interactive)
  (describe-function last-command))

(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

(defun what-face (pos)
  "Show the name of face under point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun range (n)
  "Python like range function returning list."
  (cl-loop for i from 0 to (- n 1)
           collect i))

(defun shuffle-list (its)
  "Destructive but inefficient list shuffling."
  (cl-loop for i downfrom (- (length its) 1) to 1
           do (let ((i-val (nth i its))
                    (j (random (+ i 1))))
                (setf (nth i its) (nth j its))
                (setf (nth j its) i-val)))
  its)

(defun shuffle-org-list ()
  "Shuffle list at point."
  (interactive)
  (save-excursion
    (let ((org-list (org-list-to-lisp t)))
      (insert (org-list-to-org (cons (car org-list) (shuffle-list (cdr org-list)))))
      (org-list-repair))))

(defun reading-time (&optional wpm)
  (/ (count-words (point-min) (point-max)) (or wpm 200)))

(defun firefox-profile-directory ()
  "Return profile directory for firefox."
  (-find (lambda (d) (string-match "default$" d)) (f-directories "~/.mozilla/firefox")))

(defmacro --with-temp-copy (file-path &rest body)
  "Run BODY after making a temporary copy of given FILE-PATH.

  In the BODY forms, `it' provides the path for the copy."
  (declare (indent defun))
  `(let ((it (make-temp-file (f-base ,file-path))))
     (unwind-protect
         (progn
           (copy-file ,file-path it t)
           ,@body)
       (f-delete it))))

(defun youtube-history ()
  "Return youtube history."
  (--with-temp-copy (f-join (firefox-profile-directory) "places.sqlite")
    (json-parse-string
     (shell-command-to-string
      (format "sqlite3 -json %s %s"
              (shell-quote-argument it)
              (shell-quote-argument "SELECT url, title FROM moz_places WHERE title IS NOT NULL AND rev_host LIKE '%utuoy%' AND url LIKE '%watch%' ORDER BY last_visit_date DESC")))
     :array-type 'list
     :object-type 'alist)))

(defvar youtube-process nil
  "Process for keeping youtube player.")

;; (defun youtube-play-url (url)
;;   (when (and youtube-process (process-live-p youtube-process))
;;     (kill-process youtube-process))
;;   (setq youtube-process (start-process "youtube-play" nil "mpv" "--no-video" url)))

;; (defun youtube-history-play ()
;;   (interactive)
;;   (helm :sources (helm-build-sync-source "youtube-history"
;;                                          :candidates (mapcar (lambda (it) (cons (alist-get 'title it) (alist-get 'url it))) (youtube-history))
;;                                          :action `(("Play audio" . youtube-play-url)))
;;         :buffer "*helm youtube history*"
;;         :prompt "Title: "))

(defun aleatory-assitance ()
  "Give a random strategy to get unstuck."
  (interactive)
  (let ((strategies (s-split "\n" (s-trim "1. Take the braver decision
    2. Take a nap
    3. What's the title of this book?
    4. What's the choice between?
    5. Ask ChatGPT for the final decision
    6. What will make you proud of yourself?
    7. Choose freedom
    8. Start reading a new book
    9. Be kind to people involved
    10. Name this
    11. Start a repository
    12. Where's the money coming from?
    13. Close everything, start again
    14. Connect with an expert in the area
    15. How would you have done it?
    16. Combine two unrelated concepts
    17. Toss a coin
    18. Explain it to a business person
    19. How much time will it take? Take 3 times more
    20. Talk to the nearest human
    21. What are the ingredients? What's missing?
    22. Search old notes
    23. How will this look like in the future?
    24. Find an equivalent problem
    25. Work in a different domain
    26. Take out another card
    27. What's the most ambitious option?
    28. List risks
    29. Run an experiment
    30. Record a video on current status
    31. Ship right now!
    32. What's the strongest feeling right now?
    33. What's one bias you can remove right now?
    34. Write an email
    35. What's the weather trend these days?
    36. Use a new animal
    37. Make a plot
    38. Collect data
    39. Make it efficient
    40. Remove the most meaningless portion
    41. Make a mistake
    42. What do you need other than time? Ask for it.
    "))))
    (print (car (shuffle-list strategies)))))

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

;; (setq my_shell_output
;;       (substring
;;        (shell-command-to-string "/bin/echo hello")
;;        0 -1))

(defun my/find-random-file-old-journals ()
  (interactive)
  (find-file
   (substring
    (shell-command-to-string "find ~/org/oldseq/journals -type f | shuf -n 1")
    0 -1)))

(defun my/find-random-file-old-pages ()
  (interactive)
  (find-file
   (substring
    (shell-command-to-string "find ~/org/oldseq/pages -type f | shuf -n 1")
    0 -1)))

;;; consult rg search

;; Full text search the whole org directory
(defun my/consult-ripgrep-org-directory ()
  (interactive)
  (require 'consult)
  ;; Add "--no-ignore-vcs" to the rg command so todo.org could be searched.
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore-vcs")))
    (consult-ripgrep org-directory "")))

(defun my/rg-search-clone-notes-all ()
  "Search org directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --ignore-case --type org --type md --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep "~/nosync/clone-notes/")))

(defun my/rg-search-clone-notes-ko ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --ignore-case --type org --type md --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep "~/nosync/clone-notes/ko/")))

;;; Logos focus editing toggle

(progn

  ;;;###autoload
  (defun my/logos-focus-editing-on ()
    ;; hide current global centaur-tabs
    ;; (centaur-tabs-local-mode t)

    (display-line-numbers-mode -1)
    ;; (centered-cursor-mode t)
    (logos-focus-mode t)
    (org-toggle-inline-images +1)
    ;; (logos-narrow-dwim)
    ;; (visual-line-mode -1)

    (when (fboundp 'fontaine-set-preset)
      (fontaine-set-preset 'presenting))
    (when (fboundp 'vi-tilde-fringe-mode)
      (vi-tilde-fringe-mode -1))

    (when (fboundp 'tab-line-mode)
      (tab-line-mode -1))

    (when (fboundp 'keycast-tab-bar-mode)
      (keycast-tab-bar-mode -1))

    (diff-hl-mode -1)
    )

  ;;;###autoload
  (defun my/logos-focus-editing-off ()
    (logos-focus-mode -1)
    ;; (centered-cursor-mode -1)

    (diff-hl-mode 1)
    (display-line-numbers-mode 1)
    (org-toggle-inline-images -1)
    ;; (widen)
    ;; (visual-line-mode 1)

    (when (fboundp 'fontaine-set-preset)
      (fontaine-set-preset 'default))
    (when (fboundp 'vi-tilde-fringe-mode)
      (vi-tilde-fringe-mode t))

    (when (fboundp 'tab-line-mode)
      (tab-line-mode 1))

    (when (fboundp 'keycast-tab-bar-mode)
      (when (string= (system-name) "jhnuc")
        (keycast-tab-bar-mode +1)))

    ;; show global centaur-tabs
    ;; (centaur-tabs-local-mode -1)
    )

  ;;;###autoload
  (defun my/logos-focus-editing-toggle ()
    (interactive)
    (if (eq logos-focus-mode t)
        (my/logos-focus-editing-off)
      (my/logos-focus-editing-on)))
  )

;;; eww-to-org

;; Link: https://jao.io/blog/eww-to-org.html

;; Here's a quick'n'dirty command to generate an org-mode rendering of an eww page,
;; taking into account not only links (as the built-in org-eww-copy-for-org-mode does),
;; but also headings, italic and bold faces.

;; I am pretty sure it's full of corner cases and rough edges, but it's working for me
;; as a starting point.

(defun jao-eww-to-org (&optional dest)
  "Render the current eww buffer using org markup.
If DEST, a buffer, is provided, insert the markup there."
  (interactive)
  (unless (org-region-active-p)
    (let ((shr-width 80)) (eww-readable)))
  (let* ((start (if (org-region-active-p) (region-beginning) (point-min)))
         (end (if (org-region-active-p) (region-end) (point-max)))
         (buff (or dest (generate-new-buffer "*eww-to-org*")))
         (link (eww-current-url))
         (title (or (plist-get eww-data :title) "")))
    (with-current-buffer buff
      (insert "#+title: " title "\n#+link: " link "\n\n")
      (org-mode))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((p (point))
               (props (text-properties-at p))
               (k (seq-find (lambda (x) (plist-get props x))
                            '(shr-url image-url outline-level face)))
               (prop (and k (list k (plist-get props k))))
               (next (if prop
                         (next-single-property-change p (car prop) nil end)
                       (next-property-change p nil end)))
               (txt (buffer-substring (point) next))
               (txt (replace-regexp-in-string "\\*" "·" txt)))
          (with-current-buffer buff
            (insert
             (pcase prop
               ((and (or `(shr-url ,url) `(image-url ,url))
                     (guard (string-match-p "^http" url)))
                (let ((tt (replace-regexp-in-string "\n\\([^$]\\)" " \\1" txt)))
                  (org-link-make-string url tt)))
               (`(outline-level ,n)
                (concat (make-string (- (* 2 n) 1) ?*) " " txt "\n"))
               ('(face italic) (format "/%s/ " (string-trim txt)))
               ('(face bold) (format "*%s* " (string-trim txt)))
               (_ txt))))
          (goto-char next))))
    (pop-to-buffer buff)
    (goto-char (point-min))))

;;; org-ql

;; https://sachachua.com/blog/2024/10/shuffling-my-org-mode-unscheduled-tasks
(when (locate-library "org-ql")
  (defun my/org-ql-shuffle-todo ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and
        (todo "TODO" "NEXT")
        (not (done))
        (not (scheduled))
        (not (deadline))
        (not (ts-active))
        ;; (not (tags "cooking"))
        )
      :sort 'random))

  ;; tag later
  (defun my/org-ql-shuffle-later ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and
        ;; (todo "SOMEDAY")
        (tags "later")
        (not (done))
        (not (scheduled))
        (not (deadline))
        (not (ts-active))
        ;; (not (tags "cooking"))
        )
      :sort 'random))
  )

;;; provide

(provide 'org-funcs)

;;; end-of-line
