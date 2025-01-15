;;; lisp/my-ekg-denote.el -*- lexical-binding: t; -*-

;; 한글 태그 -> 타이틀
;; 영문 태그 -> 태그 유지
;; date doc term idea 별도 처리
;; inline 태그? 태그에 반영 되어야 한다.
;; ekg 태그를 달아준다. 뭔가 다르게 관리 되어야 할 필요

;; 2024-09-13 : 잘 된다. 양방향성에서 자유롭다. 그러니까 메모 끄적이는 것은 부담이 없어야 한다는 점.

;;; code

;; (progn
;;   (require 'ekg-denote)

;;   ;; (ekg-denote-set-last-export 123)
;;   ;; (ekg-denote-get-last-export)

;;   (setq ekg-denote-export-add-front-matter nil)
;;   (setq ekg-denote-export-backup-on-conflict t)
;;   (setq ekg-denote-export-title-max-len 70)
;;   (setq ekg-denote-export-combined-keywords-len 150)

;;   (setq ekg-denote-directory (concat denote-directory "ekg/"))

;;   (defun ekg-denote-create (note)
;;     "Create a new `ekg-denote' from given NOTE."
;;     (let* ((id (format-time-string denote-id-format (ekg-note-creation-time note)))
;;            (note-id (ekg-note-id note))
;;            (text (or (ekg-note-text note) ""))
;;            (ext (if (eq ekg-capture-default-mode 'org-mode) ".org" ".md"))
;;            ;; remove date tag as denote uses date in ID.
;;            (tags (seq-filter (lambda (tag)
;;                                (not (string-prefix-p "date/" tag))) (ekg-note-tags note)))

;;            ;; 조건 추가 -> doc term idea 도 처리 할 것

;;            ;; 한글 태그는 타이틀로 가져오면 좋겠다
;;            (ekg-title (seq-filter (lambda (tag) ;; [A-Za-z0-9]
;;                                     (not (string-match "^[A-Za-z].*" tag))) tags))
;;            (tagsen (seq-filter (lambda (tag) ;; [A-Za-z0-9]
;;                                  (string-match "^[A-Za-z].*" tag)) tags))
;;            (kws (ekg-denote-sublist-keywords
;;                  (denote-sluggify-keywords tagsen) ekg-denote-export-combined-keywords-len))
;;            (title (string-limit (denote-sluggify-title (mapconcat 'identity ekg-title " ")) ekg-denote-export-title-max-len))

;;            (signature-slug "")
;;            (path (denote-format-file-name (file-name-as-directory ekg-denote-directory) id kws title ext signature-slug))) ;; title
;;       (make-ekg-denote :id id
;; 		       :note-id note-id
;; 		       :text text
;; 		       :kws kws
;; 		       :title title
;; 		       :path path)))
;;   )

;;; end-of-file
