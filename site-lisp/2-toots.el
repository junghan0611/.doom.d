;; Contact: Christian Barthel  <bch@online.de>
;;

;; Setup:
;; Set variables ‘mastodon_host’ and ‘mastodon_access_token’
;; and eventually ‘request-curl-options’

;; Usage: Posting a new status

;; (1) create Mastodon post i.e. with ‘org-capture’
;;     (‘c-c t c T’)
;; (2) write the mastodon status update
;; (3) Optional: add images as attachments to
;;     the org-item
;; (4) execute ‘x/mastodon-post’
;; (5) execute ‘org-export-dispatch’.  Create single
;;     file export and upload it to ‘base-toots’ domain

;; Usage: Replying to a Mastodon status
;; (1) Ensure that the Org Node one level above has
;;     the mastodon_id set as Org Property.
;; (2) Create a new toot one level below with ‘org-capture’
;; (3) Execute ‘x/mastodon-post’

;; Dependency: request.el <https://github.com/tkf/emacs-request>
;; 
;; Looks like url-retrieve-synchronously is not able
;; to handle binary image uploads, therefore I use
;; requests/curl backend at the moment.
;;   <https://lists.gnu.org/archive/html/help-gnu-emacs/2024-03/msg00182.html>


;; Set connection options
;;; (setq request-curl-options nil)
;;; (setq request-curl-options (list "-x" "http://127.0.0.1:3128"))

;; Debugging for request.el:
(setq request-log-level 'debug)

;; Set mastodon connection details:
(setq mastodon_host "https://your-mastodon.host"
      mastodon_access_token "XXX")

(defun x/post (body-string media replyid)
  "Send Mastodon Status Update

This function sends a Mastodon status update to ‘mastodon_host’
with ‘mastodon_access_token’.  The ‘body-string’ should be
a non-nil string.  ‘media’ is a list of IDs to link images to
the status and may be ‘nil’.  The ‘replyid’ is an ID of an existing
mastodon post.  This can be nil.
"
  (progn
    ;; Send Request:
    (request
      (format "%s/api/v1/statuses?access_token=%s"
              mastodon_host
              mastodon_access_token)
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data
      (json-encode (list
                    (cons "status" body-string)
                    (cons "in_reply_to_id" replyid)
                    (cons "poll" nil)
                    (cons "language" "ko") ; "en"
                    (cons "sensitive" nil)
                    (cons "visibility" "public")
                    (if media (cons "media_ids" (split-string media ",")) (cons "media_ids" nil))
                    ))
      :parser 'json-read
      :sync t
      :complete (cl-function
                 (lambda (&key data &allow-other-keys)
                   (progn
                     (message "I sent: %S" (assoc-default 'id data))
                     (org-entry-put (point-marker) "mastodon_id"
                                    (format "%s" (assoc-default 'id data)))
                     (org-entry-put (point-marker) "mastodon_url"
                                    (format "%s" (assoc-default 'url data)))
                     (org-entry-put (point-marker) "mastodon_uri"
                                    (format "%s" (assoc-default 'uri data)))
                     (org-entry-put (point-marker) "mastodon_created_at"
                                    (format "%s" (assoc-default 'created_at data)))
                     data))))))

;; Save the current-media ID list in the x/current-media
;; string (used between uploading media and setting this
;; value as an org-property)
(setq x/current-media nil)


(defun x/post-media (filename)
  "Post ‘filename’ as media to mastodon

This function sends ‘filename’ to the mastodon API.  The returned
ID will be added to the list in ‘x/current-media’ as side effect.

See ‘x/post’ function as well.
"
  (with-temp-buffer
    (insert-file-contents filename)
    (request
      (format "%s/api/v1/media" mastodon_host)
      :type "POST"
      :files `(("file" . ,(current-buffer)))
      :parser 'json-read
      :encoding 'binary
      :sync t
      :headers `(("Authorization" . ,(format "Bearer %s" mastodon_access_token)))
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
           (progn
             (setq x/current-media (concat x/current-media
                                           (if x/current-media "," "")
                                           (format "%s"
                                                   (assoc-default 'id data))))))))))


(defun x/get-reply-id ()
  "Get reply ID for post, return ‘nil’ if not existent

This function looks up MASTODON_ID in the parent org-item.
When an ID is available, the Mastodon status update will
be sent as reply.
"
  (interactive)
    (plist-get
     (plist-get
      (plist-get
       (plist-get (org-element-context) 'headline)
       :parent)
      'headline)
     :MASTODON_ID))


(defun x/upload-media ()
  "Search for images, upload them to mastodon

Assumptions: images are inserted with org-attach, a DIR
property is created and a full path is being constructed
by using DIR and the relative filename.
"
  (let ((list-of-ids '())
        (folder (org-entry-get (point) "DIR")))
    (save-excursion
      (progn
        (setq x/current-media nil)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        ;; find all attachments
        ;; upload them
        (while (re-search-forward "attachment:*" nil t)
          (progn
            (x/post-media
             (format "%s/%s" folder
                     (substring-no-properties (thing-at-point 'filename)
                                              (length "attachment:"))))))
        (org-entry-put (point-marker) "mastodon_media" (or x/current-media "nil"))
        (widen)
        list-of-ids))))

(defun x/upload-status ()
  "Upload Mastodon Status

Create a new Mastodon status entry or, when mastodon_id is
being found in the org-item one level above (parent), reply
to an existing mastodon status.

Also link with media when ‘x/current-media’ is set.
"
  (let ((folder (org-entry-get (point) "DIR"))
        (bodytext nil))
    (save-excursion
      (progn
        (x/post
         (progn
           (org-narrow-to-subtree)
           (goto-char (point-min))
           (setq bodytext
                 (substring-no-properties
                  (org-agenda-get-some-entry-text
                   (point-marker) 250)))
           (widen)
           (replace-regexp-in-string
            "#\\+ATTR_ORG:.*" ""
            (replace-regexp-in-string "\[\[attachment:[/a-zA-Z0-9.]*\]\]" "" bodytext)))
         x/current-media
         (x/get-reply-id)
         )))))

(defun x/mastodon-post ()
  (interactive)
  (setq x/current-media nil)
  (x/upload-media)
  (x/upload-status)
  (setq x/current-media nil))
