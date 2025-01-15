;;; ob-mit-scheme.el --- Babel Functions for Scheme      -*- lexical-binding: t; -*-
;;;
;;; Modification by Sam Ritchie to get this working with mit-scheme and the
;;; scmutils library.

;;; Requirements:

(require 'ob)
(require 'ob-scheme)
(require 'xscheme)

;; so that evaluations don't clobber the REPL evals.
(setq xscheme-buffer-name "*xscheme*")

(defun xscheme-wait-for-process ()
  (sleep-for 0 100)
  (while (not (string-equal xscheme-runlight-string "input"))
    (message xscheme-runlight-string)
    (sleep-for 0 200)))

(defun run-xscheme (command-line)
  "Run MIT Scheme in an inferior process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line."
  (interactive (list (xscheme-read-command-line current-prefix-arg)))
  (xscheme-start command-line xscheme-process-name xscheme-buffer-name))

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(prepends (cdr (assq :prologue params))))
    (concat (and prepends (concat prepends "\n"))
	    (if (null vars) body
	      (format "(let (%s)\n%s\n)"
		      (mapconcat
		       (lambda (var)
			 (format "%S" (print `(,(car var) ',(cdr var)))))
		       vars
		       "\n      ")
		      body)))))

(defvar org-babel-scheme-repl-map (make-hash-table :test #'equal)
  "Map of scheme sessions to session names.")

(defun org-babel-scheme-cleanse-repl-map ()
  "Remove dead buffers from the REPL map."
  (maphash
   (lambda (x y) (unless (buffer-name y) (remhash x org-babel-scheme-repl-map)))
   org-babel-scheme-repl-map))

(defun org-babel-scheme-get-session-buffer (session-name)
  "Look up the scheme buffer for a session; return nil if it doesn't exist."
  (org-babel-scheme-cleanse-repl-map) ; Prune dead sessions
  (gethash session-name org-babel-scheme-repl-map))

(defun org-babel-scheme-set-session-buffer (session-name buffer)
  "Record the scheme buffer used for a given session."
  (puthash session-name buffer org-babel-scheme-repl-map))

(defun org-babel-scheme-get-repl (name)
  "Switch to a scheme REPL, creating it if it doesn't exist:"
  (let ((buffer (org-babel-scheme-get-session-buffer name)))
    (or buffer
        (progn
          (run-xscheme (xscheme-default-command-line))
          (when name
            (rename-buffer name t)
            (org-babel-scheme-set-session-buffer name (current-buffer)))
          (current-buffer)))))

(defun org-babel-scheme-make-session-name (buffer name impl)
  "Generate a name for the session buffer.

For a named session, the buffer name will be the session name.

If the session is unnamed (nil), generate a name.

If the session is `none', use nil for the session name, and
org-babel-scheme-execute-with-geiser will use a temporary session."
  (cond
   ((not name) (concat buffer " " (symbol-name impl) " REPL"))
   ((string= name "none") nil)
   (name)))

(defmacro org-babel-scheme-capture-current-message (&rest body)
  "Capture current message in both interactive and noninteractive mode."
  `(if noninteractive
       (let ((original-message (symbol-function 'message))
             (current-message nil))
         (unwind-protect
             (progn
               (defun message (&rest args)
                 (setq current-message (apply original-message args)))
               ,@body
               current-message)
           (fset 'message original-message)))
     (progn
       ,@body
       (current-message))))


(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim-ret-value (s)
  (if (string-match ";No return value." s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-ret-value (s-trim-left (s-trim-right s))))

(defun org-babel-scheme-execute-with-mit-scheme (code output repl)
  "Execute code in specified REPL. If the REPL doesn't exist, create it
using the given scheme implementation.

Returns the output of executing the code if the output parameter
is true; otherwise returns the last value."
  (message "------------MIT-Scheme execution ORG----------------------")
  (message "Code: %s" code)
  (message "Output: %s" output)
  (message "Current buffer: %s" (current-buffer))
  (let ((code-to-execute (if output
                             (format "(with-output-to-string (lambda () %s))" code)
                           code))
        (result nil)
        (repl-buffer (xscheme-process-buffer)))
    (when (null repl-buffer)
      (save-current-buffer (run-xscheme (xscheme-default-command-line)))
      (setq repl-buffer (xscheme-process-buffer)))
    (with-current-buffer repl-buffer
      (message "Erasing scheme buffer...")
      (delete-region (point-min) (point-max)))
    (xscheme-send-string code-to-execute)
    (xscheme-wait-for-process)
    (message "Scheme: done!")
    (with-current-buffer (xscheme-process-buffer)
      (let ((repl-content (buffer-substring-no-properties (point-min) (point-max))))
        (message "REPL: %s" repl-buffer)
        (message "Buffer: %s" repl-content)
        (setq result (s-trim repl-content))
        (message "Result: %s" result)
        (message "--------------------------------")))
    result))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  ;; good
  (let* ((source-buffer (current-buffer))

         ;; good
         (source-buffer-name (replace-regexp-in-string ;; zap surrounding *
                              "^ ?\\*\\([^*]+\\)\\*" "\\1"
                              (buffer-name source-buffer))))
    (save-excursion
      (let* ((result-type (cdr (assoc :result-type params)))
             ;; impl goes away
             (session (org-babel-scheme-make-session-name
                       source-buffer-name (cdr (assoc :session params)) 'mit))
             (full-body (org-babel-expand-body:scheme body params))
             (result-params (cdr (assq :result-params params)))
             (result (org-babel-scheme-execute-with-mit-scheme
                      full-body			 ; code
                      (string= result-type "output") ; output?
                      (and (not (string= session "none")) session)))
             (table (org-babel-reassemble-table
                     result                             ; session
                     (org-babel-pick-name (cdr (assoc :colname-names params))
                                          (cdr (assoc :colnames params)))
                     (org-babel-pick-name (cdr (assoc :rowname-names params))
                                          (cdr (assoc :rownames params))))))
        (org-babel-result-cond result-params
          result
          (org-babel-scheme--table-or-string table))))))

(provide 'ob-mit-scheme)

;;; ob-mit-scheme.el ends here
