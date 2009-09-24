;;; twittering-mode.el --- Major mode for Twitter

;; Copyright (C) 2007 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;;         Norito Uehara <kirinbiiru+dev@gmail.com>
;; Created: Sep 4, 2007
;; Version: 0.8.1
;; Keywords: twitter web
;; URL: http://lambdarepos.svnrepository.com/share/trac.cgi/browser/lang/elisp/twittering-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; twittering-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;; URL : http://twitter.com/d00dle/statuses/577876082
;; * Status Input from Popup buffer and C-cC-c to POST.
;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)

(defconst twittering-mode-version "0.8.1")

(defvar twittering-footer "")
(defun twittering-update-footer ()
  (interactive)
  (twittering-update-footer-from-minibuffer))

(defun twittering-update-footer-from-minibuffer (&optional init-str)
  (if (null init-str) (setq init-str twittering-footer))
  (let ((footer init-str) (not-posted-p t))
    (setq footer (read-from-minibuffer "footer: " footer nil nil nil nil t))
    (setq twittering-footer footer)))


(defun twittering-mode-version ()
  "Display a message for twittering-mode version."
  (interactive)
  (let ((version-string
	 (format "twittering-mode-v%s" twittering-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar twittering-mode-map (make-sparse-keymap))

(defvar twittering-timer nil "Timer object for timeline refreshing will be
stored here. DO NOT SET VALUE MANUALLY.")

(defvar twittering-idle-time 20)

(defvar twittering-timer-interval 90)

(defvar twittering-username nil)

(defvar twittering-password nil)

(defvar twittering-user-page 1)
(defvar twittering-last-used-method-class nil)
(defvar twittering-last-used-method nil)

(defvar twittering-last-timeline-retrieved nil)

(defvar twittering-last-timeline-interactive nil)

(defvar twittering-new-tweets-count 0
  "Number of new tweets when `twittering-new-tweets-hook' is run")

(defvar twittering-new-tweets-hook nil
  "Hook run when new twits are received.

You can read `twittering-new-tweets-count' to get the number of new
tweets received when this hook is run.")

(defvar twittering-scroll-mode nil)
(make-variable-buffer-local 'twittering-scroll-mode)

(defvar twittering-jojo-mode nil)
(make-variable-buffer-local 'twittering-jojo-mode)

(defvar twittering-status-format nil)
(setq twittering-status-format "%i %S(%s)%p, %@:\n  %t // from %f%L%r")

(defvar twittering-user-format nil)
(setq twittering-user-format 
      "%i %S(%s)%p%F %L [Web: %u]\n[Friends: %k][Followers: %K]\n%d\n------------- last twit -------------\n%t")
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - " in reply to user"
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %f - source
;; %F - following(status formatではAPI戻り値が不定なので信用性なし)
;; %# - id

(defvar twittering-dm-format nil)
(setq twittering-dm-format 
      "%i %S(%s)%p, %@:\n  %t // from %L %F")

;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %F - following(status formatではAPI戻り値が不定なので信用性なし)
;; %# - id

(defvar twittering-buffer "*twittering*")
(defun twittering-buffer ()
  (twittering-get-or-generate-buffer twittering-buffer))

(defvar twittering-http-buffer "*twittering-http-buffer*")
(defun twittering-http-buffer ()
  (twittering-get-or-generate-buffer twittering-http-buffer))

(defvar twittering-timeline-data nil)
(defvar twittering-timeline-last-update nil)

(defvar twittering-user-data nil)
(defvar twittering-dm-data nil)

(defvar twittering-username-face 'twittering-username-face)
(defvar twittering-uri-face 'twittering-uri-face)

(defun twittering-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun assocref (item alist)
  (cdr (assoc item alist)))
(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

;;; Proxy
(defvar twittering-proxy-use nil)
(defvar twittering-proxy-server nil)
(defvar twittering-proxy-port 8080)
(defvar twittering-proxy-user nil)
(defvar twittering-proxy-password nil)

(defun twittering-toggle-proxy () ""
  (interactive)
  (setq twittering-proxy-use
	(not twittering-proxy-use))
  (message "%s %s"
	   "Use Proxy:"
	   (if twittering-proxy-use
	       "on" "off")))

(defun twittering-user-agent-default-function ()
  "Twittering mode default User-Agent function."
  (concat "Emacs/"
	  (int-to-string emacs-major-version) "." (int-to-string
						   emacs-minor-version)
	  " "
	  "Twittering-mode/"
	  twittering-mode-version))

(defvar twittering-sign-simple-string nil)

(defun twittering-sign-string-default-function ()
  "Tweet append sign string:simple "
  (if twittering-sign-simple-string
      (concat " [" twittering-sign-simple-string "]")
    ""))

(defvar twittering-user-agent-function 'twittering-user-agent-default-function)
(defvar twittering-sign-string-function 'twittering-sign-string-default-function)

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))

(defun twittering-sign-string ()
  "Return Tweet sign string."
  (funcall twittering-sign-string-function))

;;; to show image files

(defvar twittering-wget-buffer "*twittering-wget-buffer*")
(defun twittering-wget-buffer ()
  (twittering-get-or-generate-buffer twittering-wget-buffer))



(defun twittering-tmp-dir-name ()
  "return tmp-dir-name"
  (expand-file-name (concat "twmode-images-" (user-login-name))
		    temporary-file-directory))

(defvar twittering-tmp-dir
  (let ((tmp-dir (twittering-tmp-dir-name)))
    (when tmp-dir
      (if (not (file-directory-p tmp-dir))
	  (make-directory tmp-dir)))
    tmp-dir)
  "if not created tmp-dir, makedir and return it, else return tmp-dir"
  )

(defvar twittering-icon-mode nil "You MUST NOT CHANGE this variable
directory. You should change through function'twittering-icon-mode'")

(make-variable-buffer-local 'twittering-icon-mode)
(defun twittering-icon-mode (&optional arg)
  (interactive)
  (setq twittering-icon-mode
	(if twittering-icon-mode
	    (twittering-disable-icon-mode)
	  (twittering-enable-icon-mode))))

(defun twittering-disable-icon-mode (&optional arg)
  (if (null arg)
      nil
    (> (prefix-numeric-value arg) 0)))

(defun twittering-enable-icon-mode (&optional arg)
  (when (or (null arg)
	    (and arg (> (prefix-numeric-value arg) 0)))
    twittering-tmp-dir
    (twittering-render-timeline)))


(defun twittering-scroll-mode (&optional arg)
  (interactive)
  (setq twittering-scroll-mode
	(if (null arg)
	    (not twittering-scroll-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun twittering-jojo-mode (&optional arg)
  (interactive)
  (setq twittering-jojo-mode
	(if (null arg)
	    (not twittering-jojo-mode)
	  (> (prefix-numeric-value arg) 0))))

(defvar twittering-image-stack nil)

(defvar twittering-image-type-cache nil)

(defun twittering-image-type (file-name)
  (if (and (not (assoc file-name twittering-image-type-cache))
	   (file-exists-p file-name))
      (let* ((file-output (shell-command-to-string (concat "file -b " file-name)))
	     (file-type (cond
			 ((string-match "JPEG" file-output) 'jpeg)
			 ((string-match "PNG" file-output) 'png)
			 ((string-match "GIF" file-output) 'gif)
			 ((string-match "\\.jpe?g" file-name) 'jpeg)
			 ((string-match "\\.png" file-name) 'png)
			 ((string-match "\\.gif" file-name) 'gif)
			 (t nil))))
	(add-to-list 'twittering-image-type-cache `(,file-name . ,file-type))))
  (cdr (assoc file-name twittering-image-type-cache)))

(defun twittering-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))
(defun twittering-local-strftime (fmt string)
  (twittering-setftime fmt string nil))
(defun twittering-global-strftime (fmt string)
  (twittering-setftime fmt string t))


(defvar twittering-debug-mode t)
(defvar twittering-debug-buffer "*debug*")
(defun twittering-debug-buffer ()
  (twittering-get-or-generate-buffer twittering-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twittering-debug-mode
	   (with-current-buffer (twittering-debug-buffer)
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun twittering-debug-mode ()
  (interactive)
  (setq twittering-debug-mode
	(not twittering-debug-mode))
  (message (if twittering-debug-mode "debug mode:on" "debug mode:off")))

(if twittering-mode-map
    (let ((km twittering-mode-map))
      (define-key km "\C-c\C-f" 'twittering-friends-timeline)
      (define-key km "\C-c\C-r" 'twittering-replies-timeline)
      (define-key km "\C-c\C-g" 'twittering-public-timeline)
      (define-key km "\C-c\C-u" 'twittering-user-timeline)
      (define-key km "\C-c\C-s" 'twittering-update-status-interactive)
      (define-key km "\C-c\C-e" 'twittering-erase-old-statuses)
      (define-key km "\C-c\C-m" 'twittering-retweet)
      (define-key km "\C-m" 'twittering-enter)
      (define-key km "\C-c\C-l" 'twittering-update-lambda)
      (define-key km [mouse-1] 'twittering-click)
      (define-key km "\C-c\C-v" 'twittering-view-user-page)
      (define-key km "\C-c\C-z" 'twittering-update-footer)
      (define-key km "[" 'twittering-following-list)
      (define-key km "]" 'twittering-follower-list)
      (define-key km "{" 'twittering-other-user-following-list)
      (define-key km "}" 'twittering-other-user-follower-list)
      (define-key km "\C-c[" 'twittering-add-following)
      (define-key km "\C-c]" 'twittering-remove-following)
      (define-key km "\C-cd" 'twittering-received-direct-messages)
      (define-key km "\C-cD" 'twittering-sent-direct-messages)
      (define-key km "\C-d" 'twittering-send-direct-message)
      (define-key km "g" 'twittering-current-timeline-interactive)
      (define-key km "v" 'twittering-other-user-timeline)
      (define-key km "V" 'twittering-other-user-timeline-interactive)
      (define-key km "f" 'twittering-favorites)
      (define-key km "F" 'twittering-other-user-favorites)
      (define-key km "\C-cfa" 'twittering-add-favorite)
      (define-key km "\C-c\C-x" 'twittering-destroy-favorite)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twittering-goto-next-status)
      (define-key km "k" 'twittering-goto-previous-status)
      (define-key km " " 'scroll-up)
      (define-key km "b" 'scroll-down)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'twittering-goto-next-status-of-user)
      (define-key km "p" 'twittering-goto-previous-status-of-user)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'twittering-icon-mode)
      (define-key km "s" 'twittering-scroll-mode)
      (define-key km "t" 'twittering-toggle-proxy)
      (define-key km "\C-c\C-p" 'twittering-toggle-proxy)
      (define-key km "q" 'twittering-suspend)
      nil))

(defun twittering-keybind-message ()
  (let ((important-commands
	 '(("Timeline" . twittering-friends-timeline)
	   ("Replies" . twittering-replies-timeline)
	   ("Update status" . twittering-update-status-interactive)
	   ("Next" . twittering-goto-next-status)
	   ("Prev" . twittering-goto-previous-status))))
    (mapconcat (lambda (command-spec)
		 (let ((descr (car command-spec))
		       (command (cdr command-spec)))
		   (format "%s: %s" descr (key-description
					   (where-is-internal
					    command
					    overriding-local-map t)))))
	       important-commands ", ")))

;; (run-with-idle-timer
;;  0.1 t
;;  '(lambda ()
;;     (when (equal (buffer-name (current-buffer)) twittering-buffer)
;;       (message (twittering-keybind-message)))))

(defvar twittering-mode-syntax-table nil "")

(if twittering-mode-syntax-table
    ()
  (setq twittering-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twittering-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twittering-mode-syntax-table)
  )

(defun twittering-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface twittering-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twittering-username-face)
  (set-face-attribute 'twittering-username-face nil :underline t)
  (defface twittering-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twittering-uri-face nil :underline t)
  (add-to-list 'minor-mode-alist '(twittering-icon-mode " tw-icon"))
  (add-to-list 'minor-mode-alist '(twittering-scroll-mode " tw-scroll"))
  (add-to-list 'minor-mode-alist '(twittering-jojo-mode " tw-jojo"))
  )

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro twittering-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar twittering-mode-string "Twittering mode")

(defvar twittering-mode-hook nil
  "Twittering-mode hook.")

(defun twittering-mode ()
  "Major mode for Twitter
\\{twittering-mode-map}"
  (interactive)
  (switch-to-buffer (twittering-buffer))
  (kill-all-local-variables)
  (twittering-mode-init-variables)
  (use-local-map twittering-mode-map)
  (setq major-mode 'twittering-mode)
  (setq mode-name twittering-mode-string)
  (set-syntax-table twittering-mode-syntax-table)
  (run-hooks 'twittering-mode-hook)
  (font-lock-mode -1)
  (twittering-start))

;;;
;;; Basic HTTP functions
;;;

(defun twittering-clear-buffer ()
  "clear twittering-http-buffer"
  (save-excursion
    (set-buffer (twittering-http-buffer))
    (erase-buffer))
)

(defun twittering-setup-network (server port)
  ""
  (open-network-stream
   "network-connection-process" (twittering-http-buffer)
   server (string-to-number port))
)

(defun twittering-check-use-proxy ()
  "use-proxy?"
  (if (and twittering-proxy-use twittering-proxy-server)
      t
    nil))

(defun twittering-proxy-port-string ()
  "if proxy-port type is integer, cast to string"
  (if (integerp twittering-proxy-port)
      (int-to-string twittering-proxy-port)
    twittering-proxy-port))

(defun twittering-set-port ()
  "return port number"
  (if (twittering-check-use-proxy)
      (twittering-proxy-port-string)
    "80"))

(defun twittering-set-server ()
  "return server url"
  (if (twittering-check-use-proxy)
      twittering-proxy-server
    "twitter.com"))

(defun twittering-get-request-parameters (parameters)
  ""
  (concat "?"
	  (mapconcat
	   (lambda (param-pair)
	     (format "%s=%s"
		     (twittering-percent-encode (car param-pair))
		     (twittering-percent-encode (cdr param-pair))))
	   parameters
	   "&")))

(defun twittering-use-proxy-request (proxy-user proxy-password)
  ("Proxy-Connection: Keep-Alive" nl
   (when (and proxy-user proxy-password)
     (concat "Proxy-Authorization: Basic " 
	     (twittering-basic-authorization proxy-user proxy-password) nl))))

(defun twittering-basic-authorization (user password)
  (base64-encode-string (concat user ":" password)))

(defun twittering-http-header-base (http-method method-class method &optional parameters)
  (let ((nl "\r\n"))
    (concat http-method " http://twitter.com/" method-class "/" method ".xml"
	    (when parameters
	      (twittering-get-request-parameters parameters))
	    " HTTP/1.1" nl
	    "Host: twitter.com" nl
	    "User-Agent: " (twittering-user-agent) nl
	    "Authorization: Basic " (twittering-basic-authorization twittering-username (twittering-get-password)) nl)))
  
(defun twittering-create-request (http-method method-class method &optional parameters)
  "create http request, http-method is (GET|POST)"
  (let ((nl "\r\n"))
    (concat 
     (if parameters 
	 (twittering-http-header-base http-method method-class method parameters)
       (twittering-http-header-base http-method method-class method))
     (if (string= http-method "POST")
	 (concat
	   "Content-Type: text/plain" nl
	   "Content-Length: 0" nl))
	    
     (if (string= http-method "GET")
	 (concat
	   "Accept: text/xml" ",application/xml" ",application/xhtml+xml" ",application/html;q=0.9" ",text/plain;q=0.8" ",image/png,*/*;q=0.5" nl
	   "Accept-Charset: utf-8;q=0.7,*;q=0.7" nl))
     (when twittering-proxy-use
       (twittering-use-proxy-request proxy-user proxy-password)) nl)))

(defun twittering-http-method
  (http-method method-class method &optional noninteractive parameters sentinel contents)
  "Send HTTP POST request to twitter.com

METHOD-CLASS must be one of Twitter API method classes
 (statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel)
      (if (string= "GET" http-method)
	  (setq sentinel 'twittering-http-get-default-sentinel)
	(setq sentinel 'twittering-http-post-default-sentinel)))

  (twittering-clear-buffer)

  (let (proc server port
	     (proxy-user twittering-proxy-user)
	     (proxy-password twittering-proxy-password))
    (condition-case get-error
	(progn
	  (setq server (twittering-set-server)
		port (twittering-set-port))
	  
	  (setq proc
		(twittering-setup-network server port))
	  
;          (lexical-let ((sentinel sentinel) (noninteractive noninteractive))
;            (set-process-sentinel proc (lambda (&rest args) (apply sentinel noninteractive args))))
	  (set-process-sentinel proc sentinel)

	  (process-send-string
	   proc
	   (let (request)
	     (setq request
		   (twittering-create-request http-method method-class method parameters))
	     
	 (debug-print (concat http-method " Request\n" request))
	 request)))
      (error
       message (format "Failure: HTTP GET: %s" get-error)) nil)))


(defun twittering-is-valid-http-header (header)
  "if valid http header, return true"
  (if (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header)
	t
      nil))

(defun twittering-http-post-default-sentinel (proc stat &optional suc-msg)

  (condition-case err-signal
      (let ((header (twittering-get-response "head"))
	    ;; (body (twittering-get-response-body)) not used now.
	    (status nil))
	(string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" header)
	(setq status (match-string-no-properties 1 header))
	(case-string status
		     (("200 OK")
		      (message (if suc-msg suc-msg "Success: Post")))
		     (t (message status)))
	)
    (error (message (prin1-to-string err-signal))))
  )


(defun twittering-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (twittering-get-response "head"))
	(body (twittering-get-response "body"))
	(status nil))
    (if (twittering-is-valid-http-header header)
	(progn
	  (setq status (match-string-no-properties 1 header))
	  (case-string status
		       (("200 OK")
			(setq twittering-new-tweets-count
			      (count t (mapcar
					#'twittering-cache-status-datum
					(reverse (twittering-xmltree-to-status
						  body)))))
			(setq twittering-timeline-data
			      (sort twittering-timeline-data
				    (lambda (status1 status2)
				      (let ((created-at1
					     (twittering-created-at-to-seconds
					      (cdr (assoc 'created-at status1))))
					    (created-at2
					     (twittering-created-at-to-seconds
					      (cdr (assoc 'created-at status2)))))
					(> created-at1 created-at2)))))
			(if (and (> twittering-new-tweets-count 0)
				 noninteractive)
			    (run-hooks 'twittering-new-tweets-hook))
			(twittering-render-timeline)
			(twittering-start)
			(message (if suc-msg suc-msg "Success: Get.")))
;		       (t (message status))))
		       (t (message "Success"))))
      (message "Failure: Bad http response.")))
  )

(defun twittering-http-get-user-sentinel (proc stat &optional suc-msg)
  (let ((header (twittering-get-response "head"))
	(body (twittering-get-response "body"))
	(http-status nil))
    (if (twittering-is-valid-http-header header)
	(progn
	  (setq http-status (match-string-no-properties 1 header))
	  (case-string http-status
	   (("200 OK")
	    (setq twittering-new-tweets-count
		  (count t (mapcar
			    #'twittering-cache-user-datum
			    (reverse (twittering-xmltree-to-users
				      body)))))
	    (twittering-render-user)
	    (twittering-stop)
	    (message (if suc-msg suc-msg "Success: Get.")))
	   (t (message http-status))))
      (message "Failure: Bad http response."))))

(defun twittering-http-get-dm-sentinel (proc stat &optional suc-msg)
  (let ((header (twittering-get-response "head"))
	(body (twittering-get-response "body"))
	(http-status nil))
    (if (twittering-is-valid-http-header header)
	(progn
	  (setq http-status (match-string-no-properties 1 header))
	  (debug-print http-status)
	  (setq twittering-dm-data nil)
	  (case-string http-status
	   (("200 OK")
	    (setq twittering-new-tweets-count
		  (count t (mapcar
			    #'twittering-cache-dm-datum
			    (reverse (twittering-xmltree-to-dms
				      body)))))
	    (twittering-render-dms)
	    (twittering-stop)
	    (message (if suc-msg suc-msg "Success: Get.")))
	   (t (message http-status))))
      (message "Failure: Bad http response."))))


(defun twittering-created-at-to-seconds (created-at)
  (let ((encoded-time (apply 'encode-time (parse-time-string created-at))))
    (+ (* (car encoded-time) 65536)
       (cadr encoded-time))))


(defun twittering-render-timeline ()
  (with-current-buffer (twittering-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (twittering-format-status
		       status twittering-status-format))
	      (fill-region-as-paragraph
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    twittering-timeline-data)
      (if (and twittering-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twittering-scroll-mode (- (point-max) end) 0))))
    ))

(defun twittering-render-user ()
  (with-current-buffer (twittering-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (follow)
	      (insert (twittering-format-user
		       follow twittering-user-format))
	      (fill-region-as-paragraph
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    twittering-user-data)
      (if (and twittering-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twittering-scroll-mode (- (point-max) end) 0))))
    ))

(defun twittering-render-dms ()
  (with-current-buffer (twittering-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (dm)
	      (insert (twittering-format-dm
		       dm twittering-dm-format))
	      (fill-region-as-paragraph
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    twittering-dm-data)
      (if (and twittering-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twittering-scroll-mode (- (point-max) end) 0))))
    ))


(defun twittering-profile-image (profile-image-url)
  (let ((icon-string "\n  "))
    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
	(let* ((filename (match-string-no-properties 1 profile-image-url))
	      (fullpath (concat twittering-tmp-dir "/" filename)))
	  ;; download icons if does not exist
	  (if (file-exists-p fullpath)
	      t
	    (add-to-list 'twittering-image-stack profile-image-url))

	  (when (and icon-string twittering-icon-mode)
	    (set-text-properties
	     1 2 `(display
		   (image :type ,(twittering-image-type fullpath)
			  :file ,fullpath))
	     icon-string)
	    icon-string)))))


(defun twittering-format-status (status format-str)
  ;; Formatting strategy:
  ;; 
  ;; 1. Search the special character '%' in format-str, expand it with
  ;; corresponding string(such as username, image, description, ...),
  ;; and pushes it on 'result' until the end of format-str.
  ;; 2. concat strings in 'result' together
  ;;
  ;; Example:
  ;;  format-str: "%s, %@:\n %t", where screen name is "hayamiz",
  ;;    timestamp is "1 minute ago", and text is "hello, world"
  ;;  result: ("hello, world" ":\n " "1 minute ago" ", " "hayamiz")
  ;;
  (flet ((attr (key)
	       (assocref key status)))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)"
					  format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (list-push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))

	(case c
	  ((?s)                         ; %s - screen_name
	   (list-push (attr 'user-screen-name) result))
	  ((?S)                         ; %S - name
	   (list-push (attr 'user-name) result))
	  ((?i)                         ; %i - profile_image
	   (list-push (twittering-profile-image 
		       (attr 'user-profile-image-url)) result))
	  ((?d)                         ; %d - description
	   (list-push (attr 'user-description) result))
	  ((?l)                         ; %l - location
	   (list-push (attr 'user-location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'user-location)))
	     (unless (or (null location) (string= "" location))
	       (list-push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (list-push (attr 'user-url) result))
	  ((?j)                         ; %j - user.id
	   (list-push (attr 'user-id) result))
	  ((?r)				; %r - in_reply_to_status_id
	   (let ((reply-id (attr 'in-reply-to-status-id))
		 (reply-name (attr 'in-reply-to-screen-name)))
	     (unless (or (null reply-id) (string= "" reply-id)
			 (null reply-name) (string= "" reply-name))
	       (let ((in-reply-to-string (format "in reply to %s" reply-name))
		     (url (twittering-get-status-url reply-name reply-id)))
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight
			       face twittering-uri-face
			       uri ,url)
		  in-reply-to-string)
		 (list-push (concat " " in-reply-to-string) result)))))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'user-protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?F)                         ; %F - following
	   (let ((following (attr 'user-following)))
	     (if (string= "false" following)
		 (list-push "[未フォロー]" result)
	       (list-push "[フォロー済]" result))))

	  ((?c)                     ; %c - created_at (raw UTC string)
	   (list-push (attr 'created-at) result))
	  ((?C) ; %C{time-format-str} - created_at (formatted with
	   ; time-format-str)
	   (list-push (twittering-local-strftime
		       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
		       (attr 'created-at))
		      result))
	  ((?@)                         ; %@ - X seconds ago
	   (let ((created-at
		  (apply
		   'encode-time
		   (parse-time-string (attr 'created-at))))
		 (now (current-time)))
	     (let ((secs (+ (* (- (car now) (car created-at)) 65536)
			    (- (cadr now) (cadr created-at))))
		   time-string url)
	       (setq time-string
		     (cond ((< secs 5) "less than 5 seconds ago")
			   ((< secs 10) "less than 10 seconds ago")
			   ((< secs 20) "less than 20 seconds ago")
			   ((< secs 30) "half a minute ago")
			   ((< secs 60) "less than a minute ago")
			   ((< secs 150) "1 minute ago")
			   ((< secs 2400) (format "%d minutes ago"
						  (/ (+ secs 30) 60)))
			   ((< secs 5400) "about 1 hour ago")
			   ((< secs 84600) (format "about %d hours ago"
						   (/ (+ secs 1800) 3600)))
			   (t (format-time-string "%I:%M %p %B %d, %Y"
						  created-at))))
	       (setq url (twittering-get-status-url (attr 'user-screen-name)
						    (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face twittering-uri-face
			     uri ,url)
		time-string)
	       (list-push time-string result))))
	  ((?t)                         ; %t - text
	   (list-push (attr 'text) result))
	  ((?')                         ; %' - truncated
	   (let ((truncated (attr 'truncated)))
	     (when (string= "true" truncated)
	       (list-push "..." result))))
	  ((?f)                         ; %f - source
	   (list-push (attr 'source) result))
	  ((?#)                         ; %# - id
	   (list-push (attr 'id) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (list-push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username ,(attr 'user-screen-name)
					id ,(attr 'id)
					text ,(attr 'text))
			     formatted-status)
	formatted-status)
      )))


(defun twittering-format-user (status format-str)
  (flet ((attr (key)
	       (assocref key status)))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)"
					  format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (list-push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))
	(debug-print "before case in format-user")
	(debug-print c)
	(case c
	  ((?s)                         ; %s - screen_name
	   (list-push (attr 'screen-name) result))
	  ((?S)                         ; %S - name
	   (list-push (attr 'name) result))
	  ((?i)                         ; %i - profile_image
	   (list-push (twittering-profile-image 
		       (attr 'profile-image-url)) result))
	  ((?d)                         ; %d - description
	   (list-push (attr 'description) result))
	  ((?l)                         ; %l - location
	   (list-push (attr 'location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'location)))
	     (unless (or (null location) (string= "" location))
	       (list-push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (list-push (attr 'url) result))
	  ((?j)                         ; %j - user.id
	   (list-push (attr 'id) result))
	  ((?r)				; %r - in_reply_to_status_id
	   (let ((reply-id (attr 'status-in-reply-to-status-id))
		 (reply-name (attr 'status-in-reply-to-screen-name)))
	     (unless (or (null reply-id) (string= "" reply-id)
			 (null reply-name) (string= "" reply-name))
	       (let ((in-reply-to-string (format "in reply to %s" reply-name))
		     (url (twittering-get-status-url reply-name reply-id)))
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight
			       face twittering-uri-face
			       uri ,url)
		  in-reply-to-string)
		 (list-push (concat " " in-reply-to-string) result)))))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?F)                         ; %F - following
	   (let ((following (attr 'following)))
	     (if (string= "true" following)
		 (list-push "[フォロー済]" result)
	       (list-push "[未フォロー]" result))))

	  ((?k) ; friends count
	     (list-push (attr 'friends-count) result))
	  ((?K) ; friends count
	     (list-push (attr 'followers-count) result))

	  ((?t)                         ; %t - text
	   (list-push                   ;(clickable-text)
	    (attr 'status-text)
	    result))
	  ((?')                         ; %' - truncated
	   (let ((truncated (attr 'status-truncated)))
	     (when (string= "true" truncated)
	       (list-push "..." result))))
	  ((?f)                         ; %f - source
	   (list-push (attr 'status-source) result))
	  ((?#)                         ; %# - id
	   (list-push (attr 'status-id) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (debug-print "after case and while  in format-user")
      (list-push (substring format-str cursor) result)
      (let ((formatted-follow (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-follow)
			     `(username ,(attr 'screen-name)
					id ,(attr 'status-id)
					text ,(attr 'status-text))
			     formatted-follow)
	formatted-follow)
      )))


(defun twittering-format-dm (dm format-str)
  (flet ((attr (key)
	       (assocref key dm)))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)"
					  format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (list-push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))

	(case c
	  ((?s)                         ; %s - screen_name
	   (list-push (attr 'sender-screen-name) result))
	  ((?S)                         ; %S - name
	   (list-push (attr 'sender-name) result))
	  ((?i)                         ; %i - profile_image
	   (list-push (twittering-profile-image 
		       (attr 'sender-profile-image-url)) result))
	  ((?d)                         ; %d - description
	   (list-push (attr 'sender-description) result))
	  ((?l)                         ; %l - location
	   (list-push (attr 'sender-location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'sender-location)))
	     (unless (or (null location) (string= "" location))
	       (list-push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (list-push (attr 'sender-url) result))
	  ((?j)                         ; %j - user.id
	   (list-push (attr 'sender-id) result))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'sender-protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?F)                         ; %F - following
	   (let ((following (attr 'sender-following)))
	     (if (string= "false" following)
		 (list-push "[未フォロー]" result)
	       (list-push "[フォロー済]" result))))

	  ((?c)                     ; %c - created_at (raw UTC string)
	   (list-push (attr 'created-at) result))
	  ((?C) ; %C{time-format-str} - created_at (formatted with
	   ; time-format-str)
	   (list-push (twittering-local-strftime
		       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
		       (attr 'created-at))
		      result))
	  ((?@)                         ; %@ - X seconds ago
	   (let ((created-at
		  (apply
		   'encode-time
		   (parse-time-string (attr 'created-at))))
		 (now (current-time)))
	     (let ((secs (+ (* (- (car now) (car created-at)) 65536)
			    (- (cadr now) (cadr created-at))))
		   time-string url)
	       (setq time-string
		     (cond ((< secs 5) "less than 5 seconds ago")
			   ((< secs 10) "less than 10 seconds ago")
			   ((< secs 20) "less than 20 seconds ago")
			   ((< secs 30) "half a minute ago")
			   ((< secs 60) "less than a minute ago")
			   ((< secs 150) "1 minute ago")
			   ((< secs 2400) (format "%d minutes ago"
						  (/ (+ secs 30) 60)))
			   ((< secs 5400) "about 1 hour ago")
			   ((< secs 84600) (format "about %d hours ago"
						   (/ (+ secs 1800) 3600)))
			   (t (format-time-string "%I:%M %p %B %d, %Y"
						  created-at))))
	       (setq url (twittering-get-status-url (attr 'user-screen-name)
						    (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face twittering-uri-face
			     uri ,url)
		time-string)
	       (list-push time-string result))))
	  ((?t)                         ; %t - text
	   (list-push (attr 'text) result))
	  ((?#)                         ; %# - id
	   (list-push (attr 'id) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (list-push (substring format-str cursor) result)
      (let ((formatted-dm (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-dm)
			     `(username ,(attr 'sender-screen-name)
					id ,(attr 'id)
					text ,(attr 'text))
			     formatted-dm)
	formatted-dm)
      )))



(defun twittering-get-response (type &optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twittering-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twittering-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (if (equal type "head")
	  (substring content 0 (string-match "\r?\n\r?\n" content))
	(xml-parse-region (+ (string-match "\r?\n\r?\n" content)
			     (length (match-string 0 content)))
			  (point-max))))))

(defun twittering-data-var-is-null-or-cannot-find-id (data-var id)
  (if (or (null (symbol-value data-var))
	  (not (find-if
		(lambda (item)
		  (string= id (cdr (assq 'id item))))
		(symbol-value data-var))))
      t
    nil))


(defun twittering-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twittering-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twittering-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (twittering-data-var-is-null-or-cannot-find-id data-var id)
	(progn
	  (if twittering-jojo-mode
	      (twittering-update-jojo (cdr (assq 'user-screen-name
						 status-datum))
				      (cdr (assq 'text status-datum))))
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twittering-cache-user-datum (user-datum &optional data-var)
  "Cache status datum into data-var(default twittering-user-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twittering-user-data))
  (let ((id (cdr (assq 'id user-datum))))

    (if (twittering-data-var-is-null-or-cannot-find-id data-var id)
	(progn
	  (set data-var (cons user-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twittering-cache-dm-datum (dm-datum &optional data-var)
  "Cache status datum into data-var(default twittering-user-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twittering-dm-data))
  (let ((id (cdr (assq 'id dm-datum))))

    (if (twittering-data-var-is-null-or-cannot-find-id data-var id)
	(progn
	  (set data-var (cons dm-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twittering-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   in-reply-to-status-id
	   in-reply-to-screen-name
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-url
	   user-protected
	   user-following
	   regex-index)

      (setq id (assq-get 'id status-data))
      (setq text (twittering-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twittering-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq in-reply-to-status-id
	    (twittering-decode-html-entities
	     (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
	    (twittering-decode-html-entities
	     (assq-get 'in_reply_to_screen_name status-data)))
      (setq user-id (assq-get 'id user-data))
      (setq user-name (twittering-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (twittering-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (twittering-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (twittering-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      (setq user-following (assq-get 'following user-data))

      ;; make username clickable
      (twittering-clickable-text (concat "http://twitter.com/" user-screen-name)
				 user-name)

      ;; make screen-name clickable
      (twittering-clickable-text (concat "http://twitter.com/" user-screen-name)
				 user-screen-name)

      ;; make URI clickable
      (setq text (twittering-clickable-all-matched-string text "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"))

      ;; make @username clickable
      (setq text (twittering-clickable-all-matched-string text "@\\([_a-zA-Z0-9]+\\)"))


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
	  (let ((uri (match-string-no-properties 1 source))
		(caption (match-string-no-properties 2 source)))
	    (setq source caption)
	    (twittering-clickable-text uri caption)))

      ;; save last update time
      (setq twittering-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    in-reply-to-status-id
	    in-reply-to-screen-name
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-url
	    user-protected
	    user-following)))))


(defun twittering-follow-to-follow-datum (follow)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((follow-data (cddr follow))
	   id name screen-name location description profile-image-url url protected 
	   followers-count friends-count statuses-count following
	   (status-data (cddr (assq 'status follow-data)))
	   status-id status-text status-source status-truncated
	   status-in-reply-to-status-id status-in-reply-to-screen-name
	   status-in-reply-to-user-id)

      (setq id (assq-get 'id follow-data))
      (setq name (twittering-decode-html-entities
		  (assq-get 'name follow-data)))
      (setq screen-name (twittering-decode-html-entities
		    (assq-get 'screen_name follow-data)))
      (setq location (twittering-decode-html-entities
		    (assq-get 'location follow-data)))
      (setq description (twittering-decode-html-entities
		    (assq-get 'description follow-data)))
      (setq profile-image-url (assq-get 'profile_image_url follow-data))
      (setq url (assq-get 'url follow-data))
      (setq protected (assq-get 'protected follow-data))
      (setq followers-count (assq-get 'followers_count follow-data))
      (setq friends-count (assq-get 'friends_count follow-data))
      (setq statuses-count (assq-get 'statuses_count follow-data))
      (setq following (assq-get 'following follow-data))


      ; last status data
      (setq status-id (assq-get 'id status-data))
      (setq status-text (twittering-decode-html-entities
			 (assq-get 'text status-data)))
      (setq status-source (twittering-decode-html-entities
			   (assq-get 'status-source status-data)))
      (setq status-truncated (twittering-decode-html-entities
			      (assq-get 'truncated status-data)))
      (setq status-in-reply-to-status-id 
	    (assq-get 'in_reply_to_status_id status-data))
      (setq status-in-reply-to-screen-name
	    (twittering-decode-html-entities
	     (assq-get 'in-reply-to-screen-name status-data)))
      (setq status-in-reply-to-user-id 
	    (assq-get 'in-reply-to-user-id status-data))

      ;; make username clickable
      (twittering-clickable-text (concat "http://twitter.com/" screen-name)
				 name)

      ;; make screen-name clickable
      (twittering-clickable-text (concat "http://twitter.com/" screen-name)
				 screen-name)


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" status-source)
	  (let ((uri (match-string-no-properties 1 status-source))
		(caption (match-string-no-properties 2 status-source)))
	    (setq status-source caption)
	    (twittering-clickable-text uri caption)))

      ;; save last update time
;      (setq twittering-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id name screen-name location description profile-image-url url protected 
	    followers-count friends-count statuses-count following
	    status-id status-text status-source status-truncated
	    status-in-reply-to-status-id status-in-reply-to-screen-name
	    status-in-reply-to-user-id)))))

(defun twittering-dm-to-dm-datum (dm)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((dm-data (cddr dm))
	   id text created-at
	   (sender-data (cddr (assq 'sender dm-data)))
	   sender-id sender-name sender-screen-name sender-location
	   sender-description sender-profile-image-url sender-url
	   sender-protected sender-following
	   regex-index)

      (setq id (assq-get 'id dm-data))
      (setq text (twittering-decode-html-entities
		  (assq-get 'text dm-data)))
      (setq created-at (assq-get 'created_at dm-data))
      (setq sender-id (assq-get 'sender_id dm-data))
      (setq sender-name (twittering-decode-html-entities
		       (assq-get 'name sender-data)))
      (setq sender-screen-name (twittering-decode-html-entities
			      (assq-get 'screen_name sender-data)))
      (setq sender-location (twittering-decode-html-entities
			   (assq-get 'location sender-data)))
      (setq sender-description (twittering-decode-html-entities
			      (assq-get 'description sender-data)))
      (setq sender-profile-image-url (assq-get 'profile_image_url sender-data))
      (setq sender-url (assq-get 'url sender-data))
      (setq sender-protected (assq-get 'protected sender-data))
      (setq sender-following (assq-get 'following sender-data))

      ;; make username clickable
      (twittering-clickable-text (concat "http://twitter.com/" sender-screen-name)
				 sender-name)

      ;; make screen-name clickable
      (twittering-clickable-text (concat "http://twitter.com/" sender-screen-name)
				 sender-screen-name)

      ;; make URI clickable
      (setq text (twittering-clickable-all-matched-string text "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"))

      ;; make @username clickable
      (setq text (twittering-clickable-all-matched-string text "@\\([_a-zA-Z0-9]+\\)"))

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text created-at
	    sender-id sender-name sender-screen-name sender-location
	    sender-description
	    sender-profile-image-url
	    sender-url
	    sender-protected
	    sender-following)))))



(defun twittering-clickable-text (link-url text &optional start-point end-point)
  (if (null start-point)
      (setq start-point 0))
  (if (null end-point)
      (setq end-point (length text)))

  (add-text-properties
   start-point end-point
   `(mouse-face highlight
		uri ,link-url
		face twittering-username-face)
;		source ,text)
   text))

(defun twittering-set-url (matcher matched-string)
  (if (string-match "^@" matcher)
      (concat "http://twitter.com/" matched-string)
    matched-string))

(defun twittering-clickable-matched-string (text matcher)
  (if (string-match matcher text)
      (let ((matched-string (match-string-no-properties 1 text))
	    (match-start (match-beginning 0))
	    (match-end (match-end 0)))
	(twittering-clickable-text (twittering-set-url matcher matched-string)
				   text match-start match-end)
	(concat (substring text 0 match-end)
		(twittering-clickable-matched-string (substring text match-end) matcher)))
    text))

(defun twittering-clickable-all-matched-string (text matcher)
  (twittering-clickable-matched-string text matcher))


(defun twittering-xmltree-to-cons-cell (xml-attr)
  (if xml-attr
      (if (consp (car xml-attr))
	  (cons (car xml-attr) (twittering-xmltree-to-cons-cell (cdr xml-attr)))
	(twittering-xmltree-to-cons-cell (cdr xml-attr)))
    nil))

(defun twittering-xmltree-to-status (xmltree)
  (mapcar #'twittering-status-to-status-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings

	  (let ((statuses (cddr (car xmltree))))
	    (twittering-xmltree-to-cons-cell statuses))))


(defun twittering-xmltree-to-users (xmltree)
  (mapcar #'twittering-follow-to-follow-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
          (let ((ret nil) (users (reverse (cddr (car xmltree)))))
	    (setq max-lisp-eval-depth 1000)
	    (nreverse (twittering-xmltree-to-cons-cell users)))))

(defun twittering-xmltree-to-dms (xmltree)
  (mapcar #'twittering-dm-to-dm-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
          (let ((ret nil) (dms (reverse (cddr (car xmltree)))))
	    (nreverse (twittering-xmltree-to-cons-cell dms)))))

(defun twittering-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	  (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twittering-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twittering-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twittering-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (list-push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (setq result (twittering-ucs-to-string result number-entity)))

		  (letter-entity
		   (setq result (twittering-tag-decode result letter-entity)))

		  (t (list-push "?" result)))

	    (setq cursor (match-end 0))))

	(setq result (cons (substring encoded-str cursor) result))
	(apply 'concat (nreverse result)))
    ""))

(defun twittering-get-decoded-string (encoded-str)
  "作りかけ。twittering-decode-html-entities の while のあたりをこれに置き換える予定"
  (if (setq found-at
	    (string-match "&\\(#[0-9]+\\|[A-Za-z]+\\);"
;	    (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
			  encoded-str))
      (progn
	(let ((entity (match-string-no-properties 1 encoded-str)))
	  (concat
	   (twittering-get-unencoded-string-before-encoded-char encoded-str found-at)
	   (twittering-decode-char encoded-str found-at entity)
	   (twittering-get-decoded-string (substring encoded-str (match-end 0)))
	  )))
    encoded-str))

(defun twittering-ucs-to-string (result number-entity)
  (list-push (char-to-string 
	      (twittering-ucs-to-char (string-to-number number-entity))) result))

(defun twittering-tag-decode (result letter-entity)
  (cond ((string= "gt" letter-entity) (list-push ">" result))
	((string= "lt" letter-entity) (list-push "<" result))
	(t (list-push "?" result))))

(defun twittering-timer-action (func)
  (let ((buf (get-buffer twittering-buffer)))
    (if (null buf)
	(twittering-stop)
      (funcall func)
      )))

(defun twittering-update-status-if-not-blank (status &optional reply-to-id)
  (setq status (concat status twittering-footer))
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (setq status (concat status (twittering-sign-string)))
    (let ((parameters `(("status" . ,status)
			("source" . "twmode")
			,@(if reply-to-id
			      `(("in_reply_to_status_id"
				 . ,reply-to-id))))))
      (twittering-http-method "POST" "statuses" "update" nil parameters))
    t))

(defun twittering-update-status-from-minibuffer (&optional init-str
							   reply-to-id)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (while not-posted-p
      (setq status (read-from-minibuffer "status: " status nil nil nil nil t))
      (setq not-posted-p
	    (not (twittering-update-status-if-not-blank status reply-to-id))))
    ))

(defun twittering-send-direct-message-if-not-blank (dm &optional username)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" dm)
      nil
    (setq dm (concat dm (twittering-sign-string)))
    (string-match "^\\(@[-_a-z0-9]+: \\)?\\(.*\\)$" dm)
    (if (null username)
	(setq username (match-string-no-properties 1 dm)))
    (setq text (match-string-no-properties 2 dm))
    (let ((parameters `(("user" . ,username)
			("source" . "twmode")
			("text" . ,text))))
      (twittering-http-method "POST" "direct_messages" "new" parameters))
    t))


(defun twittering-send-direct-message-from-minibuffer (&optional init-dm
							   username)
  (if (null init-dm) (setq init-dm ""))
  (let ((dm init-dm) (not-posted-p t))
    (while not-posted-p
      (setq dm (read-from-minibuffer "DM " dm nil nil nil nil t))
      (setq not-posted-p
	    (not (twittering-send-direct-message-if-not-blank dm username))))
    ))

(defun twittering-add-favorite ()
  (interactive)
  (let ((id (get-text-property (point) 'id)
	    ))
    (when id
      (twittering-http-method "POST"
       "favorites" (concat "create/" id)
       ))))

(defun twittering-destroy-favorite ()
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (when id
      (twittering-http-method "POST"
       "favorites" (concat "destroy/" id)
       ))))


(defun twittering-add-following ()
  (interactive)
  (let ((screen-name (get-text-property (point) 'username)))
    (debug-print screen-name)
    (when screen-name
      (twittering-http-method "POST"
       "friendships" (concat "create/" screen-name)
       ))))

(defun twittering-remove-following ()
  (interactive)
  (let ((screen-name (get-text-property (point) 'username)))
    (when screen-name
      (twittering-http-method "POST"
       "friendships" (concat "destroy/" screen-name)
       ))))


(defun twittering-update-lambda ()
  (interactive)
  (twittering-http-method "POST"
   "statuses" "update"
   `(("status" . "\xd34b\xd22b\xd26f\xd224\xd224\xd268\xd34b")
     ("source" . "twmode"))))

(defun twittering-update-jojo (usr msg)
  (if (string-match "\xde21\xd24b\\(\xd22a\xe0b0\\|\xdaae\xe6cd\\)\xd24f\xd0d6\\([^\xd0d7]+\\)\xd0d7\xd248\xdc40\xd226"
		    msg)
      (twittering-http-method "POST"
       "statuses" "update"
       `(("status" . ,(concat
		       "@" usr " "
		       (match-string-no-properties 2 msg)
		       "\xd0a1\xd24f\xd243!?"))
	 ("source" . "twmode")))))

;;;
;;; Commands
;;;

(defun twittering-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twittering-current-timeline-noninteractive))
  (if twittering-timer
      nil
    (setq twittering-timer
	  (run-at-time "0 sec"
		       twittering-timer-interval
		       #'twittering-timer-action action))))

(defun twittering-stop ()
  (interactive)
  (if twittering-timer
    (cancel-timer twittering-timer))
  (setq twittering-timer nil))

(defun twittering-get-timeline (method &optional noninteractive id)
  (if (not (eq twittering-last-timeline-retrieved method))
      (setq twittering-timeline-last-update nil
	    twittering-timeline-data nil))
  (setq twittering-last-used-method method)
  (setq twittering-last-used-method-class "statuses")
  (setq twittering-last-timeline-retrieved method)
  (setq twittering-user-page 1)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (if id
          (twittering-http-method "GET" "statuses" method noninteractive
                               `(("max_id" . ,id)
                                 ("count" . "20")))
        (if (not twittering-timeline-last-update)
            (twittering-http-method "GET" "statuses" method noninteractive)
          (let* ((system-time-locale "C")
                 (since
                  (twittering-global-strftime
                   "%a, %d %b %Y %H:%M:%S GMT"
                   twittering-timeline-last-update)))
            (twittering-http-method "GET" "statuses" method noninteractive
                                 `(("since" . ,since))))))))
  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (twittering-get-image-stack))))


(defun twittering-get-favorites (username &optional noninteractive id)
  (if (or (not (eq twittering-last-used-method username)) 
	  (not (eq twittering-last-used-method-class "favorites")))
      (setq twittering-timeline-last-update nil
	    twittering-timeline-data nil))
  (setq twittering-last-used-method-class "favorites")
  (setq twittering-last-used-method username)
  (setq twittering-last-timeline-retrieved username)
  (setq twittering-user-page 1)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (if id
	  (twittering-http-method "GET" "favorites" username noninteractive
				  `(("max_id" . ,id)
				    ("count" . "20")))
	(if (not twittering-timeline-last-update)
	    (twittering-http-method "GET" "favorites" username noninteractive)
	  (let* ((system-time-locale "C")
		 (since
		  (twittering-global-strftime
		   "%a, %d %b %Y %H:%M:%S GMT"
		   twittering-timeline-last-update)))
	    (twittering-http-method "GET" "favorites" username noninteractive
				    `(("since" . ,since))))))))

  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (twittering-get-image-stack))))

(defun twittering-get-followers (username &optional noninteractive page)
  (if (or (not (eq twittering-last-used-method username)) 
	  (not (string= twittering-last-used-method-class "followers")))
      (setq twittering-user-last-update nil
	    twittering-user-data nil
	    twittering-user-page 1))

  (setq twittering-last-used-method-class "followers")
  (setq twittering-last-used-method username)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (if page
	  (twittering-http-method "GET" 
				  "statuses/followers" 
				  username 
				  noninteractive 
				  `(("page" . ,(number-to-string page)))
				  'twittering-http-get-user-sentinel)
	(twittering-http-method "GET" 
				"statuses/followers" 
				username 
				noninteractive 
				""
				'twittering-http-get-user-sentinel))))


  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (twittering-get-image-stack))))

(defun twittering-get-followings (username)
  (setq twittering-user-data nil)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (twittering-http-method "GET" "statuses/friends" username noninteractive "" 'twittering-http-get-user-sentinel)))
  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (twittering-get-image-stack))))

(defun twittering-get-received-direct-messages ()
  (setq twittering-timeline-last-update nil
	twittering-timeline-data nil)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (twittering-http-method "GET" "" "direct_messages" noninteractive "" 'twittering-http-get-dm-sentinel)))

  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (twittering-get-image-stack))))


(defun twittering-get-sent-direct-messages ()
  (setq twittering-timeline-last-update nil
	twittering-timeline-data nil)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (twittering-http-method "GET" "direct_messages" "sent" noninteractive "" 'twittering-http-get-dm-sentinel)))

  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (twittering-get-image-stack))))

(defun twittering-get-image-stack ()
  (let ((proc
	 (apply
	  #'start-process
	  "wget-images"
	  (twittering-wget-buffer)
	  "wget"
	  (format "--directory-prefix=%s" twittering-tmp-dir)
	  "--no-clobber"
	  "--quiet"
	  twittering-image-stack)))
    (set-process-sentinel
     proc
     (lambda (proc stat)
       (clear-image-cache)
       (save-excursion
	 (set-buffer (twittering-wget-buffer))
	 )))))

(defun twittering-received-direct-messages ()
  (interactive)
  (twittering-get-received-direct-messages)
)
(defun twittering-sent-direct-messages()
  (interactive)
  (twittering-get-sent-direct-messages)
)
;(defun twittering-new-direct-message())
;(defun twittering-remove-direct-message())
(defun twittering-following-list()
  (interactive)
  (twittering-get-followings twittering-username)
)

(defun twittering-follower-list()
  (interactive)
  (twittering-get-followers twittering-username)
)

(defun twittering-other-user-following-list()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
	(twittering-get-followings username)
      (message "No user selected"))))


(defun twittering-other-user-follower-list()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
	(twittering-get-followers username)
      (message "No user selected"))))

(defun twittering-favorites ()
  (interactive)
  (twittering-get-favorites twittering-username))

(defun twittering-other-user-favorites ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
	(twittering-get-favorites username)
      (message "No user selected"))))

(defun twittering-friends-timeline ()
  (interactive)
  (twittering-get-timeline "friends_timeline"))

(defun twittering-replies-timeline ()
  (interactive)
  (twittering-get-timeline "replies"))

(defun twittering-public-timeline ()
  (interactive)
  (twittering-get-timeline "public_timeline"))

(defun twittering-user-timeline ()
  (interactive)
  (twittering-get-timeline "user_timeline"))

;(defun twittering-current-timeline-interactive ()
;  (interactive)
;  (setq twittering-last-timeline-interactive t)
;  (twittering-current-timeline))


(defun twittering-current-timeline-noninteractive ()
  (twittering-current-timeline t))

(defun twittering-current-timeline (&optional noninteractive)
  (interactive)
  (twittering-set-last-timeline-unless-set)
  (twittering-get-timeline twittering-last-timeline-retrieved noninteractive))


(defun twittering-update-status-interactive ()
  (interactive)
  (twittering-update-status-from-minibuffer))

(defun twittering-set-last-timeline-unless-set ()
  (if (not twittering-last-timeline-retrieved)
      (setq twittering-last-timeline-retrieved "friends_timeline")))

(defun twittering-erase-old-statuses ()
  (interactive)
  (setq twittering-timeline-data nil)
  (twittering-set-last-timeline-unless-set)
  (if (not twittering-timeline-last-update)
      (twittering-http-method "GET" "statuses" twittering-last-timeline-retrieved)
    (let* ((system-time-locale "C")
	   (since
	    (twittering-global-strftime
	     "%a, %d %b %Y %H:%M:%S GMT"
	     twittering-timeline-last-update)))
      (twittering-http-method "GET" "statuses" twittering-last-timeline-retrieved
			   `(("since" . ,since))))))

(defun twittering-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(uri (get-text-property (point) 'uri))
	(uri-in-text (get-text-property (point) 'uri-in-text)))
    (if uri-in-text
        (browse-url uri-in-text)
      (if username
	  (twittering-update-status-from-minibuffer (concat "@" username " ") id)
	(if uri
	    (browse-url uri))))))

(defun twittering-retweet ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(text (get-text-property (point) 'text)))
    (when username
	(twittering-update-status-from-minibuffer
	 (concat "RT @" username ": " text) id))))

(defun twittering-send-direct-message ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (when username
	(twittering-send-direct-message-from-minibuffer
	 (concat "@" username ": ") username))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-other-user-timeline ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
	(twittering-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twittering-other-user-timeline-interactive ()
  (interactive)
  (let ((username (read-from-minibuffer "user: " (get-text-property (point) 'username))))
    (if (> (length username) 0)
	(twittering-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twittering-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twittering-update-status-from-minibuffer (concat "@" username " ")))))

(defun twittering-get-username ()
  (or twittering-username
      (setq twittering-username (read-string "your twitter username: "))))

(defun twittering-get-password ()
  (or twittering-password
      (setq twittering-password (read-passwd "your twitter password: "))))

(defun twittering-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twittering-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (let ((id (get-text-property (point) 'id)))
        (if id
	    (case-string twittering-last-used-method-class
;			 (("followers")
;			  (setq twittering-user-page (+ twittering-user-page 1))
;			  (twittering-get-followers twittering-last-used-method
;						    nil twittering-user-page))
			 (("statuses")
			  (twittering-get-timeline twittering-last-used-method
						   nil id))
			 (("favorites")
			  (twittering-get-favorites twittering-last-used-method
						    nil id))
			 (t (message "End of buffer"))

))))))


(defun twittering-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twittering-username-face)))
	(setq pos (next-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twittering-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (twittering-get-previous-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "Start of status."))))

(defun twittering-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twittering-username-face)))
	(setq pos (previous-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twittering-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
	(pos (twittering-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq pos (twittering-get-next-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twittering-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
	(pos (twittering-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq pos (twittering-get-previous-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twittering-get-username-at-pos (pos)
  (let ((start-pos pos)
	(end-pos))
    (catch 'not-found
      (while (eq (get-text-property start-pos 'face) twittering-username-face)
	(setq start-pos (1- start-pos))
	(when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun twittering-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%s" username id))

(defun twittering-suspend ()
  "Suspend twittering-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun twit ()
  "Start twittering-mode."
  (interactive)
  (twittering-mode))

(provide 'twittering-mode)
;;; twittering.el ends here
