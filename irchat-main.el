;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-main.el,v 3.47 2009/07/13 19:53:42 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))
(eval-and-compile  
  (require 'irchat-filter)
  (require 'irchat-vars)
  (require 'irchat-utf8))

;; Define hooks for each IRC message the server might send us.
;; The newer IRC servers use numeric reply codes instead of words.

(defvar irchat-msg-list
  '(channel error invite linreply msg namreply nick ping pong
    privmsg quit topic wall whoreply kill wallops mode kick part join
    200 203 204 205 206 209 211 212 213 214 215 216 217 218 219
    301 311 312 313 314 315 317 318 321 322 323 331 332 341 
    351 352 353 361 364 365 366 367 368 371 372 381 382 391
    401 402 403 411 412 421 431 432 433 441 451 461 462 463 
    464 465 471 472 473 474 481 482 491)
  "A list of the IRC messages and numeric reply codes irchat can handle.")

(mapcar (function
	 (lambda (sym)
	   ;; not interned at irchat-obarray to get completing reads.
	   (eval (list 'defvar (intern (concat "irchat-"
					       (prin1-to-string sym)
					       "-hook"))
		       nil
		       (concat "*A hook that is executed when the IRC "
			       "message \"" (upcase (prin1-to-string sym))
			       "\" is received.
The hook function is called with two arguments, PREFIX and REST-OF-LINE.
It should return non NIL if no further processing of the message is to be
carried out.")))
	   (eval (list 'defvar (intern (concat "irchat-after-"
					       (prin1-to-string sym)
					       "-hook"))
		       nil
		       (concat "*A hook that is executed after the IRC "
			       "message \"" (upcase (prin1-to-string sym))
			       "\" has been received and handled.
The hook function is called with two arguments, PREFIX and REST-OF-LINE.
It should return non NIL if no further processing of the hook is to be
carried out.")))))
	irchat-msg-list)

(defvar irchat-Command-mode-map nil)
(defvar irchat-Dialogue-mode-map nil)
(defvar irchat-Client-query-map nil)
(defvar irchat-Crypt-map nil)

(put 'irchat-Command-mode 'mode-class 'special)
(put 'irchat-Dialogue-mode 'mode-class 'special)

(defvar irchat-Client-query-keys
  '(("v" 	irchat-Command-client-version)
    ("u" 	irchat-Command-client-userinfo)
    ("h" 	irchat-Command-client-help)
    ("\C-b"     irchat-Command-show-online)
    ("c" 	irchat-Command-client-clientinfo)
    ("g" 	irchat-Command-client-generic)
    ("p" 	irchat-Command-client-ping)
    ("x" 	irchat-Command-client-x-face)
    ("X" 	irchat-Command-client-x-face-from-minibuffer)
    ("\C-x" 	irchat-Command-client-x-face-from-commandbuffer)
    ("U" 	irchat-Command-client-userinfo-from-minibuffer)
    ("\C-u" 	irchat-Command-client-userinfo-from-commandbuffer))
  "Key definition table for Client-query-map")
  
(defvar irchat-Dialogue-keys
  '(("\C-?" 	scroll-down)
    ("\C-h" 	scroll-down)
    (" " 	scroll-up)
    ("$" 	end-of-buffer)
    ("/" 	irchat-Command-generic)
    (">" 	end-of-buffer)
    ("<" 	beginning-of-buffer)
    ("a" 	irchat-Command-away)
    ("f" 	irchat-Dialogue-freeze)
    ("M" 	irchat-Dialogue-ownfreeze)
    ("i" 	irchat-Command-invite)
    ("j" 	irchat-Command-join)
    ("l" 	irchat-Command-load-vars)
    ("s" 	irchat-Command-save-vars)
    ("m" 	irchat-Dialogue-enter-message)
    ("n" 	irchat-Command-names)
    ("o" 	other-window)
    ("p" 	irchat-Command-part)
    ("P" 	irchat-Command-toggle-private)
    ("r" 	irchat-Command-reconfigure-windows)
    ("x" 	irchat-Dialogue-tag-line)
    ("t" 	irchat-Command-find-timestamp)
    ("T" 	irchat-Command-timestamp)
    ("w" 	irchat-Command-who)
    ("1"        irchat-Command-select-channel-1)
    ("2"        irchat-Command-select-channel-2)
    ("3"        irchat-Command-select-channel-3)
    ("4"        irchat-Command-select-channel-4)
    ("5"        irchat-Command-select-channel-5)
    ("6"        irchat-Command-select-channel-6)
    ("7"        irchat-Command-select-channel-7)
    ("8"        irchat-Command-select-channel-8)
    ("9"        irchat-Command-select-channel-9)
    ("0"        irchat-Command-select-channel-10)
    ("\C-m" 	irchat-Command-message)
;    ("%t"	irchat-Command-toggle-crypt)
;    ("%d"	irchat-Command-set-default-key)
;    ("%a"	irchat-Command-add-new-key)
    )
  "Key definition table for Dialogue mode")


(defvar irchat-Crypt-keys
  '(("t"	irchat-Command-toggle-crypt)
    ("k"	irchat-Command-set-default-key)
    ("a"	irchat-Command-add-new-key)
    ("d"	irchat-Command-delete-key))
  "Key definition table for crypt keys (Dialogue and Command mode).")

(defvar irchat-Command-keys
  '(("\C-cg" 	irchat-Command-dcc-receive)
    ("\C-c\C-a" irchat-Command-send-action)
    ("\C-cs" 	irchat-Command-dcc-send)
    ("\C-cG" 	irchat-Command-dcc-list)
    ("\C-m"  	irchat-Command-enter-message)
    ("\C-j"  	irchat-Command-enter-message)
    ("\M-\C-m" 	irchat-Command-enter-message-opposite-crypt-mode)
    ("\M-\C-j" 	irchat-Command-enter-message-opposite-crypt-mode)
    ("\C-cF" 	irchat-Command-send-file)
    ("\C-c\C-c" irchat-Client-query-prefix)
    ("\C-c\C-d" irchat-Command-debug)
    ("\C-c\C-i" irchat-Command-ison)
    ("\C-c\C-l" irchat-Command-redisplay)
    ("\C-c\C-n" irchat-Command-names)
    ("\C-c\C-r" irchat-Command-caesar-line)
    ("\C-c\C-u" irchat-Command-userhost)
    ("\C-c\C-y" irchat-Command-yank-send)
    ("\C-c\C-?" irchat-Command-scroll-down)
    ("\C-c " 	irchat-Command-scroll-up)
    ("\C-c!" 	irchat-Command-exec)
    ("\C-c2" 	irchat-Command-private-conversation)
    ("\C-ca" 	irchat-Command-away)
    ("\C-cc" 	irchat-Command-inline)
    ("\C-cf" 	irchat-Command-finger)
    ("\C-c\C-f" irchat-Command-freeze)
    ("\C-cM"	irchat-Command-ownfreeze)
    ("\C-ci" 	irchat-Command-invite)
    ("\C-cj" 	irchat-Command-join)
    ("\C-c\C-p" irchat-Command-part)
    ("\C-ck" 	irchat-Command-ignore)
    ("\C-cK" 	irchat-Command-ignore-by-regexp)
    ("\C-c\M-k"	irchat-Command-global-ignore)
    ("\C-c\C-k" irchat-Command-kick)
    ("\C-cl" 	irchat-Command-list)
    ("\C-cL" 	irchat-Command-load-vars)
    ("\C-cS" 	irchat-Command-save-vars)
    ("\C-cm" 	irchat-Command-message)
    ("\C-c\C-m" irchat-Command-modec)
    ("\C-cn" 	irchat-Command-nickname)
    ("\C-cp" 	irchat-Command-mta-private)
    ("\C-cq" 	irchat-Command-quit)
    ("\C-cr" 	irchat-Command-reconfigure-windows)
    ("\C-ct" 	irchat-Command-topic)
    ("\C-cT" 	irchat-Command-timestamp)
    ("\C-c\C-t" irchat-Command-find-timestamp)
    ("\C-cP"	irchat-Command-toggle-private)
    ("\C-cu" 	irchat-Command-lusers)
    ("\C-c\C-v"	irchat-Command-scroll-up)
    ("\C-cw" 	irchat-Command-who)
    ("\C-cW" 	irchat-Command-wait)
    ("\C-c\C-x"	irchat-Command-scroll-down)
    ("\C-c|" 	irchat-Command-show-last-kill)
    ("\C-c/" 	irchat-Command-generic)
    ("\C-i" 	irchat-Command-complete)
    ("\C-[\C-i" lisp-complete-symbol)
    ("\C-c$" 	irchat-Command-eod-buffer)
    ("\C-c>" 	irchat-Command-push)
    ("\C-c<" 	irchat-Command-pop))
  "Key definition table for Command mode")
    
(defun irchat-define-keys (map keys)
  (mapcar (function (lambda (keydef)
		      (define-key map (car keydef) (car (cdr keydef)))))
	  keys))

(if irchat-Client-query-map
    nil
  (define-prefix-command 'irchat-Client-query-map)
  (setq irchat-Client-query-map (make-keymap))
  (irchat-define-keys irchat-Client-query-map irchat-Client-query-keys)
  (fset 'irchat-Client-query-prefix irchat-Client-query-map))

(if irchat-Dialogue-mode-map
    nil
  (setq irchat-Dialogue-mode-map (make-keymap))
  (suppress-keymap irchat-Dialogue-mode-map)
  (irchat-define-keys irchat-Dialogue-mode-map irchat-Dialogue-keys))

(if irchat-Crypt-map
    nil
  (setq irchat-Crypt-map (make-sparse-keymap))
  (irchat-define-keys irchat-Crypt-map irchat-Crypt-keys)
  (define-key irchat-Dialogue-mode-map "%" irchat-Crypt-map))

(if irchat-Command-mode-map
    nil
  (setq irchat-Command-mode-map (make-sparse-keymap))
  (irchat-define-keys irchat-Command-mode-map irchat-Command-keys)
  (define-key irchat-Command-mode-map "\C-c%" irchat-Crypt-map)
  (if irchat-want-traditional
      (define-key irchat-Command-mode-map "/" 'irchat-Command-irc-compatible)))

(defun irchat-read-nickname (str)
  (substring str 0 (min 9 (length str))))

;;;
;;;
;;;

(defun irchat-start-server (&optional confirm)
  "Open network stream to remote irc server.
If optional argument CONFIRM is non-nil, ask the host that the server
is running on."
  (if (irchat-server-opened)
      ;; Stream is already opened.
      nil
    ;; Open IRC server.
    (if (or
	 (and confirm
	      (not (eq confirm 'always)))
	 (null irchat-server))
	(setq irchat-server
	      (read-string "IRC server: " irchat-server)))
    (if (and confirm
	     (not (eq confirm 'always))
	     irchat-ask-for-nickname)
	(setq irchat-nickname
	      (irchat-read-nickname
	       (read-string "Enter your nickname: " irchat-nickname))))
    (if (eq confirm 'always)
	(setq irchat-nickname
	      (irchat-read-nickname
	       (concat irchat-nickname irchat-grow-tail))))
    ;; If no server name is given, local host is assumed.
    (if (string-equal irchat-server "")
	(setq irchat-server (system-name)))
    (message "Connecting to IRC server on %s..." irchat-server)
    (cond ((irchat-open-server irchat-server irchat-service))
	  ((and (stringp irchat-status-message-string)
		(> (length irchat-status-message-string) 0))
	   ;; Show valuable message if available.
	   (error irchat-status-message-string))
	  (t (error "Cannot open IRC server on %s" irchat-server)))))


(defun irchat-open-server (host &optional service)
  "Open chat server on HOST.
If HOST is nil, use value of environment variable \"IRCSERVER\".
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "IRCSERVER")))
	(status nil))
    (setq irchat-status-message-string "")
    (cond ((and host (irchat-open-server-internal host service))
	   (if irchat-pass
	       (irchat-send "PASS %s" irchat-pass))
	   (irchat-send "USER %s %s %s :%s"
			(or (getenv "IRCUSER")
			    (user-real-login-name))
			(or (getenv "IRCHOST")
			    irchat-system-fqdname)
			irchat-server
			(or (getenv "IRCNAME")
			    (getenv "NAME")
			    (user-full-name)))
	   (if (not irchat-real-nickname)
	       (setq irchat-real-nickname irchat-nickname)
	     (let ((match (string-match "[^_]_+$" irchat-real-nickname)))
	       (if (and irchat-autoremove-virtual-penis
			match)
		   (setq irchat-real-nickname
			 (substring irchat-real-nickname 0 (+ match 1))))))
	   (setq irchat-old-nickname irchat-real-nickname)
	   (setq irchat-nick-accepted 'sent)
	   (irchat-send "NICK %s" irchat-real-nickname)
	   ; (irchat-send "PING %s" host)
	   (setq status t)
	   (if (setq status (irchat-wait-for-response ".*"))
	       (progn
		 (set-process-sentinel irchat-server-process
				       'irchat-sentinel)
		 (set-process-filter irchat-server-process
				     'irchat-filter))
	     ;; We have to close connection here, since the function
	     ;;  `irchat-server-opened' may return incorrect status.
	     (irchat-close-server-internal)))
	  ((null host)
	   (setq irchat-status-message-string "IRC server is not specified.")))
    status))


(defun irchat-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and irchat-server-process
       (memq (process-status irchat-server-process) '(open run))))


(defun irchat-open-server-internal (host &optional service)
  "Open connection to chat server on HOST by SERVICE (default is irc)."
  (condition-case err 
      (save-excursion
	;; Initialize communication buffer.
	(setq irchat-server-buffer (get-buffer-create " *IRC*"))
	(set-buffer irchat-server-buffer)
	(kill-all-local-variables)
	(irchat-buffer-disable-undo (current-buffer))
	(erase-buffer)
	(setq irchat-server-process
	      (open-network-stream "IRC" (current-buffer)
				   host (or service "irc")))
	(setq irchat-server-name host)
	(run-hooks 'irchat-server-hook)
	;; Return the server process.
	irchat-server-process)
    (error (message (car (cdr err)))
	   nil)))

(defun irchat-close-server ()
  "Close chat server."
  (unwind-protect
      (progn
	;; Un-set default sentinel function before closing connection.
	(and irchat-server-process
	     (eq 'irchat-sentinel
		 (process-sentinel irchat-server-process))
	     (set-process-sentinel irchat-server-process nil))
	;; We cannot send QUIT command unless the process is running.
	(if (irchat-server-opened)
	    (irchat-send "QUIT")))
    (irchat-close-server-internal)))


(defun irchat-close-server-internal ()
  "Close connection to chat server."
  (if irchat-server-process
      (delete-process irchat-server-process))
  (if irchat-server-buffer
      (kill-buffer irchat-server-buffer))
  (setq irchat-server-buffer nil
	irchat-server-process nil))


(defvar irchat-timers nil
  "Symbol name to store timer, timer-function and timer-interval.")
(defvar irchat-timers-list-initialized-p nil
  "Are irchat internal timers in place?")

(defun irchat (&optional confirm)
  "Connect to the IRC server and start chatting.
If optional argument CONFIRM is non-nil, ask which IRC server to connect.
If already connected, just pop up the windows."
  (interactive "P")
  (if (file-exists-p (expand-file-name irchat-variables-file))
      (progn
	(load (expand-file-name irchat-variables-file))
	(irchat-append-obsolete-vars)))
  (if (irchat-server-opened)
      (irchat-configure-windows)
    (if (irchat-crypt-support-p)
	(progn
	  (message "Initializing crypt keys...")
	  (irchat-init-crypt)
	  (message "Initializing crypt keys...done")))
    (unwind-protect
	(progn
	  (switch-to-buffer (irchat-get-buffer-create irchat-Command-buffer))
	  (irchat-Command-mode)
	  (irchat-start-server confirm))
      (if (not (irchat-server-opened))
	  (irchat-Command-quit)
	;; IRC server is successfully open. 
	(setq mode-line-process (format " {%s}" irchat-server))
	(let ((buffer-read-only nil))
	  (if (not irchat-keep-buffers)
	      (erase-buffer))
	  (sit-for 0))
	(if (irchat-frozen (car irchat-D-buffer))
	    (progn
	      (setq irchat-freeze-indicator "-")
	      (irchat-freeze-toggle (car irchat-D-buffer))))
	(irchat-set-crypt-indicator)
	(irchat-Dialogue-setup-buffer)
	(irchat-Private-setup-buffer)
	(irchat-KILLS-setup-buffer)
	(irchat-IGNORED-setup-buffer)
	(irchat-WALLOPS-setup-buffer)
	(if (irchat-crypt-support-p)
	    (irchat-CRYPT-setup-buffer))
	(irchat-configure-windows)
	(setq irchat-current-channels nil)
	(irchat-send-delayed-reset)
	(if irchat-current-channel
	    (irchat-Command-join irchat-current-channel)
	  (if irchat-startup-channel
	      (irchat-Command-join irchat-startup-channel)))
	(if (not (string-equal irchat-awaymsg ""))
	    (irchat-Command-away irchat-awaymsg))
	(run-hooks 'irchat-Startup-hook)

	(if (not irchat-timers-list-initialized-p)
	    (setq irchat-timers   
		  (append irchat-timers
			  (list
			   ;(list nil (function irchat-Command-timestamp)
			   ;      irchat-timestamp-interval)
			   (list nil (function irchat-Command-pollnames) 
				 irchat-pollnames-interval)
			   (list nil (function irchat-Command-keepalive)
				 irchat-keepalive-interval)
			   ;(list nil (function irchat-check-buffers)
			   ;     irchat-checkbuffer-interval)
			   ))
		    irchat-timers-list-initialized-p t))
	(setq
	 irchat-obarray (make-vector irchat-obarray-size nil)
	 irchat-timers
	      (mapcar 
	       (function 
		(lambda (timer)
		  (if (nth 0 timer)
		      (irchat-cancel-timer (nth 0 timer)))
		  (list (irchat-start-timer (nth 1 timer) (nth 2 timer))
			(nth 1 timer)
			(nth 2 timer))))
	       irchat-timers))

	(irchat-maybe-poll)
	(irchat-Command-timestamp)
	(irchat-Command-describe-briefly)))))


(defun irchat-Command-mode ()
  "Major mode for IRCHAT.  Normal edit function are available.
Typing Return or Linefeed enters the current line in the dialogue.
The following special commands are available:
For a list of the generic commands type \\[irchat-Command-generic] ? RET.
\\{irchat-Command-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (irchat-set-crypt-indicator)
  (setq irchat-nick-alist (list (list irchat-nickname))
	mode-line-modified "--- "
	major-mode 'irchat-Command-mode
	mode-name "IRCHAT Commands"
	irchat-privmsg-partner nil
	irchat-private-indicator nil
	irchat-away-indicator "-"
	irchat-freeze-indicator "-"
	irchat-ownfreeze-indicator "-"

	mode-line-format
	'("--- IRCHAT: Commands "
	  irchat-private-indicator
	  "{" irchat-channel-indicator "} "
	  irchat-away-indicator 
	  irchat-crypt-indicator
	  irchat-freeze-indicator
	  irchat-ownfreeze-indicator 
	  "- " 
	  irchat-real-nickname "@" irchat-server 
	  " %-"))

  (if (irchat-frozen (car irchat-D-buffer))
      (irchat-freeze-toggle (car irchat-D-buffer)))

  (use-local-map irchat-Command-mode-map)
  (if irchat-blink-parens
      nil
    (make-variable-buffer-local 'blink-matching-paren)
    (set-default 'blink-matching-paren t)
    (setq blink-matching-paren nil))
  (setq buffer-read-only nil)
  (run-hooks 'irchat-Command-mode-hook))
  

(defun irchat-Dialogue-mode ()
  "Major mode for displaying the IRC dialogue.
All normal editing commands are turned off.
Instead, these commands are available:
\\{irchat-Dialogue-mode-map}"
  (kill-all-local-variables)
  (setq mode-line-modified "--- "
	major-mode 'irchat-Dialogue-mode
	mode-name "IRCHAT Dialogue"

	mode-line-format
	'("--- IRCHAT: Dialogue " 
	  "{" irchat-channel-indicator "} "
	  irchat-away-indicator 
	  irchat-crypt-indicator
	  irchat-freeze-indicator
	  irchat-ownfreeze-indicator 
	  "-" (-3 . "%p") "-%-"))

  (use-local-map irchat-Dialogue-mode-map)
  (irchat-buffer-disable-undo (current-buffer))
  (if (not irchat-keep-buffers) 
      (erase-buffer))
  (set-buffer irchat-Dialogue-buffer)
  (setq buffer-read-only t)
  (run-hooks 'irchat-Dialogue-mode-hook))


(defun irchat-configure-windows ()
  "Configure Command mode and Dialogue mode windows.
One is for entering commands and text, the other displays the IRC dialogue."
  (if (or (one-window-p t)
	  (null (irchat-get-buffer-window irchat-Command-buffer))
	  (null (irchat-get-buffer-window irchat-Dialogue-buffer)))
      (progn
	(if irchat-command-window-on-top
	    (progn
	      (switch-to-buffer irchat-Command-buffer)
	      (if irchat-use-full-window
		  (delete-other-windows))
	      (if irchat-one-buffer-mode
		  (switch-to-buffer irchat-Dialogue-buffer)
		(split-window-vertically (max window-min-height 
					      irchat-command-window-height))
		(other-window 1)
		(switch-to-buffer irchat-Dialogue-buffer)
		(other-window 1)))
	  ;; mta@tut.fi wants it like this
	  (switch-to-buffer irchat-Dialogue-buffer)
	  (if irchat-use-full-window
	      (delete-other-windows))
	  (if irchat-one-buffer-mode
	      nil
	    (split-window-vertically
	     (- (window-height) (max window-min-height 
				     irchat-command-window-height)))
	    (other-window 1)
	    (switch-to-buffer irchat-Command-buffer))))))

(fset 'irchat-Dialogue-freeze 'irchat-Command-freeze)
(fset 'irchat-Dialogue-ownfreeze 'irchat-Command-ownfreeze)


(defun irchat-Dialogue-tag-line ()
  "Move current line to kill-ring."
  (interactive)
  (save-excursion
    (let (start)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (kill-ring-save start (point)))))


(defun irchat-set-crypt-indicator ()
  "Set crypt mode indicator"
  (if (irchat-crypt-support-p)
      (setq irchat-crypt-indicator
	    (cond ((and irchat-crypt-mode-active
			(not (null irchat-current-channel))
			(irchat-crypt-address-has-default-key-p 
			 irchat-current-channel))
		   "C")
		  (irchat-crypt-mode-active "c")
		  (t "-")))))


(defun irchat-Dialogue-setup-buffer ()
  "Initialize Dialogue mode buffer."
  (or (get-buffer irchat-Dialogue-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-Dialogue-buffer))
	(irchat-Dialogue-mode))))

(defun irchat-Private-setup-buffer ()
  "Initialize Dialogue mode buffer for private messages."
  (or (get-buffer irchat-Private-buffer)
      (save-excursion
	(set-buffer (get-buffer-create irchat-Private-buffer))
	(irchat-Dialogue-mode))))

(defun irchat-KILLS-setup-buffer ()
  "Initialize KILLS buffer."
  (or (get-buffer irchat-KILLS-buffer)
      (save-excursion
	(set-buffer (irchat-get-buffer-create irchat-KILLS-buffer)))))


(defun irchat-IGNORED-setup-buffer ()
  "Initialize IGNORED buffer."
  (or (get-buffer irchat-IGNORED-buffer)
      (save-excursion
	(set-buffer (irchat-get-buffer-create irchat-IGNORED-buffer)))))


(defun irchat-WALLOPS-setup-buffer ()
  "Initialize WALLOPS buffer."
  (or (get-buffer irchat-WALLOPS-buffer)
      (save-excursion
	(set-buffer (irchat-get-buffer-create irchat-WALLOPS-buffer)))))


(defun irchat-CRYPT-setup-buffer ()
  "Initialize CRYPT buffer."
  (or (get-buffer irchat-CRYPT-buffer)
      (save-excursion
	(set-buffer (irchat-get-buffer-create irchat-CRYPT-buffer))
	(irchat-Dialogue-mode))))


(defun irchat-clear-system ()
  "Clear all IRCHAT variables and buffers."
  (interactive)
  (if (and irchat-Command-buffer (get-buffer irchat-Command-buffer))
      (bury-buffer irchat-Command-buffer))
  (if (and irchat-Dialogue-buffer (get-buffer irchat-Dialogue-buffer))
      (bury-buffer irchat-Dialogue-buffer))
  (if (and irchat-KILLS-buffer (get-buffer irchat-KILLS-buffer))
      (bury-buffer irchat-KILLS-buffer))
  (if (and irchat-IGNORED-buffer (get-buffer irchat-IGNORED-buffer))
      (bury-buffer irchat-IGNORED-buffer))
  (if (and irchat-WALLOPS-buffer (get-buffer irchat-WALLOPS-buffer))
      (bury-buffer irchat-WALLOPS-buffer))
  (if (and irchat-debug-buffer (get-buffer irchat-debug-buffer))
      (bury-buffer irchat-debug-buffer))
  (setq irchat-debug-buffer nil
	irchat-channel-indicator "No channel"))


(defun irchat-wait-for-response (regexp)
  "Wait for server response which matches REGEXP."
  (save-excursion
    (let ((status t)
	  (wait t))
      (set-buffer irchat-server-buffer)
      (irchat-accept-response)
      (while wait
	(goto-char (point-min))
	(cond ((looking-at "ERROR")
	       (setq status nil)
	       (setq wait nil))
	      ((looking-at ".")
	       (setq wait nil))
	      (t (irchat-accept-response))))
      ;; Save status message.
      (end-of-line)
      (setq irchat-status-message-string
	    (buffer-substring (point-min) (point)))
      (if status
	  (progn
	    (setq wait t)
	    (while wait
	      (goto-char (point-max))
	      (forward-line -1)		;(beginning-of-line)
	      (if (looking-at regexp)
		  (setq wait nil)
		(message "IRCHAT: Reading...")
		(irchat-accept-response)
		(message "")))
	    ;; Successfully received server response.
	    t)))))

(defun irchat-accept-response ()
  "Read response of server. Only used at startup time"
  ;; To deal with server process exiting before accept-process-output is called.
  (or (memq (process-status irchat-server-process) '(open run))
      (if (not irchat-reconnect-automagic)
	  (error "IRCHAT: Connection closed.")
	(if irchat-grow-tail
	    (irchat 'always)
	  (irchat))))
  (condition-case errorcode
      (accept-process-output irchat-server-process)
    (error
     (cond ((string-equal "select error: Invalid argument" (nth 1 errorcode))
	    ;; Ignore select error.
	    nil)
	   (t (signal (car errorcode) (cdr errorcode)))))))

(defun irchat-maybe-poll ()
  (irchat-Command-ping))

(defun irchat-w-replace (buffer 
			 match
			 defstring
			 oldstring 
			 newstring 
			 &optional treshold)
  (if (not (integerp treshold)) (setq treshold irchat-compress-treshold))
  (if (or (not buffer) (listp buffer))
      (while buffer
	(progn
	  (irchat-w-replace (car buffer) match defstring oldstring newstring)
	  (setq buffer (cdr buffer))))
    (let* ((obuf (current-buffer))
	   (bufintern (intern (format "%s" (irchat-get-buffer-create buffer)) 
			      irchat-obarray))
	   (frozen (irchat-frozen buffer))
	   (spoint nil))
      (set-buffer (get-buffer buffer))
      (goto-char (point-max))
      (let ((buffer-read-only nil))
	(previous-line treshold)
	(if (re-search-forward match nil t)
	    (progn
	      (while (re-search-forward match nil t))
	      (beginning-of-line)
	      (if (re-search-forward oldstring nil t)
		  (replace-match newstring nil t)
		(irchat-w-insert buffer defstring)) ;This should't happen (kny)
	      (irchat-w-insert buffer ""))
	  (irchat-w-insert buffer defstring))))))


(defun irchat-w-insert (buffer string)
  (if (or (not buffer) (listp buffer))
      (let ((string (if (null irchat-utf8-kludge-disable)
			(irchat-utf8-kludge-decode-string string)
		      string))
	    (irchat-utf8-kludge-disable t))
	(while buffer
	  (progn
	    (irchat-w-insert (car buffer) string)
	    (setq buffer (cdr buffer)))))
    (let* ((obuf (current-buffer))
	   (bufintern) 
	   (nbuf (get-buffer buffer))
	   (frozen)
	   (spoint nil)
	   (oldwstart nil)
	   (oldwpoint nil)
	   (string (if (null irchat-utf8-kludge-disable)
		       (irchat-utf8-kludge-decode-string string)
		     string)))
      ;;
      ;; Deleted or nonexistent buffers are (re)created.
      ;;
      (if (not nbuf)
	  (save-excursion
	    (setq nbuf (irchat-get-buffer-create buffer))
	    (set-buffer nbuf)
	    (irchat-Dialogue-mode)))
      ;;
      ;; Check buffers and possibly insert timestamp.
      ;;
      (irchat-check-buffers-if-interval-expired)
      (if (irchat-Dialogue-buffer-p buffer)
	  (irchat-Command-timestamp-if-interval-expired))

      (if (irchat-get-buffer-window obuf)
	  (progn
	    (setq oldwstart (window-start (irchat-get-buffer-window obuf))
		  oldwpoint (window-point (irchat-get-buffer-window obuf)))))
      (setq bufintern (intern (format "%s" nbuf) irchat-obarray))
      (setq frozen (irchat-frozen nbuf))
      (set-buffer nbuf)
      ;;
      ;; have to use this kludge because save-restriction
      ;; doesn't work right, it keeps distance between point
      ;; and point-max a constant, not the distance between
      ;; point and mark, seesh mta Fri Jan  3 12:43:01 EET 1992
      ;;
      (setq spoint (point))
      (goto-char (point-max))
      (let ((buffer-read-only nil))
	(if (not (irchat-is-message-ignored string nbuf))
	    (progn
	      (if (and (or (eq irchat-beep-on-bells 'always)
			   (and irchat-beep-on-bells
				(null
				 (irchat-get-buffer-window (current-buffer)))))
		       (string-match "\007" string)
		       (irchat-Dialogue-buffer-p (current-buffer)))
		  (beep t))
	      (insert string)
	      (if (and irchat-use-smiley (fboundp 'smiley-region))
		  (smiley-region spoint (point-max)))))
	(goto-char spoint)
	(let ((win-list (irchat-get-buffer-window-list (get-buffer buffer))))
	  (mapcar
	   '(lambda (win)
	      (if (and win (not frozen)
		       (not (pos-visible-in-window-p (point-max) win)))
		  (progn
		    (goto-char (point-max))
		    (if (and (string-match "Xemacs" emacs-version)
			     (not (string-match "^1[0-9]\\." emacs-version)))
			(progn
			  (set-window-point win (point-max))
			  (vertical-motion (/ (- (or irchat-scroll-step
						     (1+ (/ 
							  (irchat-window-height
							   win)
							  2)))
						 (irchat-window-height win)) 2)
					   win)
			  (set-window-start win (point) t)
			  (goto-char (point-max))
			  )
		      (progn
			(vertical-motion (- (or irchat-scroll-step
						(1+ (/ 
						     (irchat-window-height 
						      win)
						     2)))
					    (irchat-window-height win))
					 win)
			(set-window-start win (point))
			(goto-char (point-max))
			)
		      )))
	      )
	   win-list))
	(set-buffer obuf)
	(if (and frozen (irchat-get-buffer-window obuf) (equal obuf nbuf))
	    (progn
	      (set-window-start (irchat-get-buffer-window obuf) oldwstart)
	      (set-window-point (irchat-get-buffer-window obuf) oldwpoint)
	      (goto-char oldwpoint)
	      ))))))


(defun irchat-get-buffer-create (name)
  "Get or create buffer, keep track on its name so we can kill it."
  (if (not (member name irchat-buffer-list))
      (setq irchat-buffer-list (append (list name) irchat-buffer-list)))
  (get-buffer-create name))


(defun irchat-freeze-toggle (buffer)
  (interactive "b")
  (let* ((intern-buffer (intern (format "%s" (get-buffer buffer)) 
				irchat-obarray))
	 (buffer-frozen (get intern-buffer 'frozen)))
    (if buffer-frozen
	(put intern-buffer 'frozen nil)
      (put intern-buffer 'frozen t))))


(defun irchat-frozen (buffer)
  (interactive "b")
  (let* ((intern-buffer (intern (format "%s" (get-buffer buffer)) 
				irchat-obarray))
	 (buffer-frozen (get intern-buffer 'frozen)))
    buffer-frozen))


(defun irchat-ownfreeze-toggle (buffer)
  (interactive "b")
  (let* ((intern-buffer (intern (format "%s" (get-buffer buffer)) 
				irchat-obarray))
	 (buffer-ownfrozen (get intern-buffer 'ownfrozen)))
    (if buffer-ownfrozen
	(put intern-buffer 'ownfrozen nil)
      (put intern-buffer 'ownfrozen t))))


(defun irchat-ownfrozen (buffer)
  (interactive "b")
  (let* ((intern-buffer (intern (format "%s" (get-buffer buffer)) 
				irchat-obarray))
	 (buffer-ownfrozen (get intern-buffer 'ownfrozen)))
    buffer-ownfrozen))


(defvar irchat-last-checkbuffer-time nil "Last time buffers were checked.")


(defun irchat-check-buffers-if-interval-expired ()
  (if (and (numberp irchat-checkbuffer-interval)
           (> irchat-checkbuffer-interval 0)
           (or (null irchat-last-checkbuffer-time)
               (> (irchat-time-difference irchat-last-checkbuffer-time
                                          (current-time))
                  irchat-checkbuffer-interval)))
      (progn
        (irchat-check-buffers)
        t)
    nil))


(defun irchat-check-buffers ()
  (let ((irchat-checkbuffer-interval 0))
    (if (> irchat-buffer-maxsize 0)
	(let ((buflist irchat-buffer-list)
	      (obuf (current-buffer)))
	  (while buflist
	    (set-buffer (irchat-get-buffer-create (car buflist)))
	    (if (< irchat-buffer-maxsize (buffer-size))
		(let ((buffer-read-only nil))
		  (goto-char (- (buffer-size) irchat-buffer-defsize))
		  (forward-line -1)
		  (delete-region (point-min) (point))))
	    (setq buflist (cdr buflist)))
	  (set-buffer obuf))))
  (setq irchat-last-checkbuffer-time (current-time)))


(defun irchat-is-message-ignored (string buffer)
  (let ((mylist irchat-ignore-list)
	(found nil)
	(case-fold-search t))
    (catch 'ignore
      (if (member buffer irchat-no-ignore-buffers)
	  (throw 'ignore t))
      (while mylist
	(let ((msg nil) 
	      (str nil)
	      (who nil)
	      (mymylist (car mylist)))
	  (if (listp (car mymylist))
	      (setq msg (car (car mymylist))
		    str (car (cdr (car mymylist))))
	    (if (fboundp (car mymylist))
		(let ((msgstr (apply (car mymylist) (list string))))
		  (setq msg (car msgstr)
			str (car (cdr msgstr))))
	      (message "Malformed ignore-list, no msg+str function.")))
	  (if (listp (car (cdr mymylist)))
	      (setq who (car (cdr mymylist)))
	    (if (fboundp (car (cdr mymylist)))
		(setq who (apply (car (cdr mymylist)) (list string)))
	      (if (not (car (cdr mylist)))
		  (message "Malformed ignore-list, no user function."))))
	  (if (or msg str)
	      (if (or
		   (and msg (string-match msg (nth 4 irchat-current-function)))
		   (and str (string-match str string)))
		  (let ((mywho who))
		    (while mywho
		      (if (string-match (car mywho) (nth 2 irchat-current-function))
			  (progn
			    (setq found t)
			    (throw 'ignore t))
			)
		      (setq mywho (cdr mywho))))
		)))
	(setq mylist (cdr mylist))))
    found
    )
  )

(eval-and-compile (provide 'irchat))
;;;
;;; eof
;;;
