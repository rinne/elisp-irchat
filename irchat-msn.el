;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn.el,v 3.5 2002/06/05 21:22:00 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

(eval-and-compile (require 'irchat-msn-vars))
(eval-and-compile (require 'irchat-msn-handle))
(eval-and-compile (require 'irchat-msn-proc))
(eval-and-compile (require 'irchat-msn-cmd))
(eval-and-compile (require 'irchat-msn-sub))
(eval-and-compile (require 'irchat-msn-msg))

;;;
;;; Internal variables
;;;
(defvar irchat-msn-server-int nil)
(defvar irchat-msn-seqno 0)
(defvar irchat-msn-uid-password-int nil)
(defvar irchat-msn-status-message-string nil)
(defvar irchat-msn-server-buffer nil)
(defvar irchat-msn-server-name nil)
(defvar irchat-msn-server-process nil)
(defvar irchat-msn-connection-phase nil)
(defvar irchat-msn-last-password nil)
(defvar irchat-msn-password-cache nil)
(defvar irchat-msn-recipient-cache nil)
(defvar irchat-msn-lists-in-sync nil)
(defvar irchat-msn-conversation-counter '(0))
(defvar irchat-msn-sub-servers '())
(defvar irchat-msn-messages-pending-sb '())
(defvar irchat-msn-name-cache '())
(defvar irchat-msn-indicator "")
(defvar irchat-msn-my-online-mode nil)
(defvar irchat-msn-online-list '() 
  "*Users in our forward list that are currently online")
(defvar irchat-msn-forward-list '() 
  "*FL Forward List - Users on your contact list.  (FL)")
(defvar irchat-msn-reverse-list '()
  "*RL Reverse List - Users who have you on their contact list.  (RL)")
(defvar irchat-msn-allow-list '()
  "*AL Allow List - Users who are allowed to see your status.  (AL)")
(defvar irchat-msn-block-list '()
  "*BL Block List - Users who are not allowed to see your status.  (BL)")
(defvar irchat-MSN-buffer (list irchat-Dialogue-buffer)
  "*A list of buffers where MSN messages to me are sent.")

;;;
;;; Constants
;;;
(defconst irchat-msn-challenge-cookie "Q1P7W2E4J9R8U3S5"
  "Don't change this!!!")
(defconst irchat-msn-status-strings '(("NLN" . "Online")
				      ("FLN" . "Offline")
				      ("HDN" . "Appear Offline")
				      ("IDL" . "Idle")
				      ("AWY" . "Away")
				      ("BSY" . "Busy")
				      ("BRB" . "Be Right Back")
				      ("PHN" . "On the Phone")
				      ("LUN" . "Out to Lunch"))
  "Translate table for status message codes.")

(defun irchat-msn ()
  (interactive)
  (require 'md5)
  (setq irchat-msn-seqno 0)
  (irchat-msn-start-server))

(defun irchat-msn-start-server (&optional host service)
  "Open network stream to remote MSN server."
  (if (irchat-msn-server-opened)
      t
    (setq irchat-msn-online-list '())
    (setq irchat-msn-forward-list '())
    (setq irchat-msn-reverse-list '())
    (setq irchat-msn-allow-list '())
    (setq irchat-msn-block-list '())
    (setq irchat-msn-lists-in-sync nil)
    (setq irchat-msn-conversation-counter '(0))
    (setq irchat-msn-sub-servers '())
    (setq irchat-msn-messages-pending-sb '())
    (setq irchat-msn-recipient-cache nil)
    (setq irchat-msn-my-online-mode "Offline")
    (irchat-set-msn-indicator)
    (irchat-msn-name-cache-flush)
    (setq irchat-msn-server-int nil)
    (if host
	(setq irchat-msn-server-int host))
    (if (null irchat-msn-server-int)
	(setq irchat-msn-server-int (irchat-msn-server)))
    (if (null irchat-msn-server-int)
	(setq irchat-msn-server-int
	      (read-string "MSN Messenger server: " irchat-msn-server-int)))
    (if (null irchat-msn-server)
	(setq irchat-msn-server irchat-msn-server-int))
    (if (null irchat-msn-uid)
	(setq irchat-msn-uid (read-string "MSN Messenger UID: " 
					  irchat-msn-uid)))
    (if (eq irchat-msn-connection-phase 'xfr-received)
	(message "Transfering MSN Messenger connection to server on %s..." irchat-msn-server-int)
      (message "Connecting to MSN Messenger server on %s..." irchat-msn-server-int))
    (setq irchat-msn-connection-phase nil)
    (cond ((irchat-msn-open-server irchat-msn-server-int (if service service irchat-msn-service))
	   (progn
	     (message "")
	     (setq irchat-msn-connection-phase 'ver-sent)))
	  ((and (stringp irchat-msn-status-message-string)
		(> (length irchat-msn-status-message-string) 0))
	   ;; Show valuable message if available.
	   (error irchat-msn-status-message-string))
	  (t (error "Cannot open MSN Messenger server on %s" 
		    irchat-msn-server)))))
  
(defun irchat-msn-server-opened ()
  "Return server process status, T or NIL.  T if running."
  (let ((running (and irchat-msn-server-process
		      (memq (process-status irchat-msn-server-process) 
			    '(open run)))))
    running))

(defun irchat-msn-open-server (host &optional service)
  (setq irchat-msn-status-message-string "")
  (if (and host (irchat-msn-open-server-internal host service))
      (progn
	(irchat-msn-send "VER %d MSNP7 MSNP6 MSNP5 MSNP4 CVRO"
			 (irchat-msn-seqno))
	(set-process-sentinel irchat-msn-server-process
			      'irchat-msn-sentinel)
	(set-process-filter irchat-msn-server-process
			    'irchat-msn-filter)
	t)
    (irchat-close-server-internal)
    nil))

(defun irchat-msn-open-server-internal (host &optional service)
  "Open connection to chat server on HOST by SERVICE (default is 1863)."
  (condition-case err 
      (save-excursion
	;; Initialize communication buffer.
	(setq irchat-msn-server-buffer (get-buffer-create " *MSN*"))
	(set-buffer irchat-msn-server-buffer)
	(kill-all-local-variables)
	(irchat-buffer-disable-undo (current-buffer))
	(erase-buffer)
	(setq irchat-msn-server-process
	      (open-network-stream "MSN" (current-buffer)
				   host (or service 1863)))
	(setq irchat-msn-server-name host)
	(run-hooks 'irchat-msn-server-hook)
	;; Return the server process.
	irchat-msn-server-process)
    (error (message (car (cdr err)))
	   nil)))

(defun irchat-msn-close-server ()
  "Close MSN chat server."
  (unwind-protect
      (progn
        ;; Un-set default sentinel function before closing connection.
	(irchat-msn-kill-all-conversations)
        (and irchat-msn-server-process
             (eq 'irchat-msn-sentinel
                 (process-sentinel irchat-msn-server-process))
             (set-process-sentinel irchat-msn-server-process nil))))
  (irchat-msn-close-server-internal))

(defun irchat-msn-close-server-internal ()
  "Close connection to chat server."
  (irchat-insert-to-debug "Closing server" "*MSN* ")
  (setq irchat-msn-connection-phase nil)
  (if irchat-msn-server-process
      (delete-process irchat-msn-server-process))
  (setq irchat-msn-server-process nil)
  (if irchat-server-buffer
      (kill-buffer irchat-msn-server-buffer))
  (setq irchat-msn-server-buffer nil)
  (irchat-set-msn-indicator))

(defun irchat-msn-send (&rest args)
  "Send the protocol string to the MSN server."
  (let ((item (irchat-encode-coding-string (concat (apply 'format args) 
						   "\r"))))
    (irchat-insert-to-debug item "-MSN-> ")
    (process-send-string irchat-msn-server-process (concat item "\n"))))

(defun irchat-msn-send-sub (proc &rest args)
  "Send the protocol string to the MSN server."
  (let ((item (irchat-encode-coding-string (concat (apply 'format args) 
						   "\r"))))
    (irchat-insert-to-debug item "-MSN-SUB-> ")
    (process-send-string proc (concat item "\n"))))

(defun irchat-msn-send-raw (&rest args)
  "Send the protocol string to the MSN server."
  (let ((item (irchat-encode-coding-string (apply 'format args))))
    (irchat-insert-to-debug item "-MSN-> ")
    (process-send-string irchat-msn-server-process item)))

(defun irchat-msn-send-sub-raw (proc &rest args)
  "Send the protocol string to the MSN server."
  (let ((item (irchat-encode-coding-string (apply 'format args))))
    (irchat-insert-to-debug item "-MSN-SUB-> ")
    (process-send-string proc item)))

(defun irchat-msn-seqno ()
  (let ((seqno irchat-msn-seqno))
    (setq irchat-msn-seqno (+ 1 seqno))
    seqno))

(defun irchat-msn-status-string (status)
  "Translate user status code to English."
  (let ((l irchat-msn-status-strings)
	(s (upcase status))
	(r nil))
    (while l
      (if (string-equal (car (car l)) s)
	  (setq r (cdr (car l))
		l nil)
	(setq l (cdr l))))
    (if r
	r
      (format "Unknown Status (%s)" s))))

(defun irchat-msn-decode-name (name) 
  (let ((r name)) 
    (setq r (irchat-replace-in-string r "%20" " ")) 
    (setq r (irchat-msn-iso8859-1-to-utf8 r t))
    r))
 
(defun irchat-msn-encode-name (name) 
  (let ((r name))   
    (setq r (irchat-replace-in-string r  " " "%20")) 
    r)) 

(defun irchat-search-contact-list-with-name (name lst)
  (let ((l lst)
	(r nil)
	(n (upcase name)))
    (while l
      (if (string-equal (upcase (car (car l))) n)
	  (setq r (car l)
		l nil)
	(setq l (cdr l))))
    r))

(defun irchat-remove-from-contact-list-with-name (name lst)
  (let ((l lst)
	(r nil)
	(n (upcase name)))
    (while l
      (if (not (string-equal (upcase (car (car l))) n))
	  (setq r (cons (car l) r)))
      (setq l (cdr l)))
    r))

(defun irchat-msn-contact-uids (lst)
  (let ((l lst)
	(r '()))
    (while l
      (setq r (cons (car (car l)) r)
	    l (cdr l)))
    r))

(defun irchat-msn-contact-uids-alist (lst)
  (let ((l lst)
	(r '()))
    (while l
      (setq r (cons (list (car (car l))) r)
	    l (cdr l)))
    r))

(defun irchat-msn-conversation-counter-increment ()
  (let ((x irchat-msn-conversation-counter)
	(y '()))
    (while (and x
		(eq (car x) 9))
      (setq y (cons 0 y)
	    x (cdr x)))
    (if x
	(progn
	  (setq y (cons (+ (car x) 1) y)
		x (cdr x))
	  (while x
	    (setq y (cons (car x) y)
		  x (cdr x))))
      (setq y (cons 1 y)))
    (while y
      (setq x (cons (car y) x)
	    y (cdr y)))
    (setq irchat-msn-conversation-counter x))
  irchat-msn-conversation-counter)

(defun irchat-msn-generate-conversation-name ()
  (irchat-msn-conversation-counter-increment)
  (let ((n "")
	(x irchat-msn-conversation-counter))
    (while x
      (setq n (concat (format "%d" (car x)) n)
	    x (cdr x)))
    (format "$MSN[%s]" n)))

(defun irchat-msn-protocol-error ()
  (message "MSN Connection closed because of a protocol error.")
  (irchat-msn-close-server))


(defun irchat-msn-online-users ()
  (let ((l irchat-msn-online-list)
	(r '()))
    (while l
      (if (string-equal "NLN" (nth 3 (car l)))
	  (setq r (cons (nth 0 (car l)) r)))
      (setq l (cdr l)))
    r))

(defun irchat-msn-online-users-alist ()
  (let ((l irchat-msn-online-list)
	(r '()))
    (while l
      (if (string-equal "NLN" (nth 3 (car l)))
	  (setq r (cons (list (nth 0 (car l))) r)))
      (setq l (cdr l)))
    r))

(defun irchat-msn-server ()
  (if (listp irchat-msn-server)
      (nth (random (length irchat-msn-server)) irchat-msn-server)
    irchat-msn-server))

(defun irchat-set-msn-indicator ()
  (cond ((and irchat-msn-uid
	      (not (irchat-msn-server-opened)))
	 (setq irchat-msn-indicator " {MSN: Offline} "))
	((not (irchat-msn-server-opened))
	 (setq irchat-msn-indicator ""))
	(t
	 (setq irchat-msn-indicator
	       (concat "{MSN: "
		       irchat-msn-uid
		       " "
		       irchat-msn-my-online-mode
		       (if (> (length irchat-msn-online-list) 0)
			   (format " (%d)" (length irchat-msn-online-list))
			 "")
		       "}"))))
  irchat-msn-indicator)

(defun irchat-msn-name-cache-delete (name)
  (let ((l irchat-msn-name-cache)
	(n (upcase name))
	(r '()))
    (while l
      (if (not (string-equal (car (car l)) n))
	  (setq r (cons (car l) r)))
      (setq l (cdr l)))
    (setq irchat-msn-name-cache r))
  t)

(defun irchat-msn-name-cache-add (name visible)
  (let ((n (upcase name)))
    (irchat-msn-name-cache-delete n)
    (setq irchat-msn-name-cache (cons (cons n visible)
				      irchat-msn-name-cache)))
  t)

(defun irchat-msn-name-cache-get (name)
  (let ((l irchat-msn-name-cache)
	(n (upcase name))
	(r name))
    (while l
      (if (string-equal (car (car l)) n)
	  (setq r (cdr (car l))
		l '())
	(setq l (cdr l))))
    r))

(defun irchat-msn-name-cache-flush ()
  (setq irchat-msn-name-cache '()))

(eval-and-compile (provide 'irchat-msn))

;;; eof (irchat-msn.el)
