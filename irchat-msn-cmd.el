;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-cmd.el,v 3.1 2002/06/04 15:47:27 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

(defun irchat-Command-msn-quit ()
  (interactive)
  (let ((running (irchat-msn-server-opened)))
    (irchat-msn-close-server)
    (if running
	(message "MSN Messenger connection closed.")
      (message "MSN Messenger connection is not open."))))

(defun irchat-Command-msn-send-proto ()
  (interactive)
  (if (irchat-msn-server-opened)
      (let ((msg (read-string "MSN Messenger Protocol Message: ")))
	(irchat-msn-send msg))
    (message "Not connected to MSN.")))

(defun irchat-Command-msn-add-contact (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (read-string "Add MSN-Contact: " nil)))
  (if (not (string-match "^[^@ ][^@ ]*@[^@ ][^@ ]*$" name))
      (error "Illegal MSN Name."))
  (if (null (irchat-search-contact-list-with-name name 
						  irchat-msn-forward-list))
      (progn
	(let ((data (irchat-search-contact-list-with-name name
							  irchat-msn-block-list)))
	  (if data
	      (irchat-msn-send "REM %d BL %s" (irchat-msn-seqno) (nth 0 data))))
	(if (null (irchat-search-contact-list-with-name name
							irchat-msn-allow-list))
	    (irchat-msn-send "ADD %d AL %s %s" (irchat-msn-seqno) name name))
	(irchat-msn-send "ADD %d FL %s %s 0" (irchat-msn-seqno) name name))
    (message "%s is already on MSN-Contacts list" name)))

(defun irchat-Command-msn-remove-contact (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (irchat-completing-default-read "Remove MSN-Contact: "
						 (irchat-msn-contact-uids-alist 
						  irchat-msn-forward-list) 
						 '(lambda (s) t) nil nil)))
  (let ((data (irchat-search-contact-list-with-name name
						    irchat-msn-forward-list)))
    (if data
	(irchat-msn-send "REM %d FL %s %d" 
			 (irchat-msn-seqno) 
			 (nth 0 data) 
			 (nth 2 data))
      (message "%s is not on MSN-Contacts list" name))))

(defun irchat-Command-msn-add-block (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (read-string "Add name to MSN block list: " nil)))
  (if (not (string-match "^[^@ ][^@ ]*@[^@ ][^@ ]*$" name))
      (error "Illegal MSN Name."))
  (if (null (irchat-search-contact-list-with-name name 
						  irchat-msn-block-list))
      (progn
	(let ((data (irchat-search-contact-list-with-name name
							  irchat-msn-allow-list)))
	  (if data
	      (irchat-msn-send "REM %d AL %s" (irchat-msn-seqno) (nth 0 data))))
	(irchat-msn-send "ADD %d BL %s %s" (irchat-msn-seqno) name name))
    (message "%s is already on the block list" name))
  t)

(defun irchat-Command-msn-remove-block (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (irchat-completing-default-read "Remove name from MSN block list: "
						 (irchat-msn-contact-uids-alist 
						  irchat-msn-block-list) 
						 '(lambda (s) t) nil nil)))
  (let ((data (irchat-search-contact-list-with-name name
						    irchat-msn-block-list)))
    (if data
	(irchat-msn-send "REM %d BL %s" 
			 (irchat-msn-seqno) 
			 (nth 0 data))
      (message "%s is not on MSN-Contacts list" name))))

(defun irchat-Command-msn-add-allow (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (read-string "Add name to MSN allow list: " nil)))
  (if (not (string-match "^[^@ ][^@ ]*@[^@ ][^@ ]*$" name))
      (error "Illegal MSN Name."))
  (if (null (irchat-search-contact-list-with-name name 
						  irchat-msn-allow-list))
      (progn
	(let ((data (irchat-search-contact-list-with-name name
							  irchat-msn-block-list)))
	  (if data
	      (irchat-msn-send "REM %d BL %s" (irchat-msn-seqno) (nth 0 data))))
	(irchat-msn-send "ADD %d AL %s %s" (irchat-msn-seqno) name name))
    (message "%s is already on the allow list" name))
  t)

(defun irchat-Command-msn-remove-allow (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (irchat-completing-default-read "Remove name from MSN allow list: "
						 (irchat-msn-contact-uids-alist 
						  irchat-msn-allow-list) 
						 '(lambda (s) t) nil nil)))
  (let ((data (irchat-search-contact-list-with-name name
						    irchat-msn-allow-list)))
    (if data
	(irchat-msn-send "REM %d AL %s" 
			 (irchat-msn-seqno) 
			 (nth 0 data))
      (message "%s is not on MSN-Contacts list" name))))

(defun irchat-Command-msn-kill-conversation (&optional name)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null name)
      (setq name (irchat-completing-default-read "Kill conversation: "
						 (irchat-msn-conversation-names-alist)
						 '(lambda (s) t) nil nil)))
  (irchat-msn-kill-conversation name))

(defun irchat-Command-msn-send (&optional recipient msg)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null recipient)
      (setq recipient (irchat-completing-default-read "Send message to: "
						      (append
						       (irchat-msn-conversation-names-alist)
						       (irchat-msn-online-users-alist))
						      '(lambda (s) t) nil irchat-msn-recipient-cache)))
  (let ((p (irchat-msn-sub-server-search-with-name recipient)))
    (if (null p)
	(setq p (irchat-msn-sub-server-search-with-user recipient)))
    (if p
	(progn
	  (if (null msg)
	      (setq msg (read-string "Message: ")))
	  (let ((m (irchat-msn-make-message msg)))
	    (setq irchat-msn-recipient-cache recipient)
	    (irchat-w-insert irchat-MSN-buffer
			     (concat 
			      (format irchat-msn-format-string-out
				      (if (> (length (nth 6 p)) 1)
					  (nth 1 p)
					(nth 0 (nth 6 p))))
			      " "
			      msg
			      "\n"))
	    (irchat-msn-send-sub-raw (nth 0 p)
				     "MSG %d A %d\r\n%s"
				     (irchat-msn-sub-server-seqno (nth 0 p))
				     (length m)
				     m)))
      (cond ((irchat-search-contact-list-with-name recipient irchat-msn-online-list)
	     (if (null msg)
		 (setq msg (read-string "Message: ")))
	     (setq irchat-msn-recipient-cache recipient)
	     (setq irchat-msn-messages-pending-sb (append irchat-msn-messages-pending-sb
							  (list (list recipient msg))))
	     (irchat-msn-send "XFR %s SB" (irchat-msn-seqno)))
	    ((irchat-search-contact-list-with-name recipient irchat-msn-forward-list)
	     (message "User %s is not online." recipient))
	    (t
	     (message "Recipient not found."))))))

(defun irchat-Command-msn-list-online ()
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if irchat-msn-online-list
      (let ((l irchat-msn-online-list))
	(irchat-w-insert irchat-MSN-buffer 
			 (format "%s%d contacts online:\n"
				 irchat-msn-info-prefix
				 (length l)))
	(while l
	  (irchat-w-insert irchat-MSN-buffer
			   (format "    %s <%s> is %s\n"
				   (nth 1 (car l))
				   (nth 0 (car l))
				   (irchat-msn-status-string (nth 3 (car l)))))
	  (setq l (cdr l))))
    (irchat-w-insert irchat-MSN-buffer 
		     (format "%sNo contacts online.\n"
			     irchat-msn-info-prefix))))

(defun irchat-msn-list-list (head list)
  (if list
      (let ((l list))
	(if head
	    (irchat-w-insert irchat-MSN-buffer
			     (format "%s%s\n"
				     irchat-msn-info-prefix
				     head)))
	(while l
	  (irchat-w-insert irchat-MSN-buffer
			   (format "    %s <%s>\n"
				   (nth 1 (car l))
				   (nth 0 (car l))))
	  (setq l (cdr l))))))

(defun irchat-Command-msn-list-lists ()
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (irchat-msn-list-list "Contacts online:" irchat-msn-online-list)
  (irchat-msn-list-list "Contacts list:" irchat-msn-forward-list)
  (irchat-msn-list-list "Reverse contacts list:" irchat-msn-reverse-list)
  (irchat-msn-list-list "Allow list:" irchat-msn-allow-list)
  (irchat-msn-list-list "Block list:" irchat-msn-block-list))

(defun irchat-Command-msn-set-status ()
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (let ((i irchat-msn-status-strings)
	(m '())
	(a '()))
    (while i
      (if (not (string-equal "FLN" (car (car i))))
	  (setq m (cons (car i) m)
		a (cons (list (cdr (car i))) a)))
      (setq i (cdr i)))
    (let ((s (irchat-completing-default-read "Set status to: "
					     a
					     '(lambda (s) t) t nil)))
      (if (> (length s) 1)
	  (let ((x nil))
	    (while m
	      (if (string-equal s (cdr (car m)))
		  (setq x (car (car m))
			m '()))
	      (setq m (cdr m)))
	    (if x
		(irchat-msn-send "CHG %d %s" (irchat-msn-seqno) x)))))))

(eval-and-compile (provide 'irchat-msn-cmd))

;;; eof (irchat-msn-cmd.el)
