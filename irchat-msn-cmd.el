;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-cmd.el,v 3.11 2002/06/23 15:23:43 tri Exp $
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
  (let ((p (irchat-msn-sub-server-search-with-name recipient))
	(cmsg ""))
    (if (null p)
	(setq p (irchat-msn-sub-server-search-with-user recipient)))
    (if p
	(progn
	  (if (null msg)
	      (progn
		(if irchat-msn-send-typing-notifications
		    (let ((tn (irchat-msn-make-typing-notification)))
		      (irchat-msn-send-sub-raw (nth 0 p)
					       "MSG %d U %d\r\n%s"
					       (irchat-msn-sub-server-seqno (nth 0 p))
					       (length tn)
					       tn)))
		(setq msg (read-string (format "Message to %s: " recipient)))))
	  (setq cmsg msg)
	  (if (fboundp 'irchat-encrypt-message)
	      (let ((irchat-real-nickname irchat-msn-uid))
		(setq msg (irchat-encrypt-message msg recipient))))
	  (let ((m (irchat-msn-make-message msg (not (string-equal msg cmsg)))))
	    (setq irchat-msn-recipient-cache recipient)
	    (irchat-w-insert irchat-MSN-MSG-buffer
			     (concat 
			      (format (if (string-equal msg cmsg)
					  irchat-msn-format-string-out
					irchat-msn-format-string-out-e)
				      (if (> (length (nth 6 p)) 1)
					  (nth 1 p)
					(nth 0 (nth 6 p))))
			      " "
			      cmsg
			      "\n"))
	    (irchat-msn-send-sub-raw (nth 0 p)
				     "MSG %d A %d\r\n%s"
				     (irchat-msn-sub-server-seqno (nth 0 p))
				     (length m)
				     m)))
      (cond ((irchat-search-contact-list-with-name recipient irchat-msn-online-list)
	     (if (null msg)
		 (setq msg (read-string (format "Message to %s: " recipient))))
	     (setq cmsg msg)
	     (if (fboundp 'irchat-encrypt-message)
		 (let ((irchat-real-nickname irchat-msn-uid))
		   (setq msg (irchat-encrypt-message msg recipient))))
	     (setq irchat-msn-recipient-cache recipient)
	     (setq irchat-msn-messages-pending-sb (append irchat-msn-messages-pending-sb
							  (list (list recipient msg cmsg))))
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
					     '(lambda (s) t)
					     t
					     (if (and (stringp irchat-msn-my-online-mode) 
						      (string-equal "Online" 
								    irchat-msn-my-online-mode)) 
						 "Away" 
					       "Online"))))
      (if (> (length s) 1)
	  (let ((x nil))
	    (while m
	      (if (string-equal s (cdr (car m)))
		  (setq x (car (car m))
			m '()))
	      (setq m (cdr m)))
	    (if x
		(irchat-msn-send "CHG %d %s" (irchat-msn-seqno) x)))))))

(defun irchat-Command-msn-list-discussion ()
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  t)

(defun irchat-Command-msn-invite (&optional discussion user)
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (if (null user)
      (setq user (irchat-completing-default-read "Invite user: "
						 (irchat-msn-online-users-alist)
						 '(lambda (s) t) nil nil)))
  (if (null discussion)
      (setq discussion (irchat-completing-default-read "Discussion: "
						       (irchat-msn-conversation-names-alist)
						       '(lambda (s) t) t nil)))
  (let ((p (irchat-msn-sub-server-search-with-name discussion)))
    (cond ((null p)
	   (message "No such discussion."))
	  ((null (irchat-search-contact-list-with-name user irchat-msn-forward-list))
	   (message "User not found."))
	  ((null (irchat-search-contact-list-with-name user irchat-msn-online-list))
	   (message "User not online."))
	  (t 
	   (irchat-msn-send-sub (nth 0 p)
				"CAL %d %s"
				(irchat-msn-sub-server-seqno (nth 0 p))
				user)))))

(defun irchat-Command-msn-list-discussions ()
  (interactive)
  (if (not (irchat-msn-server-opened))
      (error "MSN Messenger connection is not open."))
  (cond ((< (length irchat-msn-sub-servers) 1)
	 (irchat-w-insert irchat-MSN-buffer 
			  (format "%sNo active discussions.\n"
				  irchat-msn-info-prefix)))
	(t
	 (let ((l irchat-msn-sub-servers))
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%s%d active discussion%s.\n"
				    irchat-msn-info-prefix
				    (length l)
				    (if (> (length l) 1) "s" "")))
	   (while l
	     (let ((u (nth 6 (car l))))
	       (irchat-w-insert irchat-MSN-buffer 
				(format "%sDiscussion %s (%d guest%s).\n"
					irchat-msn-info-prefix
					(nth 1 (car l))
					(length u)
					(if (> (length u) 1) "s" "")))
	       (while u
		 (irchat-w-insert irchat-MSN-buffer 
				  (format "    %s <%s>\n"
					  (car u)
					  (irchat-msn-name-cache-get (car u))))
		 (setq u (cdr u)))
	       (setq l (cdr l))))))))

(defun irchat-Command-msn-ping-server () 
  (interactive) 
  (cond ((not (irchat-msn-server-opened)) 
         nil)
        ((stringp irchat-msn-fake-client-version) 
         (progn 
           (irchat-msn-send "CVR %d %s"  
                            (irchat-msn-seqno) 
                            irchat-msn-fake-client-version) 
           t))
        ((stringp irchat-msn-my-online-mode) 
         (progn 
           (irchat-msn-send "CHG %d %s"  
                            (irchat-msn-seqno) 
                            (irchat-msn-status-code irchat-msn-my-online-mode)))) 
        (t nil)))

(defun irchat-Command-show-online () 
  (interactive) 
  (if (not (irchat-msn-server-opened)) 
      (error "MSN Messenger connection is not open.")) 
  (let ((l (irchat-list-rnd irchat-msn-online-list)) 
        (m nil)
	(l2 nil))
    (setq l2 l)
    (while l 
      (let ((c (car l)) 
            (x nil)) 
        (setq l (cdr l)) 
	(if (string-equal "NLN" (nth 3 c))
	    (progn
	      (if (not (string-equal (nth 0 c) (nth 1 c))) 
		  (setq x (format "%s <%s>" (nth 1 c) (nth 0 c))) 
		(setq x (nth 0 c))) 
	      (if m 
		  (setq m (concat m ", " x)) 
		(setq m x))))))
    (setq l l2)
    (while l 
      (let ((c (car l)) 
            (x nil)) 
        (setq l (cdr l)) 
	(if (not (string-equal "NLN" (nth 3 c)))
	    (progn
	      (if (not (string-equal (nth 0 c) (nth 1 c))) 
		  (setq x (format "%s <%s> (%s)" (nth 1 c) (nth 0 c) (irchat-msn-status-string (nth 3 c)))) 
		(setq x (format "%s (%s)" (nth 0 c) (irchat-msn-status-string (nth 3 c)))))
	      (if m
		  (setq m (concat m ", " x)) 
		(setq m x))))))
    (message (if m m "None!"))))

(eval-and-compile (provide 'irchat-msn-cmd))

;;; eof (irchat-msn-cmd.el)
