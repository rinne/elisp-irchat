;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-handle.el,v 3.10 2002/09/02 20:28:19 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

(defun irchat-msn-handle-generic (parsed msg)
  (cond ((string-match "^[0-9][0-9]*$" (nth 0 parsed))
	 (progn
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%s%s.\n"
				    irchat-msn-error-prefix
				    (irchat-msn-protocol-error-string (nth 0 parsed))))
	   t))
	(t t)))

(defun irchat-msn-handle-MSG-message (cmd-len pp-uid pp-name len msg)
  t)

(defun irchat-msn-handle-VER (parsed msg)
  (if (eq irchat-msn-connection-phase 'ver-sent)
      (progn
	(irchat-msn-send "INF %d" (irchat-msn-seqno))
	(setq irchat-msn-connection-phase 'inf-sent))
    (irchat-msn-protocol-error)))

(defun irchat-msn-handle-INF (parsed msg)
  (if (eq irchat-msn-connection-phase 'inf-sent)
      (progn
	(irchat-w-insert irchat-MSN-buffer 
			 (format "%sTrying to sign into MSN Messenger Server %s.\n"
				 irchat-msn-info-prefix
				 irchat-msn-server-int))
	(irchat-msn-send "USR %d MD5 I %s" (irchat-msn-seqno) 
			 irchat-msn-uid)
	(setq irchat-msn-connection-phase 'usr-sent))
    (irchat-msn-protocol-error)))

(defun irchat-msn-handle-OUT (parsed msg)
  (if (> (length parsed) 1)
    (cond ((string-equal "OTH" (nth 1 parsed))
	   (progn
	     (setq irchat-msn-server-closed-because-of-another-login t)
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%sMSN Messenger connection closed because of another login.\n"
				      irchat-msn-info-prefix))))
	  ((string-equal "SSD" (nth 1 parsed))
	   (progn
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%sMSN Messenger connection closed server is going down.\n"
				      irchat-msn-info-prefix))))
	  (t
	   (progn
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%sMSN Messenger connection closed (reason=%s).\n"
				      irchat-msn-info-prefix
				      (nth 1 parsed)))))))
  (irchat-msn-close-server))

(defun irchat-msn-handle-NLN (parsed msg)
  (if (> (length parsed) 3)
      (let ((stat (nth 1 parsed))
	    (pp-uid (nth 2 parsed))
	    (pp-name (irchat-msn-decode-name (nth 3 parsed))))
	(let ((user (irchat-search-contact-list-with-name pp-uid
							  irchat-msn-online-list)))
	  (if (null user)
	      (setq user (irchat-search-contact-list-with-name pp-uid
							       irchat-msn-forward-list)))
	  (if (null user)
	      (setq user (list pp-uid pp-name -1 nil)))
	  (setq user (irchat-set-nth 3 user stat))
	  (setq user (irchat-set-nth 1 user pp-name))
	  (setq irchat-msn-online-list (cons user
					     (irchat-remove-from-contact-list-with-name pp-uid
											irchat-msn-online-list)))
	  (irchat-msn-name-cache-add pp-uid pp-name)
	  (if irchat-msn-show-status-changes
	      (irchat-w-insert irchat-MSN-buffer 
			       (format "%sUser %s <%s> status is %s.\n"
				       irchat-msn-info-prefix
				       pp-name
				       pp-uid
				       (irchat-msn-status-string stat)))))))
  (irchat-set-msn-indicator))

(defun irchat-msn-handle-ILN (parsed msg)
  (if (> (length parsed) 4)
      (let ((stat (nth 2 parsed))
	    (pp-uid (nth 3 parsed))
	    (pp-name (irchat-msn-decode-name (nth 4 parsed))))
	(let ((user (irchat-search-contact-list-with-name pp-uid
							  irchat-msn-online-list)))
	  (if (null user)
	      (setq user (irchat-search-contact-list-with-name pp-uid
							       irchat-msn-forward-list)))
	  (if (null user)
	      (setq user (list pp-uid pp-name -1 nil)))
	  (setq user (irchat-set-nth 3 user stat))
	  (setq user (irchat-set-nth 1 user pp-name))
	  (setq irchat-msn-online-list (cons user
					     (irchat-remove-from-contact-list-with-name pp-uid
											irchat-msn-online-list)))
	  (irchat-msn-name-cache-add pp-uid pp-name)
	  (if irchat-msn-show-status-changes
	      (irchat-w-insert irchat-MSN-buffer 
			       (format "%sUser %s <%s> status is %s.\n"
				       irchat-msn-info-prefix
				       pp-name
				       pp-uid
				       (irchat-msn-status-string stat)))))))
  (irchat-set-msn-indicator))

(defun irchat-msn-handle-FLN (parsed msg)
  (if (> (length parsed) 1)
      (let ((pp-uid (nth 1 parsed)))
	(setq irchat-msn-online-list (irchat-remove-from-contact-list-with-name pp-uid
										irchat-msn-online-list))
	(if irchat-msn-show-status-changes
	    (irchat-w-insert irchat-MSN-buffer 
			     (format "%sUser %s <%s> goes Offline.\n"
				     irchat-msn-info-prefix
				     (irchat-msn-name-cache-get pp-uid)
				     pp-uid)))))
  (irchat-set-msn-indicator))

(defun irchat-msn-handle-USR (parsed msg)
  (cond ((and (eq irchat-msn-connection-phase 'usr-sent)
	      (> (length parsed) 4)
	      (string-equal "MD5" (nth 2 parsed))
	      (string-equal "S" (nth 3 parsed)))
	 (let ((salt (nth 4 parsed))
	       (pass (cond ((not (null irchat-msn-password-cache))
			    (let ((r irchat-msn-password-cache))
			      (setq irchat-msn-password-cache nil)
			      r))
			   ((and irchat-msn-uid-password
				 (or (null irchat-msn-last-password)
				     (not (string-equal irchat-msn-uid-password
							irchat-msn-last-password))))
			    irchat-msn-uid-password)
			   (t (irchat-read-passphrase (format "Password for %s: " irchat-msn-uid))))))
	   (setq irchat-msn-connection-phase 'usr-pass-sent)
	   (setq irchat-msn-last-password pass)
	   (irchat-msn-send "USR %d MD5 S %s" (irchat-msn-seqno) (md5 (concat salt pass)))))
	((and (or (eq irchat-msn-connection-phase 'usr-sent)
		  (eq irchat-msn-connection-phase 'usr-pass-sent))
	      (> (length parsed) 3)
	      (string-equal "OK" (nth 2 parsed)))
	 (setq irchat-msn-password-cache irchat-msn-last-password)
	 (irchat-msn-send "CHG %d NLN" (irchat-msn-seqno))
	 (setq irchat-msn-online-list '()) ;; Server sends online list after CHG
	 (setq irchat-msn-connection-phase 'chg-sent)
	 (if irchat-msn-fake-client-version
	     (irchat-msn-send "CVR %d %s" (irchat-msn-seqno) irchat-msn-fake-client-version)))
       (t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-XFR (parsed msg)
  (cond ((and (eq irchat-msn-connection-phase 'usr-sent)
	      (> (length parsed) 4)
	      (string-equal "NS" (nth 2 parsed))
	      (string-match "^\\([0-9][0-9.]*[0-9]\\):\\([1-9][0-9]*\\)$" (nth 3 parsed)))
	 (let ((host (matching-substring (nth 3 parsed) 1))
	       (service (string-to-int (matching-substring (nth 3 parsed) 2))))
	   (irchat-w-insert irchat-MSN-buffer
			    (format "%sServer %s redirects the connection to %s.\n"
				    irchat-msn-info-prefix
				    irchat-msn-server-int
				    host))
	   (irchat-msn-close-server)
	   (setq irchat-msn-connection-phase 'xfr-received)
	   (irchat-msn-start-server host service)))
	((and (> (length parsed) 5)
	      (string-equal "SB" (nth 2 parsed))
	      (string-equal "CKI" (nth 4 parsed))
	      (string-match "^\\([0-9][0-9.]*[0-9]\\):\\([1-9][0-9]*\\)$" (nth 3 parsed)))
	 (let ((host (matching-substring (nth 3 parsed) 1))
	       (service (string-to-int (matching-substring (nth 3 parsed) 2)))
	       (hash (nth 5 parsed)))
	   (if irchat-msn-messages-pending-sb
	       (let ((pending (car irchat-msn-messages-pending-sb)))
		 (setq irchat-msn-messages-pending-sb (cdr irchat-msn-messages-pending-sb))
		 (irchat-msn-start-calling-sub-server host service hash pending))
	     t)))
	(t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-911 (parsed msg)
  (cond ((eq irchat-msn-connection-phase 'usr-pass-sent)
	 (progn
	   (setq irchat-msn-connection-phase nil)
	   (irchat-msn-close-server)
	   (irchat-msn-start-server)))
	(t 
	 (progn 
	   (message "MSN: 911, Giving up!")
	   (irchat-msn-close-server)))))

(defun irchat-msn-handle-QRY (parsed msg)
  t)

(defun irchat-msn-handle-CVR (parsed msg)
  t)

(defun irchat-msn-handle-CHL (parsed msg)
  (if (> (length parsed) 2)
      (let ((c1 (nth 2 parsed))
	    (c2 irchat-msn-challenge-cookie))
	(irchat-msn-send-raw "QRY %d %s 32\r\n%s"
			     (irchat-msn-seqno)
			     "msmsgs@msnmsgr.com"
			     (md5 (concat c1 c2))))
    (irchat-msn-protocol-error)))

(defun irchat-msn-handle-CHG (parsed msg)
  (cond ((eq irchat-msn-connection-phase 'chg-sent)
	 (progn
	   (setq irchat-msn-connection-phase 'online)
	   (setq irchat-msn-my-online-mode "Online")
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%sConnected MSN Messenger Server %s on %s.\n"
				    irchat-msn-info-prefix
				    irchat-msn-server-int
				    (if irchat-format-time-function 
					(apply irchat-format-time-function 
					       (list (current-time-string))) 
				      (current-time-string))))
	   (irchat-msn-send "SYN %d 0" (irchat-msn-seqno))))
	((> (length parsed) 2)
	 (let ((stat (nth 2 parsed)))
	   (let ((statstr (irchat-msn-status-string stat)))
	     (setq irchat-msn-my-online-mode statstr)
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%sYour status changed to %s.\n"
				      irchat-msn-info-prefix
				      statstr)))))
	(t t))
  (irchat-set-msn-indicator))

(defun irchat-msn-handle-SYN (parsed msg)
  (setq irchat-msn-lists-in-sync nil))

(defun irchat-msn-handle-LST (parsed msg)
  (cond ((> (length parsed) 7)
	 (let ((lstid (nth 2 parsed))
	       (lstver (string-to-int (nth 3 parsed)))
	       (num (string-to-int (nth 4 parsed)))
	       (tot (string-to-int (nth 5 parsed)))
	       (pp-uid (nth 6 parsed))
	       (pp-name (irchat-msn-decode-name (nth 7 parsed)))
	       (gid (if (nth 8 parsed) (string-to-int (nth 8 parsed)) -1)))
	   (cond ((string-equal "FL" lstid)
		  (progn
		    (if (eq num 1)
			(setq irchat-msn-forward-list '()))
		    (setq irchat-msn-forward-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-forward-list)))
		    t))
		 ((string-equal "RL" lstid)
		  (progn
		    (if (eq num 1)
			(setq irchat-msn-reverse-list '()))
		    (setq irchat-msn-reverse-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-reverse-list)))
		    t))
		 ((string-equal "AL" lstid)
		  (progn
		    (if (eq num 1)
			(setq irchat-msn-allow-list '()))
		    (setq irchat-msn-allow-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-allow-list)))
		    t))
		 ((string-equal "BL" lstid)
		  (progn
		    (if (eq num 1)
			(setq irchat-msn-block-list '()))
		    (setq irchat-msn-block-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-block-list)))
		    t))
		 (t t))))
	((and (> (length parsed) 5)
	      (string-equal "0" (nth 4 parsed))
	      (string-equal "0" (nth 5 parsed)))
	 (cond ((string-equal "FL" (nth 2 parsed))
		(setq irchat-msn-forward-list '()))
	       ((string-equal "RL" (nth 2 parsed))
		(setq irchat-msn-reverse-list '()))
	       ((string-equal "AL" (nth 2 parsed))
		(setq irchat-msn-allow-list '()))
	       ((string-equal "BL" (nth 2 parsed))
		(setq irchat-msn-block-list '()))
	       (t t)))
	(t t))
  (irchat-set-msn-indicator))

(defun irchat-msn-handle-ADD (parsed msg)
  (cond ((and (> (length parsed) 5)
	      (string-equal "RL" (nth 2 parsed)))
	 (let ((lstver (string-to-int (nth 3 parsed)))
	       (pp-uid (nth 4 parsed))
	       (pp-name (irchat-msn-decode-name (nth 5 parsed)))
	       (gid -1))
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%sUser %s <%s> adds you to to the contact list.\n"
				    irchat-msn-info-prefix pp-name pp-uid))
	   (setq irchat-msn-reverse-list (cons (list pp-uid pp-name gid nil)
					       irchat-msn-reverse-list))
	   (if (and irchat-msn-lists-in-sync
		    (null (irchat-search-contact-list-with-name pp-uid
								irchat-msn-allow-list))
		    (null (irchat-search-contact-list-with-name pp-uid
								irchat-msn-block-list)))
	       ;;; If it's not blocked, add it to allow list.
	       (irchat-msn-send "ADD %d AL %s %s" (irchat-msn-seqno) pp-uid pp-uid))
	   (if (and irchat-msn-lists-in-sync
		    irchat-msn-auto-add-reverse-contacts
		    (null (irchat-search-contact-list-with-name pp-uid
								irchat-msn-forward-list))
		    (null (irchat-search-contact-list-with-name pp-uid
								irchat-msn-block-list)))
	       ;;; If auto-contact is on and if it's not blocked, add it to forward list.
	       (irchat-msn-send "ADD %d FL %s %s 0" (irchat-msn-seqno) pp-uid pp-uid))
	   t))
	((> (length parsed) 5)
	 (let ((lstid (nth 2 parsed))
	       (lstver (nth 3 parsed))
	       (pp-uid (nth 4 parsed))
	       (pp-name (nth 5 parsed))
	       (gid (if (nth 6 parsed) (string-to-int (nth 6 parsed)) -1)))
	   (cond ((string-equal "FL" lstid)
		  (progn
		    (irchat-w-insert irchat-MSN-buffer
				     (format "%sUser %s <%s> added to the contact list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-forward-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-forward-list)))
		    t))
		 ((string-equal "AL" lstid)
		  (progn
		    (irchat-w-insert irchat-MSN-buffer
				     (format "%sUser %s <%s> added to the allow list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-allow-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-allow-list)))
		    t))
		 ((string-equal "BL" lstid)
		  (progn
		    (irchat-w-insert irchat-MSN-buffer
				     (format "%sUser %s <%s> added to the block list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-block-list 
			  (cons (list pp-uid pp-name gid nil)
				(irchat-remove-from-contact-list-with-name pp-uid
									   irchat-msn-block-list)))
		    t))
		 (t t))))
	(t t)))

(defun irchat-msn-handle-REM (parsed msg)
  (cond ((> (length parsed) 4)
	 (let ((lstid (nth 2 parsed))
	       (lstver (nth 3 parsed))
	       (pp-uid (nth 4 parsed)))
	   (cond ((string-equal "FL" lstid)
		  (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								       irchat-msn-forward-list))
			(pp-name pp-uid))
		    (if pp-data (setq pp-name (nth 1 pp-data)))
		    (irchat-w-insert irchat-MSN-buffer 
				     (format "%sUser %s <%s> removed from the contact list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-forward-list (irchat-remove-from-contact-list-with-name 
						   pp-uid
						   irchat-msn-forward-list))))
		 ((string-equal "RL" lstid)
		  (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								       irchat-msn-reverse-list))
			(pp-name pp-uid))
		    (if pp-data (setq pp-name (nth 1 pp-data)))
		    (irchat-w-insert irchat-MSN-buffer 
				     (format "%sUser %s <%s> removes you from his contact list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-reverse-list (irchat-remove-from-contact-list-with-name 
						   pp-uid
						   irchat-msn-reverse-list))))
		 ((string-equal "AL" lstid)
		  (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								       irchat-msn-allow-list))
			(pp-name pp-uid))
		    (if pp-data (setq pp-name (nth 1 pp-data)))
		    (irchat-w-insert irchat-MSN-buffer 
				     (format "%sUser %s <%s> removed from the allow list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-allow-list (irchat-remove-from-contact-list-with-name 
						   pp-uid
						   irchat-msn-allow-list))))

		 ((string-equal "BL" lstid)
		  (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								       irchat-msn-block-list))
			(pp-name pp-uid))
		    (if pp-data (setq pp-name (nth 1 pp-data)))
		    (irchat-w-insert irchat-MSN-buffer 
				     (format "%sUser %s <%s> removed from the block list.\n"
					     irchat-msn-info-prefix pp-name pp-uid))
		    (setq irchat-msn-block-list (irchat-remove-from-contact-list-with-name 
						   pp-uid
						   irchat-msn-block-list))))
		 (t t))))
	(t t)))

(defun irchat-msn-handle-RNG (parsed msg)
  (cond	((and (> (length parsed) 6)
	      (string-equal "CKI" (nth 3 parsed))
	      (string-match "\\([0-9][0-9.]*[0-9]\\):\\([1-9][0-9]*\\)" (nth 2 parsed)))
	 (let ((host (matching-substring (nth 2 parsed) 1))
	       (service (string-to-int (matching-substring (nth 2 parsed) 2)))
	       (sid (nth 1 parsed))
	       (hash (nth 4 parsed))
	       (pp-uid (nth 5 parsed))
	       (pp-name (irchat-msn-decode-name (nth 6 parsed))))
	   (irchat-msn-start-answering-sub-server host service (concat hash " " sid))))
	(t t)))

(eval-and-compile (provide 'irchat-msn-handle))

;;; eof (irchat-msn-handle.el)
