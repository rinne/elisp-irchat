;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-handle.el,v 3.2 2002/06/04 20:21:39 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

(defun irchat-msn-handle-generic (msg)
  t)

(defun irchat-msn-handle-MSG-message (cmd-len pp-uid pp-name len msg)
  t)

(defun irchat-msn-handle-VER (msg)
  (if (eq irchat-msn-connection-phase 'ver-sent)
      (progn
	(irchat-msn-send "INF %d" (irchat-msn-seqno))
	(setq irchat-msn-connection-phase 'inf-sent))
    (irchat-msn-protocol-error)))

(defun irchat-msn-handle-INF (msg)
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

(defun irchat-msn-handle-NLN (msg)
  (if (string-match 
       "^NLN \\([^ ][^ ]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
       msg)
      (let ((stat (matching-substring msg 1))
	    (pp-uid (matching-substring msg 2))
	    (pp-name (irchat-msn-decode-name (matching-substring msg 3))))
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
	  (irchat-w-insert irchat-MSN-buffer 
			   (format "%sUser %s <%s> status is %s.\n"
				   irchat-msn-info-prefix
				   pp-name
				   pp-uid
				   (irchat-msn-status-string stat)))))))

(defun irchat-msn-handle-ILN (msg)
  (if (string-match 
       "^ILN [0-9][0-9]* \\([^ ][^ ]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
       msg)
      (let ((stat (matching-substring msg 1))
	    (pp-uid (matching-substring msg 2))
	    (pp-name (irchat-msn-decode-name (matching-substring msg 3))))
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
	  (irchat-w-insert irchat-MSN-buffer 
			   (format "%sUser %s <%s> status is %s.\n"
				   irchat-msn-info-prefix
				   pp-name
				   pp-uid
				   (irchat-msn-status-string stat)))))))

(defun irchat-msn-handle-FLN (msg)
  (if (string-match 
       "^FLN \\([^ ][^ ]*\\)"
       msg)
      (let ((pp-uid (matching-substring msg 1)))
	(setq irchat-msn-online-list (irchat-remove-from-contact-list-with-name pp-uid
										irchat-msn-online-list))
	(irchat-w-insert irchat-MSN-buffer 
			 (format "%sUser %s goes Offline.\n"
				 irchat-msn-info-prefix
				 pp-uid)))))

(defun irchat-msn-handle-USR (msg)
  (cond ((and (eq irchat-msn-connection-phase 'usr-sent)
	      (string-match 
	       "^USR [0-9][0-9]* MD5 S \\([0-9][0-9]*\\.[0-9][0-9]*\\)"
	       msg))
	 (let ((salt (matching-substring msg 1))
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
	      (string-match 
	       "^USR [0-9][0-9]* OK \\(.*\\)"
	       msg))
	 (setq irchat-msn-password-cache irchat-msn-last-password)
	 (irchat-msn-send "CHG %d NLN" (irchat-msn-seqno))
	 (setq irchat-msn-online-list '()) ;; Server sends online list after CHG
	 (setq irchat-msn-connection-phase 'chg-sent))
       (t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-XFR (msg)
  (cond ((and (eq irchat-msn-connection-phase 'usr-sent)
	      (string-match 
	       "^XFR [0-9][0-9]* NS \\([0-9][0-9.]*[0-9]\\):\\([1-9][0-9]*\\) [0-9][0-9]*"
	       msg))
	 (let ((host (matching-substring msg 1))
	       (service (string-to-int (matching-substring msg 2))))
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%sServer %s redirects the connection to %s.\n"
				    irchat-msn-info-prefix
				    irchat-msn-server-int
				    host))
	   (irchat-msn-close-server)
	   (setq irchat-msn-connection-phase 'xfr-received)
	   (irchat-msn-start-server host service)))
	((string-match 
	  "^XFR [0-9][0-9]* SB \\([0-9][0-9.]*[0-9]\\):\\([1-9][0-9]*\\) CKI \\([^ ][^ ]*\\)"
	  msg)
	 (let ((host (matching-substring msg 1))
	       (hash (matching-substring msg 3))
	       (service (string-to-int (matching-substring msg 2))))
	   (if irchat-msn-messages-pending-sb
	       (let ((pending (car irchat-msn-messages-pending-sb)))
		 (setq irchat-msn-messages-pending-sb (cdr irchat-msn-messages-pending-sb))
		 (irchat-msn-start-calling-sub-server host service hash pending))
	     t)))
	(t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-911 (msg)
  (cond ((eq irchat-msn-connection-phase 'usr-pass-sent)
	 (progn
	   (setq irchat-msn-connection-phase nil)
	   (irchat-msn-close-server)
	   (irchat-msn-start-server)))
	(t 
	 (progn 
	   (message "MSN: 911, Giving up!")
	   (irchat-msn-close-server)))))

(defun irchat-msn-handle-QRY (msg)
  t)

(defun irchat-msn-handle-CHL (msg)
  (if (string-match 
       "^CHL [0-9][0-9]* \\([0-9][0-9]*\\)"
       msg)
      (let ((c1 (matching-substring msg 1))
	    (c2 irchat-msn-challenge-cookie))
	(irchat-msn-send-raw "QRY %d %s 32\r\n%s"
			     (irchat-msn-seqno)
			     "msmsgs@msnmsgr.com"
			     (md5 (concat c1 c2))))
    (irchat-msn-protocol-error)))

(defun irchat-msn-handle-CHG (msg)
  (cond ((eq irchat-msn-connection-phase 'chg-sent)
	 (progn
	   (setq irchat-msn-connection-phase 'online)
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%sConnected MSN Messenger Server %s on %s.\n"
				    irchat-msn-info-prefix
				    irchat-msn-server-int
				    (if irchat-format-time-function 
					(apply irchat-format-time-function 
					       (list (current-time-string))) 
				      (current-time-string))))
	   (irchat-msn-send "SYN %d 0" (irchat-msn-seqno))))
	((string-match
	  "^CHG [0-9][0-9]* \\([^ ][^ ]*\\)"
	  msg)
	 (let ((stat (matching-substring msg 1)))
	   (irchat-w-insert irchat-MSN-buffer 
			    (format "%sYour status changed to %s.\n"
				    irchat-msn-info-prefix
				    (irchat-msn-status-string stat)))))
	(t t)))

(defun irchat-msn-handle-SYN (msg)
  (setq irchat-msn-lists-in-sync nil))

(defun irchat-msn-handle-LST (msg)
  (cond ((string-match 
	  "^LST [0-9][0-9]* FL \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\) \\([0-9][0-9]*\\)"
	  msg)
	 (let ((s1 (matching-substring msg 1))
	       (s2 (matching-substring msg 2))
	       (s3 (matching-substring msg 3))
	       (s4 (matching-substring msg 4))
	       (s5 (matching-substring msg 5))
	       (s6 (matching-substring msg 6)))
	   (let ((lstver (string-to-int s1))
		 (num (string-to-int s2))
		 (tot (string-to-int s3))
		 (pp-uid s4)
		 (pp-name (irchat-msn-decode-name s5))
		 (gid (string-to-int s6)))
	     (if (eq num 1)
		 (setq irchat-msn-forward-list '()))
	     (setq irchat-msn-forward-list 
		   (cons (list pp-uid pp-name gid nil)
			 (irchat-remove-from-contact-list-with-name pp-uid
								    irchat-msn-forward-list)))
	     t)))
	((string-match
	  "^LST [0-9][0-9]* FL \\([0-9][0-9]*\\) 0 0"
	  msg)
	 (setq irchat-msn-forward-list '()))
	((string-match 
	  "^LST [0-9][0-9]* RL \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (num (string-to-int (matching-substring msg 2)))
	       (tot (string-to-int (matching-substring msg 3)))
	       (pp-uid (matching-substring msg 4))
	       (pp-name (irchat-msn-decode-name (matching-substring msg 5)))
	       (gid -1))
	   (if (eq num 1)
	       (setq irchat-msn-reverse-list '()))
	   (setq irchat-msn-reverse-list 
		 (cons (list pp-uid pp-name gid nil)
		       (irchat-remove-from-contact-list-with-name pp-uid
								  irchat-msn-reverse-list)))
	   (if (eq num tot)
	       (progn
		 (setq irchat-msn-lists-in-sync t)
		 (if irchat-msn-show-lists-in-startup
		     (irchat-Command-msn-list-lists))))
	   t))
	((string-match
	  "^LST [0-9][0-9]* RL \\([0-9][0-9]*\\) 0 0"
	  msg)
	 (progn
	   (setq irchat-msn-reverse-list '())
	   (setq irchat-msn-lists-in-sync t)
	   (irchat-Command-msn-list-lists)))
	((string-match 
	  "^LST [0-9][0-9]* AL \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (num (string-to-int (matching-substring msg 2)))
	       (tot (string-to-int (matching-substring msg 3)))
	       (pp-uid (matching-substring msg 4))
	       (pp-name (irchat-msn-decode-name (matching-substring msg 5)))
	       (gid -1))
	   (if (eq num 1)
		 (setq irchat-msn-allow-list '()))
	   (setq irchat-msn-allow-list 
		 (cons (list pp-uid pp-name gid nil)
		       (irchat-remove-from-contact-list-with-name pp-uid
								  irchat-msn-allow-list)))
	   t))
	((string-match
	  "^LST [0-9][0-9]* AL \\([0-9][0-9]*\\) 0 0"
	  msg)
	 (setq irchat-msn-allow-list '()))
	((string-match 
	  "^LST [0-9][0-9]* BL \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([0-9][0-9]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (num (string-to-int (matching-substring msg 2)))
	       (tot (string-to-int (matching-substring msg 3)))
	       (pp-uid (matching-substring msg 4))
	       (pp-name (irchat-msn-decode-name (matching-substring msg 5)))
	       (gid -1))
	   (if (eq num 1)
	       (setq irchat-msn-block-list '()))
	   (setq irchat-msn-block-list 
		 (cons (list pp-uid pp-name gid nil)
		       (irchat-remove-from-contact-list-with-name pp-uid
								  irchat-msn-block-list)))
	   t))
	((string-match
	  "^LST [0-9][0-9]* BL \\([0-9][0-9]*\\) 0 0"
	  msg)
	 (setq irchat-msn-block-list '()))
	(t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-ADD (msg)
  (cond	((string-match 
	  "^ADD [0-9][0-9]* RL \\([0-9][0-9]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (pp-uid (matching-substring msg 2))
	       (pp-name (irchat-msn-decode-name (matching-substring msg 3)))
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
	((string-match 
	  "^ADD [0-9][0-9]* \\([^ ][^ ]*\\) \\([0-9][0-9]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)\\(.*\\)"
	  msg)
	 (let ((s1 (matching-substring msg 1))
	       (s2 (matching-substring msg 2))
	       (s3 (matching-substring msg 3))
	       (s4 (matching-substring msg 4))
	       (s5 (matching-substring msg 5)))
	   (let ((ln (upcase s1))
		 (lstver (string-to-int s2))
		 (pp-uid s3)
		 (pp-name s4)
		 (rest s5))
	     (let ((gid (if (string-match "^ \\([0-9][0-9]*\\)" rest)
			    (string-to-int (matching-substring rest 1))
			  -1)))
	       (cond ((string-equal "FL" ln)
		      (progn
			(irchat-w-insert irchat-MSN-buffer
					 (format "%sUser %s <%s> added to the contact list.\n"
						 irchat-msn-info-prefix pp-name pp-uid))
			(setq irchat-msn-forward-list 
			      (cons (list pp-uid pp-name gid nil)
				    (irchat-remove-from-contact-list-with-name pp-uid
									       irchat-msn-forward-list)))
			t))
		     ((string-equal "AL" ln)
		      (progn
			(irchat-w-insert irchat-MSN-buffer
					 (format "%sUser %s <%s> added to the allow list.\n"
						 irchat-msn-info-prefix pp-name pp-uid))
			(setq irchat-msn-allow-list 
			      (cons (list pp-uid pp-name gid nil)
				    (irchat-remove-from-contact-list-with-name pp-uid
									       irchat-msn-allow-list)))
			t))
		     ((string-equal "BL" ln)
		      (progn
			(irchat-w-insert irchat-MSN-buffer
					 (format "%sUser %s <%s> added to the block list.\n"
						 irchat-msn-info-prefix pp-name pp-uid))
			(setq irchat-msn-block-list 
			      (cons (list pp-uid pp-name gid nil)
				    (irchat-remove-from-contact-list-with-name pp-uid
									       irchat-msn-block-list)))
			t))
		     (t
		      t))))))
	(t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-REM (msg)
  (cond	((string-match 
	  "^REM [0-9][0-9]* RL \\([0-9][0-9]*\\) \\([^ ][^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (pp-uid (matching-substring msg 2)))
	   (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								irchat-msn-reverse-list)))
	     (let ((pp-name (if (null pp-data) "???" (nth 1 pp-data))))
	       (irchat-w-insert irchat-MSN-buffer 
				(format "%sUser %s <%s> removes you from the contact list.\n"
					irchat-msn-info-prefix pp-name pp-uid))
	       (setq irchat-msn-reverse-list (irchat-remove-from-contact-list-with-name 
					      pp-uid
					      irchat-msn-reverse-list))
	       t))))
	((string-match 
	  "^REM [0-9][0-9]* FL \\([0-9][0-9]*\\) \\([^ ][^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (pp-uid (matching-substring msg 2)))
	   (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								irchat-msn-forward-list)))
	     (let ((pp-name (if (null pp-data) "???" (nth 1 pp-data))))
	       (irchat-w-insert irchat-MSN-buffer 
				(format "%sUser %s <%s> removed from the contact list.\n"
					irchat-msn-info-prefix pp-name pp-uid))
	       (setq irchat-msn-forward-list (irchat-remove-from-contact-list-with-name 
					      pp-uid
					      irchat-msn-forward-list))
	       t))))
	((string-match 
	  "^REM [0-9][0-9]* AL \\([0-9][0-9]*\\) \\([^ ][^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (pp-uid (matching-substring msg 2)))
	   (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								irchat-msn-allow-list)))
	     (let ((pp-name (if (null pp-data) "???" (nth 1 pp-data))))
	       (irchat-w-insert irchat-MSN-buffer 
				(format "%sUser %s <%s> removed from the allow list.\n"
					irchat-msn-info-prefix pp-name pp-uid))
	       (setq irchat-msn-allow-list (irchat-remove-from-contact-list-with-name 
					    pp-uid
					    irchat-msn-allow-list))
	       t))))
	((string-match 
	  "^REM [0-9][0-9]* BL \\([0-9][0-9]*\\) \\([^ ][^ ]*\\)"
	  msg)
	 (let ((lstver (string-to-int (matching-substring msg 1)))
	       (pp-uid (matching-substring msg 2)))
	   (let ((pp-data (irchat-search-contact-list-with-name pp-uid
								irchat-msn-block-list)))
	     (let ((pp-name (if (null pp-data) "???" (nth 1 pp-data))))
	       (irchat-w-insert irchat-MSN-buffer 
				(format "%sUser %s <%s> removed from the block list.\n"
					irchat-msn-info-prefix pp-name pp-uid))
	       (setq irchat-msn-allow-list (irchat-remove-from-contact-list-with-name 
					    pp-uid
					    irchat-msn-block-list))
	       t))))
	(t (irchat-msn-protocol-error))))

(defun irchat-msn-handle-RNG (msg)
  (cond	((string-match 
	  "^RNG \\([0-9][0-9]*\\) \\([0-9][0-9.]*[0-9]\\):\\([1-9][0-9]*\\) CKI \\([^ ][^ ]*\\) \\([^ ][^ ]*\\) \\([^ ]*\\)"
	  msg)
	 (let ((s1 (matching-substring msg 1))
	       (s2 (matching-substring msg 2))
	       (s3 (matching-substring msg 3))
	       (s4 (matching-substring msg 4))
	       (s5 (matching-substring msg 5))
	       (s6 (matching-substring msg 6)))
	   (let ((sid s1)
		 (host s2)
		 (service (string-to-int s3))
		 (hash s4)
		 (pp-uid s5)
		 (pp-name (irchat-msn-decode-name s6)))
	     (irchat-msn-start-answering-sub-server host service (concat hash " " sid)))))
	(t t)))

(eval-and-compile (provide 'irchat-msn-handle))

;;; eof (irchat-msn-handle.el)
