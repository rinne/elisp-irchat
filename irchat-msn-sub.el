;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-sub.el,v 3.8 2002/06/09 14:40:28 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

(defun irchat-msn-start-answering-sub-server (host service session)
  "Open network stream to remote MSN server."
  (if (not (irchat-msn-server-opened))
      nil
    (message "Connecting to MSN Conversation server on %s..." host)
    (if (irchat-msn-open-answering-sub-server host service session)
	(progn
	  (message "")))))
  
(defun irchat-msn-open-answering-sub-server (host service session)
  (let ((p (irchat-msn-open-sub-server-internal host service)))
    (if p
	(progn
	  (setq irchat-msn-sub-servers (cons p irchat-msn-sub-servers))
	  (irchat-msn-send-sub (nth 0 p)
			       "ANS %d %s %s"
			       (irchat-msn-sub-server-seqno (nth 0 p))
			       irchat-msn-uid
			       session)
	  (irchat-w-insert irchat-MSN-buffer 
			   (format "%sJoined conversation %s according to invitation.\n"
				   irchat-msn-info-prefix (nth 1 p)))
	  (set-process-sentinel (nth 0 p)
				'irchat-msn-sub-sentinel)
	  (set-process-filter (nth 0 p)
	  		      'irchat-msn-sub-filter)
	t)
      (progn
	(irchat-close-sub-server-internal)
	nil))))

(defun irchat-msn-start-calling-sub-server (host service session pending)
  "Open network stream to remote MSN server."
  (if (not (irchat-msn-server-opened))
      nil
    (message "Connecting to MSN Conversation server on %s..." host)
    (if (irchat-msn-open-calling-sub-server host service session pending)
	(progn
	  (message "")))))
  
(defun irchat-msn-open-calling-sub-server (host service session pending)
  (let ((p (irchat-msn-open-sub-server-internal host service)))
    (if p
	(progn
	  (setq p (irchat-set-nth 7 p pending))
	  (setq irchat-msn-sub-servers (cons p irchat-msn-sub-servers))
	  (irchat-msn-send-sub (nth 0 p)
			       "USR %d %s %s"
			       (irchat-msn-sub-server-seqno (nth 0 p))
			       irchat-msn-uid
			       session)
	  (irchat-w-insert irchat-MSN-buffer 
			   (format "%sSet up conversation %s.\n"
				   irchat-msn-info-prefix (nth 1 p)))
	  (set-process-sentinel (nth 0 p)
				'irchat-msn-sub-sentinel)
	  (set-process-filter (nth 0 p)
	  		      'irchat-msn-sub-filter)
	t)
      (progn
	(irchat-close-sub-server-internal)
	nil))))

(defun irchat-msn-open-sub-server-internal (host service)
  "Open connection to chat server on HOST by SERVICE."
  (condition-case err 
      (save-excursion
	(let ((conv-name (irchat-msn-generate-conversation-name))
	      (conv-server host)
	      (conv-service service)
	      (conv-process nil)
	      (conv-buffer nil))
	  (setq conv-buffer (get-buffer-create (concat "*"
						       (replace-in-string conv-name "\\$" "")
						       "*")))
	  (set-buffer conv-buffer)
	  (kill-all-local-variables)
	  (irchat-buffer-disable-undo (current-buffer))
	  (erase-buffer)
	  (setq conv-process
		(open-network-stream "MSN-SUB" 
				     (current-buffer)
				     conv-server
				     conv-service))
	  (list conv-process conv-name conv-server conv-service conv-buffer 1 '() nil)))
    nil))

(defun irchat-msn-sub-server-search-with-process (process)
  (let ((l irchat-msn-sub-servers)
	(r nil))
    (while l
      (if (eq (car (car l)) process)
	  (setq r (car l)
		l nil)
	(setq l (cdr l))))
    r))

(defun irchat-msn-sub-server-search-with-name (name)
  (let ((l irchat-msn-sub-servers)
	(r nil))
    (while l
      (if (string-equal (nth 1 (car l)) name)
	  (setq r (car l)
		l nil)
	(setq l (cdr l))))
    r))

(defun irchat-msn-sub-server-remove-with-process (process)
  (let ((l irchat-msn-sub-servers)
	(p nil)
	(r nil))
    (while l
      (if (not (eq (car (car l)) process))
	  (setq r (cons (car l) r))
	(setq p (car l)))
      (setq l (cdr l)))
    (setq irchat-msn-sub-servers r)
    p))

(defun irchat-msn-sub-server-seqno (process)
  (let ((p (irchat-msn-sub-server-remove-with-process process)))
    (if (null p)
	(error "Process not found."))
    (let ((seq (nth 5 p)))
      (setq p (irchat-set-nth 5 p (+ seq 1)))
      (setq irchat-msn-sub-servers (cons p irchat-msn-sub-servers))
      seq)))

(defun irchat-msn-sub-sentinel (proc status)
  "Sentinel function for Irchat MSN process."
  (let ((p (irchat-msn-sub-server-remove-with-process proc)))
    (if p
	(progn
	  (if (nth 7 p)
	      (irchat-w-insert irchat-MSN-buffer 
			       (format "%sCan't send message "%s" to user %s <%s>\n"
				       irchat-msn-error-prefix
				       (if (nth 2 (nth 7 p)) (nth 2 (nth 7 p)) (nth 1 (nth 7 p)))
				       (irchat-msn-name-cache-get (nth 0 (nth 7 p)))
				       (nth 0 (nth 7 p)))))
	  (kill-buffer (nth 4 p))
	  (irchat-w-insert irchat-MSN-buffer 
			   (format "%sConversation %s terminated.\n"
				   irchat-msn-info-prefix (nth 1 p)))))))

(defun irchat-msn-sub-filter (process output)
  "Filter function for IRC server process."
  (let ((obuf (current-buffer))
	(data (match-data))
	(output (irchat-decode-coding-string output))
	bol)
    (irchat-insert-to-debug output "<-MSN-SUB- ")
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (while (re-search-forward "\n\n" (point-max) t)
      (delete-char -1)) ; This hack (from mta) is for 2.4 servers
    (goto-char (point-min))
    (if (string-match "\n" output)
	(irchat-msn-handle-sub-message process))
    (set-buffer obuf)
    (store-match-data data)))

(defun irchat-msn-handle-sub-message (process)
  "Called when we have at least one line of output from the MSN server."
  (let ((obuf (current-buffer)))
    (while (and (buffer-live-p obuf)
		(irchat-msn-sub-consume-message process))
      (if (buffer-live-p obuf)
	  (set-buffer obuf)))))

(defun irchat-msn-sub-consume-message (proc)
  (let ((s (buffer-substring (point-min) (point-max))))
    (cond ((or (string-match "^\\(MSG \\([^ ][^ ]*\\) \\([^ ]*\\) \\([0-9][0-9]*\\)\r\n\\)"
			     s)
	       (string-match "\\(^MSG \\([^ ][^ ]*\\) \\([^ ]*\\) \\([0-9][0-9]*\\)\n\\)"
			     s))
	   (let ((s1 (matching-substring s 1))
		 (s2 (matching-substring s 2))
		 (s3 (matching-substring s 3))
		 (s4 (matching-substring s 4)))
	     (let ((cmd-len (length s1))
		   (pp-uid s2)
		   (pp-name (irchat-msn-decode-name s3))
		   (len (string-to-int s4)))
	       (setq s (substring s cmd-len (length s)))
	       (if (not (< (length s) len))
		   (let ((msg (substring s 0 len)))
		     (delete-region (point-min) (+ (point-min) cmd-len len))
		     (irchat-msn-sub-handle-MSG-message proc cmd-len pp-uid pp-name len msg)
		     t)
		 nil))))
	  ((and (not (or (string-match "^MSG [^ ][^ ]* [^ ]* [0-9][0-9]*\r\n" s)
			 (string-match "^MSG [^ ][^ ]* [^ ]* [0-9][0-9]*\n" s)))
		(or (string-match "^\\(\\(.*\\)\r\n\\)" s)
		    (string-match "^\\(\\(.*\\)\n\\)" s)))
	   (let ((msg (matching-substring s 2))
		 (len (length (matching-substring s 1))))
	     (delete-region (point-min) (+ (point-min) len))
	     (if (string-match "^\\([^ \t][^ \t]*\\)[ \t]" msg)
		 (progn
		   (setq cmd (upcase (matching-substring msg 1)))
		   (setq handler (intern (concat "irchat-msn-sub-handle-" cmd))))
	       (progn
		 (setq cmd "")
		 (setq handler nil)))
	     (let ((ml (irchat-msn-proto-msg-parse msg)))
	       (if ml
		   (if (and handler (fboundp handler))
		       (apply handler (list proc ml msg))
		     (irchat-msn-sub-handle-generic proc ml msg))))
	     t))
	  (t nil))))

(defun irchat-msn-sub-handle-generic (process parsed msg)
  nil)

(defun irchat-msn-sub-handle-MSG-message (process cmd-len pp-uid pp-name len msg)
  (let ((p (irchat-msn-sub-server-search-with-process process)))
    (if p
	(let ((m (irchat-msn-parse-message msg)))
	  (if m
	      (if (and (> (length (cdr m)) 0)
		       (irchat-msn-message-header-val "Content-type" m)
		       (string-match "TEXT/PLAIN" (upcase (irchat-msn-message-header-val "Content-type" m))))
		  (progn
		    (setq irchat-msn-recipient-cache (if (> (length (nth 6 p)) 1) (nth 1 p) pp-uid))
		    (irchat-msn-name-cache-add pp-uid pp-name)
		    (irchat-w-insert irchat-MSN-MSG-buffer
				     (concat 
				      (if (> (length (nth 6 p)) 1)
					  (format irchat-msn-format-string-in2 pp-uid (nth 1 p))
					(format irchat-msn-format-string-in pp-uid))
				      " "
				      (irchat-msn-iso8859-1-to-utf8 (cdr m) t)
				      "\n"))
		    t))))
      t)))

(defun irchat-msn-sub-handle-BYE (process parsed msg)
  (let ((p (irchat-msn-sub-server-search-with-process process)))
    (cond ((and p
		(> (length parsed) 1))
	   (let ((pp-uid (nth 1 parsed)))
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%s%s left %s.\n"
				      irchat-msn-info-prefix 
				      pp-uid
				      (nth 1 p)))
	     (irchat-msn-conversation-remove-user (nth 0 p) pp-uid)
	     (setq p (irchat-msn-sub-server-search-with-process (nth 0 p)))
	     (if (and p (> 1 (length (nth 6 p))))
		 (irchat-msn-kill-conversation (nth 1 p)))))
	  (t nil))))

(defun irchat-msn-sub-handle-JOI (process parsed msg)
  (let ((p (irchat-msn-sub-server-search-with-process process)))
    (cond ((and p
		(> (length parsed) 2))
	   (let ((pp-uid (nth 1 parsed))
		 (pp-name (irchat-msn-decode-name (nth 2 parsed))))
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%s%s <%s> joined %s.\n"
				      irchat-msn-info-prefix 
				      pp-name
				      pp-uid
				      (nth 1 p)))
	     (irchat-msn-conversation-add-user (nth 0 p) pp-uid)
	     (setq p (irchat-msn-sub-server-search-with-process process))
	     (let ((pending (if p (nth 7 p) nil)))
	       (if pending
		   (let ((m (irchat-msn-make-message (nth 1 pending))))
		     (irchat-msn-sub-server-remove-with-process (nth 0 p))
		     (setq irchat-msn-sub-servers (cons (irchat-set-nth 7 p nil)
							irchat-msn-sub-servers))
		     (irchat-w-insert irchat-MSN-MSG-buffer
				      (concat 
				       (format 
					(if (or (null (nth 2 pending))
						(string-equal (nth 1 pending)
							      (nth 2 pending)))
					    irchat-msn-format-string-out
					  irchat-msn-format-string-out-e)
					(nth 0 pending))
				       " "
				       (if (null (nth 2 pending))
					   (nth 1 pending)
					 (nth 2 pending))
				       "\n"))
		     (irchat-msn-send-sub-raw (nth 0 p)
					      "MSG %d A %d\r\n%s"
					      (irchat-msn-sub-server-seqno (nth 0 p))
					      (length m)
					      m))))))
	  (t nil))))

(defun irchat-msn-sub-handle-IRO (process parsed msg)
  (let ((p (irchat-msn-sub-server-search-with-process process)))
    (cond ((and p
		(> (length parsed) 5))
	   (let ((num (string-to-int (nth 2 parsed)))
		 (tot (string-to-int (nth 3 parsed)))
		 (pp-uid (nth 4 parsed))
		 (pp-name (nth 5 parsed)))
	     (irchat-msn-name-cache-add pp-uid pp-name)
	     (irchat-w-insert irchat-MSN-buffer 
			      (format "%s%s <%s> joined %s.\n"
				      irchat-msn-info-prefix 
				      pp-name
				      pp-uid
				      (nth 1 p)))
	     (irchat-msn-conversation-add-user (nth 0 p) pp-uid)))
	  (t nil))))

(defun irchat-msn-sub-handle-USR (process parsed msg)
  (let ((p (irchat-msn-sub-server-search-with-process process)))
    (if p
	(let ((pending (nth 7 p)))
	  (if pending
	      (irchat-msn-send-sub (nth 0 p)
				   "CAL %d %s"
				   (irchat-msn-sub-server-seqno (nth 0 p))
				   (nth 0 pending))
	    t))
      t)))

(defun irchat-msn-sub-handle-CAL (process parsed msg)
  t)

(defun irchat-msn-kill-all-conversations ()
  (while irchat-msn-sub-servers
    (irchat-msn-kill-conversation (nth 1 (car irchat-msn-sub-servers)) t)))

(defun irchat-msn-kill-conversation (conversation &optional silent)
  (let ((p (irchat-msn-sub-server-search-with-name conversation)))
    (if p
	(progn
	  (irchat-msn-sub-server-remove-with-process (nth 0 p))
	  (set-process-sentinel (nth 0 p) nil)
	  (set-process-filter (nth 0 p) nil)
	  (delete-process (nth 0 p))
	  (kill-buffer (nth 4 p))
	  (if (and irchat-msn-recipient-cache
		   (string-equal irchat-msn-recipient-cache 
				 (nth 1 p)))
	      (setq irchat-msn-recipient-cache nil))
	  (if (null silent)
	      (irchat-w-insert irchat-MSN-buffer 
			       (format "%sConversation %s terminated.\n"
				       irchat-msn-info-prefix (nth 1 p))))
	  t)
      (progn
	(message "No such conversation.")
	nil))))

(defun irchat-msn-conversation-names ()
  (let ((l irchat-msn-sub-servers)
	(r '()))
    (while l
      (setq r (cons (nth 1 (car l)) r)
	    l (cdr l)))
    r))

(defun irchat-msn-conversation-names-alist ()
  (let ((l irchat-msn-sub-servers)
	(r '()))
    (while l
      (setq r (cons (list (nth 1 (car l))) r)
	    l (cdr l)))
    r))

(defun irchat-msn-conversation-add-user (proc user)
  (let ((p (irchat-msn-sub-server-remove-with-process proc)))
    (if p
	(let ((users (nth 6 p)))
	  (setq users (cons user (string-list-ci-delete user users)))
	  (setq p (irchat-set-nth 6 p users))
	  (setq irchat-msn-sub-servers (cons p irchat-msn-sub-servers))))))

(defun irchat-msn-conversation-remove-user (proc user)
  (let ((p (irchat-msn-sub-server-remove-with-process proc)))
    (if p
	(let ((users (nth 6 p)))
	  (setq users (string-list-ci-delete user users))
	  (setq p (irchat-set-nth 6 p users))
	  (setq irchat-msn-sub-servers (cons p irchat-msn-sub-servers))))))

(defun irchat-msn-sub-server-search-with-user (name)
  (let ((l irchat-msn-sub-servers)
	(r nil))
    (while l
      (if (and (eq 1 (length (nth 6 (car l))))
	       (string-equal (upcase (nth 0 (nth 6 (car l)))) (upcase name)))
	  (setq r (car l)
		l nil)
	(setq l (cdr l))))
    r))

(eval-and-compile (provide 'irchat-msn-sub))

;;; eof (irchat-msn-sub.el)
