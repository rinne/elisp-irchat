;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-handle.el,v 1.3 1997/02/05 15:46:57 too Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright in(eval-wfo

(eval-when-compile
  (require 'irchat-globals)  
  (require 'irchat-vars)
  (require 'irchat-inlines)
  (require 'irchat-filter))

(defun irchat-handle-error-msg (prefix rest)
  (message "IRC error: %s" rest))


(defun irchat-ifrest (rest &optional quote)
  (if (= 0 (length rest)) 
      "" 
    (if quote
	(regexp-quote (format " (%s)" rest))
      (format " (%s)" rest))))


(defun irchat-handle-channel-msg (prefix rest)
  (let ((ispart (string= rest "0")))
    (if (string= prefix irchat-nickname)
	(progn
	  (setq irchat-current-channel rest)
	  (setq irchat-channel-indicator
		(if ispart
		    "No channel"
		  (format "Channel %s" rest)))))
    (if ispart
	(if (not irchat-ignore-changes)
	    (irchat-w-insert irchat-D-buffer
			     (format "%s%s has left this channel\n" 
				     irchat-change-prefix prefix)))
      (if (not irchat-ignore-changes)
	  (irchat-w-insert irchat-D-buffer 
			   (format "%s%s has joined this channel\n"
				   irchat-change-prefix prefix))))
    (irchat-change-nick-of prefix prefix)))


(defun irchat-handle-nick-msg (prefix rest)
  (irchat-change-nick-of prefix rest)
  (put (intern rest irchat-obarray) 'chnl 
       (get (intern prefix irchat-obarray) 'chnl))
  (put (intern prefix irchat-obarray) 'chnl nil)
  (if (string= prefix irchat-nickname)
      (setq irchat-nickname rest))
  (irchat-w-insert irchat-D-buffer 
		   (format "%s%s is now known as %s\n" 
			   irchat-change-prefix prefix rest)))


(defun irchat-handle-notice-msg (prefix rest)
  (if (and irchat-shorten-kills
	   (string-match 
	    "Notice[: -]*Received KILL message for \\([^.]*\\)\\. From \\([^ ]*\\) Path: \\([^ ]*\\) ?\\(.*\\)"
	    rest))
      (let ((killed (matching-substring rest 1))
	    (killer (matching-substring rest 2))
	    (reason (matching-substring rest 4))
	    (buf (current-buffer)))
	(set-buffer irchat-KILLS-buffer)
	(goto-char (point-max))
	(irchat-w-insert irchat-K-buffer 
			 (format "%s\n" rest))
	(set-buffer buf)
	(irchat-w-insert irchat-D-buffer 
			 (format "%s%s KILLed %s %s\n" 
				 irchat-notice-prefix
				 killer killed 
				 (if (= (length reason) 0) 
				     "-No reason supplied-" 
				   reason))))
    (if (or (not irchat-ignore-extra-notices)
	    (not prefix)
	    (not (string-match "as being away" rest)))
	(if prefix
	    (cond 
					; prefixed clt-a-notice
	     ((string-match "\\(.*\\)" rest) 
	      (irchat-ctl-a-notice prefix rest))
	     ((and irchat-ignore-fakes
		   (string-match ".*Notice.*Fake:.*" rest))
	      t)
					; not a clt-a, but notice
	     ((string-match ".*Notice -- \\(.*\\)" rest) 
	      (irchat-w-insert irchat-D-buffer 
			       (format "%s%s: %s\n" 
				       irchat-notice-prefix
				       prefix (matching-substring rest 1))))
					; else send user a private message
	     (t 
	      (irchat-handle-privmsglike-msg prefix rest)))
	  (progn
	    ;; no prefix
	    (string-match "^\\([^ ]*\\) :\\(.*\\)" rest)
	    (irchat-w-insert irchat-D-buffer 
			     (format "%s%s\n"
				     irchat-notice-prefix (matching-substring rest 2))))))))


(defun irchat-handle-ping-msg (prefix rest)
  (irchat-send "PONG yourself")
  (irchat-maybe-poll))


(defun irchat-handle-pong-msg (prefix rest)
  ())


(defun irchat-handle-privmsg-msg (prefix rest)
  (if (and prefix
	   (irchat-ignore-this-p prefix irchat-userathost)
	   (irchat-msg-from-ignored prefix rest))
      nil
    (if (and (string-match "\007" rest) irchat-beep-on-bells)
	(beep t))
    (string-match "^\\([^ ]+\\) :\\(.*\\)" rest)
    (let ((chnl (matching-substring rest 1))
	  (temp (matching-substring rest 2))
	  (case-fold-search t))
      (if (string-match "\\(.*\\)" temp)
	  (setq temp (irchat-ctl-a-msg prefix temp)))
      (if (not (string= temp ""))
	  (progn
	    ;; only private messages to us get time-stamp
	    (if (and (string-equal "A" irchat-away-indicator) 
		     (string-ci-equal chnl irchat-nickname))
		(setq temp (format "%s (%s)" temp (if irchat-format-time-function
						      (apply irchat-format-time-function
							     (list (current-time-string)))
						    (current-time-string)))))
	    (cond
	     ((string-ci-equal chnl irchat-nickname)
	      (irchat-w-insert irchat-P-buffer 
			       (format "%s %s\n" 
				       (format irchat-format-string1 prefix) temp)))
	     
	     ((string-ci-equal chnl (or irchat-current-channel ""))
	      (if (irchat-user-on-this-channel prefix chnl)
		  (irchat-w-insert (irchat-pick-buffer chnl) 
				   (format "%s %s\n" 
					   (format irchat-format-string2 
						   prefix) temp))
		(irchat-w-insert (irchat-pick-buffer chnl) 
				 (format "%s %s\n" 
					 (format irchat-format-string4 
						 prefix) temp))))

	     (t ;; channel we are joined (not current)
	      (if (irchat-user-on-this-channel prefix chnl)
		  (irchat-w-insert (irchat-pick-buffer chnl) 
				   (format "%s %s\n" 
					   (format irchat-format-string3 
						   prefix chnl) temp))
		(irchat-w-insert (irchat-pick-buffer chnl) 
				 (format "%s %s\n" 
					 (format irchat-format-string5 
						 prefix chnl) temp)))))

	    (or (irchat-get-buffer-window (current-buffer))
		(not (string-ci-equal chnl irchat-nickname))
		(message "IRCHAT: A private message has arrived from %s" prefix)))))))

;; NOTICE
(defun irchat-handle-privmsglike-msg (prefix rest)
  (if (and prefix
	   (irchat-ignore-this-p prefix irchat-userathost)
	   (irchat-msg-from-ignored prefix rest))
      nil
    (string-match "^\\([^ ]+\\) :\\(.*\\)" rest)
    (let ((chnl (matching-substring rest 1))
	  (temp (matching-substring rest 2))
	  (case-fold-search t))
      (if (string-match "\\(.*\\)" temp)
	  (setq temp (irchat-ctl-a-msg prefix temp)))
      (if (not (string= temp ""))
	  (cond
	   ((string-ci-equal chnl irchat-nickname)
	    (irchat-w-insert irchat-D-buffer 
			     (format "%s %s\n" 
				     (format irchat-format-string0 prefix) temp)))
	     
	   ((string-ci-equal chnl (or irchat-current-channel ""))
	    (if (irchat-user-on-this-channel prefix chnl)
		(irchat-w-insert (irchat-pick-buffer chnl) 
				 (format "%s %s\n" 
					 (format irchat-format-string2 
						 prefix) temp))
	      (irchat-w-insert (irchat-pick-buffer chnl) 
			       (format "%s %s\n" 
				       (format irchat-format-string4 
					       prefix) temp))))

	   (t ;; channel we are joined (not current)
	    (if (irchat-user-on-this-channel prefix chnl)
		(irchat-w-insert (irchat-pick-buffer chnl) 
				 (format "%s %s\n" 
					 (format irchat-format-string3 
						 prefix chnl) temp))
	      (irchat-w-insert (irchat-pick-buffer chnl) 
			       (format "%s %s\n" 
				       (format irchat-format-string5 
					       prefix chnl) temp)))))
	))))


(defun irchat-handle-wall-msg (prefix rest)
  "Handle the WALL message."
  (irchat-w-insert irchat-D-buffer 
		   (format "%s%s %s\n" 
			   irchat-broadcast-prefix 
			   (if prefix (concat "from " prefix) "") rest)))


(defun irchat-handle-wallops-msg (prefix rest)
  "Handle the WALLOPS message."
  (if irchat-show-wallops
      (irchat-w-insert irchat-D-buffer 
		       (format "%s%s: %s\n" 
			       irchat-wallops-prefix 
			       (if prefix prefix "UNKNOWN") rest)))
  (let ((buf (current-buffer)))
    (set-buffer irchat-WALLOPS-buffer)
    (goto-char (point-max))
    (irchat-w-insert irchat-W-buffer 
		     (format "%s%s %s\n" 
			     irchat-wallops-prefix 
			     (if prefix 
				 (concat "from " prefix) "") 
			     rest))
    (set-buffer buf)))


(defun irchat-handle-quit-msg (prefix rest)
  "Handle the QUIT message."
  (if (not irchat-ignore-changes)
      (if irchat-compress-changes
	  (let* ((text (format " \\(has\\|have\\) left IRC%s" 
			       (irchat-ifrest rest t)))
		 (match (format "^%s.*%s$" 
				(regexp-quote irchat-change-prefix) text))
		 (default (format "%s%s has left IRC%s\n" 
				  irchat-change-prefix prefix (irchat-ifrest rest))))
	    (irchat-w-replace irchat-D-buffer 
			      match default text
			      (format ", %s have left IRC%s" 
				      prefix (irchat-ifrest rest))))
	(irchat-w-insert irchat-D-buffer
			 (format "%s%s has left IRC%s\n" 
				 irchat-change-prefix prefix (irchat-ifrest rest)))))
  (irchat-change-nick-of prefix nil))


(defun irchat-handle-topic-msg (prefix rest)
  "Handle the TOPIC message."
  (if (string-match "\\([^ :]*\\)[: ]*\\(.*\\)" rest)
      (let ((chnl (matching-substring rest 1))
	    (topic (matching-substring rest 2)))
	(if (not irchat-ignore-changes)
	    (irchat-w-insert (irchat-pick-buffer chnl)
			     (format "%sNew topic on channel %s set by %s: %s\n" 
				     irchat-change-prefix chnl prefix topic))))
    (message "IRCHAT: Strange TOPIC")))


(defun irchat-handle-mode-msg (prefix rest)
  "Handle the MODE message."
  (if (not irchat-ignore-changes)
      (let ((chnl " ") (str ""))
	(if (or (and (string-match "\\([^ ]*\\) :*\\(.*\\)" rest)
		     (setq chnl (matching-substring rest 1)
			   str (matching-substring rest 2)
			   str (if (= (aref str (1- (length str))) 32) (substring str 0 -1) str)))
		(and (string-match " :\\(.*\\)" rest) (setq str (matching-substring rest 1))))
	    (if irchat-compress-changes
		(let* ((text (format "\n" rest))
		       (match (format "^%sNew mode for %s set by %s: " 
				      (regexp-quote irchat-change-prefix) chnl prefix))
		       (default (format "%sNew mode for %s set by %s: %s\n" 
					irchat-change-prefix chnl prefix str)))
		  (irchat-w-replace (irchat-pick-buffer chnl)
				    match default text (format ", %s\n" str)))
	      (irchat-w-insert (irchat-pick-buffer chnl)
			       (format "%sNew mode for %s set by %s: %s\n" 
				       irchat-change-prefix chnl prefix str)))
	  (message "IRCHAT: Strange MODE")))))


(defun irchat-handle-kick-msg (prefix rest)
  "Handle the KICK message."
  (if (string-match "^\\([^ ]*\\) \\([^ ]*\\) *:\\(.*$\\)" rest)
      (let ((match1 (matching-substring rest 1))
	    (match2 (matching-substring rest 2))
	    (match3 (matching-substring rest 3)))
	(if (string= match2 irchat-nickname)
	  (progn
	    (irchat-w-insert (irchat-pick-buffer match1)
			     (format "%sYou were kicked off channel %s by %s (%s).\n" irchat-change-prefix match1 prefix match3))
	    (setq 
	     irchat-current-channels 
	           (string-list-ci-delete match1 irchat-current-channels)
	     irchat-current-channel (car irchat-current-channels)
	     irchat-channel-indicator (if irchat-current-channel 
					  (format "Channel %s" 
						  irchat-current-channel) 
					"No channel"))
	    (irchat-remove-from-thischannel irchat-nickname match1))
	  (irchat-w-insert irchat-D-buffer 
			   (format "%s%s has kicked %s out%s\n" 
				   irchat-change-prefix prefix match2
				   (if (string= (or irchat-current-channel "") match1) 
				       "" 
				     (format " from channel %s" match1))))))
    (message "IRCHAT: Strange KICK.")))


(defun irchat-handle-invite-msg (prefix rest)
  (if (string-match " :\\([^ ]+\\)" rest)
      (let ((chnl (matching-substring rest 1)))
	(irchat-w-insert irchat-D-buffer 
			 (format "*** %s invites you to channel %s\n" prefix chnl))
	(setq irchat-invited-channel chnl))
    (message "IRCHAT: Strange INVITE")))


(defun irchat-handle-kill-msg (prefix rest)
  (if (string-match "[^ ]+ +:\\(.*\\)" rest)
      (let ((path (matching-substring rest 1)))
	(irchat-w-insert irchat-D-buffer 
			 (format "*** IRCHAT: You were killed by %s. Path: %s. RIP\n" 
				 prefix path)))
    (message "IRCHAT: strange KILL"))
  (setq irchat-channel-indicator "No channel"))


(defun irchat-handle-join-msg (prefix rest) ; kmk, 14101990
  (if (string-match "\\([^ ]*\\)\ .*" rest)
      (setq rest (matching-substring rest 1))) ;; throw away the channel mode
  (if (string= prefix irchat-nickname)
      (progn
	(setq irchat-current-channel rest)
	(setq irchat-current-channels
	      (cons irchat-current-channel irchat-current-channels))
 	(setq irchat-channel-indicator
	      (format "Channel %s" rest)))
    (irchat-add-to-channel prefix rest))
  (if (not irchat-ignore-changes)
      (if irchat-compress-changes
	  (let* ((text (format " \\(has\\|have\\) joined channel %s" 
			       (regexp-quote rest)))
		 (match (format "^%s.* .*%s$" 
				(regexp-quote irchat-change-prefix) text))
		 (default (format "%s%s (%s) has joined channel %s\n" 
				  irchat-change-prefix prefix irchat-userathost rest)))
	    (irchat-w-replace (irchat-pick-buffer rest) match default text
			      (format ", %s (%s) have joined channel %s" 
				      prefix irchat-userathost rest)))
	  (irchat-w-insert (irchat-pick-buffer rest) 
			   (format "%s%s (%s) has joined channel %s\n" 
				   irchat-change-prefix prefix irchat-userathost rest))))
  (irchat-change-nick-of prefix prefix))


(defun irchat-handle-part-msg (prefix rest) ; kmk, 14101990
  (if (string-match "\\([^ ]*\\)\ .*" rest)
      (setq rest (matching-substring rest 1))) ;; throw away user given info.
  (if (string= prefix irchat-nickname)
      (setq irchat-current-channels
	    (string-list-ci-delete rest irchat-current-channels)
	    irchat-current-channel (car irchat-current-channels)
	    irchat-channel-indicator (if irchat-current-channel (format "Channel %s" irchat-current-channel) "No channel")))
  (if (not irchat-ignore-changes)
      (if irchat-compress-changes
	  (let* ((text (format " \\(has\\|have\\) left channel %s"
			       (regexp-quote rest)))
		 (match (format "^%s.*%s$" (regexp-quote irchat-change-prefix) text))
		 (default (format "%s%s has left channel %s\n"
				  irchat-change-prefix prefix rest)))
	    (irchat-w-replace (irchat-pick-buffer rest) 
			      match default text (format ", %s have left channel %s" prefix rest)))
	(irchat-w-insert (irchat-pick-buffer rest) 
			 (format "%s%s has left channel %s\n" irchat-change-prefix prefix rest))))
  (irchat-remove-from-thischannel prefix rest)
  (irchat-change-nick-of prefix prefix))


(defun irchat-pick-buffer (chnl)
  (let ((mylist irchat-buffer-preferences)
	(found nil)
	(result nil))
    (while (and (not found) mylist)
      (if (string-match (car (car mylist)) chnl)
	  (setq result (car (cdr (car mylist)))
		found t))
      (setq mylist (cdr mylist)))
    (if result
	result
      irchat-D-buffer)))

;;;
;;; eof
;;;
