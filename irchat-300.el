;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-300.el,v 1.3 1997/02/06 11:49:36 tmo Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile
  (require 'irchat-globals)  
  (require 'irchat-vars)
  (require 'irchat-inlines))

;;;
;;; 300 replies
;;;

(defvar irchat-recursing-whois nil)
(defvar irchat-recursing-whowas nil)

(defun irchat-handle-300-msgs (number prefix rest)
  "Generic handler for 3?? messages. 
This is called if no specific handler exists"
  (if (string-match "[^ ]* \\([^ :]*\\) *\\([^ :]*\\) *:\\(.*\\)" rest)
      (let ((target1 (matching-substring rest 1))
	    (target2 (matching-substring rest 2))
	    (msg (matching-substring rest 3)))
	(cond ((string-equal target1 "")
	       (irchat-w-insert irchat-300-buffer 
				(format "*** %s\n" msg)))
	      ((string-equal target2 "")
	       (irchat-w-insert irchat-300-buffer 
				(format "*** %s (%s)\n" msg target1)))
	      (t
	       (irchat-w-insert irchat-300-buffer 
				(format "*** %s %s (%s)\n" 
					target1 msg target2))))
	)
    (message "IRCHAT: Strange %s reply" number)))


(defun irchat-handle-301-msg (prefix rest)
  "Handle the 301 RPL_AWAY."
  (if (string-match "^[^ ]+ \\([^ ]+\\) +:\\(.*\\)" rest)
      (let ((who (matching-substring rest 1))
	    (iswhat (matching-substring rest 2)))
	(if (not irchat-recursing-whois)
	    (irchat-w-insert irchat-300-buffer 
	     (format 
	      "*** %s is marked as being AWAY, but left the message:\n%s\n" 
	      who iswhat))))
    (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 301 reply")))


(defun irchat-handle-302-msg (prefix rest)
  "Handle the 302 USERHOST."
  (while (string-match 
	  "^[^ ]* :[ ]*\\([^*=]+\\)\\([*]*\\)=\\([+-]\\)\\([^ ]+\\)" rest)
    (let ((nick (matching-substring rest 1))
	  (oper (matching-substring rest 2))
	  (away (matching-substring rest 3))
	  (who (matching-substring rest 4)))
      (irchat-w-insert irchat-300-buffer 
		       (format "%sNick %s is %s [%s, %s]\n"
			       irchat-info-prefix
			       nick who 
			       (concat 
				(if (string= oper "")
				    "Not ")
				"Operator")
			       (concat 
				(if (string= away "+")
				    "Not ")
				"AWAY")))
      (setq rest (concat " :" (substring rest (match-end 4) nil))))))


(defun irchat-handle-303-msg (prefix rest)
  "Handle the 303 ISON"
  (if (string-match "[^ ]+ :\\(.*\\)" rest)
      (if (string= (matching-substring rest 1) "")
	  (irchat-w-insert irchat-300-buffer 
			   (format "%sNo one you requested is on now.\n"
				   irchat-info-prefix))
	(irchat-w-insert irchat-300-buffer 
			 (format "%sFollowing people(s) are on: %s\n"
				 irchat-info-prefix
				 (matching-substring rest 1))))
    (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 303 reply")))


(defun irchat-handle-305-msg (prefix rest)
  "Handle the 305 UNAWAY"
  (if (string-equal irchat-away-indicator "A")
      (progn
	(setq irchat-away-indicator "-")
	(irchat-maybe-poll)
	(if (string-match "[^:]:\\(.*\\)" rest)
	    (let ((msg (matching-substring rest 1)))
	      (irchat-w-insert irchat-300-buffer 
			       (format "*** %s (%s)\n" 
				       msg 
				       (if irchat-format-time-function
					   (apply irchat-format-time-function
						  (list (current-time-string)))
					 (current-time-string)))))
	  (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 305 reply\n")))))


(defun irchat-handle-306-msg (prefix rest)
  "Handle the 306 NOWAWAY"
  (setq irchat-away-indicator "A")
  (if (string-match "[^:]:\\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-300-buffer 
			 (format "*** %s (%s)\n" 
				 msg (if irchat-format-time-function
					   (apply irchat-format-time-function
						  (list (current-time-string)))
					 (current-time-string)))))
    (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 306 reply\n")))


(defun irchat-handle-311-msg (prefix rest)
  "Handle the 311 WHOISUSER."
  (if (string-match "[^ ]+ \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) :\\(.*\\)" rest)
      (let ((nick (matching-substring rest 1))
	    (username (matching-substring rest 2))
	    (machine (matching-substring rest 3))
	    (channel (matching-substring rest 4))
	    (realname (matching-substring rest 5)))
	(if (not irchat-recursing-whois)
	 (irchat-w-insert irchat-300-buffer 
			  (format "%s%s is %s (%s) at %s\n"
				  irchat-info-prefix
				  nick username realname machine))))
    (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 311 reply")))


(defun irchat-handle-312-msg (prefix rest)
  "Handle the 312 WHOISSERVER."
  (if (string-match "^[^ ]+ \\(\\([^ ]+\\) \\)?\\([^ ]+\\) :\\(.*\\)" rest)
      (let ((who (matching-substring rest 2))
	    (server (matching-substring rest 3))
	    (real (matching-substring rest 4)))
	(if (and (not (irchat-dcc-compare-hostnames server irchat-server))
		 (not irchat-recursing-whois)
		 (not irchat-recursing-whowas))
	    (progn
	      (setq irchat-recursing-whois t)
	      (irchat-send "WHOIS %s %s" server who))
	  (progn
	    (setq irchat-recursing-whois nil)
	    (irchat-w-insert irchat-300-buffer 
			     (format "%son via server %s (%s)\n"
				     irchat-info-prefix server real)))))
    (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 312 reply")))


(defun irchat-handle-313-msg (prefix rest)
  "Handle the 313 WHOISOPERATOR."
  (if (string-match "^[^ ]+ \\([^ ]+\\) :\\(.*\\)" rest)
      (let ((who (matching-substring rest 1))
	    (iswhat (matching-substring rest 2)))
	(if (not irchat-recursing-whois)
	     (irchat-w-insert irchat-300-buffer 
			      (format "%sSTATUS: %s\n"
				      irchat-info-prefix iswhat))))
    (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 313 reply")))


(defun irchat-handle-316-msg (prefix rest)
  "Handle the 316 WHOISCHANOP."
  (if (string-match "^\\([^ ]+\\) :\\(.*\\)" rest)
      (let ((who (matching-substring rest 1))
	    (iswhat (matching-substring rest 2)))
	(if (not irchat-recursing-whois)
	     (irchat-w-insert irchat-300-buffer 
			      (format "STATUS: %s\n" iswhat))))
    (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) :\\(.*\\)" rest)
	(let ((who (matching-substring rest 2))
	      (iswhat (matching-substring rest 3)))
	  (if (not irchat-recursing-whois)
	       (irchat-w-insert irchat-300-buffer 
				(format "%sSTATUS: %s\n"
					irchat-info-prefix iswhat))))
      (irchat-w-insert irchat-300-buffer "IRCHAT: Strange 316 reply"))))


(defun irchat-handle-319-msg (prefix rest)
  "Handle the 319 reply (what channels user is on)."
  (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) :\\(.*\\)" rest)
      (let ((who (matching-substring rest 2))
	    (isonchannels (matching-substring rest 3)))
	(if (not irchat-recursing-whois)
	    (irchat-w-insert irchat-300-buffer
			     (format "%schannels: %s\n"
				     irchat-info-prefix isonchannels))))))


(defun irchat-handle-314-msg (prefix rest)
  "Handle the 314 WHOWASUSER."
  (if (string-match "[^ ]+ \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) :\\(.*\\)" rest)
      (let ((nick (matching-substring rest 1))
	    (username (matching-substring rest 2))
	    (machine (matching-substring rest 3))
	    (channel (matching-substring rest 4))
	    (realname (matching-substring rest 5)))
	(message "")
	(setq irchat-recursing-whowas t)
	(irchat-w-insert irchat-300-buffer (format "%s%s [%s] was %s (%s) at %s\n"
			irchat-info-prefix
			nick
			(if (string= channel "*") "Priv" channel)
			username
			realname
			machine)))
    (message "IRCHAT: Strange 314 reply"))
  )

(defun irchat-handle-315-msg (prefix rest)
  "Handle the 315 ENDOFWHO."
  nil)


(defun irchat-handle-317-msg (prefix rest)
  "Handle the 317 WHOISIDLE."
  (if (string-match "^[^ ]+ [^ ]+ \\([0-9]*\\) :\\(.*\\)" rest)
      (irchat-w-insert irchat-300-buffer (format "%sIDLE for %s\n" irchat-info-prefix 
		(irchat-convert-seconds (matching-substring rest 1))))
    (if (string-match "^[^ ]+ \\([0-9]*\\) :\\(.*\\)" rest)
	(irchat-w-insert irchat-300-buffer (format "%sIDLE for %s\n" irchat-info-prefix
		(irchat-convert-seconds (matching-substring rest 1))))
      (if (string-match ; maybe older 2.6 or 2.5 server?
	   "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) +\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) :\\(.*\\)"
	   rest)
	  (let ((dofw (matching-substring rest 2))
		(month (matching-substring rest 3))
		(date (matching-substring rest 4))
		(time (matching-substring rest 5))
		(year (matching-substring rest 6)))
	    (irchat-w-insert irchat-300-buffer (format "%sLast input received %s.\n" irchat-info-prefix time))
	    )
	(message "IRCHAT: Strange 317 reply")))))


(defun irchat-handle-318-msg (prefix rest)
  "Handle the 318 ENDOFWHOIS."
  nil)


(defun irchat-handle-321-msg (prefix rest)
  "Handle the 321 LISTSTART. (first of names)"
  (irchat-w-insert irchat-300-buffer
   (format "%-10s%6s  %s\n"
	   "Channel" "Users" "Topic")))


(defun irchat-handle-322-msg (prefix rest)
  "Handle the 322 LIST.(from NAMES)."
  (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) :\\(.*\\)" rest)
      (let ((chnl (matching-substring rest 2))
	    (users (matching-substring rest 3))
	    (topic (matching-substring rest 4)))
	(if (or (string= irchat-channel-filter (downcase chnl))
		(string= irchat-channel-filter "")
		(and (string= irchat-channel-filter "0")
		     (string= chnl "*")))
	    (progn
	      (put (intern chnl irchat-obarray) 'topic topic)
	      (irchat-w-insert (irchat-pick-buffer chnl)
			       (format "%-10s%6s  %s\n"
				       (if (string= chnl "*") "Priv"
					 chnl)
				       users
				       topic)))))
    (message "IRCHAT: Strange 322 reply")))


(defun irchat-handle-323-msg (prefix rest) 
  "Handle the 323 RPL_LISTEND. (end of names)"
  nil)


(defun irchat-handle-324-msg (prefix rest)
  "Handle the 324 CHANNELMODEIS."
  (if (string-match "[^ ]* \\([^ ]*\\) \\(.*\\)" rest)
      (let ((chnl (matching-substring rest 1))
	    (str (matching-substring rest 2)))
	(irchat-w-insert (irchat-pick-buffer chnl)
			 (format "*** Mode for %s is %s\n" chnl str)))
    (message (format "IRCHAT: Strange 324 reply '%s'" rest))))


(defun irchat-handle-331-msg (prefix rest)
  "Handle the 331 NOTOPIC"
  (if (string-match "[^ ]* \\([^ ]*\\) \\(.*\\)" rest)
      (let ((ichan (intern (matching-substring rest 1) irchat-obarray)))
	(put ichan 'topic nil)
 	(irchat-w-insert irchat-300-buffer "*** IRCHAT: No topic is set\n"))))


(defun irchat-handle-332-msg (prefix rest)
  "Handle the 332 TOPIC."
  (if (string-match "[^ ]* \\([^ ]*\\) +:\\(.*\\)" rest)
      (let ((ichan (intern (matching-substring rest 1) irchat-obarray))
 	    (topic (matching-substring rest 2)))
 	(put ichan 'topic topic)
 	(irchat-w-insert irchat-300-buffer (format "*** Topic: %s\n" topic)))
    (message "IRCHAT: Strange 332 message")))


(defun irchat-handle-341-msg (prefix rest)
  "Handle the 341 INVITING."
  (if (string-match "^\\([^ ]+\\) +\\([^ ]+\\) +\\([-#&0-9+][^ ]*\\)" rest)
      (let ((who (matching-substring rest 1))
	    (nick (matching-substring rest 2))
	    (chnl (matching-substring rest 3)))
	(irchat-w-insert (irchat-pick-buffer chnl)
			 (format "*** Inviting user %s to channel %s\n"
				 nick chnl)))
    (message "Strange 341 message")))


(defun irchat-handle-351-msg (prefix rest)
  "Handle the 351 VERSION."
  (if (string-match "[^ ]+ \\([^ ]+\\) :*\\([^ ]+\\)[ :]*\\(.*\\)" rest)
      (let ((version (matching-substring rest 1))
	    (machine (matching-substring rest 2))
	    (comments (matching-substring rest 3)))
	(irchat-w-insert irchat-300-buffer (format "*** Machine %s is running IRC version %s (%s)\n"
			machine version comments)))
    (message "IRCHAT: Strange 351 reply")))


(defun irchat-handle-whoreply-msg (prefix rest)
  (irchat-handle-352-msg prefix rest))


(defun irchat-handle-352-msg (prefix rest)
  "Handle the 352 WHOREPLY."
  (if (string-match "\\([^ ]*\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) :[0-9]* ?\\(.*\\)" rest)
      (let* ((chnl (matching-substring rest 1))
	     (user (matching-substring rest 2))
	     (host (matching-substring rest 3))
	     (nick (matching-substring rest 5))
	     (oper (matching-substring rest 6))
	     (name (matching-substring rest 7))
	     (chan-buffer (irchat-pick-buffer chnl)))

	(irchat-w-insert chan-buffer
			 (format "%3s %10s %9s %-29s%s\n"
				 oper ;; Kaizzu 06/03/90
				 (if (string= chnl "*") "Priv" ; *WORK* needed
				   (if (string= chnl "0")
				       "Priv"
				     chnl))
				 nick
				 (format "<%s@%s>" user
					 (irchat-clean-hostname host))
				 name)))
    (message "IRCHAT: Strange 352 message")))


(defun irchat-handle-namreply-msg (prefix rest)
  (irchat-handle-353-msg prefix rest))

(defun irchat-count-words-from-string (str)
  "count words from a string"
  (let* ((len (- (length str) 1)) ; string is from [0,len[
	 (i len) (seen nil) (words (if (< len 0) 0 1)))
    (while (not (< i 0))
      (progn
	(if (string= (char-to-string (aref str i)) " ")
	    (if seen
		(setq words (+ words 1)))
	  (setq seen t))
	(setq i (- i 1))))
    words))

(defvar irchat-353-nameschnl nil "")
(defvar irchat-353-nameslist "" "names list reply string")
(defvar irchat-353-namescount 0 "")

(defun irchat-handle-353-msg (prefix rest)
  "Handle the 353 (NAMREPLY) message.   If we are just polling the server,
don't display anything."
  (if (string-match "[^ =*@]?[=*@] \\([^ ]*\\) :\\(.*\\)" rest)
      (let* ((chnl (matching-substring rest 1))
	     (users (matching-substring rest 2))
	     (numusers (irchat-count-words-from-string users)))
	(if (> irchat-polling 0)
	    nil
         (progn
           (setq irchat-353-nameslist (concat irchat-353-nameslist users)
                 irchat-353-nameschnl chnl
                 irchat-353-namescount (+ irchat-353-namescount numusers))))
	(irchat-scan-channels chnl)
	(irchat-update-thischannel chnl users))
    (message "IRCHAT: Strange 353 message")))

(defun irchat-handle-361-msg (prefix rest)
  "Handle the 361 KILLDONE."
  (if (string-match "[^ ]+ \\([^ ]+\\) +:\\(.*\\)" rest)
      (let ((who (matching-substring rest 1))
	    (message (matching-substring rest 2)))
	(irchat-w-insert irchat-300-buffer 
	 (format "You just KILLED %s. %s\n"
		 who
		 message)))
    (message "IRCHAT: Strange 361 reply")))


(defun irchat-handle-364-msg (prefix rest)
  (if (string-match "^\\([^ ]+\\) +\\([^ ]*\\) +[^ ]* +:\\(.*\\)" rest)
      (progn
	(irchat-w-insert irchat-300-buffer 
			 (format "%-30s%s\n"
				 (matching-substring rest 2)
				 (matching-substring rest 3))))
    (message "IRCHAT: Strange 364 message")))


(defun irchat-handle-365-msg (prefix rest)
  "Handle the 365 ENDOFLINKS."
  nil)


(defun irchat-handle-366-msg (prefix rest)
  "Handle the 366 ENDOFNAMES."
  (let ((level (- irchat-polling 1)))
    (setq irchat-polling (if (< level 0) 0 level))
    (irchat-w-insert (irchat-pick-buffer irchat-353-nameschnl)
		     (format "%9s: (%d user%s): %s\n" 
			     (if (string= irchat-353-nameschnl "*") 
				 "Priv" 
			       irchat-353-nameschnl)
			     irchat-353-namescount
			     (cond 
			      ((= irchat-353-namescount 0) "s")
			      ((= irchat-353-namescount 1) "")
			      (t "s"))
			     irchat-353-nameslist))
    (setq irchat-353-nameslist nil
	  irchat-353-nameschnl nil
	  irchat-353-namescount 0)))

(defun irchat-handle-367-msg (prefix rest)
  "Handle the 367 BAN."
  (if (string-match "[^ ]* \\([^ ]*\\) \\([^ ]*\\)" rest)
      (let ((chnl (matching-substring rest 1))
	    (regexp (matching-substring rest 2)))
	(irchat-w-insert (irchat-pick-buffer chnl)
			 (format "*** %s has been banned on %s\n"
				 regexp chnl)))
    (message "IRCHAT: Strange 367 message")))


(defun irchat-handle-368-msg (prefix rest)
  "Handle the 368 ???????."
  nil)


(defun irchat-handle-369-msg (prefix rest)
  "Handle the 369 WHOWAS."
  (setq irchat-recursing-whowas nil)
  nil)

(defun irchat-handle-371-msg (prefix rest) 
  "Handle the 371 INFO."
  (if (string-match "^\\([^ ]+\\) +:?\\(.*\\)" rest)
      (let ((msg (matching-substring rest 2)))
	(irchat-w-insert irchat-300-buffer (format "*** %s\n" msg)))
    (message "IRCHAT: Strange 371 message")))


(defun irchat-handle-372-msg (prefix rest)
  "Handle the 372 MOTD."
  (string-match "^\\([^ ]+\\) +:?\\(.*\\)" rest)
  (let ((msg (matching-substring rest 2)))
    (irchat-w-insert irchat-300-buffer (format "*** %s\n" msg))
    ))


(defun irchat-handle-381-msg (prefix rest)
  "Handle the 381 YOUREOPER. ."
  (if (string-match "^\\([^ ]+\\) +:\\(.*\\)" rest)
      (let ((message (matching-substring rest 2)))
	(irchat-w-insert irchat-300-buffer (format "OPER: %s\n" message)))
    (message "IRCHAT: Strange 381 reply")))


(defun irchat-handle-382-msg (prefix rest) 
  "Handle the 382 REHASHING."
  (string-match "^\\([^ ]+\\) +:\\(.*\\)" rest)
  (let ((name (matching-substring rest 1))
	(msg (matching-substring rest 2)))
    (irchat-w-insert irchat-300-buffer (format "*** %s: %s\n" name msg))))


(defun irchat-handle-391-msg (prefix rest)
  "Handle the 391 TIME."
  (if (string-match "^\\([^ ]+\\) +\\(.*\\)" rest)
      (let ((time (matching-substring rest 2)))
	(irchat-w-insert irchat-300-buffer 
			 (format "*** Server time: %s\n" time)))
    (message "IRCHAT: Strange 391 message")))

;;;
;;; eof
;;;
