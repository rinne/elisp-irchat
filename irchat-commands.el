;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-commands.el,v 1.6 1997/02/15 17:12:47 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile 
  (require 'irchat-globals)
  (require 'irchat-vars)
  (require 'irchat-inlines)
  (require 'irchat-dcc)
  (require 'irchat-caesar))


(defun irchat-Command-describe-briefly ()
  (message (substitute-command-keys "Type \\[describe-mode] for help")))


(defun irchat-Command-redisplay (&optional center)
  "Un-freezes and re-selects the Dialogue buffer in another window.
   With argument, recenter with that argument."
  (interactive "P")
  (if (irchat-frozen (car irchat-D-buffer))
      (irchat-freeze-toggle (car irchat-D-buffer)))
  (if (irchat-ownfrozen (car irchat-D-buffer))
      (irchat-ownfreeze-toggle (car irchat-D-buffer)))
  (setq irchat-freeze-indicator "-")
  (setq irchat-ownfreeze nil)
  (setq irchat-ownfreeze-indicator "-")
  (if (irchat-get-buffer-window irchat-Dialogue-buffer)
      nil
    (if (one-window-p)
	(irchat-configure-windows)
      (display-buffer irchat-Dialogue-buffer)))
  (set-buffer irchat-Dialogue-buffer)
  (set-window-point (irchat-get-buffer-window irchat-Dialogue-buffer) 
		    (point-max)))


(defun irchat-Command-pollnames ()
  (setq irchat-polling (+ irchat-polling (length irchat-channel-alist)))
  (mapcar (function (lambda (channel)
		      (irchat-send "NAMES %s" (car channel))))
	  irchat-channel-alist))


(defun irchat-Command-find-timestamp ()
  (interactive)
  (save-excursion
    (let ((obuffer (current-buffer))
          (range ""))
      (switch-to-buffer irchat-Dialogue-buffer)
      (if (re-search-backward "^\*\*\* Time: " (point-min) t)
          (let ((start (+ (point) 10)))
            (end-of-line)
            (setq range (format "%s   ---   "
                                (buffer-substring start (point))))))
      (if (re-search-forward "^\*\*\* Time: " (point-max) t)
          (let ((start (point)))
            (end-of-line)
            (setq range (concat range (buffer-substring start (point))))))
      (message range)
      (switch-to-buffer obuffer))))


(defun irchat-Command-keepalive ()
  (if (not (irchat-server-opened))
      (irchat 'always)))


(defun irchat-compose-servertimestring (s)
  (let* ((w '(("Mon" "Monday") ("Tue" "Tuesday") ("Wed" "Wednesday") 
	      ("Thu" "Thursday") ("Fri" "Friday") ("Sat" "Saturday") 
	      ("Sun" "Sunday")))
	 (m '(("Jan" "January") ("Feb" "February") ("Mar" "March") 
	      ("Apr" "April") ("May" "May") ("Jun" "June")  ("Jul" "July") 
	      ("Aug" "August") ("Sep" "September") ("Oct" "October") 
	      ("Nov" "November") ("Dec" "December")))
	 (wdat) (mnam)
	 (wabb (substring s 0 3))
	 (mabb (substring s 4 7)))
    (setq wdat (car (cdr (assoc wabb w)))
	  mnam (car (cdr (assoc mabb m))))
    (format "%s %s %s %s %s" 
	    wdat mnam (substring s 8 10) (substring s 20 24)
	    (substring s 11 16))))


(defvar irchat-last-timestamp-time nil "Last time timestamp was inserted")
(defvar irchat-last-timestamp-no-cons-p nil "Last timestamp was no-cons")


(defun irchat-Command-timestamp-if-interval-expired (&optional no-cons)
  (interactive)
  (if (and (not (and no-cons
		     irchat-last-timestamp-no-cons-p))
	   (numberp irchat-timestamp-interval)
	   (> irchat-timestamp-interval 0)
	   (or (null irchat-last-timestamp-time)
	       (> (irchat-time-difference irchat-last-timestamp-time
					  (current-time))
		  irchat-timestamp-interval)))
      (progn
	(irchat-Command-timestamp)
	(setq irchat-last-timestamp-no-cons-p no-cons))))


(defun irchat-Command-timestamp ()
  (interactive)
  (let ((stamp (format "%s" (format irchat-timestamp-format
				    (if irchat-format-time-function
					(apply irchat-format-time-function
					       (list (current-time-string)))
				      (current-time-string))))))
    (let ((irchat-timestamp-interval 0))
      (irchat-Dialogue-insert stamp)))
  (setq irchat-last-timestamp-time (current-time)))


(defun irchat-Command-send-message (message)
  "Send MESSAGE to current chat partner of current channel."
  (if (> (length message) 0)
      (progn
	(if (eq irchat-command-buffer-mode 'chat)
	    (if irchat-current-chat-partner
		(progn
		  (irchat-send "PRIVMSG %s :%s" 
			       irchat-current-chat-partner message)
		  (irchat-own-private-message 
		   (format (format "%s %%s"
				   irchat-format-string)
			   irchat-current-chat-partner message)))
	      (message 
	       (substitute-command-keys 
		"Type \\[irchat-Command-join] to start private conversation")))
	  (if (not irchat-current-channel)
	      (progn
		(beep t)
		(message 
		 (substitute-command-keys 
		  "Type \\[irchat-Command-join] to join a channel")))
	    (irchat-send "PRIVMSG %s :%s" irchat-current-channel message)
	    (irchat-own-message
	     (format (format (format "%s %%%%s" irchat-myformat-string) 
			     irchat-nickname) message))))
	t)
    (progn
      (message "IRCHAT: NO text to send")
      nil)))


(defun irchat-Command-enter-message ()
  "Enter the current line as an entry in the IRC dialogue on the
current channel."
  (interactive)
  (let (message start)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq message (buffer-substring start (point)))
    (if (irchat-Command-send-message message)
	(irchat-next-line 1))))



(defun irchat-Dialogue-enter-message ()
  "Ask for a line as an entry in the IRC dialogue on the current channel."
  (interactive)
  (let ((message "x"))
    (while (not (string= message ""))
      (setq message (read-string "> "))
      (if (not (string= message ""))
	  (irchat-Command-send-message message)))))


(defun irchat-Command-debug ()
  "Start debugging irchat."
  (interactive)
  (if irchat-debug-buffer
      (progn
	(setq irchat-debug-buffer nil)
	(other-window 1)
	(delete-window)
	(other-window -1))
    (if irchat-use-full-window
	(delete-other-windows))
    (irchat-configure-windows)
    (split-window-horizontally)
    (other-window 1)
    (setq irchat-debug-buffer (irchat-get-buffer-create "*IRC Debugging*"))
    (switch-to-buffer irchat-debug-buffer)
    (other-window -1)))


(defun irchat-Command-inline ()
  "Send current line as a message to the IRC server."
  (interactive)
  (let (message start stop)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (setq message (buffer-substring start stop))
    (newline)
    (irchat-send message)))


(defun irchat-Command-join (join-channel-var)
  "Join a channel or private conversation.
If user nicname is given, join the same set of channels as 
the specified user. 
If Command-buffer is in chat-mode, start private conversation 
with specified user."
  (interactive (let (join-channel-var (completion-ignore-case t))
		 (setq join-channel-var 
		       (if (eq irchat-command-buffer-mode 'chat)
			   (irchat-completing-default-read
			    "Start private conversation with: "
			    irchat-nick-alist
			    '(lambda (s) t) nil irchat-privmsg-partner)
			 (irchat-completing-default-read
			  "Join channel: "
			  (append irchat-channel-alist
				  irchat-nick-alist)
			  '(lambda (s) t) nil irchat-invited-channel)))
		 (list join-channel-var)))

  (if (eq irchat-command-buffer-mode 'chat)
      (progn
	(setq irchat-current-chat-partners (append 
					    (list join-channel-var)
					    (string-list-ci-delete
					     join-channel-var
					     irchat-current-chat-partners))
	      irchat-current-chat-partner join-channel-var
	      irchat-chat-partner-alist (list-to-assoclist 
					 irchat-current-chat-partners)
	      irchat-channel-indicator (if irchat-current-chat-partner
					   (format "Chatting with %s" 
						   irchat-current-chat-partner)
					 "No partner")))
    (progn
      (let ((nicks irchat-nick-alist)
	    (found nil))
	(while (and (not found)	nicks)
	  (if (and (car (car nicks))
		   (string-ci-equal join-channel-var (car (car nicks))))
	      (setq join-channel-var
		    (or (car (get (intern (car (car nicks)) irchat-obarray) 
 				  'chnl))
			join-channel-var)
		    found t))
	  (setq nicks (cdr nicks))))
      (let ((found nil))
	(mapcar 
	 '(lambda (elem)
	    (if (string-ci-equal join-channel-var elem)
		(setq irchat-current-channel elem
		      found t)))
	 irchat-current-channels)
	(setq irchat-channel-indicator
	      (if irchat-current-channel
		  (format "Channel %s" irchat-current-channel)
		"No channel"))
	(setq irchat-invited-channel nil)
	(or found (irchat-send "JOIN %s" join-channel-var))))))


(defun irchat-Command-part (part-channel-var)
  "Part a channel or private conversation."
  (interactive (let (part-channel-var (completion-ignore-case t))
		 (setq part-channel-var 
		       (if (eq irchat-command-buffer-mode 'chat)
			   (irchat-completing-default-read	
			    "End private conversation with: "
			    irchat-chat-partner-alist
			    '(lambda (s) t) nil 
			    irchat-current-chat-partner)
			 (irchat-completing-default-read	
			  "Part channel: "
			  (list-to-assoclist irchat-current-channels)
			  '(lambda (s) t) nil 
			  irchat-current-channel)))
		 (list part-channel-var)))
  (if (eq irchat-command-buffer-mode 'chat)
      (setq irchat-current-chat-partners (string-list-ci-delete
					  part-channel-var
					  irchat-current-chat-partners)
	    irchat-current-chat-partner (car irchat-current-chat-partners)
	    irchat-chat-partner-alist (list-to-assoclist 
				       irchat-current-chat-partners)
	    irchat-channel-indicator (if irchat-current-chat-partner
					 (format "Chatting with %s" 
						 irchat-current-chat-partner)
				       "No partner"))
    (progn
      (if (string-list-ci-memberp part-channel-var irchat-current-channels)
	  (setq irchat-current-channel part-channel-var)) ; just refocusing
      (irchat-send "PART %s" part-channel-var))))


(defun irchat-Command-kill (kill-nickname-var)
  "Ignore messages from this user. Username can be given as case insensitive
regular expression of form \".*@.*\.sub.domain\". 
If already ignoring him/her, toggle.
If variable irchat-variables-file is defined and the file is writable, its
contents are updated future sessions."
  (interactive (let ((kill-nickname-var nil) 
		     (completion-ignore-case t))
		 (setq kill-nickname-var 
		       (completing-read "Ignore nickname or regexp: " 
			irchat-nick-alist
			'(lambda (s) t) nil nil))
		 (list kill-nickname-var)))
  ;; empty, just list them
  (if (string= "" kill-nickname-var)
      (let ((buf (current-buffer))
	    (buffer-read-only))
	(set-buffer irchat-Dialogue-buffer)
	(goto-char (point-max))
	(irchat-w-insert irchat-D-buffer "*** Currently ignoring:")
	(let ((mylist irchat-kill-nickname))
	  (while mylist
	    (irchat-w-insert irchat-D-buffer (format " %s" (car mylist)))
	    (setq mylist (cdr mylist))))
	(irchat-w-insert irchat-D-buffer "\n")
	(set-buffer buf))
    ;; else not empty, check if exists
    (let ((done nil) (elem irchat-kill-nickname))
      (while (and elem (not done))
	(if (string-ci-equal (car elem) kill-nickname-var)
	    (setq irchat-kill-nickname (delete (car elem) irchat-kill-nickname)
		  done t)
	  (setq elem (cdr elem))))
      ;; did not find, add to ignored ones
      (if (not done)
	  (setq irchat-kill-nickname (cons kill-nickname-var
					   irchat-kill-nickname)))
      (irchat-Command-save-vars))))


(defun irchat-Command-send-action (&optional private)
  "Send action ctcp - if on empty line, ask for the message"
  (interactive
   (if current-prefix-arg
       (list current-prefix-arg)
     nil))
  (let ((completion-ignore-case t) message start stop)
    (if private
	(setq irchat-privmsg-partner
	      (irchat-completing-default-read 
	       "To whom: "
	       (append irchat-nick-alist irchat-channel-alist)
	       '(lambda (s) t) 
	       nil irchat-privmsg-partner)))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (if (eq start stop)
	(setq message (read-string "Action: "))
      (setq message (buffer-substring start stop))
      (irchat-next-line 1))
    (irchat-send "PRIVMSG %s :ACTION %s"
		 (if private
		     irchat-privmsg-partner
		   irchat-current-channel)
		 message)
    (if private
	(irchat-own-private-message (format "*** Action to %s: %s %s"
				    irchat-privmsg-partner irchat-nickname
				    message))
      (irchat-own-message (format "*** Action: %s %s" irchat-nickname
				  message)))))


(defun irchat-Command-kick (kick-nickname-var)
  "Kick this user out."
  (interactive (let (kick-nickname-var (completion-ignore-case t))
		 (setq kick-nickname-var 
		       (completing-read 
			"Kick out nickname: " 
			irchat-nick-alist
			'(lambda (s) t) nil nil))
		 (list kick-nickname-var)))
  (irchat-send "KICK %s %s" irchat-current-channel kick-nickname-var))


(defun irchat-Command-list (&optional channel)
  "List the given channel and its topics.
If you enter only Control-U as argument, list the current channel.
With - as argument, list all channels."
  (interactive
   (if current-prefix-arg
       (if (eq current-prefix-arg '-)
	   (list current-prefix-arg)
	 nil)
     (list
      (let ((completion-ignore-case t))
	(completing-read 
	 "LIST channel: " 
	 irchat-channel-alist
	 '(lambda (s) t) nil nil)))))
  (if (not channel)
      (irchat-send "LIST %s" irchat-current-channel)
    (if (eq channel '-)
	(irchat-send "LIST")
      (irchat-send "LIST %s" channel))))


(defun irchat-Command-lusers ()
  "List the number of users and servers"
  (interactive)
  (irchat-send "LUSERS"))


(defun irchat-Command-modec (change)
  "Send a MODE command"
  (interactive "sMode for this channel: ")
  (irchat-send "MODE %s %s" irchat-current-channel change))


(defun irchat-Command-message (message-nick-var message)
  "Send a private message to another user."
  (interactive (let (message-nick-var (completion-ignore-case t))
		 (setq message-nick-var 
		       (irchat-completing-default-read 
			"Private message to: "
			(append irchat-nick-alist irchat-channel-alist)
			'(lambda (s) t) nil irchat-privmsg-partner))
		 (list message-nick-var 
		       (read-string 
			(format "Private message to %s: " message-nick-var)))))
  (setq irchat-privmsg-partner message-nick-var)
  (irchat-send "PRIVMSG %s :%s" message-nick-var message)
  (irchat-own-private-message 
   (format (format "%s %%s" irchat-format-string) message-nick-var message)))


;; Added at mta@tut.fi's request...

(defun irchat-Command-mta-private ()
  "Send a private message (current line) to another user."
  (interactive)
  (let ((completion-ignore-case t) message start stop)
    (setq irchat-privmsg-partner
	  (irchat-completing-default-read 
	   "To whom: "
	   (append irchat-nick-alist irchat-channel-alist)
	   '(lambda (s) t) 
	   nil irchat-privmsg-partner))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (setq message (buffer-substring start stop))
    (irchat-next-line 1)
    (if (> (length message) 0)
	(progn
	  (irchat-send "PRIVMSG %s :%s" irchat-privmsg-partner message)
	  (irchat-own-private-message 
	   (format (format "%s %%s" irchat-format-string) 
		   irchat-privmsg-partner message)))
      (message "IRCHAT: No text to send"))))


(defun irchat-Command-names (&optional channel)
  "List the nicknames of the current IRC users on given channel.
With an Control-U as argument, only the current channel is listed.
With - as argument, list all channels."
  (interactive
   (if current-prefix-arg
       (if (eq current-prefix-arg '-)
	   (list current-prefix-arg)
	 nil)
     (list
      (let ((completion-ignore-case t))
	(completing-read 
	 "Names on channel: " 
	 irchat-channel-alist
	 '(lambda (s) t) nil nil)))))
  (if (not channel)
      (irchat-send "NAMES %s" irchat-current-channel)
    (if (eq channel '-)
	(irchat-send "NAMES")
      (irchat-send "NAMES %s" channel))))


(defun irchat-Command-nickname (nick)
  "Set your nickname."
  (interactive "sEnter your nickname: ")
  (let ((nickname (irchat-read-nickname nick)))
    (if (not (= (length nickname) 0))
	(progn 
	  (setq irchat-old-nickname irchat-nickname)
	  (setq irchat-nickname nickname)
	  (irchat-send "NICK %s" nick))
      (message "IRCHAT: illegal nickname \"%s\"; not changed" nickname))))


(defun irchat-Command-who (&optional expr)
  "Lists tue users that match the given expression.
If you enter only Control-U as argument, list the current channel.
With - as argument, list all users."
  (interactive 
   (if current-prefix-arg
       (if (eq current-prefix-arg '-)
	   (list current-prefix-arg)
	 nil)
     (list
      (let ((completion-ignore-case t))
	(completing-read 
	 "WHO expression: " 
	 irchat-channel-alist
	 '(lambda (s) t) nil nil)))))
  (if (not expr)
      (irchat-send "WHO %s" irchat-current-channel)
    (if (eq expr '-)
	(irchat-send "WHO")
      (irchat-send "WHO %s" expr))))


(defun irchat-Command-wait (nick &optional greeting)
  "Wait for NICK to enter IRC.  When this person appears, you will
be informed. If the optional argument GREETING is non-nil, it should 
be a string to send NICK upon entering."
  (interactive 
   (progn (setq nick (read-string "Wait for: ")
		greeting (read-string 
			  (format "Message to send %s upon entering: " nick)))
	  (if (string= greeting "")
	      (setq greeting nil))
	  (list nick greeting)))
  (put (intern nick irchat-obarray) 'irchat-waited-for t)
  (if greeting 
      (put (intern nick irchat-obarray) 'irchat-greeting greeting)))


(defun irchat-Command-finger (finger-nick-var)
  "Get information about a specific user."
  (interactive (let (finger-nick-var (completion-ignore-case t))
		 (setq finger-nick-var 
		       (completing-read 
			"Finger whom: " irchat-nick-alist
			'(lambda (s) t) nil nil))
		 (list finger-nick-var)))
  (irchat-send "WHOIS %s" finger-nick-var))


(defun irchat-Command-topic (topic)
  "Change topic of channel."
  (interactive "sTopic: ")
  (irchat-send "TOPIC %s :%s" irchat-current-channel topic))


(defun irchat-Command-invite (&optional invite-channel-var invite-nick-var)
  "Invite user to channel."
  (interactive 
   (list
    (if current-prefix-arg
	(let ((completion-ignore-case t))
	  (completing-read 
	   "Invite channel: "
	   (mapcar '(lambda (x)
		      (list x))
		   irchat-current-channels)
	   '(lambda (s) t) nil nil)
	  nil))
    (let ((completion-ignore-case t)) 
      (completing-read "Invite whom: " 
		       irchat-nick-alist
		       '(lambda (s) t) nil nil))))
  (if (not invite-channel-var)
      (setq invite-channel-var irchat-current-channel))
  (irchat-send "INVITE %s %s" invite-nick-var invite-channel-var))


(defun irchat-Command-away (awaymsg)
  "Mark/unmark yourself as being away."
  (interactive "sAway message: ")
  (progn
    (irchat-send "AWAY :%s" awaymsg)
    (setq irchat-awaymsg awaymsg)))


(defun irchat-Command-scroll-down ()
  "Scroll Dialogue-buffer down from Command-buffer."
  (interactive)
  (let ((obuffer (current-buffer)) (owindow (selected-window)))
    (select-window (get-buffer-window irchat-Dialogue-buffer t))
    (pop-to-buffer irchat-Dialogue-buffer)
    (if (pos-visible-in-window-p (point-min))
	(message "Beginning of buffer")
      (scroll-down))
    (select-window (get-buffer-window obuffer t))
    (pop-to-buffer obuffer)))


(defun irchat-Command-scroll-up ()
  "Scroll Dialogue-buffer up from Command-buffer."
  (interactive)
  (let ((obuffer (current-buffer)))
    (select-window (get-buffer-window irchat-Dialogue-buffer t))
    (pop-to-buffer irchat-Dialogue-buffer)
    (if (pos-visible-in-window-p (point-max))
	(progn
	  (goto-char (point-max))
	  (recenter 1))
      (scroll-up))
    (select-window (get-buffer-window obuffer t))
    (pop-to-buffer obuffer)))


(defun irchat-Command-freeze ()
  "Toggle the automatic scrolling of the Dialogue window."
  (interactive)
  (if (irchat-frozen (car irchat-D-buffer))
      (setq irchat-freeze-indicator "-")
    (setq irchat-freeze-indicator "F"))
  (switch-to-buffer (current-buffer))
  (irchat-freeze-toggle (car irchat-D-buffer))
  )


(defun irchat-Command-ownfreeze ()
  "Toggle the automatic scrolling of the Dialogue window when user sends messages."
  (interactive)
  (if (irchat-ownfrozen (car irchat-D-buffer))
      (setq irchat-ownfreeze-indicator "-")
    (setq irchat-ownfreeze-indicator "M"))
  (switch-to-buffer (current-buffer))
  (setq irchat-ownfreeze (not irchat-ownfreeze))
  (irchat-ownfreeze-toggle (car irchat-D-buffer))
  )


(defun irchat-Command-quit (&optional quit-msg)
  "Quit IRCHAT."
  (interactive "P")
  (if (or (not (irchat-server-opened))
	  quit-msg
	  (y-or-n-p "Quit IRCHAT? "))
      (progn
	(message "")
	(if (get-buffer-process irchat-server-buffer)
	    (if (and (irchat-server-opened)
		     quit-msg)
		(let ((quit-string (read-string "Signoff message: ")))
		  (irchat-send "QUIT :%s" quit-string))
	      (irchat-send "QUIT :%s" (or irchat-signoff-msg ""))))
	(irchat-clear-system)
;	(if irchat-timestamp-timer 
;	    (irchat-cancel-timer irchat-timestamp-timer))
	(if irchat-names-timer 
	    (irchat-cancel-timer irchat-names-timer))
	(if irchat-use-full-window
	    (delete-other-windows))
	(irchat-close-server)
	(run-hooks 'irchat-Exit-hook)
	(setq irchat-polling 0
	      irchat-current-channel nil
	      irchat-obarray nil
	      irchat-current-channels nil))))


(defun irchat-Command-generic (message)
  "Enter a generic IRC message, which is sent to the server.
 A ? lists the useful generic messages."
  (interactive "sIRC Command: ")
  (if (string= message "?")
      (with-output-to-temp-buffer "*IRC Help*"
	(princ "The following generic IRC messages may be of interest to you:
TOPIC <new topic>		set the topic of your channel
INVITE <nickname>		invite another user to join your channel
LINKS				lists the currently reachable IRC servers
SUMMON <user@host>		invites an user not currently in IRC
USERS <host>			lists the users on a host
AWAY <reason>			marks you as not really actively using IRC
				(an empty reason clears it)
WALL <message>			send to everyone on IRC
NAMES <channel>			lists users per channel
")
	(message 
	 (substitute-command-keys 
	  "Type \\[irchat-Command-redisplay] to continue")))
    (irchat-send "%s" message)))


(defun irchat-Command-irc-compatible ()
  "If entered at column 0, allows you to enter a generic IRC message to
be sent to the server.  For a list of messages, see irchat-Command-generic."
  (interactive)
  (if (eq (current-column) 0)
      (call-interactively (function irchat-Command-generic))
    (self-insert-command 1)))


(defun irchat-Command-exec (command)
  "Execute command, stdout to dialogue."
  (interactive "sShell Command: ")
  (shell-command command t)
  (let ((opoint (point)))
    (while (< (point) (mark))
      (progn
	(irchat-Command-enter-message)
	(set-buffer irchat-Command-buffer)))
    (push-mark opoint t)))

;;;
;;; client-to-client queries
;;;
(defun irchat-Command-client-version (client-version-nick-var)
  "Ask about someones client version."
  (interactive (let (client-version-nick-var (completion-ignore-case t))
		 (setq client-version-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-version-nick-var)))
  (setq irchat-query-client-nick client-version-nick-var)
  (irchat-send "PRIVMSG %s :%s" 
	       client-version-nick-var irchat-query-client-version))


(defun irchat-Command-client-userinfo (client-userinfo-nick-var)
  "Ask about someones client userinfo."
  (interactive (let (client-userinfo-nick-var (completion-ignore-case t))
		 (setq client-userinfo-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-userinfo-nick-var)))
  (setq irchat-query-client-nick client-userinfo-nick-var)
  (irchat-send "PRIVMSG %s :%s"
	       client-userinfo-nick-var irchat-query-client-userinfo))


(defun irchat-Command-client-help (client-help-nick-var)
  "Ask about someones client help."
  (interactive (let (client-help-nick-var (completion-ignore-case t))
		 (setq client-help-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-help-nick-var)))
  (setq irchat-query-client-nick client-help-nick-var)
  (irchat-send "PRIVMSG %s :%s" 
	       client-help-nick-var irchat-query-client-help))


(defun irchat-Command-client-clientinfo (client-clientinfo-nick-var)
  "Ask about someones client clientinfo."
  (interactive (let (client-clientinfo-nick-var (completion-ignore-case t))
		 (setq client-clientinfo-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-clientinfo-nick-var)))
  (setq irchat-query-client-nick client-clientinfo-nick-var)
  (irchat-send "PRIVMSG %s :%s" 
	       client-clientinfo-nick-var irchat-query-client-clientinfo))


(defun irchat-Command-client-x-face (client-x-face-nick-var)
  "Ask about someones client x-face."
  (interactive (let (client-x-face-nick-var (completion-ignore-case t))
		 (setq client-x-face-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-x-face-nick-var)))
  (setq irchat-query-client-nick client-x-face-nick-var)
  (irchat-send "PRIVMSG %s :%s"
	       client-x-face-nick-var irchat-query-client-x-face))


(defun irchat-Command-client-generic (client-generic-nick-var)
  "Ask about someones client clientinfo."
  (interactive (let (client-generic-nick-var (completion-ignore-case t))
		 (setq client-generic-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-generic-nick-var)))
  (setq irchat-query-client-nick client-generic-nick-var
	irchat-query-client-lastcommand
	(irchat-completing-default-read 
	 "What info: " 
	 irchat-query-client-alist '(lambda (s) t) nil 
	 irchat-query-client-lastcommand))
  (if (string-ci-equal irchat-query-client-lastcommand "ping")
      (setq irchat-ctcp-ping-time (current-time)))
  (irchat-send "PRIVMSG %s :%s%s"
	       client-generic-nick-var
	       irchat-query-client-lastcommand
	       irchat-query-client-insert-to-generic))


(defun irchat-Command-client-userinfo-from-minibuffer ()
  "Ask about someones client clientinfo."
  (interactive)
  (setq irchat-client-userinfo
	(read-from-minibuffer "New userinfo: "
			      irchat-client-userinfo)))


(defun irchat-Command-client-userinfo-from-commandbuffer ()
  "Ask about someones client clientinfo."
  (interactive)
  (let (start stop)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (setq irchat-client-userinfo (buffer-substring start stop))
    (irchat-next-line 1)))


(defun irchat-Command-client-x-face-from-minibuffer ()
  "Ask about someones client clientinfo."
  (interactive)
  (setq irchat-client-x-face
	(read-from-minibuffer "New X-Face: "
			      irchat-client-x-face)))


(defun irchat-Command-client-x-face-from-commandbuffer ()
  "Ask about someones client clientinfo."
  (interactive)
  (let (start stop)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq stop (point))
    (setq irchat-client-x-face (buffer-substring start stop))
    (irchat-next-line 1)))


(defun irchat-Command-client-ping (client-help-nick-var)
  "Ask about someones client help."
  (interactive (let (client-help-nick-var (completion-ignore-case t))
		 (setq client-help-nick-var 
		       (irchat-completing-default-read 
			"Whose client: " irchat-nick-alist
			'(lambda (s) t) nil irchat-query-client-nick))
		 (list client-help-nick-var)))
  (setq irchat-query-client-nick client-help-nick-var)
  (setq irchat-ctcp-ping-time (current-time))
  (irchat-send "PRIVMSG %s :%s" 
	       client-help-nick-var irchat-query-client-ping))

;;;
;;; sending files (any files actually)
;;;
(defun irchat-Command-send-file (name to-who)
  "Send a file to given  user"
  (interactive "fFile name: \nsTo who: ")
  (save-excursion 
    (set-buffer (get-buffer-create (format "*IRC S_FILE_%s*" name)))
    (delete-region (point-min) (point-max))
    (insert-file name)
    (let (str)
      (setq str (buffer-string))
      (delete-region (point-min) (point-max))
      (insert (irchat-quote-encode str)))
    (goto-char (point-min))
    (irchat-send "NOTICE %s :FILE START %s :%s" to-who name
		 (buffer-substring (point)
				   (min (point-max)
					(+ 80 (point)))))
    (goto-char (min (point-max) (+ 80 (point))))
    (while (< (point) (point-max))
      (progn
	(if (=  1 (mod (point) 800))
	    (sleep-for 1))
	(irchat-send "NOTICE %s :FILE CONT %s :%s" to-who name
		     (buffer-substring (point)
				       (min (point-max) (+ 80 (point)))))
	(goto-char (min (point-max) (+ 80 (point))))))
    (irchat-send "NOTICE %s :FILE END %s : " to-who name)
    (kill-buffer (get-buffer-create (format "*IRC S_FILE_%s*" name)))))


;;;
;;; send text in kill-buffer
;;;
(defun irchat-Command-yank-send (&optional howmany)
  (interactive)
  (let ((beg (point)) end)
    (insert (car kill-ring-yank-pointer))
    (setq end (point))
    (goto-char beg)
    (while (< (point) end)
      (progn
	(irchat-Command-enter-message)
	(set-buffer irchat-Command-buffer)))))

;;;
;;; send rot-13 encrypted data
;;;
(defun irchat-Command-caesar-line (&optional n)
  "*Caesar encrypt current line. Rotate optional N characters."
  (interactive)
  (beginning-of-line nil)
  (push-mark (point))
  (end-of-line)
  (irchat-caesar-region n))

;;;
;;;
;;;
(defun get-word-left ()
  "Return word left from point."
  (save-excursion
    (let (point-now)
      (setq point-now (point))
      (backward-word 1)
      (buffer-substring (point) point-now))))


(defun irchat-Command-complete ()
  "Complete word before point from userlist."
  (interactive)
  (insert
   (save-excursion
     (let ((completion-ignore-case t) point-now word result)
       (setq point-now (point)
	     word (get-word-left)
	     result (try-completion word irchat-nick-alist))
       (backward-word 1)
       (delete-region (point) point-now)
       (if (or (eq result t) (eq result nil))
           word
         result)))))


(defun irchat-Command-load-vars ()
  "Load configuration from irchat-variables-file."
  (interactive)
  (let ((file (expand-file-name irchat-variables-file)))
    (if (file-exists-p file)
	(let ((nick irchat-nickname))
	  (load-file file)
	  (setq irchat-nickname nick)
	  (irchat-Command-reconfigure-windows)))))

(defun irchat-Command-save-vars ()
  (interactive)
  (let ((output-buffer (find-file-noselect
                        (expand-file-name irchat-variables-file)))
	output-marker)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (if (re-search-forward "^;; Saved Settings *\n" nil 'move)
          (let ((p (match-beginning 0)))
            (goto-char p)
            (or (re-search-forward "^;; End of Saved Settings *\\(\n\\|\\'\\)"
				   nil t)
                (error (format 
			"can't find END of saved state in %s" 
			irchat-variables-file)))
	    (delete-region p (match-end 0)))
        (goto-char (point-max))
        (insert "\n"))
      (setq output-marker (point-marker))
      (let ((print-readably t) 
	    (print-escape-newlines t)
            (standard-output output-marker))
        (princ ";; Saved Settings\n")
        (mapcar (function 
		 (lambda (var)
		   (if (symbolp var)
		       (prin1 (list 'setq var
				    (let ((val (symbol-value var)))
				      (if (memq val '(t nil))
					  val
					(list 'quote val)))))
		     (setq var (eval var))
		     (cond ((eq (car-safe var) 'progn)
			    (while (setq var (cdr var))
			      (prin1 (car var))
			      (princ "\n")
			      (if (cdr var) (princ "  "))))
			   (var
			    (prin1 "xx")(prin1 var))))
		   (if var (princ "\n"))))
                irchat-saved-forms)
        (princ "\n")
        (princ ";; End of Saved Settings\n")))
    (set-marker output-marker nil)
    (save-excursion
      (set-buffer output-buffer)
      (save-buffer))))

(defun irchat-Command-reconfigure-windows ()
  (interactive)
  (let ((command-window (irchat-get-buffer-window irchat-Command-buffer))
	(dialogue-window (irchat-get-buffer-window irchat-Dialogue-buffer))
	(old-buffer (current-buffer)))
    (if (and command-window dialogue-window)
	(let ((c-height (window-height command-window))
	      (d-height (window-height dialogue-window)))
	  (delete-window command-window)
	  (pop-to-buffer irchat-Dialogue-buffer)
	  (enlarge-window (+ c-height d-height
			     (- (window-height dialogue-window)))))
      (pop-to-buffer irchat-Dialogue-buffer))
    (irchat-configure-windows)
    (if irchat-one-buffer-mode
	(pop-to-buffer irchat-Dialogue-buffer)
      (pop-to-buffer old-buffer))))

;;;
;;; command to get end of the Dialogue buffer
;;;
(defun irchat-Command-eod-buffer ()
  (interactive)
  (let ((saved-buffer (current-buffer))
	(dialogue-window (irchat-get-buffer-window irchat-Dialogue-buffer)))
    (set-buffer irchat-Dialogue-buffer)
    (goto-char (point-max))
    (set-window-point dialogue-window (point-max))
    (set-buffer saved-buffer)))

;;;
;;; commands to manage channel/conversation rings
;;;
(defun irchat-Command-private-conversation (arg)
  "Toggle between private conversation mode and channel mode. 
User can then join and part to a private conversation as he would 
join or part to a channel. 

If there are no private conversations or argument is given user is 
prompted the partner/channel (return as partner/channel means toggle 
mode, the current channel and current chat partner are not altered)"

  (interactive
   (list (if current-prefix-arg
       ;; prefixed, ask where to continue
       (if (eq irchat-command-buffer-mode 'chat)
	   (irchat-completing-default-read "Return to channel: "
					   (append irchat-channel-alist 
						   irchat-nick-alist) 
					   '(lambda (s) t) nil 
					   irchat-current-channel)
	 (completing-read "Start private conversation with: "
			  irchat-nick-alist '(lambda (s) t) nil))
     ;; no prefix, see if going to chat
     (if (eq irchat-command-buffer-mode 'channel)
	 ;; and if we have chat partner, select that
	 (if irchat-current-chat-partner
	     irchat-current-chat-partner
	   (completing-read "Start private conversation with: "
			    irchat-nick-alist '(lambda (s) t) nil)
	   )))))

  (progn
    (setq irchat-command-buffer-mode (if (eq irchat-command-buffer-mode 'chat)
					 'channel
				       'chat))
    (if arg
	(irchat-Command-join arg))
    (setq irchat-channel-indicator (if (eq irchat-command-buffer-mode 'chat)
				       (if irchat-current-chat-partner
					   (format "Chatting with %s" 
						   irchat-current-chat-partner)
					 "No partner")
				     (if irchat-current-channel
					 (format "Channel %s" 
						 irchat-current-channel)
				       "No channel")))
    ;; refresh mode line
    (set-buffer-modified-p (buffer-modified-p))))


(defun irchat-Command-push ()
  "Select next channel or chat partner."
  (interactive)
  (if (eq irchat-command-buffer-mode 'chat)
    (if irchat-current-chat-partners
	(let ((chat-partner (nth (1- (length irchat-current-chat-partners))
				 irchat-current-chat-partners)))
	  (if (string-list-ci-memberp chat-partner irchat-current-chat-partners)
	      (progn
		(setq irchat-current-chat-partner chat-partner
		      irchat-current-chat-partners
		      (cons chat-partner
			    (string-list-ci-delete 
			     chat-partner
			     irchat-current-chat-partners)))
		(setq irchat-channel-indicator
		      (if irchat-current-chat-partner
			  (format "Chatting with %s" 
				  irchat-current-chat-partner)
			"No partner"))))
	  (set-buffer-modified-p (buffer-modified-p))))
    (if irchat-current-channels
	(let ((channel (nth (1- (length irchat-current-channels))
			    irchat-current-channels)))
	  (if (string-list-ci-memberp channel irchat-current-channels)
	      (progn
		(setq irchat-current-channel channel
		      irchat-current-channels
		      (cons channel
			    (string-list-ci-delete channel 
						   irchat-current-channels)))
		(setq irchat-channel-indicator
		      (if irchat-current-channel
			  (format "Channel %s" irchat-current-channel)
			"No channel"))))
	  (set-buffer-modified-p (buffer-modified-p))))))


(defun irchat-Command-pop ()
  "Select previous channel or chat partner."
  (interactive)
  (if (eq irchat-command-buffer-mode 'chat)
      (let ((chat-partner irchat-current-chat-partner))
	(progn
	  (setq irchat-current-chat-partners (append
					      (string-list-ci-delete 
					       chat-partner 
					       irchat-current-chat-partners)
					      (list chat-partner))
		irchat-current-chat-partner (car irchat-current-chat-partners)
		irchat-channel-indicator 
		(if irchat-current-chat-partner
		    (format "Chatting with %s" 
			    irchat-current-chat-partner)
		  "No partner"))
	  (set-buffer-modified-p (buffer-modified-p))))

    (let ((channel irchat-current-channel))
      (progn
	(setq irchat-current-channels (append 
				       (string-list-ci-delete 
					channel 
					irchat-current-channels)
				       (list channel))
	      irchat-current-channel (car irchat-current-channels)
	      irchat-channel-indicator (format "Channel %s" 
					       irchat-current-channel))
	(set-buffer-modified-p (buffer-modified-p))))))

;;;
;;; Cannot use completing read, user may want to query many names
;;;
(defun irchat-Command-ison (nick)
  "IsON user."
  (interactive "sIsON: ")
  (irchat-send "ISON %s" nick))


(defun irchat-Command-userhost (nick)
  "Ask for userhost."
  (interactive "sUserhost nick(s): ")
  (irchat-send "USERHOST %s" nick))


;;;
;;; dig last kill from KILLS and show it
;;;

(defun irchat-Command-show-last-kill ()
  (interactive)
  (save-excursion
    (let ((buf (current-buffer))
	  (buffer-read-only)
	  str)
      (set-buffer irchat-KILLS-buffer)
      (goto-char (point-max))
      (forward-line -1)
      (setq str (buffer-substring (point) (point-max)))
      (goto-char (point-max))
      (set-buffer irchat-Dialogue-buffer)
      (goto-char (point-max))
      (irchat-w-insert irchat-D-buffer str)
      (goto-char (point-max))
      (set-window-point (irchat-get-buffer-window irchat-Dialogue-buffer)
			(point-max))
      (set-buffer buf))))


(defun irchat-Command-toggle-private ()
  (interactive)
  (let ((irchat-private-window 
	 (get-buffer-window 
	  (car (list (nth (- (length irchat-P-buffer) 1) irchat-P-buffer))))))
    (if irchat-private-window
	(if (interactive-p)
	    (let* ((my-IN-win (get-buffer-window irchat-Command-buffer))
		   (my-OUT-win (get-buffer-window irchat-Dialogue-buffer))
		   (sw (selected-window))
		   (INwh (window-height my-IN-win))
		   (OUTwh (+ (window-height my-OUT-win)
			     (window-height irchat-private-window))))
	      (delete-window irchat-private-window)
	      (if my-IN-win
		  (progn
		    (select-window my-IN-win)
		    (shrink-window (- (window-height my-IN-win) INwh))))
	      (if my-OUT-win
		  (progn
		    (select-window my-OUT-win)
		    (shrink-window (- (window-height my-OUT-win) OUTwh))))
	      (select-window sw)))
      (if (get-buffer-window irchat-Dialogue-buffer)
	  (let ((sw (selected-window)))
	    (select-window (get-buffer-window irchat-Dialogue-buffer))
	    (condition-case err
		(progn
		  (split-window-vertically)
		  (if irchat-swap-private
		      (progn
			(shrink-window 
			 (- irchat-private-window-height (window-height)))
			(other-window 1)
			(set-window-buffer 
			 (get-buffer-window irchat-Dialogue-buffer)
			 (irchat-get-buffer-create 
			  (nth (- (length irchat-P-buffer) 1)
			       irchat-P-buffer))))
		    (shrink-window 
		     (- (window-height) irchat-private-window-height))
		    (set-window-buffer 
		     (get-buffer-window irchat-Dialogue-buffer)
		     (irchat-get-buffer-create
		      (nth (- (length irchat-P-buffer) 1)
			   irchat-P-buffer)))))
	      (args-out-of-range 
	       (message "IRCHAT: Command-window-toggle with too small window")))
	    (select-window sw)))))
  (irchat-w-insert irchat-D-buffer "")) ;; recenter if needed

;;;
;;; eof
;;;
