;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-hooks.el,v 3.5 2005/10/02 21:24:48 tri Exp $
;;;
;;;  Example hooks to customize irchat. These are copy-pasted from 2.2beta
;;;  and modified a bit. The might or might not work. The general idea is 
;;;  that if hook returns nil, the appropriate handler is called after the 
;;;  hook. Any other value returned causes further processing of message 
;;;  cancelled.

(defun string-memberp (thing list)
  "Dummy case insensitive function to check if string THING is a member of
LIST"
  (cond ((null list) nil)
        ((string-equal (downcase thing) (downcase (car list))) t)
        (t (string-memberp thing (cdr list)))))

;;;
;;;
;;;
(defvar irchat-watched-nicks '("God" "Pope" "Paavi" "JuhaF|hr"))


(defun irchat-join-hook (nick channel)
  (cond
   ((string= "#report" channel)
    t)
   ;; greet persons at current channel
   ((and (string-memberp nick irchat-watched-nicks)
         (string= irchat-current-channel channel))
    (progn
      (beep)
      (irchat-Command-send-message (format "%s: et ole kuitenkaan aito" nick))
      (message "%s has joined channel %s" nick channel)
      nil))
   (t nil)))


(defun irchat-part-hook (prefix rest)
  (if (string= "#report" rest)
      t nil))


(setq irchat-join-hook (function irchat-join-hook))
(setq irchat-part-hook (function irchat-part-hook))


;;;
;;;
;;;
(defvar irchat-broadcast-partner nil)

(defun irchat-privmsg-hook (prefix rest)
  (if (and irchat-broadcast-partner (not irchat-chat-partner))
      (progn
	(if (eq prefix nil)
	    (setq prefix "")
	  (setq prefix (concat "<" prefix "> ")))
	(if (and (string= prefix "")
		 (string-match (format "^%s.*" 
				       (format (irchat-format-string 1 
								     nil))) 
			       rest))
	    nil
	  (progn 
	    (string-match "[^ ]* :\\(.*\\)" rest)
	    (irchat-send-privmsg "PRIVMSG %s :%s%s" 
				 irchat-broadcast-partner 
				 prefix
				 (matching-substring rest 1))))
	nil)
    nil))


(defun irchat-msg-hook (prefix rest)
  (if (and irchat-broadcast-partner (not irchat-chat-partner))
      (progn
	(if (eq prefix nil)
	    (setq prefix "")
	  (setq prefix (concat "<" prefix "> ")))
	(if (and (string= prefix "")
		 (string-match (format "^%s.*" 
				       (format (irchat-format-string 1
								     nil)))
			       rest))
	    nil
	  (progn 
	    (string-match "[^ ]* :\\(.*\\)" rest)
	    (irchat-send-privmsg "PRIVMSG %s :%s%s"
				 irchat-broadcast-partner 
				 prefix
				 (matching-substring rest 1))))
	nil)
    nil))
	    

(setq irchat-privmsg-hook (function irchat-privmsg-hook))
(setq irchat-msg-hook (function irchat-msg-hook))

;;;
;;; following stuff stolen from nAm (modified a bit), untested, 
;;; use on your own risk
;;;
(define-key irchat-Command-mode-map "\C-c\C-a"
  '(lambda (&optional private)
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
       (irchat-send-privmsg "PRIVMSG %s :ACTION %s"
			    (if private
				irchat-privmsg-partner
			      irchat-current-channel)
			    message)
       (if private
	   (irchat-own-message (format "*** Action to %s: %s %s"
				       irchat-privmsg-partner irchat-real-nickname
				       message))
	 (irchat-own-message (format "*** Action: %s %s" irchat-real-nickname
				     message))))))

(setq irchat-Startup-hook '(lambda ()
			     (sit-for 3) ; wait for possible nick collision
			     (irchat-Command-message "eu-oper" "invite")
                             (irchat-Command-message "Arska" "kutsu")
			     (setq scroll-step 1)))

(setq irchat-invite-hook '(lambda (prefix rest)
                            (if (string-match " \\([^ ]+\\)" rest)
                                (let ((chnl (matching-substring rest 1)))
				  (cond 
				   ((string= prefix "Arska")
				    (irchat-Command-join chnl))
				   ((string= prefix "Eu-Oper")
				    (let ((curchnl irchat-current-channel))
				      (irchat-Command-join chnl)
				      (if curchnl
					  (irchat-Command-join curchnl)))))))
			    nil))

;;;
;;;  to make a bot....
;;;
(defvar irchat-arska-mode nil)

(defun irchat-ownjoin-hook (prefix rest)
  (if (and (string-match "#eu-opers" rest)
	   (string-match irchat-real-nickname prefix))
      (irchat-Command-message "eu-oper" "op"))
  (if (string-match "kuolepois" prefix)
      (setq irchat-arska-mode nil)
    (if (string-match "namarska" prefix)
	(setq irchat-arska-mode t)))
  (if (and irchat-arska-mode
	   (string-match "\\(.fi\\)" irchat-userathost)
	   (= (match-end 0) (match-end 1)))
      (irchat-send "mode %s +o %s" rest prefix))
  (if (and (string-match "#report" rest)
	   (not (string-match prefix irchat-real-nickname)))
      (message "%s (%s) has joined this channel%s"
	       prefix irchat-userathost
	       (if (string= (or irchat-current-channel "") rest) ""
		 (format " (%s)" rest)))
    nil))

(defun irchat-ownpart-hook (prefix rest)
  (if (and (string-match "#report" rest)
	   (not (string-match prefix irchat-real-nickname)))
      (message "%s has left this channel%s"
	       prefix
	       (if (string= (or irchat-current-channel "") rest) ""
		 (format " (%s)" rest)))
    nil))

(defun irchat-ownquit-hook (prefix rest)
  (if (and (string-match "#report" rest)
	   (not (string-match prefix irchat-real-nickname)))
      (message "%s has left IRC%s"
	       prefix
	       (if (= 0 (length rest)) "" (format " (%s)" rest)))
    nil))

(setq irchat-join-hook (function irchat-ownjoin-hook))
(setq irchat-part-hook (function irchat-ownpart-hook))
(setq irchat-quit-hook (function irchat-ownquit-hook))

(eval-and-compile (provide 'irchat-hooks))

;;;
;;;  eof
