;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-400.el,v 3.5 1997/03/19 17:08:51 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))

;;;
;;;  400 replies -- ERRORS
;;; 
(defun irchat-handle-400-msgs (number prefix rest)
  "Generic handler for 4?? messages. This is called if no specific handler exists"
  (if (string-match "[^ ]* \\([^ :]*\\) *\\([^ :]*\\) *:\\(.*\\)" rest)
      (let ((target1 (matching-substring rest 1))
	    (target2 (matching-substring rest 2))
	    (msg (matching-substring rest 3)))
	(cond ((string-equal target1 "")
	       (irchat-w-insert irchat-400-buffer
				(format "%s%s\n" 
					irchat-error-prefix
					msg)))
	      ((string-equal target2 "")
	       (irchat-w-insert irchat-400-buffer
				(format "%s%s (%s)\n" 
					irchat-error-prefix 
					msg 
					target1)))
	      (t
	       (irchat-w-insert irchat-400-buffer
				(format "%s%s %s (%s)\n"
					irchat-error-prefix 
					target1
					msg 
					target2))))
	)
    (message "IRCHAT: Strange %s reply" number)))


(defun irchat-handle-401-msg (prefix rest) ;;; ERR_NOSUCHNICK
  (if (string-match "[^ ]+ \\([^ ]+\\) +:\\(.*\\)" rest)
      (let ((name (matching-substring rest 1))
	    (error (matching-substring rest 2)))
	(irchat-change-nick-of name nil)
	(irchat-send "WHOWAS %s" name))
    (message "IRCHAT: Strange 401 reply")))


(defun irchat-handle-406-msg (prefix rest)
  (if (string-match "[^ ]+ \\([^ ]+\\) +:\\(.*\\)" rest)
      (let ((nickorchan (matching-substring rest 1)))
	(irchat-change-nick-of nickorchan nil)
	(message "IRCHAT: No such user %s" nickorchan))))


(defun irchat-handle-412-msg (prefix rest)
  (message "IRCHAT: No text to send"))


(defun irchat-iterate-nick (nick)
  (let* ((fmt (format "%s_" nick))
	 (new (substring fmt 0 (min 9 (length fmt)))))
    (if (string= nick new)
	(irchat-iterate-nick (format "_%s" nick))
      new)))


(defun irchat-handle-432-msg (prefix rest) ;;; ERR_ERRONEUSNICKNAME
  "Handle the 432 reply (erroneus nickname)"
  (save-excursion
    (set-buffer irchat-Command-buffer)
    (beep)
    (let ((nick (cond ((string-match "^\\([^ ]+\\) +\\([^ ]+\\) +:\\(.*\\)" 
				     rest)
		       (matching-substring rest 2))
		      ((string-match "^ *\\([^ ]+\\) :.*" rest)
		       (matching-substring rest 1))
		      (t
		       "UNKNOWN (Could not figure out, contact developers)"))))
      (if (eq irchat-nick-accepted 'ok)
	  (setq irchat-real-nickname irchat-old-nickname))
      (message "IRCHAT: Erroneous nickname %s.  Choose a new one with %s."
	       nick
	       (substitute-command-keys "\\[irchat-Command-nickname]")))))
	

(defun irchat-handle-433-msg (prefix rest) ;;; ERR_NICKNAMEINUSE
  "Handle the 433 reply (nickname already in use)"
  (if (not (eq irchat-nick-accepted 'ok))
      (progn
	(setq irchat-real-nickname (irchat-iterate-nick irchat-real-nickname))
	(setq old-irchat-nickname irchat-real-nickname)
	(irchat-send "NICK %s" irchat-real-nickname)
	(setq irchat-nick-accepted 'sent))
    (save-excursion
      (set-buffer irchat-Command-buffer)
      (beep)
      (setq irchat-real-nickname irchat-old-nickname)
      (let ((nick (cond ((string-match "^\\([^ ]+\\) +\\([^ ]+\\) +:\\(.*\\)" 
				       rest)
			 (matching-substring rest 2))
			((string-match "^ *\\([^ ]+\\) :.*" rest)
			 (matching-substring rest 1))
			(t
			 "UNKNOWN (Could not figure out, contact developers)"))))
	(message 
	 "IRCHAT: Nickname %s already in use.  Choose a new one with %s."
	 nick
	 (substitute-command-keys "\\[irchat-Command-nickname]"))))))


(defun irchat-handle-482-msg (prefix rest)
  (message "IRCHAT: You are not a channel operator"))

;;;
;;; eof
;;;
