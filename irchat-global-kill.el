;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-global-kill.el,v 1.5 1998/10/06 11:47:06 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info
;;;
;;; Handle global kill ctcp messages by jsl and tri.
;;;
;;; Created              : Mon Mar 10 17:07:39 1997 tri
;;; Integrated to Irchat : Sun Oct 19          1997 tri
;;;

(eval-when-compile (require 'irchat-inlines))

;(setq irchat-global-kill-valid-senders 
;      (list ("mypal@.*\\.hut.fi" . 600) ;; max 600 minutes from my pal
;            (".*@.*\\.hut\\.fi" . 30)   ;; max 30 minutes from .hut.fi
;             \".*@.*\\.fi\")            ;; max default (99) minutes from .fi

(defun irchat-remove-from-ignore-list (name)
  "Remove NAME from ignore list, if there."
  (let ((elem (assoc-ci-string name irchat-ignore-nickname)))
    (if elem
	(setq irchat-ignore-nickname
	      (remassoc (car elem) irchat-ignore-nickname))))
  irchat-ignore-nickname)


(defun irchat-global-kill-valid-sender (uah)
  (let ((l irchat-global-kill-valid-senders)
	(r nil))
    (while l
      (let* ((ca (car l))
	     (ua (if (consp ca) (car ca) ca))
	     (lm (if (consp ca) 
		     (cdr ca) 
		   irchat-global-kill-default-timeout-limit)))
	(if (and (string-match ua uah)
		 (> lm 0))
	    (progn
	      (setq l '())
	      (setq r lm))
	  (setq l (cdr l)))))
    r))


(defun irchat-ctl-a-ignore-msg-hook (from rest)
  (if (and (stringp rest)
	   (string-match "^\\(.+\\) \\([0-9][0-9]*\\) \\(.+\\)" rest))
      (let ((nick (matching-substring rest 1))
	    (timeout (string-to-int (matching-substring rest 2)))
	    (reason (matching-substring rest 3))
	    (valid-sender (irchat-global-kill-valid-sender irchat-userathost)))
	(if (string= nick (regexp-quote nick))
	    (if valid-sender
		(let* ((nick-uah (if irchat-global-kill-use-uah-p
				     (irchat-nick-to-uah nick)
				   nil))
		       (ign-name (if nick-uah
				     nick-uah
				   nick))
		       (ign-list (if nick-uah
				     (irchat-convert-uah-to-ignore-list 
				      nick-uah)
				   '()))
		       (real-timeout (if (> timeout valid-sender)
					 valid-sender
				       timeout))
		       (bad-timeout-p (not (= timeout real-timeout))))
		  (irchat-w-insert irchat-D-buffer
				   (format 
			        "*** %s: Ignoring %s%s for %d minutes%s (%s)\n"
				    from
				    ign-name
				    (if (string= nick ign-name)
					""
				      (concat " (" nick ")"))
				    real-timeout
				    (if bad-timeout-p
					(format " [%d minutes requested]"
						timeout)
				      "")
				    reason))
		  (if (not ign-list)
		      (if (not (assoc-ci-string ign-name 
						irchat-ignore-nickname))
			  (irchat-Command-ignore
			   (downcase ign-name)
			   real-timeout 
			   irchat-global-kill-use-silent-ignore))
		    (let ((l ign-list))
		      (while l
			(if (not (assoc-ci-string (car l)
						  irchat-ignore-nickname))
			    (irchat-Command-ignore
			     (downcase (car l))
			     real-timeout
			     irchat-global-kill-use-silent-ignore))
			(setq l (cdr l))))))
	      (let ((msg (format "Rejected IGNORE from %s <%s> (%s, %d, %s)."
				 from irchat-userathost nick timeout reason)))
		(if (or (equal 'minibuffer
			       irchat-global-kill-report-ignored)
			(equal t
			       irchat-global-kill-report-ignored))
		    (message msg))
		(if (or (equal 'dialogue
			       irchat-global-kill-report-ignored)
			(equal t
			       irchat-global-kill-report-ignored))
		    (irchat-w-insert irchat-D-buffer (concat irchat-info-prefix
							     msg
							     "\n"))))))))
  t)


(defun irchat-Command-global-ignore (nickname timeout reason)
  (interactive (let ((nickname-var nil)
		     (timeout 0)
		     (reason ""))
		 (setq nickname
		       (completing-read "Global ignore nickname: "
					irchat-nick-alist
					'(lambda (s) t) nil nil))
		 (if (not (string= "" nickname))
		     (progn
		       (while (not (and (> timeout 0)
					(< timeout 1000)))
			 (setq timeout
			       (string-to-int
				(read-from-minibuffer "Timeout [1-99]: "))))
		       (setq reason (read-from-minibuffer "Reason: "))))
		 (list nickname timeout reason)))

  (if (not (string= "" nickname))
      (let* ((k1 (format "%s-ignore" irchat-current-channel))
	     (k2 (format "%s-kill" irchat-current-channel))
	     (k3 irchat-current-channel)
	     (key (if (assoc-ci-string k1 irchat-default-idea-key-list)
		      k1
		    (if (assoc-ci-string k2 irchat-default-idea-key-list)
			k2
		      (if (assoc-ci-string k3 irchat-default-idea-key-list)
			k3
			nil)))))
	(if (null key)
	    (irchat-w-insert irchat-D-buffer
			     (format 
			      "*** error: no default key (%s, %s or %s) set\n"
			      k1 k2 k3))
	  (let ((msg (irchat-encrypt-message
		      (format "IGNORE %s %d %s" nickname timeout reason)
		      key t)))
	    ; Ignore
	    (let* ((nick-uah (if irchat-global-kill-use-uah-p
				 (irchat-nick-to-uah nickname)
			       nil))
		   (ign-name (if nick-uah
				 nick-uah
			       nickname))
		   (ign-list (if nick-uah
				 (irchat-convert-uah-to-ignore-list 
				  nick-uah)
			       '())))
	      (irchat-w-insert irchat-D-buffer
			       (format 
			     "*** Globally ignoring %s%s for %d minutes (%s)\n"
				ign-name
				(if (string= nickname ign-name)
				    ""
				  (concat " (" nickname ")"))
				timeout
				reason))
	      (if (not ign-list)
		  (progn
		    (irchat-remove-from-ignore-list ign-name)
		    (irchat-Command-ignore (downcase ign-name)
					   timeout
					 irchat-global-kill-use-silent-ignore))
		(let ((l ign-list))
		  (while l
		    (irchat-remove-from-ignore-list (car l))
		    (irchat-Command-ignore (downcase (car l))
					   timeout 
					 irchat-global-kill-use-silent-ignore)
		    (setq l (cdr l)))))
	      ; Send ctcp
	      (irchat-send-privmsg "PRIVMSG %s :%s"
				   irchat-current-channel
				   msg)))))))


(setq irchat-ctl-a-ignore-msg-hook (function irchat-ctl-a-ignore-msg-hook))

(eval-and-compile (provide 'irchat-global-kill))
;;;
;;; eof
;;;
