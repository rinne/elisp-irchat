;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-misc.el,v 1.2 1997/01/31 13:01:48 too Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile
  (require 'irchat-globals)  
  (require 'irchat-vars)
  (require 'irchat-inlines)
  (require 'irchat-filter))

(defun irchat-ignore-this-p (nick uah)
  (let ((killit nil))
    (mapcar (function 
	     (lambda (kill)
	       (if (string-match kill nick)
		   (setq killit t)))) irchat-kill-nickname)
    (if (not killit)
	(mapcar (function 
		 (lambda (kill)
		   (if (string-match kill uah)
		       (setq killit t)))) irchat-kill-nickname))
    killit))

(defun irchat-own-message (message)
  (irchat-w-insert (irchat-pick-buffer irchat-current-channel)
		   (format "%s\n" message))
  (if (not irchat-ownfreeze)
      (let ((chan-buffer (irchat-pick-buffer irchat-current-channel)))
	(irchat-freeze-toggle (car chan-buffer))
	(irchat-w-insert chan-buffer "")
	(irchat-freeze-toggle (car chan-buffer)))))


(defun irchat-own-private-message (message)
  (irchat-w-insert irchat-P-buffer (format "%s\n" message)))

(defun irchat-send (&rest args)
  (let ((item (apply 'format args)) ditem)
    (let ((conv-list irchat-send-convert-list))
      (while conv-list
	(setq item (irchat-replace-in-string item (car (car conv-list))
				      (car (cdr (car conv-list)))))
	(setq conv-list (cdr conv-list))))
    (process-send-string irchat-server-process
			 (concat item "\r"))
    (setq ditem (downcase item))
    (if (string-match "^list" (downcase ditem))
	(if (string-match "\\(^list\\) \\(.+\\)" ditem)
	    (setq irchat-channel-filter (matching-substring ditem 2))
	  (setq irchat-channel-filter "")))))    


(defun irchat-clean-hostname (hostname)
  "Return the arg HOSTNAME, but if is a dotted-quad, put brackets around it."
  (let ((data (match-data)))
    (unwind-protect
	(if (string-match "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" hostname)
	    (concat "[" hostname "]")
	  hostname)
      (store-match-data data))))


(defun irchat-replace-in-string (string from to)
  (if (string-match "XEmacs" emacs-version)
      (replace-in-string str regexp newtext t)
    (save-excursion
      (let ((buf (get-buffer-create "*replace-in-string*")) res)
        (set-buffer buf)
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match newtext t t))
        (setq res (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer buf)
        res)
      )))

(defun irchat-scroll-if-visible (window)
  (if window (set-window-point window (point-max))))


(defun irchat-completing-default-read 
  (prompt table predicate require-match initial-input)
  "completing-read w/ default argument like in 'kill-buffer'"
  (let ((default-read
	  (completing-read
	   (if initial-input
	       (format "%s(default %s) "
		       prompt initial-input)
	     (format "%s" prompt))
	   table predicate require-match nil)))
    (if (and (string= default-read "") initial-input)
	initial-input
      default-read)))


(defun irchat-greet-user (user chnl)
  ;; actually user has already been interned, but who cares. 
  (let ((u (intern user irchat-obarray)))
    (if (get u 'irchat-greeting)
	(progn 
	  (beep t)
	  ;; tell me that she has arrived
	  (message "IRCHAT: %s has entered! (%s)"
		   user
		   (if (string= chnl "0") 
		       "on no channel yet"
		     (concat "on channel " chnl)))
	  ;; and send her a greeting
	  (irchat-send "PRIVMSG %s :%s" user (get u 'irchat-greeting))
	  ;; ... done
	  (put u 'irchat-waited-for nil)
	  (put u 'irchat-greeting nil)))))


(defun irchat-greet-author ()
  (setq irchat-greet-author nil)
  (irchat-send "PRIVMSG %s :%s <%s@%s> is using irchat version %s"
	       irchat-author-nickname 
	       (user-full-name) 
	       (user-login-name) 
	       irchat-system-fqdname
	       irchat-version))


(defun irchat-change-nick-of (old new)
  (let ((pair (assoc old irchat-nick-alist)))
    (if new
	(if pair
	    (rplaca pair new)
	  (setq irchat-nick-alist (cons (cons new nil) irchat-nick-alist)))
      (setq irchat-nick-alist (string-list-ci-delete old 
						     irchat-nick-alist)))))


(defun irchat-convert-seconds (time)
  "Convert seconds to printable string."
  (let* ((seconds (string-to-int time))
	 (minutes (/ seconds 60))
	 (seconds (if minutes (% seconds 60) seconds))
	 (hours (/ minutes 60))
	 (minutes (if hours (% minutes 60) minutes))
	 (days (/ hours 24))
	 (hours (if days (% hours 24) hours))
	 (ds (and (/= 0 days)
		  (format "%d day%s, " days
			  (if (> days 1) "s" ""))))
	 (hs (and (/= 0 hours)
		  (format "%d hour%s, " hours
			  (if (> hours 1) "s" ""))))
	 (ms (and (/= 0 minutes)
		  (format "%d minute%s " minutes
			  (if (> minutes 1) "s" ""))))
	 (ss (format "%d seconds" seconds)))
    (concat ds hs ms (if seconds ss ""))))


(defun irchat-msg-from-ignored (prefix rest)
  (save-excursion
    (let ((buf (current-buffer)))
      (irchat-w-insert irchat-I-buffer (format "%s::%s\n" prefix rest))
      t)))

(defun irchat-window-height (win)   ;;; No w-v-h in GNU-Emacs //tri
  "Get the visible window height portably with GNU-Emacs and XEmacs." 
  (if (fboundp 'window-displayed-height)
      (window-displayed-height win)
    (window-height win)))

;;;
;;; eof
;;;
