;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-dcc.el,v 3.2 1997/02/26 16:05:59 too Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info
;;;
;;; This file:
;;; Copyright 1994 Kim Nyberg <kny@tekla.fi>
;;; All Rights Reserved

(eval-when-compile (require 'irchat-inlines))

(defvar irchat-dcc-receive-list nil)
(defvar irchat-dcc-receive-direct t)

(defun irchat-ctl-a-dcc-msg (from chnl rest)
  (irchat-w-insert irchat-D-buffer (format "*** DCC from %s: %s\n" from rest))
  ;; old versions of ircii 2.2.1, do not send file length on DCC request.
  ;; so irchat can not do DCC with them. Sorry. ircii 2.2.9 does it on 
  ;; correctly. Why do people change protocol inside one minor version?
  (if (string-match 
       "\\(SEND\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)" rest)
      (let ((command "receive")
	    (filename (file-name-nondirectory (matching-substring rest 2)))
	    (host (matching-substring rest 3))
	    (port (matching-substring rest 4))
	    (size (matching-substring rest 5)))
	(setq irchat-dcc-receive-list
	      (append irchat-dcc-receive-list
		      (list (list command host port size filename))))
	(if irchat-dcc-receive-direct
	    (progn 
	      (irchat-w-insert irchat-D-buffer 
			       (format "*** DCC autoreceive\n"))
	      (irchat-Command-dcc-receive))))))


(defun irchat-Command-dcc-send ()
  "Send file to user."
  (interactive)
  (let ((filename (expand-file-name (read-file-name
				     "File to send: "
				     default-directory nil))))
    (setq irchat-privmsg-partner
	(irchat-completing-default-read 
	 "To whom: "
	 (append irchat-nick-alist irchat-channel-alist)
	 '(lambda (s) t) 
	 nil irchat-privmsg-partner))
    (set-process-filter
     (start-process irchat-dcc-program nil 
		    irchat-dcc-program "send" 
		    (number-to-string irchat-dcc-port)
		    filename)
     'irchat-dcc-send-filter)
    (setq irchat-dcc-port (1+ irchat-dcc-port))))


(defun irchat-dcc-send-filter (process output)
  (cond ((string-match "DCC file [^ ]* sent" output)
	 (irchat-own-message (substring output 0 (1- (length output))))
	 (message ""))
	((string-match 
	  "DCC send [^ ]*/\\([^ ]*\\) \\([^ ]+\\) \\([^ ]+\\) \\(.+\\)" output)
	 (let ((filename (matching-substring output 1))
	       (port (matching-substring output 2))
	       (machine (matching-substring output 3))
	       (size (matching-substring output 4)))
	   (irchat-send "PRIVMSG %s :DCC SEND %s %s %s %s"
			irchat-privmsg-partner filename machine port size)
	   (irchat-own-message
	    (format "*** Sending file %s (%s bytes) to %s"
		    filename size irchat-privmsg-partner))))))


(defun irchat-Command-dcc-receive (&optional number)
  "Receive next file from list."
  (interactive "P")
  (let ((dcc-object (car irchat-dcc-receive-list)))
    (setq irchat-dcc-receive-list (cdr irchat-dcc-receive-list))
    (if dcc-object
	(let ((command (car dcc-object))
	      (host (nth 1 dcc-object))
	      (port (nth 2 dcc-object))
	      (size (nth 3 dcc-object))
	      (default (nth 4 dcc-object)))
	  (irchat-own-message
	   (format "*** Getting file %s (%s bytes)" default size))
	  (let ((filename (expand-file-name default irchat-dcc-directory)))
	    (set-process-filter
	     (start-process irchat-dcc-program nil irchat-dcc-program 
			    command host port size filename)
	     'irchat-dcc-receive-filter))))))


(defun irchat-dcc-receive-filter (process output)
  (if (string-match "DCC file [^ ]* received" output)
      (irchat-own-message (substring output 0 (1- (length output))))
    (message "%s" (substring output 0 (1- (length output))))))


(defun irchat-Command-dcc-list ()
  "List files in receive queue."
  (interactive)
  (let* ((dcc-list irchat-dcc-receive-list)
	 dcc-object)
    (while dcc-list
      (setq dcc-object (car dcc-list)
	    dcc-list (cdr dcc-list))
      (let ((filename (cdr (cdr (cdr (cdr dcc-object)))))
	    (size (car (cdr (cdr (cdr dcc-object))))))
	(irchat-own-message
	 (format "*** DCC file %s (%s bytes)" filename size))))))


(defun irchat-dcc-compare-hostnames (h1 h2)
  "Compare two internet domain hostnames. Return true iff they resolve to the 
same IP-address."
  (if (string-ci-equal h1 h2)
      t
    (if irchat-dcc-program
	(let ((pob (get-buffer-create "*IRC DCC resolve*"))
	      (output) (domatch nil))
	  (save-excursion
	    (call-process irchat-dcc-program nil pob nil "resolve" h1 h2)
	    (set-buffer pob)
	    (goto-char (point-min))
	    (setq output (buffer-substring (point-min) (point-max)))
	    (if (string-match "\\([^ ]+\\)\n\\([^ ]+\\)\n" output)
		(if (string= (matching-substring output 1) 
			     (matching-substring output 2))
		    (setq domatch t))))
	  (kill-buffer pob)
	  domatch)
      (string-ci-equal h1 h2))))
      
(eval-and-compile (provide 'irchat-dcc))
;;;
;;; eof
;;;
