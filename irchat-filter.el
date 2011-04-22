;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-filter.el,v 3.14 2011/04/22 23:14:16 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile
  (require 'irchat-inlines))

(eval-and-compile  
  (require 'irchat-vars))

;;;
;;;  These are defsubst just for speed, as it is expensive to call funtions at
;;;  emacs lisp (also evals are expensive)
;;;
(defun irchat-run-message-hook-types (hook prefix rest-of-line)
  "Run either old fashion irchat hook variable or hook list."
  (let ((hook (if (and (symbolp hook)
		       (boundp hook)
		       (not (fboundp hook)))
		  (eval hook)
		hook)))
    (if (or (and (symbolp hook)
		 (fboundp hook))
	    (and (listp hook)
		 (eq (car hook) 'lambda))
	    (and (fboundp 'byte-code-function-p)
		 (byte-code-function-p hook))
	    (and (fboundp 'compiled-function-p)
		 (compiled-function-p hook)))
	(eval (list hook prefix rest-of-line))
      (if (listp hook)
	  (let ((hooks hook)
		(r nil))
	    (while hooks
	      (if (irchat-run-message-hook-types (car hooks)
						 prefix 
						 rest-of-line)
		  (progn
		    (setq hooks '())
		    (setq r t))
		(setq hooks (cdr hooks))))
	    r)
	nil))))

(defsubst irchat-handle-message-2 (parsed-sender parsed-msg prefix message rest-of-line)
  "Helper function (actually a macro) for irchat-handle-message."
  (let ((hook (intern (concat "irchat-" message "-hook")))
	(after-hook (intern (concat "irchat-after-" message "-hook")))
	buffer-read-only fun)
    (if (irchat-run-message-hook-types hook prefix rest-of-line)
	;; If we have a hook, and it returns T, do nothing more
	nil
      ;; else call the handler

      (progn
	(if (string= message "msg")
	    (irchat-handle-msg-msg parsed-sender parsed-msg prefix rest-of-line)
	    
	  (if (fboundp (setq fun (intern
				  (concat "irchat-handle-" message "-msg"))))
	      (progn
		(apply fun (list parsed-sender parsed-msg prefix rest-of-line)))
	    (let* ((message-number (string-to-int message))
		   (default-number (/ message-number 100)))
	      (if (and (> message-number 0)
		       (fboundp (setq fun (intern
					   (concat "irchat-handle-"
						   (format "%d00" 
							   default-number)
						   "-msgs")))))
		  (progn
		    (apply fun (list message-number parsed-sender parsed-msg prefix rest-of-line)))
		(message "IRCHAT: Unknown IRC message \":%s %s %s\"" prefix
			 (upcase message) rest-of-line)
		(irchat-w-insert irchat-D-buffer 
				 (format "MESSAGE: %s, %s, %s\n" 
					 prefix message rest-of-line))
		))))
	(irchat-run-message-hook-types after-hook prefix rest-of-line)))))


(defsubst irchat-handle-message ()
  "Called when we have at least one line of output from the IRC server."
  (let ((obuf (current-buffer))
	beg end prefix message rest-of-line)
    (while (or
	    (looking-at 
	     "\\(:[^! \n]*\\)!\\([^ \n]*\\) \\([^ \n]+\\) :?\\(.*\\)\r\n")
	    (looking-at 
	     "\\(:[^ \n]*\\)?\\(\\) *\\([^ \n]+\\) :?\\(.*\\)\r\n")
	    (looking-at 
	     "\\(:[^! \n]*\\)!\\([^ \n]*\\) \\([^ \n]+\\) :?\\(.*\\)\n")
	    (looking-at 
	     "\\(:[^ \n]*\\)?\\(\\) *\\([^ \n]+\\) :?\\(.*\\)\n"))

      (setq beg (match-beginning 0)
	    end (match-end 0)
	    prefix (if (match-beginning 1)
		       (buffer-substring (1+ (match-beginning 1)) 
					 (match-end 1)))
	    irchat-userathost (buffer-substring (match-beginning 2)
						(match-end 2))
	    rest-of-line (buffer-substring (match-beginning 4) (match-end 4))
	    message (downcase 
		     (buffer-substring (match-beginning 3) (match-end 3))))

      (let ((cookie (if (and (stringp irchat-userathost)
			     (> (length irchat-userathost) 2))
			(substring irchat-userathost 0 1)
		      nil)))
	(cond ((null cookie)
	       (setq irchat-userathost-type 'invalid))
	      ((string= cookie "^")
	       (setq irchat-userathost (substring irchat-userathost
						  1
						  (length irchat-userathost)))
	       (setq irchat-userathost-type 'fake))
	      ((string= cookie "~")
	       (setq irchat-userathost (substring irchat-userathost
						  1
						  (length irchat-userathost)))
	       (setq irchat-userathost-type 'not-verified))
	      (t
	       (setq irchat-userathost-type 'ok))))

      (set-buffer irchat-Dialogue-buffer)
      (let ((parsed-sender nil)
	    (parsed-msg nil))
	(setq irchat-current-function (list parsed-sender parsed-msg prefix rest-of-line message))
	(irchat-handle-message-2 parsed-sender parsed-msg prefix message rest-of-line))
      (setq irchat-current-function (list "" "" "" "" ""))
      (set-buffer obuf)
      (delete-region beg end))))


(defun irchat-filter (process output)
  "Filter function for IRC server process."
  (let ((obuf (current-buffer))
	(data (match-data))
	(output (irchat-decode-coding-string output))
	bol)
    ;;
    ;; C-c C-d creates debug buffer for incoming messages...
    ;;
    (irchat-insert-to-debug output "<- ")

    (set-buffer (process-buffer process))
    (goto-char (point-max))
    ;;
    ;; convert input before it is processed: convert any occurrences of 
    ;; of heads of convert-list to corresponding tails. heads and tails 
    ;; may be functions in case they are evaluated, pattern-function is 
    ;; called with received message as input and target-function with 
    ;; output of pattern function: this enables user to create bots for 
    ;; example... BTW: long convert list slow thing down noticeably
    ;;
    (let ((conv-list irchat-receive-convert-list))
      (while (and conv-list (not irchat-polling))
	(let* ((i (car conv-list)) (f (car i)) (s (car (cdr i)))
	       (s1 (if (stringp f) f (funcall f output)))
	       (s2 (if (stringp s) s (funcall s s1))))
	  (setq output (irchat-replace-in-string output s1 s2)
		conv-list (cdr conv-list)))))

    (insert output)
    (goto-char (point-min))
    (while (re-search-forward "\n\n" (point-max) t)
      (delete-char -1)) ; This hack (from mta) is for 2.4 servers
    (goto-char (point-min))

    (if (string-match "\n" output)
	(irchat-handle-message))
    (set-buffer obuf)
    (store-match-data data)))


(defun irchat-sentinel (proc status)
  "Sentinel function for Irchat process."
  (if irchat-reconnect-automagic
      (progn 
	(condition-case err
	    (progn
	      (set-process-filter irchat-server-process nil))
	  (wrong-type-argument 
	   nil))
	(condition-case err
	    (progn
	      (set-process-sentinel irchat-server-process nil))
	  (wrong-type-argument 
	   nil))
	(setq irchat-server-process nil)
	(irchat))
    (message "IRCHAT: Connection closed.")))

(eval-and-compile
  (provide 'irchat-filter))
;;;
;;; eof
;;;
