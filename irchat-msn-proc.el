;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-proc.el,v 3.4 2002/06/09 14:23:38 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

(defun irchat-msn-filter (process output)
  "Filter function for IRC server process."
  (let ((obuf (current-buffer))
	(data (match-data))
	(output (irchat-decode-coding-string output))
	bol)
    ;;
    ;; C-c C-d creates debug buffer for incoming messages...
    ;;
    (irchat-insert-to-debug output "<-MSN- ")
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (while (re-search-forward "\n\n" (point-max) t)
      (delete-char -1)) ; This hack (from mta) is for 2.4 servers
    (goto-char (point-min))
    (if (string-match "\n" output)
	(irchat-msn-handle-message))
    (set-buffer obuf)
    (store-match-data data)))

(defun irchat-msn-sentinel (proc status)
  "Sentinel function for Irchat MSN process."
  (setq irchat-msn-connection-phase nil)
  (message "IRCHAT: MSN connection closed.")
  (irchat-msn-kill-all-conversations)
  (irchat-w-insert irchat-MSN-buffer 
		   (format "%sMSN Messenger connection terminated.\n"
			   irchat-msn-info-prefix))
  (irchat-set-msn-indicator))

(defun irchat-msn-handle-message ()
  "Called when we have at least one line of output from the MSN server."
  (let ((obuf (current-buffer)))
    (while (and (buffer-live-p obuf)
		(irchat-msn-consume-message))
      (if (buffer-live-p obuf)
	  (set-buffer obuf)))))

(defun irchat-msn-consume-message ()
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
		     (irchat-msn-handle-MSG-message cmd-len pp-uid pp-name len msg)
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
		   (setq handler (intern (concat "irchat-msn-handle-" cmd))))
	       (progn
		 (setq cmd "")
		 (setq handler nil)))
	     (let ((ml (irchat-msn-proto-msg-parse msg)))
	       (if ml
		   (if (and handler (fboundp handler))
		       (apply handler (list ml msg))
		     (irchat-msn-handle-generic ml msg))))
	     t))
	  (t nil))))

(eval-and-compile (provide 'irchat-msn-proc))

;;; eof (irchat-msn-proc.el)
