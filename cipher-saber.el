;;;   -*- lisp -*-
;;; 
;;;  ----------------------------------------------------------------------
;;;  Cipher Saber encryption in elisp.  Cool, ha?
;;;  ----------------------------------------------------------------------
;;;  Created      : Tue Jul  7 18:55:02 1998 tri
;;;  Last modified: Wed Jul  8 15:00:08 1998 tri
;;;  ----------------------------------------------------------------------
;;;  Copyright © 1998
;;;  Timo J. Rinne <tri@iki.fi>
;;; 
;;;  Address: Cirion oy, PO-BOX 250, 00121 Helsinki, Finland
;;;  ----------------------------------------------------------------------
;;;  Any express or implied warranties are disclaimed.  In no event
;;;  shall the author be liable for any damages caused (directly or
;;;  otherwise) by the use of this software.
;;;  
;;;  irchat-copyright.el applies only if used with irchat IRC client.
;;;  Contact the author for additional copyright info.
;;;
;;;  $Id: cipher-saber.el,v 1.5 1998/07/08 12:00:16 tri Exp $
;;;

(eval-and-compile  
  (require 'rc4)
  (provide 'cipher-saber))

(defconst cipher-saber-iv-length 10
  "Length of the cipher safer initialization vector (10 in specification).")

(defvar cipher-saber-random-state nil
  "State of the Cipher Saber internal random number generator.")

(defvar cipher-saber-random-entropy "Saber!"
  "String that is added to the initial random state.  You should change this.")

; ; Example code to form cipher-saber-random-entropy.
; (setq cipher-saber-random-entropy (concat "my secret word"
;                                           (prin1-to-string (current-time))
;                                           (current-time-string)
;                                           (format "%d" (random t))
;                                           (format "%c%c%c" 
;                                                   (random 256)
;                                                   (random 256)
;                                                   (random 256))))

(defun cipher-saber-warning (msg &optional beep-p)
  "Output warning MSG and beep if BEEP-P is non nil."
  (if (not (null beep-p)) (beep t))
  (message (concat "Warning: " msg)))

(defun cipher-saber-generate-initial-salt (&optional key)
  "Generate pseudo random string from emacs standard stuff with optional KEY."
  (let ((s (if (stringp key) key "")))
    (while (< (length s) 256)
      (setq s (concat s (char-to-string (if (fboundp 'tri-random) 
					    (tri-random 256) 
					  (random 256))))))
    s))

(defun cipher-saber-init-random-state ()
  "Initialize Cipher Saber internal random number generator."
  (setq cipher-saber-random-state 
	(rc4-make-state (cipher-saber-generate-initial-salt
			 cipher-saber-random-entropy)))
  (rc4-random cipher-saber-random-state)
  (rc4-random cipher-saber-random-state)
  (let ((x (+ (rc4-random cipher-saber-random-state) 10)))
    (while (> x 0)
      (rc4-random cipher-saber-random-state)
      (setq x (- x 1)))))

(defun cipher-saber-random-byte ()
  (if (null cipher-saber-random-state)
      (cipher-saber-init-random-state))
  (rc4-random cipher-saber-random-state))

(defun cipher-saber-make-iv ()
  "Build random Cipher Safer initialization vector."
  (let ((r (make-string 10 0))
	(i 0))
    (while (< i cipher-saber-iv-length)
      (aset r i (cipher-saber-random-byte))
      (setq i (+ i 1)))
    r))

(defun cipher-saber-check-key (key)
  "Check and possibly truncate cipher saber key."
  (let ((l (length key))
	(m (- 256 cipher-saber-iv-length))
	(w (- 64 cipher-saber-iv-length)))
    (cond ((> l m) 
	   (cipher-saber-warning
	    (format "CipherSaber key exceeds %d bytes.  Truncated!" m)
	    t)
	   (setq key (substring key 0 m)))
	  ((> l 54) 
	   (cipher-saber-warning
	    (format "CipherSaber key exceeds %d bytes.  Avoid this!" w))))
    key))

(defun cipher-saber-make-encryption-state (key)
  "Build initial Cipher Safer encryption state with KEY."
  (let ((iv (cipher-saber-make-iv))
	(key (cipher-saber-check-key key)))
    (vector (rc4-make-state (concat key iv))
	    iv)))

(defun cipher-saber-encrypt (str state)
  "Cipher Safer encrypt STR with a given STATE and update state."
  (let ((head (if (stringp (elt state 1)) (elt state 1) "")))
    (aset state 1 nil)
    (concat head (rc4-encrypt str (elt state 0)))))

(defun cipher-saber-encrypt-string (str key)
  "Cipher Safer encrypt STR with a given KEY."
  (cipher-saber-encrypt str (cipher-saber-make-encryption-state key)))

(defun cipher-saber-encrypt-buffer (key)
  "Cipher Saber encrypt current buffer with KEY."
  (interactive (list (read-from-minibuffer "Key: ")))
  (let* ((p (buffer-substring (point-min) (point-max) (current-buffer)))
	 (c (cipher-saber-encrypt-string p key)))
    (delete-region (point-min) (point-max) (current-buffer))
    (insert c)
    (goto-char (point-min))))

(defun cipher-saber-make-decryption-state (key)
  "Build initial Cipher Safer decryption state with KEY."
  (let ((key (cipher-saber-check-key key)))
    (vector key "")))

(defun cipher-saber-decrypt (str state)
  "Cipher Safer decrypt STR with a given STATE and update state."
  (if (stringp (elt state 1))
      (let ((wait-iv (- cipher-saber-iv-length (length (elt state 1)))))
	(if (< (length str) wait-iv)
	    (progn
	      (aset state 1 (concat (elt state 1) str))
	      "")
	  (progn
	    (aset state 1 (concat (elt state 1) (substring str 0 wait-iv)))
	    (aset state 0 (rc4-make-state (concat (elt state 0) 
						  (elt state 1))))
	    (aset state 1 nil)
	    (setq str (substring str wait-iv (length str)))
	    (rc4-encrypt str (elt state 0)))))
    (rc4-encrypt str (elt state 0))))

(defun cipher-saber-decrypt-string (str key)
  "Cipher Safer decrypt STR with a given KEY."
  (cipher-saber-decrypt str (cipher-saber-make-decryption-state key)))

(defun cipher-saber-decrypt-buffer (key)
  "Cipher Saber decrypt current buffer with KEY."
  (interactive (list (read-from-minibuffer "Key: ")))
  (let* ((p (buffer-substring (point-min) (point-max) (current-buffer)))
	 (c (cipher-saber-decrypt-string p key)))
    (delete-region (point-min) (point-max) (current-buffer))
    (insert c)
    (goto-char (point-min))))

;;; eof (cipher-saber.el)
