;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-inlines.el,v 1.2 1997/02/18 12:31:25 too Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;; these must be loaded in the file that requires this since this
;;; won't work otherwise.

(eval-and-compile  (require 'irchat-globals)  (require 'irchat-vars))

(defsubst irchat-scan-channels (chnl)
  (setq irchat-channel-alist 
	(if (assoc chnl irchat-channel-alist)
	    irchat-channel-alist 
	  (cons (list chnl) irchat-channel-alist))))


(defsubst irchat-user-on-this-channel (user chnl)
  "return T if USER is on channel CHNL"
  (let ((u (intern user irchat-obarray)))
    (string-list-ci-memberp chnl (get u 'chnl))))


(defsubst irchat-update-user (chnl user)
  "Add CHNL to list of channels USER belongs to"
  (if (not (string= user ""))
      (let* ((u (if (string= (substring user 0 1) "@")
                    (intern (substring user 1 (length user)) irchat-obarray)
                  (intern user irchat-obarray)))
             (chnls (get u 'chnl)))
        (progn
          (if (get u 'irchat-waited-for)
              (irchat-greet-user user chnl))
          (if (and irchat-greet-author 
                   (string= user irchat-author-nickname))
              (irchat-greet-author))

	  (if (not (assoc (prin1-to-string u) irchat-nick-alist))
	      (setq irchat-nick-alist (cons 
				       (list (prin1-to-string u)) 
				       irchat-nick-alist)))

          (if (not (string-list-ci-memberp chnl chnls))
              (put u 'chnl (nconc chnls (list chnl))))))))


(defsubst irchat-add-to-channel (user chnl)
  "Add users info to his chnl"
  (irchat-update-user chnl user))


(defsubst irchat-remove-from-thischannel (user chnl)
  "Remove users info from his chnl"
  (let ((u (intern user irchat-obarray)))
    (put u 'chnl (string-list-ci-delete chnl (get u 'chnl)))))


(defsubst irchat-update-thischannel (chnl origusers)
  "Update our copy of users on channel chnl."
  (let ((users origusers))
    (while (string-match "^\\([^ ]*\\) \\(.*\\)" users)
      (irchat-update-user chnl (matching-substring users 1))
      (setq users (matching-substring users 2)))
    (irchat-update-user chnl users)))


(defsubst string-list-ci-memberp (thing list)
  "returns t if thing is member of list, not funcallable"
  (let ((item (car list))
	(uthing (upcase thing)))
    (while (and item (not (string-equal uthing (upcase item))))
      (setq item (car list)
	    list (cdr list)))
    item))


(defsubst string-list-memberp (thing list)
  "returns t if thing is member of list, not funcallable"
  (let ((item (car list)))
    (while (and item (not (string-equal thing item)))
      (setq item (car list)
	    list (cdr list)))
    item))


(defsubst string-list-ci-delete (thing list)
  (let ((uthing (upcase thing))
	(item (car list))
	(result nil))
    (while item
      (if (listp item)
	  (if (not (string-equal (upcase (car item)) uthing))
	      (setq result (nconc result (list item))))
	(if (not (string-equal (upcase item) uthing))
	    (setq result (nconc result (list item)))))
      (setq list (cdr list)
	    item (car list)))
    result))


(defsubst string-list-delete (thing list)
  (let ((item (car list))
	(result nil))
    (while item
      (if (listp item)
	  (if (not (string-equal (car item) uthing))
	      (setq result (nconc result (list item))))
	(if (not (string-equal item thing))
	    (setq result (nconc result (list item)))))
      (setq list (cdr list)
	    item (car list)))
    result))


(defsubst list-to-assoclist (list)
  (let ((result nil) (item (car list)))
    (while item
      (setq result (cons (list item) result)
	    list (cdr list)
	    item (car list)))
    result))


(defsubst matching-substring (string arg)
  (substring string (match-beginning arg) (match-end arg)))


(defsubst string-ci-equal (s1 s2)
  (string-equal (upcase s1) (upcase s2)))


(defsubst irchat-next-line (&optional n)
  (if (= (point) (point-max))
      (newline))
  (forward-line n))


(defsubst irchat-Dialogue-insert (msg)
  (save-excursion 
    (irchat-w-insert irchat-D-buffer (format "%s\n" msg))))

(eval-and-compile (provide 'irchat-inlines))

;;;
;;;  eof
;;;
