;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-globals.el,v 1.2 1996/12/19 19:39:27 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; Do not change these
;;;

(defconst irchat-client-version-major "2"
  "Major version number. Major functional changes change this number.")

(defconst irchat-client-version-minor "54"
  "Minor version number. Server Protocol changes and bug fixes change this number.")

(defconst irchat-client-version-beta-p t
  "Is this version a beta version?")

(defconst irchat-client-version-alpha-p t
  "Is this version an alpha version?")

(defconst irchat-client-version-release "$Date: 1996/12/19 19:39:27 $"
  "version release date")

(defconst irchat-client-name "Irchat"
  "*Name of this program.")

(defconst irchat-version
  (format "%s %s.%s%s %s" 
	  irchat-client-name 
	  irchat-client-version-major
	  irchat-client-version-minor
	  (cond (irchat-client-version-alpha-p "alpha")
		(irchat-client-version-beta-p  "beta")
		(t ""))
	  (if (string-match ".*: \\([^ ]*\\) .*"
			    irchat-client-version-release)
	      (concat "("
		      (substring irchat-client-version-release 
				 (match-beginning 1)
				 (match-end 1))
		      ")")
	    ""))
  "The version of irchat you are using.")

(defconst irchat-author-nickname "tml"
  "The nickname used by the author of IRCHAT.")


(defvar irchat-emacs-version (string-to-int (substring emacs-version 0 2))
  "*Emacs major version number.")

(defvar irchat-server-process nil)

(defvar irchat-status-message-string nil)

(defun irchat-buffer-disable-undo (buffer)
  "Disabling UNDO"
  (if (fboundp 'buffer-disable-undo)
      (buffer-disable-undo buffer)
    (buffer-flush-undo buffer)))

(defun irchat-get-buffer-window (buffer)
  "Getting the window for a buffer"
  (if (= irchat-emacs-version 19)
      (get-buffer-window buffer t)
    (get-buffer-window buffer)))

(defconst irchat-client-error-msg "Unrecognized command: '%s'"
  "*Error message given to anyone asking wrong CLIENT data.")

(defvar irchat-channel-filter "" 
  "*Enables use of \\[universal-argument] with NAMES and TOPIC.")

(defvar irchat-invited-channel)
(setq irchat-invited-channel nil)

(defvar irchat-old-nickname nil
  "A place to keep old nickname in case it returs thas
we cannot change to new nick")

(defvar irchat-current-channel nil
  "The channel you currently have joined.")

(defvar irchat-current-topic nil
  "The topic of current channel.")

(defvar irchat-current-channels nil
  "The channels you have currently joined.")

(defvar irchat-channel-indicator "No channel"
  "The current channel, \"pretty-printed.\"")

(defvar irchat-private-indicator nil
  "A string displayed in the mode line indicating that user is
currently engaged in a one-to-one conversation.")

(defvar irchat-polling 0
  "T when we are automatically polling the server.")

(defvar irchat-ownfreeze nil
  "If non-nil the Dialogue window will not be scrolled automatically to bring
new entries into view when the user sends messages.")

(defvar irchat-privmsg-partner nil
  "The person who got your last private message.")

(defvar irchat-current-chat-partner nil
  "The person you are in a private conversation with.")

(defvar irchat-current-chat-partners nil
  "An list containing nics user is chatting with.")

(defvar irchat-chat-partner-alist nil
  "An alist containing nics user is chatting with.")

(defvar irchat-command-buffer-mode 'channel
  "symbol chat or channel depending on which is current mode at 
command buffer.")

(defvar irchat-nick-alist nil
  "An alist containing the nicknames of users known to currently be on IRC.
Each element in the list is a list containing a nickname.")

(defvar irchat-channel-alist nil 
  "An alist containing the channels on IRC.  Each element in the list is 
a list containing a channel name.")

(defvar irchat-greet-author nil
  "T until we notice that the author of IRCHAT is present, and send him
a greeting telling what version of IRCHAT this is.")

(defvar irchat-userathost ""
  "The user@host for the current line.")

(defvar irchat-awaymsg ""
  "Current away message.")


(defvar irchat-debug-buffer nil)
(defvar irchat-server-buffer nil)
(defvar irchat-server-name nil)

(defvar irchat-away-indicator "-")
(defvar irchat-freeze-indicator "-")
(defvar irchat-ownfreeze-indicator "-")

(defvar irchat-timestamp-timer nil)
(defvar irchat-names-timer nil)

(defconst irchat-msg 'msg)
(defconst irchat-privmsg 'privmsg)
(defconst irchat-killmsg 'kill)
(defconst irchat-ignoredmsg 'ignore)
(defvar irchat-nick-accepted nil)

(defconst irchat-obarray-size 1327
  "*The size of obarray used by irchat on channelname and username space.
For efficiency this should be prime. See documentation of intern and 
make-vector for more information. Here is a list of some small primes...    

13, 29, 37, 47, 59, 71, 89, 107, 131, 163, 197, 239, 293, 353, 431, 521, 
631, 761, 919, 1103, 1327, 1597, 1931, 2333, 2801, 3371, 4049, 4861, 5839, 
7013, 8419, 10103, 12143, 14591, 17519, 21023, 25229, 30293, 36353, 
43627, 52361, 62851, 75431, 90523, 108631, 130363, 156437, 187751, 
225307, 270371, 324449, 389357, 467237, 560689, 672827, 807403, 968897,
1162687, 1395263, 1674319, 2009191, 2411033, 2893249.")

(defvar irchat-obarray (make-vector irchat-obarray-size nil))

(defvar irchat-ctcp-ping-time (list 0 0 0))


(defvar irchat-query-client-lastcommand nil
  "*Place to keep last entered command")

(defvar irchat-query-client-nick nil
  "*Place to keep last queried nick")

(defvar irchat-query-client-alist
  '(("VERSION") ("CLIENTINFO") ("HELP") ("DCC") ("USERINFO") ("PING")
    ("X-FACE")))

(defconst irchat-query-client-insert-to-generic
  "")

(defconst irchat-query-client-version
  (concat "VERSION" irchat-query-client-insert-to-generic))

(defconst irchat-query-client-userinfo
  (concat "USERINFO" irchat-query-client-insert-to-generic))

(defconst irchat-query-client-help
  (concat "HELP" irchat-query-client-insert-to-generic))

(defconst irchat-query-client-clientinfo
  (concat "CLIENTINFO" irchat-query-client-insert-to-generic))

(defconst irchat-query-client-ping
  (concat "PING" irchat-query-client-insert-to-generic))

(defconst irchat-query-client-x-face
  (concat "X-FACE" irchat-query-client-insert-to-generic))

(provide 'irchat-globals)
;;;
;;;  eof
;;; 
