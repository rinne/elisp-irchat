;;;  -*- emacs-lisp -*-
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; user modifiable variables
;;;

(eval-and-compile
  (require 'irchat-globals))

(defvar irchat-saved-forms 
  (purecopy '(irchat-ignore-nickname irchat-ignore-message-regexp))
  "*Variables whose values are saved via command irchat-Command-save-vars")

(defvar irchat-command-window-height 4
  "*How large should Command window be on startup.")

(defvar irchat-use-full-window t
  "*If non-nil, IRCHAT will use whole emacs window. Annoying for GNUS-
users, therefore added by nam.")

(defvar irchat-want-traditional nil
  "*Do we want /commands.")

(defvar irchat-command-window-on-top nil
  "*If non-nil, the Command window will be put at the top of the screen.")

(defvar irchat-one-buffer-mode nil
  "*When non-nil, irchat will put up only a dialogue-buffer (on the
screen). Useful for those (perverts) who use 24 line terminals.")

(defvar irchat-variables-file "~/.irchat_vars.el"
  "*Where to look for variables. Helps to remove clutter from your .emacs.
This feature is most likely to dissappear in near future. The preferred 
way is to put irchat variables on .emacs or file loaded from there.")

(defvar irchat-utf8-kludge-disable nil
  "*If non-nil, IRCHAT will not try to make sense of UTF-8 input.")

(defvar irchat-utf8-kludge-send-utf8-as-default nil
  "*If non-nil, IRCHAT send UTF-8 encode sent messages as default.")

(defvar irchat-backup-irchat-vars nil
  "*Do we want to backup irchat-vars file when updating it.  Nil inhibits.")

;;;
;;;  Where to connect
;;;
(defvar irchat-server (or (getenv "IRCSERVER") "ircd.eunet.fi")
  "*Name of the host running the IRC server. 
Value initialized from the IRCSERVER environment variable if one is set")

(defvar irchat-service 
  (let ((ircport-env (getenv "IRCPORT")))
    (if ircport-env
	(if (> (string-to-int ircport-env) 0)
	    (string-to-int ircport-env)
	  ircport-env)
      6667))
  "*IRC service name or (port) number.")

(defvar irchat-nickname (or (getenv "IRCNICK") (user-real-login-name))
  "*The nickname you want to use in IRC.
Default is the environment variable IRCNICK, or your login name.")

(defvar irchat-pass nil
  "*Password to be sent to the server in connection open.")

(defvar irchat-autoremove-virtual-penis t
  "*If t, virtual penis `_' is removed from nickname on reconnect")

(defvar irchat-real-nickname nil
  "Internal IRCHAT nickname.")

(defvar irchat-startup-channel nil
  "*The channel to join automatically at startup.
If nil, do not join any channel.")

(defvar irchat-reconnect-automagic t
  "*Automatic reconnection, default is disabled")

(defvar irchat-ask-for-nickname t
  "*Ask for nickname if irchat was entered with \\[universal-argument].")

(defvar irchat-grow-tail "_"
  "*Add irchat-grow-tail to nick when reconnecting. Otherwise you might get
killed again if automagic reconnect is too fast.")

(defvar irchat-message-length-limit 256
  "*Messages longer than this are split to pieces")

(defvar irchat-message-split-separator "..."
  "String to used as separator in splitted messages.")

(defvar irchat-message-alt-split-separator "��"
  "String to used as separator in splitted messages.")

(defvar irchat-message-split-^C-compat t
  "Also recognize ^C as split separator?")

(defvar irchat-default-kick-message nil
  "String to use as message in channel KICK commands.")

(defvar irchat-ask-kick-message t
  "If non-nil, the KICK message is asked from user with every kick.")

;;;
;;;  Hooks, each message type has its own hook. These are used to setup
;;;  variables, buffers &c
;;;
(defvar irchat-Command-mode-hook nil
  "*A hook for IRCHAT Command mode.")

(defvar irchat-Dialogue-mode-hook nil
  "*A hook for IRCHAT Dialogue mode.")

(defvar irchat-Exit-hook nil
  "*A hook executed when signing off IRC.")

;;;
;;;  MULE coding systems
;;;
(defvar irchat-send-coding-system 'iso-8859-1
  "String coding system for sending.")

(defvar irchat-receive-coding-system 'iso-8859-1
   "String coding system for reading.")

;;;
;;;  DCC
;;;
(defvar irchat-dcc-program
;;; set dcc program as "dcc", "dcc.perl" or nil, depending which binary found.
;;; current version takes the one which is earlier in path (if many).
  (let* ((result nil) 
	 (list exec-path)
	 (path (car list)))
    (while (and path (not result))
      (setq path (file-name-as-directory path))
      (if (file-executable-p (concat path "dcc")) (setq result "dcc")
	(if (file-executable-p (concat path "dcc.perl")) 
	    (setq result "dcc.perl")))
      (setq list (cdr list)
	    path (car list)))
    result)
  "*Name of the external dcc-program.")

(defvar irchat-dcc-directory "~/tmp" 
  "*Directory where irchat-dcc puts its files.")

(defvar irchat-dcc-port 1200 
  "*Default port for DCC operations.")

;;;
;;;  Format strings for various messages
;;;
(defvar irchat-myformat-string ">"
  "*Format for own messages.")

(defvar irchat-format-string "-> *%s*"
  "*Format string for private messages being sent.")

(defvar irchat-format-string0 "-%s-"
  "*Format string for arriving NOTICE messages.")

(defvar irchat-format-string1 "*%s*"
  "*Format string for arriving private messages.")

(defvar irchat-format-string2 "<%s>"
  "*Format string for arriving messages to current channel.")

(defvar irchat-format-string3 "<%s:%s>"
  "*Format string for arriving messages to current channel from outside the channel.")

(defvar irchat-format-string4 "(%s)"
  "*Format string for arriving messages to other channel from outside the channel.")

(defvar irchat-format-string5 ">%s:%s<"
  "*Format string for arriving messages to other channel from outside the channel.")

(defvar irchat-change-prefix "*** Change: "
  "*String to add before any change msg, used for customisation of
IRCHAT to suit old users of the irc-loser-client.")

(defvar irchat-notice-prefix "*** Notice: "
  "*String to add before any notice message.")

(defvar irchat-broadcast-prefix "*** Broadcast: "
  "*String to add before any Broadcast message")

(defvar irchat-wallops-prefix "*** Notice: "
  "*String to add before any WALLOPS message")

(defvar irchat-error-prefix "*** Error: "
  "*String to add before any ERROR message")

(defvar irchat-info-prefix "*** Info: "
  "*String to add before any informational message")

;;;
;;;  Buffers
;;;
(defvar irchat-Command-buffer "*IRC Commands*"
  "*Name of Command input buffer")
(defvar irchat-Dialogue-buffer "*IRC Dialogue*"
  "*Name of Dialogue output buffer")
(defvar irchat-Private-buffer "*IRC Private*"
  "*Name of Private message buffer*")

(defvar irchat-KILLS-buffer "*IRC KILLS*")
(defvar irchat-IGNORED-buffer "*IRC IGNORED*")
(defvar irchat-WALLOPS-buffer "*IRC WALLOPS*")

;;;
;;;  Misc
;;;
(defvar irchat-send-delayed-allow-duplicates nil
  "*Allow duplicate strings to be queued in send-delayed?")

(defvar irchat-send-delayed-interval 3
  "Seconds between delayed sends.")

(defvar irchat-use-delayed-privmsg nil
  "*Should privmsg messages be delayed too?")

(defvar irchat-blink-parens nil
  "*Should we blink matching parenthesis in irchat command buffer?")

(defvar irchat-show-wallops t
  "*Show wallops messages if usermode +ws")

(defvar irchat-ignore-extra-notices t
  "*Don't show NOTICEs with \"as being away\" unless they come from
the local server.")

(defvar irchat-ignore-changes-from-ignored nil
  "*Don't show changes from ignored users.")

(defvar irchat-timestamp-irc-I-buffer-p nil
  "*Timestamps lines inserted into *IRC IGNORED* buffer.")

(defvar irchat-shorten-kills t
  "*Shorten KILL messages to about one line.")

(defvar irchat-ignore-changes nil
  "*Ignore changes? Good in topic-wars/link troubles.")

(defvar irchat-ignore-fakes nil
  "*If non nil, ignore fake notices if receiving them.")

(defvar irchat-signoff-msg ""
  "*Default signoff message")

(defvar irchat-confirm-bell-on-channel-message t
  "*If non-nil, confirmation is asked before bell is sent to the channel.")

(defvar irchat-beep-on-bells 'always
  "*If non-nil, and the IRC Dialogue buffer is not selected in a window,
an IRC message arriving containing a bell character, will cause you
to be notified.
If value is 'always, an arriving bell will always cause a beep (or flash).")

(defvar irchat-system-fqdname (system-name)
  "*The fully qualified domain name of the system.
Default is what (system-name) returns.")

(defvar irchat-use-smiley nil
  "*Display smileys in dialogue buffer.")

(defvar irchat-use-ancient-version-format nil
  "*Use ancient ultra-long and ultra-ugly version query reply.")

;;;
;;; Global-kill system
;;;
(defvar irchat-global-kill-valid-senders '()
  "*List of strings or string-int pairs containing valid IGNORE senders.")

(defvar irchat-global-kill-use-uah-p nil
  "*Use user-at-host in ignore command if possible")

(defvar irchat-global-kill-use-silent-ignore nil
  "*If non-nil, no expire messages are shown.")

(defvar irchat-global-kill-default-timeout-limit 99
  "*Default timeout limit for valid senders.")

(defvar irchat-global-kill-report-ignored nil
  "*How to report ignored kill request. (nil, t, 'minibuffer or 'dialogue)")

;;;
;;;  Send/Receive files
;;;
(defvar irchat-file-accept nil
  "*Do we accept files.")

(defvar irchat-file-confirm-save nil
  "*Do we want confirmation on saving files.")

;;;
;;;  Userinfos
;;;
(defvar irchat-client-userinfo "No user information given."
  "*Userinfo message given to anyone asking.")

(defvar irchat-client-x-face ""
  "*X-Face message given to anyone asking.  Proper form is \"X-Face: ....\".")

(defvar irchat-keepalive-interval nil
  "*Interval on seconds the existence of server connection is checked")

(defvar irchat-pollnames-interval nil
  "*Interval the names are polled from server")

(defvar irchat-timestamp-interval (* 60 10)
  "*Interval in seconds between timestamps in dialogue-buffer, nil for none.")

(defvar irchat-timestamp-format "*** Time: %s"
  "*Format-string for timestamp.")

;;;
;;; External version identification.
;;;
(defvar irchat-external-version-string nil
  "Version id given to VERSION queries. nil is default. 'none inhibits reply.")

;;;
;;;  Conversions: Not used.
;;;
(defvar irchat-send-convert-list nil 
  "*Convert characters before sending to server." )

(defvar irchat-receive-convert-list nil
  "*Convert characters after receiving from server." )

;;;
;;;  Ignores
;;;
(defvar irchat-ignore-nickname nil
  "*A list of nicknames, as symbols, to ignore.  Messages from these people
won't be displayed.")

(defvar irchat-ignore-message-regexp nil
  "*A list of regexps to ignore.  Messages matching to them 
won't be displayed.")

;(defvar irchat-ignore-realname nil
;  "*A list of real names of people to ignore. Messages from them
;won't be displayed.")

;(defvar irchat-ignore-logon nil
;  "*A list of logon names (user@host.dom.ain). Messages from them
;won't be displayed.")

;;;
;;; New Ignore
;;;
(defvar irchat-no-ignore-buffers nil
  "*A list of buffers that don't adhere to ignore-list.")

(defvar irchat-ignore-list nil
  "*A list of lists that contain ignore-info.
It is list of lists that contain two elements,
first element may be two item list of
(match-for-messagetype match-for-string-in-message)
match-for-messagetype may be nil,
match-for-string-in-message may be nil,
or
name of function that returns this list, the function takes
one argument, the string we are asked to ignore.
The other argument is
(list-of-users) 
list-of-users may be nil (in case it does not match),
or 
name of function returning that returns similar list, the
function takes one argument, the string we are asked to ignore.

(setq irchat-ignore-list
      (list
       (list (list \"msg\" nil) (list \"!user\" \"@host\"))
       (list (list \"privmsg\" nil) 'irchat-own-ignore-who)
       (list 'irchat-own-ignore-check (list \"com\"))
       (list (list nil \"fuck\") (list \".*\"))
       )
      )
The previous ignores normal msg`s (not used so much anymore)
from username matching 'user' or from host 'host'.
Then any privmsg from list that irchat-own-ignore-who return.
Then if the user is from .com (or has com somewhere in name
and the irchat-own-ignore-check returns message-type and/or string.
The last ignores all messages that contain the word `fuck`.
")

;;;
;;; Buffer display variables
;;;
(defvar irchat-buffer-list nil
  "*A list of buffers used in displaying messages.")

(defvar irchat-D-buffer (list irchat-Dialogue-buffer)
  "*A list of buffer where normal Dialogue is sent.")
(defvar irchat-P-buffer (list irchat-Dialogue-buffer irchat-Private-buffer)
  "*A list of buffers where private messages to me are sent.")
(defvar irchat-I-buffer (list irchat-IGNORED-buffer)
  "*A list of buffers where private messages to me are sent.")
(defvar irchat-W-buffer (list irchat-WALLOPS-buffer)
  "*A list of buffers where WALLOPS messages to me are sent.")
(defvar irchat-K-buffer (list irchat-KILLS-buffer)
  "*A list of buffers where KILL messages to me are sent.")
(defvar irchat-000-buffer (list irchat-Dialogue-buffer)
  "*A list of buffers where 000 messages to me are sent.")
(defvar irchat-200-buffer (list irchat-Dialogue-buffer)
  "*A list of buffers where 200 messages to me are sent.")
(defvar irchat-300-buffer (list irchat-Dialogue-buffer)
  "*A list of buffers where 300 messages to me are sent.")
(defvar irchat-400-buffer (list irchat-Dialogue-buffer)
  "*A list of buffers where 400 messages to me are sent.")
(defvar irchat-500-buffer (list irchat-Dialogue-buffer)
  "*A list of buffers where 500 messages to me are sent.")

(defvar irchat-debugmsg nil
  "*String to add in front of debug messages.")

(defvar irchat-compress-changes t
  "*Set to t if we want instant compressed messages in the old format.")
(defvar irchat-compress-treshold 1
  "*Number of lines to search back for a previous matching join/part/quit/mode.")

(defvar irchat-buffer-maxsize 4242424
  "*Maximum size (in bytes) of any irchat buffer.")
(defvar irchat-buffer-defsize 4042424
  "*Size to shrink buffer if it grows too big.")
(defvar irchat-checkbuffer-interval (* 60 10)
  "*Interval between buffer-size checks.")

(defvar irchat-private-window-height 4
  "*How tall is the window for private messages when shown.")

(defvar irchat-format-time-function (function irchat-compose-servertimestring)
  "*Function to convert result of (current-time-string) to human readable 
form. If you like to have the short format, set this to nil or to a 
funtion (defun my-format (s) s).")

(defvar irchat-keep-buffers t
  "*When starting IRC process, instructs whether to erase contents of old
buffers if NIL or to keep them if T.")

(defvar irchat-ownfreeze nil
  "*Set this to t if you don't want your own messages to scroll the
Dialogue window.")

(defvar irchat-scroll-step 1
  "*Set this to the number of lines you want to have empty when irchat scrolls. If nil, scrolls half a page.");

(defvar irchat-swap-private nil
  "*When showing private is it between under Dialogue or not.")

(defvar irchat-buffer-preferences
  (list 
   (list ".*" irchat-D-buffer))

  "*List of lists that contain regexp to match and buffer-list to
insert. Setting this as 
(list 
 (list \"#report\" (list \"*report-buffer*\")) 
 (list \".*\" irchat-D-buffer))
would cause messages from and to channel #report to be displayed on
buffer named *report-buffer* and all other messages are displayed on
Dialogue-buffer.")

(eval-and-compile
  (provide 'irchat-vars))
;;;
;;; eof
;;;
