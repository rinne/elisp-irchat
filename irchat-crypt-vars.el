;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-crypt-vars.el,v 1.2 2009/07/13 20:29:32 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-and-compile  
  (require 'irchat-vars))

(defconst irchat-crypt-support t
  "*Does current version support crypto?")

;;;
;;; user modifiable variables needed for crypto stuff
;;;

;;;
;;;  Format strings for various messages
;;;
(defvar irchat-myformat-string-e ">>"
  "*Format for own messages. (encrypted)")

(defvar irchat-format-string-e "-> **%s**"
  "*Format string for private messages being sent. (encrypted)")

(defvar irchat-format-string0-e "--%s--"
  "*Format string for arriving NOTICE messages. (encrypted)")

(defvar irchat-format-string1-e "**%s**"
  "*Format string for arriving private messages. (encrypted)")

(defvar irchat-format-string2-e "<<%s>>"
  "*Format string for arriving messages to current channel. (encrypted)")

(defvar irchat-format-string3-e "<<%s:%s>>"
  "*Format string for arriving messages to current channel from outside the channel. (encrypted)")

(defvar irchat-format-string4-e "((%s))"
  "*Format string for arriving messages to other channel from outside the channel. (encrypted)")

(defvar irchat-format-string5-e ">>%s:%s<<"
  "*Format string for arriving messages to other channel from outside the channel. (encrypted)")

(defvar irchat-defected-message-prefix ""
  "Prefix to attach before the defected crypt message.")

(defvar irchat-suspicious-message-prefix ""
  "Prefix to attach before the suspicious crypt message.")

;;;
;;;  Buffers
;;;
(defvar irchat-CRYPT-buffer "*IRC CRYPT*")

;;;
;;;  Crypt
;;;
(defvar irchat-crypt-known-keys '() 
  "String list containing encryption keys.  e.g. '(\"foo\" \"bar\")")
(defvar irchat-crypt-default-keys '() 
  "List containing pairs (or lists) of addresses and associated default keys (and possibly key expand version).  e.g '((\"#42\" . \"asd\") (\"#xyz\" . \"zappa\") (\"friend\" \"moebius\" 1))")
(defvar irchat-crypt-timestamp-tolerance 300
  "Allow incoming messages to have N seconds old timestamp.")
(defvar irchat-crypt-ignore-suspicious nil
  "If t, ignore messages with invalid nick or timestamp.")
(defvar irchat-crypt-mode-active t
  "If t, irchat encrypts all messages it has a default key for.")
(defvar irchat-crypt-ignore-defected nil
  "If t, ignore encrypted messages that cannot be opened.")
(defvar irchat-crypt-secure-passphrase-read nil
  "If t, read passphrases with secured keyboard (if possible).")

;;;
;;; Buffer display variables
;;;
(defvar irchat-C-buffer (list irchat-CRYPT-buffer)
  "*A list of buffers where messages that were not decrypted are sent.")

(eval-and-compile
  (provide 'irchat-crypt-vars))
;;;
;;; eof
;;;
