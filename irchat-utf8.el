;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-utf8.el,v 3.7 2009/07/14 01:12:47 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; UTF-8 kludge implementation by tri@iki.fi.
;;;

(eval-and-compile
  (require 'irchat-utf8-table))

(defun irchat-utf8-kludge-nth-char (str n)
  "Get Nth character from STR as integer or nil if not in range."
  (if (and (stringp str)
	   (>= n 0)
	   (> (length str) n))
      ; For reason that escapes from me, gnu emacs 22 seems to do
      ; weird things for latin-1 characters in strings.  Function
      ; elt returns those codes 128-255 to be in range 2176-2303
      ; (i.e. 0x8## instead of 0x##).  This page in UTF-8 is unused
      ; but reserved, so let's just handle those specially.  This
      ; will certainly break big time if page 8 is sometime used.
      (let ((c (+ 0 (elt str n))))
	(if (and (>= c 2176) (<= c 2303))
	    (% c 256)
	  c))
    nil))

(defun irchat-utf8-kludge-decode-first (str)
  "Decode the first character from STR as utf-8 if possible and return
as (code . rest) pair containing the character code of the first
character and the rest of the string"
  (let ((b1 (irchat-utf8-kludge-nth-char str 0))
	(b2 (irchat-utf8-kludge-nth-char str 1))
	(b3 (irchat-utf8-kludge-nth-char str 2))
	(b4 (irchat-utf8-kludge-nth-char str 3)))
    (cond ((or (null b1)
	       (= b1 0))
	   ; This is empty string or string that contains NIL characters
	   ; and it simply means that caller is out of luck
	   nil)
	  ((< b1 128)
	   ; This is ASCII
	   (cons b1 (substring str 1)))
	  ((and (>= b1 192)
		(< b1 224)
		(not (null b2))
		(>= b2 128)
		(< b2 192))
	   ; This is 2 byte encoded UTF-8 character
	   (cons (+ (* 64 (- b1 192))
		    (- b2 128))
		 (substring str 2)))
	  ((and (>= b1 224)
		(< b1 240)
		(not (null b2))
		(>= b2 128)
		(< b2 192)
		(not (null b3))
		(>= b3 128)
		(< b3 192))
	   ; This is 3 byte encoded UTF-8 character
	   (cons (+ (* 64 64 (- b1 224))
		    (* 64 (- b2 128))
		    (- b3 128))
		 (substring str 3)))
	  ((and (>= b1 240)
		(< b1 248)
		(not (null b2))
		(>= b2 128)
		(< b2 192)
		(not (null b3))
		(>= b3 128)
		(< b3 192)
		(not (null b4))
		(>= b4 128)
		(< b4 192))
	   ; This is 4 byte encoded UTF-8 character
	   (cons (+ (* 64 64 64 (- b1 224))
		    (* 64 64 (- b2 128))
		    (* 64 (- b3 128))
		    (- b4 128))
		 (substring str 4)))
	  (t
	   ; This is NOT UTF-8 encoding at all and is returned as is
	   (cons b1 (substring str 1))))))

(defun irchat-utf8-kludge-visible-char (ch)
  "Convert a character code into a visible string."
  (let ((x))
    (cond ((< ch 128)
	   (char-to-string ch))
	  ((setq x (assq ch irchat-utf8-kludge-assoc-list))
	   (cdr x))
	  ((< ch 256)
	   (char-to-string ch))
	  ((setq x (irchat-utf8-kludge-code-range ch))
	   (format "[U+%04x '%s']" ch x))
	  (t
	   (format "[U+%04x]" ch)))))

(defun irchat-utf8-kludge-decode (str)
  "Decode STR as utf-8 opportunisticly."
  (let ((r "")
	(e nil))
    (while (setq e (irchat-utf8-kludge-decode-first str))
	(setq r (concat r (irchat-utf8-kludge-visible-char (car e))))
	(setq str (cdr e)))
    r))

(defun irchat-utf8-kludge-code-range (n)
  (cond ((and (>= n 0) (<= n 127)) "Basic Latin")
	((and (>= n 128) (<= n 255)) "Latin-1 Supplement")
	((and (>= n 256) (<= n 383)) "Latin Extended A")
	((and (>= n 384) (<= n 591)) "Latin Extended B")
	((and (>= n 592) (<= n 687)) "IPA Extension")
	((and (>= n 688) (<= n 767)) "Spacing Modifier Letter")
	((and (>= n 768) (<= n 879)) "Combining Diacritical Mark")
	((and (>= n 880) (<= n 1023)) "Greek")
	((and (>= n 1024) (<= n 1279)) "Cyrillic")
	((and (>= n 1328) (<= n 1423)) "Armenian")
	((and (>= n 1424) (<= n 1535)) "Hebrew")
	((and (>= n 1536) (<= n 1791)) "Arabic")
	((and (>= n 1792) (<= n 1871)) "Syriac")
	((and (>= n 1920) (<= n 1983)) "Thaana")
	((and (>= n 2304) (<= n 2431)) "Devanagari")
	((and (>= n 2432) (<= n 2559)) "Bengali")
	((and (>= n 2560) (<= n 2687)) "Gurmukhi")
	((and (>= n 2688) (<= n 2815)) "Gujarati")
	((and (>= n 2816) (<= n 2943)) "Oriya")
	((and (>= n 2944) (<= n 3071)) "Tamil")
	((and (>= n 3072) (<= n 3199)) "Telugu")
	((and (>= n 3200) (<= n 3327)) "Kannada")
	((and (>= n 3328) (<= n 3455)) "Malayalam")
	((and (>= n 3456) (<= n 3583)) "Sinhala")
	((and (>= n 3584) (<= n 3711)) "Thai")
	((and (>= n 3712) (<= n 3839)) "Lao")
	((and (>= n 3840) (<= n 4095)) "Tibetan")
	((and (>= n 4096) (<= n 4255)) "Myanmar")
	((and (>= n 4256) (<= n 4351)) "Georgian")
	((and (>= n 4352) (<= n 4607)) "Hangul Jamo")
	((and (>= n 4608) (<= n 4991)) "Ethiopic")
	((and (>= n 5024) (<= n 5119)) "Cherokee")
	((and (>= n 5120) (<= n 5759)) "Unified Canadian Aboriginal Syllabic")
	((and (>= n 5760) (<= n 5791)) "Ogham")
	((and (>= n 5792) (<= n 5887)) "Runic")
	((and (>= n 6016) (<= n 6143)) "Khmer")
	((and (>= n 6144) (<= n 6319)) "Mongolian")
	((and (>= n 7680) (<= n 7935)) "Latin Extended Additional")
	((and (>= n 7936) (<= n 8191)) "Greek Extended")
	((and (>= n 8192) (<= n 8303)) "General Punctuation")
	((and (>= n 8304) (<= n 8351)) "Superscript or Subscript")
	((and (>= n 8352) (<= n 8399)) "Currency Symbol")
	((and (>= n 8400) (<= n 8447)) "Combining Mark for Symbol")
	((and (>= n 8448) (<= n 8527)) "Letterlike Symbol")
	((and (>= n 8528) (<= n 8591)) "Number Form")
	((and (>= n 8592) (<= n 8703)) "Arrow")
	((and (>= n 8704) (<= n 8959)) "Mathematical Operator")
	((and (>= n 8960) (<= n 9215)) "Miscellaneous Technical")
	((and (>= n 9216) (<= n 9279)) "Control Picture")
	((and (>= n 9280) (<= n 9311)) "Optical Character Recognition")
	((and (>= n 9312) (<= n 9471)) "Enclosed Alphanumeric")
	((and (>= n 9472) (<= n 9599)) "Box Drawing")
	((and (>= n 9600) (<= n 9631)) "Block Element")
	((and (>= n 9632) (<= n 9727)) "Geometric Shape")
	((and (>= n 9728) (<= n 9983)) "Miscellaneous Symbol")
	((and (>= n 9984) (<= n 10175)) "Dingbat")
	((and (>= n 10240) (<= n 10495)) "Braille Pattern")
	((and (>= n 11904) (<= n 12031)) "CJK Radical Supplement")
	((and (>= n 12032) (<= n 12255)) "Kangxi Radical")
	((and (>= n 12272) (<= n 12287)) "Ideographic Description Character")
	((and (>= n 12288) (<= n 12351)) "CJK Symbol or Punctuation")
	((and (>= n 12352) (<= n 12447)) "Hiragana")
	((and (>= n 12448) (<= n 12543)) "Katakana")
	((and (>= n 12544) (<= n 12591)) "Bopomofo")
	((and (>= n 12592) (<= n 12687)) "Hangul Compatibility Jamo")
	((and (>= n 12688) (<= n 12703)) "Kanbun")
	((and (>= n 12704) (<= n 12735)) "Bopomofo Extended")
	((and (>= n 12800) (<= n 13055)) "Enclosed CJK Letter or Month")
	((and (>= n 13056) (<= n 13311)) "CJK Compatibility")
	((and (>= n 13312) (<= n 19893)) "CJK Unified Ideograph Extension A")
	((and (>= n 19968) (<= n 40959)) "CJK Unified Ideograph")
	((and (>= n 40960) (<= n 42127)) "Yi Syllable")
	((and (>= n 42128) (<= n 42191)) "Yi Radical")
	((and (>= n 44032) (<= n 55203)) "Hangul Syllable")
	((and (>= n 55296) (<= n 56191)) "High Surrogate")
	((and (>= n 56192) (<= n 56319)) "High Private Use Surrogate")
	((and (>= n 56320) (<= n 57343)) "Low Surrogate")
	((and (>= n 57344) (<= n 63743)) "Private Use")
	((and (>= n 63744) (<= n 64255)) "CJK Compatibility Ideograph")
	((and (>= n 64256) (<= n 64335)) "Alphabetic Presentation Form")
	((and (>= n 64336) (<= n 65023)) "Arabic Presentation Form A")
	((and (>= n 65056) (<= n 65071)) "Combining Half Mark")
	((and (>= n 65072) (<= n 65103)) "CJK Compatibility Form")
	((and (>= n 65104) (<= n 65135)) "Small Form Variant")
	((and (>= n 65136) (<= n 65278)) "Arabic Presentation Forms B")
	((and (>= n 65279) (<= n 65279)) "Special")
	((and (>= n 65280) (<= n 65519)) "Halfwidth or Fullwidth Form")
	((and (>= n 65520) (<= n 65533)) "Special")
	(t nil)))

(eval-and-compile
  (provide 'irchat-utf8))
;;;
;;; eof
;;;