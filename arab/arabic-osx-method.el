;;; arabic.el --- Quail package for inputting Arabic	-*- coding: utf-8; lexical-binding:t -*-

;; Copyright (C) 2007-2023 Free Software Foundation, Inc.

;; Author: Mahmoud Adam
;; Keywords: mule, input method, Arabic

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'quail)

(quail-define-package
 "arabic-osx" "Arabic-OSX" "ع" nil "Arabic-OSX input method.

Based on Arabic table in Mac OSX.
" nil t t t t nil nil nil nil nil t)

;;  ذّ 1! 2@ 3# 4$ 5% 6^ 7& 8* 9) 0( -_ =+
;;      ضَ صً ثُ قٌ فﻹ غإ ع` ه÷ خ× ح؛ ج< د> <>
;;       شِ سٍ ي] ب[ لﻷ اأ تـ ن، م/ ك: ط"
;;        ئ~ ءْ ؤ} ر{ ﻻﻵ ىآ ة' و, ز. ظ؟
;;

(quail-define-rules
 ("0" "\u0660")
 ("1" "\u0661")
 ("2" "\u0662")
 ("3" "\u0663")
 ("4" "\u0664")
 ("5" "\u0665")
 ("6" "\u0666")
 ("7" "\u0667")
 ("8" "\u0668")
 ("9" "\u0669")
 ("-" ?-)
 ("_" ?_)
 ("=" ?=)
 ("+" ?+)
 ("`" ?ـ)
 ("~" ? )

 ("Q" ?َ)
 ("W" ?ً)
 ("E" ?ِ)
 ("R" ?ٍ)
 ("T" ?ُ)
 ("Y" ?ٌ)
 ("U" ?ْ)
 ("I" ?ّ)
 ("O" ?\])
 ("P" ?\[)
 ("{" ?})
 ("}" ?{)

 ("A" ?»)
 ("S" ?«)
 ("D" ?ى)
 ("F" ? )
 ("G" ? )
 ("H" ?آ)
 ("J" ? )
 ("K" ?٫)
 ("L" ?٬)

 ("Z" ?')
 ;; ("X" ?)
 ("C" ?ئ)
 ("V" ?ء)
 ("B" ?أ)
 ("N" ?إ)
 ("M" ?ؤ)
 ("<" ?>)
 (">" ?<)
 ("?" ?؟)

 ("q" ?ض)
 ("w" ?ص)
 ("e" ?ث)
 ("r" ?ق)
 ("t" ?ف)
 ("y" ?غ)
 ("u" ?ع)
 ("i" ?ه)
 ("o" ?خ)
 ("p" ?ح)
 ("[" ?ج)
 ("]" ?ة)

 ("a" ?ش)
 ("s" ?س)
 ("d" ?ي)
 ("f" ?ب)
 ("g" ?ل)
 ("h" ?ا)
 ("j" ?ت)
 ("k" ?ن)
 ("l" ?م)
 (";" ?ك)
 (":" ?:)
 ("'" ?؛)

 ("z" ?ظ)
 ("x" ?ط)
 ("c" ?ذ)
 ("v" ?د)
 ("b" ?ز)
 ("n" ?ر)
 ("m" ?و)
 ("," ?،)
 ("." ?.)
 ("/" ?/)
 )

(provide 'arabic-osx-method)
;;; arabic-osx-method.el ends here
