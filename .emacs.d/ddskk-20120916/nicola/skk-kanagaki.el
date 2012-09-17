;;; skk-kanagaki.el --- SKK $B$N2>L>F~NO%5%]!<%H(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; {$B$F$C$H$jAa$$;H$$$+$?(B ($B;CDj%P!<%8%g%s(B)}
;;
;; ~/.skk $B$K(B
;;
;; (setq skk-use-kana-keyboard t)
;; (setq skk-kanagaki-keyboard-type '106-jis)
;;
;; $B$H=q$/!#(B
;;
;;
;; {$B@bL@(B}
;;
;; $B$3$N%W%m%0%i%`$O(B SKK $B$K$*$$$F%m!<%^;zF~NO$J$i$L2>L>F~NO$r%5%]!<%H$9$k$3$H(B
;; $B$rL\E*$H$7$^$9!#(B NICOLA $B$d5l(B JIS $BG[Ns$KBP1~$7$^$9!#(B
;;
;; $B$J$*!"0J2<$O!V?F;X%7%U%HF~NO!W0J30$NNc$G$9!#?F;X%7%U%HF~NO$NNc$K$D$$$F$O!"(B
;; README.NICOLA.ja $B$H(B skk-nicola.el $B$r8fMw$/$@$5$$!#(B
;;
;;  -*- $BLdBjE@(B -*-
;;
;; 1. Emacs Lisp $B$N%l%Y%k$G$NLdBj(B
;;
;; $B2>L>F~NO$K$*$$$F$O(B SHIFT $B%-!<$rMxMQ$7$FF~NO$5$l$k2>L>$b$"$k$?$a!"(B SKK $BK\Mh(B
;; $B$N(BSHIFT $B$N;H$$J}$,$G$-$^$;$s!#$=$NB>$$$m$$$m(B SKK $B$i$7$/$J$$$N$G$9$,!"(B $B$H$j(B
;; $B$"$($:!"(B
;;
;;   o $BJQ493+;OE@$N;XDj$O2>L>F~NO$H$OJL$K9T$&!#(B
;;   o $BJQ49$N3+;O$ODL>oDL$j!"(B [SPC] $B$G;X<($9$k!#(B $B$?$@$7!"Aw$j$"$j$NJQ49$N$H$-(B
;;     $B$O(B $BAw$j3+;OE@$r;XDj$9$k$?$a$NFC<l$JA`:n$r$9$k!#(B
;;
;; $B$7$F$"$j$^$9!#Nc$($P!"!V4r$7$$!W$rF~NO$9$k$?$a$K$O(B
;;
;; [fj] $B$&$l$7(B [fj] $B$$(B
;;
;; $B$N$h$&$KF~NO$7$^$9!#(B[fj] $B$H$O(B f $B$H(B j $B$r(B $BF1;~$KBG80$9$k$3$H$G$9!#(B
;;
;; 2. $B%7%9%F%`%l%Y%k$G$NLdBj(B
;;
;; $BBh(B 2 $B$NLdBjE@$H$7$F!"(B $B%-!<G[Ns$N@_Dj$K$h$j9o0uDL$j$NF~NO$,$G$-$J$$>l9g$,$"(B
;; $B$j$^$9!#Nc$($PF|K\8l(B 106 $B%-!<%\!<%I;HMQ;~!"(BXFree86 $B>e$G$O(B
;;
;; o $B!V!o!W%-!<(B ($B2>A[%-!<%3!<%I(B 133)
;; o $B!V!@!W%-!<(B ($B2>A[%-!<%3!<%I(B 123)
;;
;; $B$O$$$:$l$b(B backslash $B$H$7$F07$o$l$^$9!#$7$+$72>L>F~NO$K$*$$$FA0<T$O(B $B!V!<!W!"(B
;; $B8e<T$O!V$m!W(B $B$H$J$k$3$H$,K>$^$l$^$9!#$3$N>l9g$NBP1~:v$H$7$F!"Nc$($P(B
;;
;; % cat >> ~/.Xmodmap
;;
;; keycode 123 = underscore underscore
;; % xmodmap ~/.Xmodmap
;;
;; $B$J$I$H$7$F$*$$$F$+$i!"(B~/.skk $B$K(B
;;
;; (setq skk-kanagaki-rule-list
;;       '(("\\" nil "$B!<(B")))
;;
;; $B$H=q$/$3$H$J$I$,9M$($i$l$^$9!#(B
;; ($BF1MM$N%"%$%G%"$O(B Canna $B$G2>L>F~NO$9$k:]$K$bM-8z$G$"$k$h$&$G$9!#(B)
;;
;; $B$b$7$"$J$?$,(B XEmacs $B$N%Y!<%?%F%9%?!<$J$i$P(B
;;
;; keycode 123 = kana_RO underscore
;; keycode 19 = 0 kana_WO
;;
;; $B$J$s$F@_Dj$G$H$F$b9,$;$K$J$l$k$+$b$7$l$^$;$s!#(B (Mr. XEmacs $B$N$7$o$6$+$J(B?)
;;
;; $B$5$i$K!"$b$7$"$J$?$,(B PC-98 $B%f!<%6(B $B$G(B XEmacs $B$N%Y!<%?%F%9%?!<$J$i$P!"$*$b$`(B
;; $B$m$K!V$+$J!W%-!<$r%m%C%/$7$F$_$F$/$@$5$$!#(B ;')
;;
;;  -*- $B;H$$J}(B -*-
;;
;; 1. $BJQ493+;OE@$N;XDj(B
;;
;; $BDL>o$N(B SKK $B$K$*$$$F$O!"(B SHIFT $B$r2!$7$J$,$iF~NO$9$k$3$H$GJQ493+;O0LCV$rL@<((B
;; $B$7$F$$$^$7$?$,!"2>L>F~NO$G$O$3$l$,$G$-$^$;$s!#$=$3$G!"JL$NJ}K!$GJQ493+;OE@(B
;; $B$r;XDj$7$J$1$l$P$J$j$^$;$s!#$=$3$G!"!V(Bf $B$H(B j $B$rF1;~$K2!$9!W$H$$$&<jK!$r;H$$(B
;; $B$^$9!#0J2<$N(B [fj] $B$O!"F1;~BG80$r0UL#$7$^$9!#(B
;;
;; [fj] $B$O$k(B $B"M(B $B"&$O$k(B [SPC] $B"M(B $B"'=U(B
;;
;; $B$^$?$O(B
;;
;; $B$O$k(B ^B^B [fj] $B"M(B $B"&$O$k(B ^F^F [SPC] $B"M(B $B"'=U(B
;;
;; 2. $BAw$j$"$j$NJQ49$N$7$+$?(B
;;
;; $BDL>o$N(B SKK $B$K$*$$$F$O!"(B SHIFT $B$r2!$7$J$,$iF~NO$9$k$3$H$GAw$j2>L>$N0LCV$rL@(B
;; $B<($7$F$$$^$7$?!#2>L>F~NO(B SKK $B$K$*$$$F$O$=$l$O$G$-$^$;$s!#$=$3$G(B
;;
;; o [fj] $B$,2!$5$l$?$H$-$K!"(B $BD>A0$N(B 1 $BJ8;z$rAw$j2>L>$H8+Jo$7$FJQ49$r3+;O$9$k!#(B
;;
;; $B$H$$$&<jK!$r;H$$$^$9!#(B $BNc$($P!"!VC#$9!W$HF~NO$7$?$$>l9g$O(B
;;
;; $B"&$?$C$9(B [fj]  $B"M(B $B"'C#$9(B
;;
;; $B$N$h$&$K$J$j$^$9!#!VBT$C$F!W$HF~NO$7$?$$>l9g$O(B
;;
;; $B"&$^$C(B [fj] $B"M(B $B"'BT$C(B
;;
;; $B$H$7$F$+$i!V$F!W$rF~NO$7$^$9!#(B
;;
;; 3. $B$$$/$D$+$N=EMW$J%-!<Dj5A$K$D$$$F(B
;;
;; $B%+%JF~NO$,(B $B!V(Bq$B!W!"(B abbrev $B%b!<%I$,(B $B!V(B/$B!W!"(Blatin $B%b!<%I$,(B $B!V(Bl$B!W$J$I$ODjHV$G(B
;; $B$9$,!"2>L>F~NO$G$O$3$l$b;H$($^$;$s!#$=$N$?$a!"$3$l$i$N$&$A=EMW$H;W$o$l$k$b(B
;; $B$N$rJL$N%-!<Dj5A$K$7$F$"$j$^$9!#(BC-h 3 $B$HF~NO$9$k$H!"8=:_$N%b!<%I$K$*$1$kFC(B
;; $B<l$J%-!<Dj5A$r3NG'$G$-$^$9!#(B
;; $B$J$*!"$3$l$i$OF1;~$K%U%!%s%/%7%g%s%-!<$KB`Hr$7$F$"$j$^$9!#(B
;;
;; [f2]  $B!D(B $BJQ493+;OE@$N;XDj(B
;; [f3]  $B!D(B $B@\F,<-$^$?$O@\Hx<-JQ49(B
;; [f5]  $B!D(B $B%3!<%IF~NO(B
;; [f6]  $B!D(B abbrev $B%b!<%I(B
;; [f7]  $B!D(B $B%+%J%b!<%I$^$?$O%+%JJQ49(B
;; [f8]  $B!D(B $BA41Q%b!<%I(B
;; [f9]  $B!D(B $BH>3Q%+%J%b!<%I$^$?$OH>3Q%+%JJQ49(B
;; [f10] $B!D(B latin $B%b!<%I(B
;; [f12] $B!D(B $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B $B$N@Z$jBX$((B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-dcomp)
  (require 'skk-kanagaki-util)
  (require 'skk-macs)
  (require 'skk-vars))

(require 'nicola-ddskk-autoloads)

(when window-system
  (require 'skk-kanagaki-menu))

;; Variables.

(defcustom skk-kanagaki-keyboard-type '106-jis "\
*$B2>L>F~NO$K;HMQ$9$k%-!<%\!<%I$N<oJL!#(B
$BCM$OG$0U$N%7%s%\%k!#(B $B$?$@$7(B `skk-kanagaki-{$B%7%s%\%kL>(B}-base-rule-list' $B$H$$$&(B
$BJQ?t$rMQ0U$7$J$1$l$P$J$i$J$$!#2?$b@_Dj$7$J$1$l$PF|K\8l(B 106 $B%-!<%\!<%IMQ$N@_Dj(B
$B$rMQ0U$7!"$3$l$r;HMQ$9$k!#(B"
  :type '(radio (const 106-jis)
		(const nicola-jis)
		(const nicola-us)
		(const nicola-dvorak)
		(const omelet-jis)
		(const omelet-us)
		(const omelet-dvorak)
		(const oasys)
		(symbol :tag "Another Keyboard Type"))
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-set-henkan-point-key [f2] "\
*$B$3$N%-!<$r2!$9$3$H$GJQ493+;O0LCV$r@_Dj$9$k!#(B
$BJQ493+;O0LCV$N@_Dj$O2>L>$rF~NO$9$kA0$K$*$3$J$C$F$b!"(B $BF~NO$7=*$o$C$?8e$G$*$3$J$C(B
$B$F$b9=$o$J$$!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-abbrev-mode-key [f6] "\
*$B$3$N%-!<$r2!$9$3$H$G(B abbrev $B%b!<%I$KF~$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-katakana-mode-key [f7] "\
*$B$3$N%-!<$r2!$9$3$H$G%+%J%b!<%I$H$+$J%b!<%I$r@Z$j$+$($k!#(B
$BJQ493+;O0LCV$N@_Dj8e$K2!$9$3$H$GBP>]J8;zNs$r%+%J$KJQ49$9$k$3$H$b$G$-$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-jisx0208-mode-key [f8] "\
*$B$3$N%-!<$r2!$9$3$H$GA43Q1Q?t%b!<%I$KF~$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-hankaku-mode-key [f9] "\
*$B$3$N%-!<$r2!$9$3$H$GH>3Q%+%J%b!<%I$K@Z$j$+$($k!#(B
$BJQ493+;O0LCV$N@_Dj8e$K2!$9$3$H$GBP>]J8;zNs$rH>3Q%+%J$KJQ49$9$k$3$H$b$G$-$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-latin-mode-key [f10] "\
*$B$3$N%-!<$r2!$9$3$H$G(B latin $B%b!<%I$KF~$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-toggle-rom-kana-key [f12] "\
*$B$3$N%-!<$r2!$9$3$H$G(B $B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO$N@Z$jBX$($,$G$-$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-code-input-key [f5] "\
*$B$3$N%-!<$r2!$9$3$H$G%3!<%IF~NO$,$G$-$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-midashi-henkan-key [f3] "\
*$B$3$N%-!<$r2!$9$3$H$G@\F,<-$^$?$O@\Hx<-JQ49$r$9$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-help-key "1" "\
*\\[help] $B$K$*$$$F%X%k%W$rI=<($9$k%-!<!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-previous-candidate-key "\C-p"
  "*$BA08uJd$rI=<($9$k$?$a$N%-!<!#(B
XFree86 $B>e$G;HMQ$9$k>l9g!"(B $BNc$($P$3$NCM$r(B [henkan]  (XEmacs $B$G$O(B [henkan-mode])
$B$K$9$l$P!"F|K\8l%-!<%\!<%I$N(B [$BA08uJd(B] $B%-!<$K3d$jEv$F$k$3$H$,$G$-$k!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-start-henkan-key " " "\
*$BJQ49$r3+;O$9$k$?$a$N%-!<!#(B"
  :type (if (get 'key-sequence 'widget-type)
	    'key-sequence
	  'sexp)
  :group 'skk-kanagaki)

(defcustom skk-kanagaki-rule-list
  '((skk-kakutei-key nil skk-kakutei)) "\
*$B%-!<F~NO$KBP$9$kJQ49J8;z$N5,B'$G!";HMQ<T$NDI2C$N@_Dj$r9T$J$&$b$N!#(B
$BNc$($P!"(B $B%-!<G[Ns$rFH<+$K@_Dj$7$F$$$k>l9g$J$I$O!"$3$NJQ?t$rMQ$$$F$=$l$KBP1~$7(B
$B$?@_Dj$r$9$k$3$H$,$G$-$k!#(B"
  :type '(repeat
	  (list :tag "$B%k!<%k%j%9%H(B"
		(radio :tag "1 ($B%-!<F~NO(B)"
		       (string :tag "$BJ8;zNs(B")
		       (symbol :tag "$BJQ?tL>(B"))
		(radio :tag "2 ($BB%2;$N>l9g!VJ8;zNs!W$rA*$S$^$9(B)"
		       (string :tag "$BJ8;zNs(B")
		       (const nil))
		(radio :tag "3 ($B$$$:$l$+$rA*$V(B)"
		       (symbol :tag "$B4X?t(B")
		       (string :tag "$BJ8;zNs(B ($B%+%J(B/$B$+$J6&DL(B)")
		       (cons :tag "$BJ8;zNs$NAH(B ($B%+%J(B . $B$+$J(B)"
			     (string :tag "3-1 ($B%+%J(B)")
			     (string :tag "3-2 ($B$+$J(B)")))))
  :group 'skk-kanagaki)

;; Internal constants and variables.

(defvar skk-kanagaki-base-rule-list nil)
(defvar skk-kanagaki-rule-tree nil)
(defvar skk-kanagaki-rom-kana-rule-tree nil)

(defvar skk-kanagaki-state 'kana)

;; Hooks.

;; Functions.

(unless (fboundp 'help-mode)
  (defalias 'help-mode 'fundamental-mode))

;;;###autoload
(defun skk-kanagaki-midashi-henkan (&optional arg)
  "$B@\F,<-$^$?$O@\Hx<-JQ49$r$9$k!#(B"
  (interactive "*p")
  (cond ((eq skk-henkan-mode 'active)
	 (skk-kakutei)
	 (let (skk-kakutei-history)
	   (skk-set-henkan-point-subr))
	 (insert-and-inherit ?>))
	((eq skk-henkan-mode 'on)
	 ;; $B@\F,8l$N=hM}(B
	 (skk-kana-cleanup 'force)
	 (insert-and-inherit ?>)
	 (skk-set-marker skk-henkan-end-point (point))
	 (setq skk-henkan-count 0
	       skk-henkan-key (buffer-substring-no-properties
			       skk-henkan-start-point
			       (point))
	       skk-prefix "")
	 (skk-henkan))))

;;;###autoload
(defun skk-kanagaki-help ()
  (interactive)
  (skk-kanagaki-help-1
   "* SKK $B2>L>F~NO(B $B%X%k%W(B*"
   "$B8=:_$N2>L>F~NO%b!<%I$N<g$J%-!<Dj5A(B:"
   (append
    '((skk-kanagaki-set-henkan-point-key . "$BJQ493+;OE@$r%;%C%H(B")
      (skk-kanagaki-midashi-henkan-key . "$B@\F,<-(B or $B@\Hx<-JQ49(B")
      (skk-kanagaki-code-input-key . "$B%3!<%IF~NO(B")
      (skk-kanagaki-abbrev-mode-key . "abbrev $B%b!<%I(B")
      (skk-kanagaki-katakana-mode-key . "$B%+%J%b!<%I(B or $B%+%JJQ49(B")
      (skk-kanagaki-latin-jisx0208-mode-key . "$BA41Q%b!<%I(B")
      (skk-kanagaki-hankaku-mode-key . "$BH>3Q%+%J%b!<%I(B or $BH>3Q%+%JJQ49(B")
      (skk-kanagaki-latin-mode-key . "latin $B%b!<%I(B")
      (skk-kanagaki-toggle-rom-kana-key . "$B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B")
      (skk-kanagaki-previous-candidate-key . "$BA08uJdI=<((B")
      (skk-kanagaki-start-henkan-key . "$BJQ49!&<!8uJdI=<((B"))
    (list
     (cons (format "M-x help %s" skk-kanagaki-help-key)
	   "$B$3$N%X%k%W$rI=<((B"))
    ;;
    (list
     (do ((spec (nth 4 skk-kanagaki-rule-tree)
		(cdr spec))
	  (list nil (car spec))
	  (str nil (when (memq
			  (nth 3 list)
			  '(skk-kanagaki-set-okurigana
			    skk-kanagaki-set-okurigana-no-sokuon))
		     (nth 1 list))))
	 ((or str (null spec))
	  (when (stringp str)
	    (cons str "$BAw$j$"$jJQ493+;O(B"))))))))

(defun skk-kanagaki-adjust-rule-tree ()
  (unless skk-kanagaki-rule-tree
    (setq skk-kanagaki-rule-tree
	  (skk-compile-rule-list
	   skk-kanagaki-base-rule-list
	   skk-kanagaki-rule-list)))
  (unless skk-kanagaki-rom-kana-rule-tree
    (setq skk-kanagaki-rom-kana-rule-tree
	  (or skk-rule-tree
	      (skk-compile-rule-list
	       skk-rom-kana-base-rule-list
	       skk-rom-kana-rule-list))))
  (let ((rule
	 (case skk-kanagaki-state
	   (kana
	    skk-kanagaki-rule-tree)
	   (t
	    skk-kanagaki-rom-kana-rule-tree))))
    (setq skk-rule-tree rule)
    (when (skk-local-variable-p 'skk-rule-tree)
      (setq-default skk-rule-tree rule))))

;;;###autoload
(defun skk-kanagaki-insert (&optional arg parg)
  "SPC $B%-!<$@$1$3$l$r(B `skk-insert' $B$NBe$o$j$K;H$&!#(B"
  (interactive "*p")
  (cond
   ((or (integerp parg)
	;; C-u $B$G$O$J$$>l9g(B
	(not (and parg (listp parg))))
    (skk-bind-last-command-char ?\ 
      (skk-insert arg parg)))
   (t
    ;; C-u [SPC] $B$GAw$j$"$jJQ49$r$9$k!#(B
    (when (featurep 'skk-dcomp)
      (skk-dcomp-cleanup-buffer))
    (skk-kanagaki-set-okurigana-no-sokuon t))))

;;;###autoload
(defalias 'skk-kanagaki-set-okurigana 'skk-set-char-before-as-okurigana)

;;;###autoload
(defun skk-kanagaki-set-okurigana-no-sokuon (&optional arg)
  "$B%]%$%s%H$ND>A0$NJ8;z$rAw$j2>L>$H8+Jo$7$F!"JQ49$r3+;O$9$k!#(B"
  (interactive "*p")
  (skk-kanagaki-set-okurigana
   (not (eq 4 (prefix-numeric-value arg)))))

;;;###autoload
(defun skk-kanagaki-initialize ()
  "SKK $B5/F0;~$NE,Ev$J%?%$%_%s%0$G2>L>F~NOMQ$N@_Dj$r9T$&!#(B"
  ;; $B<B:]$K$O(B `skk-regularize' $B$N<B9T8e!"(BSKK $B$N4pK\%k!<%k$,(B compile $B$5$l$?8e(B
  ;; $B$K8F$P$l$k!#(B

  ;; $BI,MW$J%b%8%e!<%k$r%m!<%I!#(B
  (when skk-kanagaki-keyboard-type
    (require (intern
	      (format "skk-%s"
		      skk-kanagaki-keyboard-type))))
  (unless skk-kanagaki-base-rule-list
    (setq skk-kanagaki-base-rule-list
	  (symbol-value (intern
			 (format
			  "skk-kanagaki-%s-base-rule-list"
			  skk-kanagaki-keyboard-type)))))
  ;;
  (add-hook 'skk-mode-hook
	    (function skk-kanagaki-adjust-rule-tree)
	    t)
  ;; $B6gFIE@F~NO;~$NLdBj$r2sHr!#(B $BF|K\8l(B 106 $B%-!<%\!<%I$G$O(B "<" $B$H(B ">" $B$K$h$k@\(B
  ;; $BHx<-$NF~NO$O$G$-$J$/$J$k!#(B "?" $B$K$h$k@\Hx<-$NF~NO$O$G$-$k!#(B
  (dolist (char skk-special-midashi-char-list)
    (when (and skk-use-kana-keyboard
	       (memq (nth 2 (assoc
			     (skk-char-to-unibyte-string char)
			     (symbol-value
			      (intern
			       (format
				"skk-kanagaki-%s-base-rule-list"
				skk-kanagaki-keyboard-type)))))
		     '(skk-current-kuten skk-current-touten)))
      (setq skk-special-midashi-char-list
	    (delq char skk-special-midashi-char-list)))))

;; Pieces of advice.

(defadvice skk-setup-keymap (after skk-kanagaki-keys activate preactivate)
  ;; $B%-!<%P%$%s%I!#$?$@$7$3$l$O!"$h$jE,@Z$J%-!<Dj5A$r8+$D$1$k$^$G$N;CDjE*=hCV!#(B
  ;; $B$3$3$G8@$&!V$h$jE,@Z$J%-!<Dj5A!W$H$O!"F~NOJ}<0$K0MB8$9$k$?$a!"(BSKK $B$N=EMW(B
  ;; $B$J%-!<Dj5A$r%U%!%s%/%7%g%s%-!<$K;D$7$F$*$/$3$H$O!"<BMQ$N$?$a$h$j$b$`$7$m(B
  ;; $B;29M$N$?$a!#(B
  (dolist (cell '((skk-kanagaki-set-henkan-point-key
		   . skk-set-henkan-point-subr)
		  (skk-kanagaki-abbrev-mode-key
		   . skk-abbrev-mode)
		  (skk-kanagaki-katakana-mode-key
		   . skk-toggle-kana)
		  (skk-kanagaki-latin-jisx0208-mode-key
		   . skk-jisx0208-latin-mode)
		  (skk-kanagaki-latin-mode-key
		   . skk-latin-mode)
		  (skk-kanagaki-code-input-key
		   . skk-input-by-code-or-menu)
		  (skk-kanagaki-toggle-rom-kana-key
		   . skk-kanagaki-toggle-rom-kana)
		  (skk-kanagaki-hankaku-mode-key
		   . skk-toggle-katakana)
		  (skk-kanagaki-midashi-henkan-key
		   . skk-kanagaki-midashi-henkan)
		  (skk-kanagaki-previous-candidate-key
		   . skk-previous-candidate)))
    (when (and (symbol-value (car cell))
	       (commandp (cdr cell))
	       (or (eq (car cell) 'skk-kanagaki-previous-candidate-key)
		   (string-match "\\[f[1-9][1-9]\\]"
				 (format "%s" (symbol-value (car cell))))
		   (eq skk-j-mode-function-key-usage 'kanagaki)))
      (define-key skk-j-mode-map (symbol-value (car cell)) (cdr cell))))
  (define-key help-map skk-kanagaki-help-key 'skk-kanagaki-help))

(defadvice skk-insert (around skk-kanagaki-workaround activate compile)
  "$B2>L>F~NOMQ$N(B work around $B!#(B"
  ;;
  (when (and skk-process-okuri-early
	     (eq skk-kanagaki-state 'kana))
    ;; `skk-process-okuri-early' $B$,I{:nMQ$r;}$D$+$bCN$l$J$$!#2>L>F~NO$G$O$=$b(B
    ;; $B$=$b0UL#$N$J$$%*%W%7%g%s$J$N$G6/@)E*$K(B off $B$K$9$k!#(B
    (setq skk-process-okuri-early nil))
  ;;
  (let ((skk-set-henkan-point-key
	 (cond
	  ((and (eq skk-kanagaki-state 'kana)
		(not skk-jisx0201-mode))
	   nil)
	  (t
	   skk-set-henkan-point-key))))
    ad-do-it))

(defadvice skk-compute-henkan-lists-sub-adjust-okuri (around
						      skk-kanagaki-adjust-okuri
						      activate compile)
  (cond
   (skk-use-kana-keyboard
    ;; $B2>L>F~NOMQ$NFC<l=hM}(B
    (let ((item (ad-get-arg 0))
	  (okuri-key (ad-get-arg 1)))
      (setq ad-return-value
	    (cond
	     ((or (and (eq skk-kanagaki-state 'kana)
		       ;; okuri-key $B$,(B "$B$C(B" $B$G(B item $B$,(B "$B$C$F(B" $B$J$I$@$C$?>l9g!#(B
		       (string-match (concat "^" (regexp-quote okuri-key))
				     item))
		  (and (eq skk-kanagaki-state 'rom)
		       ;; okuri-key $B$,(B "$B$C$F(B" $B$G(B item $B$,(B "$B$C(B" $B$J$I$@$C$?>l9g!#(B
		       (string-match (concat "^" (regexp-quote item))
				     okuri-key)))
	      okuri-key)
	     (t
	      item)))))
   (t
    ad-do-it)))

(provide 'skk-kanagaki)

;;; skk-kanagaki.el ends here
