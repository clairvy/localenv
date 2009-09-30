;;; carbon-font.el -- fontsets for Carbon Emacs -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004-2008 by T. Hiromatsu <matsuan@users.sourceforge.jp>
;; Version 1_5_8
;; 2008-02-24

;;; Commentary:

;; This package defines fixed-width multilingual fontsets for Carbon Emacs
;; on Mac OS X. Comments, questions and feedback will be sent to an english
;; list <http://lists.sourceforge.jp/mailman/listinfo/macemacsjp-english>
;; of MacEmacs JP project <http://macemacsjp.sourceforge.jp/en/>.
;;----------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License can be gotten from
;; the Free Software Foundation, Inc.,
;;     59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;     http://www.gnu.org/licenses/gpl.html
;;
;;----------------------------------------------------------------------
;;      $BK\%W%m%0%i%`$O%U%j!<!&%=%U%H%&%'%"$G$9!#(B
;;      $B$"$J$?$O!"(BFree Software Foundation$B$,8xI=$7$?(BGNU $B0lHL8xM-;HMQ5vBz$N(B
;;      $B!V%P!<%8%g%s#2!W0?$$$O$=$l0J9_$N3F%P!<%8%g%s$NCf$+$i$$$:$l$+$rA*Br$7!"(B
;;      $B$=$N%P!<%8%g%s$,Dj$a$k>r9`$K=>$C$FK\%W%m%0%i%`$r(B
;;      $B:FHRI[$^$?$OJQ99$9$k$3$H$,$G$-$^$9!#(B
;;
;;      $BK\%W%m%0%i%`$OM-MQ$H$O;W$$$^$9$,!"HRI[$K$"$?$C$F$O!"(B
;;      $B;T>l@-5Z$SFCDjL\E*E,9g@-$K$D$$$F$N0EL[$NJ]>Z$r4^$a$F!"(B
;;      $B$$$+$J$kJ]>Z$b9T$J$$$^$;$s!#(B
;;      $B>\:Y$K$D$$$F$O(BGNU $B0lHL8xM-;HMQ5vBz=q$r$*FI$_$/$@$5$$!#(B
;;
;;      GNU$B0lHL8xM-;HMQ5vBz$O!"!!(B
;;      Free Software Foundation,
;;         59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
;;         http://www.gnu.org/licenses/gpl.html
;;      $B$+$iF~<j2DG=$G$9!#(B
;;
;;----------------------------------------------------------------------
;; carbon-font.el 2007-07-22$BHG(B;;
;;
;;  1. Introduction
;;  1.1. idea
;;      carbon-font provides font-width-compensation for fixed-width
;;      fontset for Emacs on Mac OSX. The reasons are:
;;          Monaco bold has different width from normal font.
;;          CJK font has different width from ascii font. (We want to use
;;          2 times width for CJK).
;;
;;      Defined fontset names are
;;          osaka = osaka + monaco
;;          hiramaru = $B%R%i%.%N4]%4(B + monaco
;;          hirakaku_w3 = $B%R%i%.%N3Q%4(B w3 + monaco
;;          hirakaku_w6 = $B%R%i%.%N3Q%4(B w6 + monaco
;;          hirakaku_w8 = $B%R%i%.%N3Q%4(B w8 + monaco
;;          hiramin_w3 = $B%R%i%.%NL@D+(B w3 + courier 
;;          hiramin_w6 = $B%R%i%.%NL@D+(B w6 + courier 
;;
;;      Defined sizes are
;;          point 7,8,9,10,12,14,16,18,20,24
;;
;;      then totally 70 fontsets were defined.
;;
;;  1.2. Emacs version
;;      carbon-font supports only CVS version of Emacs after June 1st, 2005.
;;
;;  2. Usage
;;  2.1. Installation
;;      Please put two files in the folder on load-path.
;;          carbon-font.el (this file)
;;          fixed-width-fontset.el
;;
;;  2.2. load package
;;      (if (eq window-system 'mac) (require 'carbon-font))
;;
;;  2.3. set fontset
;;      Use `fixed-width-set-fontset'.
;;          Set fontset and size to `default-frame-alist' and `frame-parameter' of
;;          current frame as `font'. if size is nil, default size of fontset will be used.
;;          To get available fontset, use `fontset-list'.
;;
;;      example:
;;          (fixed-width-set-fontset "hiramaru" 14)
;;
;;  2.4. disable compensation of font width
;;          (setq fixed-width-rescale nil)
;;
;;  3. create your own fontset
;;      If you want to create another fontset, please use  new function
;;          (carbon-font-create-fontset fontset size list)
;;              fontset : fontset name(striings)
;;              size : size or list of size that you want to create
;;              list : alist of encodings and font family name
;;    
;;      example : courier and $B%R%i%.%N4]%4%7%C%/(B(hiragino maru gothic)
;;
;;      (setq carbon-font-encode-family-list-courier
;;        '((ascii . "courier")
;;          (japanese-jisx0208 . "hiragino maru gothic pro")
;;          (katakana-jisx0201 . "hiragino maru gothic pro")
;;          (thai-tis620 . "ayuthaya")
;;          (chinese-gb2312 . "stheiti*")
;;          (chinese-big5-1 . "lihei pro*")
;;          (korean-ksc5601 . "applegothic*")))
;;
;;          (carbon-font-create-fontset "courier"
;;                                      carbon-font-defined-sizes
;;                                      carbon-font-encode-family-list-courier)
;;
;;      Then, you can get new fontsets "fontset-courier", that have sizes
;;      from 7 to 24 point.
;;
;;  4. Supported encodings on Carbon Emacs
;;      `mac-charset-info-alist shows
;;      (("mac-dingbats" 34 nil)
;;       ("adobe-fontspecific" 33 nil)
;;       ("mac-symbol" 33 nil)
;;       ("mac-centraleurroman" 29 mac-centraleurroman)
;;       ("gb2312.1980-0" 25 chinese-iso-8bit)
;;       ("mac-cyrillic" 7 mac-cyrillic)
;;       ("ksc5601.1989-0" 3 korean-iso-8bit)
;;       ("big5-0" 2 chinese-big5)
;;       ("jisx0201.1976-0" 1 japanese-shift-jis)
;;       ("jisx0208.1983-sjis" 1 japanese-shift-jis)
;;       ("mac-roman" 0 mac-roman))
;;
;;       And also "mac-roman" is described 
;;      ;; Create a fontset that uses mac-roman font.  With this fontset,
;;      ;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;;      ;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
;;
;;----------------------------------------------------------------------
;;
;; 1. Introduction
;; 1.1. $B$3$N%U%!%$%k$NCf?H(B
;;	$B$3$N%U%!%$%k$O!"(Bcarbon emacs on Mac OSX $B$G!"(B2$B%P%$%HJ8;z$H!"(Bascii$B$r(B
;;	1:2$B$NI}$G(B($B=j0bEyI}(B)$B$G!"I=<($9$k$?$a$N(Bfontset$BDj5A$NNc$r<($7$F$$$^$9!#(B
;;
;;	$BDj5A$7$F$$$k$N$O!"2<5-$N(B7$B<o$NJ8;z%;%C%H$G$9!#(B
;;          osaka = osaka + monaco
;;          hiramaru = $B%R%i%.%N4]%4(B + monaco
;;          hirakaku_w3 = $B%R%i%.%N3Q%4(B w3 + monaco
;;          hirakaku_w6 = $B%R%i%.%N3Q%4(B w6 + monaco
;;          hirakaku_w8 = $B%R%i%.%N3Q%4(B w8 + monaco
;;          hiramin_w3 = $B%R%i%.%NL@D+(B w3 + courier 
;;          hiramin_w6 = $B%R%i%.%NL@D+(B w6 + courier 
;;
;;	    point 7,8,9,10,12,14,16,18,20,24 $B$N%5%$%:(B
;;
;;	$B$rDj5A$7$F$$$^$9!#$D$^$j!"$3$N%U%!%$%k$G$O!"(B70$B<o$N!"(Bfontset $B$rDj5A(B
;;	$B$7$F$$$k$3$H$K$J$j$^$9!#(B
;;      default$B$N%5%$%:$O!"(B12$B$G$9!#(B
;; 
;; 1.2. $BF0:n4D6-(B
;;	carbon emacs $B$O!"(B2005-06-01 $B0J9_$N(BCVS$B$+$iF~<j$7$?J*$r8f;H$$$/$@$5$$!#(B
;;	$B$=$l0JA0$NJ*$O!"(B.emacs$BFI$_9~$_$N;~$K%(%i!<$K$J$k2DG=@-$,$"$j$^$9!#(B
;;
;;	$B<o!9$JCN7C$r<x$1$F$/$@$5$C$?!"(Bmac-emacsen ML $B$d(B 2ch mac de emacs$B2q5D(B
;;	$B<<$NJ}!9$K46<U$7$^$9!#(B
;;
;; 2. Usage($B;H$$J}(B)
;;  2.1. Install
;;      $B2<5-(B2$B$D$N%U%!%$%k$r%m!<%I%Q%9$NDL$C$?$H$3$m$KCV$$$F2<$5$$!#(B
;;          carbon-font.el (this file)
;;          fixed-width-fontset.el
;;
;;  2.2. $BFI$_9~$_(B
;;      (if (eq window-system 'mac) (require 'carbon-font))
;;      $B$H$7$F$/$@$5$$!#(B
;;
;;  2.3. $B%U%)%s%H%;%C%H$r@_Dj(B
;;      `fixed-width-set-fontset' $B$r;H$$$^$9!#(B
;;          Set fontset and size to `default-frame-alist' and `frame-parameter' of
;;          current frame as `font'. if size is nil, default size of fontset will be used.
;;          To get available fontset, use `fontset-list'.
;;
;;      $BNc(B:
;;          (fixed-width-set-fontset "hiramaru" 14)
;;
;;  2.4. $BEyI}Jd@5$r@Z$j$?$$>l9g(B
;;          (setq fixed-width-rescale nil)
;;      $B$H$7$F$/$@$5$$!#(B
;;
;;  2.5. fixed-width-fontset.0.9.0 $B$H$NI=<(8_49@-(B
;;      (require 'carbon-font) $B$NA0$K!"(B
;;          (setq fixed-width-use-QuickDraw-for-ascii t)
;;      $B$r@_Dj$7$F2<$5$$!#(B
;;
;;      $B$3$l$r$d$k$H!"(Bmonaco $B$b!"(BQuickDraw $B$r;H$&$h$&$K$J$j$^$9!#$=$NBe$o$j!"(B
;;      bold $B$,>/$7DY$l$F$7$^$$$^$9!#9T4V$,5M$^$j(B 0.9.0 $B$HF1MM$NI=<($K$J$j(B
;;      $B$^$9!#(B
;;      default $B$O(B (setq fixed-width-use-QuickDraw-for-ascii nil) $B$G!"(B
;;      monaco $B$O(BATSUI $B$r;H$$$^$9!#(Bbold $B$OEyI}$K$J$C$F!"$9$C$-$j8+$($^$9!#(B
;;      $BKt!"9T4V$,>/$73+$-$^$9!#(B
;;
;;  3. $BJL$N%U%)%s%H%;%C%H$r:n$j$?$$>l9gJL$NAH9g$;$N(B fontset $B$r@_Dj$7$?$$>l(B
;;      $B9g!#(B($B?75!G=(B)
;;
;;      (carbon-font-create-fontset fontset size list) $B$r;H$C$F$/$@$5$$!#(B
;;          fontset : fontset $B$NL>A0(B(striings)
;;          size : $B@_Dj$7$?$$%5%$%:!"Kt$O%5%$%:$N%j%9%H(B
;;          list : $B%(%s%3!<%G%#%s%0$H%U%)%s%H$N%U%!%_%j!<%M!<%`$NO"A[%j%9%H(B
;;         
;;      $BNc$($P!"(Bcourier $B$K(B $B%R%i%.%N4]%4%7%C%/$rAH$_9g$o$;$?$$>l9g(B
;;
;;      (setq carbon-font-encode-family-list-courier
;;        '((ascii . "courier")
;;          (japanese-jisx0208 . "hiragino maru gothic pro")
;;          (katakana-jisx0201 . "hiragino maru gothic pro")
;;          (thai-tis620 . "ayuthaya")
;;          (chinese-gb2312 . "stheiti*")
;;          (chinese-big5-1 . "lihei pro*")
;;          (korean-ksc5601 . "applegothic*")))
;;
;;      $BEy$HDj5A$7$F$*$$$F!"(B    
;;         
;;      (carbon-font-create-fontset "courier"
;;                                  carbon-font-defined-sizes
;;                                  carbon-font-encode-family-list-courier)
;;
;;      $B$rI>2A$9$l$P!"(B7$B!A(B24 $B$^$G$N%5%$%:$N(B fontset $B$,!"(Bfontset-courier $B$H$$$&L>A0$G(B
;;      $BDj5A$5$l$^$9!#(B
;;
;;  4. $B8=:_!"(Bcarbon emacs $B$,!"%5%]!<%H$7$F$$$k%(%s%3!<%G%#%s%0(B
;;      `mac-charset-info-alist shows
;;      (("mac-dingbats" 34 nil)
;;       ("adobe-fontspecific" 33 nil)
;;       ("mac-symbol" 33 nil)
;;       ("mac-centraleurroman" 29 mac-centraleurroman)
;;       ("gb2312.1980-0" 25 chinese-iso-8bit)
;;       ("mac-cyrillic" 7 mac-cyrillic)
;;       ("ksc5601.1989-0" 3 korean-iso-8bit)
;;       ("big5-0" 2 chinese-big5)
;;       ("jisx0201.1976-0" 1 japanese-shift-jis)
;;       ("jisx0208.1983-sjis" 1 japanese-shift-jis)
;;       ("mac-roman" 0 mac-roman))
;;
;;      "mac-roman" $B$O!"2<5-$N$h$&$K(B3$B$D$N%(%s%3!<%G%#%s%0$r4^$s$G$$$^$9!#(B
;;      ;; Create a fontset that uses mac-roman font.  With this fontset,
;;      ;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;;      ;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
;;
;;                                                  T.Hiromatsu
;;                                                  matsuan@users.sourceforge.jp

;;
;; fontset section
;;

(require 'fixed-width-fontset)

(defvar fixed-width-encode-reg-alist
  '((japanese-jisx0208 . "iso10646-*")
    (katakana-jisx0201 . "iso10646-*")
    (japanese-jisx0212 . "iso10646-*")
    (thai-tis620 . "iso10646-*")
    (chinese-gb2312 . "iso10646-*")
    (chinese-big5-1 . "iso10646-*")
    (korean-ksc5601 . "iso10646-*")
    (latin-iso8859-1 . "mac-roman")
    (latin-iso8859-2 . "mac-centraleurroman")
    (cyrillic-iso8859-5 . "mac-cyrillic")))

(defvar fixed-width-use-QuickDraw-for-ascii nil)

(defvar fixed-width-xlfd-template
  (if fixed-width-use-QuickDraw-for-ascii
      "-apple-%s-medium-r-normal--%d-*-*-*-*-*-mac-roman"
    "-apple-%s-medium-r-normal--%d-*-*-*-*-*-iso10646-1"))

(defvar fixed-width-fontset-template "-*-*-medium-r-normal--%d-*-*-*-*-*-fontset-%s")

(defalias 'fixed-width-create-fontset-func 'create-fontset-from-mac-roman-font)

(defalias 'carbon-font-create-fontset 'fixed-width-create-fontset)

;;
;; fontset definition section
;;

(defvar carbon-font-defined-sizes '(12 7 8 9 10 14 16 18 20 24))

;;
;; osaka = osaka + monaco
;;

(defvar carbon-font-encode-family-list-osaka
  '((ascii . "monaco")
    (japanese-jisx0208 . "osaka")
    (katakana-jisx0201 . "osaka")
    (japanese-jisx0212 . "osaka")
    (chinese-gb2312 . "stheiti*")
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "osaka"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-osaka)

;;
;; use Quick Draw
;;

(setcdr (assoc 'japanese-jisx0208 fixed-width-encode-reg-alist) "jisx0208.*")
(setcdr (assoc 'katakana-jisx0201 fixed-width-encode-reg-alist) "jisx0201.*")

;;
;; hiramaru = $B%R%i%.%N4]%4(B + monaco
;;

(defvar carbon-font-encode-family-list-hiramaru
  `((ascii . "monaco")
    (japanese-jisx0208 . "$B%R%i%.%N4]%4(B pro w4")
    (katakana-jisx0201 . "$B%R%i%.%N4]%4(B pro w4")
    (japanese-jisx0212 . "hiragino maru gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . "stheiti*")
    (chinese-big5-1 . ,(if (x-list-fonts "*apple ligothic*")
                           "apple ligothic*" "lihei pro*"))
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hiramaru"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiramaru)

;;
;; hirakaku_w3 = $B%R%i%.%N3Q%4(B w3 + monaco
;;

(defvar carbon-font-encode-family-list-hirakaku_w3
  `((ascii . "monaco")
    (japanese-jisx0208 . "$B%R%i%.%N3Q%4(B pro w3")
    (katakana-jisx0201 . "$B%R%i%.%N3Q%4(B pro w3")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*-hei-*") "hei*" "stheiti*"))
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hirakaku_w3"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hirakaku_w3)

;;
;; hirakaku_w6 = $B%R%i%.%N3Q%4(B w6 + monaco
;;

(defvar carbon-font-encode-family-list-hirakaku_w6
  `((ascii . "monaco")
    (japanese-jisx0208 . "$B%R%i%.%N3Q%4(B pro w6")
    (katakana-jisx0201 . "$B%R%i%.%N3Q%4(B pro w6")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*-hei-*") "hei*" "stheiti*"))
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hirakaku_w6"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hirakaku_w6)

;;
;; hirakaku_w8 = $B%R%i%.%N3Q%4(B w8 + monaco
;;

(defvar carbon-font-encode-family-list-hirakaku_w8
  `((ascii . "monaco")
    (japanese-jisx0208 . "$B%R%i%.%N3Q%4(B std w8")
    (katakana-jisx0201 . "$B%R%i%.%N3Q%4(B std w8")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*-hei-*") "hei*" "stheiti*"))
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hirakaku_w8"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hirakaku_w8)

;;
;; hiramin_w3 = $B%R%i%.%NL@D+(B w3 + courier 
;;

(defvar carbon-font-encode-family-list-hiramin_w3
  `((ascii . "courier")
    (japanese-jisx0208 . "$B%R%i%.%NL@D+(B pro w3")
    (katakana-jisx0201 . "$B%R%i%.%NL@D+(B pro w3")
    (japanese-jisx0212 . "hiragino mincho pro")
    (chinese-gb2312 . ,(if (x-list-fonts "*stkaiti*") "stkaiti*" "stheiti*"))
    (chinese-big5-1 . ,(if (x-list-fonts "*lisong pro*") "lisong pro*" "lihei pro*"))
    (korean-ksc5601 . ,(if (x-list-fonts "*applemyungjo*")
                           "applemyungjo*" "applegothic*"))))

(carbon-font-create-fontset "hiramin_w3"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiramin_w3)

;;
;; hiramin_w6 = $B%R%i%.%NL@D+(B w6 + courier 
;;

(defvar carbon-font-encode-family-list-hiramin_w6
  `((ascii . "courier")
    (japanese-jisx0208 . "$B%R%i%.%NL@D+(B pro w6")
    (katakana-jisx0201 . "$B%R%i%.%NL@D+(B pro w6")
    (japanese-jisx0212 . "hiragino mincho pro")
    (chinese-gb2312 . ,(if (x-list-fonts "*stkaiti*") "stkaiti*" "stheiti*"))
    (chinese-big5-1 . ,(if (x-list-fonts "*lisong pro*") "lisong pro*" "lihei pro*"))
    (korean-ksc5601 . ,(if (x-list-fonts "*applemyungjo*")
                           "applemyungjo*" "applegothic*"))))

(carbon-font-create-fontset "hiramin_w6"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiramin_w6)

;;
;; $BJQ?t(B section
;;

;;  $B;HMQ$9$k%U%)%s%H%;%C%H$rJQ$($?8e!"<+F0$G!"%\!<%k%I$r%j%9%1!<%k$5$;$k!#(B
;;  $B%\!<%k%I$r%j%9%1!<%k$9$k0Y$N%U%!%/%?!<$NDj5A(B

(defvar fixed-width-scale-alist-hiragino
  '(("7" . 1.15) ("8" . 1.35) ("9" . 1.35) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-two-byte-bold
  '(("8" . 1.2) ("9" . 1.25) ("10" . 1.1) ("12" . 1.15)
    ("14" . 1.1) ("16" . 1.2) ("18" . 1.2) ("20" . 1.15) ("24" . 1.15)))

(defvar fixed-width-scale-alist-osaka-normal
  '(("7" . 1.15) ("8" . 1.25) ("9" . 1.35) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-osaka-bold
  '(("7" . 1.15) ("8" . 1.25) ("9" . 1.35) ("10" . 1.25) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-hirakaku-bold
  '(("7" . 1.1) ("8" . 1.2) ("9" . 1.3) ("10" . 1.1) ("12" . 1.1)
    ("14" . 1.1) ("16" . 1.2) ("18" . 1.2) ("20" . 1.15) ("24" . 1.15)))

(defvar fixed-width-scale-alist-hirahan-bold
  '(("7" . 0.8) ("8" . 1.1) ("9" . 1.2) ("10" . 1.0) ("12" . 1.0)
    ("14" . 1.0) ("16" . 1.1) ("18" . 1.1) ("20" . 1.1) ("24" . 1.1)))

(defvar fixed-width-scale-alist-hiramin
  '(("7" . 1.15) ("8" . 1.35) ("9" . 1.2) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-monaco-bold
  '(("7" . 0.8) ("8" . 0.95) ("9" . 0.9) ("10" . 0.8) ("12" . 0.9)
    ("14" . 0.9) ("16" . 0.95) ("18" . 0.9) ("20" . 0.95) ("24" . 0.92)))

(defvar fixed-width-get-scale-alist
  `((".*monaco-bold-.*-mac-roman" . ,fixed-width-scale-alist-monaco-bold)
    (".*monaco cy-bold-.*-mac-cyrillic" . ,fixed-width-scale-alist-monaco-bold)
    (".*courier-bold-.*-mac-roman" . (( "9" . 0.9) ("10" . 0.9)))
    (".*osaka-medium.*" . ,fixed-width-scale-alist-osaka-normal)
    (".*osaka-bold.*" . ,fixed-width-scale-alist-osaka-bold)
    ("^-apple-hiragino.*" . ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*$B%R%i%.%N4]%4(B pro w4.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*$B%R%i%.%N3Q%4(B pro w3-medium.*" 'emacs-mule) .
     ,fixed-width-scale-alist-osaka-normal)
    (,(encode-coding-string ".*$B%R%i%.%N3Q%4(B pro w3-bold.*jisx0208.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hirakaku-bold)
    (,(encode-coding-string ".*$B%R%i%.%N3Q%4(B pro w3-bold.*jisx0201.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hirahan-bold)
    (,(encode-coding-string ".*$B%R%i%.%N3Q%4(B pro w6.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*$B%R%i%.%N3Q%4(B std w8.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*$B%R%i%.%NL@D+(B pro w3.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiramin)
    (,(encode-coding-string ".*$B%R%i%.%NL@D+(B pro w6.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiramin)
    ("^-apple-stheiti-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-lihei pro-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-applegothic-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-applemyungjo-.*" . ,fixed-width-scale-alist-hiramin)
    ("^-apple-lisong pro-.*" . ,fixed-width-scale-alist-hiramin)
    ("^-apple-stkaiti-.*" . ,fixed-width-scale-alist-hiramin)
    ("^-apple-hei-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-apple ligothic-.*" . ,fixed-width-scale-alist-hiragino))
  "ReScale factor alist for each fonts and size.")

(provide 'carbon-font)

;;; carbon-font.el ends here
