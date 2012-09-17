;; skk-ja-names.el --- localization $B$N2DG=@-$K4X$9$k>.$5$J;n$_(B

;; Copyright (C) 2010 SKK Development Team <skk@ring.gr.jp>

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

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

;; 21 $B@$5*$K$J$j!"%"%W%j%1!<%7%g%s$N(B localization $B$O(B $B0lHLE*$J$b$N$K$J$j(B
;; $B$^$7$?$,!"(BGNU Emacs $B$O4pK\E*$K1Q8lCN<1$,A0Ds$H$5$l$F$$$^$9(B ($B%I%-%e%a(B
;; $B%s%H$N0lIt$O?t3X1Q8l$NCN<1$rA0Ds$K$9$i$7$F$$$^$9(B)$B!#(B

;; $B$3$N%U%!%$%k$O(B SKK $B$N%3%^%s%IL>!&%*%W%7%g%sL>$r(B localize $B$9$k$3$H$G(B
;; $B%f!<%6!<$KMxE@$,$"$k$+$I$&$+$r3N$+$a$k$?$a$K@_CV$5$l$^$7$?!#$h$$L?L>(B
;; $BK!$K;j$k$^$G$O>/$J$/$H$b(B experimental $B$H$7$^$9!#(B

;; $B:#$N$H$3$m;W$$$D$/4pK\J}?K(B

;; 1. $B=i?4<T!"<cG/<T!"1Q8l$,6l<j$J5$;}$A$r9M$($k(B

;; 2. $BJXMx$J$b$N!"J,$+$j$d$9$$$b$N$r87A*$9$k(B

;; 3. Emacs $B$K$*$$$F1Q8l0J30$N%7%s%\%kL>$NF~NO$OEvA3<j4V$G$"$k!#$=$&$9$k$H(B
;;    $BJd40A0Ds$G!"Jd40$7$d$9$$L>A0$NIU$1J}$,$h$$$HA[A|$5$l$k!#(B

;;; Code:

(defalias 'SKK$B%b!<%I(B 'skk-mode)
(defalias 'SKK$B$N%P!<%8%g%s(B 'skk-version)
(defalias 'SKK$B$N%P%0$rJs9p$9$k(B 'skk-submit-bug-report)
(defalias 'SKK$B$N%A%e!<%H%j%"%k(B 'skk-tutorial)

(defvaralias 'SKK$B4pK\@_Dj(B-$B8D?M%U%!%$%k$rCV$/%G%#%l%/%H%jL>(B 'skk-user-directory)

(defvaralias 'SKK$B%-!<@_Dj(B-$B%j%?!<%s%-!<$r3NDj$K;H$&(B? 'skk-egg-like-newline)
(defvaralias 'SKK$B%-!<@_Dj(B-$BA08uJdI=<($9$k%-!<72(B 'skk-egg-like-newline)
(defvaralias 'SKK$B%-!<@_Dj(B-$B3NDj$K;H$&%-!<(B 'skk-kakutei-key)

(defvaralias 'SKK$B<-=q@_Dj(B-L$B<-=q$N%U%!%$%kL>(B 'skk-large-jisyo)
(defvaralias 'SKK$B<-=q@_Dj(B-CDB$B<-=q$N%U%!%$%kL>(B 'skk-cdb-large-jisyo)
(defvaralias 'SKK$B<-=q@_Dj(B-$B8D?M<-=q$r6&M-$9$k(B? 'skk-share-private-jisyo)

(defvaralias 'SKK$BJQ49@_Dj(B-$BAw$j2>L>$,@5$7$$8uJd$rM%@h$9$k(B?
  'skk-henkan-strict-okuri-precedence)
(defvaralias 'SKK$BJQ49@_Dj(B-$BM>7W$JAw$j2>L>$N=hM}J}K!$rA*$V(B!
  'skk-check-okurigana-on-touroku)

(defvaralias 'SKK$BI=<(@_Dj(B-$B%$%s%i%$%sI=<($9$k(B? 'skk-show-inline)
(defvaralias 'SKK$BI=<(@_Dj(B-$B%D!<%k%F%#%C%WI=<($9$k(B? 'skk-show-inline)
(defvaralias 'SKK$BI=<(@_Dj(B-$BF|K\8l%a%K%e!<I=<($9$k(B? 'skk-show-japanese-menu)
(defvaralias 'SKK$BI=<(@_Dj(B-$BF|K\8l%a%C%;!<%8$rI=<($9$k(B?
  'skk-japanese-message-and-error)

(defvaralias 'SKK$B3HD%@_Dj(B-$B@hFI$_$7$F$*$/(B? 'skk-preload)
(defvaralias 'SKK$B3HD%@_Dj(B-SKK$B$N%"%$%3%s$rI=<($9$k(B? 'skk-show-japanese-menu)

(provide 'skk-ja-names)

;; skk-ja-names.el ends here

