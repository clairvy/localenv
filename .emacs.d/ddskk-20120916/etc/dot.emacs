;;; dot.emacs --- SKK related customization in ~/.emacs  -*- mode: emacs-lisp; coding: iso-2022-jp -*-

;;; Commentary:

;; ~/.emacs $B$KDI2C$9$k$?$a$N@_DjNc$G$9!#(B

;;; $BCm0U(B:

;; SKK $B$N@_Dj$O!"(B~/.skk $B$NJ}$,M%@h$5$l$^$9!#0J2<$O!"FC<l$J;v>p$G(B
;; ~/.skk $B$G$O$&$^$/5!G=$7$J$$@_Dj$r=8$a$F$$$^$9!#$3$l$i0J30$O(B
;; ~/.skk $B$G@_Dj$9$k$3$H$r$*4+$a$7$^$9!#(B

;;; Code:

;; @@ $B4pK\$N@_Dj(B

;; $B%+%?%+%J(B/$B$R$i$,$J(B $B%-!<$G(B SKK $B$r5/F0$9$k(B
(global-set-key [hiragana-katakana] 'skk-mode)

;; ~/.skk $B$K$$$C$Q$$@_Dj$r=q$$$F$$$k$N$G%P%$%H%3%s%Q%$%k$7$?$$(B
(setq skk-byte-compile-init-file t)
;; $BCm(B) $B0[$J$k<oN`$N(B Emacsen $B$r;H$C$F$$$k>l9g$O(B nil $B$K$7$^$9(B

;; SKK $B$r(B Emacs $B$N(B input method $B$H$7$F;HMQ$9$k(B
;;   C-\ $B$G(B DDSKK $B$,5/F0$7$^$9(B
(setq default-input-method
      "japanese-skk"			; (skk-mode 1)
;;    "japanese-skk-auto-fill"		; (skk-auto-fill-mode 1)
      )

;; SKK $B$r5/F0$7$F$$$J$/$F$b!"$$$D$G$b(B skk-isearch $B$r;H$&(B
(setq skk-isearch-mode-enable 'always)

;; @@ $B1~MQE*$J@_Dj(B

;; ~/.skk* $B$J%U%!%$%k$,$?$/$5$s$"$k$N$G@0M}$7$?$$(B
(setq skk-user-directory "~/.ddskk")
;; $BCm(B 1) $B>e5-$N@_Dj$r$7$?>l9g!"(B~/.skk $B$d(B ~/.skk-jisyo $B$NBe$o$j$K(B
;;       ~/.ddskk/init $B$d(B ~/.ddskk/jisyo $B$,;H$o$l$^$9!#$?$@$7!"(B
;;       $B$3$l$i$N%U%!%$%kL>$r8DJL$K@_Dj$7$F$$$k>l9g$O$=$N@_Dj$,M%@h(B
;;       $B$5$l$k$N$GCm0U$7$F$/$@$5$$!#$^$?!"(B~/.skk $B$d(B ~/.skk-jisyo $B$r(B
;;       $B4{$K$b$C$F$$$k>l9g$O<jF0$G%3%T!<$9$kI,MW$,$"$j$^$9!#(B
;;       -- $B1F6A$r<u$1$kJQ?t$N0lMw(B --
;;          skk-init-file, skk-jisyo, skk-backup-jisyo
;;          skk-emacs-id-file. skk-record-file,
;;          skk-study-file, skk-study-backup-file
;; $BCm(B 2) SKK $B$N8D?M<-=q$O(B skkinput $B$J$I$N%W%m%0%i%`$G$b;2>H$7$^$9$+$i!"(B
;;       $B>e5-$N@_Dj$r$7$?>l9g$O$=$l$i$N%W%m%0%i%`$N@_Dj%U%!%$%k$b=q$-(B
;;       $B49$($kI,MW$,$"$j$^$9!#(B

;; migemo $B$r;H$&$+$i(B skk-isearch $B$K$O$*$H$J$7$/$7$F$$$FM_$7$$(B
(setq skk-isearch-start-mode 'latin)

;; YaTeX $B$N$H$-$@$16gFIE@$rJQ99$7$?$$(B
(add-hook 'yatex-mode-hook
	  (lambda ()
	    (require 'skk)
	    (setq skk-kutouten-type 'en)))

;; $BJ8>O7O$N%P%C%U%!$r3+$$$?;~$K$O<+F0E*$K1Q?t%b!<%I(B($B!V(BSKK$B!W%b!<%I(B)$B$KF~$k(B
(let ((function #'(lambda ()
		    (require 'skk)
		    (skk-latin-mode-on))))
  (dolist (hook '(find-file-hooks
		  ;; ...
		  mail-setup-hook
		  message-setup-hook))
    (add-hook hook function)))

;; Emacs $B5/F0;~$K(B SKK $B$rA0$b$C$F%m!<%I$9$k(B
(setq skk-preload t)
;; $BCm(B) skk.el $B$r%m!<%I$9$k$@$1$J$i(B (require 'skk) $B$G$b$h$$!#>e5-@_Dj$N(B
;; $B>l9g$O!"(Bskk-search-prog-list $B$K;XDj$5$l$?<-=q$b$3$N;~E@$GFI$_9~$s$G(B
;; $B=`Hw$9$k!#(BEmacs $B$N5/F0$OCY$/$J$k$,!$(BSKK $B$r;H$$;O$a$k$H$-$N%l%9%]%s%9(B
;; $B$,7Z2w$K$J$k!#(B

;;; dot.emacs ends here
