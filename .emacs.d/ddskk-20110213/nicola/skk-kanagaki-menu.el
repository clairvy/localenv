;;; skk-kanagaki-menu.el --- NICOLA-DDSKK $B$N%a%K%e!<%5%]!<%H(B -*- coding: iso-2022-jp -*-

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

;; NICOLA-DDSKK $B$N%a%K%e!<$r(B SKK $BI8=`$N%a%K%e!<$KDI2C$7$^$9!#(B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-vars))

(eval-and-compile
  (autoload 'browse-url-netscape "browse-url"))

(require 'easymenu)

(eval-and-compile
  (defvar skk-kanagaki-menu-items
    '("NICOLA DDSKK"
      ["Set Henkan point" skk-set-henkan-point-subr t]
      ["Input Prefix or Suffix" skk-kanagaki-midashi-henkan t]
      ["Start Conversion with Okuri" skk-kanagaki-set-okurigana t]
      "--"
      ["Input a Character by Code" skk-input-by-code-or-menu t]
      ["Enter SKK Abbrev Mode" skk-abbrev-mode t]
      ["\
Convert  Hiragana <=> Katakana  or  Toggle Hiragana <=> Katakana Mode"
       skk-toggle-kana t]
      ["Enter SKK JIS X 0208 Latin Mode" skk-jisx0208-latin-mode t]
      ["\
Convert to Hankaku Katakana  or  Toggle Katakana <=> Hankaku Katakana Mode"
       skk-toggle-katakana t]
      ["Enter SKK Latin Mode" skk-latin-mode t]
      ["Enter SKK Japanese Mode" skk-kakutei t]
      ["Toggle Roma <=> Kana" skk-kanagaki-toggle-rom-kana t]
      "--"
      ["Show Key Bindings" skk-kanagaki-help t]
      ["Show the Current Keymap based on NICOLA" skk-nicola-help
       (featurep 'skk-nicola)]
      ["Show NICOLA-Specific Key Bindings" skk-nicola-2nd-help
       (featurep 'skk-nicola)]
      "--"
      ["Visit NIHONGO-NYURYOKU CONSORTIUM Web Site"
       skk-nicola-visit-nicola-website (locate-library "browse-url")])))

(cond
 ((eval-and-compile (featurep 'xemacs))
  (add-hook 'skk-mode-hook
	    #'(lambda ()
		(add-submenu
		 '("SKK")
		 skk-kanagaki-menu-items))))
 (t
  (dolist (map (list skk-j-mode-map
		     skk-latin-mode-map
		     skk-abbrev-mode-map
		     skk-jisx0208-latin-mode-map))
    (easy-menu-add-item
     map
     '("menu-bar" "SKK")
     skk-kanagaki-menu-items))
  (when (eval-when-compile skk-running-gnu-emacs)
    (setq skk-emacs-menu-resource-ja
	  (append
	   skk-emacs-menu-resource-ja
	   '(("Set Henkan point" . "$BJQ493+;OE@$r%;%C%H(B")
	     ("Input Prefix or Suffix" . "$B@\F,<-!&@\Hx<-$rF~NO(B")
	     ("Start Conversion with Okuri" . "$BAw$j$"$jJQ49$r3+;O(B")
	     ("Input a Character by Code" . "$B%3!<%IF~NO(B")
	     ("Enter SKK Abbrev Mode" . "Abbrev $B%b!<%I$KF~$k(B")
	     ("\
Convert  Hiragana <=> Katakana  or  Toggle Hiragana <=> Katakana Mode"
	      . "$B$+$J(B <=> $B%+%J(B $BJQ49(B  $B$^$?$O(B  $B$+$J%b!<%I(B <=> $B%+%J%b!<%I(B $B@Z49$((B")
	     ("Enter SKK JIS X 0208 Latin Mode" . "$BA41Q%b!<%I$KF~$k(B")
	     ("\
Convert to Hankaku Katakana  or  Toggle Katakana <=> Hankaku Katakana Mode"
	      . "\
$BH>3Q%+%J$KJQ49(B  $B$^$?$O(B  $BA43Q%+%J%b!<%I(B <=> $BH>3Q%+%J%b!<%I(B $B@Z49$((B")
	     ("Enter SKK Latin Mode" . "$B%"%9%-!<%b!<%I$KF~$k(B")
	     ("Enter SKK Japanese Mode" . "$B$+$J%b!<%I$KF~$k(B")
	     ("Toggle Roma <=> Kana" . "$B$+$JF~NOJ}<0(B  $B%m!<%^(B  <=> $B$+$J(B $B@Z49$((B")
	     ("Show Key Bindings" . "$B$+$JF~NOFH<+$N%-!<Dj5A$rI=<((B")
	     ("Show the Current Keymap based on NICOLA"
	      . "NICOLA $B%-!<G[Ns$rI=<((B")
	     ("Show NICOLA-Specific Key Bindings"
	      . "NICOLA $BFCM-$N%-!<Dj5A$rI=<((B")
	     ("Visit NIHONGO-NYURYOKU CONSORTIUM Web Site"
	      . "$BF|K\8lF~NO%3%s%=!<%7%"%`$N%5%$%H$X(B")))))))

(provide 'skk-kanagaki-menu)

;;; skk-kanagaki-menu.el ends here
