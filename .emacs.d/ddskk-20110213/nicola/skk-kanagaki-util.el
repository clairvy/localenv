;;; skk-kanagaki-util.el --- SKK $B$N2>L>F~NO%5%]!<%H$N$?$a$NF;6qH"(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004
;;   Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

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

;; macro$B!"(Binline function $B$O$3$3$KCV$-$^$9!#I,MW$J>l9g$O3F%b%8%e!<%k$NCf$+$i(B
;; $B$3$N%W%m%0%i%`$r%m!<%I$7$^$9!#(B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-vars)
  (require 'skk-macs))

(eval-when-compile
  (defvar skk-dcomp-start-point)
  (defvar skk-dcomp-end-point)
  (defvar skk-isearch-current-buffer)
  (defvar skk-nicola-okuri-flag)
  (defvar skk-nicola-hiragana-mode-string)
  (defvar skk-nicola-katakana-mode-string)
  (defvar skk-nicola-hiragana-rom-string)
  (defvar skk-nicola-katakana-rom-string))

;; Variables.

(defconst skk-kanagaki-dakuten-alist
  '(("$B$+(B" "$B$,(B") ("$B$-(B" "$B$.(B") ("$B$/(B" "$B$0(B") ("$B$1(B" "$B$2(B") ("$B$3(B" "$B$4(B")
    ("$B$5(B" "$B$6(B") ("$B$7(B" "$B$8(B") ("$B$9(B" "$B$:(B") ("$B$;(B" "$B$<(B") ("$B$=(B" "$B$>(B")
    ("$B$?(B" "$B$@(B") ("$B$A(B" "$B$B(B") ("$B$D(B" "$B$E(B") ("$B$F(B" "$B$G(B") ("$B$H(B" "$B$I(B")
    ("$B$O(B" "$B$P(B" "$B$Q(B") ("$B$R(B" "$B$S(B" "$B$T(B") ("$B$U(B" "$B$V(B" "$B$W(B") ("$B$X(B" "$B$Y(B" "$B$Z(B")
    ("$B$[(B" "$B$\(B" "$B$](B")
    ("$B%&(B" "$B%t(B")
    ("$B%+(B" "$B%,(B") ("$B%-(B" "$B%.(B") ("$B%/(B" "$B%0(B") ("$B%1(B" "$B%2(B") ("$B%3(B" "$B%4(B")
    ("$B%5(B" "$B%6(B") ("$B%7(B" "$B%8(B") ("$B%9(B" "$B%:(B") ("$B%;(B" "$B%<(B") ("$B%=(B" "$B%>(B")
    ("$B%?(B" "$B%@(B") ("$B%A(B" "$B%B(B") ("$B%D(B" "$B%E(B") ("$B%F(B" "$B%G(B") ("$B%H(B" "$B%I(B")
    ("$B%O(B" "$B%P(B" "$B%Q(B") ("$B%R(B" "$B%S(B" "$B%T(B") ("$B%U(B" "$B%V(B" "$B%W(B") ("$B%X(B" "$B%Y(B" "$B%Z(B")
    ("$B%[(B" "$B%\(B" "$B%](B"))
  "$BByE@$HH>ByE@$rF~NO$9$k$?$a$N%k!<%k!#(B")

(defconst skk-kanagaki-print-help-function
  (cond ((and skk-running-gnu-emacs
	      (>= emacs-major-version 23))
	 #'help-print-return-message)
	(t
	 #'print-help-return-message)))

;;;###autoload
(defmacro skk-kanagaki-help-1 (bufname title list)
  `(let ((buf (get-buffer-create ,bufname)))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert
	(concat
	 (format "%s\n\n" ,title)
	 (mapconcat
	  #'(lambda (cons)
	      (cond
	       ((and (symbolp (car cons))
		     (symbol-value (car cons)))
		(format "%s $B!D(B %s\n"
			(key-description (symbol-value (car cons)))
			(cdr cons)))
	       (t
		(format "%s $B!D(B %s\n" (car cons) (cdr cons)))))
	  ;;
	  (delq nil ,list) "")))
       ;;
       (setq buffer-read-only t)
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (help-mode))
     (let ((standard-output buf))
       (funcall skk-kanagaki-print-help-function))
     (display-buffer buf)))

;;;###autoload
(defun skk-nicola-visit-nicola-website ()
  (interactive)
  (let ((func (cond
	       ((fboundp 'browse-url)
		'browse-url)
	       (t
		'browse-url-netscape))))
    (funcall func "http://nicola.sunicom.co.jp/")))

;;;###autoload
(defun skk-kanagaki-toggle-rom-kana (&optional arg)
  "$B%m!<%^;zF~NO(B $B"N(B $B2>L>F~NO(B $B$r@Z$jBX$($k!#(B"
  (interactive)
  ;;
  (when (featurep 'skk-nicola)
      (setq skk-nicola-okuri-flag nil))
  ;;
  (setq skk-kanagaki-state
	(if (memq arg '(kana rom))
	    arg
	  (case skk-kanagaki-state
	    (kana 'rom)
	    (rom 'kana)
	    ;; $B$H$j$"$($:!#(B
	    (t 'kana))))
  (skk-kanagaki-adjust-rule-tree)
  ;;
  (when (featurep 'skk-nicola)
    ;; $B%b!<%I9T$NI=<($ND4@a!#(B
    (case skk-kanagaki-state
      (kana
       (setq skk-hiragana-mode-string skk-nicola-hiragana-mode-string
	     skk-katakana-mode-string skk-nicola-katakana-mode-string))
      (rom
       (setq skk-hiragana-mode-string skk-nicola-hiragana-rom-string
	     skk-katakana-mode-string skk-nicola-katakana-rom-string)))
    ;;
    (skk-modify-indicator-alist 'katakana skk-katakana-mode-string)
    (skk-modify-indicator-alist 'hiragana skk-hiragana-mode-string)
    ;;
    (skk-loop-for-buffers (buffer-list)
      (when (and skk-j-mode
		 (listp mode-line-format))
	(skk-update-modeline (if skk-katakana
				 'katakana
			       'hiragana))))))

;;;###autoload
(defun skk-kanagaki-dakuten (&optional arg handakuten)
  "$BD>A0$NJ8;z$r8+$F2DG=$J$iByE@$rIU2C$7!"$5$b$J$1$l$P(B \"$B!+(B\" $B$rF~NO$9$k!#(B"
  (interactive "*p")
  (let ((list skk-kanagaki-dakuten-alist)
	(pt1 (point))
	(henkan-on (and skk-isearch-switch
			(with-current-buffer
			    (get-buffer-create skk-isearch-working-buffer)
			  (eq skk-henkan-mode 'on))))
	char1 char2 str)
    (ignore-errors
      (setq char1 (cond
		   ((and skk-isearch-switch
			 (not (skk-in-minibuffer-p)))
		    (if henkan-on
			(with-current-buffer skk-isearch-working-buffer
			  (skk-save-point
			   (backward-char 1)
			   (buffer-substring-no-properties
			    (point)
			    pt1)))
		      (substring isearch-string -1)))
		   (t
		    (skk-save-point
		     (backward-char 1)
		     (buffer-substring-no-properties
		      (point)
		      pt1))))))
    (cond
     ((setq char2 (nth (if handakuten 2 1) (assoc char1 list)))
      (cond
       ((and skk-isearch-switch
	     (not (skk-in-minibuffer-p)))
	(if henkan-on
	    (with-current-buffer skk-isearch-working-buffer
	      (delete-char -1)
	      (skk-insert-str char2))
	  (setq str isearch-string)
	  (while (string= str (if (vectorp (car isearch-cmds))
				  (aref (car isearch-cmds) 0)
				(caar isearch-cmds)))
	    (with-current-buffer skk-isearch-current-buffer
	      (skk-isearch-delete-char arg)))
	  (setq isearch-string (concat (if (vectorp (car isearch-cmds))
					   (aref (car isearch-cmds) 0)
					 (caar isearch-cmds))
				       char2)
		isearch-message (concat
				 (skk-isearch-mode-string)
				 (mapconcat
				  #'isearch-text-char-description
				  isearch-string "")))
	  (put 'isearch-barrier 'skk-kanagaki t)
	  (skk-unread-event (character-to-event
			     (aref (where-is-internal
				    (if isearch-forward 'isearch-repeat-forward
				      'isearch-repeat-backward)
				    isearch-mode-map t)
				   0)))))
       (t
	(delete-char -1)
	(skk-insert-str char2))))
     (t
      (skk-insert-str (if handakuten
			  "$B!,(B"
			"$B!+(B"))))))

(defadvice isearch-repeat (around skk-kanagaki-workaround activate)
  (cond ((get 'isearch-barrier 'skk-kanagaki)
	 (goto-char isearch-barrier)
	 ad-do-it
	 (put 'isearch-barrier 'skk-kanagaki nil))
	(t
	 ad-do-it)))

;;;###autoload
(defun skk-kanagaki-handakuten (&optional arg)
  "$BD>A0$NJ8;z$r8+$F2DG=$J$iH>ByE@$rIU2C$7!"$5$b$J$1$l$P(B \"$B!,(B\" $B$rF~NO$9$k!#(B"
  (interactive "*p")
  (skk-kanagaki-dakuten arg t))

;;;###autoload
(defun skk-kanagaki-bs (arg)
  ;; OASYS $B$K$*$1$k(B BS $B%-!<$N5!G=$NBe$o$j!#$I$N$h$&$J5sF0$r$5$;$k$Y$-$+$^$@7h$^(B
  ;; $B$C$F$$$J$$!#8=:_$N$H$3$m(B
  ;;
  ;; o $B"'%b!<%I$G$O(B `skk-kanagaki-esc' $B$HF1$85sF0(B
  ;; o $B"&%b!<%I$G$O(B `skk-delete-backward-char' $B$HF1$85sF0(B
  ;; o $B"#%b!<%I$G$O(B `delete-backward-char' $B$HF1$85sF0(B
  ;;
  ;; $B$H$$$&$U$&$K9M$($F$$$k!#(B
  (interactive "*p")
  ;;
  (cond
   ((eq skk-henkan-mode 'active)
    (call-interactively 'keyboard-quit))
   ((and (eq skk-henkan-mode 'on)
	 (= (point) (marker-position
		     skk-henkan-start-point)))
    (skk-kakutei arg))
   ((eq skk-henkan-mode 'on)
    (forward-char -1)
    (delete-char 1))
   ((and skk-isearch-switch
	 (buffer-live-p skk-isearch-current-buffer))
    (with-current-buffer skk-isearch-current-buffer
      (skk-isearch-delete-char arg)))
   (t
    (delete-char (- 0 arg))))
  ;;
  (when skk-dcomp-activate
    (skk-dcomp-after-delete-backward-char)))

;;;###autoload
(let ((property (if (featurep 'xemacs)
		    'pending-del
		  'delete-selection)))
  (put 'skk-kanagaki-bs property 'supersede))

;;;###autoload
(defun skk-kanagaki-esc (&optional arg)
  ;; OASYS $B$K$*$1$k<h$j>C$75!G=$NBe$o$j!#(B $B$H$j$"$($:(B keyboard-quit $B$N>l9g$HF1MM(B
  ;; $B$NF0:n$r$9$k$h$&$K$K$7$F$*$/!#(BOAK $B&BHG$@$H(B
  ;;
  ;; o 1 $B2sL\$N<h$j>C$7$G!"JQ49A0$N>uBV$KLa$7$?>e$GJQ493+;OE@$K%]%$%s%H$r0\F0(B
  ;; o 2 $B2sL\$N<h$j>C$7$GJQ49BP>]$NJ8;zNsA4BN$r>C5n(B
  ;;
  ;; $B$9$k$h$&$K$J$C$F$$$k$,!"(BSKK $B$K$*$1$kJQ49BP>]$NJ8;zNs$O(B $B"&(B $B$H%]%$%s%H$N4V$N(B
  ;; $BJ8;zNs$G$"$j!"%]%$%s%H$r0\F0$9$k$HJQ49BP>]$,JQ$o$C$F$7$^$&!#$=$N$?$a!"%]%$(B
  ;; $B%s%H$O0\F0$7$J$$$3$H$H$9$k!#(B
  (interactive "*P")
  (cond
   ((skk-in-minibuffer-p)
    (call-interactively
     (if (fboundp 'minibuffer-keyboard-quit)
	 'minibuffer-keyboard-quit
       'abort-recursive-edit)))
   (skk-henkan-mode
    (call-interactively 'keyboard-quit))
   (t
    nil)))

(provide 'skk-kanagaki-util)

;;; skk-kanagaki-util.el ends here
