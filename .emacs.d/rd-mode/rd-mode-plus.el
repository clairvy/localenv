;;; rd-mode-plus.el --- Convenient commands for tree-style memo.
;;; Copyright (c) 2002, 2003, 2004, 2006, 2007
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: rd-mode-plus.el,v 1.17 2007/05/07 17:08:10 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; DESCRIPTION:
;;; 
;;; See
;;; <http://lists.sourceforge.jp/mailman/archives/howm-eng/2005/000014.html>
;;; 
;;; See also: the original idea (memo-mode) by OSHIRO Naoki.
;;; <http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/> (in Japanease)

;;; * コマンド
;;;   * item 追加
;;;     * C-c C-a → 同レベル item を追加
;;;     * C-c C-o → 上位レベル item を追加
;;;     * C-c C-l → 下位レベル item を追加
;;;   * 階層変更
;;;     * C-c < → 階層を上げる
;;;       * 下位 item もひきつれて移動
;;;       * C-u C-c < なら, 同レベル item すべてを移動
;;;     * C-c > → 階層を下げる
;;;   * yank
;;;     * C-c C-p → 字下げして yank
;;;       * C-u C-c C-p なら, fill も
;;;       * (setq rd-yank-ignore-empty-line t) しておくと,
;;;         空白行のインデントにまどわされなくなる
;;;   * ChangeLog メモとの連携 (experimental)
;;;     * M-x rd-to-change-log → ChangeLog にエントリを作ってタイトルをコピー
;;;       (ChangeLog のファイル名は変数 rd-change-log-file で設定)
;;;     * C-u M-x rd-to-change-log → 章(=)内の全節(==)のタイトルもコピー
;;; 
;;; * インストール
;;;   * ~/.emacs に
;;;       (load "このファイル")
;;;     でいいはず
;;; 
;;; * バグ
;;;   * verbatim 内の itemlist
;;;       例
;;;       * この行は本当は verbatim なのに itemlist と解釈
;;;   * outline-mode で ---(ア)
;;;     * 例 ---(イ)
;;;     この行は本当は(ア)に属するのに, (イ)に属すると解釈
;;; 
;;; * 更新履歴
;;;   * [2007-05-07] fix: outline 関連の変数をバッファローカルに
;;;   * [2006-02-07] provide とライセンス文面を追加. (thx > 931 さん)
;;;     http://pc8.2ch.net/test/read.cgi/unix/1077881095/931
;;;   * [2004-11-23] 行頭「==>」を outline header とみなさない(thx > taku さん).
;;;   * [2004-04-10] カスタマイズ変数 rd-yank-ignore-empty-line
;;;   * [2004-03-31] fix: "++" の正規表現にエスケープ抜けてた (thx > 'も'さん).
;;;     http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?RdModePlus
;;;     ついでに, 見出しの === や + などの後には空白を必須に.
;;;   * [2003-12-15] C-u M-x rd-to-change-log
;;;     (thx > 'UCONNのポスドク'さん)
;;;   * [2003-12-12] fix: Symbol's value as variable is void: title
;;;     (thx > 'UCONNのポスドク'さん)
;;;   * [2003-12-12] fix: rd-to-change-log で余計なディレクトリ名
;;;   * [2003-12-03] 'UCONNのポスドク'さんのアイデア(ChangeLog メモ)を実装
;;;   * [2003-11-06] 'や'さんの XEmacs 用パッチを取りこみ
;;;     http://pc.2ch.net/test/read.cgi/unix/1063800495/236-237n
;;;   * …途中は忘却…
;;;   * [2002-09-25] 初版

;;; require

(require 'outline)
(require 'rd-mode)
(require 'cl) ;; needed??

;;; key binding

(define-key rd-mode-map "\C-c\C-a" 'rd-same-level-item)
(define-key rd-mode-map "\C-c\C-o" 'rd-higher-level-item)
(define-key rd-mode-map "\C-c\C-l" 'rd-lower-level-item)
(define-key rd-mode-map "\C-c<" 'rd-shift-subtree-left)
(define-key rd-mode-map "\C-c>" 'rd-shift-subtree-right)
(define-key rd-mode-map "\C-c\C-p" 'rd-yank-as-verbatim*)

;;; insert item

(defvar rd-item-header "* ")

;; Assume exactly same number of space(s) as rd-item-header.
(defun rd-same-level-item ()
  "Add same level item in the next line."
  (interactive)
  (rd-insert-item 0))
(defun rd-higher-level-item ()
  "Add higher level item in the next line."
  (interactive)
  (rd-insert-item (- (rd-item-indent))))
(defun rd-lower-level-item ()
  "Add lower level item in the next line."
  (interactive)
  (rd-insert-item (rd-item-indent)))

(defun rd-insert-item (offset)
  (let* ((base (rd-current-baseline))
         (indent (max 0
                      (+ base offset (- (rd-item-indent)))))
         (head-spaces (rd-spaces indent)))
    (end-of-line)
    (insert "\n" head-spaces rd-item-header)))

(defun rd-current-baseline ()
  (max 0 (ceiling (rd-current-level))))

(defun rd-headline-p ()
  (< (rd-current-level) 0))

(defun rd-stringline-p ()
  (= (rd-current-level) (rd-current-baseline)))

(defun rd-spaces (length)
  (format (format "%%%ds" length) ""))

(defun rd-current-level ()
  "Equivalent to baseline of current line except for some extensions.
Headlines have negative levels. Item lines have non-integer levels."
  (save-excursion
    (rd-beginning-of-line)
    (let ((epsilon 0.5))
      (cond ((looking-at "=begin *$") -7)
            ((looking-at "=end *$") -7)
            ((looking-at "[+][+] ") -1)
            ((looking-at "[+] ") -2)
            ((looking-at "==== ") -3)
            ((looking-at "=== ") -4)
            ((looking-at "== ") -5)
            ((looking-at "= ") -6)
            ((looking-at " *\\([*:]\\|([0-9]+)\\|---\\) *")
             (- (rd-column (match-end 0)) epsilon))
            ((looking-at " *")
             (rd-column (match-end 0)))
            (t (error "Can't happen."))))))

(defun rd-column (pos)
  (length (buffer-substring-no-properties (rd-line-beginning-position)
                                          pos)))

(defun rd-item-indent ()
  (length rd-item-header))

;;; shift item

(defun rd-shift-subtree-right (all-same-levels-p)
  "Shift subtree to right.
Same level subtrees are also moved simultaneously if ALL-SAME-LEVELS-P
is non-nil."
  (interactive "P*")
  (save-excursion
    (rd-beginning-of-subtree)
    (if (rd-headline-p)
        (error "No item.")
      (rd-shift-subtree (rd-item-indent) all-same-levels-p))))

(defun rd-shift-subtree-left (all-same-levels-p)
  "Shift subtree to left.
Same level subtrees are also moved simultaneously if ALL-SAME-LEVELS-P
is non-nil."
  (interactive "P*")
  (save-excursion
    (let* ((i (rd-item-indent))
           (sp (rd-spaces i)))
      (rd-beginning-of-subtree)
      (if (looking-at sp)
          (rd-shift-subtree (- i) all-same-levels-p)
        (error "No space.")))))

(defun rd-shift-subtree (offset all-same-levels-p)
  (save-excursion
    (let ((beg (progn
                 (rd-beginning-of-subtree)
                 (point)))
          (end (let ((lv (rd-current-level)))
                 (rd-forward-subtree)
                 (while (and all-same-levels-p
                             (not (eobp))
                             (= (rd-current-level) lv))
                   (rd-forward-subtree))
                 (point))))
      (rd-indent-region* beg end offset))))

(defun rd-beginning-of-subtree ()
  (let ((base (rd-current-baseline)))
    (rd-beginning-of-line) ;; here too? [2003-11-06]
    (when (rd-stringline-p)
      (while (cond ((bobp) nil)
                   ((< (rd-current-level) base) (rd-beginning-of-subtree) nil)
                   (t t))
        (forward-line -1)))))

(defun rd-forward-subtree ()
  (rd-beginning-of-subtree)
  (let ((lv (rd-current-level)))
    (forward-line)
    (while (and (not (eobp))
                (< lv (rd-current-level)))
      (forward-line))))

;; !! Original rd-memo.el has a function 'rd-indent-region'. !!
;; Modified from outline.el so that indent can be negative.
(defun rd-indent-region* (beg end &optional indent)
  "Make the indent of region deeper by INDENT. INDENT can be negative.
Return updated END."
  (interactive "r*")
  (setq indent (or indent 2))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if (> indent 0)
          (progn
            (insert-char ?  indent)
            (setq end (+ end indent)))
        (progn
          (re-search-forward " *" (+ (point) (- indent)) t)
          (let* ((b (match-beginning 0))
                 (e (match-end 0))
                 (d (- e b)))
            (delete-region b e)
            (setq end (- end d)))))
      (forward-line 1))
    end))

;;; outline-mode

(defvar rd-mode-hook)

;; override
(add-hook 'rd-mode-hook
          (lambda ()
            (set (make-local-variable 'outline-regexp)
                 "[=+]+ \\| *\\([*:]\\|([0-9]+)\\|---\\)")
            (set (make-local-variable 'outline-level)
                 'rd-current-level)))

;;; yank

(defvar rd-yank-fill-column-min 20)
(defvar rd-yank-ignore-empty-line nil)

;; !! Original rd-memo.el has a function 'rd-yank-as-verbatim'. !!
(defun rd-yank-as-verbatim* (&optional fill-p)
  "Yank as verbatim.
If ARG is non-nil, fill-region is also applied."
  (interactive "P*")
  (set-mark-command nil)
  (let* ((indent (save-excursion
                   (forward-line -1)
                   (+ (rd-current-baseline) (rd-item-indent))))
         (content (with-temp-buffer
                    (yank)
                    (rd-flushleft-region (point-min) (point-max)
                                         rd-yank-ignore-empty-line)
                    (when fill-p
                      (let ((fill-column (max rd-yank-fill-column-min
                                              (- fill-column indent))))
                        (fill-region (point-min) (point-max))))
                    (buffer-substring-no-properties (point-min) (point-max))))
         (beg (point))
         (end (progn
                (insert content)
                (point))))
    (rd-indent-region* beg end indent)
;     (rd-cite-region beg end)
    ))

(defun rd-flushleft-region (beg end &optional ignore-empty-line)
  (save-excursion
    (let* ((infinity 777)
           (min-indent infinity))
      (goto-char beg)
      (while (and (not (eobp))
                  (< (point) end))
        (looking-at " *")
        (if (and ignore-empty-line (looking-at "^ *$"))
            nil ;; do nothing
          (setq min-indent (min min-indent (rd-column (match-end 0)))))
        (forward-line))
      (rd-indent-region* beg end (- min-indent)))))

;;; with ChangelogMemo

(defvar rd-change-log-file "ChangeLog")

(defun rd-to-change-log (&optional all-sections)
  (interactive "P")
  (let* ((a (rd-get-subject all-sections))
         (chap (car a))
         (sec-list (second a)))
    (when (not all-sections)
      (setq sec-list (last sec-list)))
    ;; cheat add-change-log-entry
    (let ((buffer-file-name chap)
          (default-directory (or (file-name-directory rd-change-log-file)
                                 default-directory)))
      (add-change-log-entry nil rd-change-log-file)
      (mapc (lambda (sec)
              (when (not (string= sec ""))
                (newline)
                (insert "- " sec)))
            sec-list))))

;; copied and modified from outline.el
(defun rd-outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (let ((found (re-search-forward (concat "\n\\(" outline-regexp "\\)")
                                  nil 'move)))
    (if found
      (goto-char (1+ (match-beginning 0))))
    found))

(defun rd-get-subject (&optional all-sections)
  (let ((chap-level -6)
        (chap-regexp "^=[ \t]*\\([^=\n\r].*\\)?$")
        (chap-regexp-pos 1)
        (sec-level -5)
        (sec-regexp "^==[ \t]*\\([^=\n\r].*\\)?$")
        (sec-regexp-pos 1))
    (let ((chap nil)
          (sec-list nil))
      (save-excursion
        (when all-sections
          (end-of-line)
          (let ((pos (point)))
            (while (and (rd-outline-next-heading)
                        (> (rd-current-level) chap-level))
              (setq pos (point)))
            (goto-char pos)))
        (beginning-of-line)
        (catch 'rd-get-subject-done
          (while t
            (let ((level (rd-current-level)))
              (cond ((< level chap-level)
                     (throw 'rd-get-subject-done nil))
                    ((= level chap-level)
                     (progn
                       (when (looking-at chap-regexp)
                         (setq chap
                               (match-string-no-properties chap-regexp-pos)))
                       (throw 'rd-get-subject-done nil)))
                    ((= level sec-level)
                     (when (looking-at sec-regexp)
                       (setq sec-list
                             (cons (match-string-no-properties sec-regexp-pos)
                                   sec-list))))))
            (when (< (forward-line -1) 0)
              (throw 'rd-get-subject-done nil))))
        (list (or chap "") sec-list nil)))))

;;; fox XEmacs

;;; If you use APEL, you can replace a below block with (require 'poe).
(if (not (fboundp 'line-beginning-position))
    (defalias 'line-beginning-position 'point-at-bol))
(if (not (fboundp 'line-end-position))
    (defalias 'line-end-position 'point-at-eol))

(defun rd-beginning-of-line ()
  (if (re-search-backward "[\r\n]" nil t)
      (forward-char +1)
    (beginning-of-line)))

(defun rd-line-beginning-position ()
  (save-excursion
    (if (re-search-backward "[\r\n]" nil t)
        (progn (forward-char +1) (point))
      (line-beginning-position))))

;;; provide

(provide 'rd-mode-plus)

;;; rd-mode-plus.el ends here
