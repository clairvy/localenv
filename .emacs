;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-

;;; より一般度の高いものを上にする方針．
;;; 途中でコケて，C-h が効かなかったらアレでしょ？

;;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\M-g" 'goto-line)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;;; コマンドキィをMeta キィとして利用
(setq mac-commandkey-is-meta t)

(defvar home (getenv "HOME"))
(defvar hostname nil)
(let ((local-config-file (expand-file-name ".emacs.local" home)))
  (load local-config-file t))
(setenv "PATH" (concat (expand-file-name "local/bin" home) ":"
                       (getenv "PATH")))
(defvar os-type nil)
(cond ((string-match "apple-darwin" system-configuration)
       (setq os-type 'mac))
      (t 'unknown))

;;; apel
(if (or (string-equal hostname "mbp1")
	(= (string-to-int emacs-version) 23))
    (setq load-path (cons (concat home "/.emacs.d/apel")
			  load-path)))

;;; dired HACK
(setq load-path (cons (concat home "/.emacs.d/dired")
                      load-path))
(require 'sorter)
(defface my-face-f-2 '((t (:foreground "GreenYellow"))) nil)
(defvar my-face-f-2 'my-face-f-2)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
;   (concat (format-time-string "%Y-%m-%d" (current-time)) " [0-9]....") arg t))
   (concat (format-time-string "%b %e" (current-time)) " [0-9]....") arg t))
(add-hook 'dired-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              major-mode
              (list
               '(my-dired-today-search . my-face-f-2)
               ))))
(defun ls-lisp-handle-switches (file-alist switches)
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  ;; Return new alist sorted according to SWITCHES which is a list of
  ;; characters.  Default sorting is alphabetically.
  (let (index)
    (setq file-alist
          (sort file-alist
                (cond
                 ((memq ?S switches)    ; sorted on size
                  (function
                   (lambda (x y)
                     ;; 7th file attribute is file size
                     ;; Make largest file come first
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (< (nth 7 (cdr y))
                            (nth 7 (cdr x)))
                       (nth 0 (cdr x))))))
                 ((memq ?t switches)    ; sorted on time
                  (setq index (ls-lisp-time-index switches))
                  (function
                   (lambda (x y)
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (ls-lisp-time-lessp (nth index (cdr y))
                                             (nth index (cdr x)))
                       (nth 0 (cdr x))
                       ))))
                 ((memq ?X switches)    ; sorted on ext
                  (function
                   (lambda (x y)
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (string-lessp (file-name-extension (upcase (car x)))
                                       (file-name-extension (upcase (car y))))
                       (nth 0 (cdr x))))))
                 (t                     ; sorted alphabetically
                  (if ls-lisp-dired-ignore-case
                      (function
                       (lambda (x y)
                         (if (equal (nth 0 (cdr y))
                                    (nth 0 (cdr x)))
                             (string-lessp (upcase (car x))
                                           (upcase (car y)))
                           (nth 0 (cdr x)))))
                    (function
                     (lambda (x y)
                       (if (equal (nth 0 (cdr y))
                                  (nth 0 (cdr x)))
                           (string-lessp (car x)
                                         (car y))
                         (nth 0 (cdr x)))))
                    )))))
    )

  (if (memq ?r switches)                ; reverse sort order
      (setq file-alist (nreverse file-alist)))
  file-alist)

;;; elscreen
(cond ((= (string-to-int emacs-version) 23)
       (setq load-path (cons (concat home "/.emacs.d/elscreen")
                             load-path))
       (load "elscreen" "ElScreen" t)))

;;; yasnippet
(setq load-path (cons (concat home "/.emacs.d/yasnippet")
                      load-path))

;;; anything
(setq load-path (cons (concat home "/.emacs.d/anything")
                      load-path))
(require 'anything)
(setq yas/trigger-key "TAB")
(require 'yasnippet-config)
(yas/setup (expand-file-name ".emacs.d/yasnippet" home))

;;; session
(setq load-path (cons (concat home "/.emacs.d/session/lisp")
                      load-path))
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;;; tails-history
(setq load-path (cons (concat home "/.emacs.d/tails-history")
                      load-path))
(load-library "tails-history")

;;; color-moccur
(setq load-path (cons (concat home "/.emacs.d/occur")
                      load-path))
(require 'color-moccur)
(require 'moccur-edit)
(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "*Completions*" "*Messages*"
        "newsrc.eld" ".bbdb"))
(setq moccur-split-word t)
(setq dmoccur-use-list t)
(setq dmoccur-use-project t)
(setq dmoccur-list
      '(
        ("dir" default-directory (".*") dir)
        ("soft" "~/www/soft/" ("\\.texi$") nil)
        ("config" "~/mylisp/"  ("\\.js" "\\.el$") nil)
        ("1.99" "d:/unix/Meadow2/1.99a6/" (".*") sub)
        ))
(global-set-key "\C-x\C-o" 'occur-by-moccur)
(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
;(define-key dired-mode-map "O" 'dired-do-moccur)
(global-set-key "\C-c\C-x\C-o" 'moccur)
(global-set-key "\M-f" 'grep-buffers)
(global-set-key "\C-c\C-o" 'search-buffers)

;;; skk
(setq load-path (cons (concat home "/.emacs.d/ddskk")
                      load-path))
(setq skk-use-azik t)
(cond ((string-equal hostname "canaan")
       (setq skk-azik-keyboard-type 'jp106))
      (t
       (setq skk-azik-keyboard-type 'en)))
(add-hook 'skk-mode-hook
          (lambda ()
            (setq skk-server-host "localhost")
            (setq skk-server-portnum 1178)
            (setq skk-auto-insert-paren t)
            (setq skk-kutouten-type 'en)))
(require 'skk-setup)
(require 'skk-study)

;;; コントロールキーをシステムにとられないようにする
(setq mac-pass-control-to-system nil)

;;; Localeに合わせた環境の設定
(set-locale-environment nil)
(set-default-coding-systems 'utf-8-unix)

;;; 対応する括弧を光らせる。
(show-paren-mode t)

;;; ツールバーを消す
(tool-bar-mode 0)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;; スクロールを一行ずつにする
(setq scroll-step 1)

;;; スクロールバーを右側に表示する
(set-scroll-bar-mode nil)

;;; モードラインに時間を表示する
(setq display-time-day-and-date 't)
(display-time)

;;; 同名のファイルを開いたときなどでファイル名がわかりやすくなる設定
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; elscreen
(load "elscreen" "Elscreen" t)

;;; git(egg)
(setq load-path (cons (concat home "/.emacs.d/egg")
                      load-path))
(require 'egg)

;;; gist
(setq load-path (cons (concat home "/.emacs.d/gist")
                      load-path))
(require 'gist)

;;; OCaml
(setq load-path (cons (concat home "/.emacs.d/tuareg-mode")
                      load-path))
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; JavaScript
(setq load-path (cons (concat home "/.emacs.d/js2-mode/build")
                      load-path))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;; howm
(cond ((string-equal hostname "canaan") ; for private pc
       (setq howm-directory (expand-file-name "Dropbox/howm" home)))
      (t
       (setq howm-directory "/Volumes/共有フォルダ/社員フォルダ/永谷/howm/")))
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))

;;; rd-mode
(setq load-path (cons (concat home "/.emacs.d/rd-mode")
                      load-path))
(autoload 'rd-mode "rd-mode" "major mode for ruby document formatter RD" t)
(add-to-list 'auto-mode-alist '("\\.rd$" . rd-mode))
(require 'rd-mode-plus)

;;; perl
(setq load-path (cons (concat home "/.emacs.d/cperl-mode")
                      load-path))
(setq load-path (cons (concat home "/.emacs.d/cperl-calculate-indent")
                      load-path))
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(fset 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq cperl-font-lock t)
             (cperl-set-style "BSD")
             (setq tab-width 8)
             (c-set-offset 'arglist-intro '++)
             (c-set-offset 'arglist-close 0)
             ;; from Perl Best Practice
             '(cperl-indent-level 4)
             '(cperl-continued-statement-offset 4)
             '(cperl-close-paren-offset -4)
             '(cperl-indent-parens-as-block t)
             '(cperl-tab-always-indent t)
             ))
;;; perl flymake
;;; - http://d.hatena.ne.jp/sun-basix/20080705/1215278204
;;; - http://d.hatena.ne.jp/antipop/20080701/1214838633
(require 'flymake)
(push '("\\.pl$" flymake-perl-init) flymake-allowed-file-name-masks)
(push '("\\.pm$" flymake-perl-init) flymake-allowed-file-name-masks)
(push '("\\.t$" flymake-perl-init) flymake-allowed-file-name-masks)
(push '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1) flymake-err-line-patterns)
(add-hook 'cperl-mode-hook
 '(lambda ()
;    (set-perl5lib)
    (flymake-mode)))

;;; for flymake
;;; - http://www.credmp.org/index.php/2007/07/20/on-the-fly-syntax-checking-java-in-emacs/
;;; copy - http://d.hatena.ne.jp/khiker/20070720/emacs_flymake
(define-key global-map "\C-ce" 'credmp/flymake-display-err-minibuf)
(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

;; perl-completion
(setq load-path (cons (concat home "/.emacs.d/perl-completion")
                      load-path))
(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'perl-completion)
            (perl-completion-mode t)))
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (auto-complete-mode t)
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(ac-source-perl-completion)))))
(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'cperl-calculate-indent)))

;;; scala
(add-to-list 'load-path (expand-file-name ".emacs.d/scala-mode" home))
(require 'scala-mode-auto)
;;; with yasnippet
(setq yas/my-directory
      (expand-file-name ".emacs.d/scala-mode/contrib/yasnippet/snippets" home))
(yas/load-directory yas/my-directory)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)
             ))
;;; flymake - http://d.hatena.ne.jp/kiris60/20091004/1254586627
(require 'scala-mode-flymake)

;;; tiarra-conf
(setq load-path (cons (concat home "/.emacs.d/tiarra-conf")
                      load-path))
(load-library "tiarra-conf")

;;; twittering-mode
(setq load-path (cons (concat home "/.emacs.d/twittering-mode")
                      load-path))
(require 'twittering-mode)

;;; for simple-hatena-mode
(cond ((>= (string-to-int emacs-version) 23)
       (setq load-path (cons (expand-file-name ".emacs.d/html-helper-mode" home)
                             load-path))
       (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)))

(setq load-path (cons (expand-file-name ".emacs.d/simple-hatena-mode" home)
                      load-path))
(require 'simple-hatena-mode)
(setq simple-hatena-root (expand-file-name ".hatena" home))
;(require 'hatenahelper-mode)
;(add-hook 'simple-hatena-mode-hook
;          '(lambda ()
;             (hatenahelper-mode 1)))

;;; transrate URL to id notation                                                
(defun my-hatena-convert-url-to-id ()
  "カーソル付近のURLをはてなID記法に変換する。"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (idstr (my-hatena-convert-url-to-id-1
                       (buffer-substring beg end))))
          (if idstr
              (progn
                (delete-region beg end)
                (insert idstr)))))))

(defun my-hatena-convert-url-to-id-1 (url)
  "与えられた URL をはてなID記法に変換する"
  (let* ((delim ":")
         (urls (split-string url "/"))
         (authority (nth 2 urls))
         (user (nth 3 urls))
         (date (nth 4 urls))
         (item (nth 5 urls))
         (idstr (concat "id:" user)))
    (if (string-match "\\<\\(.\\)\\.hatena\\.ne\\.jp\\>" authority)
        (setq idstr (concat (match-string 1 authority) delim idstr)))
    (if (and date (not (string-equal date "")))
        (setq idstr (concat idstr delim date)))
    (if (and item (not (string-equal item "")))
        (setq idstr (concat idstr delim item)))
    idstr))

(defun my-hatena-convert-amazon ()
  "カーソル付近の値をはてなamazon記法に変換する。"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (amazon-str (my-hatena-convert-amazon-1
                       (buffer-substring beg end))))
          (if amazon-str
              (progn
                (delete-region beg end)
                (insert amazon-str)))))))

(defun my-hatena-convert-amazon-1 (id &optional title)
  "id でamazon へのリンクをつくる"
  (let ((url '("[http://www.amazon.co.jp/dp/")))
    (if (stringp id)
        (setq url (append url (list id "/search042-22/ref=nosim/")))
      (error "Invalid argument %s" x))
    (if (stringp title)
        (setq url (append url (list ":title=" title "]"))))
    (setq url (append url '("]")))
    (mapconcat (lambda (x) x) url "")))

(defun my-hatena-get-near-word (string-maker)
;  (let ((bounds (bounds-of-thing-at-point 'word)))                             
  (let ((bounds (bounds-of-thing-at-point 'filename))) ; :: のためにこっちに    
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (new-str (funcall string-maker (buffer-substring beg end))))
          (if new-str
              (progn
                (delete-region beg end)
                (insert new-str)))))))

(defun my-hatena-convert-cpan ()
  "カーソル付近のモジュール名をcpan URLに変換する。"
  (interactive)
  (my-hatena-get-near-word 'my-hatena-convert-cpan-1))

(defun my-hatena-convert-cpan-1 (name)
  "search.cpan へのリンクを作る"
  (let ((url '("[http://search.cpan.org/dist/")))
    (if (stringp name)
        (let ((new-name name))
          (while (string-match "::" new-name)
            (setq new-name (replace-match "-" t t new-name)))
          (setcdr url (list new-name "/:title=" name "]")))
      (error "Invalid argument %s" x))
    (mapconcat (lambda (n) n) url "")))

;;; shell
(setq sh-basic-offset 2)

;;; fixed-width-font for mac
;;; http://macemacsjp.sourceforge.jp/matsuan/FontSettingJp.html
(if (eq window-system 'mac)
    (progn
      (setq load-path (cons (expand-file-name ".emacs.d/fixed-width-fontset"
                                              home)
                            load-path))
      (require 'carbon-font)
      (fixed-width-set-fontset "hiramaru" 12)
      ))

;;; font for ubuntu
(cond ((string-match "linux" system-configuration)
       (custom-set-faces
	;; custom-set-faces was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	'(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "VL ゴシック"))))
	)
       ))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(show-paren-mode t))

(put 'dired-find-alternate-file 'disabled nil)
