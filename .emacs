;;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\M-g" 'goto-line)

;;; コマンドキィをMeta キィとして利用
(setq mac-commandkey-is-meta t)

(setq load-path (cons (concat (getenv "HOME") "/.emacs.d")
		      load-path))

;;; skk
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/ddskk")
		      load-path))
(require 'skk-setup)
(setq skk-use-azik t)
(setq skk-azik-keyboard-type 'en)
(setq skk-kutouten-type 'en)

;;; Localeに合わせた環境の設定
(set-locale-environment nil)

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
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/egg")
		      load-path))
(require 'egg)

;;; tt-mode
(autoload 'tt-mode "tt-mode")
;(setq auto-mode-alist
;      (append '(("\\.tt$" . tt-mode))  auto-mode-alist))

;;; perl
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
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "VL ゴシック")))))
