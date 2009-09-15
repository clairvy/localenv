;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-

;;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\M-g" 'goto-line)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;;; コマンドキィをMeta キィとして利用
(setq mac-commandkey-is-meta t)

(defvar hostname nil)
(let ((local-config-file (expand-file-name ".emacs.local" (getenv "HOME"))))
  (load local-config-file))
(defvar os-type nil)
(cond ((string-match "apple-darwin" system-configuration)
       (setq os-type 'mac))
      (t 'unknown))

;;; anything
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/anything")
                      load-path))
(require 'anything)

;;; session
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/session/lisp")
                      load-path))
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;;; tails-history
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/tails-history")
                      load-path))
(load-library "tails-history")

;;; color-moccur
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/occur")
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
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/ddskk")
                      load-path))
;;(setq skk-server-host "localhost")
;;(setq skk-server-portnum 1178)
(setq skk-use-azik t)
(if (not (string-equal hostname "canaan"))
    (setq skk-azik-keyboard-type 'en)
  (setq skk-azik-keyboard-type 'jp106))
(add-hook 'skk-mode-hook
          (lambda ()
            (setq skk-kutouten-type 'en)))
(require 'skk-setup)
(require 'skk-study)

;;; とりあえずファイルで
(cond ((string-match "apple-darwin" system-configuration)
       (setq skk-jisyo (concat (getenv "HOME") "/Library/AquaSKK/SKK-JISYO.L")))
      )

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
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/egg")
                      load-path))
(require 'egg)

;;; gist
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/gist")
                      load-path))
(require 'gist)

;;; OCaml
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/tuareg-mode")
                      load-path))
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; howm
(cond ((string-equal hostname "canaan") ; for private pc
       (setq howm-directory (expand-file-name "Dropbox/howm" (getenv "HOME"))))
      (t
       (setq howm-directory "/Volumes/共有フォルダ/社員フォルダ/永谷/howm/")))
                                        
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/rd-mode")
                      load-path))
(add-to-list 'auto-mode-alist '("\\.howm$" . rd-mode))
(autoload 'rd-mode "rd-mode" "major mode for ruby document formatter RD" t)
(add-to-list 'auto-mode-alist '("\\.rd$" . rd-mode))
(require 'rd-mode-plus)

;;; perl
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/cperl-mode")
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
;; perl-completion
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/perl-completion")
                      load-path))
(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)))
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (auto-complete-mode t)
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(ac-source-perl-completion)))))

;;; tiarra-conf
(setq load-path (cons (concat (getenv "HOME") "/.emacs.d/tiarra-conf")
                      load-path))
(load-library "tiarra-conf")

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
