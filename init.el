(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; color theme
(require 'solarized)
(deftheme solarized-dark "The dark variant of the Solarized colour theme")
(create-solarized-theme 'dark 'solarized-dark)
(provide-theme 'solarized-dark)

;; 行番号
(require 'linum)
(global-linum-mode 1)

;; 警告音もフラッシュもすべて無効
(setq ring-bell-function 'ignore)

;; company mode 自動補完
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)    ;たぶん表示するまでの時間(defult:0.5)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

;; よくわからん ivy error??

;; cask set up
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ivy設定
(require 'ivy)
(ivy-mode　1)
(setq ivy-use-virtual-buffers t)

;; counsel設定
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; find-fileもcounsel任せ！
(defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

;; swiper 
(global-set-key "\C-s" 'swiper)
(defvar swiper-include-line-number-in-search t) ;; line-numberでも検索可能

