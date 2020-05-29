(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; color theme
(load-theme 'dracula t)

;; 行番号
(require 'linum)
(global-linum-mode 1)

;; メニューバーを非表示
(menu-bar-mode 0)

;; ツールバーを非表示
(tool-bar-mode 0)

;;スクロールバーを非表示
(scroll-bar-mode 0)

;; 警告音もフラッシュもすべて無効
(setq ring-bell-function 'ignore)

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; org-mode
(setq org-startup-folded nil)

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
;; company-tern
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(setq tern-command '("tern" "--no-port-file"))
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode)))

;; ivy setting
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; counsel setting
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; find-fileもcounsel任せ！
(defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

;; swiper
(global-set-key "\C-s" 'swiper)
(defvar swiper-include-line-number-in-search t) ;; line-numberでも検索可能

;; jedi
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi) ; add backend

;; flycheck
(require 'flycheck)
(global-flycheck-mode)
(define-key global-map (kbd "\C-cn") 'flycheck-next-error)
(define-key global-map (kbd "\C-cp") 'flycheck-previous-error)
(define-key global-map (kbd "\C-cd") 'flycheck-list-errors)

;; irony
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-to-list 'company-backends 'company-irony) ; backend追加
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (auto-complete-sage sage-shell-mode web-mode rainbow-delimiters markdown-mode undo-tree projectile spaceline spaceline-all-the-icons all-the-icons-dired all-the-icons-ivy all-the-icons nyan-mode yasnippet multiple-cursors rjsx-mode ace-window wgrep magit dracula-theme company-go go-mode js2-mode company-tern markdown-preview-mode solarized-theme flycheck counsel company-jedi company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; js2-mode
;;(require 'js2-mode)
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Goのパスを通す
(add-to-list 'exec-path (expand-file-name "/home/yoshiking/go/bin"))
;; go get で入れたツールのパスを通す
(add-to-list 'exec-path (expand-file-name "/home/yoshiking/go/src/github.com"))

;; flycheck-mode (go version)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
			  (add-hook 'before-save-hook' 'gofmt-before-save)
			  (local-set-key (kbd "M-.") 'godef-jump)
			  (set (make-local-variable 'company-backends) '(company-go))
			  (setq indent-tabs-mode nil)
			  (setq c-basic-offset 4)
			  (setq tab-width 4)
			  (company-mode)))

;; company-go
(require 'company-go)
(add-hook 'go-mode-hook 'company-mode)

;; magit
(global-set-key (kbd "C-x g") `magit-status)

;; wgrep
(require 'wgrep)
(setq wgrep-enable-key "r")

;; ace-window
(global-set-key (kbd "M-o") `ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; rjsx-mode
;;(add-to-list `auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
;;(add-hook 'rjsx-mode-hook
;;	  (lambda ()
;;	    (setq indent-tabs-mode nil) ;; インデントはタブではなく空白
;;	    (setq js-indent-level 2) ;; default 4 -> 2
;;	    ))
;;
;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; yasnippet
(require 'yasnippet)
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
(yas-global-mode 1)

;; nyan-mode
(require 'nyan-mode)
(nyan-mode 1)

;; all-the-icons
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'after-init-hook 'all-the-icons-ivy-setup)

;; spaceline
;; (require 'spaceline-config)
;; (spaceline-all-the-icons-theme)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "M-s p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-command "pandoc")

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; sagemath
(setq sage-shell:use-prompt-toolkit nil)
(add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
(add-hook 'sage-shell-mode-hook 'ac-sage-setup)
(eval-after-load "auto-complete-sage"
  '(setq sage-shell:completion-function 'completion-at-point))
(defun jpk/sage-mode-hook ()
  (company-mode -1))
(add-hook 'sage-shell-mode-hook 'jpk/sage-mode-hook)

;; org
(require 'org)
(require 'org-tempo)
(setq org-log-done 'time)
(setq org-use-speed-commands t)
(setq org-src-fontify-natively t)
(setq org-agenda-files '("~/Dropbox/emacs/org/task.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("e" "Experiment" entry (file+headline "~/Dropbox/emacs/org/experiment.org" "Experiment")
	 "* %? %U %i
#+BEGIN_SRC emacs-lisp

#+END_SRC")
	("i" "Idea" entry (file+headline "~/Dropbox/emacs/org/idea.org" "Idea")
	 "* %? %U %i")
	("r" "Remember" entry (file+headline "~/Dropbox/emacs/org/remember.org" "Remember")
	 "* %? %U %i")
	("m" "Memo" entry (file+headline "~/Dropbox/emacs/org/memo.org" "Memo")
	 "* %? %U %i")
	("a" "Archives" entry (file+headline "~/Dropbox/emacs/org/archives.org" "Archives")
	 "* %? %U %i")
	("t" "Task" entry (file+headline "~/Dropbox/emacs/org/task.org" "Task")
	 "** TODO %? \n   SCHEDULED: %^t \n")))
(setq org-refile-targets
      (quote (("~/Dropbox/emacs/org/archives.org" :level . 1)
	      ("~/Dropbox/emacs/org/remember.org" :level . 1)
	      ("~/Dropbox/emacs/org/memo.org" :level . 1)
	      ("~/Dropbox/emacs/org/task.org" :level . 1))))
