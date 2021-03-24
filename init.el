;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; color theme
(load-theme 'sanityinc-tomorrow-blue t)

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

;;; バックアップファイル(*.~)
(setq backup-directory-alist '((".*" . "~/.backup_emacs"))) ;; ~/.backup_emacs 以下に配置
(setq version-control t) ;; 複数保存
(setq kept-new-versions 5) ;; 最新の保持数
(setq kept-old-versions 1) ;; 最古の保持数
(setq delete-old-versions t) ;; 範囲外削除

;;; lock file(.#*) は作成しない
(setq create-lockfiles nil)

;; dismiss startup screen
(setq inhibit-startup-screen t)

;; org-mode
(setq-default org-startup-folded nil)

;; tab -> space
(setq-default indent-tabs-mode nil)

;; company mode 自動補完
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

;; ivy
(require 'ivy)
;; ivy-rich
(require 'ivy-rich)
(ivy-rich-mode 1)
;; ivy + swiper + counsel
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c i f") 'counsel-describe-function)
(global-set-key (kbd "C-c i v") 'counsel-describe-variable)
(global-set-key (kbd "C-c i o") 'counsel-describe-symbol)
(global-set-key (kbd "C-c i l") 'counsel-find-library)
(global-set-key (kbd "C-c i i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c i u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; flycheck
(require 'flycheck)
(global-flycheck-mode)
(define-key global-map (kbd "\C-c n") 'flycheck-next-error)
(define-key global-map (kbd "\C-c p") 'flycheck-previous-error)
(define-key global-map (kbd "\C-c d") 'flycheck-list-errors)

;; eglot
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

;; magit
(global-set-key (kbd "C-x g") `magit-status)

;; wgrep
(require 'wgrep)
(setf wgrep-enable-key "e")
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

;; ace-window
(global-set-key (kbd "M-o") `ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; yasnippet
(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
(yas-global-mode 1)

;; nyan-mode
(require 'nyan-mode)
(nyan-mode 1)

;; all-the-icons
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'after-init-hook 'all-the-icons-ivy-setup)
;; TODO: check ivy-rich

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
;; TODO: use lsp-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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

;; emacs-emoji-cheat-sheet-plus
(require 'emoji-cheat-sheet-plus)
(global-set-key (kbd "C-c C-e") 'emoji-cheat-sheet-plus-insert)
(add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
(add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode)
(add-hook 'magit-mode-hook 'emoji-cheat-sheet-plus-display-mode)

;; org-mode local TODO lists
(add-hook 'org-mode-hook 'my-org-mode-stuff)
(defun my-org-mode-stuff ()
  (local-set-key (kbd "<f12>")
		 (lambda ()
		   (interactive)
		   (push ?t unread-command-events)
		   (push ?< unread-command-events)
		   (call-interactively 'org-agenda)))
  )

;;; init.el ends here
