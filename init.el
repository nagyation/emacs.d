;;; init.el --- Mahmoud Nagy's custom emacs config
;;; Commentary:
;;; Code: .
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-list '(magit
		     flycheck
		     helm-gtags
		     helm-projectile
		     projectile
		     helm
		     company-c-headers
		     gruvbox-theme
		     dumb-jump
		     highlight-parentheses
		     neotree
		     ;; org-trello
		     ))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;HELM configuration
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-autoresize-mode 1)
(helm-mode 1)


;; helm-gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Flycheck configs

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/workspace/"))

;;Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)

;; C level makefile config
;; (setq cc-mode-compile-make-command "make")
;; (setq cc-mode-compile-flash-command "make flash")
;; (setq cc-mode-compile-clean-command "make clean")

;; (defun cc-mode-compile-make ()
;;   (interactive)
;;   (setq compile-command cc-mode-compile-make-command)
;;   (call-interactively 'compile))
;; (defun cc-mode-compile-flash ()
;;   (interactive)
;;   (setq compile-command cc-mode-compile-flash-command)
;;   (call-interactively 'compile))
;; (defun cc-mode-compile-clean ()
;;   (interactive)
;;   (setq compile-command cc-mode-compile-clean-command)
;;   (call-interactively 'compile))
;; (defun cc-mode-compile ()
;;   (local-set-key (kbd "C-x C m") 'cc-mode-compile-make)
;;   (local-set-key (kbd "C-x C f") 'cc-mode-compile-flash)
;;   (local-set-key (kbd "C-x C c") 'cc-mode-compile-clean))

;; (add-hook 'c-mode-hook 'cc-mode-compile)
;; (add-hook 'c++-mode-hook 'cc-mode-compile)
(add-hook 'c-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)))

;; dump jump configs

(dumb-jump-mode)

;; neo tree configs
(require 'neotree)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))
(global-set-key [f9] 'neotree-project-dir)

;;======================ORG Mode Configs=================================
;; (require 'org-trello)


;;=====================General Key Mapping==============================

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)


;;====================Look and Feel=====================================
(require 'highlight-parentheses) ;; highlighting
(global-highlight-parentheses-mode)

(load-theme 'gruvbox-dark-hard t)
(set-frame-font "Hack 12" nil t)
;gui remove bars etc..
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
;; power line customization
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(setq powerline-arrow-shape 'arrow)
(custom-set-faces
 '(mode-line ((t (:foreground "Black" :background "#cf6e00" :box nil)))))

(provide 'init)
;;; init.el ends here
