;;; init.el --- Mahmoud Nagy's custom emacs config
;;; Commentary:
;;; Code:

;; performance optimization
(setq gc-cons-threshold 100000000)

;; packages handling
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(defvar package-list)
(setq package-list '(magit
		     flycheck
                     flycheck-irony
		     helm-gtags
		     projectile
                     helm-projectile
		     helm
		     gruvbox-theme
		     dumb-jump
		     highlight-parentheses
		     neotree
		     cyphejor
		     diminish
                     minions
		     highlight-doxygen
		     org-trello
		     switch-window
		     markdown-mode
		     elpy
		     py-autopep8
		     flyspell
                     sudo-edit
                     yaml-mode
                     company-irony
                     company-irony-c-headers
                     disaster
                     camcorder
                     xkcd
                     telega
                     nyan-mode
                     alarm-clock
                     fancy-battery
                     mood-line
                     fix-input
                     srefactor
                     fira-code-mode
                     use-package
                     ;; DOOM
                     doom-themes
                     doom-modeline
                     ;; EXWM
                     exwm
                     helm-exwm
                     swiper-helm
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
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(helm-autoresize-mode 1)
(helm-mode 1)


(require 'swiper-helm)
(global-set-key (kbd "C-s") 'swiper-helm)


;; helm-gtags
(require 'helm-gtags)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-c"
 helm-gtags-suggested-key-mapping t
 )

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

(setq helm-echo-input-in-header-line t)
(add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)
;; Flycheck configs

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; FlySpell
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
(add-hook hook (lambda () (flyspell-mode -1))))

(add-hook 'c-mode-hook (lambda () (flyspell-prog-mode)))

;; Projectile
(require 'projectile)
(projectile-mode +1)
(setq projectile-indexing-method 'alien)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/workspace/"))
(setq projectile-use-git-grep t)

(require 'helm-projectile)
(helm-projectile-on)

;;Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-tooltip-align-annotations t)

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; Magit

(global-set-key (kbd "C-x g") 'magit-status)

;; C
(require 'cc-mode)
(setq-default indent-tabs-mode nil)
(setq c-default-style "linux"
      c-basic-offset 4)
(add-hook 'c-mode-hook #'irony-mode)

(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-alist
      '(("\\.h\\'"
         nil
         '(setq v2 (upcase (concat (file-name-nondirectory
                                 (file-name-sans-extension buffer-file-name)))))
         "#ifndef __" v2 "__"n
         "#define __" v2 "__" n n n n n
         "#endif")))

;; srefactor
(require 'srefactor)
(require 'srefactor-lisp)

(semantic-mode 1)

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

;; disaster

(require 'disaster)
(define-key c-mode-base-map (kbd "C-c d") 'disaster)

;;========================= Python Configs ==================================
(elpy-enable)
(when (require 'flycheck nil t)

  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8

;; (add-hook 'elpy-mode-hook (lambda ()
;;                             (add-hook 'before-save-hook
;;                                       'elpy-autopep8-fix-code nil t)))


;; ======================== Yaml Configs =============================
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; dump jump configs
;; TODO: use better jump
(dumb-jump-mode)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; neo tree configs
(require 'neotree)
(setq neo-smart-open t)
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

;; Highlighting Doxgyen

(highlight-doxygen-global-mode 1)


;; switch window

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x <up>") 'switch-window-mvborder-up)
(global-set-key (kbd "C-x <down>") 'switch-window-mvborder-down)
(global-set-key (kbd "C-x <left>") 'switch-window-mvborder-left)
(global-set-key (kbd "C-x <right>") 'switch-window-mvborder-right)
(setq switch-window-multiple-frames t)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts '("a" "o" "e" "u" "h" "t" "n" "s" "-" "i" "d" "q"))

;; sudo edit
(require 'sudo-edit)

;; telega

(require 'telega)
(add-hook 'telega-chat-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '(telega-company-emoji
                           telega-company-username
                           telega-company-hashtag)
                         (when (telega-chat-bot-p telega-chatbuf--chat)
                           '(telega-company-botcmd))))
            (company-mode 1)))
(telega-notifications-mode 1)

;; fix input
(require 'fix-input)
(fix-input "english-dvorak" "arabic" "dvorak-arabic")

;; mail
(setq user-full-name "Mahmoud Nagy")
(setq user-mail-address "mnagy1312@gamil.com")
(setq send-mail-function 'mailclient-send-it)

;;======================ORG Mode Configs=================================

;; plantuml

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/vendor/plantuml.jar"))

(require 'org-trello)
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED")))

(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
   ("DOING" . "yellow")
   ("BLOCKED" . "red")
   ("REVIEW" . "orange")
   ("DONE" . "green")
   ("ARCHIVED" .  "blue")))


;;=====================Custom Functions=================================

;; move line up&down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))


;;=====================General Key Mapping==============================

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-t") 'transpose-chars)
(global-set-key (kbd "C-x C-n") 'make-frame)
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "C-h") 'forward-char)
(global-set-key (kbd "M-h") 'forward-word)
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)


;;========================EXWM=========================================
(require 'exwm)

(defun my-exwm-config ()
  "Default configuration of EXWM."
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            (,(kbd "s-i") . exwm-input-toggle-keyboard) ;; Toggle between "line-mode" and "char-mode" in an EXWM window
            (,(kbd "s-D") . kill-this-buffer)
            (,(kbd "s-f") . exwm-floating-toggle-floating) ;; Toggle the current window between floating and non-floating states
            (,(kbd "s-h") . exwm-floating-hide) ;; Toggle the current window between floating and non-floating states
                        
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9)))))
  ;; Line-editing shortcuts
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete]))))
  ;; Enable EXWM
  (setq exwm-layout-show-all-buffers t)
  (exwm-enable)
  ;; Configure Ido
  (exwm-config-ido)
  ;; ;; Other configurations
  (exwm-config-misc))

(require 'exwm-systemtray)
(require 'exwm-config)
(my-exwm-config)
(exwm-systemtray-enable)

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(1 "HDMI-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-1 --left-of eDP-1 --auto")))
(exwm-randr-enable)

(add-to-list 'exwm-input-simulation-keys '([?\M-w] . [C-c]))
(add-to-list 'exwm-input-simulation-keys '([?\C-y] . [C-v]))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "qutebrowser"))
              (call-interactively #'exwm-input-release-keyboard))))

(require 'helm-exwm)

(setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
(setq helm-exwm-source (helm-exwm-build-source))
(setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                  helm-exwm-source
                                  helm-source-recentf))

;;====================Look and Feel=====================================

(setq custom-file "/dev/null") ;; so far I manually edit emacs, no need for customization

(require 'highlight-parentheses) ;; highlighting
(global-highlight-parentheses-mode)
(electric-pair-mode)
;; (load-theme 'gruvbox-dark-hard  t)
(add-to-list 'default-frame-alist '(font . "Fira Code 12"))
;(set-frame-font "Office Code Pro D 12" nil t)

;; gui remove bars etc..
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame (or frame (selected-frame))
            (when window-system
              (use-package fira-code-mode
                 :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
                 :hook prog-mode)))))

(when (display-graphic-p)
  ;; fire code font lingature
  (use-package fira-code-mode
    :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
    :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes
  )

;; mode line
(require 'doom-modeline)
(doom-modeline-mode)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-buffer-encoding nil)

(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-tomorrow-night t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(require 'nyan-mode)
(nyan-mode t)
(setq nyan-bar-length 18)
(setq nyan-minimum-window-width 72)
(display-time-mode t)
(display-battery-mode t)
(nyan-toggle-wavy-trail)
;; (nyan-start-animation)

;;  shrink mode names using cyphejor

(require 'cyphejor)
(setq
 cyphejor-rules
 '(:upcase
   ("bookmark"    "→")
   ("buffer"      "β")
   ("diff"        "Δ")
   ("dired"       "δ")
   ("emacs"       "ε")
   ("inferior"    "i" :prefix)
   ("interaction" "i" :prefix)
   ("interactive" "i" :prefix)
   ("lisp"        "λ" :postfix)
   ("menu"        "▤" :postfix)
   ("mode"        "")
   ("package"     "↓")
   ("python"      "π")
   ("shell"       "sh" :postfix)
   ("text"        "ξ")
   ("wdired"      "↯δ")
   ))

(cyphejor-mode 1)

(require 'minions)
(minions-mode)

(require 'diminish)
(diminish 'projectile-mode "p")
(diminish 'flycheck-mode "fc")
(diminish 'company-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'abbrev-mode "Abv")

(put 'narrow-to-region 'disabled nil)
(provide 'init)
;;; init.el ends here
