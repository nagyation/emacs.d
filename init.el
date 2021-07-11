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
                     kdeconnect
                     nyan-mode
                     alarm-clock
                     fancy-battery
                     mood-line
                     fix-input
                     srefactor
                     fira-code-mode
                     use-package
                     which-key
                     rainbow-delimiters
                     hydra
                     undo-tree
                     mode-line-bell
                     ;; DOOM
                     doom-themes
                     doom-modeline
                     ;; EXWM
                     exwm
                     helm-exwm
                     ;; search/goto
                     swiper-helm
                     avy
                     ;; MAIL GNUS
                     gnus-desktop-notify
                     bbdb
                     dianyou
                     ;; dired+
                     image-dired+
                     dired-narrow
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

(add-to-list 'load-path "~/.emacs.d/vendor/")

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


;; swiper

(require 'swiper-helm)
(global-set-key (kbd "C-M-s") 'swiper-helm)

;; avy

(require 'avy)

(setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i ?d))

(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; undo tree
(require 'undo-tree)

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
(setq projectile-indexing-method 'hybrid)
(setq projectile-switch-project-action #'projectile-find-dir)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/workspace/"))
(setq projectile-use-git-grep t)
(setq projectile-find-dir-includes-top-level t)
(projectile-mode +1)

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

;; EPG passphrase
(use-package epg
  :defer t
  :config
  ;; Let Emacs query the passphrase through the minibuffer
  (setq epg-pinentry-mode 'loopback))

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

;; dired+

(eval-after-load 'dired '(require 'dired+))

;; dired-narrow

(require 'dired-narrow)

;; image-dired+

(require 'image-dired+)
(eval-after-load 'image-dired+ '(image-diredx-async-mode 1))
(eval-after-load 'image-dired+ '(image-diredx-adjust-mode 1))

;; switch window

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x <up>") 'switch-window-mvborder-up)
(global-set-key (kbd "C-x <down>") 'switch-window-mvborder-down)
(global-set-key (kbd "C-x <left>") 'switch-window-mvborder-left)
(global-set-key (kbd "C-x <right>") 'switch-window-mvborder-right)
(setq switch-window-multiple-frames t)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts '("a" "o" "e" "u" "h" "t" "n" "s" "-" "i" "d" "q" "'" "," "." "c" "r" "l" "p" "y" "g"))

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
(setq telega-use-images t)
(setq telega-emoji-use-images t)
(setq telega-chat-title-emoji-use-images t)
(setq telega-msg-use-images-in-one-line t)

;; kdeconnect
(require 'kdeconnect)

;; fix input
(require 'fix-input)
(fix-input "english-dvorak" "arabic" "dvorak-arabic")

;; mail
(setq user-full-name "Mahmoud Nagy")
(setq user-mail-address "mnagy1312@gamil.com")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-message-archive-method '(nnimap "imap.gmail.com")
      gnus-message-archive-group "[Gmail]/Sent Mail")

(setq send-mail-function		'smtpmail-send-it
      message-send-mail-function	'smtpmail-send-it)

;; (require 'nnir)
;; (nnir-search-engine imap)
;; (add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
;; 				 (nnimap-stream ssl)
;;  				 (nnimap-address "imap.gmail.com")
;;  				 (nnimap-server-port 993)
;;  				 (nnir-search-engine imap)))
;; (add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))
;; (setq nnir-imap-default-search-key "gmail")

(setq gnus-use-cache t)

(require 'gnus-desktop-notify)
(gnus-demon-add-handler 'gnus-group-get-new-news 2 t)
(gnus-demon-init)
(gnus-desktop-notify-mode)
(gnus-demon-add-scanmail)

(require 'bbdb)
(setq bbdb/news-auto-create-p t)

;; save emails from mails
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'message) ;; use 'gnus for incoming messages too
(setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking

;; use insert mail from recieved
(require 'dianyou)

;; import gmail contacts
(add-to-list 'load-path "~/.emacs.d/vendor/gmail2bbdb/")
(autoload 'gmail2bbdb-import-file "gmail2bbdb" nil t nil)

;; GNUS HYDRA
(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups A A")
       ("l" gnus-group-list-all-groups "LOCAL groups L")
       ("c" gnus-topic-catchup-articles "Read all c")
       ("G" gnus-group-make-nnir-group "Search server G G")
       ("g" gnus-group-get-new-news "Refresh g")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose m OR C-x m")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do?"
       ("s" gnus-summary-show-thread "Show thread")
       ("h" gnus-summary-hide-thread "Hide thread")
       ("n" gnus-summary-insert-new-articles "Refresh / N")
       ("f" gnus-summary-mail-forward "Forward C-c C-f")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all c")
       ("e" gnus-summary-resend-message-edit "Resend S D e")
       ("R" gnus-summary-reply-with-original "Reply with original R")
       ("r" gnus-summary-reply "Reply r")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
       ("w" gnus-summary-wide-reply "Reply all S w")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original R")
       ("r" gnus-article-reply "Reply r")
       ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
       ("o" gnus-mime-save-part "Save attachment at point o")
       ("w" gnus-article-wide-reply "Reply all S w")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach C-c C-a")
       ("cc" message-send-and-exit "Send C-c C-c")
       ("q" nil "cancel"))
     (global-set-key (kbd "C-c C-y") 'hydra-message/body)))

;;======================ORG Mode Configs=================================

;; plantuml

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/vendor/plantuml.jar"))

;; (require 'org-trello)
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

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

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
(global-set-key (kbd "C-c f") 'proced)
(global-set-key (kbd "C-c SPC") 'whitespace-mode)

;;====================Look and Feel=====================================

(setq custom-file "/dev/null") ;; so far I manually edit emacs, no need for customization
(put 'narrow-to-region 'disabled nil) ;; narrow

(require 'highlight-parentheses) ;; highlighting
(global-highlight-parentheses-mode)
(electric-pair-mode)
(load-theme 'gruvbox-dark-hard  t)

;; color all delimiters 
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-to-list 'default-frame-alist '(font . "Fira Code 12"))
;(set-frame-font "Office Code Pro D 12" nil t)

;; key hints
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; gui remove bars etc..
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)

(require 'mode-line-bell)
(mode-line-bell-mode)

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
;; (require 'doom-modeline)
;; (doom-modeline-mode)
;; (setq doom-modeline-enable-word-count t)
;; (setq doom-modeline-buffer-encoding nil)

;; (require 'doom-themes)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; (load-theme 'doom-tomorrow-night t)

;; ;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; ;; Enable custom neotree theme (all-the-icons must be installed!)
;; (doom-themes-neotree-config)

;; ;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'org-mode)

(require 'nyan-mode)
(nyan-mode t)
(setq nyan-bar-length 24)
(setq nyan-minimum-window-width 64)
;; (display-time-mode t)
;; (display-battery-mode t)
;; (nyan-toggle-wavy-trail)
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

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner 'logo)

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)


(provide 'init)
;;; init.el ends here

