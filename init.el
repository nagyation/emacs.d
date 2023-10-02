;;; init.el --- Mahmoud Adam's custom emacs config

(load "~/.emacs.d/sanemacs.el" nil t)

;;; Your configuration goes below this line.
;;; use-package is already loaded and ready to go!
;;; use-package docs: https://github.com/jwiegley/use-package


;; ========== mode line ==========
(use-package ivy
  :bind
  (("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume))
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))
;; enable this if you want `swiper' to use it
(use-package swiper
  :config
  (setq search-default-mode #'char-fold-to-regexp)
  :bind
  (("C-M-s" . swiper)))


(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ;; ("<f1> f" . counsel-describe-function)
   ;; ("<f1> v"  counsel-describe-variable)
   ;; ("<f1> o" . counsel-describe-symbol)
   ;; ("<f1> l" . counsel-find-library)
   ;; ("<f2> i" . counsel-info-lookup-symbol)
   ;; ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))


;; icomplete mode
;; (fido-mode t)
;; (fido-vertical-mode t)

(setq c-default-style "linux"
          c-basic-offset 8)

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i ?d))
  :bind
  (("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)))


(use-package switch-window
  :bind
  (("C-x o"      . switch-window)
  ("C-x <up>"    . switch-window-mvborder-up)
  ("C-x <down>"  . switch-window-mvborder-down)
  ("C-x <left>"  . switch-window-mvborder-left)
  ("C-x <right>" . switch-window-mvborder-right))
  :config
  (setq switch-window-multiple-frames nil)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "o" "e" "u" "h" "t" "n" "s" "-"
	  "i" "d" "q" "'" "," "." "c" "r" "l" "p" "y" "g")))

;; (require 'ace-window)
;; (global-set-key (kbd "C-x o") 'ace-window)
;; (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i))
;; (setq aw-background nil)

(use-package company
  :hook
  (after-init-hook . global-company-mode))


;; FlySpell
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
(add-hook hook (lambda () (flyspell-mode -1))))

(add-hook 'c-ts-mode-hook (lambda () (flyspell-prog-mode)))


;; flymake-shellcheck

(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

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

(setq org-log-done t)

(org-babel-do-load-languages 'org-babel-load-languages '((C . t)))

;; =====================================  Custom Functions =====================================================

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (undo-auto-amalgamate)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (undo-auto-amalgamate)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun mn-enlarge-window ()
  "enlarge window by 10."
  (interactive)
  (enlarge-window-horizontally 10))

(defun mn-shrink-window ()
  "shrink window by 10."
  (interactive)
  (shrink-window-horizontally 10))

(setq path-to-ctags "/usr/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s -e -R %s" path-to-ctags (file-name-concat dir-name "TAGS") (directory-file-name dir-name)))
  )


(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
        (comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (paste-to-mark arg)
  )

(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  (paste-to-mark arg)
  )

(defun custom-upcase-char ()
  "Upcase charcter and move one char"
  (interactive)
  (upcase-char 1)
  (forward-char 1))

(setq fill-column 79)
(require 'epa-file)
(epa-file-enable)

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(setq browse-url-browser-function 'eww-browse-url)

(global-set-key (kbd "C-t") 'transpose-chars)
(global-set-key (kbd "C-x ]") 'mn-enlarge-window)
(global-set-key (kbd "C-x [") 'mn-shrink-window)
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key (kbd "ESC <up>")  'move-line-up)
(global-set-key (kbd "ESC <down>")  'move-line-down)
(global-set-key (kbd "C-x C-_")  'comment-line)
(global-set-key (kbd "C-c w") 'copy-word)
(global-set-key (kbd "C-c l") 'copy-line)
(global-set-key (kbd "C-c p") 'copy-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-u") 'custom-upcase-char)


;; magit
(use-package magit
  :config
  (setq magit-save-repository-buffers nil))


;; narrow to region
(put 'narrow-to-region 'disabled nil)

;; elpy
(use-package elpy
  :config
  (setq elpy-rpc-python-command "/usr/bin/python3")
  ;; use flycheck
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; semantic, stickyfunc
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode 1)
;; (require 'stickyfunc-enhance)

;; add own custom modules
(add-to-list 'load-path "~/.emacs.d/custom/")
(use-package header2
  :ensure nil)

(use-package nmawaqit
  :ensure nil)


;; treesit modes

(use-package treesit
  :ensure nil
  :config
  (setq major-mode-remap-alist
	'((python-mode . python-ts-mode)
	  (bash-mode . bash-ts-mode)
	  (c-mode . c-ts-mode)
	  (c++-mode . c++-ts-mode)
	  (c-or-c++-mode . c-or-c++-ts-mode))))

(load-if-exists "~/.emacs.d/work.el")


;; UI changes?
(use-package  gruvbox-theme
    :config
    (load-theme 'gruvbox-dark-hard t nil))

(column-number-mode t)
(setq mode-line-position (list "%lL %cC"))

;; mac specific
(when (eq system-type `darwin)
  (setq mac-command-modifier 'meta))

(message "init is loaded correctly, you are awesome!")
