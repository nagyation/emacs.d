;;========================EXWM=========================================
;; polybar

(defvar polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun kill-panel ()
  (interactive)
  (when polybar-process
    (ignore-errors
      (kill-process polybar-process)))
  (setq polybar-process nil))

(defun start-panel ()
  (interactive)
  (kill-panel)
  (setq polybar-process (start-process-shell-command "polybar" nil "polybar panel -rq")))

(defun polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun send-polybar-exwm-workspace ()
  (send-polybar-hook "exwm-workspace" 1))

  ;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'send-polybar-exwm-workspace)

(defun start-nmapplet ()
  (interactive)
  (start-process-shell-command "nm-applet" nil "nm-applet --indicator"))

;; dunst for notifications

(defun start-dunst ()
  (interactive)
  (start-process-shell-command "dunst" nil "dunst"))

(defun dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-N") (lambda () (interactive) (dunstctl "close-all")))

(defun dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-N") (lambda () (interactive) (dunstctl "close-all")))


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
            ([?\s-c] . exwm-input-release-keyboard)
            ([?\s-g] . exwm-input-grab-keyboard)
            ;; 's-&': Launch application.
            ([?\s-x] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            (,(kbd "s-i") . exwm-input-toggle-keyboard) ;; Toggle between "line-mode" and "char-mode" in an EXWM window
            (,(kbd "s-D") . kill-this-buffer)
            (,(kbd "s-f") . exwm-floating-toggle-floating) ;; Toggle the current window between floating and non-floating states
            (,(kbd "s-h") . exwm-floating-hide) ;; Toggle the current window between floating and non-floating states
            (,(kbd "s-RET") . exwm-workspace-move-window) ;; Toggle the current window between floating and non-floating states
            (,(kbd "s-s") . exwm-workspace-swap) ;; Toggle the current window between floating and non-floating states
                        
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


(require 'exwm-config)
(my-exwm-config)

(require 'exwm-xim)
(exwm-xim-enable)

;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

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

;; EXWM startup hooks:
(add-hook 'exwm-init-hook #'start-panel)
(add-hook 'exwm-init-hook #'start-nmapplet)
(add-hook 'exwm-init-hook #'start-dunst)

(require 'helm-exwm)

(setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
(setq helm-exwm-source (helm-exwm-build-source))
(setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                  helm-exwm-source
                                  helm-source-recentf))

;; Desktop Environment
(require 'desktop-environment)
(desktop-environment-mode t)

(setq desktop-environment-brightness-get-command "light")
(setq desktop-environment-brightness-set-command "light %s")
(setq desktop-environment-brightness-get-regexp "^\\([0-9]+\\)")
(setq desktop-environment-brightness-normal-increment "-A 10")
(setq desktop-environment-brightness-normal-decrement "-U 10")
(setq desktop-environment-brightness-small-increment "-A 5")
(setq desktop-environment-brightness-small-decrement "-U 5")

;; EXWM maps
(exwm-input-set-key (kbd "s-t") (lambda () (interactive) (ansi-term "bash")))
