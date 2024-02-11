;;; nspace.el --- Not workspace -*- lexical-binding: t -*-
;;
;; Filename: nspace.el
;; Description:
;; Author: Mahmoud Nagy Adam <mnagy1312@gmail.com>
;; Created: Fri Jan 19 01:09:26 2024 (+0100)
;; Version: 0.0.1
;; Package-Requires: ((ivy))

;; URL:
;; Keywords: workspace

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; nspace rely uses ivy ignore and tabs to filters buffers per tabs

;; FIXME: open file from dired don't run the hook
;; FIXME: open file from tags don't add the buffer automatically

;;; Code:

(require 'ivy)

(require 'tab-bar)

(defgroup nspace nil
  "Use tabs as basic workspace."
  :group 'convenience)

(defvar nspace-buffers 'nil)

(defcustom nspace-ask-before-kill t
  "Should nspace ask before killing the to be closed tab's buffers?"
  :type 'boolean)

(defcustom nspace-global-buffers
  '("*Messages*" "*scratch*" "*Backtrace*"
    "*Warnings*" "*Buffer List*" "*Help*")
  "Set global buffers that would appear in all tabs."
  :type 'list)

(defun nspace-filter (bname)
  "Filters ivy switch buffer, BNAME is buffer name to match."
  (unless (member bname nspace-global-buffers)
    (unless (member
	     bname
	     (assoc (tab-bar--current-tab-index) nspace-buffers))
      't)))

(defun nspace-add-buffers (buffers)
  "Add list of BUFFERS to the current tab list."
  (setf (alist-get (tab-bar--current-tab-index) nspace-buffers)
	(append
	 (alist-get (tab-bar--current-tab-index) nspace-buffers)
	 buffers)))

(defun nspace-start-buffers ()
  "Fill the nspace-buffer variable."
  (nspace-add-buffers (all-completions "" 'internal-complete-buffer nil)))

(defun nspace-remove-buffer-from-tabs (buffer)
  "Remove a BUFFER from all tabs."
  (dolist (elt nspace-buffers)
    (setf (alist-get (car elt) nspace-buffers)
	  (delete buffer (alist-get (car elt) nspace-buffers)))))

(defun nspace-add-current-buffer-on-window-change (win)
  "Add buffer to the tab local list of buffers, and remove it from other tabs.
WIN is not used for now."
  (ignore win)
  (nspace-remove-buffer-from-tabs (buffer-name))
  (nspace-add-buffers (list (buffer-name))))


(defun nspace-remove-and-shift-tabs (tab)
  "Remove TAB from the alist of buffers and shift."
  (setq nspace-buffers (assoc-delete-all tab nspace-buffers))
  (setq nspace-buffers (mapcar (lambda (elt)
				 (if (> (car elt) tab)
				     (cons (1- (car elt)) (cdr elt))
				   elt))
			       nspace-buffers)))

(defun nspace-kill (&optional kill-tab)
  "Kill space which is tab and buffers.
KILL-TAB used to kill certain tab and its buffers."
  (or kill-tab (setq kill-tab (tab-bar--current-tab-index)))
  (when (or (not nspace-ask-before-kill)
	    (y-or-n-p "Should nspace kill the buffers also?"))
    (let ((buffer-names (cdr (assoc kill-tab nspace-buffers))))
      (mapc (lambda (bname)
	      (unless (member bname nspace-global-buffers)
		(let ((buffer (get-buffer bname)))
		  (when buffer
		    (kill-buffer buffer)))))
	    buffer-names))
    (message "nspace killed associated buffers"))
  (nspace-remove-and-shift-tabs kill-tab))

(defun nspace-kill-on-tab-close (tab last)
  "Pre TAB close this will run to kill the buffer.
LAST not used."
  (ignore last)
  (nspace-kill (tab-bar--tab-index tab)))

(define-minor-mode nspace-mode
  "Enable nspace minor mode, which use tabs as workspace."
  :global t
  (if nspace-mode
      (progn
	(setq nspace-buffers 'nil)
	(add-hook 'window-selection-change-functions
		  'nspace-add-current-buffer-on-window-change)
	(add-hook 'tab-bar-tab-pre-close-functions
		  'nspace-kill-on-tab-close)
	(add-to-list 'ivy-ignore-buffers #'nspace-filter)
	(nspace-start-buffers)
	(setq tab-bar-new-tab-choice "*scratch*")
	(setq tab-bar-new-tab-to 'rightmost)
	(setq ivy-use-ignore-default 'always)
	(tab-bar-mode t)
	(message "nspace-mode enabled"))
    (progn
      (remove-hook 'window-selection-change-functions
		   'nspace-add-current-buffer-on-window-change)
      (remove-hook 'tab-bar-tab-pre-close-functions
		   'nspace-kill-on-tab-close)
      (setq ivy-ignore-buffers
	    (delq #'nspace-filter ivy-ignore-buffers))
      ;; return default values back to defaults
      (setq tab-bar-new-tab-choice t)
      (setq tab-bar-new-tab-to 'righ)
      (setq ivy-use-ignore-default t)
      (message "nspace-mode disabled"))))

(provide 'nspace)

;;; nspace.el ends here
