;;; nmawaqit.el ---
;;
;; Filename: nmawaqit.el
;; Description:
;; Author: Mahmoud Adam
;; Maintainer:
;; Created: Thu Sep 21 09:08:26 2023 (+0000)
;; Package-Requires: ()
;;
;;; Code:

(setq mawaqit-api-url "https://mawaqit.net/api/2.0/mosque")
(setq mawaqit-api-search "/search")

(defconst nmawaqit-mode-line-string nil)
(defvar nmawaqit-timer nil)
(defvar nmawaqit-api-timer nil)

(setq location-query 'nil)
(setq prayer-names '("fajr" "shrouq" "zuhr" "asr" "maghrib" "isha"))
(setq last-prayer-update 'nil)

(defcustom nmawaqit-latlon-values 'nil
  "Set the latitude and Longitude of
position nearest to the mosque vaule in
the form of (\"51.071658\" \"13.674561\")"
  :type 'list)

(defun parse-json (string)
  (json-parse-string string :object-type 'alist :array-type 'list))

(defun assoc-value (key alist)
  (cdr (assoc key alist)))

(defun check-next-prayer (curtime tprayer)
  (let ((tdiff (+ (* 60 (- (caddr tprayer) (caddr curtime)))
		  (- (cadr tprayer) (cadr curtime)))))
    (if (< tdiff 0)
	(+ (* 24 60) tdiff)
      tdiff)))

(defun call-api (url)
  (shell-command-to-string (concat "curl -s " "'" url "'")))

(defun nmawaqit-search (location)
  (call-api (concat mawaqit-api-url mawaqit-api-search location)))

(defun nmawaqit-get-prayer-times ()
  ;; only update prayers when one day has passed from the last values
  (when (or (not last-prayer-update)
	    (< (cadddr last-prayer-update)
	       (cadddr (decode-time (current-time)))))
    (let ((response (parse-json (nmawaqit-search location-query))))
      (setq prayer-times (assoc-value 'times (car response)))
      (setq prayer-times
	    (seq-mapn #'(lambda (a b) (cons a b)) prayer-times prayer-names)))
    (setq last-prayer-update (decode-time (current-time)))))

(defun nmawaqit-get-next-prayer ()
  (setq next-prayer 'nil) ;; reset prayer
  (let ((duration (* 24 60)))
    (dolist (prayertime prayer-times next-prayer)
      (setq next-prayer
	    (let ((diff (check-next-prayer
			 (decode-time (current-time))
			 (parse-time-string (car prayertime)))))
	      (if (> duration diff)
		  (progn
		    (setq duration diff)
		    (cons diff prayertime)) ;; now append time difference to prayer list
		next-prayer))))))

(defun nmawaqit-display-color (prayermsg timeleft)
  (if (< timeleft 10)
      (setq nmawaqit-mode-line-string (propertize prayermsg 'face '(:foreground "red")))
    (if (< timeleft 20)
	(setq nmawaqit-mode-line-string (propertize prayermsg 'face '(:foreground "orange")))
      (setq nmawaqit-mode-line-string prayermsg))))

(defun nmawaqit-display-next-prayer()
  (nmawaqit-get-next-prayer)
  (nmawaqit-display-color (concat (number-to-string (car next-prayer))
				  "m->" (cddr next-prayer) " ")
			  (car next-prayer))
  (when (> 15 (car next-prayer)) (force-mode-line-update)))


(defun nmawaqit-start ()
  (unless nmawaqit-latlon-values
    (error "Please set nmawaqit-latlon-values first"))
  (setq location-query (format
			"?lat=%s8&lon=%s"
			(car nmawaqit-latlon-values)
			(cadr nmawaqit-latlon-values)))
  (nmawaqit-get-prayer-times)
  (nmawaqit-display-next-prayer)
  (unless nmawaqit-api-timer
    (setq nmawaqit-api-timer (run-with-idle-timer 30 t #'nmawaqit-get-prayer-times)))
  (unless nmawaqit-timer
    (setq nmawaqit-timer (run-at-time nil 60 #'nmawaqit-display-next-prayer))))

(defun nmawaqit-stop ()
  (when nmawaqit-timer
    (cancel-timer nmawaqit-timer))
  (when nmawaqit-api-timer
    (cancel-timer nmawaqit-api-timer))
  (setq nmawaqit-timer 'nil)
  (setq nmawaqit-api-timer 'nil)
  (setq last-prayer-update 'nil)
  (setq-default mode-line-misc-info 'nil))


(define-minor-mode nmawaqit-mode
  "Show Prayer times in mode line"
  :global t
  (setq nmawaqit-mode-line-string "")
  (unless mode-line-misc-info (setq mode-line-misc-info '("")))
  (if nmawaqit-mode
      (progn
	(add-to-list 'mode-line-misc-info 'nmawaqit-mode-line-string t)
	(nmawaqit-start)
	(force-mode-line-update)
	(message "nmawaqit started"))
    (progn
      (setq mode-line-misc-info
            (delq 'nmawaqit-mode-line-string mode-line-misc-info))
      (nmawaqit-stop)
      (message "nmawaqit stopped"))))


(provide 'nmawaqit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nmawaqit.el ends here
