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

(setq location-query 'nil)
(setq prayer-names '("fajr" "shrouq" "zuhr" "asr" "maghrib" "isha"))

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
    (when (> tdiff 0)
      tdiff)))

(defun call-api (url)
  (shell-command-to-string (concat "curl -s " "'" url "'")))

(defun nmawaqit-search (location)
  (call-api (concat mawaqit-api-url mawaqit-api-search location)))

(defun nmawaqit-get-prayer-times ()
  (let ((response (parse-json (nmawaqit-search location-query))))
    (setq prayer-times (assoc-value 'times (car response)))
    (setq prayer-times
	  (seq-mapn #'(lambda (a b) (cons a b)) prayer-times prayer-names))))

(defun nmawaqit-get-next-prayer ()
  (setq next-prayer 'nil) ;; reset prayer
  (dolist (prayertime prayer-times next-prayer)
    (unless next-prayer
      (setq next-prayer
	    (let ((diff (check-next-prayer
			 (decode-time (current-time))
			 (parse-time-string (car prayertime)))))
	      (when diff ;; now append time difference to prayer list
		(cons diff prayertime)))))))

(defun nmawaqit-display-color (prayermsg timeleft)
  (if (< timeleft 10)
      (setq-default mode-line-misc-info (propertize prayermsg 'face '(:foreground "red")))
    (if (< timeleft 20)
	(setq-default mode-line-misc-info (propertize prayermsg 'face '(:foreground "orange")))
      (setq-default mode-line-misc-info prayermsg))))

(defun nmawaqit-display-next-prayer()
  (nmawaqit-get-next-prayer)
  (nmawaqit-display-color (concat (number-to-string (car next-prayer))
				  "->" (cddr next-prayer) " ")
			  (car next-prayer)))


(setq nmawaqit-latlon-values '("51.071658" "13.674561"))
(defun nmawaqit-start ()
  (interactive)
  (unless nmawaqit-latlon-values
    (error "Please set nmawaqit-latlon-values first"))
  (setq location-query (format
			"?lat=%s8&lon=%s"
			(car nmawaqit-latlon-values)
			(cadr nmawaqit-latlon-values)))
  (nmawaqit-get-prayer-times)
  (nmawaqit-display-next-prayer)
  (unless nmawaqit-timer
    (setq nmawaqit-api-timer (run-with-idle-timer 120 t 'nmawaqit-get-prayer-times)))
  (unless nmawaqit-api-timer
    (setq nmawaqit-timer (run-with-timer 0 60 'nmawaqit-display-next-prayer))))

(defun nmawaqit-stop ()
  (interactive)
  (cancel-timer nmawaqit-timer)
  (cancel-timer nmawaqit-api-timer)
  (setq nmawaqit-timer 'nil)
  (setq nmawaqit-api-timer 'nil)
  (setq-default mode-line-misc-info ""))

(provide 'nmawaqit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nmawaqit.el ends here
