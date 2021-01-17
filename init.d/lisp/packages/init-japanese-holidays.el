
;;;; init-japanese-holidays.el


(premise init)
(premise custom)
(premise inst-japanese-holidays)

(custom-set-variables
 '(calendar-holidays
   (append japanese-holidays holiday-local-holidays holiday-other-holidays)
   nil
   (japanese-holidays)))


(resolve init-japanese-holidays)
