;; A calendar framework for Emacs

(require 'calfw-org)
(require 'calfw-cal)
(require 'calfw-ical)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    (cfw:cal-create-source "Orange") ; diary source
    (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
   ))) 

(provide 'setup-calfw)
