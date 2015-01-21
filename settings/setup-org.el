(require 'org)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; default org note file directory
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; default org-mode keybindings
(global-set-key (kbd "C-c M-l") 'org-store-link)
(global-set-key (kbd "C-c M-c") 'org-capture)
(global-set-key (kbd "C-c M-a") 'org-agenda)
(global-set-key (kbd "C-c M-b") 'org-iswitchb)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
		("TODO" . (:foreground "DeepSkyBlue" :weight bold))
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
		("CANCELLED" . (:foreground "magenta" :weight bold))
		("DEFERRED" . (:foreground "DarkGoldenrod3" :weight bold))
        ))

(provide 'setup-org)
