;;; ======================================
;;; + Emacs ace-jump-mode configurations +
;;; ======================================

(require 'ace-jump-mode)

;;
;; ace jump mode major function
;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC c") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c SPC w") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c SPC l") 'ace-jump-line-mode)

;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(provide 'setup-ace-jump-mode)
