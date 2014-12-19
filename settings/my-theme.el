;; Initialize frame size
(setq initial-frame-alist '((top . 100) (left . 550) (width . 81) (height . 36)))

;; Use color-theme-almost-monikai as the default color theme
(load-file "~/.emacs.d/themes/color-theme-almost-monokai.el")
(color-theme-almost-monokai)
(set-face-background hl-line-face "gray21")

;; Graphically indicate the location of the fill column
(setq fci-rule-width 2)
(setq fci-rule-column 80)
(setq fci-rule-color "gray21")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Whitespace-style
(require 'whitespace)
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
		
;; Fix whitespace on save, but only if the file was clean
(global-whitespace-cleanup-mode)

;; 中文使用微软雅黑字体
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp")) 

(provide 'my-theme)
