;;; ==============================================================
;;; + slime - common lisp development environment configurations +
;;; ==============================================================

(require 'slime)

;; Default common lisp complier
(setq inferior-lisp-program "sbcl")
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(require 'slime-autoloads) ; 注意这里加载的是 slime-autoloads，而不是 slime，要不然C-c C-c等很多功能都没有

(slime-setup '(slime-fancy))

(provide 'setup-slime)