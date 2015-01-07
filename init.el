;;; ===================================================
;;; + This is where everything starts, live in Emacs! +
;;; ===================================================

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Run at all power
(setq disabled-command-function nil)

;; Run Emacs in server mode to speed up subsequent startups of Emacs significantly
(load "server")
(unless (server-running-p) (server-start))

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq themes-dir
      (expand-file-name "themes" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path themes-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" settings-dir))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Common lisp library
(require 'cl-lib)

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(ac-cider
     ac-slime
     ace-jump-mode
     achievements
     angular-snippets
     async
     auto-complete
     browse-kill-ring
     buffer-move
     buster-snippets
     calfw
     change-inner
     cider
     cl-lib-highlight
     clojure-mode
     color-theme
     css-eldoc
     dash
     datomic-snippets
     diminish
     dired-details
     dired-single
     elisp-slime-nav
     expand-region
     fill-column-indicator
     flx
     flx-ido
     flycheck
     flycheck-pos-tip
     fold-this
     frame-cmds
     frame-fns
     gist
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     groovy-mode
     guide-key
     highlight-escape-sequences
     highlight-indentation
     highlight-tail
     htmlize
     ido-at-point
     ido-ubiquitous
     ido-vertical-mode
     impatient-mode
     iy-go-to-char
     js2-refactor
     magit
     markdown-mode
     move-text
     multiple-cursors
     nodejs-repl
     paradox
     paredit
     prodigy
     restclient
     ruby-mode
     shell-command
     simple-httpd
     skewer-mode
     slime
     smart-compile
     smartparens
     smex
     smooth-scrolling
     tagedit
     undo-tree
     visual-regexp
     whitespace-cleanup-mode
     yasnippet
     zoom-frm
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Load all extra packages
(setq elisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(dolist (file (directory-files elisp-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load all user defined elisp functions
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;;;
;;;============================================================================
;;;

;; Lets start with a smattering of sanity
(require 'sane-defaults)
(require 'mode-mappings)
(require 'key-bindings)
(require 'my-theme)
(require 'my-misc)
(require 'setup-os)
(require 'setup-ido)
(require 'setup-org)
(require 'setup-dired)
(require 'setup-shell)
(require 'setup-rgrep)
(require 'setup-magit)
(require 'setup-slime)
(require 'setup-calfw)
(require 'setup-hippie)
(require 'setup-skewer)
(require 'setup-paredit)
(require 'setup-clojure)
(require 'setup-flycheck)
(require 'setup-js2-mode)
(require 'setup-html-mode)
(require 'setup-ruby-mode)
(require 'setup-yasnippet)
(require 'setup-markdown-mode)
(require 'setup-ace-jump-mode)
(require 'setup-auto-complete)

;; Additional package setup
(require 'achievements)

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))