(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package evil
  :ensure t
  :init
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(defun gh/apply-solarized-theme ()
    (setq solarized-use-variable-pitch nil)
    (setq solarized-height-plus-1 1.0)
    (setq solarized-height-plus-2 1.0)
    (setq solarized-height-plus-3 1.0)
    (setq solarized-height-plus-4 1.0)
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-dark t))

(defcustom default-light-color-theme 'solarized-light
"default light theme")

(defcustom default-dark-color-theme 'solarized-dark
"default dark theme")

(defun gh/toggle-dark-light-theme ()
(interactive)

(let ((is-light (find default-light-color-theme custom-enabled-themes)))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme (if is-light default-dark-color-theme default-light-color-theme))
  (if (org-mode) (org-mode-restart))))

(global-set-key (kbd "<f12>") 'gh/toggle-dark-light-theme)

(if (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
                (gh/apply-solarized-theme)))
(gh/apply-solarized-theme))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(when window-system
  (global-hl-line-mode))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-hierarchical-todo-statistics nil)

(setq org-directory "~/Dropbox/org")
(defun org-file-path (filename)
	"Return the absolute address of an org file, given its relative name."
	(concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
	(concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files '("~/Dropbox/org"))

(defun gh/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'gh/mark-done-and-archive)

(setq org-log-done 'time)

(setq org-capture-templates
      '(("a" "Appointment"
	 entry
	 (file  "~/Dropbox/org/calendar.org" )
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

	("b" "Blog idea"
	 entry
	 (file (org-file-path "blog-ideas.org"))
	 "* %?\n")

	("e" "Email" entry
	 (file+headline org-index-file "Inbox")
	 "* TODO %?\n\n%a\n\n")

	("f" "Finished book"
	 table-line (file "~/documents/notes/books-read.org")
	 "| %^{Title} | %^{Author} | %u |")

	("r" "Reading"
	 checkitem
	 (file (org-file-path "to-read.org")))

	("s" "Subscribe to an RSS feed"
	 plain
	 (file "~/documents/rss/urls")
	 "%^{Feed URL} \"~%^{Feed name}\"")

	("t" "Todo"
	 entry
	 (file+headline org-index-file "Inbox")
	 "* TODO %?\n")))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun gh/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'gh/open-index-file)

(setq package-check-signature nil)


(use-package org-gcal
  :ensure t
  :config
(setq org-gcal-client-id "107011808994-g9s382a66p4d3f78ibkccl15sjgh7a9n.apps.googleusercontent.com"
	  org-gcal-client-secret "Gjfci0moPki0d_APpcqEL3WF"
	  org-gcal-file-alist '(("gmv.henriques@gmail.com" .  "~/Dropbox/org/calendar.org"))))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

(use-package calfw
    :ensure t
    :config
    (use-package calfw-ical
    :ensure t
    :config
    (use-package calfw-org
    :ensure t
    :config
    (setq cfw:display-calendar-holidays nil)
    (defun mycalendar ()
      (interactive)
      (cfw:open-calendar-buffer
       :contents-sources
       (list
        (cfw:org-create-source "Green")
;	(cfw:ical-create-source "Gcal" "https://calendar.google.com/calendar/ical/gmv.henriques%40gmail.com/private-549e154258dff1844e9f91f62688c84b/basic.ics" "White")
	(cfw:ical-create-source "Feriados" "https://calendar.google.com/calendar/ical/pt-pt.portuguese%23holiday%40group.v.calendar.google.com/public/basic.ics" "Red")
	)))
  )
  )
  )

(defun gh/visit-emacs-config ()
  (interactive)
  (find-file "~/Dropbox/emacs/configuration.org"))

(global-set-key (kbd "C-c e") 'gh/visit-emacs-config)

(global-set-key (kbd "C-x k") 'gh/kill-current-buffer)

(use-package company
:ensure t
:init
(add-hook 'after-init-hook 'global-company-mode)
)

(use-package saveplace
 :ensure t
 :init
 (save-place-mode 1)
)

(setq-default indent-tabs-mode nil)

(use-package yasnippet
:ensure t
:init
  (setq yas-snippet-dirs '("~/Dropbox/emacs/snippets/text-mode"))
  (yas-global-mode 1))

(setq yas/indent-line nil)

(use-package ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

  (use-package flx-ido
    :ensure t
    :init
    (flx-ido-mode 1) ; better/faster matching
  )

(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers

  (use-package ido-vertical-mode
    :ensure t
    :init
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  )
)

(electric-pair-mode 1)

(use-package smex
  :ensure t
  :init
  (smex-initialize)
)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(defun gh/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun gh/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'gh/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'gh/split-window-right-and-switch)

(defun hrs/split-horizontally-for-temp-buffers ()
(when (one-window-p t)
  (split-window-horizontally)))

(add-hook 'temp-buffer-window-setup-hook
        'hrs/split-horizontally-for-temp-buffers)

(global-set-key
[f3]
(lambda ()
    (interactive)
    (ispell-change-dictionary "pt_PT-preao")))

(global-set-key
[f4]
(lambda ()
    (interactive)
    (ispell-change-dictionary "en")))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
