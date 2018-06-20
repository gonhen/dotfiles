(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(require 'evil)
(evil-mode t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(defun hrs/apply-solarized-theme ()
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                  (hrs/apply-solarized-theme)))
  (hrs/apply-solarized-theme))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(when window-system
  (global-hl-line-mode))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-directory "~/Dropbox/org")
(defun org-file-path (filename)
	"Return the absolute address of an org file, given its relative name."
	(concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
	(concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files '("~/Dropbox/org"))

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

(defun gh/visit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/configuration.org"))

(global-set-key (kbd "C-c e") 'gh/visit-emacs-config)

(global-set-key (kbd "C-x k") 'gh/kill-current-buffer)

(add-hook 'after-init-hook 'global-company-mode)

(save-place-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

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
