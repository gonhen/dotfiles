;;; evil-mu4e.el --- evil-based key bindings for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Joris Engbers
;; Copyright (C) 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Homepage: https://github.com/JorisE/evil-mu4e
;; Version: 0.0.8
;; Package-Version: 20180613.339
;; Package-Requires: ((emacs "24.4") (evil "1.2.10"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; evil-mu4e keybindings for mu4e that make sense for Evil users.  The following
;; keybindings are defined:
;;
;; General commands:
;; | Commmand                 | evil-mu4e | Alternative |
;; |--------------------------+-----------+-------------|
;; | Jump to maildir          | J         |             |
;; | Update                   | u         |             |
;; | Compose message          | cc        | C           |
;; | Kill update mail process | x         |             |

;; Commands for header-mode and view-mode:
;; | Command                         | evil-mu4e | Alternative |
;; |---------------------------------+-----------+-------------|
;; | Next message                    | C-j       |             |
;; | Previous message                | C-k       |             |
;; | Mark the current thread as read | T         |             |
;; | Compose message                 | cc        | C           |
;; | Compose edit**                  | ce        | E           |
;; | Compose forward**               | cf        | F           |
;; | Compose reply                   | cr        | R           |
;; | Change sorting***               | o         | O           |
;; | Rerun search                    | gr        |             |
;; | Toggle include related          | zr        |             |
;; | Toggle threading                | zt        |             |
;; | Toggle hide cited               | za        |             |
;; | Skip duplicates                 | zd        |             |
;; | Show log                        | gl        |             |
;; | Select other view               | gv        |             |
;; | Save attachement(s)             | p         | P           |
;; | Save url                        | yu        |             |
;; | Go to url                       | gx        |             |
;; | Fetch url                       | gX        |             |
;;
;;  - * denotes only in header-mode
;;  - ** denotes Alternative only in header-mode
;;  - *** denotes Alternative only in view-mode
;;
;;; Code:

(require 'evil)
(require 'mu4e)

(defcustom evil-mu4e-state 'normal
  "State to use in mu4e buffers where keybindings are altered."
  :group 'mu4e
  :type  'symbol)


;;; By default all mu4e modes except for mu4e-compose-mode will start in
;;; evil-emacs-state. This section makes all modes start in `evil-mu4e-state'.

(defvar evil-mu4e-emacs-to-evil-mu4e-state-modes
  '(mu4e-main-mode
    mu4e-headers-mode
    mu4e-view-mode
    mu4e-org-mode)
  "Modes that should switch from Emacs state to `evil-mu4e-state'.")

(defun evil-mu4e-set-state ()
  "Associate all relevant modes with the evil-mu4e-state."
  (dolist (mode evil-mu4e-emacs-to-evil-mu4e-state-modes)
    (evil-set-initial-state mode evil-mu4e-state))
  (evil-set-initial-state 'mu4e-compose-mode 'insert))



;;; Define bindings

;; TODO: Inhibit insert-state functions as per Evil Collection.
(defvar evil-mu4e-mode-map-bindings
  `((,evil-mu4e-state mu4e-main-mode-map "J"               mu4e~headers-jump-to-maildir)
    (,evil-mu4e-state mu4e-main-mode-map "j"               next-line)
    (,evil-mu4e-state mu4e-main-mode-map "k"               previous-line)
    (,evil-mu4e-state mu4e-main-mode-map "u"               mu4e-update-mail-and-index)
    (,evil-mu4e-state mu4e-main-mode-map "gr"              revert-buffer)
    (,evil-mu4e-state mu4e-main-mode-map "b"               mu4e-headers-search-bookmark)
    (,evil-mu4e-state mu4e-main-mode-map "B"               mu4e-headers-search-bookmark-edit)
    (,evil-mu4e-state mu4e-main-mode-map "N"               mu4e-news)
    (,evil-mu4e-state mu4e-main-mode-map ";"               mu4e-context-switch)
    (,evil-mu4e-state mu4e-main-mode-map "H"               mu4e-display-manual)
    (,evil-mu4e-state mu4e-main-mode-map "C"               mu4e-compose-new)
    (,evil-mu4e-state mu4e-main-mode-map "cc"              mu4e-compose-new)
    (,evil-mu4e-state mu4e-main-mode-map "x"               mu4e-kill-update-mail)
    (,evil-mu4e-state mu4e-main-mode-map "A"               mu4e-about)
    (,evil-mu4e-state mu4e-main-mode-map "f"               smtpmail-send-queued-mail)
    (,evil-mu4e-state mu4e-main-mode-map "m"               mu4e~main-toggle-mail-sending-mode)
    (,evil-mu4e-state mu4e-main-mode-map "s"               mu4e-headers-search)
    (,evil-mu4e-state mu4e-main-mode-map "q"               mu4e-quit)

    (,evil-mu4e-state mu4e-headers-mode-map "q"            mu4e~headers-quit-buffer)
    (,evil-mu4e-state mu4e-headers-mode-map "J"            mu4e~headers-jump-to-maildir)
    (,evil-mu4e-state mu4e-headers-mode-map "C"            mu4e-compose-new)
    (,evil-mu4e-state mu4e-headers-mode-map "E"            mu4e-compose-edit)
    (,evil-mu4e-state mu4e-headers-mode-map "F"            mu4e-compose-forward)
    (,evil-mu4e-state mu4e-headers-mode-map "R"            mu4e-compose-reply)
    (,evil-mu4e-state mu4e-headers-mode-map "cc"           mu4e-compose-new)
    (,evil-mu4e-state mu4e-headers-mode-map "ce"           mu4e-compose-edit)
    (,evil-mu4e-state mu4e-headers-mode-map "cf"           mu4e-compose-forward)
    (,evil-mu4e-state mu4e-headers-mode-map "cr"           mu4e-compose-reply)
    (,evil-mu4e-state mu4e-headers-mode-map "o"            mu4e-headers-change-sorting)
    (,evil-mu4e-state mu4e-headers-mode-map "j"            mu4e-headers-next)
    (,evil-mu4e-state mu4e-headers-mode-map "k"            mu4e-headers-prev)
    (,evil-mu4e-state mu4e-headers-mode-map "gr"           mu4e-headers-rerun-search)
    (,evil-mu4e-state mu4e-headers-mode-map "b"            mu4e-headers-search-bookmark)
    (,evil-mu4e-state mu4e-headers-mode-map "B"            mu4e-headers-search-bookmark-edit)
    (,evil-mu4e-state mu4e-headers-mode-map ";"            mu4e-context-switch)
    (,evil-mu4e-state mu4e-headers-mode-map ,(kbd "RET")   mu4e-headers-view-message)
    (,evil-mu4e-state mu4e-headers-mode-map "/"            mu4e-headers-search-narrow)
    (,evil-mu4e-state mu4e-headers-mode-map "s"            mu4e-headers-search)
    (,evil-mu4e-state mu4e-headers-mode-map "S"            mu4e-headers-search-edit)
    (,evil-mu4e-state mu4e-headers-mode-map "x"            mu4e-mark-execute-all)
    (,evil-mu4e-state mu4e-headers-mode-map "a"            mu4e-headers-action)
    (,evil-mu4e-state mu4e-headers-mode-map "*"            mu4e-headers-mark-for-something) ; TODO: Don't override evil-seach-word-forward?
    (,evil-mu4e-state mu4e-headers-mode-map "&"            mu4e-headers-mark-custom)
    (,evil-mu4e-state mu4e-headers-mode-map "A"            mu4e-headers-mark-for-action)
    (,evil-mu4e-state mu4e-headers-mode-map "m"            mu4e-headers-mark-for-move)
    (,evil-mu4e-state mu4e-headers-mode-map "r"            mu4e-headers-mark-for-refile)
    (,evil-mu4e-state mu4e-headers-mode-map "D"            mu4e-headers-mark-for-delete)
    (,evil-mu4e-state mu4e-headers-mode-map "d"            mu4e-headers-mark-for-trash)
    (,evil-mu4e-state mu4e-headers-mode-map "="            mu4e-headers-mark-for-untrash)
    (,evil-mu4e-state mu4e-headers-mode-map "u"            mu4e-headers-mark-for-unmark)
    (,evil-mu4e-state mu4e-headers-mode-map "U"            mu4e-mark-unmark-all)
    (,evil-mu4e-state mu4e-headers-mode-map "?"            mu4e-headers-mark-for-unread)
    (,evil-mu4e-state mu4e-headers-mode-map "!"            mu4e-headers-mark-for-read)
    (,evil-mu4e-state mu4e-headers-mode-map "%"            mu4e-headers-mark-pattern)
    (,evil-mu4e-state mu4e-headers-mode-map "+"            mu4e-headers-mark-for-flag)
    (,evil-mu4e-state mu4e-headers-mode-map "-"            mu4e-headers-mark-for-unflag)
    (,evil-mu4e-state mu4e-headers-mode-map "["            mu4e-headers-prev-unread)
    (,evil-mu4e-state mu4e-headers-mode-map "]"            mu4e-headers-next-unread)
    (,evil-mu4e-state mu4e-headers-mode-map "gk"           mu4e-headers-prev-unread)
    (,evil-mu4e-state mu4e-headers-mode-map "gj"           mu4e-headers-next-unread)
    (,evil-mu4e-state mu4e-headers-mode-map "\C-j"         mu4e-headers-next)
    (,evil-mu4e-state mu4e-headers-mode-map "\C-k"         mu4e-headers-prev)
    (,evil-mu4e-state mu4e-headers-mode-map "zr"           mu4e-headers-toggle-include-related)
    (,evil-mu4e-state mu4e-headers-mode-map "zt"           mu4e-headers-toggle-threading)
    (,evil-mu4e-state mu4e-headers-mode-map "zd"           mu4e-headers-toggle-skip-duplicates)
    (,evil-mu4e-state mu4e-headers-mode-map "gl"           mu4e-show-log)
    (,evil-mu4e-state mu4e-headers-mode-map "gv"           mu4e-select-other-view)
    (,evil-mu4e-state mu4e-headers-mode-map "T"           (lambda ()
                                                          (interactive)
                                                          (mu4e-headers-mark-thread nil '(read))))

    ;; (,evil-mu4e-state mu4e-compose-mode-map "gg" mu4e-compose-goto-top) ; TODO: Make this work.

    (,evil-mu4e-state mu4e-view-mode-map ,(kbd "SPC")      mu4e-view-scroll-up-or-next)
    (,evil-mu4e-state mu4e-view-mode-map ,(kbd "<tab>")    shr-next-link)
    (,evil-mu4e-state mu4e-view-mode-map ,(kbd "<backtab>") shr-previous-link)
    (,evil-mu4e-state mu4e-view-mode-map "q"               mu4e~view-quit-buffer)
    (,evil-mu4e-state mu4e-view-mode-map "gx"              mu4e-view-go-to-url)
    (,evil-mu4e-state mu4e-view-mode-map "gX"              mu4e-view-fetch-url)
    (,evil-mu4e-state mu4e-view-mode-map "C"               mu4e-compose-new)
    (,evil-mu4e-state mu4e-view-mode-map "H"               mu4e-view-toggle-html)
    ;; (,evil-mu4e-state mu4e-view-mode-map "E"               mu4e-compose-edit)
    ;; (,evil-mu4e-state mu4e-view-mode-map "F"               mu4e-compose-forward)
    (,evil-mu4e-state mu4e-view-mode-map "R"               mu4e-compose-reply)
    (,evil-mu4e-state mu4e-view-mode-map "cc"              mu4e-compose-new)
    (,evil-mu4e-state mu4e-view-mode-map "ce"              mu4e-compose-edit)
    (,evil-mu4e-state mu4e-view-mode-map "cf"              mu4e-compose-forward)
    (,evil-mu4e-state mu4e-view-mode-map "cr"              mu4e-compose-reply)
    (,evil-mu4e-state mu4e-view-mode-map "p"               mu4e-view-save-attachment)
    (,evil-mu4e-state mu4e-view-mode-map "P"               mu4e-view-save-attachment-multi) ; Since mu4e 1.0, -multi is same as normal.
    (,evil-mu4e-state mu4e-view-mode-map "O"               mu4e-headers-change-sorting)
    (,evil-mu4e-state mu4e-view-mode-map "o"               mu4e-view-open-attachment)
    (,evil-mu4e-state mu4e-view-mode-map "A"               mu4e-view-attachment-action)
    (,evil-mu4e-state mu4e-view-mode-map "a"               mu4e-view-action)
    (,evil-mu4e-state mu4e-view-mode-map "J"               mu4e~headers-jump-to-maildir)
    (,evil-mu4e-state mu4e-view-mode-map "["               mu4e-view-headers-prev-unread)
    (,evil-mu4e-state mu4e-view-mode-map "]"               mu4e-view-headers-next-unread)
    (,evil-mu4e-state mu4e-view-mode-map "gk"              mu4e-view-headers-prev-unread)
    (,evil-mu4e-state mu4e-view-mode-map "gj"              mu4e-view-headers-next-unread)
    (,evil-mu4e-state mu4e-view-mode-map "\C-j"            mu4e-view-headers-next)
    (,evil-mu4e-state mu4e-view-mode-map "\C-k"            mu4e-view-headers-prev)
    (,evil-mu4e-state mu4e-view-mode-map "x"               mu4e-view-marked-execute)
    (,evil-mu4e-state mu4e-view-mode-map "&"               mu4e-view-mark-custom)
    (,evil-mu4e-state mu4e-view-mode-map "*"               mu4e-view-mark-for-something) ; TODO: Don't override "*".
    (,evil-mu4e-state mu4e-view-mode-map "m"               mu4e-view-mark-for-move)
    (,evil-mu4e-state mu4e-view-mode-map "r"               mu4e-view-mark-for-refile)
    (,evil-mu4e-state mu4e-view-mode-map "D"               mu4e-view-mark-for-delete)
    (,evil-mu4e-state mu4e-view-mode-map "d"               mu4e-view-mark-for-trash)
    (,evil-mu4e-state mu4e-view-mode-map "="               mu4e-view-mark-for-untrash)
    (,evil-mu4e-state mu4e-view-mode-map "u"               mu4e-view-unmark)
    (,evil-mu4e-state mu4e-view-mode-map "U"               mu4e-view-unmark-all)
    (,evil-mu4e-state mu4e-view-mode-map "?"               mu4e-view-mark-for-unread)
    (,evil-mu4e-state mu4e-view-mode-map "!"               mu4e-view-mark-for-read)
    (,evil-mu4e-state mu4e-view-mode-map "%"               mu4e-view-mark-pattern)
    (,evil-mu4e-state mu4e-view-mode-map "+"               mu4e-view-mark-for-flag)
    (,evil-mu4e-state mu4e-view-mode-map "-"               mu4e-view-mark-for-unflag)
    (,evil-mu4e-state mu4e-view-mode-map "\C-u"            evil-scroll-up)
    (,evil-mu4e-state mu4e-view-mode-map "zr"              mu4e-headers-toggle-include-related)
    (,evil-mu4e-state mu4e-view-mode-map "zt"              mu4e-headers-toggle-threading)
    (,evil-mu4e-state mu4e-view-mode-map "za"              mu4e-view-toggle-hide-cited)
    (,evil-mu4e-state mu4e-view-mode-map "gl"              mu4e-show-log)
    (,evil-mu4e-state mu4e-view-mode-map "s"               mu4e-view-search-edit)
    (,evil-mu4e-state mu4e-view-mode-map "|"               mu4e-view-pipe)
    (,evil-mu4e-state mu4e-view-mode-map "."               mu4e-view-raw-message)
    (,evil-mu4e-state mu4e-view-mode-map ,(kbd "C--")      mu4e-headers-split-view-shrink)
    (,evil-mu4e-state mu4e-view-mode-map ,(kbd "C-+")      mu4e-headers-split-view-grow)
    (,evil-mu4e-state mu4e-view-mode-map "T"               (lambda ()
                                                           (interactive)
                                                           (mu4e-headers-mark-thread nil '(read)))))
  ;; TODO: Add mu4e-headers-search-bookmark?
  "All evil-mu4e bindings.")

(defun evil-mu4e-set-bindings ()
  "Set the bindings."
  ;; WARNING: With lexical binding, lambdas from `mapc' and `dolist' become
  ;; closures in which we must use `evil-define-key*' instead of
  ;; `evil-define-key'.
  (dolist (binding evil-mu4e-mode-map-bindings)
    (evil-define-key*
      (nth 0 binding) (nth 1 binding) (nth 2 binding) (nth 3 binding)))
  (evil-define-key 'operator mu4e-view-mode-map
    "u" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (memq evil-this-operator
                                '(evil-yank evil-cp-yank evil-sp-yank lispyville-yank))
                      (setq evil-inhibit-operator t)
                      #'mu4e-view-save-url)))))


;;; Update mu4e-main-view
;;; To avoid confusion the main-view is updated to show the keys that are in use
;;; for evil-mu4e.

(defvar evil-mu4e-begin-region-basic "\n  Basics"
  "The place where to start overriding Basic section.")

(defvar evil-mu4e-end-region-basic "a new message\n"
  "The place where to end overriding Basic section.")

(defvar evil-mu4e-new-region-basic
  (concat (mu4e~main-action-str "\t* [J]ump to some maildir\n" 'mu4e-jump-to-maildir)
          (mu4e~main-action-str "\t* enter a [s]earch query\n" 'mu4e-search)
          (mu4e~main-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new))
  "Define the evil-mu4e Basic region.")

(defvar evil-mu4e-begin-region-misc "\n  Misc"
  "The place where to start overriding Misc section.")

(defvar evil-mu4e-end-region-misc "q]uit"
  "The place where to end overriding Misc section.")

(defvar evil-mu4e-new-region-misc
  (concat
   (mu4e~main-action-str "\t* [;]Switch focus\n" 'mu4e-context-switch)
   (mu4e~main-action-str "\t* [u]pdate email & database (Alternatively: gr)\n"
                         'mu4e-update-mail-and-index)

   ;; show the queue functions if `smtpmail-queue-dir' is defined
   (if (file-directory-p smtpmail-queue-dir)
       (mu4e~main-view-queue)
     "")
   "\n"

   (mu4e~main-action-str "\t* [N]ews\n" 'mu4e-news)
   (mu4e~main-action-str "\t* [A]bout mu4e\n" 'mu4e-about)
   (mu4e~main-action-str "\t* [H]elp\n" 'mu4e-display-manual)
   (mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit))
  "Define the evil-mu4e Misc region.")

(defun evil-mu4e-replace-region (new-region start end)
  "Replace region between START and END with NEW-REGION.
START end END end are regular expressions."
  ;; move to start of region
  (goto-char (point-min))
  (re-search-forward start)

  ;; insert new headings
  (insert "\n\n")
  (insert new-region)
  ;; Delete text until end of region.
  (let ((start-point (point))
        (end-point (re-search-forward end)))
    (delete-region start-point end-point)))


(defun evil-mu4e-update-main-view ()
  "Update 'Basic' and 'Misc' regions to reflect the new
keybindings."
  (evil-mu4e-replace-region evil-mu4e-new-region-basic
                            evil-mu4e-begin-region-basic evil-mu4e-end-region-basic)
  (evil-mu4e-replace-region evil-mu4e-new-region-misc
                            evil-mu4e-begin-region-misc evil-mu4e-end-region-misc))



;;; Initialize evil-mu4e

(defun evil-mu4e-init ()
  "Initialize evil-mu4e if necessary.
If mu4e-main-mode is in evil-state-motion-modes, initialization
is already done earlier."
    (evil-mu4e-set-state)
    (evil-mu4e-set-bindings)
    (add-hook 'mu4e-main-mode-hook 'evil-mu4e-update-main-view))

;; Evil-mu4e is only needed if mu4e is loaded.
(with-eval-after-load 'mu4e
  (evil-mu4e-init))

(provide 'evil-mu4e)
;;; evil-mu4e.el ends here
