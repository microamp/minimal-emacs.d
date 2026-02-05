;; Internal dependencies

;;
;; Packages:
;;

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "librewolf"))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh"))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode +1))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package frame
  :ensure nil
  :custom
  (blink-cursor-blinks 15)
  :config
  (blink-cursor-mode +1))

(use-package gnus
  :bind (:map
         gnus-group-mode-map
         ("k" . bury-buffer)
         :map
         gnus-summary-mode-map
         ("<S-return>" . gnus-summary-scroll-down))
  :custom
  (gnus-select-method '(nnnil ""))
  (gnus-permanently-visible-groups "INBOX")
  (gnus-secondary-select-methods
   '(
     ;; (nntp "news.gwene.org")
     (nnimap "home"
             (nnimap-inbox "INBOX")
             (nnimap-address "imap.migadu.com")
             (nnimap-server-port 993)
             (nnimap-stream tls)
             (nnir-search-engine imap)
             (nnmail-expiry-wait 14)
             (nnimap-split-methods default))))
  (gnus-posting-styles
   '((".*"
      (name "Sangho Na")
      (address "sangho@nsh.nz")
      (signature-file "~/.emacs.d/imap-sig-home")
      (gcc "nnimap+home:Sent")
      ("X-Message-SMTP-Method" "smtp smtp.migadu.com 465 sangho@nsh.nz"))))
  (gnus-article-browse-delete-temp t)
  (gnus-auto-select-first t)
  (gnus-extra-headers '(To Newsgroups X-GM-LABELS))
  (gnus-group-line-format "%M%S%p%P%5y:%B %G\n")
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
  (gnus-keep-backlog '0)
  (gnus-keep-backlog 'nil)
  (gnus-large-newsgroup 200)
  (gnus-mime-display-multipart-related-as-mixed t)
  (gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")
  (gnus-summary-display-arrow nil)
  (gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n")
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-treat-strip-trailing-blank-lines 'last)
  (gnus-use-cache t)
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
  (smiley-style 'medium)
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

;; Prefer vertico
(use-package icomplete
  :ensure nil
  :config
  (fido-vertical-mode -1))

(use-package isearch
  :ensure nil
  :bind (:map
         minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map
         isearch-mode-map
         ("C-g" . isearch-cancel)
         ("M-/" . isearch-complete)
         ("M-j" . isearch-yank-symbol-or-char)
         ("M-n" . isearch-yank-symbol-or-char))
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lax-whitespace t)
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (isearch-yank-on-move 'shift)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  (search-whitespace-regexp ".*?")
  :init
  (setq isearch-regexp-lax-whitespace nil))

(use-package message
  :ensure nil
  :custom
  (message-cite-reply-position 'above)
  (message-dont-reply-to-names "sangho@nsh.nz")
  :config
  (defun nsh/confirm-before-send-mail ()
    (or (yes-or-no-p "Are you sure to send this mail? ")
        (keyboard-quit)))
  (add-hook 'message-send-mail-hook #'nsh/confirm-before-send-mail))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode +1))

(use-package speedbar
  :ensure nil
  :custom
  (speedbar-use-images nil))

(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;;
;; Other customisations:
;;

;; Unbind M-x m
(global-unset-key (kbd "C-x m"))

;; Bind C-x k to kill-current-buffer (kill-buffer by default)
(global-set-key (kbd "C-x k") #'kill-current-buffer)
;; Bind C-x | to split-window-horizontally
(global-set-key (kbd "C-x |") #'split-window-horizontally)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Smarter navigation to the beginning of a line:
;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defun move-beginning-of-line-dwim (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line-dwim)

;; Cursor
(setq cursor-type 'box)

;; Font (default monospace)
(set-frame-font "monospace:pixelsize=13" nil t)

;; Theme
(mapc #'disable-theme custom-enabled-themes) ; disable all active themes
(load-theme 'modus-operandi t)               ; load built-in modus-operandi theme
