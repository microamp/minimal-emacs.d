;; Internal dependencies

;;
;; Packages:
;;

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :custom
  ;; Poll every 1 second as a fallback (file notifications handle most cases
  ;; instantly without any polling cost).
  (auto-revert-interval 1)
  ;; Use OS file-change notifications (inotify on Linux) — zero polling cost.
  (auto-revert-use-notify t)
  ;; Don't revert while the user is typing or interacting.
  (auto-revert-stop-on-user-input t)
  :config
  (global-auto-revert-mode +1))

(use-package avoid
  :ensure nil
  :config
  (mouse-avoidance-mode 'banish))

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

(use-package treesit
  :ensure nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars))

(use-package eglot
  :ensure nil
  :hook ((python-mode python-ts-mode) . eglot-ensure)
  :hook ((typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs '((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode +1))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

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
  (gnus-keep-backlog nil)
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

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

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

(defun org-move-beginning-of-line-dwim (arg)
  "Use `move-beginning-of-line-dwim' inside src blocks, `org-beginning-of-line' elsewhere.

`org-mode-map' remaps `move-beginning-of-line' to `org-beginning-of-line', which
only provides dwim behaviour for headlines and list items — source block lines fall
straight through to a plain `move-beginning-of-line' with no back-to-indentation
toggle.  This wrapper restores the expected dwim toggle when point is inside a src
block."
  (interactive "^p")
  (if (org-in-src-block-p)
      (move-beginning-of-line-dwim arg)
    (org-beginning-of-line arg)))

(use-package org
  :ensure nil
  :bind (:map
         org-mode-map
         ("C-j" . org-return)
         ("M-N" . org-move-item-down)
         ("M-P" . org-move-item-up))
  :config
  (define-key org-mode-map [remap move-beginning-of-line]
              #'org-move-beginning-of-line-dwim)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t))))

(use-package python
  :ensure nil
  :bind (:map
         python-base-mode-map
         ("M-n" . python-nav-forward-statement)
         ("M-p" . python-nav-backward-statement)
         ("M-[" . python-nav-backward-defun)
         ("M-]" . python-nav-forward-defun)))

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

;; Cursor
(setq cursor-type 'box)

;; Font (default monospace)
(set-frame-font "monospace:pixelsize=13" nil t)

;; Theme is loaded via doric-themes in post-init-extras.el
;; (mapc #'disable-theme custom-enabled-themes) ; disable all active themes
;; (load-theme 'modus-operandi-tinted t)        ; load built-in modus-operandi theme
