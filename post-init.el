;; Internal dependencies

;;
;; Packages:
;;

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode +1))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode +1))

;;
;; Other customisations:
;;

;; Unbind M-x m
(global-unset-key (kbd "C-x m"))

;; Bind C-x k to kill-current-buffer (kill-buffer by default)
(global-set-key (kbd "C-x k") #'kill-current-buffer)
pp
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
