;; Internal dependencies

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

(use-package frame
  :ensure nil
  :custom
  (blink-cursor-blinks 15)
  :config
  (blink-cursor-mode +1))

(use-package icomplete
  :ensure nil
  :config
  (fido-vertical-mode +1))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode +1))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode +1))

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

;; Font
(set-frame-font "monospace:pixelsize=13" nil t)

;; Theme
(mapc #'disable-theme custom-enabled-themes) ; disable all active themes
(load-theme 'modus-operandi t)               ; load built-in modus-operandi theme
