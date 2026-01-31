;; Internal dependencies

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

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
(unbind "M-x m" global-map)

;; cursor
(setq cursor-type 'box)

;; font
(set-frame-font "monospace:pixelsize=13" nil t)

;; theme
(mapc #'disable-theme custom-enabled-themes) ; disable all active themes
(load-theme 'modus-operandi t)               ; load built-in modus-operandi theme
