;; External dependencies

(use-package expand-region
  :ensure t
  :bind ("C-x j" . er/expand-region)
  :custom ((expand-region-contract-fast-key "k")))

(use-package magit
  :ensure t)
