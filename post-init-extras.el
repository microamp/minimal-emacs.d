;; External dependencies

(use-package agent-shell
  :ensure t)

(use-package combobulate
  :ensure t
  :vc (:url "https://github.com/mickeynp/combobulate" :branch "master")
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)
         (markdown-mode . combobulate-mode)
         (yaml-mode . combobulate-mode)))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x p q" . consult-ripgrep)
         ("M-g i" . consult-imenu)
         ("M-s M-l" . consult-line)))

(use-package diminish
  :ensure t)

;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :custom
  (denote-known-keywords '("misc" "work"))
  :config
  (setq denote-directory (expand-file-name "~/.emacs.d/notes/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package eat
  :ensure t
  :bind (("C-c C-SPC" . my/eat-switch-or-create)
         (:map eat-mode-map ("C-c C-SPC" . bury-buffer)))
  :config
  (defun my/eat-switch-or-create ()
    "Switch to an existing eat buffer, or create one in the current directory."
    (interactive)
    (if (get-buffer "*eat*")
        (switch-to-buffer "*eat*")
      (eat))))

(use-package eca
  :ensure t)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package expand-region
  :ensure t
  :bind ("C-x j" . er/expand-region)
  :custom ((expand-region-contract-fast-key "k")))

(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-variables '("GPG_TTY" "PATH"))
  :hook (emacs-startup . exec-path-from-shell-initialize))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package gptel
  :ensure t)

(use-package magit
  :ensure t)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :custom
  (magit-diff-refine-hunk t)

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package markdown-mode
  :ensure t)

(use-package mwim
  :ensure t
  :functions mwim-beginning-of-code-or-line
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package pi-coding-agent
  :ensure t
  :bind (("C-x , c" . pi-coding-agent)
         ("C-x , d" . pi-coding-agent-cycle-thinking)
         ("C-x , k" . pi-coding-agent-quit)
         ("C-x , t" . pi-coding-agent-toggle))
  :config
  (defvar pi-coding-agent-thinking-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") #'pi-coding-agent-cycle-thinking)
      map)
    "Repeat map for cycling pi thinking level.")
  (put 'pi-coding-agent-cycle-thinking 'repeat-map 'pi-coding-agent-thinking-repeat-map))

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.hcl\\'" . terraform-mode)))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package yaml-mode
  :ensure t)

;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 15) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))
