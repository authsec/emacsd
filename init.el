(setq inhibit-startup-message t) ;Don't show the start screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta) ; Map the command key as the Meta Key, this will give a similar feel on windoze keyboards
  (setq mac-option-modifier 'alt) ;
  (setq mac-right-option-modifier 'none) ; Write accents/umlauts with the right option modifier
  (setq dired-use-ls-dired nil) ; The ls command on MacOS does not support --dired
  )

(scroll-bar-mode -1) ; Disable the visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1)   ; Disable the menubar

(setq visible-bell t); Setup visible bell

;; Setup a font
(set-face-attribute 'default nil :font "PragmataPro" :height 180)

(global-visual-line-mode t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Initialize package source
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("Org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; initialize use-package
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package solarized-theme)
(load-theme 'solarized-light t)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init (unless (find-font (font-spec :name "all-the-icons"))
	  (all-the-icons-install-fonts t)))
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package command-log-mode)

;; Install sensible dependencies
(use-package swiper
  :ensure t
  )
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 )
  )
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 )
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-height 15) ; Just set this below the fontsize to be as minimal as possible
  )

;; enable line numbering
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for selected modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

(use-package general
  :config
  (general-create-definer authsec/leader-key
    :prefix "A-C-M-SPC"
    )
  )

(authsec/leader-key
  "a" 'org-agenda
  "b" 'counsel-bookmark
  )

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package org
  :custom
  (org-ellipsis " ⮷")
  :bind(
	("C-c a c" . org-capture)
	)
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))
   )

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("java" . "src java"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(setq org-agenda-files
      '("~/research/org/tasks.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-capture-templates
      '(("t" "Todo" entry (file+datetree "~/research/org/tasks.org")
	 "* TODO %?\n  %i\n  %a")))

(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "CONTACT(c)" "WAITING_FOR_RESPONSE(w)" "|" "DONE(d)")
	)

      )

(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
	(:endgroup)
	("email" . ?e)
	("phone" . ?p)
	("message" . ?m)
	)
      )

(setq my-roam-directory (concat (getenv "HOME") "/research/roam-notes"))
(setq org-roam-v2-ack t)
(use-package org-roam
  :ensure t
  :custom
  ;; make sure this directory exists
  (org-roam-directory (file-truename my-roam-directory))
  ;; configure the folder where dailies are stored, make sure this exists as well
  (org-roam-dailies-directory "dailies")
  ;; Lets you use completion-at-point
  (org-roam-completion-everywhere t)
  ;; (org-roam-graph-executable "~/bin/dot")
  :bind(
	("C-c n l" . org-roam-buffer-toggle)
	("C-c n f" . org-roam-node-find)
	("C-c n i" . org-roam-node-insert)
	:map org-mode-map
	("C-M-i" . completion-at-point)
	:map org-roam-dailies-map
	("Y" . org-roam-dailies-capture-yesterday)
	("T" . org-roam-dailies-capture-tomorrow)
	)
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure keymap is available
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  )

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(use-package org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :bind (
	 ("C-c b i" . org-ref-url-html-to-bibtex)
	 )
  :config
  (setq reftex-default-bibliography '("~/research/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/research/bibliography/notes.org")
  (setq org-ref-default-bibliography '("~/research/bibliography/references.bib"))
  (setq org-ref-pdf-directory "~/research/bibliography/bibtex-pdfs/")
  :demand t ;; Demand loading, so links work immediately
  )

(use-package deft
  :config
  (setq deft-directory my-roam-directory
	deft-recursive t
	deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
	deft-use-filename-as-title t)
  :bind
  ("C-c n s" . deft))

(setq org-latex-pdf-process
      (list
       "docker run --rm -v $\(pwd\):/docs authsec/sphinx /bin/sh -c 'pdflatex -interaction nonstopmode -shell-escape %b.tex && biber %b;  pdflatex -interaction nonstopmode -shell-escape %b.tex && pdflatex -interaction nonstopmode -shell-escape %b.tex'"
       ))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-minted-options '(("breaklines" "true")
				 ("breakanywhere" "true"))
      )

(use-package ivy-bibtex)

;; use the newer biblatex
(add-to-list 'org-latex-packages-alist '("backend=biber,sortlocale=de" "biblatex"))

;;setup dialect to be biblatex as bibtex is quite a bit old
(setq bibtex-dialect 'biblatex)
;; variables that control bibtex key format for auto-generation
;; I want firstauthor-year-title-words
;; this usually makes a legitimate filename to store pdfs under.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-classes
		'("koma-article"
		  "\\documentclass{scrartcl}"
		  ("\\section{%s}" . "\\section*{%s}")
		  ("\\subsection{%s}" . "\\subsection*{%s}")
		  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		  ("\\paragraph{%s}" . "\\paragraph*{%s}")
		  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
