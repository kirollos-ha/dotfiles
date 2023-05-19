(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(package-initialize)

(setq muh-packages '(;; required by evil mode on newer emacsen
                     undo-tree
                     undo-fu
                     goto-last-change
                     ;; fundamental
                     evil
                     neotree
                     ;; almost fundamental
                     flycheck
                     auto-complete
                     ;; colours
                     color-theme-modern
                     dracula-theme
                     solarized-theme
                     color-theme-modern
                     ef-themes
nord-theme
nordless-theme
northcode-theme
nordic-night-theme
noctilux-theme
                     ;; some other 9001 installed here and not listed
                     ;; ides
                     sly
                     ;; geiser
                     ;; geiser-racket
                     ;; geiser-guile
                     ess
                     ediprolog
                     auctex
                     ;; language support
                     sxhkdrc-mode ;(not supported in older emacsen)
                     rust-mode
                     toml-mode
                     racket-mode
                     ;; utilities
                     beacon
                     ;; and this is where things get get stupid
                     yasnippet
                     yasnippet-snippets
                     company
                     olivetti
                     all-the-icons
                     treemacs
                     org-bullets
                     meow
                     ))

(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(defun potential-reinstall ()
  "look at all the packages again, installing whatever is in the list you didn't install"
  (interactive)
  (package-refresh-contents)
  (dolist (pkg muh-packages)
	(unless (package-installed-p pkg)
	  (package-install pkg))))

(setq evil-disable-insert-state-bindings t)
(require 'evil)
(setq evil-esc-delay 0.0)
(evil-mode 1)
(evil-normal-state)

(require 'neotree)
(setq neotree-smart-open t)

(with-eval-after-load 'eglot
  (progn
    (add-to-list 'eglot-server-programs
		 '(racket-mode . ("racket" "-l" "racket-langserver")))
    (add-to-list 'eglot-server-programs
		 '(js-mode . ("npx" "typescript-language-server" "--stdio")))))

(require 'company)
(add-hook 'after-init-hook  'global-company-mode)
(setq company-miminum-prefix-length 3)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

(require 'yasnippet)
(yas-global-mode 1)

(require 'ess)
(setq ac-use-quick-help nil)

(require 'beacon)
(beacon-mode 1)

(require 'rust-mode)
(require 'racket-mode)
(require 'sxhkdrc-mode)

(require 'org)

(add-to-list 'load-path "~/.emacs.d/github") ;; la onde dove si scaricano brutalmente pacchetti
(require 'org-pretty-table)

(add-hook 'org-mode-hook (lambda ()
			   (progn
			     ;; (word-wrap-whitespace-mode) ; for newer emacsen
			     (visual-line-mode) ; for older emacsen
			     (org-pretty-table-mode)
			     (org-indent-mode)
			     (org-bullets-mode))))

(add-to-list 'org-preview-latex-process-alist 'dvipng)

(require 'org-tempo)
(require 'ob-python)
(require 'ob-lisp)
(require 'ob-scheme)

(setq org-plantuml-jar-path (expand-file-name "/home/diccu/uml/plantuml.jar")) 
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(add-to-list 'org-src-lang-modes '("lisp" . lisp))
(add-to-list 'org-src-lang-modes '("python" . python))
(add-to-list 'org-src-lang-modes '("scheme" . scheme))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   (python . t)
   (lisp . t)
   (scheme . t)))

(setq org-src-window-setup 'current-window)

(setq org-babel-lisp-eval-fn 'sly-eval)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; quando capisco come funziona per non anglofoni
;;(add-hook 'LaTeX-mode-hook 'flyspell-mode) 
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(modify-coding-system-alist 'file "\\.tex\\'" 'utf-8)

(setq inferior-lisp-program "sbcl")
(setq scheme-program-name "guile3.0") ;; per racket c'è racket mode

(setq prolog-system 'swi)

(defun kill-hide()
  (interactive)
  "nasconde la finestra corrente (tipo C-x 0  on 'delete-window'), uccidendo però anche il buffer associato, per non lasciarlo erroneamente a galla"
  (let ((selwin (selected-window)))
    (kill-buffer (window-buffer selwin))
    (delete-window selwin)))

(defun docsfag-rust()
  (interactive)
  (eww-open-file "~/docs/rust/book/book/index.html"))

(defun docsfag-rust-example()
  (interactive)
  (eww-open-file "~/docs/rust/rust-by-example/book/index.html"))

;; da riscaricare, che ho reinstallato il sistema
;; probabile la funzione andrà rifatta per allora
(defun docsfag-sicp()
  (interactive)				;
  (eww-open-file "/home/diccu/Documents/lang/lisp/book/book.html"))

(defun rustdown()
  (interactive)
  (next-line 150))

(defun rustdown-ex()
  (interactive)
  (next-line 300))

(setq *snippet-shorthand-list*
      '((?b "\\mathbb" . 1)
        (?c "\\mathcal" . 1)
        (?f "\\frac" . 2)
        (?s "\\sum" . 0)
        (?l "\\lim" . 0)
        (?i "\\int" . 0)
        (?d "_" . 1)    ;down
        (?u "^" . 1)))  ;up

(defun subsnippet-from-char (c) (assoc c *snippet-shorthand-list*))
(defun subsnippet-symbol (s) (cadr s))
(defun subsnippet-arg-count (s) (cddr s))

(defun create-snippet-from-shorthand (short)
  "the short arg is a shorthand for a snippet, retuns a yasnippet snippet created from the shorthand"
  ;; input cleanup
  (setq short (string-clean-whitespace short))
  ;; now expand every char of the shorthand
  ;; some initial setting
  (let ((s-len (length short))
        (acc "")
        (index-in-snippet 1))
    ;; then iterate every char of the shorthand
    ;; appending the expansion to an accumulator
    (dotimes (i s-len)
      (let* ((c (aref short i))
             (ss (subsnippet-from-char c))
             (s-sym (subsnippet-symbol ss))
             (s-argc (subsnippet-arg-count ss)))
        (setq acc (concat acc s-sym))
        (dotimes (x s-argc)
          (setq acc (concat acc "{$" (number-to-string index-in-snippet) "}"))
          (setq index-in-snippet (1+ index-in-snippet)))))
    (concat acc "$0")))

;; TODO
  ;; provide an api for add-theme-hook, remove-theme-hook, et al
  ;; this would make for a good package
  
  (defvar theme-hooks-alist
    '((doom-opera . ((lambda ()
  		    (progn
  		   (setq doom-opera-comment-bg t)
  		   (set-face-foreground 'font-lock-comment-face "#bfbfbf")))))
      (doom-zenburn . ((lambda ()
  		      (progn
  		      (setq doom-zenburn-comment-bg t)
  		      (set-face-background 'show-paren-match "#7f8f9f")))))))
  
  (defun run-theme-hooks (theme)
    (let ((hooks-cons (assoc theme theme-hooks-alist)))
      (when hooks-cons
        (dolist (hook (cdr hooks-cons))
  	(funcall hook)))))

  (defun disable-all-themes ()
    (dolist (th custom-enabled-themes)
      (disable-theme th)))

  (defun change-theme-nonint (theme)
  ;; TODO, undo dei hook già runnati
  ;; la cosa potrebbe richiedere struct ad hoc,
;; o il caricamemento e disabilitazione di "sottotemi" custom
    (disable-all-themes)
    (load-theme theme t)
    (run-theme-hooks theme))

  (defun change-theme ()
    (interactive)
    (let ((themestr (completing-read
		     "Change to custom theme : "
		     (mapcar #'symbol-name (custom-available-themes)))))
      (change-theme-nonint (intern themestr))))

(setq muh-light-theme 'ef-trio-light)
(setq muh-dark-theme 'doom-opera)

(setq muh-terminal-light-theme 'standard)
(setq muh-terminal-dark-theme 'doom-opera)

(defun going-light () (interactive) (if (display-graphic-p)
					(disable-all-themes)
				      (change-theme-nonint muh-terminal-light-theme)))

(defun going-dark () (interactive) (if (display-graphic-p)
				       (change-theme-nonint muh-dark-theme)
				     (change-theme-nonint muh-terminal-dark-theme)))
(defun back-to-insanity () (interactive) (disable-all-themes))

(defun out-term ()
  (interactive)
  (start-process "terminal" nil "st" "-e" "zsh"))

;; a volte non riesco a usare zsh, e la cosa è abbastanza comune da richiedere un
(defun out-bash ()
  (interactive)
  (start-process "terminal" nil "st"))

;; if you want to use mupdf, xreader, firefox... change this variable
(defvar pdf-reading-program "zathura")
;; vedi se fare C-u m M-x outer-pdf magari puoi impostarlo col prefisso
;; per girare con mupdf invece di zathura, se andasse di cambiare ogni tanto
(defun out-pdf ()
  (interactive)
  (let ((file (expand-file-name
               (car (find-file-read-args "pdf: " t))))) ; car, questa funzione è strana
    (start-process "reader" nil pdf-reading-program file)))

(defun nuke-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))

(set-face-attribute 'default nil :family "Jetbrains Mono" :height 130)

(defun org-like-em-big ()
  (interactive)
  (set-face-attribute 'org-level-1 nil :height 2.00)
  (set-face-attribute 'org-level-2 nil :height 1.75)
  (set-face-attribute 'org-level-3 nil :height 1.50)
  (set-face-attribute 'org-level-4 nil :height 1.25))

(org-like-em-big)

(add-to-list 'default-frame-alist '(undecorated . t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-zenburn-brighter-comments t
        doomt-themes-enable-italic t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/everforest-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/rose-pine-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(going-dark)

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'insert (kbd "M-SPC"))

(global-set-key "\M-w" 'shell-command)
(global-set-key "\M-W" 'async-shell-command)
(evil-define-key 'normal 'global (kbd "<leader>w") 'shell-command)
(evil-define-key 'normal 'global (kbd "<leader>W") 'async-shell-command)

(global-set-key "\M-a" (lambda () (interactive) (other-window 1)))
(global-set-key "\M-A" (lambda () (interactive) (other-window -1)))
(evil-define-key 'normal 'global (kbd "<leader>a")
  (lambda () (interactive) (other-window 1)))

(with-eval-after-load 'c-mode
  (define-key c-mode-map (kbd "M-q") 'neotree-toggle))
(with-eval-after-load 'c++-mode
  (define-key c++-mode-map (kbd "M-q") 'neotree-toggle))
(with-eval-after-load 'java-mode
  (define-key java-mode-map (kbd "M-q") 'neotree-toggle))
(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "M-q") 'neotree-toggle))

(with-eval-after-load 'lisp-mode
  (define-key lisp-mode-map (kbd "M-q") 'neotree-toggle))
(with-eval-after-load 'scheme-mode
  (define-key scheme-mode-map (kbd "M-q") 'neotree-toggle))
(with-eval-after-load 'emacs-lisp-mode
  (define-key emacs-lisp-mode-map (kbd "M-q") 'neotree-toggle))
(with-eval-after-load 'typescript-mode
  (define-key typescript-mode-map (kbd "M-q") 'neotree-toggle))

(global-set-key "\C-q" 'neotree-toggle)
(evil-define-key 'normal 'global (kbd "<leader>q") 'neotree-toggle)

(evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-create-node)
(evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)

(evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-rename-node)
(evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-copy-node)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)

(global-set-key "\C-x\C-b" 'ibuffer)

(evil-define-key 'normal 'global "ç" 'evil-ex)

(global-set-key (kbd "C-c C-x C-k") 'kill-hide)

(global-set-key (kbd "C-c m")
		(lambda (s)
		  (interactive "sShorthand: ")
		  (yas-expand-snippet (create-snippet-from-shorthand s))))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)

(setq tab-width 4)
(setq scroll-conservatively most-positive-fixnum)
(show-paren-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq c-basic-offset 4)
(setq python-indent-offset 4)

(setq read-process-output-max (* 1024 1024)
      gc-cons-threshold 102400000
      gc-cons-threshold 102400000)

(setq lsp-enable-indentation nil)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; boh
(add-hook 'prog-mode-hook 'hs-minor-mode) ; potrebbe far partire tutto, occhio
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-c-headers lsp-pyright lsp-python-ms yuck-mode yasnippet-snippets use-package undo-tree undo-fu treemacs tree-sitter-langs toml-mode sxhkdrc-mode solarized-theme sly rust-mode rainbow-delimiters racket-mode orthodox-christian-new-calendar-holidays org-table-color org-bullets olivetti nordless-theme nordic-night-theme nord-theme nodejs-repl neotree meow lua-mode lsp-mode js2-mode iscroll goto-last-change flycheck evil ess eglot ef-themes ediprolog dracula-theme doom-themes company color-theme-sanityinc-tomorrow color-theme-modern beacon auto-complete auctex all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
