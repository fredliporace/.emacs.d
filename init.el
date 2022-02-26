;;; init.el --- Emacs configuration

;; Note that you'll need to run M-x package-refresh-contents
;; or M-x package-list-packages to ensure that Emacs has
;; fetched the MELPA package list before you can install
;; packages with M-x package-install or similar.

;;; This is not necessary anymore, was used to install node
;;; Adding path to packages included in the git repo,
;;; typically these are the ones not available from MELPA
;;; (add-to-list 'load-path "~/.emacs.d/nomelpa/")

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 90)
(defvar efs/default-variable-font-size 90)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(defvar myPackages
  '(better-defaults
    ;;material-theme ; old theme
    ;; elpy
    yaml-mode
    markdown-mode
    impatient-mode
    ;; flycheck
    ace-window
    irony
    pytest
    magit
    use-package))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;;; END INSTALL PACKAGES

;;; BEGIN BASIC CUSTOMIZATION

(setq inhibit-startup-message t) ;; hide the startup message

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode) ;; showing lines and columns
(global-display-line-numbers-mode t) ;; enable line numbers globally

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (use-package general
;;   :after evil
;;   :config
;;   (general-create-definer efs/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (efs/leader-keys
;;     "t"  '(:ignore t :which-key "toggles")
;;     "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;; NOTE: The first time you load your configuration on a new machine, you’ll need
;; to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; offer completion for prefixes
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/github")
    (setq projectile-project-search-path '("~/github")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

;; Configuration below is not from daviwil/emacs-from-scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not sure if this is required
;; Save registers between sessions

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(desktop-save-mode)

;; https://github.com/pashinin/workgroups2
(use-package workgroups2
  :config
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  (workgroups-mode 1)
)


;; Remove, replaced by workgroups2
;; don't use desktop mode for terminal
;; (when (display-graphic-p)
;;   (desktop-save-mode);; is x window
;;   ())
;; ;; Add variables to desktop saving
;; (add-to-list 'desktop-globals-to-save 'register-alist)

;;Old theme configuration
;;(load-theme 'material t) ;; load material theme

;; https://stackoverflow.com/questions/11700934/emacs-set-and-toggle-show-trailing-whitespace
(setq-default show-trailing-whitespace t) ;; showing trailing whitespace

;; https://stackoverflow.com/questions/19174302/emacs-only-delete-trailing-whitespace-while-saving-in-programming-mode
;;(add-hook 'write-file-hooks 'delete-trailing-whitespace) ;; remove trailing whitespace when saving
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)
(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(setq-default indent-tabs-mode nil) ;; no tabs

;; Compilation output
;; https://stackoverflow.com/questions/4657142/how-do-i-encourage-emacs-to-follow-the-compilation-buffer
(setq compilation-scroll-output t)
;;(setq compilation-scroll-output t)

;;; END BASIC CUSTOMIZATION

;;
;; Key-bindings
;;
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\C-x\C-d" 'insert-date)
(global-set-key "\C-x\C-t" 'insert-timestamp)
(global-set-key [f1] '(lambda () (interactive) (manual-entry (current-word))))
(global-set-key [f2] 'ecb-toggle-ecb-windows)
(global-set-key [f3] 'ecb-activate)
(global-set-key [f4] 'query-replace)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'next-error)
(global-set-key [f8] 'ispell-buffer)
(global-set-key [f9] 'gdb)
;; f10 goes to menu
(global-set-key [f11] 'add-change-log-entry-other-window)
(global-set-key [f12] 'next-multiframe-window)
(global-set-key "\C-z" 'undo)
;; toggle cpp/hpp files
(global-set-key "\C-t" 'ff-find-other-file)



;; set title of frame
(setq-default frame-title-format
              (list '((buffer-file-name " %f" (dired-directory
                                               dired-directory
                                               (revert-buffer-function " %b"
           ("%b - Dir:  " default-directory) ))))))

;; yank over selection
;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
(delete-selection-mode 1)

;; toggle to previous window
;; https://emacs.stackexchange.com/questions/7409/is-there-a-generic-toggle-previous-window-function
(defun switch-to-last-window ()
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

;; elpy
;; Elpy creates its own ve under ~/.emacs.d/elpy/rpc-venv. It seems to update
;; this ve when required. I'm currently using flycheck to code check.
(use-package elpy
:ensure t
:init
(elpy-enable))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; Enable flycheck for elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; Enable flycheck
;; http://code.litomisky.com/2014/10/24/getting-with-pylint-in-emacs/
(add-hook 'after-init-hook #'global-flycheck-mode)
;; pylint as default python checker
(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
;; gcc as default C++ checker
(add-hook 'c++-mode-hook #'(lambda () (setq flycheck-checker 'c/c++-gcc)))

;; python-pytest
;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :config
  ;; --runslow custom option
  (transient-append-suffix
    'python-pytest-dispatch
    '(-2)
    ["My custom options"
     ("--rs" "Run slow tests" "--runslow")]))
(global-set-key (kbd "C-x j") 'python-pytest-dispatch)


;; markdown & friends
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;;https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

;; https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

;; ace window
;; was orignally using M-p but this conflicts with 'previous error' for *compiling*
;; and *nose*
(global-set-key (kbd "M-o") 'ace-window)

;; disable C-x C-c
;; (global-unset-key (kbd "C-x C-c"))

;; irony, company-irony
;; does not work on CentOS6, requires update CMake
;; https://github.com/Sarcasm/irony-mode
;; https://github.com/Sarcasm/company-irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;; Electric (auto pair close)
(electric-pair-mode 1)
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair))
    (self-insert-command 1)))

;;  Python configuration
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))

;; Pytest
(use-package pytest)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; hideshow
(defun my-hide-all()
  (interactive)
  (hs-minor-mode)
  (hs-hide-all))
(add-hook 'prog-mode-hook 'my-hide-all)
;;hiding block of code
(global-set-key (kbd "C-c h") (kbd "C-c @ C-h"))
;;show block of code
(global-set-key (kbd "C-c s") (kbd "C-c @ C-s"))

;; tools to reload .dir-locals.el from
;; https://emacs.stackexchange.com/a/13096/31354
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

;; activating recentf-mode
(recentf-mode 1)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; don't use pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; revert buffers when the underlying file name has changed
(global-auto-revert-mode 1)
;; revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Turn debug on to check sentinel peculiar errors
;; (setq debug-on-error t)
(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
