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

;;; Melpa
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl
;;     (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the-middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this warning from your init file so you won't see it again."))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

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
    material-theme
    elpy
    yaml-mode
    markdown-mode
    impatient-mode
    flycheck
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
                eshell-mode-hook))
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

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


(load-theme 'material t) ;; load material theme
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

;; Enable el-py
(elpy-enable)

;; Enable flycheck for elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable flycheck
;; http://code.litomisky.com/2014/10/24/getting-with-pylint-in-emacs/
(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-hide-results t)
 '(auto-package-update-interval 7)
 '(auto-package-update-prompt-before-update t)
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(package-selected-packages '(impatient-mode flymd flycheck flx-isearch))
 '(safe-local-variable-values
   (quote
    ((setq write-file-hooks nil)
     (eval custom-set-variables
           (quote
            (flycheck-python-pycompile-executable "python3"))
           (quote
            (flycheck-python-pylint-executable "python3")))
     (py-indent-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; pylint as default python checker
(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
;; gcc as default C++ checker
(add-hook 'c++-mode-hook #'(lambda () (setq flycheck-checker 'c/c++-gcc)))

;; markdown & friends
;; @todo check if 'use-package is used only here
(require 'use-package)
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

(provide 'init)
;;; init.el ends here
