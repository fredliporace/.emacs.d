;;; init.el --- Emacs configuration

;;; BEGIN INSTALL PACKAGES

(require 'package)

;;; Melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme
    elpy
    nose
    flycheck))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;;; END INSTALL PACKAGES

;;; BEGIN BASIC CUSTOMIZATION

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(setq column-number-mode t) ;; showing lines and columns
(setq-default indent-tabs-mode nil) ;; no tabs

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

;; set title of frame
(setq-default frame-title-format
              (list '((buffer-file-name " %f" (dired-directory
                                               dired-directory
                                               (revert-buffer-function " %b"
           ("%b - Dir:  " default-directory) ))))))


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
 '(package-selected-packages (quote (nose flycheck flx-isearch)))
 '(safe-local-variable-values (quote ((py-indent-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; pylint as default python checker
(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))


;; nose
(require 'nose)
;;next line only for people with non-eco non-global test runners
;;(add-to-list 'nose-project-names "my/crazy/runner")
