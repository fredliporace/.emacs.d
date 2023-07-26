# .emacs.d

My emacs configuration

## Recovering from ELPY errors in initialization

* Remove `.emacs.d/elpy/rpc-venv`
* Start emacs from base conda environment, close all buffers.
* Open test .py file and let emacs install rpc-env dependencies.
* Install rpc-env again: ```M-x elpy-rpc-reinstall-virtualenv```

## Windows cygwin notes

To install the fonts copy the ```.ttf``` files to ```/usr/share/fonts/microsoft``` and run ```/etc/postinstall/zp_fontconfig_cache_1.sh``` [reference](https://stackoverflow.com/questions/50748216/how-do-i-use-third-party-ttf-fonts-on-cygwinx-how-about-windows-fonts).

## Windows native notes

The ```.emacs.d``` must be cloned in ```C:\users\liporace\AppData\Roaming``` from git within Windows Power Shell (WPS).

To start ```ssh-agent``` use also WPS:

```
PS C:\Arquivos de Programas\Git\cmd> .\start-ssh-agent.cmd
Found ssh-agent at 12452
Failed to find ssh-agent socket
Starting ssh-agent:  done
Enter passphrase for ****:
Identity added: ***
```

Currently the push operation must be done from WPS.

## pyvenv

Example for .dir-locals.el to activate venv automatically:
```
((nil . ((pyvenv-activate . "~/ve/project"))))
```

## key bindings

```
C-x C-+: increase font size
C-x C--: decrease font size
C-x C-0: restore font size
C-x C-o: delete-blank-lines
M-\: delete-horizontal-space
```

### window management

```
C-x 0: delete selected window
C-x 1: delete all windows except selected
C-x 2: window above
C-x 3: window below
C-x ^^: increase window height
```

```
M-x switch-to-last-window
```

```
M-o: ace-window
```

```
C-t: switch cpp/hpp
```

### shell

```
M-x shell, create if not exists
C-u M-x shell, new window shell
C-<UP>, earlier command
C-<DOWN>, next later old command
```

## useful commands

### general

```
M-x desktop-*
```

```
C-x r w             write register
C-x r j             jump to register
```

```
M-x eval-region
```

```
M-x comment-region
M-x uncomment-region
```

```
M-x highlight-identation-mode
M-x aj-toggle-fold
```

### current function

```
M-x which-function-mode
```

## markdown

[Reference](https://stackoverflow.com/a/51860126/1259982)

Start an emacs' web server with `M-x httpd-start`.

Start impatient mode in the buffers you're interested to live preview: `M-x markdown-preview-like-god`.

Open your browser to `localhost:8080/imp`. You'll see the list of buffers with the mode enabled. Click on one: you see live rendering of the buffer.

## snippets

### setting vars for flycheck in .dir-locals.el

```lisp
;;; https://stackoverflow.com/questions/37720869/emacs-how-do-i-set-flycheck-to-python-3
;;; https://stackoverflow.com/questions/16237506/wrong-type-argument-listp-eval-after-load-in-dir-locals-el
((nil . ((eval .
               (custom-set-variables
                '(flycheck-python-pycompile-executable "python3")
                '(flycheck-python-pylint-executable "python3")))))
 (c++-mode
  (flycheck-gcc-include-path "/home/liporace/workspace/ms301/sync_loss_per_band/sandbox/inpe/amazonia" "/home/liporace/workspace/ms301/sync_loss_per_band/install/include/" "/usr/local/include/terralib/kernel" "/usr/local/include/terralib/shapelib" "/usr/local/lib/qt3/include")
  (flycheck-gcc-definitions "HAVE_CONFIG_H")
  ))
```

### remove hooks for a given hook variable in a given mode (in .dir-locals.el)
```lisp
((markdown-mode
  (eval remove-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)))
```

# Development

## All languages

Hide/Show block
```
C-c s
C-c h
```

Execute current buffer: `C-c C-c`

## python

Go to definition:
```
elpy-goto-definition M-.
```

Go back:
```
xref-pop-marker-stack M-,
```

List of class and function signatures
```
elpy-occur-definitions C-c C-o
```
