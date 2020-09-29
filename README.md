# .emacs.d

My emacs configuration

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

### nose

```
\C-ca nosetests-all
\C-cm nosetests-module
\C-c. nosetests-one
\C-cc nosetests-again
\C-cpa nosetests-pdb-all
\C-cpm nosetests-pdb-module
\C-cp. nosetests-pdb-one
```

To use pdb within nose:
```python
import nose.tools; nose.tools.set_trace()
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

Start an emacs' web server with `M-x httpd-start`.

Start impatient mode in the buffers you're interested to live preview: `M-x impatient-mode`.

Open your browser to `localhost:8080/imp`. You'll see the list of buffers with the mode enabled. Click on one: you see live rendering of the buffer.

To enable markdown conversion (on each buffer): `M-x imp-set-user-filter RET markdown-html RET`

## snippets

### setting vars for flycheck in .dir-locals.el

```
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
