# .emacs.d

My emacs configuration

## key bindings

### window management

```
C-x 0: delete selected window
C-x 1: delete all windows except selected
C-x 2: window above
C-x 3: window below
```

```
M-x switch-to-last-window
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

## markdown

Start an emacs' web server with `M-x httpd-start`.

Start impatient mode in the buffers you're interested to live preview: `M-x impatient-mode`.

Open your browser to `localhost:8080/imp`. You'll see the list of buffers with the mode enabled. Click on one: you see live rendering of the buffer.

To enable markdown conversion (on each buffer): `M-x imp-set-user-filter RET markdown-html RET`
