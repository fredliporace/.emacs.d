# .emacs.d

My emacs configuration

## Key bindings

### general

```
M-x comment-region
M-x uncomment-region

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

## markdown

Start an emacs' web server with `M-x httpd-start`.

Start impatient mode in the buffers you're interested to live preview: `M-x impatient-mode`.

Open your browser to `localhost:8080/imp`. You'll see the list of buffers with the mode enabled. Click on one: you see live rendering of the buffer.

To enable markdown conversion (on each buffer): `M-x imp-set-user-filter RET markdown-html RET`


