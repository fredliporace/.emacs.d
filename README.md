# .emacs.d

My emacs configuration

## Key bindings

### general

M-x comment-region
M-x uncomment-region

### nose
```
(define-key nose-mode-map "\C-ca" 'nosetests-all)
(define-key nose-mode-map "\C-cm" 'nosetests-module)
(define-key nose-mode-map "\C-c." 'nosetests-one)
(define-key nose-mode-map "\C-cc" 'nosetests-again)
(define-key nose-mode-map "\C-cpa" 'nosetests-pdb-all)
(define-key nose-mode-map "\C-cpm" 'nosetests-pdb-module)
(define-key nose-mode-map "\C-cp." 'nosetests-pdb-one)
```
