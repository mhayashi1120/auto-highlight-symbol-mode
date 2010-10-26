auto-highlight-symbol-mode 1.0
==============================

a minor mode for emacs.   automatic highlighting symbol like [eclipse](http://www.eclipse.org) IDE.

ScreenCast
----------

available on YouTube and ScreenToaster

* YouTube -- [http://www.youtube.com/watch?v=xzJ2r4-s7fo](http://www.youtube.com/watch?v=xzJ2r4-s7fo)
* ScreenToaster -- [http://www.screentoaster.com/watch/stUE9VQ0dMRFtXRlVeU19cX1Bd/auto_highlight_symbol_mode_screencast](http://www.screentoaster.com/watch/stUE9VQ0dMRFtXRlVeU19cX1Bd/auto_highlight_symbol_mode_screencast)

Installation
------------

	cd /your-emacs-load-path/
	wget http://github.com/mitsuo-saito/auto-highlight-symbol-mode/raw/master/auto-highlight-symbol.el
	emacs -batch -f batch-byte-compile auto-highlight-symbol.el

SetUp
-----

### basic use

in your `.emacs.el`

	(require 'auto-highlight-symbol)
	(global-auto-highlight-symbol-mode t)


### set idle interval

	(require 'auto-highlight-symbol)
	(ahs-set-idle-interval 1.5 )
	(global-auto-highlight-symbol-mode t)

or

	M-x ahs-set-idle-interval <RET>


### affects whole buffer

all buffer

	(setq-default ahs-search-whole-buffer t )

buffer local (mode decision)	

	(add-hook 'emacs-lisp-mode-hook
	   (function
	     (lambda()
	       (ahs-toggle-search-whole-buffer t t))))

or

	M-x ahs-toggle-search-whole-buffer <RET>

momentary

	C-u C-x C-a

License
-------
GPLv3