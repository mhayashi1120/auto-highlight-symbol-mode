auto-highlight-symbol-mode 1.51
===============================

a minor mode for emacs.   automatic highlighting current symbol like [eclipse](http://www.eclipse.org) IDE.

What's New in 1.5
-----------------

### Range Plugin Available

3 built-in plugins available

* display area
* whole buffer
* current function(begnning-of-defun)

you can change `M-x ahs-change-range <RET>` or `C-x C-'`

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

Basic SetUp
-----------

in your `.emacs.el`

	(require 'auto-highlight-symbol)
	(global-auto-highlight-symbol-mode t)

that's all.
now highligting symbol you can see.

Writing Plugin
--------------

minimum requirement

* `name` -- plugin name. must be string.
* `lighter` -- mode line lighter.
* `start` -- symbol search start point. 
* `end` -- symbol search end point(BOUND).

built-in whole buffer

    (ahs-regist-range-plugin whole-buffer
        '((name    . "whole buffer")
          (lighter . " HSA")
          (start   . point-min)
          (end     . point-max))
      "Whole buffer")

available properties

* `init` --         
* `condition` --
* `major-mode` --    
* `before-search` --

License
-------
GPLv3