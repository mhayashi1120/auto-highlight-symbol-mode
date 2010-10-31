auto-highlight-symbol-mode 1.52
===============================

A minor mode for emacs.   

* automatic highlighting current symbol like [eclipse](http://www.eclipse.org) IDE.
* cycle through highlighted locations.
* can specify the range to highlight.
* can edit the highlighted symbols at a time.

What's New in 1.5
-----------------

### Range Plugin Available

Search range is determined by plugin.

3 built-in plugins available.

* display area
* whole buffer
* current function(begnning-of-defun)

You can change the search range 
`M-x ahs-change-range <RET>` or `C-x C-'`

ScreenCast
----------

Available on YouTube and ScreenToaster.

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

Writing Plugin
--------------

### Plugin definition

If you want to define the plugin, please use this macro.

	(ahs-regist-range-plugin PLUGIN-NAME BODY &optional DOCSTRING)

### Requirement properties

The following properties are required

* `name` -- plugin name.
* `lighter` -- mode line lighter.
* `start` -- symbol search start point.
* `end` -- symbol search end point(BOUND).

return value of `name`,`lighter` must be a string. `start`,`end` must be a number.

ex. built-in whole buffer plugin

    (ahs-regist-range-plugin
		whole-buffer
        '((name    . "whole buffer")
          (lighter . " HSA")
          (start   . point-min)
          (end     . point-max))
      "Whole buffer")

### Other properties

* `init` -- executed when the plugin is selected.
* `condition` -- conditions can be selected plug-in.
* `major-mode` -- specify the major mode can be selected plug-in.
* `before-search` -- executed when before symbol search. called with the arguments(current symbol).

License
-------
GPLv3