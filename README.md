# Doom N Λ N O Modeline

This package contains an attempt to port [N Λ N O
Modeline](https://github.com/rougier/nano-modeline) to Doom Emacs. It has the
same design principle as the original one, but adapted to work correctly with
Doom Emacs and Evil mode.

## Installation

This package is not in MELPA yet. The recommended way to install it is using
`use-package` as follows.

Add the following code in your `packages.el`:

```emacs-lisp
(package! doom-nano-modeline
  :recipe (:host github
  :repo "ronisbr/doom-nano-modeline"))
```

And the following code in you `config.el`:

``` emacs-lisp
(use-pacakge! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))
```

It is also recommended to disable the module `modeline` in the section `ui` of
your `init.el`.

# Screenshot

The following image shows a screenshot of Doom Emacs using this package with the
Doom N Λ N O theme.

![Screenshot](./screenshots/screenshot_01.png)

## Customization

The package provides some faces to customize its design. Check the available
options under the group name `doom-nano-modeline-`.

### Customizing major modes

If you want to customize the modeline in a particular mode, you need to add a
new entry to the variable `doom-nano-modeline-mode-formats`. This new entry must
be a property list with the following entries:

- `mode-p` (**REQUIRED**): A function that returns `t` if the are in the desired
  mode or `nil` otherwise.
- `format` (**REQUIRED**): A function that returns the decorated string to be
  rendered in the modeline. Check the helper function
  `doom-nano-modeline--render` that provides an easier interface to create this
  string.
- `on-activate` (**OPTIONAL**): A function that will be run when the modeline is
  loaded.
- `on-inactivate` (**OPTIONAL**): A functions that will be run when the modeline
  is deactivated.
