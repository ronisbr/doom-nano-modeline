;;; doom-nano-modeline-core.el --- Core functions for doom-nano-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ronan Arraes Jardim Chagas
;;
;; This package was highly based on N Λ N O modeline by Nicolas P. Rougier
;; <Nicolas.Rougier@inria.fr>.
;;
;; Author           : Ronan Arraes Jardim Chagas
;; Created          : January 2023
;; Keywords         : mode-line, header-line
;; Package-Requires : ((emacs "28") (doom-themes "2"))
;; URL              : https://github.com/ronisbr/doom-nano-modeline
;; Version          : 0.1.0
;;
;;; License:
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Core functions for doom-nano-modeline.

;;; Code:

(defun doom-nano-modeline ()
  "Build and set the doom-nano modeline."
  (let* ((format
          '((:eval
             (funcall
              (or (catch 'found
                    (dolist (elt doom-nano-modeline-mode-formats)
                      (let* ((config (cdr elt))
                             (mode-p (plist-get config :mode-p))
                             (format (plist-get config :format)))
                        (when mode-p
                          (when (funcall mode-p)
                            (throw 'found format))))))
                  #'doom-nano-modeline-default-mode-format))))))
    (if (eq doom-nano-modeline-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))

(defun doom-nano-modeline-mode--activate ()
  "Activate the doom-nano modeline."

  ;; Save current mode-line and header-line so that we can restore if the mode
  ;; is inactivated.
  (unless doom-nano-modeline--saved-mode-line-format
    (setq doom-nano-modeline--saved-mode-line-format mode-line-format)
    (setq doom-nano-modeline--saved-header-line-format header-line-format))

  ;; Run all functions recorded in the format list using the configuration
  ;; `:on-activate'.
  (dolist (elt doom-nano-modeline-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-activate)))
      (when fn (funcall fn))))

  (run-hooks 'doom-nano-modeline-mode-format-activate-hook)

  (doom-nano-modeline--update-selected-window)

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (doom-nano-modeline)

  ;; This hooks is necessary to register selected window because when
  ;; a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'doom-nano-modeline--update-selected-window)

  (force-mode-line-update t))

(defun doom-nano-modeline-mode--inactivate ()
  "Inactivate the doom-nano modeline and restored default modeline."

  ;; Run all functions recorded in the format list using the configuration
  ;; `:on-inactivate'.
  (dolist (elt doom-nano-modeline-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-inactivate)))
      (when fn (funcall fn))))

  (run-hooks 'doom-nano-modeline-mode-format-inactivate-hook)

  (remove-hook 'post-command-hook #'doom-nano-modeline--update-selected-window)

  (setq         mode-line-format doom-nano-modeline--saved-mode-line-format)
  (setq-default mode-line-format doom-nano-modeline--saved-mode-line-format)
  (setq         header-line-format doom-nano-modeline--saved-header-line-format)
  (setq-default header-line-format doom-nano-modeline--saved-header-line-format))

(defun doom-nano-modeline--render (left right &optional hide-evil-mode)
  "Render the doom-nano modeline string.

LEFT is the information that will be rendered to the left of the modeline. RIGHT
is the information that will be rendered to the right of modeline. Both
variables must be a list in which each element has the following syntax:

    (text . face)

where TEXT will be decorated with FACE.

If HIDE-EVIL-MODE is nil, the Evil mode state is not shown in the modeline."
  (let* ((window (get-buffer-window (current-buffer)))

         ;; Variable to store if the this window is active.
         (active (and (frame-focus-state)
                      (eq window doom-nano-modeline--selected-window)))

         ;; Status of the buffer.
         (status (doom-nano-modeline-status))

         ;; Check if we are recording a macro and get its name.
         (hasmacro (or defining-kbd-macro executing-kbd-macro))
         (macroname (if (bound-and-true-p evil-this-macro)
                        (char-to-string evil-this-macro)
                      "?"))

         ;; String to indicate the current evil mode.
         (evilstate
          (if hide-evil-mode
              nil
            (concat (cond ((eq evil-state 'emacs)    "E ")
                          ((eq evil-state 'motion)   "M ")
                          ((eq evil-state 'normal)   "N ")
                          ((eq evil-state 'insert)   "I ")
                          ((eq evil-state 'replace)  "R ")
                          ((eq evil-state 'operator) "O ")
                          ((eq evil-state 'visual) (cond ((eq evil-visual-selection 'line)  "L ")
                                                         ((eq evil-visual-selection 'block) "B ")
                                                         (t                                 "V ")))
                          (t "? ")))))

         ;; String to indicate if a macro is being recorded.
         (macrostring (if hasmacro (concat "● " macroname ) nil))

         ;; Select the modeline face.
         (modeline-face (if active
                            'doom-nano-modeline-active-face
                          'doom-nano-modeline-inactive-face))

         ;; Select the face to highlight the evil state.
         (evilstate-face
          (cond (hide-evil-mode            modeline-face)
                ((not active)              modeline-face)
                ((eq evil-state 'emacs)    'doom-nano-modeline-evil-emacs-state-face)
                ((eq evil-state 'normal)   'doom-nano-modeline-evil-normal-state-face)
                ((eq evil-state 'motion)   'doom-nano-modeline-evil-motion-state-face)
                ((eq evil-state 'insert)   'doom-nano-modeline-evil-insert-state-face)
                ((eq evil-state 'replace)  'doom-nano-modeline-evil-replace-state-face)
                ((eq evil-state 'operator) 'doom-nano-modeline-evil-operator-state-face)
                ((eq evil-state 'visual)   'doom-nano-modeline-evil-visual-state-face)
                (t                         modeline-face)))

         ;; Select the face to highlight the macro recording indicator.
         (macro-face (if hasmacro 'doom-nano-modeline-macro-face modeline-face))

         ;; Assemble the left string with the highlights.
         (pleft (concat
                 (propertize " "
                             'face evilstate-face
                             'display `(raise ,doom-nano-modeline-top-padding))

                 ;; Evil state.
                 (when evilstate
                     (concat (propertize evilstate 'face evilstate-face)
                             (propertize " " 'face modeline-face)))

                 ;; Macro recording indicator.
                 (when macrostring
                     (concat (propertize macrostring 'face macro-face)
                             (propertize " " 'face modeline-face)))

                 ;; Left list.
                 (if left
                     (mapconcat
                      (lambda (element)
                        (if (and active (cdr element))
                            (propertize (car element) 'face (cdr element))
                          (propertize (car element) 'face modeline-face)))
                      left
                      "")
                   "")))

         ;; Assemble the right string with the highlights.
         (pright (concat

                  (propertize " "
                              'face modeline-face
                              'display `(raise ,(- 0 doom-nano-modeline-bottom-padding)))

                  (if right
                      (mapconcat
                       (lambda (element)
                         (if (and active (cdr element))
                             (propertize (car element) 'face (cdr element))
                           (propertize (car element) 'face modeline-face)))
                       right
                       "")
                    "")))

         ;; Compute the right string length, which is used to align the string
         ;; to the right.
         (pright-length (length (format-mode-line pright))))

    ;; Concatenate and return the modeline string.
    (concat pleft
            (propertize " "
                        'face modeline-face
                        'display `(space
                                   :align-to
                                   (- (+ right right-fringe right-margin scroll-bar)
                                      ,pright-length 1)))
            pright

            ;; We have one final space as margin, so we make sure it is
            ;; highlighted with the correct face.
            (propertize " " 'face modeline-face))))

(defun doom-nano-modeline--update-selected-window ()
  "Update selected window."
  (setq doom-nano-modeline--selected-window (selected-window)))

(provide 'doom-nano-modeline-core)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; doom-nano-modeline-core.el ends here
