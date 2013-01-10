;;; header-button.el --- clickable buttons in header lines

;; Copyright (C) 2010-2013  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100604
;; Version: 0.3.0
;; Homepage: https://github.com/emacsattic/header-button
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;    Starting with 24.4 (24.3.50.1 as of 20121212) this library is
;;    no longer required; the changes have been merged into Emacs.

;; This package extends `button' by adding support for adding buttons to
;; the header line.  Since the header line is very limited compared to a
;; buffer most of the functionality provided by `button' is not available
;; for buttons in the header line.

;; While `button' provides the function `insert-button' (as well as
;; others) to insert a button into a buffer at point, something similar
;; can't be done here, due to the lack of point in header lines.

;; Instead use `header-button-format' like this:
;;
;; (setq header-line-format
;;       (concat "Here's a button: "
;;               (header-button-format "Click me!" :action 'my-action)))

;; Like with `button' you can create your own derived button types:
;;
;; (define-button-type 'my-header
;;   :supertype 'header
;;   :action 'my-action)
;;
;; (setq header-line-format
;;       (concat (header-button-format "Click me!" :action 'my-action) " "
;;               (header-button-format "No me!" :type 'my-header)))

;; The function associated with `:action' is called with the button plist
;; as only argument.  Do no use `plist-get' to extract a value from it.
;; Instead use `header-button-get' which will also extract values stored
;; in it's type.
;;
;; (defun my-action (button)
;;   (message "This button labeled `%s' belongs to category `%s'"
;;            (header-button-label button)
;;            (header-button-get button 'category)))

;;; Code:

(require 'button)

(defvar header-button-map
  (let ((map (make-sparse-keymap)))
    ;; $$$ follow-link does not work here
    (define-key map [header-line mouse-1] 'header-button-push)
    (define-key map [header-line mouse-2] 'header-button-push)
    map)
  "Keymap used by buttons in header lines.")

(define-button-type 'header
  'keymap header-button-map
  'help-echo (purecopy "mouse-1: Push this button"))

(defun header-button-get (button prop)
  "Get the property of header button BUTTON named PROP."
  (let ((entry (plist-member button prop)))
    (if entry
        (cadr entry)
      (get (plist-get button 'category) prop))))

(defun header-button-label (button)
  "Return header button BUTTON's text label."
  (plist-get button 'label))

(defun header-button-format (label &rest properties)
  "Format a header button string with the label LABEL.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.
In addition, the keyword argument :type may be used to specify a
button-type from which to inherit other properties; see
`define-button-type'.

To actually create the header button set the value of variable
`header-line-format' to the string returned by this function
\(or a string created by concatenating that string with others."
  (let ((type-entry (or (plist-member properties 'type)
                        (plist-member properties :type))))
    (when (plist-get properties 'category)
      (error "Button `category' property may not be set directly"))
    (if (null type-entry)
        (setq properties
              (cons 'category
                    (cons (button-category-symbol 'header) properties)))
      (setcar type-entry 'category)
      (setcar (cdr type-entry)
              (button-category-symbol (car (cdr type-entry)))))
    (apply #'propertize label
           (nconc (list 'button (list t) 'label label) properties))))

(defun header-button-activate (button)
  "Call header button BUTTON's `:action' property."
  ;; Older versions only supported `:action' but button.el uses
  ;; `action' instead.  Now we support both and query `:action'
  ;; first because `action' defaults to function `ignore'.
  (funcall (or (header-button-get button :action)
               (header-button-get button 'action))
           button))

(defun header-button-push ()
  "Perform the action specified by the pressed header button."
  (interactive)
  (let* ((posn (event-start last-command-event))
         (object (posn-object posn))
         (buffer (window-buffer (posn-window posn)))
         (button (text-properties-at (cdr object) (car object))))
    (with-current-buffer buffer
      (header-button-activate button))))

(provide 'header-button)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; header-button.el ends here
