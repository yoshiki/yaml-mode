;;;; yaml-mode-test.el --- Tests for yaml-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 - Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; many test utilities are copied from markdown-mode

;;; Code:

(require 'yaml-mode)
(require 'ert)

;; for version < 25
(defconst yaml-test-font-lock-function
  (if (fboundp 'font-lock-ensure)
      #'font-lock-ensure
    #'font-lock-fontify-buffer))

(defun yaml-test-font-lock--ranges (string)
  (let (ranges)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (search-forward "«" nil t)
        (let ((beg (match-beginning 0)))
          (replace-match "")
          (when (not (search-forward "»" nil t))
            (user-error "unmatched range on line %d (%d)"
                        (line-number-at-pos beg) beg))
          (let ((end (match-beginning 0)))
            (replace-match "")
            (push (cons beg end) ranges)))))
    (nreverse ranges)))

(defun yaml-test-font-lock (string faces)
  "Ensure that STRING contains specified FACES in `yaml-mode'.
STRING contains areas delimited by chevrons («...») that
correspond to FACES in the listed order."
  (declare (indent 1))
  (let ((win (selected-window))
        (ranges (yaml-test-font-lock--ranges string)))
    (when (not (= (length ranges) (length faces)))
      (user-error "Mismatch between number of ranges and specified faces."))
    (with-temp-buffer
      (set-window-buffer win (current-buffer) t)
      (insert (replace-regexp-in-string "[«»]" "" string))
      (yaml-mode)
      (funcall yaml-test-font-lock-function)
      (dolist (face faces)
        (let* ((range (pop ranges))
               (beg (car range))
               (end (1- (cdr range))))
          (yaml-test-range-has-face beg end face))))))

(defun yaml-test-report-property-range (begin end prop)
  "Report buffer substring and property PROP from BEGIN to END."
  (message "Buffer substring: %s" (buffer-substring begin (1+ end)))
  (message "Properties in range are as follows:")
  (dolist (loc (number-sequence begin end))
    (message "%d: %s" loc (get-char-property loc prop))))

(defun yaml-test-range-has-property (begin end prop value)
  "Verify that range BEGIN to END has PROP equal to or containing VALUE."
  (let ((fail-loc
         (catch 'fail
           (dolist (loc (number-sequence begin end))
             (let ((vals (get-char-property loc prop)))
               (if (and vals (listp vals))
                   (unless (memq value vals)
                     (throw 'fail loc))
                 (unless (eq vals value)
                   (throw 'fail loc))))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (yaml-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun yaml-test-range-has-face (begin end face)
  "Verify that the range from BEGIN to END has face FACE."
  (yaml-test-range-has-property begin end 'face face))

(defmacro yaml-test-string (string &rest body)
  "Run BODY in a temporary buffer containing STRING in `yaml-mode'."
  (declare (indent 1))
  `(let ((win (selected-window)))
     (with-temp-buffer
       (set-window-buffer win (current-buffer) t)
       (insert ,string)
       (yaml-mode)
       (funcall yaml-test-font-lock-function)
       (setq-default indent-tabs-mode nil)
       (goto-char (point-min))
       ,@body)))
(def-edebug-spec yaml-test-string (form body))

;;; major-mode tests:

(ert-deftest test-yaml-major-mode ()
  "Test auto-mode-alist setting."
  (dolist (extension '(".yml" ".yaml" ".eyml" ".eyaml" ".raml"))
    (let ((file (make-temp-file "a" nil extension)))
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (should (eq major-mode 'yaml-mode)))
        (delete-file file)))))

;;; Regression tests:

(ert-deftest highlighting/string-syntax ()
  "Highlighting of string syntax.
Detail: https://github.com/yoshiki/yaml-mode/issues/75
PR: https://github.com/yoshiki/yaml-mode/pull/76"
  (yaml-test-font-lock "«some's'strings'some's'nots»:
- here: «syntax is not string»
- this: «'is a string with \"quotes\"'»
- and: «'to express one single quote, use '' two of them'»
- finally: «syntax is not string»
- singlequotedoesntfreeze: '
"
    '(font-lock-variable-name-face
      nil
      font-lock-string-face
      font-lock-string-face
      nil)))

(ert-deftest highlighting/list-of-dicts-containing-literal-block ()
  "Highlighting literal blocks in list of dicts.
PR: https://github.com/yoshiki/yaml-mode/pull/81"
  (yaml-test-font-lock "example:
  - key1: «Correctly propertized»
    key2: |
      «Correctly propertized.»
  - key3: |
      «Correctly propertized»
    key4: «Incorrectly propertized as part of preceding yaml-literal-block»
"
    '(nil
      font-lock-string-face
      font-lock-string-face
      nil)))

(ert-deftest highlighting/constant-before-comment ()
  "Highlighting constant before comment.
Detail: https://github.com/yoshiki/yaml-mode/issues/96"
  (yaml-test-font-lock "services:
  - keystone:
    tls: «True»
  - horizon:
    tls: «True» # comment
  - nova:
    tls: «True#123»
"
    '(font-lock-constant-face
      font-lock-constant-face
      nil)))

(provide 'yaml-mode-test)

;;; yaml-mode-test.el ends here
