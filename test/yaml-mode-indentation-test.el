;;;; test-yaml-mode.el --- ert Tests for yaml-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs -Q --batch -l ert -L . -l test/yaml-mode-indentation-test.el -f ert-run-tests-batch

(require 'ert)

(load-file "yaml-mode.el")
(require 'yaml-mode)


(ert-deftest test-yaml-mode-indentation-sanity ()
  "Test YAML indentation case 1 is a sanitycheck to make sure we understand ert files. the ert file contains a single format which is both before and after, it does not contain the optional function and we will do nothing to it so we expect this test to succeed!."
  (ert-test-erts-file "test-files/yaml-mode-sanity.erts"
                      (lambda ()
                        

                        )
                      ))
(ert-deftest test-yaml-mode-indentation-simple ()
  "the second line should not indent here."
  (ert-test-erts-file "test-files/yaml-mode-simple.erts"
                      (lambda ()

                        (yaml-mode)
                        (indent-region (point-min) (point-max)))))
(ert-deftest test-yaml-mode-indentation-comments ()
  "comments at the end of the line should not matter."
  (ert-test-erts-file "test-files/yaml-mode-comments.erts"
                      (lambda ()
                        (yaml-mode)
                        (indent-region (point-min) (point-max)))))

(ert-deftest test-yaml-mode-indentation-deep ()
  "lets have a look at a deeper nested tree"
  (ert-test-erts-file "test-files/yaml-mode-deep.erts"
                      (lambda ()
                        (yaml-mode)
                        (indent-region (point-min) (point-max)))))

(ert-deftest test-yaml-mode-indentation-quoted-strings ()
  "lets have a look at at /yaml-mode/test-files/test-quotes-in-strings.yaml using an erts file"
  (ert-test-erts-file "test-files/yaml-mode-quotes-in-strings.erts"
                      (lambda ()
                        (yaml-mode)
                        (indent-region (point-min) (point-max)))))

(ert-deftest test-yaml-mode-indentation-yaml-mode-list-items-with-dash ()
  "lets have a look at at list items  using an erts file"
  (ert-test-erts-file "test-files/yaml-mode-list-items-with-dash.erts"
                      (lambda ()
                        (yaml-mode)
                        (indent-region (point-min) (point-max)))))
(provide 'test-yaml-mode)

;;; test-yaml-mode.el ends here
