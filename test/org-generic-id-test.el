;;; org-generic-id-test.el --- Tests for org-generic-id.el -*- lexical-binding: t -*-

(require 'org-generic-id)
(require 'cl-lib)
(require 'el-mock)

(defconst org-generic-id-test--source-file
  (expand-file-name "../org-generic-id.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the org-generic-id source file.")

(defun org-generic-id-test--load-source-as-first-load ()
  "Load org-generic-id as if it has not already provided its feature."
  (let ((old-features features))
    (unwind-protect
        (progn
          (setq features (remove 'org-generic-id features))
          (load-file org-generic-id-test--source-file))
      (setq features old-features))))

(ert-deftest org-generic-id--round-trip ()
  "Verify that ‘org-generic-id-locations-save’ and
‘org-generic-id-locations-load' round-trip."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("ID"
             ("/tmp/bar" "id1" "id2" "id3")
             ("/tmp/foo" "fid"))
            ("SN"
             ("/tmp/bar" "sn1" "sna"))))
         (org-generic-id-locations
          (org-generic-id--locations-alist-to-hash
           org-generic-id-locations-alist)))
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--first-load-loads-empty-location-cache ()
  "Verify first load reads persisted locations when memory cache is empty."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("entry-id"
             ("/tmp/calendar.org" "event-1/calendar@example.com"))))
         (org-generic-id-locations (org-generic-id--make-hash-table)))
    (with-temp-file org-generic-id-locations-file
      (let ((print-level nil)
            (print-length nil))
        (pp org-generic-id-locations-alist (current-buffer))))
    (org-generic-id-test--load-source-as-first-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--first-load-keeps-populated-location-cache ()
  "Verify first load preserves an already-populated memory cache."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("entry-id"
             ("/tmp/memory.org" "event-1/calendar@example.com"))))
         (org-generic-id-locations
          (org-generic-id--locations-alist-to-hash
           org-generic-id-locations-alist)))
    (with-temp-file org-generic-id-locations-file
      (let ((print-level nil)
            (print-length nil))
        (pp '(("entry-id"
               ("/tmp/disk.org" "event-2/calendar@example.com")))
            (current-buffer))))
    (org-generic-id-test--load-source-as-first-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--find ()
  "Verify ‘org-generic-id-find’.
‘org-generic-id-locations-load' round-trip."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-file
          ;; When I rescan ID locations, the symlink "/var" is resolved to
          ;; "/private/var" on macOS. The simplest way to fix this is just to
          ;; resolve the symlink manually at the start.
          (abbreviate-file-name (file-truename (make-temp-file "my.org."))))
         (org-agenda-files (list org-file))
         (org-generic-id-extra-files nil))
    ;; Reset various variables by saving and reloading.
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    ;; We run the test in a temp buffer because ‘org-generic-id-find’ will by
    ;; default search for the ID in the current buffer if it’s not found in
    ;; ‘org-generic-id-locations’.  We want to exercise the ID search/update, so
    ;; we don’t want to fall back to the current buffer.  We thus need to switch
    ;; buffer to ‘org-file’ to update it.
    (with-temp-buffer
      (with-current-buffer (find-file-noselect org-file)
        (org-mode)
        (insert "\
* id1
:PROPERTIES:
:ID: id1
:SN: sn1
:END:"))
      (org-generic-id-update-id-locations "ID")
      (org-generic-id-update-id-locations "SN")
      ;; Round trip variables
      (org-generic-id-locations-save)
      (org-generic-id-locations-load)
      (let (m)
        (setq m (org-generic-id-find-id-in-file
                 "ID" "id1" org-file))
        (should (equal m
                       `(,org-file . 1)))
        (setq m (org-generic-id-find "ID" "id1"))
        (should (equal m
                       `(,org-file . 1)))
        (with-current-buffer (get-file-buffer org-file)
          (goto-char (point-max))
          (insert "
* id2
:PROPERTIES:
:ID: id2
:END:"))
        (setq m (org-generic-id-find "ID" "id2" nil t t))
        (should (equal m nil))
        (setq m (org-generic-id-find "ID" "id2"))
        (should (equal m
                       `(,org-file . 44)))

        (setq m (org-generic-id-find-id-in-file
                 "SN" "sn1" org-file))
        (should (equal m
                       `(,org-file . 1)))
        (setq m (org-generic-id-find "SN" "sn1"))
        (should (equal m
                       `(,org-file . 1)))
        (with-current-buffer (get-file-buffer org-file)
          (goto-char (point-max))
          (insert "
* sn2
:PROPERTIES:
:SN: sn2
:END:"))
        (setq m (org-generic-id-find "SN" "sn2" nil t t))
        (should (equal m nil))
        (setq m (org-generic-id-find "SN" "sn2"))
        (should (equal m
                       `(,org-file . 78)))))))

(ert-deftest org-generic-id--canonical-file-identity ()
  "Verify symlink aliases of one file collapse to one ID location.
The scan list, the stored ID locations, and `org-generic-id-files'
must all use the canonical name while genuinely distinct files stay
distinct."
  (let* ((dir (make-temp-file "org-generic-id-alias." t))
         (real-dir (expand-file-name "real" dir))
         (alias-dir (expand-file-name "alias" dir))
         (org-file (expand-file-name "calendar.org" real-dir))
         (alias-file (expand-file-name "calendar.org" alias-dir))
         (archive-file (expand-file-name "calendar.org_archive" real-dir))
         (canonical-file (org-generic-id--canonical-file org-file))
         (canonical-archive (org-generic-id--canonical-file archive-file))
         (org-generic-id-locations-file (expand-file-name "locations.el" dir))
         (org-generic-id-locations (org-generic-id--make-hash-table))
         (org-generic-id--last-update-id-time nil)
         (org-agenda-files nil)
         (org-generic-id-extra-files nil))
    (unwind-protect
        (progn
          (make-directory real-dir)
          (make-symbolic-link real-dir alias-dir)
          (with-temp-file org-file
            (insert "\
* Event one
:PROPERTIES:
:entry-id: event-1/calendar@example.com
:END:
* Event two
:PROPERTIES:
:entry-id: event-2/calendar@example.com
:END:
"))
          (with-temp-file archive-file
            (insert "\
* Archived event
:PROPERTIES:
:entry-id: old-1/calendar@example.com
:END:
"))
          (let (duplicates)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (let ((text (apply #'format format-string args)))
                           (when (string-match-p "Duplicate :entry-id:" text)
                             (push text duplicates))))))
              (org-generic-id-update-id-locations
               "entry-id" (list alias-file org-file archive-file)))
            (should-not duplicates))
          (should (equal (sort (org-generic-id-files) #'string<)
                         (sort (list canonical-file canonical-archive)
                               #'string<)))
          (let ((locations (gethash "entry-id" org-generic-id-locations)))
            (should (= 3 (hash-table-count locations)))
            (should (equal (gethash "event-1/calendar@example.com" locations)
                           canonical-file))
            (should (equal (gethash "event-2/calendar@example.com" locations)
                           canonical-file))
            (should (equal (gethash "old-1/calendar@example.com" locations)
                           canonical-archive)))
          ;; `org-gcal--put-id' reaches the location index through this
          ;; function, so an alias must be canonicalized here as well.
          (org-generic-id-add-location
           "entry-id" "event-3/calendar@example.com" alias-file)
          (should (equal (gethash "event-3/calendar@example.com"
                                  (gethash "entry-id" org-generic-id-locations))
                         canonical-file))
          (should (equal (sort (org-generic-id-files) #'string<)
                         (sort (list canonical-file canonical-archive)
                               #'string<))))
      (delete-directory dir t))))
