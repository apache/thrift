(cl:in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "asdf")
  (unless (find-package '#:asdf)
    (error "ASDF could not be required")))

(let ((indicator '#:ql-bundle-v1)
      (searcher-name '#:ql-bundle-searcher)
      (base (make-pathname :name nil :type nil
                           :defaults #. (or *compile-file-truename*
                                            *load-truename*))))
  (labels ((file-lines (file)
             (with-open-file (stream file)
               (loop for line = (read-line stream nil)
                     while line
                     collect line)))
           (relative (pathname)
             (merge-pathnames pathname base))
           (pathname-timestamp (pathname)
             #+clisp
             (nth-value 2 (ext:probe-pathname pathname))
             #-clisp
             (file-write-date pathname))
           (system-table (table pathnames)
             (dolist (pathname pathnames table)
               (setf (gethash (pathname-name pathname) table)
                     (relative pathname))))

           (initialize-bundled-systems-table (table data-source)
             (system-table table
                           (mapcar (lambda (line)
                                     (merge-pathnames line data-source))
                                   (file-lines data-source))))

           (local-projects-system-pathnames (data-source)
             (let ((files (directory (merge-pathnames "**/*.asd"
                                                      data-source))))
               (stable-sort (sort files #'string< :key #'namestring)
                            #'<
                            :key (lambda (file)
                                   (length (namestring file))))))
           (initialize-local-projects-table (table data-source)
             (system-table table (local-projects-system-pathnames data-source)))

           (make-table (&key data-source init-function)
             (let ((table (make-hash-table :test 'equalp)))
               (setf (gethash "/data-source" table)
                     data-source
                     (gethash "/timestamp" table)
                     (pathname-timestamp data-source)
                     (gethash "/init" table)
                     init-function)
               table))

           (tcall (table key &rest args)
             (let ((fun (gethash key table)))
               (unless (and fun (functionp fun))
                 (error "Unknown function key ~S" key))
               (apply fun args)))
           (created-timestamp (table)
             (gethash "/timestamp" table))
           (data-source-timestamp (table)
             (pathname-timestamp (data-source table)))
           (data-source (table)
             (gethash "/data-source" table))

           (stalep (table)
             ;; FIXME: Handle newly missing data sources?
             (< (created-timestamp table)
                (data-source-timestamp table)))
           (meta-key-p (key)
             (and (stringp key)
                  (< 0 (length key))
                  (char= (char key 0) #\/)))
           (clear (table)
             ;; Don't clear "/foo" keys
             (maphash (lambda (key value)
                        (declare (ignore value))
                        (unless (meta-key-p key)
                          (remhash key table)))
                      table))
           (initialize (table)
             (tcall table "/init" table (data-source table))
             (setf (gethash "/timestamp" table)
                   (pathname-timestamp (data-source table)))
             table)
           (update (table)
             (clear table)
             (initialize table))
           (lookup (system-name table)
             (when (stalep table)
               (update table))
             (values (gethash system-name table)))

           (search-function (system-name)
             (let ((tables (get searcher-name indicator)))
               (dolist (table tables)
                 (let* ((result (lookup system-name table))
                        (probed (and result (probe-file result))))
                   (when probed
                     (return probed))))))

           (make-bundled-systems-table ()
             (initialize
              (make-table :data-source (relative "system-index.txt")
                          :init-function #'initialize-bundled-systems-table)))
           (make-bundled-local-projects-systems-table ()
             (let ((data-source (relative "bundled-local-projects/system-index.txt")))
               (when (probe-file data-source)
                 (initialize
                  (make-table :data-source data-source
                              :init-function #'initialize-bundled-systems-table)))))
           (make-local-projects-table ()
             (initialize
              (make-table :data-source (relative "local-projects/")
                          :init-function #'initialize-local-projects-table)))

           (=matching-data-sources (tables)
             (let ((data-sources (mapcar #'data-source tables)))
               (lambda (table)
                 (member (data-source table) data-sources
                         :test #'equalp))))

           (check-for-existing-searcher (searchers)
             (block done
               (dolist (searcher searchers)
                 (when (symbolp searcher)
                   (let ((plist (symbol-plist searcher)))
                     (loop for key in plist by #'cddr
                           when
                           (and (symbolp key) (string= key indicator))
                           do
                           (setf indicator key)
                           (setf searcher-name searcher)
                           (return-from done t)))))))

           (clear-asdf (table)
             (maphash (lambda (system-name pathname)
                        (declare (ignore pathname))
                        (asdf:clear-system system-name))
                      table)))

    (let ((existing (check-for-existing-searcher
                     asdf:*system-definition-search-functions*)))
      (let* ((local (make-local-projects-table))
             (bundled-local-projects
              (make-bundled-local-projects-systems-table))
             (bundled (make-bundled-systems-table))
             (new-tables (remove nil (list local
                                           bundled-local-projects
                                           bundled)))
             (existing-tables (get searcher-name indicator))
             (filter (=matching-data-sources new-tables)))
        (setf (get searcher-name indicator)
              (append new-tables (delete-if filter existing-tables)))
        (map nil #'clear-asdf new-tables))
      (unless existing
        (setf (symbol-function searcher-name) #'search-function)
        (push searcher-name asdf:*system-definition-search-functions*)))
    t))
