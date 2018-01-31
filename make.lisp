(in-package :site-generator)

(uiop:run-program "rm .database" :ignore-error-status t) ; clear database

(let* ((current-directory (uiop:getcwd))
       (output-directory (merge-pathnames "export/" current-directory)))
  (ensure-directories-exist output-directory)
  (setf *site-dir* output-directory)
  (generate-site current-directory))

(uiop:run-program "rm export/static" :ignore-error-status t) ; remove symlink
(uiop:run-program "rsync -r ./static/ ./export/static/") ; synchronize static directory
(uiop:run-program "rm .database" :ignore-error-status t) ; clear database
