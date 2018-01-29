(in-package :site-generator)

(uiop:run-program "rm .database" :ignore-error-status t) ; clear database

(let* ((current-directory (uiop:getcwd))
       (output-directory (merge-pathnames "export/" current-directory)))
  (setf *site-dir* output-directory)
  (print *site-dir*)
  (generate-site current-directory))

(uiop:run-program "rm export/static" :ignore-error-status t) ; remove symlink
(uiop:run-program "cp -r ./static export/static") ; copy files over

