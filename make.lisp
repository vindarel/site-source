(in-package :site-generator)

(uiop:run-program "rm .database" :ignore-error-status t) ; clear database

(let* ((current-directory (uiop:getcwd)))
  (generate-site current-directory))

(uiop:run-program "rm site/static") ; remove symlink
(uiop:run-program "cp -r ./static site/static") ; copy files over

