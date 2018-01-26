(in-package :site-generator)

(generate-site (get-path))
(uiop:run-program "rm site/static") ; remove symlink
(uiop:run-program "cp -r ./static site/static") ; copy files over

