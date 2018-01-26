(in-package :site-generator)

(setf *path* (uiop:getcwd))
(setf *site-dir* (uiop:physicalize-pathname "/Users/jmercouris/Downloads/output-dir"))
(generate-site (get-path))

