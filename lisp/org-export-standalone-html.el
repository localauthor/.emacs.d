;; https://emacs.stackexchange.com/questions/28781/export-to-single-html-file-like-reveal-single-filet-in-regular-html-export

(require 'org)
(require 'ox-html)
(require 'base64)

(defcustom org-html-image-base64-max-size #x40000
  "Export embedded base64 encoded images up to this size."
  :type 'number
  :group 'org-export-html)

(defun file-to-base64-string (file &optional image prefix postfix)
  "Transform binary file FILE into a base64-string prepending PREFIX and appending POSTFIX.
Puts \"data:image/%s;base64,\" with %s replaced by the image type before the actual image data if IMAGE is non-nil."
  (concat prefix
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents file nil nil nil t)
        (base64-encode-region (point-min) (point-max) 'no-line-break)
        (when image
          (goto-char (point-min))
          (insert (format "data:image/%s;base64," (image-type-from-file-name file))))
        (buffer-string))
      postfix))

(defun orgTZA-html-base64-encode-p (file)
  "Check whether FILE should be exported base64-encoded.
The return value is actually FILE with \"file://\" removed if it is a prefix of FILE."
  (when (and (stringp file)
             (string-match "\\`file://" file))
    (setq file (substring file (match-end 0))))
  (and
   (file-readable-p file)
   (let ((size (nth 7 (file-attributes file))))
     (<= size org-html-image-base64-max-size))
   file))

(defun orgTZA-html--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (if (string= "svg" (file-name-extension source))
      (org-html--svg-image source attributes info)
    (let* ((file (orgTZA-html-base64-encode-p source))
           (data (if file (file-to-base64-string file t)
                   source)))
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (org-combine-plists
         (list :src data
               :alt (if (string-match-p "^ltxpng/" source)
                        (org-html-encode-plain-text
                         (org-find-text-property-in-string 'org-latex-src source))
                      (file-name-nondirectory source)))
         attributes))
       info))))

(advice-add 'org-html--format-image :override #'orgTZA-html--format-image)
