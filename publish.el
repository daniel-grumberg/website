;;; publish.el --- Build and Org blog -*- lexical-binding: t -*-

;; Copyright (C) 2020 Daniel Grumberg <dany.grumberg@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; The entry point is dang-publish-all
;;
;;

(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(dash org-plus-contrib htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'dash)
(require 'org)
(require 'ox-rss)
(require 'ox-publish)

(defun dang/pre-postamble-format (name)
  "Formats the pre/postamble named NAME by reading a file from the snippets directory."
  `(("en" ,(with-temp-buffer
             (insert-file-contents (expand-file-name (format "%s.html" name) "./snippets"))
             (buffer-string)))))

(defun dang/head-common-list (plist)
  "List of elements going in head for all pages.  Takes PLIST as context."
  (let ((description "The personal website and blog of Daniel Grumberg"))
    (list
     "<script src=\"https://kit.fontawesome.com/9c9a6c64c4.js\" crossorigin=\"anonymous\"></script>"
     `("meta"   . (("description" . ,description)))
     '("link"   . (("rel" . "apple-touch-icon") ("sizes" . "180x180") ("href" . "/assets/apple-touch-icon.png")))
     '("link"   . (("rel" . "icon") ("type" . "image/png") ("sizes" . "32x32") ("href" . "/assets/favicon-32x32.png")))
     '("link"   . (("rel" . "icon") ("type" . "image/png") ("sizes" . "16x16") ("href" . "/assets/favicon-16x16.png")))
     '("link"   . (("rel" . "stylesheet") ("href" . "/css/code.css") ("type" . "text/css")))
     '("link"   . (("rel" . "stylesheet") ("href" . "/css/site.css") ("type" . "text/css")))
     `("link"   . (("rel" . "alternate") ("type" . "application+rss/xml") ("title" . ,description) ("href" . "/rss.xml"))))))

(defun dang/post-get-metadata-from-frontmatter (filename key)
  "Extract the KEY as`#+KEY:` from FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (ignore-errors
        (progn
          (search-forward-regexp (format "^\\#\\+%s\\:\s+\\(.+\\)$" key))
          (match-string 1))))))

(defun dang/org-html-timestamp (timestamp contents info)
  "We are not going to leak org mode silly <date> format when rendering TIMESTAMP to the world, aren't we?.  CONTENTS and INFO are passed down to org-html-timestamp."
  (let ((org-time-stamp-custom-formats
       '("%b. %d %Y" . "%b. %d %Y (%H:%M)"))
        (org-display-custom-times 't))
    (org-html-timestamp timestamp contents info)))

; We derive our own backend in order to override the timestamp format of the html backend
(org-export-define-derived-backend 'dang/html 'html
  :translate-alist
  '((timestamp . dang/org-html-timestamp)))

(defun dang/org-html-head (tags plist)
  "Generates head elements from TAGS, accepts PLIST as extra context"
  (mapconcat (lambda (elem)
               (if (stringp elem) elem
                 (let ((tag (car elem))
                       (attrs (cdr elem)))
                   (format "<%s %s/>" tag
                           (mapconcat (lambda (a)
                                        (format "%s='%s'" (car a) (cdr a))) attrs " ")))))
             tags "\n"))

(defun dang/org-html-publish-to-html (plist filename pub-dir)
  "Analog to org-html-publish-to-html using dang/html backend.  PLIST, FILENAME and PUB-DIR are passed as is."
  (plist-put plist :html-head
             (concat
              (dang/org-html-head
               (append (dang/head-common-list plist)
                       (plist-get plist :html-head-list)) plist)))
  (org-publish-org-to 'dang/html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))

(defun dang/org-html-publish-post-to-html (plist filename pub-dir)
  "wraps org-html-publish-to-html and inserts date as subtitle"
  (let ((project (cons 'blog plist)))
    (plist-put plist :subtitle
               (format "%s by %s"
                       (format-time-string "%b. %d %Y" (org-publish-find-date filename project))
                       (dang/post-get-metadata-from-frontmatter filename "AUTHOR")))
    (dang/org-html-publish-to-html plist filename pub-dir)))

(defun dang/org-html-publish-site-to-html (plist filename pub-dir)
  "Wraps org-html-publish-to-html.  Append css to hide title to PLIST and other front-page styles.  FILENAME and PUB-DIR are passed."
  (when (equal "index.org" (file-relative-name filename (plist-get plist :base-directory)))
    (plist-put plist :html-head-list
               (list
                '("link" . (("rel" . "stylesheet") ("href" . "/css/index.css"))))))
  (dang/org-html-publish-to-html plist filename pub-dir))

(defun dang/org-publish-sitemap--valid-entries (entries)
  "Filter ENTRIES that are not valid or skipped by the sitemap entry function."
  (-filter (lambda (x) (car x)) entries))

(defun dang/org-publish-sitemap-latest-posts (title sitemap)
  "posts.org generation. Only publish the latest 5 posts from SITEMAP (https://orgmode.org/manual/Sitemap.html).  Skips TITLE."
  (let* ((posts (cdr sitemap))
         (posts (dang/org-publish-sitemap--valid-entries posts))
         (last-five (seq-subseq posts 0 (min (length posts) 5))))
    (org-list-to-org (cons (car sitemap) last-five))))

(defun dang/org-publish-sitemap-archive (title sitemap)
 "archive.org page (Blog full post list). Wrapper to skip TITLE and just use LIST (https://orgmode.org/manual/Sitemap.html)."
 (let* ((title "Blog")
        (subtitle "Archive @@html:<a href='../rss.xml'><i class='fas fa-rss-square'></i></a>@@")
        (posts (cdr sitemap))
        (posts (dang/org-publish-sitemap--valid-entries posts)))
    (concat (format "#+TITLE: %s\n\n* %s\n" title subtitle)
            (org-list-to-org (cons (car sitemap) posts)))))

(defun dang/org-publish-sitemap-entry (entry style project)
  "archive.org and posts.org (latest) entry formatting. Format sitemap ENTRY for PROJECT with the post date before the link, to generate a posts list.  STYLE is not used."
  (let* ((base-directory (plist-get (cdr project) :base-directory))
         (filename (expand-file-name entry base-directory))
         (draft? (dang/post-get-metadata-from-frontmatter filename "DRAFT")))
    (unless (or (equal entry "404.org") draft?)
      (format "[[file:%s][%s]] %s"
              entry
              (org-publish-find-title entry project)
              (format-time-string "<%Y-%m-%d>" (org-publish-find-date entry project))))))

(defun dang/org-publish-rss (title sitemap)
  "Publish rss.org which needs each entry as a headline."
  (let* ((title "Blog") (subtitle "Archive")
         (posts (cdr sitemap))
         (posts (dang/org-publish-sitemap--valid-entries posts)))
    (concat (format "#+TITLE: %s\n\n" title)
            (org-list-to-subtree posts))))

(defun dang/org-publish-rss-entry (entry style project)
  "Format ENTRY for rss.org for exclusive use of exporting to RSS/XML. Each entry needs to be a headline. STYLE is not used."
  (let* ((base-directory (plist-get (cdr project) :base-directory))
         (filename (expand-file-name entry (expand-file-name base-directory (plist-get project :base-directory))))
         (draft? (dang/post-get-metadata-from-frontmatter filename "DRAFT")))
    (unless (or (equal entry "404.org") draft?)
      (format "* %s [[file:%s][%s]]"
              (format-time-string "<%Y-%m-%d>" (org-publish-find-date entry project))
              entry
              (org-publish-find-title entry project)))))

; Project definition
(defvar dang--publish-project-alist
      (list
       (list "blog"
             :base-directory "./posts"
             :exclude (regexp-opt '("posts.org" "archive.org" "rss.org"))
             :base-extension "org"
             :recursive t
             :publishing-directory "./public/posts"
             :publishing-function 'dang/org-html-publish-post-to-html
             :section-numbers nil
             :with-toc nil
             :html-preamble t
             :html-preamble-format (dang/pre-postamble-format 'preamble)
             :html-postamble t
             :html-postamble-format (dang/pre-postamble-format 'postamble)
             :html-head-include-scripts nil
             :html-head-include-default-style nil
             :auto-sitemap t
             :sitemap-filename "posts.org"
             :sitemap-style 'list
             :sitemap-title nil
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'dang/org-publish-sitemap-latest-posts
             :sitemap-format-entry 'dang/org-publish-sitemap-entry)

        (list "archive"
              :base-directory "./posts"
              :recursive t
              :exclude (regexp-opt '("posts.org" "archive.org" "rss.org"))
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function 'ignore
              :html-link-home "https://www.dangrumberg.com"
              :html-link-use-abs-url t
              :auto-sitemap t
              :sitemap-style 'list
              :sitemap-filename "archive.org"
              :sitemap-sort-files 'anti-chronologically
              :sitemap-function 'dang/org-publish-sitemap-archive
              :sitemap-format-entry 'dang/org-publish-sitemap-entry)

        (list "archive-for-rss"
              :base-directory "./posts"
              :recursive t
              :exclude (regexp-opt '("posts.org" "archive.org" "rss.org"))
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function 'ignore
              :auto-sitemap t
              :sitemap-style 'list
              :sitemap-filename "rss.org"
              :sitemap-sort-files 'anti-chronologically
              :sitemap-function 'dang/org-publish-rss
              :sitemap-format-entry 'dang/org-publish-rss-entry)

        (list "rss"
              :base-directory "./posts"
              :exclude "."
              :include '("rss.org")
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function 'org-rss-publish-to-rss
              :html-link-home "https://www.dangrumberg.com"
              :html-link-use-abs-url t)

        (list "site"
              :base-directory "./"
              :include '("posts/archive.org" "README.org")
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function 'dang/org-html-publish-site-to-html
              :section-numbers nil
              :html-preamble t
              :html-preamble-format (dang/pre-postamble-format 'preamble)
              :html-postamble t
              :html-postamble-format (dang/pre-postamble-format 'postamble)
              :html-validation-link nil
              :html-head-include-scripts nil
              :html-head-include-default-style nil)

        (list "assets"
              :base-directory "./"
              :exclude (regexp-opt '("public"))
              :include '("CNAME" "LICENSE" ".nojekyll")
              :recursive t
              :base-extension (regexp-opt '("jpg" "gif" "png" "js" "svg" "css" "pdf"))
              :publishing-directory "./public"
              :publishing-function 'org-publish-attachment)))

; Our publishing definition
(defun dang-publish-all ()
  "Publish the blog to HTML."
  (interactive)
  (let ((make-backup-files nil)
        (org-publish-project-alist       dang--publish-project-alist)
        ;; deactivate cache as it does not take the publish.el file into account
        (org-publish-cache nil)
        (org-publish-use-timestamps-flag nil)
        (org-export-with-section-numbers nil)
        (org-export-with-smart-quotes    t)
        (org-export-with-toc             nil)
        (org-export-with-sub-superscripts '{})
        (org-html-divs '((preamble  "header" "preamble")
                         (content   "main"   "content")
                         (postamble "footer" "postamble")))
        (org-html-container-element         "section")
        (org-html-metadata-timestamp-format "%d %b. %Y")
        (org-html-checkbox-type             'html)
        (org-html-html5-fancy               t)
        (org-html-validation-link           nil)
        (org-html-doctype                   "html5")
        (org-html-htmlize-output-type       'css))
    (org-publish-all)))

(provide 'publish)
;;; publish.el ends here
