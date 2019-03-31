;;; personal-blog.el --- My static blog.

;; Copyright (C) 2014 Yu Yang

;;; Author: Yu Yang <yy2012cn@NOSPAM.gmail.com>
;;; URL: https://github.com/yuyang0/

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;; Code:
(with-eval-after-load 'org
  (require 's)
  ;; markdown export for emacs25
  (require 'ox-md nil t)

  ;; stop treating [n] as footnote
  (require 'ox-publish)
  (defun my-ignore-false-footnotes (ast backend info)
    (org-element-map ast 'footnote-reference
      (lambda (f)
        (let ((label (org-element-property :label f)))
          (when (org-string-match-p "\\`[0-9]+\\'" label)
            (org-element-set-element
             f
             (concat "[" label "]"
                     (make-string (org-element-property :post-blank f) ?\s)))))))
    ast)

  (add-to-list 'org-export-filter-parse-tree-functions #'my-ignore-false-footnotes)

  (defgroup personal-blog nil
    "Personal static blog."
    :group 'editing)

  (defcustom personal-blog-home-link "https://yuyang0.github.io/"
    "The home link of personal static blog."
    :type 'string
    :group 'personal-blog)

  (defvar header-str "<div id=\"header\">
      <div class=\"inner\">
        <h1 id=\"site-title\"><a href=\"/\"> 编码者言 </a></h1>
        <div>
          <a href=\"###\" id=\"site-nav-btn\">菜单</a>
          <ul id=\"site-nav\" class=\"vertical-nav mobi-hid\">
            <li> <a href=\"/\">Home</a></li>
            <li> <a href=\"/about.html\">About</a></li>

            <li> <a href=\"/tags.html\">Tags</a></li>

            <li> <a href=\"/atom.xml\">RSS</a></li>
          </ul>

          <form id=\"site-search\" method=\"get\" action=\"https://google.com/search\">
            <input type=\"hidden\" name=\"q\" value=\"site:yuyang0.github.io\" />
            <input type=\"text\" name=\"q\" placeholder=\"Search...\" />
            <button class=\"btn-search\" type=\"submit\">Search</button>
          </form>
          <div style=\"clear:both;\"> </div>
        </div>
      </div>
    </div>
")

  (defvar disqus-str "<div id=\"disqus_comment\">
<div id=\"disqus_thread\"></div>
<script>
    /**
     *  RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
     *  LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
     */

    var disqus_config = function () {
        var url_path = window.location.pathname;
        var disqus_identifier = url_path.substring(url_path.lastIndexOf('/')+1);
        disqus_identifier? disqus_identifier: 'index';

        var full_url = window.location.href.replace(/^http:/, 'https:');
        this.page.url = full_url;
        this.page.identifier = disqus_identifier;
        // this.page.title = document.title;
    };

    (function() {  // REQUIRED CONFIGURATION VARIABLE: EDIT THE SHORTNAME BELOW
        var d = document, s = d.createElement('script');

        s.src = '//yuyang.disqus.com/embed.js';  // IMPORTANT: Replace EXAMPLE with your forum shortname!

        s.setAttribute('data-timestamp', +new Date());
        (d.head || d.body).appendChild(s);
    })();
</script>
<noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">comments powered by Disqus.</a></noscript>
</div>
")

  (defvar duoshuo-str "<!-- 多说评论框 start -->
  <div id=\"duoshuo-id\" class=\"ds-thread\" data-thread-key=\"请将此处替换成文章在你的站点中的ID\" data-title=\"请替换成文章的标题\" data-url=\"请替换成文章的网址\"></div>
<!-- 多说评论框 end -->
<!-- 多说公共JS代码 start (一个网页只需插入一次) -->
<script type=\"text/javascript\">
   var url = window.location.pathname;
   var duoshuo_identifier = url.substring(url.lastIndexOf('/')+1);
   if (!!! duoshuo_identifier) {
     duoshuo_identifier = \"index.html\";
   }
   var title = document.title;
   var ele = document.getElementById(\"duoshuo-id\");
   ele.setAttribute(\"data-thread-key\", duoshuo_identifier);
   ele.setAttribute(\"data-title\", title);
   ele.setAttribute(\"data-url\", url);

</script>

<script type=\"text/javascript\">
var duoshuoQuery = {short_name:\"yuyang0\"};
  (function() {
    var ds = document.createElement('script');
    ds.type = 'text/javascript';ds.async = true;
    ds.src = (document.location.protocol == 'https:' ? 'https:' : 'http:') + '//static.duoshuo.com/embed.js';
    ds.charset = 'UTF-8';
    (document.getElementById('postamble')
     || document.getElementsByTagName('body')[0]).appendChild(ds);
  })();
  </script>
<!-- 多说公共JS代码 end -->")

  (defvar footer-str "<!-- begin footer -->
<div id=\"footer\">
  <ul class=\"links vertical-nav\">
    <li><a href=\"/sitemap.xml\">Sitemap</a></li>
    <li><a href=\"/atom.xml\">RSS</a></li>
    <li><a href=\"/about.html\">About Me</a></li>
    <li><a class=\"back-to-top\" href=\"#\">Back to Top</a></li>
  </ul>
  <span>© 2013 Yu Yang's Blog, Created by org-mode and dropbox</span>
  <a href=\"#\" class=\"back-to-top\" id=\"fixed-back-to-top\" ></a>
</div>
<script src=\"//apps.bdimg.com/libs/jquery/2.0.0/jquery.min.js\"></script>
<script type=\"text/javascript\">
	window.jQuery || document.write('<script src=\"static/js/jquery-2.0.0.min.js\"><\\/script>');
	</script>
<script type=\"text/javascript\" src=\"static/js/custom.js\"></script>
<!-- end footer -->")

  (setq org-publish-project-alist
        `(
          ("blog-notes"
           :base-directory "~/Documents/note/"
           :base-extension "org"
           :publishing-directory "~/Documents/blog/"
           :exclude "others"              ;skip the others directory
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 5
           :section-numbers nil
           :auto-preamble t
           :auto-sitemap t
           ;; :sitemap-function org-publish-sitemap-rss-tags
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap"
           :author "Yu Yang"
           :email "yy2012cn@gmail.com"
           :html-head  "<link rel=\"stylesheet\" type=\"text/css\" href=\"static/css/main.css\"/>
<link rel=\"shortcut icon\" href=\"static/img/favicon.ico\" />"
           :html-preamble ,header-str
           :html-postamble ,(concat disqus-str footer-str)
           )
          ("blog-static"
           :base-directory "~/Documents/note/"
           :base-extension "css\\|js\\|pdf\\|png\\|jpg\\|gif\\|mp3\\|ogg\\|swf\\|ico\\|svg"
           :publishing-directory "~/Documents/blog/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ;;("blog" :components ("root" "notes" "articles" "static"))
          ("blog" :components ("blog-notes" "blog-static"))
          ))

  (defun org-publish-sitemap-rss-tags (project &optional sitemap-filename)
    "Create sitemap.xml and rss feed for `PROJECT'."
    (org-publish-sitemap project sitemap-filename)
    (org-publish-sitemap-xml project)
    (org-publish-rss-xml project)
    (org-publish-tags project))

  (defun org-publish-sitemap-xml (project &optional sitemap-filename)
    "Create sitemap.xml for `PROJECT'."
    (let* ((project-plist (cdr project))
           (base-dir (file-name-as-directory
                      (expand-file-name (plist-get project-plist :base-directory))))
           (pub-dir (file-name-as-directory
                     (expand-file-name (plist-get project-plist :publishing-directory))))
           (exclude-regexp (plist-get project-plist :exclude))
           (files (nreverse
                   (org-publish-get-base-files project exclude-regexp)))
           (base-link personal-blog-home-link)
           (sitemap-filename (expand-file-name "sitemap.xml" pub-dir))
           (visiting (find-buffer-visiting sitemap-filename))
           sitemap-buffer)

      (with-current-buffer (setq sitemap-buffer
                                 (or visiting (find-file sitemap-filename)))
        (erase-buffer)
        (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>

<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
")
        (dolist (afile files)
          (let* ((cur-url (concat base-link
                                  (replace-regexp-in-string "\.org$" "\.html"
                                                            (file-relative-name afile base-dir))))
                 (entry-str (format "<url>
      <loc>%s</loc>
      <lastmod>%s</lastmod>
      <changefreq>weekly</changefreq>
      <priority>0.5</priority>
   </url>
" cur-url (format-time-string "%Y-%m-%d" (org-publish-find-date afile)))))
            (insert entry-str)))
        (insert "</urlset>")
        (save-buffer))
      (or visiting (kill-buffer sitemap-buffer))))

  (defun org-publish-rss-xml (project &optional sitemap-filename)
    "Create rss feed for `PROJECT'."
    (let* ((project-plist (cdr project))
           (base-dir (file-name-as-directory
                      (expand-file-name (plist-get project-plist :base-directory))))
           (pub-dir (file-name-as-directory
                     (expand-file-name (plist-get project-plist :publishing-directory))))
           (exclude-regexp (plist-get project-plist :exclude))
           (files (nreverse (org-publish-get-base-files project exclude-regexp)))
           (base-link personal-blog-home-link)
           (rss-filename (expand-file-name "atom.xml" pub-dir))
           (visiting (find-buffer-visiting rss-filename))
           rss-buffer)

      (with-current-buffer (setq rss-buffer
                                 (or visiting (find-file rss-filename)))
        (erase-buffer)
        (insert (format "<?xml version=\"1.0\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">

  <title>编码者言</title>
  <link href=\"%s\"/>
  <link type=\"application/atom+xml\" rel=\"self\" href=\"/atom.xml\"/>
  <updated>%s</updated>
  <id>%s</id>
  <author>
    <name>Yu Yang</name>
    <email>yy2012cn@gmail.com</email>
  </author>
" base-link (current-time-string) base-link))

        (dolist (afile files)
          ;; only include files in articles directory.
          (when (s-match "articles" afile)
              (let* ((id (file-name-nondirectory afile))
                     (title (org-publish-find-title afile))
                     (cur-url (replace-regexp-in-string "\.org$" "\.html"
                                                        (concat base-link (file-relative-name afile base-dir))))
                     (entry-str (format "<entry>
    <id>%s</id>
    <link type=\"text/html\" rel=\"alternate\" href=\"%s\"/>
    <title>%s</title>
    <updated>%s</updated>
    <author>
      <name>Yu Yang</name>
      <uri>%s</uri>
    </author>
    <content type=\"html\"> </content>
  </entry>" id cur-url title
  (format-time-string " %Y-%m-%d" (org-publish-find-date afile)) base-link)))
                (insert entry-str))))
        (insert "</feed>")
        (save-buffer))
      (or visiting (kill-buffer rss-buffer))))


  (defun org-publish-find-keywords (file &optional reset)
    "Find the KEYWORDS of FILE in project."
     (let* ((visiting (find-buffer-visiting file))
            (buffer (or visiting (find-file-noselect file))))
       (with-current-buffer buffer
         (let ((keywords
                (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward "#\\+KEYWORDS:" 500 t 1)
                      (buffer-substring-no-properties (point) (line-end-position))
                    ""))
                ))
           (unless visiting (kill-buffer buffer))
           (s-trim keywords )))))

  ;; (org-publish-find-keywords "~/Documents/note/notes/how-to-read-a-book.org" 1)

  (defun org-publish-tags (project)
    "Create tags.org (specified by #+KEYWORDS) for `PROJECT'."
    (let* ((project-plist (cdr project))
           (base-dir (file-name-as-directory
                      (expand-file-name (plist-get project-plist :base-directory))))
           (exclude-regexp (plist-get project-plist :exclude))
           (files (nreverse (org-publish-get-base-files project exclude-regexp)))
           (base-link personal-blog-home-link)
           (tags-filename (expand-file-name "tags.org" base-dir))
           (visiting (find-buffer-visiting tags-filename))
           (tags-alist '())
           tags-buffer)

      (with-current-buffer (setq tags-buffer
                                 (or visiting (find-file tags-filename)))
        (erase-buffer)
        (insert (concat "#+TITLE:tags\n"
                        "#+OPTIONS: ^:nil toc:nil\n\n"))
        (insert (format "#+HTML_HEAD_EXTRA: <script type=\"text/javascript\" src=\"//libs.baidu.com/jquery/2.0.0/jquery.min.js\"> </script> \n"))
        (insert (format "#+HTML_HEAD_EXTRA: <script type=\"text/javascript\" src=\"static/js/jquery.tagcloud.js\"> </script> \n"))
        (insert (format "#+HTML_HEAD_EXTRA: <script type=\"text/javascript\" src=\"static/js/tags.js\"> </script> \n"))

        (dolist (afile files)
          (let* ((id (file-name-nondirectory afile))
                 (title (org-publish-find-title afile))
                 (keywords (org-publish-find-keywords afile 1)) ;;reset cache
                 (kw-lst (and keywords (s-split-words keywords)))
                 (cur-url (concat "file:"(file-relative-name afile base-dir)))
                 (cur-plist `(:title ,title :url ,cur-url)))
            (dolist (kw kw-lst)
              (let* ((entry (assoc kw tags-alist))
                     (lst (cons cur-plist (and entry (cdr entry)))))
                (setq tags-alist (cons `(,kw . ,lst)
                                       (delq (assoc kw tags-alist) tags-alist)))))))
        (dolist (entry tags-alist)
          (let ((kw (car entry))
                (lst (cdr entry)))
            (insert (format "* %s\n" kw))
            (insert (format "  :PROPERTIES:\n  :CUSTOM_ID: %s \n  :END:\n\n" kw))


            (dolist (plist lst)
              (let ((title (plist-get plist :title))
                    (url (plist-get plist :url)))
                (insert (format "  + [[%s][%s]]\n" url title))))
            ))
        (save-buffer))
      (or visiting (kill-buffer tags-buffer))
      ;; (org-insert-tags-to-index-org tags-alist (expand-file-name "index.org" base-dir))
      ))

  (defun org-insert-tags-to-index-org (tags-alist index-filename)
    "Insert tag links to Index.org."
    (let ((visiting (find-buffer-visiting index-filename))
          index-buffer)
      (with-current-buffer (setq index-buffer
                                 (or visiting (find-file index-filename)))
        (goto-char (point-min))
        (if (search-forward "* Tags" nil t)
            (progn
              (beginning-of-line)
              (delete-region (point) (point-max)))
          (goto-char (point-max)))
        (insert "* Tags\n")
        (insert "  :PROPERTIES:\n  :CUSTOM_ID: tags \n  :END:\n\n")
        (dolist (entry tags-alist)
          (let ((kw (car entry)))
            (insert (format "+ [[file:tags.org::#%s][%s]]\n" kw kw))))
        (save-buffer))
      (or visiting (kill-buffer index-buffer))))



  (setq org-html-mathjax-options '(
                                   ;;(path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                                   (path "//cdn.bootcss.com/mathjax/2.6.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
                                   (scale "100")
                                   (align "center")
                                   (indent "2em")
                                   (mathml t)))
  ;; highlight the src block
  (setq org-src-fontify-natively t)


  ;;---------------------------------------------------------
  ;; Babel
  ;;---------------------------------------------------------
  ;; (require 'org-install)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     ;;    (sh . t)
     (python . t)
     (latex  . t)
     ;;    (ruby . t)
     ;;    (perl . t)

     ;;    (C . t)
     ;;    ;; (cpp . t)

     ;;    (ditaa . t)
     (dot . t)
     (plantuml . t)
     ;;    (R . t)
     ;;    (octave . t)
     ;;    (matlab . t)
     ;;    (gnuplot . t)
     ;;    (sqlite . t)
     ))
  ;; ;; don't confirm when evaluate the code
  (setq org-confirm-babel-evaluate nil)

  )
