== coq-au-vin

=== Description

Civet is an XML-based templating system. It is intended primarily for
generating dynamic web pages in XHTML, but may be useful for other XML
applications.

[[toc:]]

=== Authors

Matt Gushee <matt@gushee.net>

=== Requirements

[[utf8]], [[uri-common]], [[sql-de-lite]], [[civet]], [[lowdown]], [[crypt]], [[simple-sha1]]

=== Introduction

{{Coq au Vin}} is a blogging engine designed to provide typical blog features (and a
few atypical ones) in a straightforward, easily installed package. It is intended
for users who have basic web development skills (i.e. HTML, CSS, JavaScript) and
a hosting environment that permits installing arbitrary software (VPS or the like).

It uses [[civet]] to generate dynamic pages, and has an abstract database layer that
allows for different databases to be used. There is currently a SQLite3 backend 
included in this egg. Additional databases will be supported in separate eggs.

The body text of articles should be written in Markdown syntax.

Please note that the current release does not implement any form of HTTP request
handling. However, there is an
[[https://github.com/mgushee/coq-au-vin-examples|examples collection]], which 
includes several toy examples and one real web application.

Also, general info, tutorials, and announcements will be posted at the
[[http://coq-au-vin.sapparicms.org/|Coq au Vin blog]], which of course is
powered by {{Coq au Vin}}.

Expect bugs.


=== Scheme API

==== Initialization & Configuration

<procedure>(app-init #!key (site-path #f) (template-path #f))</procedure>

Initialize the application. The SITE-PATH and TEMPLATE-PATH arguments are
passed to [[civet]], and you must specify one of the two. If SITE-PATH is
specified, {{civet}} will find templates in the '''templates''' subdirectory
of that path. The value of either SITE-PATH or TEMPLATE-PATH should be an
absolute pathname.


<procedure>(register-roles #!optional (roles (%default-roles%)))</procedure>

Since each user has a role, you must call this procedure before creating any
users. The ROLES argument, if supplied, should be a list of strings; the
default value is {{'("admin" "editor" "author" "member" "guest")}}.

NOTE: as of version 0.1, these roles have no effect, but one or more roles
must be defined in order to register users.


<procedure>(config-set! (KEY . VALUE) ...)</procedure>

Set multiple variables. Each argument must be a dotted pair where KEY is
a symbol and VALUE is a string or a number.


<procedure>(config-get KEY ...)</procedure>

Retrieve multiple variables. Returns an alist. Any undefined variables are
omitted from the result.


<procedure>(config KEY [VALUE])</procedure>

Sets or retrieves one variable.


<procedure>(config*)</procedure>

Returns an alist of all defined variables.


==== Content API

<procedure>(get-article-page/html ID/ALIAS #!key (out (current-output-port)) (date-format #f))</procedure>

Generate an HTML page that displays the full text of one article. ID/ALIAS may be
either the article's node id or its alias, if defined.


<procedure>(get-article-list-page/html #!key (out (current-output-port)) (criterion 'all) (sort '(created desc))
                                             (date-format #f) (limit 10) (offset 0) (show 'teaser))</procedure>

Generate an HTML page displaying a list of articles. The list may be filtered using the
CRITERION argument; currently supported values are {{'all}}, {{'(tag TAG)}}, {{'(author AUTHOR)}},
{{'(series SERIES-TITLE)}}, {{'(category CATEGORY)}}. As of version 0.1, the SHOW and SORT arguments
are unimplemented.


<procedure>(get-meta-list-page/html SUBJECT #!optional (out (current-output-port)))</procedure>

Generate an HTML page listing all items of a particular type. SUBJECT must be one of {{'tags}},
{{'categories}}, {{'series}}, or {{'authors}}.


<procedure>(get-new-article-form/html #!optional (out (current-output-port)))</procedure>

Generate an HTML form for creating a new article.


<procedure>(get-article-edit-form/html ID/ALIAS #!optional (out (current-output-port)))</procedure>

Generate an HTML form for editing an existing article, specified by ID/ALIAS.


<procedure>(add-article FORM-DATA #!optional (out (current-output-port)))</procedure>

Given FORM-DATA (as an alist), this procedure adds a new article to the database.

NOTE: as of version 0.1, any registered user may submit an article; since login sessions are
not implemented, the form data must include a valid username and password.


<procedure>(update-article ID/ALIAS FORM-DATA #!optional (out (current-output-port)))</procedure>

Given an ID/ALIAS for an existing article and FORM-DATA (as an alist), this procedure
updates the content and metadata of the specified article.

As with {{add-article}} a valid username and password are required.


==== SQLite3 Backend

This database layer uses a SQLite3 database only to store metadata. The body text of
articles is stored in the filesystem. This reduces the load on the database and allows
the content to be placed under version control (though this egg does not yet provide
version control functionality).


<procedure>(setup-db DB-FILE #!optional (force #f))</procedure>

Opens DB-FILE and sets up all tables for the application.


<procedure>(enable-sqlite DB-FILE CONTENT-PATH)</procedure>

Configures the database layer to use the procedures in this egg. DB-FILE is the Sqlite3
database file, which should be the same file as specified in {{setup-db}}. CONTENT-PATH
is a directory where article content files will be stored.


=== In case of bugs

If you have a GitHub account, please use the 
[[https://github.com/mgushee/coq-au-vin/issues|GitHub issue tracker]] -- likewise
for any technical questions or suggestions you may have (other than how-to
type questions). If you are unable to do this, the chicken-users mailing
list will also work.


=== License

Copyright (c) 2013, Matthew C. Gushee
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:


    Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
    
    Neither the name of the author nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.


=== Repo

[[https://github.com/mgushee/coq-au-vin]]


=== Version History

;0.1.3:     Switched password hashing to (crypt).

;0.1.2:     Fixed formatting bug in article teasers.

;0.1.1:     Added normalize-sxml procedure to work around bad href attributes in body text.

;0.1:       Initial release.
