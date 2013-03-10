[![Build Status](https://secure.travis-ci.org/ekoontz/italianquiz.png?branch=master)](http://travis-ci.org/ekoontz/italianquiz)

# Usage:

1. git clone git://github.com/ekoontz/italianquiz.git italianquiz
2. cd italianquiz/
3. lein deps
4. sudo mongod 
5. lein ring server

  ..then point your browser to http://localhost:3000
  That's all there is to it!

# Hacking:

1. Start with src/italianverbs/core.clj
2. Follow the routes in (defroutes main-routes).
3. Look at :body attribute for each route:
   This holds the function that is executed for that route.

# REPL: see comments at the top of src/italianverbs/repl.clj.

# Apache HTTP Server Proxying:

--->% begin http configuration --->%----

    # LoadModule might not be necessary depending on your Apache installation.
    # If it is necessary, module location may vary according to your Apache installation.
    # For example, you don't need this on Mac OS X because it's already loaded in
    # /private/etc/apache2/httpd.conf
    # LoadModule proxy_module modules/mod_proxy.so

    ProxyPreserveHost on

    ProxyPass /italian http://localhost:3000
    ProxyPassReverse /italian http://localhost:3000

    <Location /italian>
      Options All
      Order allow,deny
      allow from all
    </Location>

--->% end http configuration --->%----

After restarting your HTTP server, you should be able to access : http://yourhost/italian/ .

# Ajax, Routes, and Page Structure

1. create a static html page whose head is:

(html/head)

This includes all the necessary CSS and javascript.

and a <body> that simply has:

2. <body onload="ajax_quiz()"/>

3. The ajax_quiz() javascript function is defined in resources/public/js/quiz.js.

See quiz.clj:quiz/minimal() for a minimal function that does step 1.
See core.clj:defroutes()'s "/quiz/minimal" for a route that calls quiz/minimal().

Sequentially:

-ajax_quiz() creates the form elements: empty #quiz_table and user-guess-submittal input form and button.
ajax_quiz() then calls get_next_question() which AJAX-GETs /guess/question/. Resulting content is put in
top of form to pose question to user.

-button element created in previous step calls submit_user_response() with the user's guess.

-submit_user_response() AJAX-POSTs to /evaluate/tr.

-resulting content (evaluation of user's guess) is appended by submit_user_response()'s callback to #quiz_table.

Schematically:

ajax_quiz()
\---> get_next_question() ---> "GET /guess/question" ---HTML---> #ajax_question
|
\---> user clicks button ---> submit_user_response() ---> "POST /evaluate/tr" --HTML---> #quiz_table
|
\---> user checks a preference box ---> submit_quiz_filters() --> "POST /quiz/filter/ajax" ---(quiz/set-filters) 
                                                                                           |
                                                                                           \--> --302 GET /quiz/filter/ajax --HTML--> #prefs


Dependencies:
...................................................
.  .  .   .  .  .   .                             .
.  .  .   .  .  .   . quiz                        .
.  .  .   .  .  .   ...............................
.  .  .   .  .  .          .         .      .     . 
.  .  .   .  .  . generate .         . lev  . xml . 
.  .  .   .  .  ............         ..............
.  .  .   .  .             .         .      .         
.  .  .   .  .  search     . lexicon . html .         
.  .  .   .  ...............         ........         
.  .  .   .                .         .                
.  .  .   . grammar        .         .
.  .  .   ............................ 
.  .  .                              .
.  .  .  lexiconfn                   .
.  .  ................................
.  .                                 .
.  . morphology                      .
.  ...................................
.                                    .
.  fs                                .
......................................


Note that morphology does not have direct access to the lexicon for now: it should
only be concerned with regular morphology rules, which excludes lexical information
(or so we assume for now).

# License:

-resources/public/js/jsquery-1.6.4.min.js 
  is provided under the LGPL version 2 per http://jquery.org/license/.
-remainder: see LICENSE file.

Structure sharing using Clojure refs

user> (def myref (ref {}))
#'user/myref
user> (def mymap2 {:a myref :b myref})
#'user/mymap2
user> (dosync (ref-set myref {:c 42}))
{:c 42}
user> mymap2
{:a #<Ref@55187eb3: {:c 42}>, :b #<Ref@55187eb3: {:c 42}>}
user> (dosync (ref-set myref {:c 43}))
{:c 43}
user> mymap2
{:a #<Ref@55187eb3: {:c 43}>, :b #<Ref@55187eb3: {:c 43}>}
user> (dosync (alter myref (fn [ref] (merge ref {:foo 99}))))
{:foo 99, :c 43}
user> mymap2
{:a #<Ref@55187eb3: {:foo 99, :c 43}>, :b #<Ref@55187eb3: {:foo 99, :c 43}>}
user> (dosync (alter myref (fn [ref] (merge ref {:foo 100}))))
{:foo 100, :c 43}
user> mymap2
{:a #<Ref@55187eb3: {:foo 100, :c 43}>, :b #<Ref@55187eb3: {:foo 100, :c 43}>}
user> 


