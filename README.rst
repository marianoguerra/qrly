qrly - a query module for erlang
--------------------------------

qrly attempts to be a query module for erlang that provides an API to extract
information from structured data using a jquery-like API.

the implementation provides a standard api that allows to extend it to support
other languages

supported selectors
===================

for details on selectors check the jquery documentation: http://api.jquery.com/category/selectors/

* .class
* #id
* tagname
* Attribute Contains Selector [name*="value"]
* Attribute Contains Word Selector [name~="value"]
* Attribute Ends With Selector [name$="value"]
* Attribute Equals Selector [name="value"]
* Attribute Not Equal Selector [name!="value"]
* Multiple Attribute Selector [name="value"][name2="value2"]
* Attribute Starts With Selector [name^="value"]

other selectors are not implemented because of lack of time in spawnfest, they
are really easy to implement (already parsed, but not implemented in qrly:filter)
and should come after the spawnfest ends and I have some time to add them.

supported data formats
======================

XML
...

the **qrly_xml** provides an api to manipulate and query xml using xmerl as a
backend

HTML
....

the **qrly_html** provides an api to manipulate and query html using mochiweb_html
as a backend

usage
=====

use **qrly_html:parse** or **qrly_html:parse_string** to load from file or
string an html file and convert it to a qrly data structure and then use
**qrly_html:filter** to apply the filtering.

if you want your string back call **qrly_html:to_string**.
if you want to store it on a file **call qrly_html:to_file**.

the same instructions work for **qrly_xml** and any other future implementation
of the api (imagine querying json structures with this).

example
=======

::

    1> {ok, Qrly} = qrly_html:parse("../extra/test/test.html").
    ...
    2> Q1 = qrly_html:filter(Qrly, "h1").                      
    [{<<"h1">>,
      [{<<"class">>,<<"first-title asdwordclass">>}],
      [<<"personal">>]},
     {<<"h1">>,
      [{<<"class">>,<<"second-title wordclass">>}],
      [<<"projects">>]},
     {<<"h1">>,
      [{<<"title">>,<<"others">>},
       {<<"class">>,<<"third-title wordclass">>}],
      [<<"others">>]}]
    3> Q2 = qrly_html:filter(Qrly, "h1.first-title").
    [{<<"h1">>,
      [{<<"class">>,<<"first-title asdwordclass">>}],
      [<<"personal">>]}]
    4> io:format("~s~n", [qrly_html:to_string(Q1)]).
    <div><h1 class="first-title asdwordclass">personal</h1><h1 class="second-title wordclass">projects</h1><h1 title="others" class="third-title wordclass">others</h1></div>
    ok
    5> io:format("~s~n", [qrly_html:to_string(Q2)]).
    <div><h1 class="first-title asdwordclass">personal</h1></div>
    ok

license
=======

BSD + optional beer invitation to the creator

see LICENSE for details
