-module(qrly_html).
-export([parse/1, parse_string/1, to_file/2, to_string/1, filter/2, test/0]).

-behaviour(qrly).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE, "../extra/test/test.html").

% external api

parse(FilePath) ->
    {Status, Content} = file:read_file(FilePath),

    if
        Status == ok ->
            Result = mochiweb_html:parse(Content);

        Status == error ->
            Result = Content
    end,

    {Status, Result}.

parse_string(Str) ->
    mochiweb_html:parse(Str).

to_file(Qrly, Path) ->
    Str = to_string(Qrly),

    case file:open(Path, [write]) of
        {ok, Device} ->
            file:write(Device, Str),
            {ok, Qrly};
        {error, _Reason} = Error ->
            Error
    end.

to_string(Qrly) ->
    mochiweb_html:to_html(Qrly).

filter(Qrly, Expression) -> {Qrly, Expression}.

% test helpers

filter_file(Expr) ->
    {ok, Content} = parse(?TEST_FILE),
    qrly:filter(Content, Expr).

test() ->
    eunit:test(?MODULE).

assertContent(Tag, ExpectedContent) ->
    {_, _, [Content]} = Tag,
    ?assertEqual(ExpectedContent, Content).

assertTagName(Tag, ExpectedName) ->
    {TagName, _, _} = Tag,
    ?assertEqual(ExpectedName, TagName).

% tests

parse_existing_test() ->
    ok, _Content = parse(?TEST_FILE).

parse_inexisting_test() ->
    error, _Content = parse("extra/test/inexisting.html").

get_tag_test() ->
    Result = filter_file("h1"),
    ?assertEqual(3, length(Result)),
    [FirstTag, SecondTag, ThirdTag] = Result,
    assertContent(FirstTag, <<"personal">>),
    assertContent(SecondTag, <<"projects">>),
    assertContent(ThirdTag, <<"others">>).

get_by_class_test() ->
    Result = filter_file(".first-title"),
    ?assertEqual(1, length(Result)),
    [FirstTag] = Result,
    assertContent(FirstTag, <<"personal">>).

get_by_class_and_tag_test() ->
    Result = filter_file("h1.first-title"),
    ?assertEqual(1, length(Result)),
    [FirstTag] = Result,
    assertContent(FirstTag, <<"personal">>).

get_by_tag_and_attr_equal_test() ->
    Result = filter_file("a[href=\"http://www.emesene.org\"]"),
    ?assertEqual(1, length(Result)),
    [FirstTag] = Result,
    assertContent(FirstTag, <<"emesene">>).

get_has_attr_test() ->
    Result = filter_file("[title]"),
    ?assertEqual(2, length(Result)),
    [BodyTag, H1Tag] = Result,
    assertTagName(BodyTag, <<"body">>),
    assertTagName(H1Tag, <<"h1">>),
    assertContent(H1Tag, <<"others">>).

get_tag_has_attr_test() ->
    Result = filter_file("h1[title]"),
    ?assertEqual(1, length(Result)),
    [H1Tag] = Result,
    assertTagName(H1Tag, <<"h1">>),
    assertContent(H1Tag, <<"others">>).

get_attr_contains_test() ->
    Result = filter_file("[href*=\"marianoguerra\"]"),
    ?assertEqual(5, length(Result)),
    [FirstTag|_] = Result,
    assertContent(FirstTag, <<"blog">>).

get_by_tag_and_attr_contains_test() ->
    Result = filter_file("a[href*=\"marianoguerra\"]"),
    ?assertEqual(5, length(Result)),
    [FirstTag|_] = Result,
    assertContent(FirstTag, <<"blog">>).

get_tag_has_attr_not_equal_test() ->
    Result = filter_file("h1[title!=\"others\"]"),
    ?assertEqual(2, length(Result)),
    [FirstTag, SecondTag] = Result,
    assertContent(FirstTag, <<"personal">>),
    assertContent(SecondTag, <<"projects">>).

get_by_tag_and_attr_ends_with_test() ->
    Result = filter_file("a[href$=\"guerra\"]"),
    ?assertEqual(2, length(Result)),
    [FirstTag, SecondTag] = Result,
    assertContent(FirstTag, <<"pictures">>),
    assertContent(SecondTag, <<"code">>).

get_by_tag_and_attr_begins_with_test() ->
    Result = filter_file("a[href^=\"http://marianoguerra\"]"),
    ?assertEqual(2, length(Result)),
    [FirstTag, SecondTag] = Result,
    assertContent(FirstTag, <<"blog">>),
    assertContent(SecondTag, <<"fnt">>).

get_by_tag_and_attr_contains_word_test() ->
    Result = filter_file("h1[class~=\"wordclass\"]"),
    ?assertEqual(2, length(Result)),
    [FirstTag, SecondTag] = Result,
    assertContent(FirstTag, <<"projects">>),
    assertContent(SecondTag, <<"others">>).
