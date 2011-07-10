-module(qrly).
-export([behaviour_info/1, parse_query/1, filter/2, test/0]).

-include_lib("eunit/include/eunit.hrl").

behaviour_info(callbacks) ->
    [{parse, 1},
     {parse_string, 1},
     {to_file, 2},
     {to_string, 1},
     {filter, 2}
    ];

behaviour_info(_Other) ->
    undefined.

parse_query(Str) ->
    {Status, Result} = case qrly_lexer:string(Str) of
        {ok, Toks, _Endline} ->
            {ok, Toks};
        {error, Errs, _} ->
            {error, Errs}
    end,

    if
        Status == ok ->
            case qrly_parser:parse(Result) of
                {ok, Tree}    -> {ok, Tree};
                {ok, Tree, _} -> {ok, Tree};
                {error, _Warnings, Errors} -> {error, Errors};
                Error -> Error
            end;

        true ->
            {Status, Result}
    end.

filter(Qrly, [H|_] = FilterStr) when is_list(FilterStr) andalso  is_integer(H) ->
    {ok, Filter} = parse_query(FilterStr),
    filter(Qrly, Filter, 0).

% when we got no more results to filter return it
filter([], _, _) ->
    [];

% when we got no more filters we return what we have
filter(Qrly, [], Count) ->
    if
        Count rem 2 == 1 ->
            lists:reverse(Qrly);
        true ->
            Qrly
    end;

filter(Qrly, [{tag, _Line, {Name, Filters}}|T], Count) ->
    NewQrly = walk(Qrly, fun filter_by_tagname/2, Name),
    filter(NewQrly, Filters ++ T, Count + 1);

filter(Qrly, [{tag, _Line, Name}|T], Count) ->
    NewQrly = walk(Qrly, fun filter_by_tagname/2, Name),
    filter(NewQrly, T, Count + 1);

filter(Qrly, [{filters, _, Filters}|T], Count) ->
    filter(Qrly, Filters ++ T, Count) ;

filter(Qrly, [{class, _Line, Name}|T], Count) ->
    NewQrly = walk(Qrly, fun filter_by_class/2, Name),
    filter(NewQrly, T, Count + 1);

filter(Qrly, [{op, _Line, Operation}|T], Count) ->
    NewQrly = walk(Qrly, fun filter_by_attr/2, Operation),
    filter(NewQrly, T, Count + 1).

% filters
filter_by_tagname(TagName, {TagName, _Attrs, _Childs}) ->
    keep;

filter_by_tagname(_, _) ->
    discard.

filter_by_class(ClassName, {_, Attrs, _}) when length(Attrs) > 0 ->
    Class = proplists:get_value(<<"class">>, Attrs),

    if
        Class == undefined ->
            discard;
        true ->
            Classes = binary:split(Class, <<" ">>, [global]),

            case lists:member(ClassName, Classes) of
                true -> keep;
                false -> discard
            end
    end;

filter_by_class(_, _) ->
    discard.

filter_by_attr({Op, AttrName, Expected}, {_, Attrs, _}) when length(Attrs) > 0 ->
    Value = proplists:get_value(AttrName, Attrs),

    Result = applyOp(Op, Expected, Value),

    if
        Value == undefined ->
            discard;
        Result == true ->
            keep;
        true ->
            discard
    end;

filter_by_attr(_, _) ->
    discard.


% helpers

applyOp(<<"=">>, Left, Right) ->
    Left == Right.

test() ->
    eunit:test(?MODULE).

walk(Qrly, Fun, Arg) ->
    walk(Qrly, Fun, Arg, []).

walk([], _Fun, _Arg, Accum) ->
    Accum;

walk(Qrly, Fun, Arg, Accum) when is_tuple(Qrly) ->
    walk([Qrly], Fun, Arg, Accum);

walk([H|T], Fun, Arg, Accum) ->
    Result = Fun(Arg, H),

    Childs = case H of
        {_Tag, _Attrs, Childrens} -> Childrens;
        _ -> []
    end,

    NewAccum = case Result of
        keep             -> walk(Childs, Fun, Arg, [H|Accum]);
        keep_and_stop    -> [H|Accum];
        discard          -> walk(Childs, Fun, Arg, Accum);
        discard_and_stop -> Accum
    end,

    walk(T, Fun, Arg, NewAccum).

% common tests

assertQuery(Expected, Expr) ->
    {ok, [Query]} = parse_query(Expr),
    ?assertEqual(Expected, Query).

assertQueries(Expected, Expr) ->
    {ok, Query} = parse_query(Expr),
    ?assertEqual(Expected, Query).

query_with_class_test() ->
    assertQuery({filters, 1, [{class, 1, <<"first-title">>}]},
        ".first-title").

query_with_classes_test() ->
    assertQuery({filters, 1, [{class, 1, <<"first-title">>}, {class, 1, <<"important">>}]},
        ".first-title.important").

query_has_attr_test() ->
    assertQuery({filters, 1, [{op, 1, {<<"has">>, <<"attrname">>, nil}}]},
        "[attrname]").

query_with_attr_test() ->
    assertQuery({filters, 1, [{op, 1, {<<"=">>, <<"attrname">>, <<"foo">>}}]},
        "[attrname=\"foo\"]").

query_with_attrs_test() ->
    assertQuery({filters, 1,
            [{op, 1, {<<"=">>, <<"attrname">>, <<"foo">>}},
            {op, 1, {<<"*=">>, <<"attrname1">>, <<"bar">>}}
            ]},
        "[attrname=\"foo\"][attrname1*=\"bar\"]").

query_with_filter_test() ->
    assertQuery({filters, 1, [{filter, 1, {<<"first">>, nil}}]},
        ":first").

query_with_filters_test() ->
    assertQuery({filters, 1,
            [{filter, 1, {<<"first">>, nil}},
            {filter, 1, {<<"odd">>, nil}}
            ]},
        ":first:odd").

query_with_filter_param_test() ->
    assertQuery({filters, 1, [{filter, 1, {<<"eq">>, {integer, 1, 2}}}]},
        ":eq(2)").

query_with_filters_param_test() ->
    assertQuery({filters, 1, [{filter, 1, {<<"eq">>, {integer, 1, 2}}}, {filter, 1, {<<"has">>, {string, 1, <<"something">>}}}]},
        ":eq(2):has(\"something\")").

query_with_mixed_filters_test() ->
    assertQuery({filters, 1,
            [{filter, 1, {<<"first">>, nil}},
            {op, 1, {<<"*=">>, <<"attrname1">>, <<"bar">>}},
            {filter, 1, {<<"eq">>, {integer, 1, 42}}},
            {class, 1, <<"someclass">>}
            ]},
        ":first[attrname1*=\"bar\"]:eq(42).someclass").

query_tag_with_mixed_filters_test() ->
    assertQuery({tag, 1, {<<"div">>,
            [{filter, 1, {<<"first">>, nil}},
            {op, 1, {<<"*=">>, <<"attrname1">>, <<"bar">>}},
            {filter, 1, {<<"eq">>, {integer, 1, 42}}},
            {class, 1, <<"someclass">>}
        ]}},
        "div:first[attrname1*=\"bar\"]:eq(42).someclass").

query_with_tag_and_class_test() ->
    assertQuery({tag, 1, {<<"h1">>, [{class, 1, <<"first-title">>}]}},
        "h1.first-title").

query_with_tag_and_classes_test() ->
    assertQuery({tag, 1, {<<"h1">>, [{class, 1, <<"first-title">>}, {class, 1, <<"important">>}]}},
        "h1.first-title.important").

query_with_multiple_selectors_test() ->
    assertQueries([{tag, 1, <<"h1">>}, {tag, 1, <<"h2">>}, {tag, 1, <<"h3">>}],
        "h1, h2, h3").

child_test() ->
    assertQuery({child, 1, [{tag, 1, <<"div">>}, {tag, 1, <<"h1">>}]},
        "div > h1").

childs_test() ->
    assertQuery({child, 1, [{tag, 1, <<"div">>}, {tag, 1, <<"p">>}, {tag, 1, <<"h1">>}]},
        "div > p > h1").

sibling_test() ->
    assertQuery({sibling, 1, {{tag, 1, <<"div">>}, {tag, 1, <<"h1">>}}},
        "div ~ h1").

adjacent_test() ->
    assertQuery({adjacent, 1, {{tag, 1, <<"div">>},{tag, 1, <<"h1">>}}},
        "div + h1").
