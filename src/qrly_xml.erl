-module(qrly_xml).
-export([parse/1, parse_string/1, to_file/2, to_string/1, filter/2]).
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(qrly).

-include_lib("eunit/include/eunit.hrl").

% external api

parse(FilePath) ->
    {Status, Content} = case xmerl_scan:file(FilePath) of
        {error, _Reason} = Error -> Error;
        {Xml, _} -> {ok, Xml}
    end,

    if
        Status == ok ->
            Result = to_neutral(Content);

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
    mochiweb_html:to_html({<<"div">>, [], Qrly}).

filter(Qrly, Expression) -> qrly:filter(Qrly, Expression).

% internal api

to_neutral(#xmlElement{name=Name, attributes=Attrs, content=Content}) ->
    io:format("~s~n", [Name]),
    {atom_to_binary(Name, latin1),
        [to_neutral(Attr) || Attr <- Attrs],
        [to_neutral(Child) || Child <- Content]};

to_neutral(#xmlAttribute{name=Name, value=Value}) ->
    {atom_to_binary(Name, latin1), list_to_binary(Value)};

to_neutral(#xmlText{value=Value}) -> list_to_binary(Value).

% tests

parse_existing_test() ->
    ok, _Content = parse("extras/test/test.xml").

parse_inexisting_test() ->
    error, _Content = parse("extras/test/inexisting.xml").

