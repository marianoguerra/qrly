-module(qrly_html).
-export([parse/1, parse_string/1, to_file/2, to_string/1, filter/2]).

-behaviour(qrly).

-include_lib("eunit/include/eunit.hrl").

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

% tests

parse_existing_test() ->
    ok, _Content = parse("extras/test/test.html").

parse_inexisting_test() ->
    error, _Content = parse("extras/test/inexisting.html").

