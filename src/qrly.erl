-module(qrly).
-export([behaviour_info/1, parse_query/1]).

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
