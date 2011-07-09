-module(qrly).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{parse, 1},
     {parse_string, 1},
     {to_file, 2},
     {to_string, 1},
     {filter, 1}
    ];

behaviour_info(_Other) ->
    undefined.
