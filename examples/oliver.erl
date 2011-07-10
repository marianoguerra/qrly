-module(oliver).
-export([run/0]).

print_entry(Entry) ->

    [{_, _, [Published]}, {_, _, [Title]}, {_, _, Content}] = qrly:filter(Entry, "published, title, content"),

    io:format("title: ~s~n~npublished: ~s~n~n~s~n~n", [Published, Title,
            Content]).

run() ->
{ok, Qrly} = qrly_xml:parse("../extra/test/atom.xml"),
    Entries = qrly:filter(Qrly, "entry"),

    [print_entry(Entry) || Entry <- Entries].
