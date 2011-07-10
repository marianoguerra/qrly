
Nonterminals
    selectors selector filter_param attr childs parameter tag_filters .

Terminals
    sep open close open_list close_list integer all op sibling children
    adjacent tag class id filter string
    .

Rootsymbol selectors.

selectors -> selector sep selectors : ['$1'|'$3'].
selectors -> selector : ['$1'].

selector -> all     : '$1'.
selector -> tag     : '$1'.
selector -> id      : '$1'.

selector -> tag tag_filters: {tag, line('$1'), {unwrap('$1'), '$2'}}.
selector -> all tag_filters: {filters, line('$1'), '$1'}.
selector -> tag_filters: {filters, line('$1'), '$1'}.

tag_filters -> attr tag_filters   : ['$1'|'$2'].
tag_filters -> class tag_filters  : ['$1'|'$2'].
tag_filters -> filter_param tag_filters : ['$1'|'$2'].

tag_filters -> filter_param  : [{filter, line('$1'), '$1'}].
tag_filters -> class : ['$1'].
tag_filters -> attr : [{attr, line('$1'), '$1'}].

%selector -> selector selector : {descendant, line('$1'), ['$1'|'$2']}.
selector -> selector sibling selector : {sibling, line('$1'), {'$1', '$3'}}.
selector -> selector adjacent selector : {adjacent, line('$1'), {'$1', '$3'}}.
selector -> childs : {child, line('$1'), '$1'}.

parameter -> integer :  '$1'.
parameter -> string  :  '$1'.
parameter -> selector : '$1'.

childs -> tag children tag : ['$1', '$3'].
childs -> tag children childs : ['$1'|'$3'].

filter_param -> filter open parameter close : {filter, line('$1'), {unwrap('$1'), '$3'}}.
filter_param -> filter : {filter, line('$1'), {unwrap('$1'), nil}}.

attr -> open_list tag op string close_list :
    {op, line('$1'),
        {unwrap('$3'), unwrap('$2'), unwrap('$4')}}.

Erlang code.

unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H).
