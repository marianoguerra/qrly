
Nonterminals

    selectors selector filters filter_param attrs attr attrs_items item item_single
    childs
    .

Terminals
    sep open close open_list close_list integer all op sibling children
    adjacent tag class id filter string
    .

Rootsymbol selectors.

selectors -> selector sep selectors : ['$1'|'$3'].

selector -> item : '$1'.
selector -> item attrs: {attr, line('$1'), {unwrap('$1'), '$2'}}.
selector -> item filters: {filters, line('$1'), ['$1'|'$2']}.
selector -> filters:      {filters, line('$1'), '$1'}.

item -> item_single item: {descendant, line('$1'), ['$1'|'$2']}.
item -> item_single : '$1'.
item -> item_single sibling item_single: {sibling, line('$1'), {'$1', '$3'}}.
item -> item_single adjacent item_single: {adjacent, line('$1'), {'$1', '$3'}}.
item -> childs : {child, line('$1'), '$1'}.

childs -> item_single children item_single : ['$1', '$3'].
childs -> item_single children childs : ['$1'|'$3'].

item_single -> tag : '$1'.
item_single -> class: '$1'.
item_single -> id: '$1'.
item_single -> integer: '$1'.
item_single -> all: '$1'.

filters -> filter_param filters : ['$1'|'$2'].
filters -> filter_param : ['$1'].

filter_param -> filter open selector close : {filter, line('$1'), '$3'}.
filter_param -> filter : {filter, line('$1'), []}.

attrs -> attrs_items : {attrs, line('$1'), '$1'}.

attrs_items -> attr attrs_items : ['$1'|'$2'].
attrs_items -> attr : ['$1'].

attr -> open_list tag op string close_list :
    {op, line('$1'),
        {unwrap('$3'), unwrap('$2'), unwrap('$4')}}.

Erlang code.

unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H).
