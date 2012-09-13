-module(gexf).
-export([node/1,
         edge/3,
         set_label/2,
         set_weight/2,
         set_nodes/2,
         document/1,
         document_viz/1,
         graph/2]).

%% Vizualization
-export([color/3,
         add_color/2]).

%% IO
-export([to_string/1]).

-compile({parse_transform, cut}).
-type elem() :: {atom(), orddict:orddict(), iolist()}.


-spec node(Id) -> elem() when
    Id :: string().

node(Id) ->
    elem(node, attr(id, Id), "").

-spec edge(Id, Source, Target) -> elem() when
    Id :: string(),
    Source :: string(),
    Target :: string().

edge(Id, Source, Target) ->
    Attrs = [attr(id, Id), attr(source, Source), attr(target, Target)],
    elem(edge, Attrs, "").

-spec set_nodes(SubNodes, Node) -> Node when
    SubNodes :: [Node],
    Node :: elem(). 

set_nodes(SubNodes, Node) ->
    NodesElem = elem(nodes, [attr(count, length(SubNodes))], SubNodes),
    set_sub_elem(NodesElem, Node).

%% ------------------------------------------------------------------
%% Label
%% ------------------------------------------------------------------

%% @doc Add a label for a node or an edge.
-spec set_label(Label, Record) -> Record when
    Label :: string(),
    Record :: record().

set_label(Label, Record) -> 
    set_attribute(label, Label, Record).

%% ------------------------------------------------------------------
%% Edge weight
%% ------------------------------------------------------------------

%% @doc Add weight for an edge.
-spec set_weight(Weight, Record) -> Record when
    Weight :: string(),
    Record :: record().

set_weight(Weight, Record) -> 
    set_attribute(weight, Weight, Record).


%% ------------------------------------------------------------------
%% Color
%% ------------------------------------------------------------------


color(R, G, B) -> 
    Attrs = orddict:from_list([attr(r, R), 
                               attr(g, G), 
                               attr(b, B)]),
    elem('viz:color',  Attrs, "").


%% @doc Add a color for a node or an edge.
-spec add_color(Color, Elem) -> Elem when
    Color :: elem(),
    Elem :: elem().

add_color(Color, Elem) ->
    add_sub_elem(Color, Elem).
    
%% ------------------------------------------------------------------
%% Graph
%% ------------------------------------------------------------------

-spec graph(Nodes, Edges) -> Graph when
    Nodes :: [elem()],
    Edges :: [elem()],
    Graph :: elem().

graph(Nodes, Edges) ->
    elem(graph,
            [ attr(defaultedgetype, "directed")
            , attr(mode, "static")],
            [ elem(edges, [attr(count, length(Edges))], Edges)
            , elem(nodes, [attr(count, length(Nodes))], Nodes)]).


%% ------------------------------------------------------------------
%% Lifetime
%% ------------------------------------------------------------------

set_lifetime(Start, End, Record) ->
    set_lifetime_end(End, set_lifetime_start(Start, Record)).


set_lifetime_start({open, Start}, Record) ->
    set_attribute(startopen, Start, Record);
set_lifetime_start(Start, Record) ->
    set_attribute(start, Start, Record).

set_lifetime_end({open, End}, Record) ->
    set_attribute('endopen', End, Record);
set_lifetime_end(End, Record) ->
    set_attribute('end', End, Record).


open({open, Endpoint}) -> {open, Endpoint};
open(Endpoint) -> {open, Endpoint}.


%% ------------------------------------------------------------------
%% Document
%% ------------------------------------------------------------------

document(Graph) ->
    NS = "http://www.gexf.net/1.2draft",
    elem('gexf', [attr(version, "1.2"), attr(xmlns, NS)], [Graph]).

document_viz(Graph) ->
    NS = "http://www.gexf.net/1.1draft/viz",
    set_attribute('xmlns:viz', NS, document(Graph)).

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

set_attribute(Key, Value, Elem) ->
    %% Use cut.
    with_attributes(orddict:store(Key, Value, _), Elem).

with_attributes(Fun, {Tag, Attrs, Value}) ->
    {Tag, Fun(Attrs), Value}.

with_value(Fun, {Tag, Attrs, Value}) ->
    {Tag, Attrs, Fun(Value)}.

add_sub_elem(SubElem, {Tag, Attrs, Value}) ->
    {Tag, Attrs, [SubElem|Value]}.

add_sub_elems(SubElems, {Tag, Attrs, Value}) ->
    {Tag, Attrs, SubElems ++ Value}.

set_sub_elem(SubElem, Elem) ->
    with_value(set_elem(Elem, _), Elem).


%% @doc Add an element `Elem' or replace one with same tag. 
%% Returns `Elems' with stored `Elem'.
-spec set_elem(Elem, Elems) -> Elems when
    Elems :: [Elem],
    Elem :: elem().
set_elem(E, []) ->                            [E];
set_elem({Tag, _, _} = E, [{Tag, _, _}|T]) -> [E|T];
set_elem(E, [H|T]) ->                         [H|set_elem(E, T)].


elem_tag({Tag, _, _}) -> Tag.

elem(Tag, Attrs, Value) -> {Tag, fix_attributes(Attrs), Value}.

fix_attributes({_K, _V} = Attr) -> [Attr];
fix_attributes(Attrs) -> Attrs.

attr(Key, Value) -> {Key, Value}.


%% ------------------------------------------------------------------
%% IO
%% ------------------------------------------------------------------

to_string(Doc) ->
    xmerl:export_simple([Doc], xmerl_xml, []).
