-module(gexf).
-export([node/1,
         edge/3,
         set_label/2,
         set_weight/2,
         set_nodes/2,
         document/1,
         document_viz/1,
         graph/2]).

%% Data
-export([add_attribute_value/3]).

%% Metadata
-export([add_attribute_metadata/3,
         attribute_metadata/3]).

%% Vizualization
-export([color/3,
         add_color/2,
         size/1,
         add_size/2,
         position/3,
         add_position/2]).

%% Position helpers
-export([relative_position/2,
         scale_position/2,
         update_position/2,
         rotate_position/2]).

%% IO
-export([to_string/1]).

-compile({parse_transform, cut}).
-compile({parse_transform, chacha}).
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
%% Position
%% ------------------------------------------------------------------

position(X, Y, Z) -> 
    Attrs = orddict:from_list([attr(x, write_number(X)), 
                               attr(y, write_number(Y)), 
                               attr(z, write_number(Z))]),
    elem('viz:position',  Attrs, "").


%% @doc Add a position for a node.
-spec add_position(Pos, Elem) -> Elem when
    Pos :: elem(),
    Elem :: elem().

add_position(Pos, Elem) ->
    add_sub_elem(Pos, Elem).


relative_position(BasePos, NodePos) ->
    X = read_number(get_attribute(x, BasePos)),
    Y = read_number(get_attribute(y, BasePos)),
    Z = read_number(get_attribute(z, BasePos)),
    chain(update_attribute(x, number_id(_ + X)), 
          update_attribute(y, number_id(_ + Y)), 
          update_attribute(z, number_id(_ + Z)) -- NodePos).


scale_position(Factor, NodePos) ->
    F = number_id(_ * Factor),
    chain(update_attribute(x, F), 
          update_attribute(y, F), 
          update_attribute(z, F) -- NodePos).


update_position(F, NodePos) when is_function(F, 3) ->
    X = read_number(get_attribute(x, NodePos)),
    Y = read_number(get_attribute(y, NodePos)),
    Z = read_number(get_attribute(z, NodePos)),
    {NewX, NewY, NewZ} = F(X, Y, Z),
    chain(set_attribute(x, write_number(NewX)), 
          set_attribute(y, write_number(NewY)), 
          set_attribute(z, write_number(NewZ)) -- NodePos).


%% @doc Move a point around a circle based on the circle's rotation.
%%
%% If you rotate point (px, py) around point (ox, oy) by angle theta you'll get:
%% p'x = cos(theta) * (px-ox) - sin(theta) * (py-oy) + ox
%% p'y = sin(theta) * (px-ox) + cos(theta) * (py-oy) + oy
rotate_position(A, Pos) ->
    F = fun(X, Y, Z) ->
            Sin = math:sin(A),
            Cos = math:cos(A),
            DeltaX = Cos * X - Sin * Y,
            DeltaY = Cos * Y + Sin * X,
            {DeltaX, DeltaY, Z}
        end,
    gexf:update_position(F, Pos).


%% ------------------------------------------------------------------
%% Size
%% ------------------------------------------------------------------

size(Size) -> 
    elem('viz:size', [attr(value, Size)], "").


%% @doc Add a size for a node.
-spec add_size(Size, Elem) -> Elem when
    Size :: elem(),
    Elem :: elem().

add_size(Size, Elem) ->
    add_sub_elem(Size, Elem).

    
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
            [ elem(nodes, [attr(count, length(Nodes))], Nodes)
            , elem(edges, [attr(count, length(Edges))], Edges)]).


-spec add_attribute_metadata(Class, Attrs, Graph) -> Graph when
    Attrs :: [elem()],
    Graph :: elem(),
    Class :: node | edge.

add_attribute_metadata(Class, Attrs, GraphElem) ->
    Elem = elem(attributes, [attr(class, Class)], Attrs),
    add_sub_elem(Elem, GraphElem).


attribute_metadata(Id, Title, Type) ->
    elem(attribute, [attr(id, Id), attr(title, Title), attr(type, Type)], "").


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


get_attribute(Key, {_Tag, Attrs, _Value}) ->
    %% Use cut.
    orddict:fetch(Key, Attrs).


update_attribute(Key, Fun, Elem) ->
    %% Use cut.
    with_attributes(orddict:update(Key, Fun, _), Elem).

%% @doc Call `Fun(Value)', if the `{Key, Value}' pair exists as an attribute.
%% If the `Key' does not exist, then `Fun(undefined)' will be called.
with_attribute(Key, Fun, Elem) ->
    %% Use cut.
    with_attributes(orddict:update(Key, Fun, undefined, _), Elem).


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

with_sub_elem(Tag, Fun, Elem) ->
    with_value(with_elem(Tag, Fun, _), Elem).


%% @doc Add an element `Elem' or replace one with same tag. 
%% Returns `Elems' with stored `Elem'.
-spec set_elem(Elem, Elems) -> Elems when
    Elems :: [Elem],
    Elem :: elem().
set_elem(E, []) ->                            [E];
set_elem({Tag, _, _} = E, [{Tag, _, _}|T]) -> [E|T];
set_elem(E, [H|T]) ->                         [H|set_elem(E, T)].


%% @doc Run `Fun(Elem)', if an element with `Tag' exists. 
%%      Otherwise, run `Fun(undefined)'.
%%
%%      `Fun' returns an element, that will be stored inside the
%%      modified version of `Elems'. 
%%
%%      This function returns the updated version of `Elems'.
-spec with_elem(Tag, Fun, Elems) -> Elems when
    Tag :: atom(),
    Elems :: [Elem],
    Elem :: elem(),
    Fun :: fun((undefined | Elem) -> Elem).
with_elem(_Tag, Fun, []) ->                         [Fun(undefined)];
with_elem(Tag, Fun, [{Tag, _, _}=H|T]) ->           [Fun(H)|T];
with_elem(Tag, Fun, [H|T]) ->                       [H|with_elem(Tag, Fun, T)].


elem_tag({Tag, _, _}) -> Tag.

elem(Tag, Attrs, Value) -> {Tag, fix_attributes(Attrs), Value}.

fix_attributes({_K, _V} = Attr) -> [Attr];
fix_attributes(Attrs) -> Attrs.

attr(Key, Value) -> {Key, Value}.

number_id(F) -> 
    chain(write_number, F, read_number).

read_number(X) when is_list(X) -> 
    try list_to_integer(X) catch error:badarg -> list_to_float(X) end;
read_number(X) when is_number(X) -> 
    X.

write_number(X) when is_list(X)    -> X;
write_number(X) when is_integer(X) -> X;
write_number(X) when is_float(X)   -> float_to_string(X).

float_to_string(X) -> mochinum:digits(X).

%% ------------------------------------------------------------------
%% IO
%% ------------------------------------------------------------------

to_string(Doc) ->
    xmerl:export_simple([Doc], xmerl_xml, []).


%% ------------------------------------------------------------------
%% Data
%% ------------------------------------------------------------------

%% Create:
%%
%% ```
%%  <attvalues>                                       
%%    <attvalue for="0" value="http://gephi.org"/>  
%%    <attvalue for="1" value="1"/>                 
%%  </attvalues>
%% '''
add_attribute_value(Key, Value, Elem) ->
    %% OldAttValues is
    %% <attvalue for="0" value="http://gephi.org"/>
    %% <attvalue for="1" value="1"/>
    Fun = fun(OldAttValues) -> 
            AttValue = elem(attvalue, [attr(for, Key), attr(value, Value)], ""),
            case OldAttValues of
                undefined ->
                    elem(attvalues, [], [AttValue]);
                _Elem ->
                    add_sub_elem(AttValue, OldAttValues)
            end
        end,
    with_sub_elem(attvalues, Fun, Elem).

