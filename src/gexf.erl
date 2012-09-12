-module(gexf).
-export([node/1,
         edge/3,
         set_label/2,
         document/1,
         graph/2]).


-compile({parse_transform, sangria}).

-define(LIFETIME_RECORDS, ['node-content', 'edge-content', 'graph-content']).
-define(LABEL_RECORDS, ['node-content', 'edge-content']).


-include_lib("gexf/src/gexf.hrl").


-spec node(Id) -> 'node-content'() when
    Id :: string().

node(Id) ->
    #'node-content'{id = id_to_string(Id)}.

-spec edge(Id, Source, Target) -> 'edge-content'() when
    Id :: string(),
    Source :: string(),
    Target :: string().

edge(Id, Source, Target) ->
    #'edge-content'{id = id_to_string(Id), 
                    source = id_to_string(Source), 
                    target = id_to_string(Target)}.


id_to_string(Id) when is_integer(Id) ->
    integer_to_list(Id);
id_to_string(Id) ->
    Id.

%% ------------------------------------------------------------------
%% Label
%% ------------------------------------------------------------------

%% @doc Add a label for a node or an edge.
-spec set_label(Label, Record) -> Record when
    Label :: string(),
    Record :: record().

set_label(Label, Record) -> 
    with_any(?LABEL_RECORDS, Record#any{label = Label}).


%% ------------------------------------------------------------------
%% Color
%% ------------------------------------------------------------------


color(R, G, B) -> 
    #'viz:color-content'{r = R, g = G, b = B}.


%% @doc Add a color for a node or an edge.
-spec set_color(Color, Record) -> Record when
    Color :: 'viz:color-content'(),
    Record :: record().

set_color(Color, Record) ->
    ok.

%% ------------------------------------------------------------------
%% Graph
%% ------------------------------------------------------------------

-spec graph(Nodes, Edges) -> 'graph-content'() when
    Nodes :: ['node-content'()],
    Edges :: ['edge-content'()].

graph(Nodes, Edges) ->
    #'graph-content'{
%%      defaultedgetype = "directed",
%%      mode = "static",
        choice = [
                     #'edges-content'{
                        count = integer_to_list(length(Edges)),
                        edge = Edges},
                     #'nodes-content'{
                        count = integer_to_list(length(Nodes)),
                         node = Nodes}]}.


%% ------------------------------------------------------------------
%% Lifetime
%% ------------------------------------------------------------------

set_lifetime(Start, End, Record) ->
    set_lifetime_end(End, set_lifetime_start(Start, Record)).


set_lifetime_start({open, Start}, Record) ->
    with_any(?LIFETIME_RECORDS, Record#any{startopen = Start});
set_lifetime_start(Start, Record) ->
    with_any(?LIFETIME_RECORDS, Record#any{start = Start}).

set_lifetime_end({open, End}, Record) ->
    with_any(?LIFETIME_RECORDS, Record#any{endopen = End});
set_lifetime_end(End, Record) ->
    with_any(?LIFETIME_RECORDS, Record#any{'end' = End}).


open({open, Endpoint}) -> {open, Endpoint};
open(Endpoint) -> {open, Endpoint}.


%% ------------------------------------------------------------------
%% Document
%% ------------------------------------------------------------------

document(Graph) ->
    #'_document-gexf'{version = "1.2", graph = Graph}.
