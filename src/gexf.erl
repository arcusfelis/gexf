-module(gexf).

-compile({parse_transform, sangria}).

-define(LIFETIME_RECORDS, ['node-content', 'edge-content', 'graph-content']).
-define(LABEL_RECORDS, ['node-content', 'edge-content']).


-include_lib("gexf/src/gexf.hrl").


-spec node(Id) -> 'node-content'() when
    Id :: string().

node(Id) ->
    #'node-content'{id = Id}.

-spec edge(Id, Source, Target) -> 'edge-content'() when
    Id :: string(),
    Source :: string(),
    Target :: string().

edge(Id, Source, Target) ->
    #'edge-content'{id = Id, source = Source, target = Target}.


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
%% Graph
%% ------------------------------------------------------------------

-spec graph(Nodes, Edges) -> 'graph-content'() when
    Nodes :: ['node-content'()],
    Edges :: ['edge-content'()].

graph(Nodes, Edges) ->
    #'graph-content'{
%%      defaultedgetype = "directed",
%%      mode = "static",
        choice = 
            [#'graph-content/CH1'{
                 choice = 
                     #'nodes-content'{
                         count = length(Nodes),
                         node = Nodes}},
             #'graph-content/CH1'{
                 choice = 
                     #'edges-content'{
                         count = length(Edges),
                         edge = Edges}}]}.


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
