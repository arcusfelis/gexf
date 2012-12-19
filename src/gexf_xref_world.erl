-module(gexf_xref_world).
-export([generate/1]).
-compile({parse_transform, mead}).
-compile({parse_transform, cut}).
-compile({parse_transform, chacha}).
-compile({parse_transform, seqbind}).

-include_lib("eunit/include/eunit.hrl").

%-define(check(X), ok).
-define(check(X), X).

-define(NODE_TYPE_ATTR_ID, 0).
-define(EDGE_TYPE_ATTR_ID, 0).

-define(NODE_TITLE_ATTR_ID, 1).
-define(EDGE_TITLE_ATTR_ID, 1).

-define(NODE_LINE_NUM_ATTR_ID, 2).
-define(MFA_NODE_TYPE_ATTR_ID, 3).


application_colors(Apps) ->
%   crypto:rand_bytes(length(Apps) * 3).
    RGBs =
    %% Get a hash of length 3 of each module name.
    [binary:part(crypto:md5(unicode:characters_to_binary(application_to_string(App))), 0, 3)
     || App <- Apps],
    %% Join all RGBs together.
    iolist_to_binary(RGBs).


application_color(Colors, AppId) ->
    %% First AppId = 1, Skip = 0
    %% Second AppId = 2, Skip = 3
    Skip = (AppId - 1) * 24,
    <<_:Skip, R, G, B, _/binary>> = Colors, 
    gexf:color(R, G, B).

brighter_colors(Mask, Colors) ->
    << <<(X bor Mask)>> || <<X>> <= Colors>>.

mod_color(Mod) ->
    Bin = binary:part(crypto:md5(unicode:characters_to_binary(application_to_string(Mod))), 0, 3),
    <<R, G, B>> = brighter_colors(128, Bin),
    gexf:color(R, G, B).


union(D1, D2) -> 
    orddict:merge(fun(_K, X, _Y) -> X end, D1, D2).

generate(Xref) ->
    %% Apps = [kernel,snmp,stdlib]
    {ok, Apps}  = xref:q(Xref, "A"),
    %% [{kernel, [array, base64,...], {snmp, [...], ...]
    ModByApps = [{App, application_modules(Xref, App)} || App <- Apps],
    Mod2AppPL = [{Mod, App} || {App, Mods} <- ModByApps, Mod <- Mods],
    %% Convert proplist to dict.
    Mod2App = dict:from_list(Mod2AppPL),

    %% `strict' deletes `{M, M}' (same module name).
    %% `|| AM' deletes `{M,$M_EXPR}'.
    {ok, M2MCalls} = xref:q(Xref, "(Mod) strict AE || AM"),

    %% `AM' - analyzed modules.
    {ok, Mods} = xref:q(Xref, "AM"),

    %% Aplication edges.
    {ok, AA} = xref:q(Xref, "strict AE"),

    [erlang:error(no_data) || Mods =:= []],
    AppColors = brighter_colors(128, application_colors(Apps)),

    %% Add an unique id for each node.
    {App2Num,  Next@} = enumerate(Apps),
    {Mod2Num,  Next@} = enumerate(Mods, Next@),

    %% Calculate how many modules are in each application.
    ModCountByApps = [{A, length(Ms)} || {A, Ms} <- ModByApps],

    %% Module to its node id.
    Mod2Id = module_id(Mod2Num, _),
    App2Id = application_id(App2Num, _),

    Mod2AppNumPL = [{Mod, App2Id(App)} || {Mod, App} <- Mod2AppPL],
    Mod2AppNum = dict:from_list(Mod2AppNumPL),
    Mod2AppId = dict:fetch(_, Mod2AppNum),

    %% Function, that converts MFA to GEXF color.
%   ModColor = chain(application_color(AppColors), Mod2AppId),
    ModColor = fun mod_color/1,

    %% The count of clusters.
    ClusterCount = length(Apps),
    ClusterPos = get_application_circle_position(ClusterCount),

    %% Module to its virtual cluster number.
    %% For large modules, the cluster center and the module center are the same.
    Mod2ClusterId = orddict:fetch(_, Mod2AppNum),
    App2ClusterId = orddict:fetch(_, App2Num),

    App2Pos = chain(ClusterPos, App2ClusterId),

    ModNodes = % [[Node]]
    [begin
        %% Calculate the center of the cluster.
        %% This function calculates the coordinates of the point on the circle.
        %% xs and ys are in [-1..1].
        ParentCirclePosValue = App2Pos(App),

        ModPos = get_module_circle_position(length(Mods)),
        WithFuns = fun
            (Mod, NumInCluster) ->
               ModId = Mod2Id(Mod), % :: integer()
               chain(gexf:add_position(chain(
                     %% Decrease the size of the calls' circle and move 
                     %% (the center of this circle is the center of the cluster
                     %% or its virtual center).
                     gexf:relative_position(ParentCirclePosValue),
                     gexf:scale_position(0.07),
                     ModPos -- NumInCluster)),
                   gexf:add_size(gexf:size(5)),
                   gexf:add_color(ModColor(Mod))
                   -- module_node(ModId, Mod))
           end,
        lists2:cmap(WithFuns, Mods)
     end
        || {App, Mods} <- ModByApps],


    ?check(?assertEqual(length(ModNodes), length(ModByApps))),

    %% Render centers of applications.
    AppNodes = 
       [begin
            Id = App2Id(App),
            PosValue = App2Pos(App),
            chain(gexf:add_position(PosValue), 
                  gexf:add_size(gexf:size(15)),
                  gexf:add_color(application_color(AppColors, Id))
                  -- application_node(Id, App))
        end || App <- Apps],

    ?check(?assertEqual(length(AppNodes), length(Apps))),

    Nodes = AppNodes ++ lists:flatten(ModNodes),

    {AA2Num, Next@}   = enumerate(AA),
    {Call2Num, Next@} = enumerate(M2MCalls, Next@),
    {AM2Num,  Next@}  = enumerate(Mod2AppPL, Next@),
    Mod2ModEdges = 
        [call_edge(Mod2Id, Id, FromMod, ToMod)
            || {{FromMod, ToMod}, Id} <- Call2Num],
    App2ModEdges = 
        [application_module_edge(App2Id, Mod2Id, Mod, App, Id)
            || {{Mod, App}, Id} <- AM2Num],
    App2AppEdges = 
        [application_application_edge(App2Id, App1, App2, Id)
            || {{App1, App2}, Id} <- AA2Num],
    Edges = App2AppEdges ++ Mod2ModEdges ++ App2ModEdges, 

    NAttrs = [gexf:attribute_metadata(?NODE_TYPE_ATTR_ID, "node_type", "string")
             ,gexf:attribute_metadata(?NODE_TITLE_ATTR_ID, "node_title", "string")
             ,gexf:attribute_metadata(?NODE_LINE_NUM_ATTR_ID, "line_num", "integer")
             ,gexf:attribute_metadata(?MFA_NODE_TYPE_ATTR_ID, "is_exported", "boolean")
             ],
    EAttrs = [gexf:attribute_metadata(?EDGE_TYPE_ATTR_ID, "edge_type", "string")
             ,gexf:attribute_metadata(?EDGE_TITLE_ATTR_ID, "edge_title", "string")],
    chain(gexf:document_viz,
          gexf:add_attribute_metadata(node, NAttrs),
          gexf:add_attribute_metadata(edge, EAttrs)
          -- gexf:graph(Nodes, Edges)).



module_node(Id, Module) ->
    chain(gexf:add_attribute_value(?NODE_TYPE_ATTR_ID, module),
          gexf:set_label(module_to_string(Module))
          -- gexf:node(Id)).

application_node(Id, App) ->
    chain(gexf:add_attribute_value(?NODE_TYPE_ATTR_ID, app),
          gexf:set_label(application_to_string(App))
          -- gexf:node(Id)).


application_module_edge(App2Id, Mod2Id, Mod, App, Id) ->
    chain(gexf:add_attribute_value(?EDGE_TYPE_ATTR_ID, am)
      -- gexf:edge(Id, Mod2Id(Mod), App2Id(App))).
    
application_application_edge(App2Id, App1, App2, Id) ->
    chain(gexf:set_weight(15),
          gexf:add_attribute_value(?EDGE_TYPE_ATTR_ID, aa)
      -- gexf:edge(Id, App2Id(App1), App2Id(App2))).


module_id(Mod2Num, Module) ->
    orddict:fetch(Module, Mod2Num).

application_id(App2Num, App) ->
    orddict:fetch(App, App2Num).

module_to_application(Mod2App, Mod) ->
    orddict:fetch(Mod, Mod2App).



call_edge(Mod2Id, Id, FromMod, ToMod) ->
    gexf:edge(Id, Mod2Id(FromMod), Mod2Id(ToMod)).

enumerate(Objects) ->
    enumerate(Objects, 1).

enumerate(Objects, From) ->
    {Obj2Num, NextNum} = lists:mapfoldl(fun enumerate_single/2, From, Objects),
    {orddict:from_list(Obj2Num), NextNum}.

enumerate_single(Obj, Num) -> {{Obj, Num}, Num+1}.

mfa_to_string({_M, F, A}) ->
    lists:flatten(io_lib:format("~p/~p", [F, A])).

application_to_string(App) -> atom_to_list(App).

module_to_string(Mod) -> atom_to_list(Mod).


%% ------------------------------------------------------------------
%% Circle
%% ------------------------------------------------------------------

degrees_to_radians(Angle) ->
    Angle * math:pi()/180.

%% A in deegrees.
-spec angle_to_coordinates(Rad) -> {X, Y} when
    X :: 0 .. 1,
    Y :: 0 .. 1,
    Rad :: float().
angle_to_coordinates(A) ->
    {math:cos(A), math:sin(A)}.


%% In degrees.
-define(GAP_SIZE, 45).

arc_length(PointCount) ->
    360 / PointCount.

%% 45 degrees gap.
arc_length_with_gap(PointCount) ->
    (360 - ?GAP_SIZE*2) / PointCount.


get_module_circle_position(PointCount) when PointCount == 0 ->
    0;
get_module_circle_position(PointCount) when PointCount < 20 ->
    get_random_sparse_circle_position(PointCount);

get_module_circle_position(PointCount) ->
    get_dense_circle_position(PointCount).


get_application_circle_position(PointCount) ->
    Gen = ellipint:cached_point_generator(1.7, 1, PointCount, 0),
    fun(Num) ->
        {X, Y} = Gen(Num),
        gexf:position(X, Y, 0)
        end.


get_random_sparse_circle_position(PointCount) ->
    RandomOffset = crypto:rand_uniform(0, 45),
    get_sparse_circle_position(PointCount, RandomOffset).

get_sparse_circle_position(PointCount, Offset) ->
    AL = arc_length(PointCount),
%   io:format(user, "~n~p\t~p~n", [PointCount, Offset]),
    fun(Num) ->
%       io:format(user, "~p\t~p\t~p~n", [PointCount, Offset, Num]),
        {X, Y} = angle_to_coordinates(degrees_to_radians(AL * Num + Offset)),
        gexf:position(X, Y, 0)
        end.

get_dense_circle_position(PointCount) ->
    AL = arc_length_with_gap(PointCount),
    %% There are two places, where node labels are clashes: 
    %% bottom and top of the circle.
    %%
    %% Top node's id is 0. Bottom node's id is CenterNum.
    %% Create gaps.
    CenterNum     = round(PointCount / 2),
    
    %% Offset in deegrees. Round the circle counterclockwise by 90 degrees.
    Offset1 = 90 + (?GAP_SIZE * 0.5) - AL,
    Offset2 = 90 + (?GAP_SIZE * 1.5) - AL,
%   io:format("~n~p ~p ~p  ~p ~p", [CenterNum, AL, PointCount, Offset1, Offset2]),
    fun(Num) when Num =< CenterNum ->
        {X, Y} = angle_to_coordinates(degrees_to_radians(AL * Num + Offset1)),
        gexf:position(X, Y, 0);
       (Num) ->
        {X, Y} = angle_to_coordinates(degrees_to_radians(AL * Num + Offset2)),
        gexf:position(X, Y, 0)
        end.




od_get_value(Key, Dict, Def) ->
    case orddict:find(Key, Dict) of
        {ok, Val} -> Val;
        error -> Def
    end.


application_modules(Xref, App) ->
    %% 1. stdlib : App         = [stdlib]
    %% 2. (Mod) stdlib : App   = [array, base64, ...]
    {ok, Mods} = xref:q(Xref, "(Mod) \"" ++ atom_to_list(App) ++ "\" : App"),
    Mods.


