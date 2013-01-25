-module(gexf_xref).
-export([generate/2]).
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
-define(NODE_APP_NAME_ATTR_ID, 4).
-define(NODE_APP_COLOR_ATTR_ID, 5).


module_colors(Mods) ->
%   crypto:rand_bytes(length(Mods) * 3).
    RGBs =
    %% Get a hash of length 3 of each module name.
    [binary:part(crypto:md5(unicode:characters_to_binary(module_to_string(Mod))), 0, 3)
     || Mod <- Mods],
    %% Join all RGBs together.
    iolist_to_binary(RGBs).


module_color(Colors, ModId) ->
    %% First ModId = 1, Skip = 0
    %% Second ModId = 2, Skip = 3
    Skip = (ModId - 1) * 24,
    <<_:Skip, R, G, B, _/binary>> = Colors, 
    gexf:color(R, G, B).


app_color(App) ->
    Bin = binary:part(crypto:md5(unicode:characters_to_binary(application_to_string(App))), 0, 3),
    <<R, G, B>> = brighter_colors(128, Bin),
    html_color(R, G, B).

html_color(R, G, B) ->
    lists:flatten(io_lib:format("#~2.16.0B~2.16.0B~2.16.0B", [R, G, B])).


maybe_app_color(undefined) -> undefined;
maybe_app_color(App)       -> app_color(App).

brighter_colors(Mask, Colors) ->
    << <<(X bor Mask)>> || <<X>> <= Colors>>.


union(D1, D2) -> 
    orddict:merge(fun(_K, X, _Y) -> X end, D1, D2).

generate(Xref, Info) ->
    %% Returned values were already sorted and they are unique.
    {ok, XFuns}  = xref:q(Xref, "X"),
    {ok, LFuns}  = xref:q(Xref, "L"),
    {ok, Calls}  = xref:q(Xref, "XC|||(X+L)|||AM"),
    {ok, Mods}   = xref:q(Xref, "AM"), %% Mods :: [atom()]
    [erlang:error(no_data) || Mods =:= []],

    %% Connected functions are called from outside or calls something outside.
    ConFuns = lists:usort(lists:flatmap(fun tuple_to_list/1, Calls)),
    LConFuns = ordsets:intersection(LFuns, ConFuns),
    XConFuns = ordsets:intersection(XFuns, ConFuns),

    ModColors = brighter_colors(128, module_colors(Mods)),

    %% Add an unique id for each node.
    {Mod2Num,  Next@} = enumerate(Mods),
    {XFun2Num, Next@} = enumerate(XConFuns, Next@),
    {LFun2Num, Next@} = enumerate(LConFuns, Next@),


    %% Convert MFA to its node id.
    Fun2Num = union(XFun2Num, LFun2Num),

    %% Group functions by module.
    FunByMods = lists2:group_with(fun({MFA, _}) -> mfa_to_module(MFA) end, Fun2Num),

    %% Calculate how many functions are in each module.
    FunCountByMods = [{M, length(Fs)} || {M, Fs} <- FunByMods],

    %% Mode small functions in the special clusters.
    {LargeFunCountByMods,
     TinyFunCountByModsList} = move_tiny_modules_out(FunCountByMods),

    %% Extract modules' names.
    LargeMods = [M || {M, _C} <- LargeFunCountByMods],

    %% Define how many large modules.
    LargeModCount = length(LargeMods),

    %% Define other numbers on the circle for tiny modules.
    TinyMod2ClusterNum = 
        orddict:from_list(
            packets_to_modules_positions(TinyFunCountByModsList, LargeModCount + 1)),

    %% Function, that converts MFA to GEXF color.
    FunColor = chain(module_color(ModColors), 
                     module_id(Mod2Num), 
                     mfa_to_module),

    %% The count of clusters.
    ClusterCount = LargeModCount + length(TinyFunCountByModsList),

    %% This function calculates the position of the cluster from the cluster id.
    ExpClusterPos = get_module_exp_circle_position(ClusterCount),
    LocClusterPos = get_module_loc_circle_position(ClusterCount),

    %% Module to its node id.
    Mod2Id = orddict:fetch(_, Mod2Num),
    {LargeMod2ClusterNum, _} = enumerate(LargeMods),
    Mod2ClusterNum = union(TinyMod2ClusterNum, LargeMod2ClusterNum),

    %% Module to its virtual cluster number.
    %% For large modules, the cluster center and the module center are the same.
    Mod2ClusterId = orddict:fetch(_, Mod2ClusterNum),

    %% This function converts MFA to its cluster id.
    Fun2ClusterId = chain(Mod2ClusterId, mfa_to_module),

    %% This function returns true, if the passed MFA is exported.
    IsFunExported = ordsets:is_element(_, XConFuns),


    %% SMALL MODULE NODES
    %% Few small modules are combined info one circle, so Funs can have modules
    %% from different modules.
    %% Layout for small modules consists of a set of segments.
    %% While for usual layout the module node is in center, 
    %% the place for small module node is on a circle.
    %%
    %% This code inserts a module node in the head of segment.
    %%
    %% This code sorts functions and modules to put modules before functions 
    %% in each module group.
    SortKeyMaker = fun
        ({{M,_,_} = _MFA, _}) -> 
            M;
        ({M, _ClusterNum}) ->
            M
        end,
    SortedNodes = lists2:collate_with(SortKeyMaker, TinyMod2ClusterNum ++ Fun2Num),

    %% A function for splitting a list into a list of groups.
    GroupKeyMaker = fun
        ({{_,_,_} = MFA, _}) -> 
            {Fun2ClusterId(MFA), IsFunExported(MFA)};
        ({_M, ClusterNum}) ->
            {ClusterNum, true}
        end,
      
    %% This orddict contains function names grouped by their cluster id and function type.
    %% group_with/2 saves order.
    FunGroups = lists2:group_with(GroupKeyMaker, SortedNodes),

    FunNodes = % [[Node]]
    [begin
        %% Calculate the center of the cluster.
        %% This function calculates the coordinates of the point on the circle.
        %% xs and ys are in [-1..1].
        ParentCirclePosValue = 
            if 
                IsExported -> ExpClusterPos(ClusterId);
                %% This paint is the second center of the cluster 
                %% (it is a point of the small ellipse).
                true -> LocClusterPos(ClusterId)
            end,
        Scale = function_node_scale(IsExported),
        NodeSizeValue = chain(gexf:size, function_node_size -- IsExported),
    
        FunPos = get_function_circle_position(length(Funs)),
        WithFuns = fun
            ({MFA = {_,_,_}, FunId}, FunNumInCluster) ->
%          io:format(user, "~p\n~p\t", [MFA, FunId]),
               chain(gexf:add_position(chain(
                     %% Decrease the size of the calls' circle and move 
                     %% (the center of this circle is the center of the cluster
                     %% or its virtual center).
                     gexf:relative_position(ParentCirclePosValue),
                     gexf:scale_position(Scale),
                     FunPos -- FunNumInCluster)),
                   gexf:add_size(NodeSizeValue),
                   gexf:add_color(FunColor(MFA)),
                   mfa_node(Info, FunId, IsExported) -- MFA);

            %% Step 3. It is a special case for module node.
            ({Mod, _ClusterId}, ModNumInCluster) ->
               Id = Mod2Id(Mod),
               chain(gexf:add_position(chain(
                     %% Decrease the size of the calls' circle and move 
                     %% (the center of this circle is the center of the cluster
                     %% or its virtual center).
                     gexf:relative_position(ParentCirclePosValue),
                     gexf:scale_position(Scale),
                     FunPos -- ModNumInCluster)),
                   gexf:add_size(gexf:size(9)),
                   gexf:add_color(module_color(ModColors, Id))
                   -- module_node(Info, Id, Mod))
           end,
        lists2:cmap(WithFuns, Funs)
     end
     %% The `Funs' variable is a list of MFA.
        || {{ClusterId, IsExported}, Funs} <- FunGroups],

    ?check(?assertEqual(length(FunNodes), length(FunGroups))),

    %% Render centers of large modules.
    LargeModNodes = 
       [begin
            Id = Mod2Id(Mod),
            ClusterId = Mod2ClusterId(Mod),
            chain(gexf:add_position(ExpClusterPos(ClusterId)), 
                  gexf:add_size(gexf:size(15)),
                  gexf:add_color(module_color(ModColors, Id))
                  -- module_node(Info, Id, Mod))
        end || Mod <- LargeMods],

    ?check(?assertEqual(length(LargeModNodes), length(LargeMods))),


    %% ME ||| AM: Analyzed modules calls.
    %% AME to a call count
    %% AME is `{ModuleName, ModuleName}'.
    %%
    %% TODO: Are `Calls' sorted? then we can remove `usort'.
    AME2Count = calls_to_ame_count(lists:usort(Calls)),

    Nodes = LargeModNodes ++ lists:flatten(FunNodes),
    {Call2Num, Next@} = enumerate(Calls),
    {MF2Num, Next@}   = enumerate(XConFuns ++ LConFuns, Next@),
    {MMC2Num, _Next}  = enumerate(AME2Count, Next@),
    Fun2FunEdges = 
        [call_edge(Fun2Num, Id, FromMFA, ToMFA)
            || {{FromMFA, ToMFA}, Id} <- Call2Num],
    Mod2FunEdges = 
        [module_function_edge(Fun2Num, Mod2Num, Id, MFA)
            || {MFA, Id} <- MF2Num],
    Mod2ModEdges = 
        [module_module_edge(Mod2Num, M1, M2, CallCount, Id)
            || {{{M1, M2}, CallCount}, Id} <- MMC2Num],
    Edges =  Mod2ModEdges ++ Fun2FunEdges ++ Mod2FunEdges,

    NAttrs = [gexf:attribute_metadata(?NODE_TYPE_ATTR_ID, "node_type", "string")
             ,gexf:attribute_metadata(?NODE_TITLE_ATTR_ID, "node_title", "string")
             ,gexf:attribute_metadata(?NODE_LINE_NUM_ATTR_ID, "line_num", "integer")
             ,gexf:attribute_metadata(?MFA_NODE_TYPE_ATTR_ID, "is_exported", "boolean")
             ,gexf:attribute_metadata(?NODE_APP_NAME_ATTR_ID, "app_name", "string")
             ,gexf:attribute_metadata(?NODE_APP_COLOR_ATTR_ID, "app_color", "string")
             ],
    EAttrs = [gexf:attribute_metadata(?EDGE_TYPE_ATTR_ID, "edge_type", "string")
             ,gexf:attribute_metadata(?EDGE_TITLE_ATTR_ID, "edge_title", "string")],
    chain(gexf:document_viz,
          gexf:add_attribute_metadata(node, NAttrs),
          gexf:add_attribute_metadata(edge, EAttrs)
          -- gexf:graph(Nodes, Edges)).


mfa_to_module({M, _F, _A}) -> M.

mfa_node(Info, Id, IsExported, MFA) ->
    Node =
    chain(gexf:add_attribute_value(?NODE_TYPE_ATTR_ID, mfa),
          add_default_attribute_value(?MFA_NODE_TYPE_ATTR_ID, IsExported, false),
          gexf:set_label(mfa_to_string(MFA))
          -- gexf:node(Id)),
    try
      [Desc, LineNum] = inferno_server:function_info(Info, MFA, [title, position]),
      Node1 = gexf:add_attribute_value(?NODE_LINE_NUM_ATTR_ID, LineNum, Node),
      case Desc of
        undefined ->
          Node1;
        _ ->
          gexf:add_attribute_value(?NODE_TITLE_ATTR_ID, Desc, Node1)
      end
    catch error:_Reason ->
        Node
    end.



module_node(Info, Id, Mod) ->
    Node =
    chain(gexf:add_attribute_value(?NODE_TYPE_ATTR_ID, module),
          gexf:set_label(module_to_string(Mod))
          -- gexf:node(Id)),
    try
      [Desc, AppName] = 
          inferno_server:module_info(Info, Mod, [title, application_name]),
        chain(maybe_attribute_value(?NODE_APP_NAME_ATTR_ID, AppName),
              maybe_attribute_value(?NODE_APP_COLOR_ATTR_ID, maybe_app_color(AppName)),
              maybe_attribute_value(?NODE_TITLE_ATTR_ID, Desc)
              -- Node)
    catch error:_Reason ->
        Node
    end.

module_function_edge(Fun2Num, Mod2Num, Id, MFA) ->
    MFA2Id = orddict:fetch(_, Fun2Num),
    Mod2Id = orddict:fetch(_, Mod2Num),
    chain(gexf:add_attribute_value(?EDGE_TYPE_ATTR_ID, mf)
      -- gexf:edge(Id, MFA2Id(MFA), Mod2Id(mfa_to_module(MFA)))).
    
module_module_edge(Mod2Num, M1, M2, CallCount, Id) ->
    Mod2Id = orddict:fetch(_, Mod2Num),
    chain(gexf:set_weight(CallCount),
          gexf:add_attribute_value(?EDGE_TYPE_ATTR_ID, mm)
      -- gexf:edge(Id, Mod2Id(M1), Mod2Id(M2))).


module_id(Mod2Num, Module) ->
    orddict:fetch(Module, Mod2Num).

call_edge(Fun2Num, Id, FromMFA, ToMFA) ->
    MFA2Id = orddict:fetch(_, Fun2Num),
    gexf:edge(Id, MFA2Id(FromMFA), MFA2Id(ToMFA)).

enumerate(Objects) ->
    enumerate(Objects, 1).

enumerate(Objects, From) ->
    {Obj2Num, NextNum} = lists:mapfoldl(fun enumerate_single/2, From, Objects),
    {orddict:from_list(Obj2Num), NextNum}.

enumerate_single(Obj, Num) -> {{Obj, Num}, Num+1}.

mfa_to_string({_M, F, A}) ->
    lists:flatten(io_lib:format("~p/~p", [F, A])).

module_to_string(Mod) -> atom_to_list(Mod).
application_to_string(App) -> atom_to_list(App).

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


get_function_circle_position(0) ->
    not_fun;
get_function_circle_position(PointCount) when PointCount == 0 ->
    0;
get_function_circle_position(PointCount) when PointCount < 20 ->
    get_random_sparse_circle_position(PointCount);

get_function_circle_position(PointCount) ->
    get_dense_circle_position(PointCount).


get_module_exp_circle_position(0) ->
    not_fun;
get_module_exp_circle_position(PointCount) ->
    {W,H,_,_} = ellipse_dimentions(PointCount),
    Gen = ellipint:cached_point_generator(W, H, PointCount, 0),
    fun(Num) ->
        {X, Y} = Gen(Num),
        gexf:position(X, Y, 0)
        end.


get_module_loc_circle_position(0) ->
    not_fun;
get_module_loc_circle_position(PointCount) ->
    {_,_,W,H} = ellipse_dimentions(PointCount),
    Gen = ellipint:cached_point_generator(W, H, PointCount, 0.05),
    fun(Num) ->
        {X, Y} = Gen(Num),
        gexf:position(X, Y, 0)
        end.


%% `{LargeCircleWidth, LargeCircleHeight, SmallCircleWidth, SmallCircleHeight}'.
ellipse_dimentions(PointCount) when PointCount < 10 -> {0.6, 0.3,   0.3, 0.15};
ellipse_dimentions(_PointCount)                     -> {1.7, 1,     1.4, 0.7}.


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



-spec function_node_size(IsExported) -> Size when 
    IsExported :: boolean(), Size :: number().
function_node_size(true)  -> 5;
function_node_size(false) -> 4.


%% @doc The scale controls the size of the function circle.
-spec function_node_scale(IsExported) -> Scale when 
    IsExported :: boolean(), Scale :: number().

function_node_scale(true)  -> 0.07;
function_node_scale(false) -> 0.05.


-spec move_tiny_modules_out(FunCountByMods) -> {LargeModules, [TinyModules]}
    when LargeModules :: [MFs], TinyModules :: [MFs],
    FunCountByMods :: [MFs],
    FunCount :: non_neg_integer(),
    MFs :: {Module, FunCount},
    Module :: atom().

move_tiny_modules_out([]) -> {[], []};
move_tiny_modules_out(FunCountByMods) ->
    TotalCount = total_function_count(FunCountByMods),
    MCount = length(FunCountByMods),
    AvgFCount = TotalCount / MCount,
    AvgFLimit = round(AvgFCount / 3),
    %% Different algorithm for large applications.
    LimitFCount = if MCount > 30 -> max(MCount div 10, AvgFLimit); true -> AvgFLimit end,
    {Large, Tiny} = 
        lists:partition(fun({_M, C}) -> C > LimitFCount end, FunCountByMods),
    TinyTotalCount = total_function_count(Tiny),
    %% The count of large modules
    LMCount = length(Large),
    LargeTotalCount = TotalCount - TinyTotalCount,
    LargeAvgFCount = round(LargeTotalCount) / LMCount,
    {Large, packets(Tiny, LargeAvgFCount)}.


%% @doc Create a proplist `Mod2Pos', where each module from any `P' has the same position.
%%
%% Example:
%%
%% packets_to_modules_positions([[{x, 0}, {y, 0}], [{z, 0}]], 5) ->
%%     [{x, 5}, {y, 6}, {z, 7}].
packets_to_modules_positions([P|Ps], Pos) ->
    Mod2Pos = [{M, Pos} || {M, _C} <- P],
    Mod2Pos ++ packets_to_modules_positions(Ps, Pos+1);
packets_to_modules_positions([], _Pos) ->
    [].
    


total_function_count(FunCountByMods) ->
    lists:foldl(fun({_M, C}, A) -> A + C end, 0, FunCountByMods).


%% @doc Split `FunCountByMods' on chunks maximum size `MaxPacketSize'.
packets([], _) ->
    [];
packets(FunCountByMods, MaxPacketSize) ->
    packets(FunCountByMods, [], [], 0, MaxPacketSize).


%% CurPacketSize and MaxPacketSize are count of functions.
-spec packets(FunCountByMods, MAcc, MMAcc, CurPacketSize, MaxPacketSize) ->
    MMAcc when
    MAcc :: FunCountByMods,
    MMAcc :: [FunCountByMods],
    CurPacketSize :: PacketSize,
    MaxPacketSize :: PacketSize,
    PacketSize :: non_neg_integer().

packets([{_M, C}|_], MAcc, MMAcc, CurPacketSize, MaxPacketSize)
    when (CurPacketSize + C) > MaxPacketSize ->
    % The part was formed. Clear the buffer, run again.
    me(_, [], [lists:reverse(MAcc)|MMAcc], 0, _);

packets([{_M, C}=H|T], MAcc, _, CurPacketSize, _) ->
    me(T, [H|MAcc], _, CurPacketSize + C, _);

%% Handle the end of `FunCountByMods'.
packets([], [_|_] = MAcc, MMAcc, _, _) ->
    lists:reverse(MMAcc, [lists:reverse(MAcc)]).


%% @doc Group elements in the tuple using E1 as a key and E2 as a value.
%% Input list is an ordset.
%% tuple_e1_key_e2_value_groups([{x, 1}, {x, 2}, {y, 2}]) -> [{x, [1,2]}, {y, [2]}].
tuple_e1_key_e2_value_groups([]) -> 
    [];
tuple_e1_key_e2_value_groups([{K, V}|Ts]) ->
    tuple_e1_key_e2_value_groups(Ts, K, [V], []).
    

%% tuple_e1_key_e2_value_groups(Tuples, K, Acc1, Acc2).

tuple_e1_key_e2_value_groups([{K, V}|Ts], K, Acc1, _) ->
    me(Ts, _, [V|Acc1], _);

tuple_e1_key_e2_value_groups([{K, V}|Ts], OldK, Acc1, Acc2) ->
    me(Ts, K, [V], [{OldK, lists:reverse(Acc1)}|Acc2]);

tuple_e1_key_e2_value_groups([], K, Acc1, Acc2) ->
    lists:reverse(Acc2, [{K, lists:reverse(Acc1)}]).


swap_pairs(Pairs) ->
    [{V, K} || {K, V} <- Pairs].








%% @doc A wrapper, that allows to skip default value of the element.
add_default_attribute_value(_AttrId, Default, Default, Node) -> Node;
add_default_attribute_value(AttrId, Value, _Default, Node) ->
    gexf:add_attribute_value(AttrId, Value, Node).


maybe_attribute_value(AttrId, Value, Node) ->
    add_default_attribute_value(AttrId, Value, undefined, Node).


%% @doc Converts a list of `{FromMFA, ToMFA}' to a list of `{{Mod, Mod}, CountOfCalls}'.
calls_to_ame_count(Calls) ->
    lists2:group_count_with(fun call_to_ame/1, Calls).

call_to_ame({{M1,_,_}, {M2,_,_}}) ->
    {M1,M2}.


od_get_value(Key, Dict, Def) ->
    case orddict:find(Key, Dict) of
        {ok, Val} -> Val;
        error -> Def
    end.
