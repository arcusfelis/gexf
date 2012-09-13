-module(gexf_xref).
-export(['e-v'/1]).
-export(['e-v-m'/1]).
-compile({parse_transform, cut}).

'e-v'(Xref) ->
    {ok, Funs}  = xref:q(Xref, "V"),
    {ok, Calls} = xref:q(Xref, "E"),
    Fun2Num  = enumerate(Funs),
    Call2Num = enumerate(Calls),
    Nodes = [mfa_node(Id, MFA) || {MFA, Id} <- Fun2Num],
    Edges = [call_edge(Fun2Num, Id, FromMFA, ToMFA) 
                || {{FromMFA, ToMFA}, Id} <- Call2Num],
    gexf:document(gexf:graph(Nodes, Edges)).

'e-v-m'(Xref) ->
    {ok, Funs}  = xref:q(Xref, "V"),
    {ok, Calls} = xref:q(Xref, "E"),
    {ok, Mods}  = xref:q(Xref, "M"),
    ModColor = gexf:color(255, 0, 0),
    FunColor = gexf:color(0, 255, 0),
    Mod2Num  = enumerate(Mods),
    Fun2Num  = enumerate(Funs, length(Mods)+1),
    Call2Num = enumerate(Calls),
    ModFunNodes = fun(Mod) ->
        [gexf:add_color(FunColor, mfa_node(FunId, MFA)) 
         || {MFA, FunId} <- Fun2Num, mfa_to_module(MFA) =:= Mod] end,
    ModNodes = 
        [gexf:set_nodes(ModFunNodes(Mod),
                        gexf:add_color(ModColor, module_node(Id, Mod)))
            || {Mod, Id} <- Mod2Num],
    Fun2FunEdges = 
        [gexf:set_weight(10, 
                         call_edge(Fun2Num, Id, FromMFA, ToMFA))
            || {{FromMFA, ToMFA}, Id} <- Call2Num],
    Mod2FunEdges = 
        [gexf:set_weight(5, 
                         module_function_edge(Fun2Num, Mod2Num, Id, MFA))
            || {MFA, Id} <- Fun2Num],
    Edges = Fun2FunEdges ++ Mod2FunEdges,
    gexf:document_viz(gexf:graph(ModNodes, Edges)).

mfa_to_module({M, _F, _A}) -> M.

mfa_node(Id, MFA) ->
    gexf:set_label(mfa_to_string(MFA), gexf:node(Id)).

module_node(Id, Mod) ->
    gexf:set_label(module_to_string(Mod), gexf:node(Id)).

module_function_edge(Fun2Num, Mod2Num, Id, MFA) ->
    MFA2Id = orddict:fetch(_, Fun2Num),
    Mod2Id = orddict:fetch(_, Mod2Num),
    gexf:edge(Id, MFA2Id(MFA), Mod2Id(mfa_to_module(MFA))).
    

call_edge(Fun2Num, Id, FromMFA, ToMFA) ->
    MFA2Id = orddict:fetch(_, Fun2Num),
    gexf:edge(Id, MFA2Id(FromMFA), MFA2Id(ToMFA)).

enumerate(Objects) ->
    enumerate(Objects, 1).

enumerate(Objects, From) ->
    {Obj2Num, _NextNum} = lists:mapfoldl(fun enumerate_single/2, From, Objects),
    orddict:from_list(Obj2Num).

enumerate_single(Obj, Num) -> {{Obj, Num}, Num+1}.

mfa_to_string({M, F, A}) ->
    lists:flatten(io_lib:format("~p:~p/~p", [M, F, A])).

module_to_string(Mod) -> atom_to_list(Mod).
