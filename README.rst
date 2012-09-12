
    rr(code:lib_dir(erlsom, src) ++ "/*.hrl").

     {ok, Xref} = xref:start(test).
     xref:add_application(Xref, code:lib_dir(gexf)).
     gexf_xref:'e-v'(Xref).                         
     erlsom:write(gexf_xref:'e-v'(Xref), element(2, gexf_xsd:compile_xsd())).

