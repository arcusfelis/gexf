
    rr(code:lib_dir(erlsom, src) ++ "/*.hrl").

    SD = code:priv_dir(gexf) ++ "/schema/",
    {ok, Model} = erlsom:compile_xsd_file(SD ++ "gexf.xsd", [{include_dirs, [SD]}]).

    erlsom:write_hrl(Model, code:lib_dir(gexf, src) ++ "/gexf.hrl").
    {ok, XML} = erlsom:write(gexf:tree(), erlang:element(2, gexf:compile_xsd())).
