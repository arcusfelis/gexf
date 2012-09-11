-module(gexf_xsd).

-export([ parse_file/2
        , compile_xsd/0
        , write_hrl/0]).


-type schema_model() :: term().
-type xml_tree() :: term().

-spec compile_xsd() -> {ok, Model} | {error, Reason} when
    Model :: schema_model(),
    Reason :: term().

compile_xsd() ->
    XsdDir = filename:join(code:priv_dir(gexf), "schema"),
    XsdFilePath = filename:join(XsdDir, "gexf.xsd"),
    Viz = file_config("http://www.gexf.net/1.2draft/viz", "viz", undefined),
    erlsom:compile_xsd_file(XsdFilePath, [{include_files, [Viz]}, 
                                          {include_dirs, [XsdDir]}]).

write_hrl() ->
    {ok, Model} = compile_xsd(),
    erlsom:write_hrl(Model, code:lib_dir(gexf, src) ++ "/gexf.hrl").


file_config(Namespace, Prefix, Path) ->
    {Namespace, Prefix, Path}.


%% @doc Parse an example data file using this XSD.
-spec parse_file(XsdModel, DataFileName) ->
        {ok, Tree} | {error, Reason} when
    DataFileName :: string(),
    XsdModel :: schema_model(),
    Tree :: xml_tree(),
    Reason :: term().

parse_file(XsdModel, DataFileName) ->
    DataPath = filename:join([code:priv_dir(gexf), "data", DataFileName]),
    erlsom:parse_file(DataPath, XsdModel).
