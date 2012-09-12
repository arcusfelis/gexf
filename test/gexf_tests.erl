-module(gexf_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% @doc makeAttrRef returns ":parent-content". It is an error.
simple_test() ->
    Doc = gexf:document(gexf:graph([gexf:node("1")], [gexf:edge("1", "1", "2")])),
    {ok, Xsd}  = gexf_xsd:compile_xsd(),
    {ok, Xml} = erlsom:write(Doc, Xsd),
    io:format(user, "~p", [Xml]).


