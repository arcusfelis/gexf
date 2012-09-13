-module(gexf_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% @doc makeAttrRef returns ":parent-content". It is an error.
simple_test() ->
    _Xml = document_string().
%   io:format(user, "~s", [Xml]).

validate_test() ->
    Xml = document_string(),
%   io:format(user, "~s", [Xml]),
    {E,_} = xmerl_scan:string(unicode:characters_to_list(Xml)),
    {ok,S} = xmerl_xsd:process_schema(schema_path(), []),
    case xmerl_xsd:validate(E,S) of
        {error, Reason} -> 
            Info = xmerl_xsd:format_error(Reason),
            io:format(user, "Error: ~s~n", [Info]),
            erlang:error(Reason);
        {_ValidElem, _GlobalState} ->
%           io:format(user, "~s", [xmerl:export([ValidElem], xmerl_xml)])
            ok
    end.

document() ->
    N3 = gexf:node("3"),
    Color = gexf:color(255, 0, 0),
    N1 = gexf:add_color(Color,
                        gexf:set_label("1", 
                                       gexf:set_nodes([N3], gexf:node("1")))),
    gexf:document_viz(gexf:graph([N1], [gexf:edge("1", "1", "2")])).

document_string() ->
    Doc = document(),
    xmerl:export_simple([Doc], xmerl_xml, []).

schema_path() ->
    filename:join([code:priv_dir(gexf), schema, gexf.xsd]).
