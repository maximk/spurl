-module(ason).

-export([toAson/1,fromAson/1]).

-import(lists,[map/2,seq/2,all/2,mapfoldl/3,member/2,foldl/3]).

toAson([]) ->
    "<list />";

toAson(true) ->
    "<value boolean=\"true\" />";

toAson(false) ->
    "<value boolean=\"false\" />";

toAson(A) when is_atom(A) ->
    ["<value atom=\"",xml:encode(atom_to_list(A)),"\" />"];

toAson(N) when is_integer(N) ->
    ["<value number=\"",integer_to_list(N),"\" />"];

toAson(N) when is_float(N) ->
    ["<value number=\"",float_to_list(N),"\" />"];

toAson(S) when is_list(S) ->
    case all(fun(C) when is_integer(C), C > 0, C =< 255 -> true; (_) -> false end, S) of
    true ->
        ["<value string=\"",xml:encode(S),"\" />"];

    false ->
        ["<list>",map(fun(E) -> toAson(E) end, S),"</list>"]
    end;

toAson(T) when is_tuple(T) ->
    ["<tuple>",map(fun(E) -> toAson(E) end, tuple_to_list(T)),"</tuple>"];

toAson(<<>>) ->
    "<binary />";
toAson(B) when is_binary(B) ->
    {Ds,_} = mapfoldl(fun(_, <<A,R/binary>>) ->
        [D1,D2] = map(fun(D) when D < 10 ->
            $0+D;
        (D) ->
            $a+D-10
        end, [A div 16,A rem 16]),
        {[D1,D2],R}
    end, B, seq(1, size(B))),
    
    ["<binary>",Ds,"</binary>"];

toAson(P) when is_port(P) ->
    {Node,Serial,Creation} = erlang:prp_triple(P),
    ["<port node=\"",xml:encode(atom_to_list(Node)),
        "\" serial=\"",integer_to_list(Serial),
        "\" creation=\"",integer_to_list(Creation),"\" />"];

toAson(Pid) when is_pid(Pid) ->
    {Node,Serial,Creation} = erlang:prp_triple(Pid),
    ["<pid node=\"",xml:encode(atom_to_list(Node)),
        "\" serial=\"",integer_to_list(Serial),
        "\" creation=\"",integer_to_list(Creation),"\" />"];

toAson(Ref) when is_reference(Ref) ->
    {Node,Serial,Creation} = erlang:prp_triple(Ref),
    ["<ref node=\"",xml:encode(atom_to_list(Node)),
        "\" serial=\"",integer_to_list(Serial),
        "\" creation=\"",integer_to_list(Creation),"\" />"].

fromAson(Ason) ->
    case xml:string(Ason) of
    {ok,Xml} ->
        fromXml(Xml);
    E ->
        E
    end.

fromXml({"list",_,[]}) -> [];

fromXml({"value",[{"boolean","true"}],_}) -> true;
fromXml({"value",[{"boolean","false"}],_}) -> false;

fromXml({"value",[{"number",Number}],_}) ->
    case member($., Number) of
    true -> list_to_float(Number);
    false -> list_to_integer(Number)
    end;

fromXml({"value",[{"atom",Atom}],_}) ->
    list_to_atom(xml:decode(Atom));
fromXml({"value",[{"string",S}],_}) ->
    xml:decode(S);

fromXml({"list",_,Es}) ->
    map(fun(E) -> fromXml(E) end, Es);
fromXml({"tuple",_,Es}) ->
    Ls = map(fun(E) -> fromXml(E) end, Es),
    list_to_tuple(Ls);

fromXml({"binary",_,[]}) ->
    <<>>;
fromXml({"binary",_,[{text,Bs}]}) ->
    {Ds,_} = mapfoldl(fun(_, [A,B|Rs]) ->
        [C,D] = map(fun(Q) when Q >= $a ->
            Q-$a+10;
        (Q) ->
            Q-$0
        end, [A,B]),
        {C*16+D,Rs}
    end, Bs, seq(1, length(Bs) div 2)),
    list_to_binary(Ds);

fromXml({"port",As,_}) ->
    {Node,Serial,Creation} = foldl(fun({"node",Node},{_N,S,C}) ->
        {list_to_atom(Node),S,C};
    ({"serial",Serial},{N,_S,C}) ->
        {N,list_to_integer(Serial),C};
    ({"creation",Creation},{N,S,_C}) ->
        {N,S,list_to_integer(Creation)}
    end, {undefined,undefined,undefined}, As),
    erlang:make_port(Node, Serial, Creation);

fromXml({"pid",As,_}) ->
    {Node,Serial,Creation} = foldl(fun({"node",Node},{_N,S,C}) ->
        {list_to_atom(Node),S,C};
    ({"serial",Serial},{N,_S,C}) ->
        {N,list_to_integer(Serial),C};
    ({"creation",Creation},{N,S,_C}) ->
        {N,S,list_to_integer(Creation)}
    end, {undefined,undefined,undefined}, As),
    erlang:make_pid(Node, Serial, Creation);

fromXml({"ref",As,_}) ->
    {Node,Serial,Creation} = foldl(fun({"node",Node},{_N,S,C}) ->
        {list_to_atom(Node),S,C};
    ({"serial",Serial},{N,_S,C}) ->
        {N,list_to_integer(Serial),C};
    ({"creation",Creation},{N,S,_C}) ->
        {N,S,list_to_integer(Creation)}
    end, {undefined,undefined,undefined}, As),
    erlang:make_ref(Node, Serial, Creation).
    
%% EOF      
