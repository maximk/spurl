-module(json).

-export([toJson/1,toJson/2,fromJson/1,fromJson/2]).

-import(lists,[all/2,map/2,filter/2,seq/2,zip/2,reverse/1,keysearch/3]).
-import(lists,[keysort/2,unzip/1]).
-import(string,[join/2]).

%%
%% Erlang term -> Javascript value
%%
%% ""					-> ""
%% "abc"				-> "abc"
%% #rec{T1,T2,T3}		-> {record: rec, field1: T1, field2: T2, field3, T3}
%% true					-> true
%%
%% 123					-> 123
%% 123.5				-> 123.5
%%
%% abc					-> {atom: "abc"}
%%
%% <<1,2,3>>			-> {binary: "abc5463ef647"}
%%
%% #Ref<1.2.3>			-> {reference: serial, node: abc, creation: 0}
%% #Port<1.2.3>			-> {port: serial, node: abc, creation: 0}
%% <1.2.3>				-> {pid: serial, node: abc, creation: 0}
%%
%% {T1,T2,T3}			-> {1: T1, 2: T2, 3: T3}
%% [T1,T2,T3]			-> [T1, T2, T3]
%% 
%% #Fun<>				-> {fun: name, other}
%%

%%
%% RecInfo:
%%  [{record,Fields}], e.g. [{employee,["last_name","first_name"]}]
%%
%%

toJson(Term) -> toJson(Term, []).		%% no info on records

toJson([], _) -> "[]";
toJson([_|_]=Ls, RecInfo) ->
	case all(fun(L) when is_integer(L), L >= 0, L =< 255 -> true; (_) -> false end, Ls) of
	true ->
		jstring(Ls);
	false ->
		"[ " ++
			join(map(fun(L) ->
				toJson(L, RecInfo)
			end, Ls), ", ")
		++ " ]"
	end;
toJson(T, RecInfo) when is_tuple(T) ->
	case filter(fun({Rec,Flds}) ->
			element(1, T) =:= Rec andalso size(T) =:= length(Flds) + 1
		end, RecInfo) of
	[] ->
		"{ " ++
			join(map(fun(N) ->
					integer_to_list(N) ++ ": " ++ toJson(element(N, T), RecInfo)
				end, seq(1, size(T))), ", ")
		++ " }";
			
	[{_,Fields}] ->
		[V0|Values] = tuple_to_list(T),
		Record = atom_to_list(V0),
		"{ " ++
			join(map(fun({Fld,Val}) ->
					Fld ++ ": " ++ toJson(Val, RecInfo)
				end, zip(["record"|Fields], [Record|Values])), ", ")
		++ " }"
	end;

toJson(true, _) -> "true";
toJson(false, _) -> "false";

toJson(N, _) when is_integer(N) -> integer_to_list(N);
toJson(N, _) when is_float(N) -> float_to_list(N);

toJson(A, _) when is_atom(A) ->

	"{ atom: " ++ jstring(atom_to_list(A)) ++ " }";

toJson(Bin, _) when is_binary(Bin) ->
	
	"{ binary: '" ++ hexify(Bin) ++ "' }";
	
toJson(Ref, _R) when is_reference(Ref) ->

	{Node,Serial,Creation} = erlang:prp_triple(Ref),
	"{ reference: " ++ integer_to_list(Serial) ++ ", "
	  "node: " ++ toJson(Node, _R) ++ ", "
	  "creation: " ++ integer_to_list(Creation) ++ " }";

toJson(Port, _R) when is_port(Port) ->

	{Node,Serial,Creation} = erlang:prp_triple(Port),
	"{ port: " ++ integer_to_list(Serial) ++ ", "
	  "node: " ++ toJson(Node, _R) ++ ", "
	  "creation: " ++ integer_to_list(Creation) ++ " }";

toJson(Pid, _R) when is_pid(Pid) ->

	{Node,Serial,Creation} = erlang:prp_triple(Pid),
	"{ pid: " ++ integer_to_list(Serial) ++ ", "
	  "node: " ++ toJson(Node, _R) ++ ", "
	  "creation: " ++ integer_to_list(Creation) ++ " }";

toJson(Fun, _) when is_function(Fun) ->
	
	{name,Name} = erlang:fun_info(Fun, name),
	"{ function: " ++ jstring(atom_to_list(Name)) ++ " }".

hexify(Bin) when is_binary(Bin) -> hexify(binary_to_list(Bin), []).
hexify([N|Ns], As) -> hexify(Ns, [digit(N rem 16),digit(N div 16)|As]);
hexify([], As) -> reverse(As).

digit(N) when N =< 9 -> $0 + N;
digit(N) -> $a + N - 10.

tigid(C) when C >= $a, C =< $f -> C - $a + 10;
tigid(C) -> C - $0.

unhexify(Ps) -> unhexify(Ps, []).
unhexify([A,B|Ps], Os) ->
	Byte = tigid(A) * 16 + tigid(B),
	unhexify(Ps, [Byte|Os]);
unhexify([], Os) -> list_to_binary(reverse(Os)).

jstring(Ss) -> jstring(Ss, [$']).
jstring([$'|Ss], As) ->
	jstring(Ss, [$',$\\|As]);
jstring([$\n|Ss], As) ->
	jstring(Ss, [$n,$\\|As]);
jstring([$\r|Ss], As) ->
	jstring(Ss, [$r,$\\|As]);
jstring([$\\|Ss], As) ->
	jstring(Ss, [$\\,$\\|As]);
jstring([N|Ss], As) when is_integer(N), N >= $ , N =< $~ ->
	jstring(Ss, [N|As]);
jstring([S|Ss], As) when is_integer(S) ->
	Os = erlang:integer_to_list(S, 8),
	Os2 = string:copies("0", 3 - length(Os)) ++ Os,
	jstring(Ss, lists:reverse(Os2) ++ [$\\|As]);
jstring([], As) -> lists:reverse([$'|As]).

fromJson(Json) -> fromJson(Json, []).
fromJson(Json, RecInfo) -> {V,[]} = term(Json, RecInfo), V.

term(L, RecInfo) -> term1(ws(L), RecInfo).

term1([$"|L], _) -> str2(L);
term1([$'|L], _) -> str1(L);
term1([$[|L], RecInfo) -> array(L, RecInfo);
term1([${|L], RecInfo) -> object(L, RecInfo);
term1([C|_]=L, _) when C >= $0, C =< $9; C =:= $- -> number(L);
term1(L, _) -> unexpected(L).

str2(L) -> str2(L, []).
str2([$"|L], O) -> {reverse(O),ws(L)};
str2([$\\,$t|L], O) -> str2(L, [8|O]);
str2([$\\,$n|L], O) -> str2(L, [10|O]);
str2([$\\,$r|L], O) -> str2(L, [13|O]);
str2([$\\,C|L], O) -> str2(L, [C|O]);
str2([C|L], O) -> str2(L, [C|O]);
str2([], _) -> throw(unterminated_string).

str1(L) -> str1(L, []).
str1([$'|L], O) -> {reverse(O),ws(L)};
str1([$\\,$t|L], O) -> str1(L, [8|O]);
str1([$\\,$n|L], O) -> str1(L, [10|O]);
str1([$\\,$r|L], O) -> str1(L, [13|O]);
str1([$\\,C|L], O) -> str1(L, [C|O]);
str1([C|L], O) -> str1(L, [C|O]);
str1([], _) -> throw(unterminated_string).

number(L) -> number(L, [], false).
number([$-|L], O, IsFloat) -> number(L, [$-|O], IsFloat);
number([$+|L], O, IsFloat) -> number(L, [$+|O], IsFloat);
number([C|L], O, IsFloat) when C >= $0, C =< $9 -> number(L, [C|O], IsFloat);
number([$.|L], O, _) -> number(L, [$.|O], true);
number([$#|L], O, IsFloat) -> number(L, [$#|O], IsFloat);
number([$e|L], O, _) -> number(L, [$e|O], true);
number([$E|L], O, _) -> number(L, [$E|O], true);
number(L, O, false) -> {list_to_integer(reverse(O)),ws(L)};
number(L, O, true) -> {list_to_float(reverse(O)),ws(L)}.

array(L, RecInfo) -> array(ws(L), [], RecInfo).
array([$]|L], O, _) -> {reverse(O),ws(L)};
array(L, O, RecInfo) -> {V,L1} = term(L, RecInfo), array1(L1, [V|O], RecInfo).

array1([$]|L], O, _) -> {reverse(O),ws(L)};
array1([$,|L], O, RecInfo) -> {V,L1} = term(L, RecInfo), array1(L1, [V|O], RecInfo);
array1(L, _, _) -> unexpected(L).

object(L, RecInfo) -> object(ws(L), [], RecInfo).
object([$}|L], O, RecInfo) -> {transform_object(reverse(O), RecInfo),ws(L)};
object(L, O, RecInfo) ->
	{Fld,L1} = field(L), {V,L2} = term(L1, RecInfo), 
	object1(L2, [{Fld,V}|O], RecInfo).

object1([$}|L], O, RecInfo) -> {transform_object(reverse(O), RecInfo),ws(L)};
object1([$,|L], O, RecInfo) ->
	{Fld,L1} = field(L), {V,L2} = term(L1, RecInfo), 
	object1(L2, [{Fld,V}|O], RecInfo);
object1(L, _, _) -> unexpected(L).

field(L) -> field(ws(L), []).
field([$"|L], O) -> field2(L, O);
field([$'|L], O) -> field1(L, O);
field(L, O) -> field3(L, O).

field3([$:|L], O) -> {reverse(O),ws(L)};
field3([C|L], O) when C >= $0, C =< $9; C >= $a, C =< $z; C >= $A, C =< $Z; C =:= $_ ->
	field3(L, [C|O]);
field3(L, O) -> field0(ws(L), O).

field1([$'|L], O) -> field0(ws(L), O);
field1([C|L], O) -> field1(L, [C|O]).

field2([$"|L], O) ->field0(ws(L), O);
field2([C|L], O) -> field2(L, [C|O]).

field0([$:|L], O) -> {reverse(O),ws(L)};
field0(L, _) -> unexpected(L).

unexpected(L) ->
	throw({unexpected_input,L}).
	
ws([32|L]) -> ws(L);
ws([ 8|L]) -> ws(L);
ws([10|L]) -> ws(L);
ws([13|L]) -> ws(L);
ws(L) -> L.

transform_object([{"atom",S}], _) -> list_to_atom(S);
transform_object([{"binary",H}], _) -> unhexify(H);
transform_object([{"fun",Name}], _) -> throw({fun_term,Name});

transform_object(Obj, RecInfo) ->

	Ref = keysearch("reference", 1, Obj),
	Pid = keysearch("pid", 1, Obj),
	Port = keysearch("port", 1, Obj),
	
	if Ref /= false; Pid /= false; Port /= false ->

		{value,{_,Node}} = keysearch("node", 1, Obj),
		{value,{_,Creation}} = keysearch("creation", 1, Obj),
		
		case {Ref,Pid,Port} of
		{{value,{_,Serial}},false,false} ->
			erlang:make_ref(Node, Serial, Creation);
		{false,{value,{_,Serial}},false} ->
			erlang:make_pid(Node, Serial, Creation);
		{false,false,{value,{_,Serial}}} ->
			erlang:make_port(Node, Serial, Creation)
		end;
	
	true ->
	
		case keysearch("record", 1, Obj) of
		{value,{_,Record0}} ->
			%erlang:display(Obj),
			Record = list_to_atom(Record0),
		
			case keysearch(Record, 1, RecInfo) of
			{value,{_,Fields}} ->
				reconstruct_record(Obj, Record, Fields);
			false ->
				throw({unknown_record,Record})
			end;
		
		false ->
			Obj2 = map(fun({Fld,Val}) -> {list_to_integer(Fld),Val} end, Obj),				
			{_,Values} = unzip(keysort(1, Obj2)),
		
			list_to_tuple(Values)
		end
	end.

reconstruct_record(Obj, Record, Fields) ->

	Values = map(fun(Fld) ->
		case keysearch(Fld, 1, Obj) of
		{value,{_,Val}} ->
			Val;
		false ->
			undefined
		end
	end, Fields),
	
	list_to_tuple([Record|Values]).

%%EOF
