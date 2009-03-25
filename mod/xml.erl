-module(xml).
-export([string/1]).
-export([encode/1,decode/1]).

-import(lists, [reverse/1]).

-define(IS_NAME_CHAR(X), X >= $a, X =< $z; X >= $A, X =< $Z; X >= $0, X =< $9; X =:= $.; X =:= $-; X =:= $_).
-define(IS_SPACE(X), X =:= 32; X =:= 9; X =:= 13; X =:= 10).
-define(IS_DIGIT(X), X >= $0, X =< $9).
-define(IS_HEX_DIGIT(X), X >= $0, X =< $9; X >= $a, X =< $f; X >= $A, X =< $F).

string(Xml) ->

	%
	% convert utf-8 encoded characters to one-byte
	% representation - if codepoint > 255 replace with "?"
	% 
	Xml1 = asciify(Xml),
	
    case catch doc(Xml1) of
    {'EXIT',E} ->
        {error,E};
    {stag,E,[]} ->
        {ok,E};
    X ->
        {error,X}
    end.

%% 0xxxxxxx - one byte (ASCII)
%% 110yyyxx 10xxxxxx - two bytes (y x)
%% 1110yyyy 10yyyyxx 10xxxxxx - three bytes (y x)
%% 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx - four bytes (z y x)

asciify(Xs) -> asciify(Xs, []).
asciify([X|Xs], Os) when X < 2#10000000 ->
	asciify(Xs, [X|Os]);
asciify([X|_Xs], _Os) when X < 2#11000000 ->
	erlang:error({utf8,X});	%% 2nd, 3rd or 4th byte of sequence
asciify([X|_Xs], _Os) when X < 2#11000010 ->
	erlang:error({utf8,X});	%% overlong encoding
asciify([X,Y|Xs], Os) when X < 2#11100000, Y >= 2#10000000, Y < 2#11000000 ->
	<<_:3,B1:5,_:2,B2:6>> = <<X,Y>>,
	<<C:16>> = <<0:5,B1:5,B2:6>>,
	asciify(Xs, [question(C)|Os]);
asciify([X,Y,Z|Xs], Os) when X < 2#11110000,
		Y >= 2#10000000, Y < 2#11000000,
		Z >= 2#10000000, Z < 2#11000000 ->
	<<_:4,B1:4,_:2,B2:6,_2,B3:6>> = <<X,Y,Z>>,
	<<C:16>> = <<B1:4,B2:6,B3:6>>,
	asciify(Xs, [question(C)|Os]);
asciify([X,Y,Z,Q|Xs], Os) when X < 2#11110101,
		Y >= 2#10000000, Y < 2#11000000,
		Z >= 2#10000000, Z < 2#11000000,
		Q >= 2#10000000, Q < 2#11000000 ->
	<<_:5,B1:3,_:2,B2:6,_2,B3:6,_:2,B4:6>> = <<X,Y,Z>>,
	<<C:24>> = <<0:3,B1:3,B2:6,B3:6,B4:6>>,
	asciify(Xs, [question(C)|Os]);
asciify([X|_Xs], _Os) ->
	erlang:error({utf8,X});
asciify([], Os) ->
	reverse(Os).

question(C) when C < 256 ->	C;
question(_) -> $?.

%% the first character of the document
doc("<?xml" ++ Xml) -> decl_elem(Xml);
doc("<" ++ Xml) -> elem(Xml).

%% after "<?xml"
decl_elem("?>" ++ Xml) -> elem0(ws(Xml));
decl_elem([_|Xml]) -> decl_elem(Xml).

elem0("<" ++ Xml) -> elem(Xml).

%% after "<"
elem("/" ++ Xml) -> elem_end_name(Xml, []);
elem(Xml) -> elem_name(Xml, []).

elem_end_name([X|Xml], Ns) when ?IS_NAME_CHAR(X) ->
    elem_end_name(Xml, [X|Ns]);
elem_end_name(Xml, Ns) when Ns =/= [] ->
    elem_end_name0(ws(Xml), reverse(Ns)).
    
elem_end_name0(">" ++ Xml, Ns) ->
    {etag,Ns,Xml}.

elem_name([X|Xml], Ns) when ?IS_NAME_CHAR(X) ->
    elem_name(Xml, [X|Ns]);
elem_name(Xml, Ns) when Ns =/= [] ->
    elem_attrs(ws(Xml), reverse(Ns), []).

%% at 'attr=' or '>' or '/>'
elem_attrs(Xml, Ns, As) -> elem_attrs(Xml, Ns, As, []).

elem_attrs(">" ++ Xml, Ns, As, []) ->
    content(Xml, Ns, As);
elem_attrs("/>" ++ Xml, Ns, As, []) ->
    {stag,{Ns,As,[]},Xml};
elem_attrs([X|Xml], Ns, As, Rs) when ?IS_NAME_CHAR(X) ->
    elem_attrs(Xml, Ns, As, [X|Rs]);
elem_attrs(Xml, Ns, As, Rs) ->
    elem_attrs_eq(ws(Xml), Ns, As, reverse(Rs)).

elem_attrs_eq("=" ++ Xml, Ns, As, Rs) ->
    elem_attrs_val0(ws(Xml), Ns, As, Rs).

elem_attrs_val0([$"|Xml], Ns, As, Rs) ->
    elem_attrs_val(Xml, Ns, As, Rs, []).

elem_attrs_val([$"|Xml], Ns, As, Rs, Vs) ->
    A = {Rs,reverse(Vs)},
    elem_attrs(ws(Xml), Ns, [A|As]);
elem_attrs_val([X|Xml], Ns, As, Rs, Vs) ->
    elem_attrs_val(Xml, Ns, As, Rs, [X|Vs]).

%% after '>'
content(Xml, Ns, As) -> content(Xml, Ns, As, [], []).

content("<" ++ Xml0, Ns, As, Cs, []) ->
    case elem(Xml0) of
    {etag,Ns,Xml} ->
        {stag,{Ns,As,reverse(Cs)},Xml};
    {stag,T,Xml} ->
        content(Xml, Ns, As, [T|Cs], [])
    end;
content("<" ++ _ =Xml, Ns, As, Cs, Bs) ->
	case ws(Bs) of
	[] ->
		content(Xml, Ns, As, Cs, []);
	Bs1 ->
		Text = {text,ws(reverse(Bs1))},
		content(Xml, Ns, As, [Text|Cs], [])
	end;
content([X|Xml], Ns, As, Cs, Bs) ->
    content(Xml, Ns, As, Cs, [X|Bs]).

ws([X|Xml]) when ?IS_SPACE(X) -> ws(Xml);
ws(Xml) -> Xml.

encode(Bin) when is_binary(Bin) ->
    encode(binary_to_list(Bin));
encode(Ls) -> encode(Ls, []).

encode([X|Xs], Os) when ?IS_NAME_CHAR(X); ?IS_SPACE(X) ->  %% TODO
    encode(Xs, [X|Os]);
encode([X|Xs], Os) ->
    A = X div 100,
    B = (X - A * 100) div 10,
    C = X - A * 100 - B * 10,
    case {A,B,C} of
    {0,0,C1} -> encode(Xs, [["&#",$0+C1,";"]|Os]);
    {0,B1,C1} -> encode(Xs, [["&#",$0+B1,$0+C1,";"]|Os]);
    {A1,B1,C1} -> encode(Xs, [["&#",$0+A1,$0+B1,$0+C1,";"]|Os])
    end;
encode([], Os) -> reverse(Os).

decode(Xs) ->
    decode(Xs, []).

decode("&amp;" ++ Xs, Os) ->
    decode(Xs, [$&|Os]);
decode("&apos;" ++ Xs, Os) ->
    decode(Xs, [$'|Os]);
decode("&quot;" ++ Xs, Os) ->
    decode(Xs, [$"|Os]);
decode("&lt;" ++ Xs, Os) ->
    decode(Xs, [$<|Os]);
decode("&gt;" ++ Xs, Os) ->
    decode(Xs, [$>|Os]);
decode("&#x" ++ [A,$;|Xs], Os) when ?IS_HEX_DIGIT(A) ->
    decode(Xs, [hex(A)|Os]);
decode("&#x" ++ [A,B,$;|Xs], Os) when ?IS_HEX_DIGIT(A),?IS_HEX_DIGIT(B) ->
    decode(Xs, [hex(A)*16+hex(B)|Os]);
decode("&#" ++ [A,$;|Xs], Os) when ?IS_DIGIT(A) ->
    decode(Xs, [A-$0|Os]);
decode("&#" ++ [A,B,$;|Xs], Os) when ?IS_DIGIT(A),?IS_DIGIT(B) ->
    decode(Xs, [(A-$0)*10+B-$0|Os]);
decode("&#" ++ [A,B,C,$;|Xs], Os) when ?IS_DIGIT(A),?IS_DIGIT(B),?IS_DIGIT(C) ->
    decode(Xs, [(A-$0)*100+(B-$0)*10+C-$0|Os]);
decode([X|Xs], Os) ->
    decode(Xs, [X|Os]);
decode([], Os) ->
    reverse(Os).

hex(X) when X >= $0, X =< $9 -> X-$0;
hex(X) when X >= $a, X =< $f -> X-$a+10;
hex(X) when X >= $A, X =< $F -> X-$A+10.

%% EOF
