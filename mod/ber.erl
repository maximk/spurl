-module(ber).

-export([decode/1]).

-import(lists,[reverse/1]).

%%
%%	decode() returns hierarchical structure and remaining binary,
%%		e.g. {{seq,[...]},R} or {{integer,2},R}
%%

decode(R) ->
	{T,<<>>} = tlv(R),
	T.

tlv(<<Class:2,Composite:1,2#11111:5,R/binary>>) ->
	{Tag,R1} = long_tag(R),
	tlv0(Class, Composite, Tag, R1);
tlv(<<Class:2,Composite:1,Tag:5,R/binary>>) ->
	tlv0(Class, Composite, Tag, R).

long_tag(R) -> long_tag(R, 0).
long_tag(<<0:1,T:7,R/binary>>, Tag) ->
	long_tag(R, Tag*128 + T);
long_tag(<<1:1,T:7,R/binary>>, Tag) ->
	{Tag*128+T,R}.

tlv0(_Class, 1, Tag, <<1:1,0:7,R/binary>>) ->
	{Ds,R1} = decode_until_eoc(R),
	{{tag(Tag),Ds},R1};
tlv0(Class, Composite, Tag, <<1:1,LL:7,R/binary>>) ->
	<<Len:LL/unit:8,R1/binary>> = R,
	tlv1(Class, Composite, Tag, Len, R1);
tlv0(Class, Composite, Tag, <<0:1,Len:7,R/binary>>) ->
	tlv1(Class, Composite, Tag, Len, R).

tlv1(_Class, Composite, Tag, Len, R) ->
	<<V:Len/binary,R1/binary>> = R,
	case Composite of
	0 -> {tv(Tag, V),R1};
	1 -> {{tag(Tag),decode_seq(V)},R1}
	end.

decode_until_eoc(R) -> decode_until_eoc(R, []).
decode_until_eoc(<<0,0,R/binary>>, Ds) ->
	{reverse(Ds),R};
decode_until_eoc(R, Ds) ->
	{TV,R1} = tlv(R),
	decode_until_eoc(R1, [TV|Ds]).

decode_seq(R) -> decode_seq(R, []).
decode_seq(<<>>, Ds) ->
	reverse(Ds);
decode_seq(R, Ds) ->
	{TV,R1} = tlv(R),
	decode_seq(R1, [TV|Ds]).

%% class(0) -> u;
%% class(1) -> a;
%% class(2) -> c;
%% class(3) -> p.

tag(2) -> integer;
tag(3) -> bitstr;
tag(4) -> octstr;
tag(5) -> null;
tag(6) -> objid;
tag(16) -> seq;
tag(17) -> set;
tag(19) -> printstr;
tag(20) -> t61str;
tag(22) -> ia5str;
tag(23) -> utctime;
tag(N) -> N.

tv(2, V) ->
	N = size(V),
	<<I:N/unit:8>> = V,
	{integer,I};
tv(3, <<0,V/binary>>) ->
	{bitstr,V};	%% only zero unused bits
tv(4, V) ->
	{octstr,V};
tv(5, <<>>) ->
	{null, null};
tv(6, V) ->
	{objid,obj_id(V)};
tv(19, V) ->
	{printstr,binary_to_list(V)};
tv(20, V) ->
	{t61str,binary_to_list(V)};
tv(22, V) ->
	{ia5str,binary_to_list(V)};
tv(23, V) ->
	{utctime,V};
tv(N, V) ->
	{N, V}.

obj_id(<<F,R/binary>>) ->
	V1 = F rem 40,
	V2 = F div 40,
	[V1,V2|obj_id0(R)].

obj_id0(R) -> obj_id0(R, []).
obj_id0(<<>>, Vs) ->
	reverse(Vs);
obj_id0(<<0:1,V:7,R/binary>>, Vs) ->
	obj_id0(R, [V|Vs]);
obj_id0(<<1:1,V:7,R/binary>>, Vs) ->
	obj_id0(R, Vs, V).

obj_id0(<<0:1,V:7,R/binary>>, Vs, V0) ->
	obj_id0(R, [V0*128+V|Vs]);
obj_id0(<<1:1,V:7,R/binary>>, Vs, V0) ->
	obj_id0(R, Vs, V0*128+V).

%% EOF
