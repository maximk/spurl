-module(nip).

-export([get/2,get/3,post/3,post_json/3,post_json/4]).
-export([rsp0/2,rsp1/2,rsp2/2,rsp3/2]).

-export([test/1]).

-import(lists,[concat/1,reverse/1,map/2,splitwith/2]).

-record(rsp,{tls,code,desc,vsn,hdrs,body}).

-define(dbg(X, Y), io:format(X, Y)).

test(_) -> test.
	%code:source_line(json, 3657).
	%{ok,Tls} = gen_tcp:connect("localhost", 8080, []),
	%nip:get(Tls, "/orkut.html").

get(Tls, What) ->
	get(Tls, What, []).

get(Tls, What, Params) ->

	Url = if Params =:= [] ->
		ue(What);
	true ->
		[ue(What),$?,
			join2([ue(atom_to_list(Name)) ++ "=" ++ ue(Value)
				|| {Name,Value} <- Params], "&")] 
	end,

	ReqLine = ["GET ",Url," HTTP/1.1\r\n\r\n"],
	
	Bin = list_to_binary(ReqLine),
	Tls1 = ssl:send(Tls, Bin),

	%?dbg("Request ~p sent~n", [Bin]),
	
	{Rsp,_Pre} = response(Tls1),
	{ok,Rsp#rsp.body}.

post(Tls, What, Params) ->

	Ps = [ue(atom_to_list(Name)) ++ "=" ++ ue(Value)
			|| {Name,Value} <- Params],
	Body0 = join2(Ps, "&"),
	Body = list_to_binary(Body0),
	Hdrs = [{"Content-Type","application/json"}],
	post_raw(Tls, What, Hdrs, Body).

post_json(Tls, What, T) ->
	post_json(Tls, What, T, []).

post_json(Tls, What, T, RecInfo) ->
	Body0 = json:toJson(T, RecInfo),
	Body = list_to_binary(Body0),
	Hdrs = [{"Content-Type","application/x-form-urlencoded"}],
	post_raw(Tls, What, Hdrs, Body).

post_raw(Tls, What, Hdrs, Body) ->
	ReqLine = ["POST ",ue(What)," HTTP/1.1\r\n",
		"Content-Length: ",integer_to_list(size(Body)),"\r\n",
		[[K,": ",V,"\r\n"] || {K,V} <- Hdrs],
		"\r\n"],
	
	Bin0 = list_to_binary(ReqLine),
	Tls1 = ssl:send(Tls, Bin0),
	Tls2 = ssl:send(Tls1, Body),
	
	%?dbg("Request ~p,~p sent~n", [Bin0,Bin1]),
	
	%% read response
	{Rsp,_Pre} = response(Tls2),
	{ok,Rsp#rsp.body}.

response(Tls) ->
	response(Tls, rsp0, [], []).

response(Tls, State, Post, []) ->
	case ssl:recv(Tls) of
	{ok,Data,Tls1} ->
		response(Tls1, State, Post, binary_to_list(Data));
	{error,Error} ->
		erlang:error(Error)
	end;

response(Tls, State, Post, Pre) ->
	case ?MODULE:State(Post, Pre) of
	{more,State1,Post1} ->
		response(Tls, State1, Post1, []);
		
	{done,Post1,Pre1} ->
		{Code,Desc,Vsn,Hdrs} = status_headers(Post1),

		Value = proplists:get_value("Content-Length", Hdrs, "0"),
		BodySize = list_to_integer(Value),
		{Body,Pre2,Tls1} = body(Tls, Pre1, BodySize),
		
		{#rsp{tls=Tls1,
			code=Code,
			desc=Desc,
			vsn=Vsn,
			hdrs=Hdrs,
			body=Body},Pre2}
	end.

status_headers(S) ->	%% {Code,Desc,Vsn,Hdrs}
	{ok,[StatusLine|HdrLines]} = regexp:split(S,"\r\n"),
	{Code,Desc,Vsn} = status_line(StatusLine),
	Hdrs = hdr_lines(HdrLines),
	{Code,Desc,Vsn,Hdrs}.

status_line(Line) ->
	%% HTTP/1.1 404 Not Found
	IsNotSpace = fun(32) -> false; (_) -> true end,
	
	{Proto0,[_|Line0]} = splitwith(IsNotSpace, Line),
	{Code0,[_|Desc]} = splitwith(IsNotSpace, Line0),
	
	[_,V] = string:tokens(Proto0, "/"),
	[Major,Minor] = string:tokens(V, "."),
	Vsn = {list_to_integer(Major),list_to_integer(Minor)},
	Code = list_to_integer(Code0),
	{Code,Desc,Vsn}.

hdr_lines(Lines) ->
	[{Key,Value}
		|| {ok,[Key,Value]} <- [regexp:split(Line, ": ")
			|| Line <- Lines, Line =/= []]].

body(Tls, Chunk, N) -> body(Tls, Chunk, N, []).
body(Tls, Chunk, 0, Body) ->
	{concat(reverse(Body)),Chunk,Tls};
body(Tls, [], N, Body) ->
	case ssl:recv(Tls) of
	{ok,Bin,Tls1} ->
		body(Tls1, binary_to_list(Bin), N, Body);
	{error,Error} ->
		erlang:error(Error)
	end;
body(Tls, Chunk, N, Body) when length(Chunk) =< N ->
	body(Tls, [], N-length(Chunk), [Chunk|Body]);
body(Tls, LargeChunk, N, Body) ->
	{Chunk,Tail} = lists:split(N, LargeChunk),
	body(Tls, Tail, 0, [Chunk|Body]).

rsp0(Post, [$\r|Pre]) -> rsp1([$\r|Post], Pre);
rsp0(Post, [Any|Pre]) -> rsp0([Any|Post], Pre);
rsp0(Post, []) -> {more,rsp0,Post}.

rsp1(Post, [$\n|Pre]) -> rsp2([$\n|Post], Pre);
rsp1(Post, [$\r|Pre]) -> rsp1([$\r|Post], Pre);
rsp1(Post, [Any|Pre]) -> rsp0([Any|Post], Pre);
rsp1(Post, []) -> {more,rsp1,Post}.

rsp2(Post, [$\r|Pre]) -> rsp3([$\r|Post], Pre);
rsp2(Post, [Any|Pre]) -> rsp0([Any|Post], Pre);
rsp2(Post, []) -> {more,rsp2,Post}.

rsp3(Post, [$\n|Pre]) -> {done,reverse([$\n|Post]),Pre};
rsp3(Post, [$\r|Pre]) -> rsp1([$\r|Post], Pre);
rsp3(Post, [Any|Pre]) -> rsp0([Any|Post], Pre);
rsp3(Post, []) -> {more,rsp3,Post}.

ue(Us) -> ue(Us, []).
ue([U|Us], Os) when U >= $a, U =< $z; U >= $A, U =< $Z -> ue(Us, [U|Os]);
ue([U|Us], Os) when U >= $0, U =< $9 -> ue(Us, [U|Os]);
ue([$_|Us], Os) -> ue(Us, [$_|Os]);
ue([$/|Us], Os) -> ue(Us, [$/|Os]);
ue([$.|Us], Os) -> ue(Us, [$.|Os]);

ue([U|Us], Os) ->
	[C1,C2] = map(fun(D) when D =< 9 ->
		$0 + D;
	(D) ->
		$a + D - 10
	end, [U div 16,U rem 16]),
	
	ue(Us, [C2,C1,$%|Os]);

ue([], Os) -> reverse(Os).

join2([], _) -> "";
join2(Ss, D) -> string:join(Ss, D).

%% EOF
