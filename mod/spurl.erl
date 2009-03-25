-module(spurl).

-export([start/3]).
-export([parse_bindings/1]).
-export([r/3,r/4]).

-export([req0/2,req1/2,req2/2,req3/2]).

-import(lists,[member/2,concat/1,reverse/1]).

-include("spurl.hrl").

start(TcpPort, Handler, Config)
		when is_integer(TcpPort), is_function(Handler, 2) ->
	
	case gen_tcp:listen(TcpPort, []) of
	{ok,ListenSock} ->
		Pid = spawn(fun() ->
			acceptor(ListenSock, Handler, Config)
		end),
		{ok,Pid};
	{error,_}=E ->
		E
	end.
	
acceptor(ListenSock, Handler, Config) ->

	{ok,Tls,Pre} = ssl:accept(ListenSock),
	
	spawn(fun() ->
		acceptor(ListenSock, Handler, Config)
	end),
	
	requestor(Tls, Pre, Handler, Config).

requestor(Tls, Pre, Handler, Config) ->

	case request(Tls, Pre) of
	done ->
		ok;

	{Req,Pre2} ->
	
		%io:format("Req: ~p~n", [Req#req.what]),

		%%
		%% call to Handler always ends with call
		%% to spurl:r which returns updated Tls
		%% structure
		%%
			
		Tls1 = Handler(Req, Config),
		
		requestor(Tls1, Pre2, Handler, Config)
	end.

request(Tls, Pre) -> request(Tls, req0, [], Pre).
request(Tls, State, Post, []) ->
	case ssl:recv(Tls) of
	{ok,Data,Tls1} ->
		request(Tls1, State, Post, binary_to_list(Data));
	{error,closed} ->
		done;
	{error,Error} ->
		erlang:error(Error)
	end;
request(Tls, State, Post, Pre) ->
	case ?MODULE:State(Post, Pre) of
	{more,State1,Post1} ->
		request(Tls, State1, Post1, []);
		
	{done,Post1,Pre1} ->
		{Meth,What,Params,Vsn,Hdrs} = status_headers(Post1),

		Value = proplists:get_value("content-length", Hdrs, "0"),
		BodySize = list_to_integer(Value),
		{Body,Pre2} = body(Tls, Pre1, BodySize),
		
		{#req{tls=Tls,
			method=Meth,
			what=expand(What),
			params=Params,
			vsn=Vsn,
			headers=Hdrs,
			body=Body},Pre2}
	end.

status_headers(S) ->	%% {Meth,What,Vsn,Hdrs}
	{ok,[ReqLine|HdrLines]} = regexp:split(S,"\r\n"),
	{Meth,WP,Vsn} = req_line(ReqLine),
	
	[What,Params] = case string:tokens(WP, "?") of
		[W,P] -> [W,P];
		[W] -> [W,[]]
	end,	

	Hdrs = hdr_lines(HdrLines),
	{Meth,What,Params,Vsn,Hdrs}.

req_line(Line) ->
	[Verb,What,Proto0] = string:tokens(Line, " "),
	[_,Vs] = string:tokens(Proto0, "/"),
	[Major,Minor] = string:tokens(Vs, "."),
	Proto = {list_to_integer(Major),list_to_integer(Minor)},
	{Verb,What,Proto}.

hdr_lines(Lines) ->
	[{string:to_lower(Key),Value}
		|| {ok,[Key,Value]} <- [regexp:split(Line, ": ")
			|| Line <- Lines, Line =/= []]].

body(Tls, Chunk, N) -> body(Tls, Chunk, N, []).
body(_Tls, Chunk, 0, Body) ->
	{concat(reverse(Body)),Chunk};
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

req0(Post, [$\r|Pre]) -> req1([$\r|Post], Pre);
req0(Post, [Any|Pre]) -> req0([Any|Post], Pre);
req0(Post, []) -> {more,req0,Post}.

req1(Post, [$\n|Pre]) -> req2([$\n|Post], Pre);
req1(Post, [$\r|Pre]) -> req1([$\r|Post], Pre);
req1(Post, [Any|Pre]) -> req0([Any|Post], Pre);
req1(Post, []) -> {more,req1,Post}.

req2(Post, [$\r|Pre]) -> req3([$\r|Post], Pre);
req2(Post, [Any|Pre]) -> req0([Any|Post], Pre);
req2(Post, []) -> {more,req2,Post}.

req3(Post, [$\n|Pre]) -> {done,reverse([$\n|Post]),Pre};
req3(Post, [$\r|Pre]) -> req1([$\r|Post], Pre);
req3(Post, [Any|Pre]) -> req0([Any|Post], Pre);
req3(Post, []) -> {more,req3,Post}.

parse_bindings(T) ->
	lists:map(fun(Pair) ->
		case string:tokens(Pair, "=") of
		[L,R] -> {expand(L),expand(R)};
		[L] -> {expand(L),""}
		end
	end, string:tokens(T, "&")).

expand(L) -> expand(L, []).
expand([$%,H1,H2|L], R) -> expand(L, [from_hex(H1, H2)|R]);
expand([$+|L], R) -> expand(L, [$ |R]);
expand([H|L], R) -> expand(L, [H|R]);
expand([], R) -> lists:reverse(R).

from_hex(H1, H2) ->
	hex(H1) * 16 + hex(H2).

hex(H) when H >= $0, H =< $9 -> H - $0;
hex(H) when H >= $a, H =< $f -> H - $a + 10;
hex(H) when H >= $A, H =< $F -> H - $A + 10.

r(Req, Status, Hdrs) ->
	{NumStat,TxtStat} = status(Status),
	StatusLine = io_lib:format("HTTP/~w.~w ~w ~s\r\n", [1,1,NumStat,TxtStat]),
	Rsp = [StatusLine,
	 [io_lib:format("~s: ~s\r\n", [Key,Value])
		|| {Key,Value} <- Hdrs],
	 "\r\n"
	],
	
	ssl:send(Req#req.tls, list_to_binary(Rsp)).

r(Req, Status, Hdrs, Body) ->
	B = if is_list(Body) ->
		list_to_binary(Body);
	true ->
		Body
	end,

	{NumStat,TxtStat} = status(Status),
	StatusLine = io_lib:format("HTTP/~w.~w ~w ~s\r\n", [1,1,NumStat,TxtStat]),
	Rsp = [StatusLine,
	 [io_lib:format("~s: ~s\r\n", [Key,Value])
		|| {Key,Value} <- Hdrs],
	 io_lib:format("Content-Length: ~w\r\n", [size(B)]),
	 "\r\n"
	],
	
	%io:format("Sending: ~p,~p~n", [Rsp,B]),
	
	Tls1 = ssl:send(Req#req.tls, list_to_binary(Rsp)),
	Tls2 = ssl:send(Tls1, B),
	Tls2.

status(ok) -> {200,"OK"};
status(request_failed) -> {302,"Server error"};
status(bad_request) -> {400,"Bad Request"};
status(404) -> {404,"Not found"}.
		
%%
