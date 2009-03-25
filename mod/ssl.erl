-module(ssl).

-include("tls.hrl").

-export([accept/1,connect/3]).
-export([recv/1,send/2]).
-export([close/1]).

-import(lists, [member/2]).

accept(ListenSock) ->	%% {ok,Tls,FiveBytes}
	{ok,Sock} = gen_tcp:accept(ListenSock),
	
	%%
	%% discern between http: and https:
	%%
	
	%% 5 bytes is the length of header of
	%% record subprotocol of TLS, for TLS1.0
	%% these 5 bytes will start with 22,3,1
		
	case gen_tcp:recv(Sock, 5) of
	{ok,<<22,3,1,_,_>>=FiveBytes} ->
		{ok,Tls} = tls:server(Sock, FiveBytes),
		{ok,Tls,[]};
	{ok,Bin} ->
		{ok,#tls{sock=Sock},binary_to_list(Bin)}
	end.

connect(Host, Port, Opts) ->		%% {ok,Tls}
	case gen_tcp:connect(Host, Port, Opts) of
	{ok,Sock} ->
		case member(nossl, Opts) of
		true ->
			{ok,#tls{sock=Sock,encrypted=false}};
		false ->
			tls:client(Sock)
		end;
	{error,_}=Error ->
		Error
	end.

recv(#tls{sock=Sock,encrypted=false}=Tls) ->
	%?dbg("recv open: ~w~n", [Tls]),
	case gen_tcp:recv(Sock) of
	{ok,Bin} ->
		{ok,Bin,Tls};
	{error,_}=Error ->
		Error
	end;
recv(Tls) ->
	tls:recv_app_data(Tls).

send(#tls{sock=Sock,encrypted=false}=Tls, Bin) when is_binary(Bin) ->
	gen_tcp:send(Sock, Bin),
	Tls;
send(Tls, Bin) when is_binary(Bin) ->
	tls:send_app_data(Tls, Bin).

close(Tls) ->
	erlang:close_port(Tls#tls.sock).

%% EOF
