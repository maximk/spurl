-module(muse).
-export([start/1,handler/2]).

-import(lists,[map/2,filter/2,reverse/1,flatten/1]).

-include("spurl.hrl").

-define(APPLY_THROWS_EXCEPTION, 1).
-define(APPLY_RETURNS_VALUE, 0).

start(Args) ->

	%code:load_module(json),
	%erlang:display(code:source_line(json, 3657)),

	Opts = opts(Args),
		
	case proplists:get_value(node, Opts, local) of
	local ->
		ok;
	N1 ->
		Node = list_to_atom(N1),
		
		S1 = proplists:get_value(sinks, Opts, ""),
		E1 = proplists:get_value(endpoints, Opts, ""),
		R1 = proplists:get_value(routes, Opts, ""),
		
		Sinks = parse_sinks(S1),
		Endpoints = parse_endpoints(E1),
		Routes = parse_routes(R1),
		
		netmesh:start(Node, Sinks, Endpoints, Routes),
		
		io:format("Node ~w started...~n", [Node])
	end,
	
	case proplists:get_value(port, Opts, noweb) of
	noweb ->
		ok;
		
	O ->
		WebPort = list_to_integer(O),
		
		{ok,Cwd} = file:get_cwd(),
		WebRoot = proplists:get_value(webroot, Opts, Cwd ++ "/web"),
		
		RecInfo = [
			
			%% recognize 'axe' request envelopes
			{axe,["n","m","f","as"]},
			
			%% dojo datastores
			{datastore,["id","label","items"]},	
			
			%% dojo FileStore item
			{file_store_item,
				["directory","name","path","size","parentDir","children"]}
		],
		
		ShowFiles = proplists:get_value(showfiles, Opts, false),
		
		case spurl:start(WebPort,
			fun ?MODULE:handler/2,
			#conf{webroot=WebRoot,recinfo=RecInfo,showfiles=ShowFiles}) of
		{ok,_} ->
			io:format("Muse started...~n", []);
		{error,Error} ->
			io:format("Unable to start muse: ~p~n", [Error])
		end,
        
        comet:start(),
		io:format("Comet started...~n", []),
        
		%%attach to stdio
		stdio:add_hook(comet, fun(IOList) ->
			
			Bin = erlang:list_to_binary([IOList]),
			Msg = erlang:binary_to_list(Bin),
			
			comet ! {message,self(),{stdout,Msg}}
		end)
	end,
	
	case proplists:get_value(daemonize, Opts, false) of
	true ->
		erlang:daemonize();
	false ->
		ok
	end.

opts(As) -> opts(As, []).

opts(["--" ++ Ks|As], Os) ->
	K = list_to_atom(Ks),
	opts(As, [K|Os]);
opts(["-" ++ Ks,V|As], Os) ->
	K = list_to_atom(Ks),
	opts(As, [{K,unquote(V)}|Os]);
opts([], Os) -> Os.

unquote([$"|V]) -> unquote1(reverse(V));
unquote(V) -> V.
unquote1([$"|V]) -> reverse(V);
unquote1(V) -> reverse(V).

%% netudp:8888,netudp:9999
parse_sinks("") -> [];
parse_sinks(S) ->
	lists:map(fun(T) ->
		[M,P] = string:tokens(T, ":"),
		Mod = list_to_atom(M),
		Port = list_to_integer(P),
		{Mod,Port}
	end, string:tokens(S, ",")).

%% bar:netudp:localhost:8888,que:netudp:localhost:7777
parse_endpoints("") -> [];
parse_endpoints(S) ->
	lists:map(fun(T) ->
		[N,M,Host,P] = string:tokens(T, ":"),
		Node = list_to_atom(N),
		Mod = list_to_atom(M),
		Port = list_to_integer(P),
		{Node,Mod,{Host,Port}}
	end, string:tokens(S, ",")).

%% bar:inter:10,bar:far:99
parse_routes("") -> [];
parse_routes(S) ->
	lists:map(fun(T) ->
		[N,I,M] = string:tokens(T, ":"),
		Node = list_to_atom(N),
		Inter = list_to_atom(I),
		Metric = list_to_integer(M),
		{Node,Inter,Metric}
	end, string:tokens(S, ",")).

handler(#req{what="/axe",method="POST"}=Req, Conf) ->
	%io:format("Req: ~p~n", [Req]),
	
	%% {record: 'axe', n: 'local', m: 'mod', f: 'fun', as: 'args'}
		
	ContentType = proplists:get_value("content-type", Req#req.headers, default),
	{_,N,M,F,As} = if ContentType =:= "application/xml" ->
		ason:fromAson(Req#req.body);
	true ->
		json:fromJson(Req#req.body, Conf#conf.recinfo)
	end,
	
	if M =:= "undefined"; F =:= "undefined" -> erlang:error(missing_bindings);
		true -> ok end,
		
	Mod = list_to_atom(M),
	Fun = list_to_atom(F),
	Args = As,
	%io:format("Args: ~p~n", [Args]),
	
	Result = if N /= "local" ->
		catch rpc:call(list_to_atom(N), Mod, Fun, Args);
	true ->
		catch apply(Mod, Fun, Args)
	end,
	
	Result2 = case Result of
	{'EXIT',Reason} ->
		{error,Reason};
	Value ->
		{ok,Value}
	end,
	
	Enveloped = if ContentType =:= "application/xml" ->
		ason:toAson(Result2);
	true ->
		json:toJson(Result2, Conf#conf.recinfo)
	end,
	
	%io:format("Enveloped: ~p~n", [Enveloped]),
	spurl:r(Req, ok, [], Enveloped);

handler(#req{what="/apply",method="POST"}=Req, _Conf) ->
	%io:format("Req: ~p~n", [Req]),
	{Node,Mod,Fun,Args} = ason:fromAson(Req#req.body), %% ASON only
	%io:format("fromAson: was ~s, got ~p~n", [Req#req.body, Args]),
	
	Result = if Node /= local ->
		catch rpc:call(Node, Mod, Fun, Args);
	true ->
		catch apply(Mod, Fun, Args)
	end,
	
	Result2 = case Result of
	{'EXIT',MachineReason} ->
		%% make it human-readable too
		HumanReason = flatten(io_lib:format("~p", [MachineReason])),
		{?APPLY_THROWS_EXCEPTION,HumanReason,MachineReason};
	Value ->
		{?APPLY_RETURNS_VALUE,Value}
	end,
	
	Enveloped = ason:toAson(Result2),
	%io:format("Enveloped: ~p~n", [Enveloped]),
	spurl:r(Req, ok, [], Enveloped);

handler(#req{what="/comet_json"}=Req, Conf) ->
    %io:format("Comet ready ~p~n", [Req]),  
    comet ! {ready_for_messages,self()},
    receive
    {push,_,Package} ->
        spurl:r(Req, ok, [], json:toJson(Package, Conf#conf.recinfo))
    after 30000 ->
		spurl:r(Req, ok, [], json:toJson([], Conf#conf.recinfo))
    end;

handler(#req{what="/comet",body=""}=Req, _Conf) ->
    comet ! {ready_for_messages,self()},
    receive
    {push,_,Package} ->
        spurl:r(Req, ok, [], ason:toAson(Package))
    after 30000 ->
		spurl:r(Req, ok, [], ason:toAson([]))
    end;

handler(#req{what="/comet"}=Req, _Conf) ->
    [{client_id,ClientID}] = ason:fromAson(Req#req.body),
    %erlang:display({comet_connect,ClientID}),

    comet ! {ready_for_messages,self(),ClientID},
    receive
    {push,_,Package} ->
        spurl:r(Req, ok, [], ason:toAson(Package));
    {drop,_} ->
		%erlang:display({comet_dropped,ClientID}),
		spurl:r(Req, ok, [], ason:toAson(drop))
    after 30000 ->
		spurl:r(Req, ok, [], ason:toAson([]))
    end;

handler(#req{what="/fs"}=Req, Conf) ->
	%% {d,Name} or {f,Name,Size}
	{ok,Dir} = file:list_dir2("."),
	
	Items = map(fun({dir,"."}) ->
		hide;
	({dir,".."}) ->
		hide;
	({dir,Name}) ->
		{file_store_item,true,Name,".",0,"..",[]};
	({file,Name,Size}) ->
		{file_store_item,false,Name,".",Size,"..",[]}
	end, Dir),
	
	%% get rid of . and ..
	Items2 = filter(fun(hide) -> false; (_) -> true end, Items),
	
	Ds = {datastore,"name","name",Items2},

	spurl:r(Req, ok, [], json:toJson(Ds, Conf#conf.recinfo));

%% {datastore,["id","label","items"]},	
%% {file_store_item,
%%		["directory","name","path","size","parentDir","children"]}

handler(#req{what="/echo"}=Req, _Conf) ->
	io:format("Comet ready ~p~n", [Req]),
	spurl:r(Req, ok, [], []);
	
handler(#req{what=What}=Req, Conf) ->
	
	if Conf#conf.showfiles ->
		io:format("Requested '~s'~n", [What]);
	true -> quiet end,
	
    File = Conf#conf.webroot ++ What,
    case file:read_file(File) of
    {ok,Bin} ->
        spurl:r(Req, ok, [], Bin);
    {error,_} ->
        io:format("'~s' not found~n", [What]),
        spurl:r(Req, 404, [], "<html><body>Not found</body></html>")
    end.

%% EOF
