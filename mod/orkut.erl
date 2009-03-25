-module(orkut).

-export([processes/0,eval/1,eval/2]).
-export([read_text_file/1,write_text_file/2]).
-export([list_modules/0,replace_module/2]).
-export([format_any_error/2]).
-export([module_is_stale/2]).

-import(lists,[map/2,member/2,keysort/2]).

processes() ->
	map(fun(Pid) ->
		{registered_name,RegName} = erlang:process_info(Pid, registered_name),
		{heap_size,HeapSize} = erlang:process_info(Pid, heap_size),
		{module,Module} = erlang:process_info(Pid, module),
		{offset,Offset} = erlang:process_info(Pid, offset),
		{Pid,RegName,HeapSize,Module,Offset}
	end, erlang:processes()).

eval(Expr) -> eval(Expr, erl_eval:new_bindings()).
eval(Expr, Bindings) ->		%% {Value,HumanBinds,NewBinds}
	{ok,Toks,_} = erl_scan:string(Expr),
	{ok,Exprs} = erl_parse:parse_exprs(Toks),

	{value,Value,NewBinds} = erl_eval:exprs(Exprs, Bindings),
	
	HumanBinds = map(fun({N,V}) ->
		Readable = io_lib:format("~32P", [V,11]),
		{N,lists:flatten(Readable)}
	end, NewBinds),
	
	S = io_lib:format("~p", [Value]),
	{lists:flatten(S),HumanBinds,NewBinds}.

read_text_file(Name) ->		%% {ok,Text} | {error,Error}
	case file:read_file(Name) of
	{ok,Bin} ->
		{ok,binary_to_list(Bin)};
	{error,_}=E ->
		E
	end.

write_text_file(Name, Text) ->	%% ok | {error,Error}
	file:write_file(Name, list_to_binary(Text)).

list_modules() ->		%% [{Mod,Size,Type,IsLoaded}]
	Embedded = code:list_embedded(),	%% {Mod,Size,IsPreloaded}
	External = code:list_external(),	%% {Mod,Size}
	AllLoaded = code:all_loaded(),
	
	Master = [{Mod,Size,preloaded}
		|| {Mod,Size,IsLoaded} <- Embedded, IsLoaded =:= true] ++
	[{Mod,Size,embedded}
		|| {Mod,Size,IsLoaded} <- Embedded, IsLoaded =:= false] ++
	[{Mod,Size,external}
		|| {Mod,Size} <- External],
	
	keysort(1, map(fun({Mod,Size,Type}) ->
		{Mod,Size,Type,member(Mod, AllLoaded)}
	end, Master)).

replace_module(Mod, Bin) ->
	case code:is_loaded(Mod) of
	true ->
		case code:delete(Mod) of
		false ->
			case code:soft_purge(Mod) of
			false ->
				false;
			true ->
				code:delete(Mod),
				code:add_module(Mod, Bin),
				true
			end;
		true ->
			code:add_module(Mod, Bin),
			true
		end;
	false ->
		code:add_module(Mod, Bin),
		true
	end.

format_any_error(Module, ErrDesc) ->
	R = Module:format_error(ErrDesc),
	lists:flatten(R).

module_is_stale(Mod, File) ->
	{ok,Finfo} = file:read_file_info(File),
	FileTime = element(5, Finfo),
	case code:mtime(Mod) of
	false ->
		true;		%% not loaded module is stale
	CodeTime ->
		(CodeTime < FileTime)
	end.

%%
