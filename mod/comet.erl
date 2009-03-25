-module(comet).

-export([start/0]).

-record(comet,{messages=[],links=[]}).

-import(lists,[member/2,keytake/3,foreach/2]).

start() ->
	Comet = spawn(fun() ->
		comet()
	end),
	register(comet, Comet).

comet() ->
	%comet0(#comet{}).
	
  case catch (comet0(#comet{})) of
  {'EXIT',Why} ->
	io:format("comet fails: ~p, restarting...~n", [Why]),
	comet();
  _ ->
	ok
  end.

comet0(St0) ->
	if St0#comet.messages /= [], St0#comet.links /= [] ->

		Messages = lists:reverse(St0#comet.messages),
		foreach(fun({_ClientID,Link}) ->
			try 
				Link ! {push,self(),Messages}
			catch
			_:_ ->
				ok
			end;		
		(Link) ->	%% old-style
			try 
				Link ! {push,self(),Messages}
			catch
			_:_ ->
				ok		%% Link may not be there already
			end
		end, St0#comet.links),
	    
		comet0(St0#comet{messages=[],links=[]});

	true ->
		receive
		{ready_for_messages,From,ClientID} ->
			%erlang:display({comet_connected,From}),
			
			Links = case keytake(ClientID, 1, St0#comet.links) of
			{value,{_,OldFrom},Links0} ->
				%% most probably OldFrom is not there already
				try	OldFrom ! {drop,self()}
				catch _:_ -> ignore
				end,
				Links0;
			false ->
				St0#comet.links
			end,
			comet0(St0#comet{links=[{ClientID,From}|Links]});

		{ready_for_messages,From} ->
			%erlang:display({comet_connected,From}),

			St1 = case member(From, St0#comet.links) of
			false ->
			    St0#comet{links=[From|St0#comet.links]};
			true ->
				St0
			end,
			comet0(St1);

		{message,_From,Body} ->
			%erlang:display({msg,Body}),
			St1 = St0#comet{messages=[Body|St0#comet.messages]},
			comet0(St1)
		end
	end.

%%
