-module(rc4).

-export([init/1,next/1,test/2]).

-import(lists,[nth/2,seq/2,foldl/3,mapfoldl/3,mapfoldr/3]).

init(Key) when is_list(Key) -> 	%% Opaque
	KeyLen = length(Key),
	
	S0 = list_to_tuple(seq(0, 255)),
	
	{_,S} = foldl(fun(I, {J,S}) ->
		K = nth(I rem KeyLen + 1, Key),
		J1 = (J+K+element(I+1,S)) band 255,
		S1 = swap(I, J1, S),
		{J1,S1}
	end, {0,S0}, seq(0, 255)),
	
	{0,0,S}.

next({I,J,S}) ->		%% {X,Opaque}
	I1 = (I+1) band 255,
	J1 = (J+element(I1+1, S)) band 255,
	
	S1 = swap(I1, J1, S),
	
	T = (element(I1+1, S1)+element(J1+1,S1)) band 255,
	X = element(T+1, S1),
	
	{X,{I1,J1,S1}}.

test(Key, Text) ->
	{Text1,_} = mapfoldl(fun(T, Opaque) ->
		{X,Opaque1} = next(Opaque),
		{T bxor X,Opaque1}
	end, init(Key), Text),
	Text1.

swap(I, J, S) ->
	A = element(I+1, S),
	B = element(J+1, S),
	
	setelement(J+1, setelement(I+1, S, B), A).
  
%% EOF
