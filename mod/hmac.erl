-module(hmac).

-export([hmac_md5/2,hmac_sha1/2]).
-export([test/0]).

-import(lists,[map/2,duplicate/2]).

%% ipad = the byte 0x36 repeated B times 
%% opad = the byte 0x5C repeated B times.
%%
%% H(K XOR opad, H(K XOR ipad, text))

hmac_md5(Key, Text) when is_binary(Key), is_binary(Text), size(Key) =< 64 ->
	Key1 = xor_pad(Key, 16#36),
	I = crypto:md5(list_to_binary([Key1,Text])),
	Key2 = xor_pad(Key, 16#5c),
	crypto:md5(list_to_binary([Key2,I])).

hmac_sha1(Key, Text) when is_binary(Key), is_binary(Text), size(Key) =< 64 ->
	Key1 = xor_pad(Key, 16#36),
	I = crypto:sha1(list_to_binary([Key1,Text])),
	Key2 = xor_pad(Key, 16#5c),
	crypto:sha1(list_to_binary([Key2,I])).

xor_pad(Key, Byte) ->
	Zeros = duplicate(64-size(Key), 0),
	map(fun(X) ->
		X bxor Byte
	end, binary_to_list(Key)++Zeros).

test() ->
	K1 = <<11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11>>,
	T1 = <<"Hi There">>,
	<<D1:16/unit:8>> = hmac_md5(K1, T1),
	16#9294727a3638bb1c13f48ef8158bfc9d = D1,
	
	K2 = <<"Jefe">>,
	T2 = <<"what do ya want for nothing?">>,
	<<D2:16/unit:8>> = hmac_md5(K2, T2),
	16#750c783e6ab0b503eaa86e310a5db738 = D2,
	
	K3 = list_to_binary(duplicate(16, 16#aa)),
	T3 = list_to_binary(duplicate(50, 16#dd)),
	<<D3:16/unit:8>> = hmac_md5(K3, T3),
	16#56be34521d144c88dbb8c733f0e8b3f6 = D3,
	
	"All tests ok".

%% EOF
