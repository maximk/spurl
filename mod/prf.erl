-module(prf).

-export([generate/4]).
-export([test/0]).

-import(lists,[reverse/1,zipwith/3,duplicate/2]).

p_md5(Secret, Seed, Len) -> p_md5(Secret, Seed, Len, Seed, 0, []).
p_md5(_Secret, _Seed, Len, _A, GenLen, Bs) when GenLen >= Len ->
	<<Result:Len/binary,_/binary>> = list_to_binary(reverse(Bs)),
	Result;
p_md5(Secret, Seed, Len, A, GenLen, Bs) ->
	A1 = hmac:hmac_md5(Secret, A),
	B = hmac:hmac_md5(Secret, list_to_binary([A1,Seed])),
	p_md5(Secret, Seed, Len, A1, GenLen+size(B), [B|Bs]).

p_sha1(Secret, Seed, Len) -> p_sha1(Secret, Seed, Len, Seed, 0, []).
p_sha1(_Secret, _Seed, Len, _A, GenLen, Bs) when GenLen >= Len ->
	<<Result:Len/binary,_/binary>> = list_to_binary(reverse(Bs)),
	Result;
p_sha1(Secret, Seed, Len, A, GenLen, Bs) ->
	A1 = hmac:hmac_sha1(Secret, A),
	B = hmac:hmac_sha1(Secret, list_to_binary([A1,Seed])),
	p_sha1(Secret, Seed, Len, A1, GenLen+size(B), [B|Bs]).

generate(Secret, Label, Seed, Len) ->
	Seed1 = list_to_binary([Label,Seed]),
	
	N = size(Secret),
	N1 = N div 2 + N rem 2,
	N2 = N-N1,
	<<Secret1:N1/binary,_/binary>> = Secret,
	<<_:N2/binary,Secret2/binary>> = Secret,
	
	P1 = p_md5(Secret1, Seed1, Len),
	P2 = p_sha1(Secret2, Seed1, Len),
	
	L = zipwith(fun(X, Y) ->
		X bxor Y
	end, binary_to_list(P1), binary_to_list(P2)),
	
	list_to_binary(L).

%%out[104]       = PRF(secret, label, seed)
%%PRF Testvector = MD5(out[104])
%%               = CD 7C A2 CB 9A 6A 3C 6F 34 5C 46 65 A8 B6 81 6B
%%				 
%%The following parameters are passed to PRF:
%%  - secret: 48 Byte 0xab
%%    Length of pre_master_secret
%%  - label : 14 Byte "PRF Testvector"
%%  - seed  : 64 Byte 0xcd
%%    Length of client_random + server_random
%%
%%
%%Below the whole 104 bytes. These are only attached for verification.
%%They sould not appear in the TLS spec.
%%
%%0x00  D3 D4 D1 E3 49 B5 D5 15 04 46 66 D5 1D E3 2B AB
%%0x10  25 8C B5 21 B6 B0 53 46 3E 35 48 32 FD 97 67 54
%%0x20  44 3B CF 9A 29 65 19 BC 28 9A BC BC 11 87 E4 EB
%%0x30  D3 1E 60 23 53 77 6C 40 8A AF B7 4C BC 85 EF F6
%%0x40  92 55 F9 78 8F AA 18 4C BB 95 7A 98 19 D8 4A 5D
%%0x50  7E B0 06 EB 45 9D 3A E8 DE 98 10 45 4B 8B 2D 8F
%%0x60  1A FB C6 55 A8 C9 A0 13

test() ->
	Secret = list_to_binary(duplicate(48, 16#ab)),
	Label = "PRF Testvector",
	Seed = list_to_binary(duplicate(64, 16#cd)),
	PRF = generate(Secret, Label, Seed, 104),
	
	<<16#CD,16#7C,16#A2,16#CB,16#9A,16#6A,16#3C,16#6F,
	16#34,16#5C,16#46,16#65,16#A8,16#B6,16#81,16#6B>> = crypto:md5(PRF),
	
	test_ok.
	
%% EOF
