-module(rsa).

-export([decrypt/2,encrypt/2]).

-import(lists,[reverse/1,map/2]).

-define(dbg(X, Y), io:format(X, Y)).

%% CipherText = binary()
%% PriveKey = binary(), BER-encoded RSA private key
decrypt(CipherText, PrivKey) ->	    %% PlainText
	
	%RSAPrivateKey ::= SEQUENCE {
    %version INTEGER,
    %modulus INTEGER, -- n
    %publicExponent INTEGER, -- e
    %privateExponent INTEGER, -- d
    %prime1 INTEGER, -- p
    %prime2 INTEGER, -- q
    %exponent1 INTEGER, -- d mod (p-1)
    %exponent2 INTEGER, -- d mod (q-1)
    %coefficient INTEGER -- (inverse of q) mod p }
    %}

	{seq,[{integer,0},
		   {integer,Modulus},		     		  	   %% n
		   {integer,_PublicExponent},  %% e
		   {integer,PrivateExponent},  %% d
		   {integer,_Prime1},		          %% p
		   {integer,_Prime2},		          %% q
		   {integer,_Exponent1},		       %% d mod (p-1) 
		   {integer,_Exponent2},		       %% d mod (q-1)
		   {integer,_Coefficient}]} = ber:decode(PrivKey),
	
	N2 = size(CipherText),
	<<Y:N2/unit:8>> = CipherText,
	
	true = Y < Modulus,
	
	X = ipow(Y, PrivateExponent, Modulus),
	unwrap_block(<<X:128/unit:8>>).

%%
%% Certificate is BER-encoded X.509 certificate
encrypt(PlainText, Certificate) -> %% CipherText

	{seq,[{seq,[_Version,
				 _SerialNum,
	     		 _Signature,
	      		 _Issuer,
	      		 _Validity,
	      		 _Subject,
	      		 PubKeyInfo|_]}|_]} = ber:decode(Certificate),
	{seq,[_Algo,
	      {bitstr,PubKey}]} = PubKeyInfo,
	{seq,[{integer,Modulus},
	      {integer,PublicExponent}]} = ber:decode(PubKey),
	
	WrappedBlock = wrap_block(PlainText),
	
	N2 = size(WrappedBlock),
	<<X:N2/unit:8>> = WrappedBlock,
	
	true = X < Modulus,
	
	Y = ipow(X, PublicExponent, Modulus),
	<<Y:128/unit:8>>.

unwrap_block(<<0,2,PaddingData/binary>>) -> unwrap_block0(PaddingData).
unwrap_block0(<<0,Data/binary>>) ->
	Data;
unwrap_block0(<<_,PaddingData/binary>>) ->
	unwrap_block0(PaddingData).

wrap_block(T) ->
	PLen = 128-size(T)-3,
	P = map(fun(0) -> 255; (N) -> N end, crypto:random_bytes(PLen)),
	Padding = list_to_binary(P),
	<<0,2,Padding/binary,0,T/binary>>.

ipow(A, 1, M) ->
    A rem M;
ipow(A, 2, M) ->
    A*A rem M;
ipow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = ipow(A, B1, M),
    case B2 of
        B1 -> (P*P) rem M;
        _  -> (P*P*A) rem M
    end.

%% EOF
