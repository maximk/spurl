-module(tls).

%%
%% The simplest implementation of TLS protocol 1.0
%%

-export([client/1,server/2,server/5]).

-export([recv_app_data/1,send_app_data/2]).
%% -export([recv/1,send/2]).

-import(lists,[reverse/1,seq/2,mapfoldl/3,map/2,member/2]).

-define(dbg(X, Y), io:format(X, Y)).

-define(SHA1_DIGEST_SIZE, 20).

-include("tls.hrl").

-define(CONTENT_TYPE_CHANGE_CIPHER_SPEC, 20).
-define(CONTENT_TYPE_ALERT, 21).
-define(CONTENT_TYPE_HANDSHAKE, 22).
-define(CONTENT_TYPE_APPLICATION_DATA, 23).

-define(HANDSHAKE_HELLO_REQUEST, 0).
-define(HANDSHAKE_CLIENT_HELLO, 1).
-define(HANDSHAKE_SERVER_HELLO, 2).
-define(HANDSHAKE_CERTIFICATE, 11).
-define(HANDSHAKE_SERVER_KEY_EXCHANGE, 12).
-define(HANDSHAKE_CERTIFICATE_REQUEST, 13).
-define(HANDSHAKE_SERVER_HELLO_DONE, 14).
-define(HANDSHAKE_CERTIFICATE_VERIFY, 15).
-define(HANDSHAKE_CLIENT_KEY_EXCHANGE, 16).
-define(HANDSHAKE_FINISHED, 20).

-define(TLS_RSA_WITH_RC4_128_SHA, {0,5}).

%%
%% Establish a client TLS connection
%%
client(Sock) ->	%% {ok,Tls} | {error,Error}
	initial(#tls{side=client,sock=Sock}).											

%%
%% Establish a server TLS connection
%%
server(Sock, FiveBytes) when is_port(Sock),
		is_binary(FiveBytes), size(FiveBytes) == 5 ->	 	%% {ok,Tls} | {error,Error}
	initial(#tls{side=server,
				  sock=Sock,
				  five_bytes=FiveBytes,
				  certificate=signed_cert(),
				  private_key=priv_key(),
				  ca_cert=ca_cert()}).

server(Sock, FiveBytes, MyCert, CaCert, PrivKey) when is_port(Sock),
		is_binary(FiveBytes), size(FiveBytes) == 5 ->	 	%% {ok,Tls} | {error,Error}
	initial(#tls{side=server,
				  sock=Sock,
				  five_bytes=FiveBytes,
				  certificate=MyCert,
				  private_key=PrivKey,
				  ca_cert=CaCert}).

handshake(St) -> handshake(0, [], St).
handshake(Frag, St) -> handshake(size(Frag), [Frag], St).

handshake(NRecv, Fs, St) when NRecv < 4 ->
	{?CONTENT_TYPE_HANDSHAKE,Frag,St1} = record(St),	
	handshake(NRecv+size(Frag), [Frag|Fs], St1);
handshake(NRecv, Fs, St) ->
	Head = list_to_binary(reverse(Fs)),
	<<_MsgType,Len:24,_/binary>> = Head,
	handshake1(NRecv, [Head], Len+4, St).

handshake1(NRecv, Fs, NNeed, St) when NRecv < NNeed ->
	{?CONTENT_TYPE_HANDSHAKE,Frag,St1} = record(St),	
	handshake1(NRecv+size(Frag), [Frag|Fs], NNeed, St1);
handshake1(_NRecv, Fs, NNeed, St) ->
	<<Handshake:NNeed/binary,Pre/binary>> = list_to_binary(reverse(Fs)),
	%?dbg("handshake2: ~w,~w~n", [Handshake,Pre]),
	{Handshake,Pre,St}.

%% struct {
%%    ProtocolVersion client_version;
%%    Random random;
%%    SessionID session_id;
%%    CipherSuite cipher_suites<2..2^16-1>;
%%    CompressionMethod compression_methods<1..2^8-1>;
%% } ClientHello;

%% send ClientHello
initial(#tls{side=client}=St) ->

	ClientRandom = list_to_binary(crypto:random_bytes(32)),
	{MySuiteA,MySuiteB} = ?TLS_RSA_WITH_RC4_128_SHA,
	
	ClientHelloBody = <<3,1,
		ClientRandom/binary,
		0, %% no sessionid
		0,2,MySuiteA,MySuiteB, %% rsa, rc4 128, sha1 only
		1,0>>, %% null compression only
		
	ClientHello = <<?HANDSHAKE_CLIENT_HELLO,
		(size(ClientHelloBody)):24,
		ClientHelloBody/binary>>,

	St1 = record_send(?CONTENT_TYPE_HANDSHAKE, ClientHello, St),
	%?dbg("ClientHello sent: ~p~n", [ClientHello]),

	Hs = [ClientHello],
	hello(St1#tls{handshakes=Hs,client_random=ClientRandom});

%% ClientHello expected
initial(#tls{side=server}=St) ->
	{?CONTENT_TYPE_HANDSHAKE,Frag,St1} = record_5bytes(St),
	
	{ClientHello,<<>>,St2} = handshake(Frag, St1),
	%?dbg("client_hello: ~p~n", [ClientHello]),
	
	<<?HANDSHAKE_CLIENT_HELLO,
	  _Len:24,
	  _VerMaj,_VerMin,
	  ClientRandom:32/binary,
	  Nsi,R1/binary>> = ClientHello,
	<<_SessionId:Nsi/binary,
	  Ncs:16,R2/binary>> = R1,
	<<CSs:Ncs/binary,
	  Ncm,R3/binary>> = R2,
	<<CMs:Ncm/binary,
	  _Ignore/binary>> = R3,
	  
	%CipherSuites = CSs,
	{CipherSuites,_} = mapfoldl(fun(_, <<A,B,Other/binary>>) ->
		{{A,B},Other}
	end, CSs, seq(1, Ncs div 2)),
		
	_CompressionMethods = binary_to_list(CMs),
		
	true = member(?TLS_RSA_WITH_RC4_128_SHA, CipherSuites),

	Hs = [ClientHello|St#tls.handshakes],
	hello(St2#tls{handshakes=Hs,client_random=ClientRandom}).

%% struct {
%%    ProtocolVersion server_version;
%%    Random random;
%%    SessionID session_id;
%%    CipherSuite cipher_suite;
%%    CompressionMethod compression_method;
%% } ServerHello;

%% ClientHello sent -- receive ServerHello, etc
hello(#tls{side=client}=St) ->

	{ServerHello,<<>>,St1} = handshake(St),
	%?dbg("server_hello: ~p~n", [ServerHello]),
	
	<<?HANDSHAKE_SERVER_HELLO,
	  _:24,
	  _VerMaj,_VerMin,
	  ServerRandom:32/binary,
	  SidLen,SidOthers/binary>> = ServerHello,
	<<_SessionId:SidLen/binary, %% ignore
	  _, _, %% if anything it must be our ciphersuite
	  0>> = SidOthers, %% null compression
	%?dbg("ServerHello received: ~w~n", [ServerHello]),
	
	{Certificate,<<>>,St2} = handshake(St1),
	<<?HANDSHAKE_CERTIFICATE,
	  _:24, %% msg len
	  ChainLen:24,CertChain/binary>> = Certificate,
	ChainLen = size(CertChain),

	{ServerHelloDone,<<>>,St3} = handshake(St2),
	<<?HANDSHAKE_SERVER_HELLO_DONE,
	  0:24>> = ServerHelloDone,
	
	Certs = cert_chain(CertChain),
	[Cert|_] = Certs,
	%?dbg("Server provided ~w certificate(s)~n", [length(Certs)]),
	
	%% ?dbg("First certificate: ~w~n", [Cert]),
	
	%% signing certificate may be saved too

	Hs = [ServerHelloDone,Certificate,ServerHello|St3#tls.handshakes],
	client_keys(St3#tls{handshakes=Hs,
						  certificate=Cert,
						  server_random=ServerRandom});

%% ClientHello received -- sending ServerHello
hello(#tls{side=server}=St) ->

	ServerRandom = list_to_binary(crypto:random_bytes(32)),
	{MySuiteA,MySuiteB} = ?TLS_RSA_WITH_RC4_128_SHA,

	ServerHelloBody = <<3,1,
		ServerRandom/binary,
		0,
		MySuiteA,MySuiteB,
		0>>,
	ServerHello = <<?HANDSHAKE_SERVER_HELLO,
		(size(ServerHelloBody)):24,
		ServerHelloBody/binary>>,

	St1 = record_send(?CONTENT_TYPE_HANDSHAKE, ServerHello, St),
	%?dbg("ServerHello sent: ~p~n", [ServerHello]),
	
	SignedCert = St1#tls.certificate,
	Cert1 = <<(size(SignedCert)):24,SignedCert/binary>>,
	
	CaCert = St1#tls.ca_cert,
	Cert2 = <<(size(CaCert)):24,CaCert/binary>>,
	
	CertificateBody = <<(size(Cert1)+size(Cert2)):24,
		Cert1/binary,Cert2/binary>>,
	
	Certificate = <<?HANDSHAKE_CERTIFICATE,
		(size(CertificateBody)):24,
		CertificateBody/binary>>,
	St2 = record_send(?CONTENT_TYPE_HANDSHAKE, Certificate, St1),
	%% ?dbg("Certificate sent~n", []),
	
	ServerHelloDone = <<?HANDSHAKE_SERVER_HELLO_DONE,	0:24>>,
	St3 = record_send(?CONTENT_TYPE_HANDSHAKE, ServerHelloDone, St2),
	%% ?dbg("ServerHelloDone sent~n", []),
	
	Hs = [ServerHelloDone,Certificate,ServerHello|St#tls.handshakes],
	client_keys(St3#tls{handshakes=Hs,server_random=ServerRandom}).

cert_chain(Chain) -> cert_chain(Chain, []).
cert_chain(<<N:24,CertCerts/binary>>, Cs) ->
	<<Cert:N/binary,Certs/binary>> = CertCerts,
	cert_chain(Certs, [Cert|Cs]);
cert_chain(<<>>, Cs) ->
	reverse(Cs).

%%struct {
%%           public-key-encrypted PreMasterSecret pre_master_secret;
%%       } EncryptedPreMasterSecret;

client_keys(#tls{side=client}=St) ->

	%% premaster secret must start with protocol version bytes
	PreMasterSecret = list_to_binary([3,1|crypto:random_bytes(46)]),
	EncPreMasterSecret = rsa:encrypt(PreMasterSecret, St#tls.certificate),
	%?dbg("EncPreMasterSecret: ~w~n", [EncPreMasterSecret]),
	
	ClientKeyExchangeBody =
		<<(size(EncPreMasterSecret)):16,EncPreMasterSecret/binary>>,
	 
	ClientKeyExchange = <<?HANDSHAKE_CLIENT_KEY_EXCHANGE,
	  (size(ClientKeyExchangeBody)):24,
	  ClientKeyExchangeBody/binary>>,
	
	St1 = record_send(?CONTENT_TYPE_HANDSHAKE, ClientKeyExchange, St),
	%?dbg("ClientKeyExchange sent~n", []),

	%% master_secret = PRF(pre_master_secret, "master secret",
	%%                         ClientHello.random + ServerHello.random)[0..47];
	MasterSecret = prf:generate(PreMasterSecret, "master secret",
		list_to_binary([St#tls.client_random,St#tls.server_random]), 48),

	%%key_block = PRF(SecurityParameters.master_secret,
	%%                   "key expansion",
	%%                   SecurityParameters.server_random +
	%%                   SecurityParameters.client_random);
	%%client_write_MAC_secret[SecurityParameters.hash_size]
	%%server_write_MAC_secret[SecurityParameters.hash_size]
	%%client_write_key[SecurityParameters.key_material_length]
	%%server_write_key[SecurityParameters.key_material_length]
	%%client_write_IV[SecurityParameters.IV_size]
	%%server_write_IV[SecurityParameters.IV_size]

	%%       
	%% sha1 hash_size is 20 x 2 = 40
	%% rc4 128 key_length is 16 x 2 = 32
	%% total key_material_length is 72
	%%
      
	KeyMaterial = prf:generate(MasterSecret, "key expansion",
		list_to_binary([St#tls.server_random,St#tls.client_random]), 72),
		
	<<ClientMacSecret:20/binary,ServerMacSecret:20/binary,
		  ClientKey:16/binary,ServerKey:16/binary>> = KeyMaterial,
	
	St2 = record_send(?CONTENT_TYPE_CHANGE_CIPHER_SPEC, <<1>>, St1),
	%?dbg("Switching to encrypted mode...~n", []),

	%% change_cipher_spec is not a handshake message
	Hs = [ClientKeyExchange|St2#tls.handshakes],
		
	finished(St2#tls{handshakes=Hs,
		encrypted=true,
		master_secret=MasterSecret,
		client_mac=ClientMacSecret,
		server_mac=ServerMacSecret,
		client_state=crypto:rc4_init(ClientKey),
		server_state=crypto:rc4_init(ServerKey)});

client_keys(#tls{side=server}=St) ->

	{ClientKeyExchange,<<>>,St1} = handshake(St),
	%% ?dbg("client_keys: ~w,~p~n", [size(ClientKeyExchange),ClientKeyExchange]),
		
	<<?HANDSHAKE_CLIENT_KEY_EXCHANGE,
	  _Len:24,
	  N:16,EncPreMasterSecret/binary>> = ClientKeyExchange,
	N = size(EncPreMasterSecret),

	PreMasterSecret = rsa:decrypt(EncPreMasterSecret, St1#tls.private_key),
	%% ?dbg("premaster secret: ~p~n", [PreMasterSecret]),
				
	%% master_secret = PRF(pre_master_secret, "master secret",
	%%                         ClientHello.random + ServerHello.random)[0..47];
	MasterSecret = prf:generate(PreMasterSecret, "master secret",
		list_to_binary([St#tls.client_random,St#tls.server_random]), 48),
      
	KeyMaterial = prf:generate(MasterSecret, "key expansion",
		list_to_binary([St#tls.server_random,St#tls.client_random]), 72),
		
	<<ClientMacSecret:20/binary,ServerMacSecret:20/binary,
		  ClientKey:16/binary,ServerKey:16/binary>> = KeyMaterial,
		
	{?CONTENT_TYPE_CHANGE_CIPHER_SPEC,<<1>>,St2} = record(St1),
	%?dbg("Switching to encrypted mode...~n", []),
		
	%% this should be sent after Finished message received
	%% but it is easier to send it here befor encryption kicks in
	St3 = record_send(?CONTENT_TYPE_CHANGE_CIPHER_SPEC, <<1>>, St2),
	
	%% change_cipher_spec is not a handshake message
	Hs = [ClientKeyExchange|St#tls.handshakes],
		
	finished(St3#tls{handshakes=Hs,
		encrypted=true,
		master_secret=MasterSecret,
		client_mac=ClientMacSecret,
		server_mac=ServerMacSecret,
		client_state=crypto:rc4_init(ClientKey),
		server_state=crypto:rc4_init(ServerKey)}).

finished(#tls{side=client}=St) ->
	Handshakes1 = list_to_binary(reverse(St#tls.handshakes)),
	HH1 = list_to_binary([crypto:md5(Handshakes1),crypto:sha1(Handshakes1)]),
	
	%%	PRF(master_secret, finished_label, MD5(handshake_messages) +
	%%        SHA-1(handshake_messages)) [0..11];
	
	ClientVerifyData = prf:generate(St#tls.master_secret,
		"client finished", HH1, 12),
	
	ClientFinished =
	<<?HANDSHAKE_FINISHED,
	  (size(ClientVerifyData)):24,
	  ClientVerifyData/binary>>,
	St1 = record_send(?CONTENT_TYPE_HANDSHAKE, ClientFinished, St),
		
	%% XXX: encryption turned off for the message, new state dropped
	{?CONTENT_TYPE_CHANGE_CIPHER_SPEC,<<1>>,_} = record(St1#tls{encrypted=false}),
	%?dbg("server switches to encrypted mode...~n", []),

	%% include ClientFinished message to the list 
	%% of handshake messages hashed
		
	Handshakes2 = list_to_binary([Handshakes1,ClientFinished]),
	HH2 = list_to_binary([crypto:md5(Handshakes2),crypto:sha1(Handshakes2)]),
		
	ServerVerifyData = prf:generate(St#tls.master_secret,
		"server finished", HH2, 12),

	{ServerFinished,<<>>,St2} = handshake(St1),	
	<<?HANDSHAKE_FINISHED,
	  _Len:24,
	  ServerVerifyData/binary>> = ServerFinished,  %% assertion
	%?dbg("server Finished ok: ~p~n", [ServerFinished]),

	{ok,St2};

finished(#tls{side=server}=St) ->
	Handshakes1 = list_to_binary(reverse(St#tls.handshakes)),
	HH1 = list_to_binary([crypto:md5(Handshakes1),crypto:sha1(Handshakes1)]),
	
	%%	PRF(master_secret, finished_label, MD5(handshake_messages) +
	%%        SHA-1(handshake_messages)) [0..11];
	
	ClientVerifyData = prf:generate(St#tls.master_secret,
		"client finished", HH1, 12),

	{ClientFinished,<<>>,St1} = handshake(St),
		
	<<?HANDSHAKE_FINISHED,
	  _Len:24,
	  ClientVerifyData/binary>> = ClientFinished,		  %% assertion
	  
	%?dbg("client Finished ok: ~p~n", [ClientFinished]),
	%?dbg("ClientVerifyData=~p~n", [ClientVerifyData]),
		
	%% change cipher spec sent earlier

	%% include ClientFinished message to the list 
	%% of handshake messages hashed
		
	Handshakes2 = list_to_binary([Handshakes1,ClientFinished]),
	HH2 = list_to_binary([crypto:md5(Handshakes2),crypto:sha1(Handshakes2)]),
		
	ServerVerifyData = prf:generate(St#tls.master_secret,
		"server finished", HH2, 12),
		
	ServerFinished = <<?HANDSHAKE_FINISHED,	
		(size(ServerVerifyData)):24,
		ServerVerifyData/binary>>,

	St2 = record_send(?CONTENT_TYPE_HANDSHAKE, ServerFinished, St1),
	%?dbg("server Finished sent~n", []),
	
	{ok,St2}.

%%
%% Read a single record
%%

%% struct {
%%    uint8 major, minor;
%% } ProtocolVersion;

%% enum {
%%    change_cipher_spec(20), alert(21), handshake(22),
%%    application_data(23), (255)
%% } ContentType;

%% struct {
%%    ContentType type;
%%    ProtocolVersion version;
%%    uint16 length;
%%    opaque fragment[TLSPlaintext.length];
%% } TLSPlaintext;

record(St) ->		%% {ContentType,Fragment,St1)

	{ok,<<ContentType,
	      3,1,
	      Length:16>>} = gen_tcp:recv(St#tls.sock, 1+2+2),
	
	record0(ContentType, Length, St).

record_5bytes(St) ->
	<<ContentType,
	  3,1,
	  Length:16>> = St#tls.five_bytes,
	record0(ContentType, Length, St).

record0(ContentType, Length, St) ->

	{ok,Data} = gen_tcp:recv(St#tls.sock, Length),
	
	%?dbg("record recv: ~p~n", [Data]),
	
	case St#tls.encrypted of
	true ->
		case St#tls.side of
		server ->
			Opaque = St#tls.client_state,
			{FragMac,Opaque1} = crypto:rc4_update(Data, Opaque),
			
			Fragment = check_mac(FragMac,
				[ContentType,3,1],
				St),
	
			SeqNum1 = St#tls.client_seq_num+1,
			{ContentType,Fragment,St#tls{client_seq_num=SeqNum1,client_state=Opaque1}};
		client ->
			Opaque = St#tls.server_state,
			{FragMac,Opaque1} = crypto:rc4_update(Data, Opaque),
			
			Fragment = check_mac(FragMac,
				[ContentType,3,1],
				St),
	
			SeqNum1 = St#tls.server_seq_num+1,
			{ContentType,Fragment,St#tls{server_seq_num=SeqNum1,server_state=Opaque1}}		
		end;
	false ->
		{ContentType,Data,St}
	end.

%% HMAC_hash(MAC_write_secret, seq_num + TLSCompressed.type +
%%	  TLSCompressed.version + TLSCompressed.length +
%%	  TLSCompressed.fragment));

check_mac(FragMac, TypeVersion, St) ->
	FragSize = size(FragMac)-?SHA1_DIGEST_SIZE,
	<<Fragment:FragSize/binary,Mac/binary>> = FragMac,
		
	%% TLSCompressed.length == FragSize
		
	ExpectedMac = case St#tls.side of
	server ->
		%erlang:display(St#tls.client_seq_num),
		B = list_to_binary([<<(St#tls.client_seq_num):64>>,
				TypeVersion,
				<<FragSize:16>>,
				Fragment]),
		hmac:hmac_sha1(St#tls.client_mac, B);
	client ->
		B = list_to_binary([<<(St#tls.server_seq_num):64>>,
				TypeVersion,
				<<FragSize:16>>,
				Fragment]),
		hmac:hmac_sha1(St#tls.server_mac, B)
	end,
	
	if ExpectedMac =/= Mac ->
		?dbg("bad mac: side: ~w~n"
			 "server_seq_num: ~w~n"
			 "client_seq_num: ~w~n",
			 [St#tls.side,
			  St#tls.server_seq_num,
			  St#tls.client_seq_num]);
	true ->
		ok
	end,
	 
	ExpectedMac = Mac,
	%?dbg("msg mac ok: ~w vs ~w~n", [Mac,ExpectedMac]),

	Fragment.

record_send(ContentType, <<Chunk:8192/binary,Rest/binary>>, St) ->
	St1 = record_send_chunk(ContentType, Chunk, St),
	record_send(ContentType, Rest, St1);
record_send(ContentType, Payload, St) ->
	record_send_chunk(ContentType, Payload, St).

record_send_chunk(_ContentType, <<>>, St) ->
	St;
record_send_chunk(ContentType, Payload, #tls{encrypted=false}=St) ->
	N = size(Payload),
	gen_tcp:send(St#tls.sock, <<ContentType,3,1,N:16,Payload/binary>>),
	St;
record_send_chunk(ContentType, Payload, St) ->
	N = size(Payload),
	
	case St#tls.side of
	server ->
		Data = <<(St#tls.server_seq_num):64,
			ContentType,3,1,N:16,Payload/binary>>,
		
		Mac = hmac:hmac_sha1(St#tls.server_mac, Data),
		PayloadMac = list_to_binary([Payload,Mac]),
		N1 = size(PayloadMac),
		
		{EncPayloadMac,Opaque1} = crypto:rc4_update(PayloadMac, St#tls.server_state),
		gen_tcp:send(St#tls.sock, <<ContentType,3,1,N1:16,EncPayloadMac/binary>>),
		
		SeqNum1 = St#tls.server_seq_num+1,
		St#tls{server_seq_num=SeqNum1,server_state=Opaque1};
		
	client ->
		Data = <<(St#tls.client_seq_num):64,
			ContentType,3,1,N:16,Payload/binary>>,
		
		Mac = hmac:hmac_sha1(St#tls.client_mac, Data),
		PayloadMac = list_to_binary([Payload,Mac]),
		N1 = size(PayloadMac),
		
		{EncPayloadMac,Opaque1} = crypto:rc4_update(PayloadMac, St#tls.client_state),
		gen_tcp:send(St#tls.sock, <<ContentType,3,1,N1:16,EncPayloadMac/binary>>),
		
		SeqNum1 = St#tls.client_seq_num+1,
		St#tls{client_seq_num=SeqNum1,client_state=Opaque1}
	end.

priv_key() ->
	base64:decode("MIICXAIBAAKBgQC7c5urRKo2tSeGSEHSsSvNYEQNPq1erBT7jCKuOri4TD3jjo3i"
		"0ymTmJrvdNgIYa1uR1QJs4K5Mkk0SRYaO34DP0R7TS4dv8RW/OZ+hlgxbx4CnWSZ"
		"KE9iGCY+Q9OjlWATzFDbsLZia8hjx6yfEuemkRpkJJDbSUD7PuOI4KNeDwIDAQAB"
		"AoGADMyxFXnD0aZ/A2lNrVBRuF1ZaDoeZneDm0WPXKw/zr3OAArM328hyKVD9xPY"
		"aIIa8IvITj1GsNShMQPIkZP8X6hrhLDUpkA1smENjILbXmmU1duEMOmgrx/oQWbU"
		"BT98R76PjDRuYNJw1x+pvMphZeYlUg13Sfal4Rqv0eUIlRkCQQDoTkFRl1Zt2SDG"
		"Qjxe8GmIVXt3RgcCkbhdq26e/QzlKskrkpSCIdxtoXD5az4cR8uP15/lM4DZ3GEJ"
		"QKQeLjKlAkEAzpIqZw+YMJH+KkoC3C95XAr6HT/l2ehpG7WaF/ifTC497+aQovZj"
		"vH+Hg6pxCB5wHJfSVSQQlbr/qW1QmkVzowJAQkRof6TjI8zHe6bTX+XGxPxVjw2r"
		"3EDDTf0kPjhDYHAXGNXkhUaxgZiHCK9d9WVJnUvOMezCOeOSzrfg2TcmfQJAX1tP"
		"4aVsPreWGZVyvovXvOqATPJLO40ywAzT5k7p0jPieZT4nZGZttE0EE6kwPe0Dy7y"
		"dEUitsvM6zxAIgb/ewJBAJp5XIj1SqXplRsHvUIKg/I2WSBe4ZpP2e2iBSGTI/HF"
		"jm4Ojt7as/WHorIe3MRDEoKLMFW3S3UtY0C6K23tmIQ=").
		
signed_cert() ->
	base64:decode("MIICiDCCAfGgAwIBAgICAR4wDQYJKoZIhvcNAQEFBQAwRTELMAkGA1UEBhMCQVUx"
		"EzARBgNVBAgTClNvbWUtU3RhdGUxITAfBgNVBAoTGEludGVybmV0IFdpZGdpdHMg"
		"UHR5IEx0ZDAeFw0wOTAxMjkxODQyMjlaFw0xMDAxMjkxODQyMjlaMFExCzAJBgNV"
		"BAYTAkFVMRMwEQYDVQQIEwpTb21lLVN0YXRlMSEwHwYDVQQKExhJbnRlcm5ldCBX"
		"aWRnaXRzIFB0eSBMdGQxCjAIBgNVBAMUAV8wgZ8wDQYJKoZIhvcNAQEBBQADgY0A"
		"MIGJAoGBALtzm6tEqja1J4ZIQdKxK81gRA0+rV6sFPuMIq46uLhMPeOOjeLTKZOY"
		"mu902AhhrW5HVAmzgrkySTRJFho7fgM/RHtNLh2/xFb85n6GWDFvHgKdZJkoT2IY"
		"Jj5D06OVYBPMUNuwtmJryGPHrJ8S56aRGmQkkNtJQPs+44jgo14PAgMBAAGjezB5"
		"MAkGA1UdEwQCMAAwLAYJYIZIAYb4QgENBB8WHU9wZW5TU0wgR2VuZXJhdGVkIENl"
		"cnRpZmljYXRlMB0GA1UdDgQWBBQhMz3Ujh07y3EgsjNhG8mjtASboTAfBgNVHSME"
		"GDAWgBQcHM2i2iSWlbwf8c7iA1W92vO+KjANBgkqhkiG9w0BAQUFAAOBgQB3YHqx"
		"nWFd1meIhAC9tPOU/ct38bLyjFO+0kbfCKBFIRs6OAHXIvCZUknMKEaLxjufRCRS"
		"70XRtKFLXERKpteA0ARNH6+UNP9Cy7Rs9oMtD3eLBiMtkm+A74dXopY3a7K0iEvj"
		"BL5hzn7g6oL21EzjnvGMddSqnWXnzHG1Kd+DLw==").

ca_cert() ->
	base64:decode("MIICsDCCAhmgAwIBAgIJANm++2HZFYR4MA0GCSqGSIb3DQEBBQUAMEUxCzAJBgNV"
		"BAYTAkFVMRMwEQYDVQQIEwpTb21lLVN0YXRlMSEwHwYDVQQKExhJbnRlcm5ldCBX"
		"aWRnaXRzIFB0eSBMdGQwHhcNMDkwMTI5MTgxNDQ0WhcNMDkwMjI4MTgxNDQ0WjBF"
		"MQswCQYDVQQGEwJBVTETMBEGA1UECBMKU29tZS1TdGF0ZTEhMB8GA1UEChMYSW50"
		"ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKB"
		"gQC2+j3YMlUTpKYcNA+efWCBH/9NwvQGgkVBNwMx3qDdjbULdfAxqULDY62Fyd36"
		"1XSPneWixMuU04Nzqk3mOa9cYhFF7lj7HDcuPztBouNq2/QgTprBhqQjjB2rE0Nc"
		"yGx6LeYWydN3qbB5HDsg3bV3AkN1LY624jTag5/UMuAl+QIDAQABo4GnMIGkMB0G"
		"A1UdDgQWBBQcHM2i2iSWlbwf8c7iA1W92vO+KjB1BgNVHSMEbjBsgBQcHM2i2iSW"
		"lbwf8c7iA1W92vO+KqFJpEcwRTELMAkGA1UEBhMCQVUxEzARBgNVBAgTClNvbWUt"
		"U3RhdGUxITAfBgNVBAoTGEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZIIJANm++2HZ"
		"FYR4MAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcNAQEFBQADgYEAlNMiXk7erw7K9TBA"
		"NIM5bjjVjdR05gteu7tE2q3nPMDSFhfqzf43iApSSJjdedMF8aWIow4pKYM80uLS"
		"Udn/BitkVqF34sCgL/56ZIBFxfFOuFXtIRaPnR1ld/x2ASiBswS4qvA9YwKM8dff"
		"PtF4VmAc19fvbNw25N61jtB4Q00=").

recv_app_data(Tls) ->
	case record(Tls) of
	{?CONTENT_TYPE_APPLICATION_DATA,Bin,Tls1} ->
		{ok,Bin,Tls1};
	{ContentType,Data,Tls1} ->
		?dbg("tls rec: ~w, ~w~n", [ContentType,Data]),
		recv_app_data(Tls1)
	end.

send_app_data(Tls, Bin) ->
	record_send(?CONTENT_TYPE_APPLICATION_DATA, Bin, Tls).

%% EOF
