%%
%%
%%

-record(tls,{sock,
	side,					%% client or server
	five_bytes,				%% first 5 bytes read to distinguish between http and https
	certificate,
	private_key,			%% only for servers
	ca_cert,				%% not filled in for client connections
	encrypted=false,
	handshakes=[],			%% all handshake messages sent and received
	master_secret,
	client_mac,
	server_mac,
	client_random,
	server_random,
	client_seq_num=0,
	server_seq_num=0,
	client_state,
	server_state}).

%% EOF
