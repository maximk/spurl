-module(html).

-export([string/1]).

-import(lists,[reverse/1]).

string(Html) ->
	doc(Html, [], []).

doc("<!DOCTYPE" ++ Hs, Ts, Txt) ->
	case ws(Txt) of
	[] ->
		dtype(Hs, Ts);
	_ ->
		dtype(Hs, [{t,reverse(Txt)}|Ts])
	end;
	
doc("&amp;" ++ Hs, Ts, Txt) ->
	doc(Hs, Ts, [$&|Txt]);
doc("&gt;" ++ Hs, Ts, Txt) ->
	doc(Hs, Ts, [$>|Txt]);
doc("&lt;" ++ Hs, Ts, Txt) ->
	doc(Hs, Ts, [$<|Txt]);
doc("&quot;" ++ Hs, Ts, Txt) ->
	doc(Hs, Ts, [$"|Txt]);

%% TODO: &nbsp, &copy

doc("<!--" ++ Hs, Ts, Txt) ->
	case ws(Txt) of
	[] ->
		remark(Hs, Ts);
	_ ->
		remark(Hs, [{t,reverse(Txt)}|Ts])
	end;

doc("</" ++ Hs, Ts, Txt) ->
	case ws(Txt) of
	[] ->
		etag(ws(Hs), Ts, []);
	_ ->
		etag(ws(Hs), [{t,reverse(Txt)}|Ts], [])
	end;
doc("<" ++ Hs, Ts, Txt) ->
	case ws(Txt) of
	[] ->
		otag(ws(Hs), Ts, []);
	_ ->
		otag(ws(Hs), [{t,reverse(Txt)}|Ts], [])
	end;
doc([H|Hs], Ts, Txt) ->
	doc(Hs, Ts, [H|Txt]);
doc([], Ts, Txt) ->
	case ws(Txt) of
	[] ->
		{ok,reverse(Ts)};
	_ ->
		{ok,reverse([{t,reverse(Txt)}|Ts])}
	end.

dtype(">" ++ Hs, Ts) ->
	doc(Hs, Ts, []);
dtype([_|Hs], Ts) ->
	dtype(Hs, Ts);
dtype([], Ts) ->
	{ok,reverse(Ts)}.

remark("-->" ++ Hs, Ts) ->
	doc(Hs, Ts, []);
remark([_|Hs], Ts) ->
	remark(Hs, Ts);
remark([], Ts) ->
	{ok,reverse(Ts)}.

script("</script>" ++ Hs, Ts, Code) ->
	case ws(Code) of
	[] ->
		doc(Hs, [{e,"script"}|Ts], []);
	_ ->
		doc(Hs, [{e,"script"},{s,reverse(Code)}|Ts], [])
	end;
script("</SCRIPT>" ++ Hs, Ts, Code) ->
	case ws(Code) of
	[] ->
		doc(Hs, [{e,"script"}|Ts], []);
	_ ->
		doc(Hs, [{e,"script"},{s,reverse(Code)}|Ts], [])
	end;
script([H|Hs], Ts, Code) ->
	script(Hs, Ts, [H|Code]);
script([], Ts, _Code) ->
	{ok,reverse(Ts)}.	%% unterminated script dropped

etag(">" ++ Hs, Ts, Ns) ->
	doc(Hs, [{e,reverse(Ns)}|Ts], []);
etag(" " ++ Hs, Ts, Ns) ->
	etag0(ws(Hs), Ts, Ns);
etag([H|Hs], Ts, Ns) ->
	etag(Hs, Ts, [string:to_lower(H)|Ns]);
etag([], Ts, Ns) ->
	{ok,reverse([{e,reverse(Ns)}|Ts])}.

etag0(">" ++ Hs, Ts, Ns) ->
	doc(Hs, [{e,reverse(Ns)}|Ts], []);
etag0(Hs, Ts, Ns) ->
	doc(Hs, [{e,reverse(Ns)}|Ts], []).

otag("/>" ++ Hs, Ts, "tpircs") ->
	script(Hs, [{b,"script",[]}|Ts], []);
otag("/>" ++ Hs, Ts, Ns) ->
	doc(Hs, [{b,reverse(Ns),[]}|Ts], []);
otag(">" ++ Hs, Ts, "tpircs") ->
	script(Hs, [{o,"script",[]}|Ts], []);
otag(">" ++ Hs, Ts, Ns) ->
	doc(Hs, [{o,reverse(Ns),[]}|Ts], []);
otag(" " ++ Hs, Ts, Ns) ->
	attr(ws(Hs), Ts, Ns, [], []);
otag([H|Hs], Ts, Ns) ->
	otag(Hs, Ts, [string:to_lower(H)|Ns]);
otag([], Ts, Ns) ->
	{ok,reverse([{o,reverse(Ns),[]}|Ts])}.

attr("/>" ++ Hs, Ts, "tpircs", As, []) ->
	script(Hs, [{b,"script",reverse(As)}|Ts], []);
attr("/>" ++ Hs, Ts, Ns, As, []) ->
	doc(Hs, [{b,reverse(Ns),reverse(As)}|Ts], []);
attr(">" ++ Hs, Ts, "tpircs", As, []) ->
	script(Hs, [{o,"script",reverse(As)}|Ts], []);
attr(">" ++ Hs, Ts, Ns, As, []) ->
	doc(Hs, [{o,reverse(Ns),reverse(As)}|Ts], []);
attr("/>" ++ Hs, Ts, "tpircs", As, Qs) ->
	As1 = [{reverse(Qs),""}|As],
	script(Hs, [{b,"script",reverse(As1)}|Ts], []);
attr("/>" ++ Hs, Ts, Ns, As, Qs) ->
	As1 = [{reverse(Qs),""}|As],
	doc(Hs, [{b,reverse(Ns),reverse(As1)}|Ts], []);
attr(">" ++ Hs, Ts, "tpircs", As, Qs) ->
	As1 = [{reverse(Qs),""}|As],
	script(Hs, [{o,"script",reverse(As1)}|Ts], []);
attr(">" ++ Hs, Ts, Ns, As, Qs) ->
	As1 = [{reverse(Qs),""}|As],
	doc(Hs, [{o,reverse(Ns),reverse(As1)}|Ts], []);
attr("=\"" ++ Hs, Ts, Ns, As, Qs) ->
	valq(Hs, Ts, Ns, As, Qs, []);
attr("=" ++ Hs, Ts, Ns, As, Qs) ->
	aval(Hs, Ts, Ns, As, Qs, []);
attr(" " ++ Hs, Ts, Ns, As, Qs) ->
	As1 = [{reverse(Qs),""}|As],
	attr(ws(Hs), Ts, Ns, As1, []);
attr([H|Hs], Ts, Ns, As, Qs) ->
	attr(Hs, Ts, Ns, As, [string:to_lower(H)|Qs]);
attr([], Ts, Ns, As, []) ->
	{ok,reverse([{o,reverse(Ns),reverse(As)}|Ts])};
attr([], Ts, Ns, As, Qs) ->
	As1 = [{reverse(Qs),""}|As],
	{ok,reverse([{o,reverse(Ns),reverse(As1)}|Ts])}.

aval("/>" ++ Hs, Ts, "tpircs", As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	script(Hs, [{b,"script",reverse(As1)}|Ts], []);
aval("/>" ++ Hs, Ts, Ns, As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	doc(Hs, [{b,reverse(Ns),reverse(As1)}|Ts], []);
aval(">" ++ Hs, Ts, "tpircs", As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	script(Hs, [{o,"script",reverse(As1)}|Ts], []);
aval(">" ++ Hs, Ts, Ns, As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	doc(Hs, [{o,reverse(Ns),reverse(As1)}|Ts], []);
aval(" " ++ Hs, Ts, Ns, As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	attr(ws(Hs), Ts, Ns, As1, []);
aval([H|Hs], Ts, Ns, As, Qs, Vs) ->
	aval(Hs, Ts, Ns, As, Qs, [H|Vs]);
aval([], Ts, Ns, As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	{ok,reverse([{o,reverse(Ns),reverse(As1)}|Ts])}.

valq([$\\,$"|Hs], Ts, Ns, As, Qs, Vs) ->		%% only " is escaped
	valq(Hs, Ts, Ns, As, Qs, [$"|Vs]);
valq([$"|Hs], Ts, Ns, As, Qs, Vs) ->
	As1 = [{reverse(Qs),reverse(Vs)}|As],
	attr(ws(Hs), Ts, Ns, As1, []);
valq([H|Hs], Ts, Ns, As, Qs, Vs) ->
	valq(Hs, Ts, Ns, As, Qs, [H|Vs]);
valq([], Ts, Ns, As, _Qs, _Vs) ->
	{ok,reverse([{o,reverse(Ns),reverse(As)}|Ts])}.	%% drop attribute with unterminated quoted value

ws([32|Hs]) -> ws(Hs);
ws([13|Hs]) -> ws(Hs);
ws([10|Hs]) -> ws(Hs);
ws([9|Hs]) -> ws(Hs);
ws(Hs) -> Hs.

%% EOF
