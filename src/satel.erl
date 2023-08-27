-module(satel).

-include("moo_record.hrl").

-export([filter/1, expr/2, get_uv/4, get_uv/6, erat/0, expr/3, un/2, vn/2, get_j/3, send_prime/4,fa/3]).
%% -import(eredis).
%% ok
%% let's try to bend it
%% D number [5, -7, 9, -11 etc.]
%% Luca pseudoprimes&sequences
%% P set to 1 
%% Q is (1-D)/4 - 2nd
%% N testing number
%% R and S: N-e=R*2^S with e=J(D, N)
%% 
%% simple Luca test: U(N+1) rem N == 0 with J(D, N)=-1
%% 
%% strong test Luca: U(S) rem N == 0 orelse V(S*2^j) rem N == 0 for j<R with J(D, N)=1
%% U is U(Q, Number)
%% V is V(Q, Number)
%% U(0)=0 U(1)=1 U(n+2)=P*U(n+1)-Q*U(n)
%% V(0)=2 V(1)=P V(n+2)=P*V(n+1)-Q*V(n)
%% 
%% using rules: 2*V 2*U U1+U2 V1+V2
%% 
%% for 10M number processing time is ~40 seconds for simple Luca test
%% 
%% strong test' set INCLUDED TO simple test' set, hence, the 2nd set has more elements
%% 

fa(Ip, Port, Rlist) -> 
	receive
		{gimme, Pid} ->
			RN=[13, 10],
			S=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
			Res=case S of
				{ok, Sock}->
					Msgto="*2\r\n$4\r\nLPOP\r\n$"++integer_to_list(length(Rlist))++RN++Rlist++RN,
					gen_tcp:send(Sock, Msgto),
					Dta=receive
						{tcp, _, D} -> D
					end,
					gen_tcp:close(Sock),
					Msg=string:split(Dta, "\r\n", all),
					case length(Msg) of
						3 ->  %% $numlength numstring [empty]
							lists:nth(2, Msg);
						_Any1 -> 
							"ok"
					end;
				_Any2 -> "ok"
			end,
			Pid!{takeit, Res},
			fa(Ip, Port, Rlist);
		_Any ->
			fa(Ip, Port, Rlist)
	end.

erat() ->
	erat_gen(lists:seq(3, 1000000, 2), []).

erat_gen([H|T], Acc) when H > 997 -> lists:flatten([lists:reverse(lists:flatten(Acc)), T]);
erat_gen([H|T], Acc) ->
	erat_gen(lists:filter(fun(El) -> (El rem H) /=0 end, T), [H|Acc]).

%% i had a trouble with garbage collectiopn of small elements: pids/ports and data sets, so any VOID communications handles dedicated entity

send_prime(Ip, Port, Msg, Attempt) ->
	S=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
	case S of
		{ok, Sock} ->
			gen_tcp:send(Sock, list_to_binary(Msg)),
			receive
				_Dta -> ok 
			end,
			gen_tcp:close(Sock),
			exit(kill);
		_Other ->
			case Attempt of
				3 -> exit(kill);
				_Else ->
					send_prime(Ip, Port, Msg, Attempt+1)
			end
	end.

filter(#rnd_sat{r_set=Rset, r_list=Rlist, lst=L, fapid=FA}=R_sat) -> %% BPSW test
	%% FA!{gimme, self()},
	%% Dta=receive
	%% 	{takeit, Dat} -> Dat
	%% end,
	{ok, Bin}=eredis:q(FA, ["LPOP", Rlist]),
	Dta=case is_binary(Bin) of
		true -> binary_to_list(Bin);
		false -> "nop"
	end,
	Tryit=re:run(Dta, "[0-9]+"),
	case Tryit of
		{match, [{Bg, Ln}]} ->
			Number=list_to_integer(lists:sublist(Dta, Bg+1, Ln)),
			case (Number band 1) ==0 of
				true -> %% even
					ok;
				false ->
					Bool2=(Number > 2) andalso (Number < 1000000) andalso lists:member(Number, L),
					if
						Bool2 == true -> %% prime
							Num=integer_to_list(Number),
							%% Msg="*3\r\n$4\r\nSADD\r\n$"++integer_to_list(length(Rset))++"\r\n"++Rset++"\r\n$"++integer_to_list(length(Num))++"\r\n"++Num++"\r\n",
							%% spawn(satel, send_prime, [Ip, Port, Msg, 1]);
							eredis:q(FA, ["SADD", Rset, Num]);
						Number < 1000000 -> %% not prime
							ok;
						true -> 
							Bool=lists:foldl(fun(El, A) -> (not ((Number rem El) == 0)) and A end, true, [3, 5, 7, 11, 13, 17]) andalso  not fullsq(Number),
							case Bool of
								true -> %% maybe
									%% Miller-Rabin
									{D, S}=getodd(Number-1), %% N-1=D*2^S
									MR1=expr(D, 2) rem Number,
									MR2=next_mr(D, S-1, Number),
									FU=fun(El, Q, Ul, Vl,  Test) ->
										{U,_}=get_uv(Q, El, Ul, Vl),
										case U rem Test of
											0 -> true;
											_Any -> false
										end
									end,
									Res=case MR1==1 orelse MR2==true of
										true ->
											Dd=get_j(5, Number, 1), %% Jacobi symbol for [5, -7, 9, -11 etc.]
											Vlist=gimmelist(Number-1),
											Q=(1-Dd) div 4,
											Ul=un(Q, 300), %% optimal
											Vl=vn(Q, 300),
											BPSW_VRes=fv(Vlist, Q, Ul, Vl, Number, false),
											BPSW_URes=FU(hd(Vlist), Q, Ul, Vl, Number),
											BPSW_VRes or BPSW_URes;
										false -> false
									end,
									%% send data to redis set
									case Res of
										true ->
											Snum=integer_to_list(Number),
											%% Msg2="*3\r\n$4\r\nSADD\r\n$"++integer_to_list(length(Rset))++"\r\n"++Rset++"\r\n$"++integer_to_list(length(Snum))++"\r\n"++Snum++"\r\n",
											%% spawn(satel, send_prime, [Ip, Port, Msg2, 1]);
											eredis:q(FA, ["SADD", Rset, Snum]);
										false -> 
											ok
									end;
								false -> %% not prime,
									ok
							end
					end
			end;
		_Nop ->
			ok
	end,
	filter(R_sat).
	%% gen_server:cast(rndogen, {ch_pid, Num, NewPid}), 

%% strong test Luca for V	
fv([], _, _, _, _, Res) -> Res;
fv([H|T], Q, Ul, Vl, Test, Res) ->
	{_,V}=get_uv(Q, H, Ul, Vl),
	ResN=case V rem Test of
		0 -> true;
		_Any -> Res
	end,
	fv(T, Q, Ul, Vl, Test, ResN).

%% Miller-Rabin	with 2 base
next_mr(_D, 0, _N) -> false;
next_mr(D, R, N) ->
	Med=expr(expr(R, 2) * D, 2) rem N,
	M=N-1,
	case Med of
		M -> 
			true;
		_Other -> 
			next_mr(D, R-1, N)
	end.

%% Jacobi start fun
get_j(D, N, Res) ->
	case j(1, D, N) of
		Res -> D;
		_Any -> get_j(-1*(abs(D)+2), N, Res)
	end.

%% Jacobi symbol. Here I using only the quiadratic law.	
j(Signum, 1, _Q) -> %% (1/Q) = 1
	Signum;
j(Signum, -1, Q) -> %% (-1/Q) = -1^((Q-1)/2) 
	S=((Q-1) div 2) band 1,
	case S of
		0 ->
			Signum;
		1 ->
			-Signum
	end;
j(Signum, 2, Q) -> %% (2/Q) = -1^((Q^2-1)/8) 
	S=(((Q*Q)-1) bsr 3) band 1,
	case S of
		0 ->
			Signum;
		1 ->
			-Signum
	end;
j(Signum, P,Q) when P<0 -> %% (-P/Q) = (-1/Q)(P/Q)
	S=j(1, -1, Q),
	j(Signum*S, -P, Q);
j(Signum, P, Q) when (P band 1) == 0 -> %% (2*P/Q) = (P/Q)(2/Q)
	{Num, Exp}=getodd(P),
	Med=j(1, 2, Q),
	S=case Med of
		1 -> 1;
		-1 ->
			One=Exp band 1,
			case One of
				0 -> 1;
				1 -> -1
			end
	end,
	j(S*Signum, Num, Q);

j(Signum, P, Q) when Q>P ->
	{A, B, C}=case  P band 1 of %% prevent loop
		1 -> %% odd P
			{Signum, P, Q};
		0 -> %% even P
			{N, Exp}=getodd(P),
			Si=((((Q*Q)-1) div 8)*Exp) band 1,
			Sig=case Si of
				0 -> Signum;
				1 -> -Signum
			end,
			{Sig, N, Q}
	end,
	case B of
		1 -> j(A, B, C);
		_Other -> %% and just here is Q>P (P/Q) = -1^(((P-1)*(Q-1))/2)(Q/P)
			S=(((B-1)*(C-1)) bsr 1) band 1,
			Sign=case S of
				0 -> 1;
				1 -> -1
			end,
			j(A*Sign, C, B)
	end; 
j(Signum, P,Q) ->  %% typical
	j(Signum, P rem Q, Q).

%% for parallel handling
gimmelist(Number) ->
	gimmelist(Number bsr 1, []).

gimmelist(Number, L) when (Number band 1) ==1 ->
	lists:flatten([Number|L]);
gimmelist(Number, L) -> gimmelist(Number bsr 1, [Number|L]).

%% basic count u seq
un(Q, Num) ->
	case Num of
		0 -> 0;
		1 -> 1;
		_Any -> el(Q, Num-1, 1, 0, [1])
	end.

%% basic count of v seq	
vn(Q, Num) ->
	case Num of
		0 -> 2;
		1 -> 1;
		_Any -> el(Q, Num-1, 1, 2, [1])
	end.


el(_, 0, _, _, L) -> lists:reverse(lists:flatten(L));
el(Q, Num, U_1, U_0, L) -> %% num +1 is ok for simple Luca test
	Next=U_1 - (U_0 * Q),
	el(Q, Num-1, Next, U_1, [Next|L]).

%% {P, Expr}=P*2^Expr	
getodd(P) ->
	getodd(P bsr 1, 1).
getodd(P, Exp) when (P band 1) ==1 ->
	{P, Exp};
getodd(P, Exp) ->
	getodd(P bsr 1, Exp+1).

%% get Luca U&V number	
get_uv(Q, N, Ul, Vl) ->
	R=erlang:make_ref(),
	spawn(satel, get_uv, [Q, N, self(), R, Ul, Vl]),
	{_, {U, V}}=receive
		Data -> Data
	end,
	{U, V}.

%% integer power	
expr(_X, 0, Acc) -> Acc;
expr(X, E, Acc) when (E band 1) == 1 -> expr(X*X, E bsr 1, Acc*X);
expr(X, E, Acc) -> expr(X *X, E bsr 1, Acc).

expr(E, N)->
	{Signn, Sn}=case N > 0 of
		true -> {1, N};
		false ->
			case E band 1 of
				1 -> {-1, -N};
				0 -> {1, N}
			end
	end,
	Res=expr(Sn, E, 1),
	case Signn of
		-1 -> -Res;
		_Any -> Res
	end.

get_uv(Q, N, Pid, Ref, Ul, Vl) when (N band 1) == 1 ->
	case N > 300 of
			true ->
				Half=N bsr 1,
				R1=erlang:make_ref(),
				R2=erlang:make_ref(),
				spawn(satel, get_uv, [Q, Half+1, self(), R1, Ul, Vl]),
				spawn(satel, get_uv, [Q, Half, self(), R2, Ul, Vl]),
				L1=receive
					{K1, Data1}-> {K1, Data1}
				end,
				L2=receive
					{K2, Data2} -> {K2, Data2}
				end,
				L=[L1, L2],
				{Un1, Vn1}=proplists:get_value(R1, L),
				{Un2, Vn2}=proplists:get_value(R2, L),
				Pid!{Ref, {(Un1*Vn2 + Un2*Vn1) bsr 1, Vn1*Vn2 - expr(Half, Q)}};
 			false ->
				Pid!{Ref, {lists:nth(N, Ul), lists:nth(N, Vl)}}
	end,
	exit(self(), kill);
get_uv(Q, N, Pid, Ref, Ul, Vl) ->
	case N > 300 of
			true ->
				R=erlang:make_ref(),
				spawn(satel, get_uv, [Q, N bsr 1, self(), R, Ul, Vl]),
				{Un, Vn}=receive
					{_, {U, V}} -> {U, V}
				end,
				Pid!{Ref, {Un* Vn, Vn*Vn - (expr(N bsr 1, Q) bsl 1)}};
			false ->
				Pid!{Ref, {lists:nth(N, Ul), lists:nth(N, Vl)}}
	end,
	exit(self(), kill).

%% test for full square	
fullsq(N) ->
	{A, Exp}=med(N, 0),
 	fullsq(N, 1, 2, ((N bsr Exp)+A) bsr 1).

fullsq(_N, D, _Other, D) -> false;
fullsq(N, _Other, D, D) -> 
	case D*D of
		N -> true;
		_Else -> false
	end;
fullsq(N, _A, B, C) ->
	fullsq(N, B, C, (C+(N div C)) bsr 1).

%% calculate median	
med(0,  Acc) -> 
	{1 bsl Acc, Acc};
med(Q, Acc) ->
	if 
		Q>=8 -> med(Q bsr 4, Acc + 2);
		Q>=4 -> med(Q bsr 3, Acc + 1);
		Q>=2 -> med(Q bsr 2, Acc);
		true -> med(Q bsr 1, Acc)
	end.


