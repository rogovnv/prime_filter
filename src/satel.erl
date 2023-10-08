-module(satel).

-export([filter/1, erat/0, fa/6, handler/2]).

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
%% but here I using Miller-Rabin`s test

%% i/o handler for filter
fa(_Ip, _Port, _Rlist, _Rset, _Rdb, 9) ->
	receive
		 _Any -> fa(_Ip, _Port, _Rlist, _Rset, _Rdb, 9)
	end;
fa(Ip, Port, Rlist, Rset, Rdb, {0, _Atmt}) ->
	case ca:get_connect(Ip, Port, 0, Rdb) of
        {ok, Sock} ->
            fa(Ip, Port, Rlist, Rset, Rdb, {sock, Sock});
        nop ->
            gen_server:cast(rndogen, noconn),
			fa(Ip, Port, Rlist, Rset, Rdb, 9)
    end;
fa(Ip, Port, Rlist, Rset, Rdb, {sock, Sock}) -> 
	receive
		{queue, Pid} ->
			gen_tcp:send(Sock, list_to_binary("LLEN "++Rlist++"\r\n")),
			receive
                	{tcp, Sock, Dta} ->
						Pid!{queue, Dta},
                    	fa(Ip, Port, Rlist, Rset, Rdb, {sock, Sock}); 
            	    {tcp_closed, Sock} -> 
						Pid!":0",
                		fa(Ip, Port, Rlist, Rset, Rdb, {0, 0});
                	{tcp_error, Sock, _Reason} ->
						Pid!":0",
                    	gen_server:cast(rndogen, fadown),
						fa(Ip, Port, Rlist, Rset, Rdb, 9)
            end;
		{sendprime,Number} ->
			Msg="SADD "++Rset++" "++Number++"\r\n",
			gen_tcp:send(Sock, list_to_binary(Msg)),
			receive
				{tcp, Sock, _Dta} ->
					fa(Ip, Port, Rlist, Rset, Rdb, {sock, Sock});
				{tcp_closed, Sock} ->
					fa(Ip, Port, Rlist, Rset, Rdb, {0, 0});
				{tcp_error,Sock, _Reason} ->
					gen_server:cast(rndogen, fadown),
					fa(Ip, Port, Rlist, Rset, Rdb, 9)
			end;
		{gimme, Pid} ->
			Msgto="BLPOP "++Rlist++" 1\r\n",
			gen_tcp:send(Sock, list_to_binary(Msgto)),
			receive
				{tcp, Sock, D} -> 
					io:format("~nsatel got ~p", [D]),
					Res=case re:run(D, "[0-9]+", [global]) of
						nomatch ->
							"ok";
						{match, List} ->
							[{Bg, Len}]=hd(lists:reverse(List)),
							lists:sublist(D, Bg+1, Len)
					end,
					Pid!{takeit, Res},
					fa(Ip, Port, Rlist, Rset, Rdb, {sock, Sock});
				{tcp_closed, Sock} ->
					fa(Ip, Port, Rlist, Rset, Rdb, 0);
				{tcp_error,Sock, _Reason} ->
					gen_server:cast(rndogen, fadown),
					fa(Ip, Port, Rlist, Rset, Rdb, 9)
			end;
		stop ->
			gen_tcp:close(Sock),
			exit(kill);
		{tcp_error, Sock, _Reason} ->
			gen_server:cast(rndogen, fadown),
			fa(Ip, Port, Rlist, Rset, Rdb, 9);
		{tcp_close, Sock} ->
			fa(Ip, Port, Rlist, Rset, Rdb, {0, 0});
		_Any ->
			fa(Ip, Port, Rlist, Rset, Rdb, {sock, Sock})
	end.

erat() ->
	erat_gen(lists:seq(3, 1000000, 2), []).

erat_gen([H|T], Acc) when H > 997 -> lists:flatten([lists:reverse(lists:flatten(Acc)), T]);
erat_gen([H|T], Acc) ->
	erat_gen(lists:filter(fun(El) -> (El rem H) /=0 end, T), [H|Acc]).

filter({{start, N}, FA}) ->
	L=erat(),
	{ok, Tr}=timer:send_interval(5000, test_perf),
	Ps=lists:map(fun(_E) -> spawn(satel, handler, [L, FA]) end, lists:seq(1, N)),
	filter({{loop, Ps, Tr}, FA});
filter({{loop, Ps, Tr}, FA}) ->
	receive
		stop ->
			timer:cancel(Tr),
			Fn=fun(X) ->
				try exit(X,kill)
				catch _:_ -> ok
				end
			end,
			[Fn(X)||X<-Ps],
			exit(kill);
		test_perf ->
			FA!{queue, self()},
			Data=receive
				{queue, Dta} ->
					Dta
			end,
			{match, [{Bg, Len}]}=re:run(Data, "[0-9]+"),
			case list_to_integer(lists:sublist(Data, Bg+1, Len)) < 5000 of %% ask length of Rlist
				true ->
					filter({{loop, Ps, Tr}, FA});
				false ->
					case length(Ps) < 10 of
						true ->
							L=erat(),
							filter({{loop, [spawn(satel, handler, [L, FA])|Ps], Tr}, FA});
						false -> filter({{loop, Ps, Tr}, FA})
					end
			end
	end.

handler(L,FA) ->
	FA!{gimme, self()},
	Dta=receive
		{takeit, Dat} -> Dat
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
							FA!{sendprime, integer_to_list(Number)};
						Number < 1000000 -> %% not prime
							ok;
						true -> 				
							Bool=lists:foldl(fun(El, A) -> (Number rem El /= 0) and A end, true, [3, 5, 7, 11, 13, 17]) andalso (fullsq(Number) == false),
							case Bool of
								true -> %% maybe
									%% Miller-Rabin								
									{D, S}=getodd(Number-1), %% N-1=D*2^S
									Attempts=get_attempt(Number, 0),
									FMR=fun(Nn, Dd, Ss, Testn) ->
										MR1=modexpr(split_expr(Testn, Dd, [], Nn), Number),
										MR2=next_mr(Testn, Dd, Ss-1, Nn),
										if
											MR1/=1 andalso MR2/=true -> false;
											MR1==1 orelse MR2==true -> true
										end
									end,
									Lrand=lists:map(fun(_) -> rand:uniform(Number-2)+2 end, lists:seq(1, Attempts)),
									Lres=[FMR(Number, D, S, X)||X<-Lrand],
									case lists:foldl(fun(El, Acc) -> El and Acc end, true, Lres) of
										true ->
											FA!{sendprime, integer_to_list(Number)};
										false ->
											ok
									end;
								false ->
									ok
							end
					end
			end;
		_Else -> ok
	end,		
	handler(L, FA).

get_attempt(0, Acc) -> Acc;
get_attempt(N, Acc) ->
	get_attempt(N bsr 4, Acc+4).

split_expr(_, 0, Acc, _) -> Acc;
split_expr(Num, Expr, Acc, Current) ->
	case Expr band 1 of
		0 -> 
			split_expr(Num, Expr div 2, Acc, Current*2);
		1 ->
			split_expr(Num, Expr div 2, [{Num, Current}|Acc], Current *2)
	end.

modexpr(Prepared_L, Mod) -> 
	H=hd(Prepared_L),
	case is_tuple(H) of
		true ->
			{N, _}=H,
			Rem=(N*N) rem Mod,
			F=fun(El) ->
				case is_tuple(El) of
					true ->
						case El of
							{A, 1} ->
								A;
							{_A, 2} ->
								Rem;
							{_A, B} ->
								{Rem, B div 2}
						end;
					false -> El
				end
			end,
			modexpr(lists:map(F, Prepared_L), Mod);
		false ->
			lists:foldl(fun(El, Acc) -> El*Acc end, 1,  Prepared_L) rem Mod
	end.

%% Miller-Rabin	
next_mr(_J, _D, 0, _N) -> false;
next_mr(J, D, R, N) ->
	Med=modexpr(split_expr(J, (1 bsl R)*D, [], 1), N),
	M=N-1,
	case Med of
		M -> 
			true;
		_Other -> 
			next_mr(J, D, R-1, N)
	end.

%% {P, Expr}=P*2^Expr	
getodd(P) ->
	getodd(P div 2, 1).
getodd(P, Exp) when (P band 1) ==1 ->
	{P, Exp};
getodd(P, Exp) ->
	getodd(P div 2, Exp+1).


%% test for full square	
fullsq(N) ->
	{A, Exp}=med(N, 0),
 	fullsq(N, 1, 2, ((N bsr Exp)+A) div 2).

fullsq(_N, D, _Other, D) -> false;
fullsq(N, _Other, D, D) -> 
	case D*D of
		N -> true;
		_Else -> false
	end;
fullsq(N, _A, B, C) ->
	fullsq(N, B, C, (C+(N div C)) div 2).

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





