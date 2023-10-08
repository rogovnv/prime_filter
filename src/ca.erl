-module(ca).

-export([ca/1, two/1, get_connect/4, gogo/6]).

-record(rnd_ca, {tref, acc, pids, mons}).
-record(worker, {interval, period, pid, conn}).

%% core algo for generating 3000 random numbers per second
ca({fin, _My_rec}) ->
    receive
        _Any -> ok
    end,
    ca({fin, _My_rec});
ca({init_, {N, S1, S2, S3}}) ->   
    P3=spawn(ca, two, [{loop, #worker{interval=N, period=2000000, conn=S3, pid=nop}}]), 
    P2=spawn(ca, two, [{loop, #worker{interval=N, period=1000000, conn=S2, pid=P3}}]),
    P1=spawn(ca, two, [{loop, #worker{interval=N, period=0, conn=S1, pid=P2}}]),
    Mons=[erlang:monitor(process, X)||X <- [P1, P2, P3]],
    {ok, Tr}=timer:send_interval(1000, next),
    ca({loop, #rnd_ca{tref=Tr, acc=[], pids=[P1, P2, P3], mons=Mons}});
ca({loop, My_rec}) ->
    #rnd_ca{tref=Tr, mons=Mons, pids=Pids}=My_rec,   
    receive
        next ->
            T=erlang:system_time(), %% rem 1000000,
            hd(Pids)!{gogo, T},
            ca({loop, My_rec});
        stop ->
            timer:cancel(Tr),
            [erlang:demonitor(X, [flush])||X <- Mons],
            [X!stop||X <- Pids],
            exit(kill);
        {'DOWN', _Ref, prosess, _Pid, _Reason} ->
            timer:cancel(Tr),
            [erlang:demonitor(X, [flush])||X <- Mons],
            [X!stop||X <- Pids],
            gen_server:cast(rndogen, fadown),
            ca({fin, My_rec});
        _Any -> 
            ca({loop, My_rec})
    end.

%% handler 0 us 333 us 666 us

two({loop, My_rec}) -> 
    #worker{period=Period, interval=N, conn=S, pid=P}=My_rec,
    receive
        {gogo, T} ->
            time_support(T+Period),
            S!{faster, integer_to_list(rand:uniform(N)), erlang:system_time()},
            time_support(T+Period+333333),
            S!{faster, integer_to_list(rand:uniform(N)), erlang:system_time()},
            time_support(T+Period+666666),
            S!{faster, integer_to_list(rand:uniform(N)), erlang:system_time()},
            case is_pid(P) of
                true -> P!{gogo, T};
                false -> ok
            end,
            two({loop, My_rec});
        stop ->
            exit(kill);
        _Any -> 
            two({loop, My_rec})
    end.          

get_connect(Ip, Port, Atmt, Rdb) ->
    Conn=case gen_tcp:connect(Ip, Port, [list, {active, true}], 3) of
        {ok, Sock} ->
            gen_tcp:send(Sock, "SELECT "++integer_to_list(Rdb)++"\r\n"),
            receive
                {tcp, Sock, Data} ->
                    case re:run(Data, "\\+[o|O][k|K]") /= nomatch of
                        true ->
                            {ok, Sock};
                        false ->
                            gen_tcp:close(Sock),
                            nop
                    end;
                {tcp_error, Sock, _Reason} ->
                    nop;
                {tcp_closed, Sock} ->
                    nop
            end;
        _Error -> 
            nop
    end,
    case Conn of
        {ok, S} ->
            {ok, S};
        nop ->
            timer:sleep(100),
            case Atmt==2 of
                true -> 
                    nop;
                false ->
                    get_connect(Ip, Port, Atmt+1, Rdb)
            end
    end.

gogo(_Ip, _Port, _Rlist, _Rset, _Rdb, 9) -> %% wait for kill
    receive
         _Any -> gogo(_Ip, _Port, _Rlist, _Rset, _Rdb, 9)
    end;
gogo(Ip, Port, Rlist, Rset, Rdb, 0) -> %% tcp closed
    case ca:get_connect(Ip, Port, 0, Rdb) of
        {ok, Sock} ->
            gogo(Ip, Port, Rlist, Rset, Rdb, {sock, Sock});
        nop ->
            gen_server:cast(rndogen, fadown),
            gogo(Ip, Port, Rlist, Rset, Rdb, 9)
    end;
gogo(Ip, Port, Rlist, Rset, Rdb, {sock, Sock}) -> %% regular 
    receive
        {faster, Num, _Tme} ->
            gen_tcp:send(Sock, list_to_binary("RPUSH "++Rlist++" "++Num++"\r\n")),
             receive
                {tcp, Sock, _Data} ->
                    gogo(Ip, Port, Rlist, Rset, Rdb, {sock, Sock}); 
                 {tcp_closed, Sock} -> 
                    gogo(Ip, Port, Rlist, Rset, Rdb, 0);
                 {tcp_error, Sock, _Reason} ->
                    gen_server:cast(rndogen, fadown),
                    gogo(Ip, Port, Rlist, Rset, Rdb, 9)
            end;
        {tcp, Sock, _Data} -> %% (integer) NUMBER
            gogo(Ip, Port, Rlist, Rset, Rdb, {sock, Sock}); 
        {tcp_closed, Sock} -> 
            gogo(Ip, Port, Rlist, Rset, Rdb, 0);
        {tcp_error, Sock, _Reason} ->
            gen_server:cast(rndogen, fadown),
            gogo(Ip, Port, Rlist, Rset, Rdb, 9);
        stop ->
            gen_tcp:close(Sock),
            exit(kill);
        _Any ->
            gogo(Ip, Port, Rlist, Rset, Rdb, {sock, Sock})
    end.

time_support(T) ->
    case (erlang:system_time() - T) > 0 of
        false -> time_support(T);
        true -> ok
    end.