-module(ca).

-export([ca/1, sendme/1, two/1, get_connect/5]).

-record(rnd_ca, {one, two, three, tref, mons, interval, ip, port, r_list, r_db, errcnt}).
-record(worker, {interval, ip, port, r_list, r_db, period, tme, conn}).

%% core algo for generating 3000 random numbers per second
ca({init_, {N, Ip, Port, Rlist, Rdb, Erct}}) -> 
    R1=#worker{interval=N, ip=Ip, port=Port, r_list=Rlist, tme=0, r_db=Rdb},
    R2=#worker{interval=N, ip=Ip, port=Port, r_list=Rlist, period=333000, tme=0, r_db=Rdb},
    R3=#worker{interval=N, ip=Ip, port=Port, r_list=Rlist, period=666000, tme=0, r_db=Rdb},
    Sender=spawn(ca, sendme, [{init_, R1}]),
    Two=spawn(ca, two, [{init_, R2}]),
    Three=spawn(ca, two, [{init_, R3}]),
    M1=erlang:monitor(process, Sender),
    M2=erlang:monitor(process, Two),
    M3=erlang:monitor(process, Three),
    timer:sleep(1000),
    {ok, Tr}=timer:send_interval(1, next),
    ca({loop, #rnd_ca{one=Sender,two=Two, three=Three, tref=Tr, mons=[M1, M2, M3], ip=Ip, port=Port, r_list=Rlist, r_db=Rdb, interval=N, errcnt=Erct}});
ca({loop, My_rec}) ->
    #rnd_ca{one=Sender,two=Two, three=Three, tref=Tr,mons=Ms, errcnt = Erct}=My_rec,   
receive
    next ->
        T=os:system_time(),
        Sender!gogo,
        Two!{gogo, T},
        Three!{gogo, T},
        ca({loop, My_rec});
    stop ->
        timer:cancel(Tr),
        [erlang:demonitor(X, [flush])||X<-Ms],
        Sender!stop,
        Two!stop,
        Three!stop,
        exit(kill);
    {'DOWN', _Ref, prosess, _Pid, _Reason} ->
        timer:cancel(Tr),
        [erlang:demonitor(X, [flush])||X<-Ms],
        Sender!stop,
        Two!stop,
        Three!stop,
        #rnd_ca{ip=Ip, port=Port, r_list=Rlist, r_db=Rdb, interval=N}=My_rec,
        Erct==6 andalso gen_server:cast(rndogen, noconn),
        ca({init_, {N, Ip, Port, Rlist, Rdb, Erct+1}});
    _Any -> ca({loop, My_rec})
end.

%% handler2&handler 333 us 666 us
two({init_, Start_rec}) ->
    #worker{ip=Ip, port=Port, r_db=Rdb}=Start_rec,
    get_connect(Ip,Port,Rdb,0, self()),
    receive
        {sock, Sock} ->
            two({loop, Start_rec#worker{conn=Sock}});
        noconn ->
            exit(kill)
    end;
two({loop, My_rec}) -> 
    #worker{conn=Conn}=My_rec,
    receive
        {gogo, T} ->
            two({waitfor, My_rec#worker{tme=T}});
        stop ->
            gen_tcp:close(Conn),
            exit(kill);
        {tcp_closed, _Sock} ->
            two({init_, My_rec});
        {tcp_error, _Sock, _Reason} ->
            exit(kill);
        _Any -> two({0, My_rec})
    end;
two({waitfor, My_rec}) ->
    #worker{interval=N, r_list=Rlist, period=Period, tme=T, conn=Conn, r_db=Rdb}=My_rec,
    Zero=os:system_time(),
    D=Zero-T,
	Diff=case D >0 of
			true ->
				D;
			false ->
				1000000+Zero-T
	end,
    case Diff < Period of
        true ->
            two({waitfor, My_rec#worker{tme=T}});
        false ->
            Num=integer_to_list(rand:uniform(N)),
            gen_tcp:send(Conn, list_to_binary("select "++integer_to_list(Rdb)++"\r\n"++"rpush "++Rlist++" "++Num++"\r\n")),
            receive
                {tcp, _Sock, _Data} -> 
                    two({loop, My_rec}); 
                {tcp_closed, _Sock} -> 
                    two({init_, My_rec#worker{tme=0}});
                {tcp_error, _Sock, _Reason} ->
                    exit(kill)
            end
    end.

%% handler1 0 us    
sendme({init_, Start_rec}) ->
    #worker{ip=Ip, port=Port, r_db=Rdb}=Start_rec,
    get_connect(Ip, Port, Rdb, 0, self()),
    receive
        {sock, Sock} ->
            sendme({loop, Start_rec#worker{conn=Sock}});
        noconn ->
            exit(kill)
    end;
sendme({loop, My_rec}) ->
    #worker{interval=N, conn=Sock, r_list=Rlist, r_db=Rdb}=My_rec,
    receive
        gogo ->
            Num=integer_to_list(rand:uniform(N)),
            gen_tcp:send(Sock, list_to_binary("select "++integer_to_list(Rdb)++"\r\n"++"rpush "++Rlist++" "++Num++"\r\n")),
            receive
                {tcp, Sock, _Data} -> 
                    sendme({loop, My_rec}); 
                {tcp_closed, Sock} -> 
                    sendme({init_, My_rec#worker{tme=0}});
                {tcp_error, Sock, _Reason} ->
                    exit(kill)
            end;
        {tcp_closed, Sock} ->
            sendme({init_, My_rec});
        {tcp_error, Sock, _Reason} ->
            exit(kill);
        stop ->
            %% exit(Conn, kill),
            gen_tcp:close(Sock),
            exit(kill);
        _Any -> sendme({loop, My_rec})
    end.

%% connecting to redis, waiting 100 ms beside attempts 3 times   
get_connect(Ip, Port, Rdb, N, Pid) ->
    Conn=case gen_tcp:connect(Ip, Port, [list, {active, true}], 3) of
        {ok, Sock} ->
            gen_tcp:send(Sock, "select "++integer_to_list(Rdb)++"\r\n"),
            receive
                {tcp, Sock, Data} ->
                    case re:run(Data, "\\+[o|O][k|K]") /= nomatch of
                        true ->
                            {ok, Sock};
                        false ->
                            gen_tcp:close(Sock),
                            nop
                    end;
                {tcp_error,Sock,_Reason} ->
                    nop;
                {tcp_closed, Sock} ->
                    nop
            end;
        _Error -> 
            nop
    end,
    case Conn of
        {ok, S} ->
            gen_tcp:controlling_process(S, Pid),
            Pid!{sock, S};
        nop ->
            timer:sleep(100),
            case N==2 of
                true -> Pid!noconn;
                false -> get_connect(Ip, Port, Rdb, N+1, Pid)
            end
    end.
