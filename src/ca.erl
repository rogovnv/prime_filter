-module(ca).

%% -import("moo_record.hrl").

-export([ca/1, sendme/1, two/1]).

-record(rnd_ca, {interval, ip, port, r_list, pid, one, two, three, tref, r_db}).
-record(worker, {interval, ip, port, r_list, r_db, pid, period, tme, conn, cnt}).

ca({-1, Start_rec}) -> 
    #rnd_ca{interval=N, ip=Ip, port=Port, r_list=Rlist,pid=Pid, r_db=Rdb}=Start_rec,
    R1=#worker{interval=N, ip=Ip, port=Port, r_list=Rlist, pid=self(), r_db=Rdb},
    R2=#worker{interval=N, ip=Ip, port=Port, r_list=Rlist, period=333000, pid=self(), tme=0, r_db=Rdb},
    R3=#worker{interval=N, ip=Ip, port=Port, r_list=Rlist, period=666000, pid=self(), tme=0, r_db=Rdb},
    Sender=spawn(ca, sendme, [{-1, R1}]),
    Two=spawn(ca, two, [{-1, R2}]),
    Three=spawn(ca, two, [{-1, R3}]),
    timer:sleep(1000),
    {ok, Tr}=timer:send_interval(1, next),
    ca({0, #rnd_ca{one=Sender,two=Two, three=Three, tref=Tr, pid=Pid}});
ca({0, My_rec}) ->
    #rnd_ca{one=Sender,two=Two, three=Three, tref=Tr, pid=Pid}=My_rec,   
receive
    noconn ->
        timer:cancel(Tr),
        Sender!stop,
        Two!stop,
        Three!stop,
        Pid!noconn;
    next ->
        T=os:system_time(),
        Sender!gogo,
        Two!{gogo, T},
        Three!{gogo, T},
        ca({0, My_rec});
    stop ->
        timer:cancel(Tr),
        Sender!stop,
        Two!stop,
        Three!stop,
        exit(kill);
    _Any -> ca({0, My_rec})
end.

two({9, Moo_rec}) -> 
    receive
        stop -> exit(kill);
        _Data -> ok
    end,
    two({9, Moo_rec});
two({-1, Start_rec}) ->
    #worker{ip=Ip, port=Port, r_db=Rdb}=Start_rec,
    IpS=integer_to_list(element(1, Ip))++"."++integer_to_list(element(2, Ip))++"."++integer_to_list(element(3, Ip))++"."++integer_to_list(element(4, Ip)),
    {ok, Conn}=eredis:start_link(IpS, Port, Rdb),
    two({0, Start_rec#worker{conn=Conn}});
two({0, My_rec}) -> 
    #worker{interval=N, conn=Conn, r_list=Rlist, period=Period}=My_rec,
    receive
    {gogo, T} ->
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
                two({1, My_rec#worker{tme=T}});
            false ->
                Num=integer_to_list(rand:uniform(N)),
                {ok, _Res}=eredis:q(Conn, ["RPUSH", Rlist, Num]),
                two({0, My_rec})
        end;
    stop ->
        exit(Conn, kill),
        exit(kill);
    _Any -> two({0, My_rec})
    end;
two({1, My_rec}) ->
    #worker{interval=N, conn=Conn, r_list=Rlist, period=Period, tme=T}=My_rec,
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
            two({1, My_rec#worker{tme=T}});
        false ->
            Num=integer_to_list(rand:uniform(N)),
            {ok, _Res}=eredis:q(Conn, ["RPUSH", Rlist, Num]),
            two({0, My_rec})
    end.
            
sendme({9, Moo_rec}) ->
    receive
        stop -> exit(kill);
        _Data -> ok
    end,
    sendme({0, Moo_rec});
sendme({-1, Start_rec}) ->
    #worker{ip=Ip, port=Port, r_db=Rdb}=Start_rec,
    IpS=integer_to_list(element(1, Ip))++"."++integer_to_list(element(2, Ip))++"."++integer_to_list(element(3, Ip))++"."++integer_to_list(element(4, Ip)),
    {ok, Conn}=eredis:start_link(IpS, Port, Rdb),
    sendme({0, Start_rec#worker{conn=Conn}});
sendme({0, My_rec}) ->
    #worker{interval=N, conn=Conn, r_list=Rlist}=My_rec,
    receive
        gogo ->
            Num=integer_to_list(rand:uniform(N)),
            {ok, _Res}=eredis:q(Conn, ["RPUSH", Rlist, Num]),
            sendme({0, My_rec});
        stop ->
            exit(Conn, kill),
            exit(kill);
        _Any -> sendme({0, My_rec})
    end.


