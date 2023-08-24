-module(uctest_SUITE).
-author("mt").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, check_interval}]. %%  {group, on_start}, 

groups() ->
    [
        %% {on_start, [], [no_data, wrong_data]},
        {check_interval, [], [gogo_speed, gogo9m]}
    ].

init_per_suite(Config) ->
    application:start(sasl),
    {ok, Name}=inet:gethostname(),
    {ok, Med}=inet:gethostbyname(Name, inet),
    [Addr]=element(tuple_size(Med), Med),
    Haddr={haddr, Addr},
    {A, B, C, D}=Addr,
    Priv="/home/mt/prime-filter/priv",
    Saddr={saddr, integer_to_list(A)++"."++integer_to_list(B)++"."++integer_to_list(C)++"."++integer_to_list(D)},
    Priv_dir={priv, Priv}, %% no last slash
    Conf={conf, Priv++"/conn.conf"},
    {ok, Bin}=file:read_file(Priv++"/conn.conf"),
    Data=binary_to_list(Bin),
    {match, [{Bg, Ln}]}=re:run(Data, "port [0-9]+"),
    {match, [{BgN, LnN}]}=re:run(lists:sublist(Data, Bg+1, Ln), "[0-9]+"),
    Port={port, list_to_integer(lists:sublist(Data, Bg+BgN+1, LnN))}, 
    [Haddr, Saddr, Priv_dir, Port, Conf|Config].

init_per_group(on_start, Config) ->
    filelib:is_file(?config(conf, Config)) andalso file:delete(?config(conf, Config)),
    file:write_file(?config(conf, Config), "ip "++?config(saddr, Config) ++"\nport "++integer_to_list(?config(port, Config))++"interval 9000000\nredis_db 0\nredis_list my_list\nredis_set my_set\n"),
    ct:sleep(500),
    Config;
init_per_group(check_interval, Config) ->
    ct:sleep(500),
    Config.

end_per_group(on_start, Config) ->
    application:stop(prime_filter),
    file:delete(?config(conf, Config)),
    file:write_file(?config(conf, Config), "ip "++?config(saddr, Config) ++"\nport "++integer_to_list(?config(port, Config))++"\ninterval 9000\nredis_db 0\nredis_list my_list\nredis_set my_set\n"),
    Config;
end_per_group(check_interval, Config) ->
    Config.

end_per_suite(Config) ->
    application:stop(sasl),
    Config.

no_data(Config) ->
    filelib:is_file(?config(conf, Config)) andalso file:delete(?config(conf, Config)),
    F=application:start(prime_filter),
    application:stop(prime_filter),
    ct:sleep(1000),
    ?assert(F/=ok),
    Config.

wrong_data(Config) ->
    filelib:is_file(?config(conf, Config)) andalso file:write_file(?config(conf, Config), "\ninterval 9000\nredis_db 0\nredis_list my_list\nredis_set my_set\n"),
    F=application:start(prime_filter),
    application:stop(prime_filter),
    ?assert(F/=ok),
    ct:sleep(1000),
    Config.

gogo_speed(Config) ->
    filelib:is_file(?config(conf, Config)) andalso file:delete(?config(conf, Config)),
    file:write_file(?config(conf, Config), "ip "++?config(saddr, Config) ++"\nport "++integer_to_list(?config(port, Config)+2)++"\ninterval 9000\nredis_db 0\nredis_list my_list\nredis_set my_set\n"),
    Pid=spawn(uctest_SUITE, moo_lite_server, [?config(port, Config)+2, 0, 0, 0, self(), 0]),
    application:start(prime_filter),
    receive
        Data -> Data
    end,
    application:stop(prime_filter),
    ct:sleep(5000),
    exit(Pid, kill),
    [{speed, Data}|Config].

gogo9m(Config) ->
    filelib:is_file(?config(conf, Config)) andalso file:delete(?config(conf, Config)),
    file:write_file(?config(conf, Config), "ip "++?config(saddr, Config) ++"\nport "++integer_to_list(?config(port, Config)+3)++"\ninterval 9000000\nredis_db 0\nredis_list my_list\nredis_set my_set\n"),
    Pid=spawn(uctest_SUITE, moo_server, [?config(port, Config)+3, self()]),
    application:start(prime_filter),
    receive
        Data -> Data
    end,
    application:stop(prime_filter),
    ct:sleep(5000),
    exit(Pid, kill),
    [{m9, Data}|Config].

moo_server(Port, Pid) ->
    Ret=["7000003", "1", "6000000", "1046529", "1358027", "5000000", "1604941", "3000017", "2240", "9001", "5678903", "2064919", "2510537", "4024021", "8991989"], 
    moo(Port, Ret, 0, 0, 1, Pid). %% is_started( -1 true ) retlist 1check 2check 3check lsock num(1/2/3) cnt retcnt pid

moo(_, _, Lsock, 600, _, Pid) -> 
    gen_tcp:close(Lsock),
    Pid!stopped;
moo(-1, Ret, Lsock, Cnt, RetCnt, Pid) ->
    {ok, Sock}=gen_tcp:accept(Lsock),
    receive
        Data -> Data
    end,
    case Data of
        {tcp, _, Msg} -> %% RPUSH LPOP SADD
            Rpush=re:run(Msg, "RPUSH"),
            Lpop=re:run(Msg, "LPOP"),
            Sadd=re:run(Msg, "SADD"),
            if
                Rpush/=nomatch -> 
                    gen_tcp:send(Sock, list_to_binary(":1\r\n")),
                    gen_tcp:close(Sock),
                    moo(-1, Ret, Lsock, Cnt, RetCnt, Pid);
                Lpop/=nomatch ->
                    N=lists:nth(RetCnt, Ret),
                    gen_tcp:send(Sock, list_to_binary("$"++integer_to_list(length(N))++"\r\n"++N++"\r\n")),
                    gen_tcp:close(Sock),
                    Newcnt=case length(Ret) - RetCnt of
                        0 -> 1;
                        _AnyN -> RetCnt+1
                    end,
                    moo(-1, Ret, Lsock, Cnt+1, Newcnt, Pid);
                Sadd/=nomatch ->
                    gen_tcp:send(Sock, list_to_binary(":1\r\n")),
                    gen_tcp:close(Sock),
                    moo(-1, Ret, Lsock, Cnt, RetCnt, Pid);
                true ->
                    gen_tcp:close(Sock),
                    moo(-1, Ret, Lsock, Cnt, RetCnt, Pid)
            end;
        _Else ->
            gen_tcp:close(Sock),
            moo(-1, Ret, Lsock, Cnt, RetCnt, Pid)
    end;   
moo(Port, Ret, _, _, _, Pid) ->
    {ok, Lsock}=gen_tcp:listen(Port, [list, {active, true}]),
    moo(-1, Ret, Lsock, 0, 1, Pid).

moo_lite_server(-1, Lsock, 180000, Acc, Pid, _) ->
    gen_tcp:close(Lsock),
    Pid!{result, lists:min(Acc), lists:max(Acc), lists:sum(Acc) div 180000};
moo_lite_server(-1, Lsock, Cnt, Acc, Pid, Last) ->
    {ok, Sock}=gen_tcp:accept(Lsock),
    receive 
        Data -> Data
    end,
    case Data of
        {tcp, _, Msg} ->
            Rpush=re:run(Msg, "RPUSH"),
            Lpop=re:run(Msg, "LPOP"),
            Sadd=re:run(Msg, "SADD"),
            if
                Rpush/=nomatch ->
                    gen_tcp:send(Sock, list_to_binary(":1\r\n")),
                    gen_tcp:close(Sock),
                    Z=os:system_time(),
                    moo_lite_server(-1, Lsock, Cnt+1, [(Z-Last)|Acc], Pid, Z);
                Lpop/=nomatch ->
                    gen_tcp:send(Sock, list_to_binary("$7\r\n1234567\r\n")),
                    gen_tcp:close(Sock),
                    moo_lite_server(-1, Lsock, Cnt, Acc, Pid, Last);
                Sadd/=nomatch ->
                    gen_tcp:send(Sock, list_to_binary(":1\r\n")),
                    gen_tcp:close(Sock),
                    moo_lite_server(-1, Lsock, Cnt, Acc, Pid, Last);
                true -> moo_lite_server(-1, Lsock, Cnt, Acc, Pid, Last)
            end;
        _Any -> gen_tcp:close(Sock), moo_lite_server(-1, Lsock, Cnt, Acc, Pid, Last)
    end;
moo_lite_server(Port, _, _,  _, Pid, _) ->
    {ok, Lsock}=gen_tcp:listen(Port, [list, {active, true}]),
    moo_lite_server(-1, Lsock, 0, [], Pid, os:system_time()).




            