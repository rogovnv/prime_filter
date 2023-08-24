-module(ca).
-export([ca/7, sendme/4, two/6, three/6]).

ca(-1, N, Ip, Port, Rlist, _, _) -> %% rnd interval
    io:format("~n~p ~p ~p~n", [Ip, Port, Rlist]),
    Sender=spawn(ca, sendme, [N, Ip, Port, Rlist]),
    Two=spawn(ca, two, [0, N, Ip, Port, Rlist, 0]),
    Three=spawn(ca, three, [0, N, Ip, Port, Rlist, 0]),
    {ok, Tr}=timer:send_interval(1, next),
    gen_server:cast(rndogen, {tref, Tr}),
    ca(Sender,N, Ip, Port, Rlist, Two, Three);
ca(Sender,N, Ip, Port, Rlist, Two, Three) ->
receive
    next ->
        T=os:system_time(),
        Sender!gogo,
        Two!{gogo, T},
        Three!{gogo, T},
        ca(Sender,N, Ip, Port, Rlist, Two, Three);
    stop ->
        Sender!stop,
        Two!stop,
        Three!stop,
        exit(kill);
    _Any -> ca(Sender,N, Ip, Port, Rlist, Two, Three)
end.

two(0, N, Ip, Port, Rlist, _) -> 
    receive
    {gogo, T} ->
        Zero=os:system_time(),
        Diff=Zero-T,
        case Diff < 333000 of
            true ->
                two(1, N, Ip, Port, Rlist, T);
            false ->
                Num=integer_to_list(rand:uniform(N)),
            Msg="*3\r\n$5\r\nRPUSH\r\n$"++integer_to_list(length(Rlist))++"\r\n"++Rlist++"\r\n$"++integer_to_list(length(Num))++"\r\n"++Num++"\r\n",
            S=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
                case S of
                    {ok, Sock} ->
                        gen_tcp:send(Sock, Msg),
                        gen_tcp:close(Sock); 
                    _Any2 -> ok
                end,
                two(0,N, Ip, Port, Rlist, 0)
        end;
    stop -> exit(kill);
    _Any -> two(0, N, Ip, Port, Rlist, 0)
    end;
two(1, N, Ip, Port, Rlist, T) ->
    Zero=os:system_time(),
    Diff=Zero-T,
    case Diff < 333000 of
        true ->
            two(1, N, Ip, Port, Rlist, T);
        false ->
            Num=integer_to_list(rand:uniform(N)),
            Msg="*3\r\n$5\r\nRPUSH\r\n$"++integer_to_list(length(Rlist))++"\r\n"++Rlist++"\r\n$"++integer_to_list(length(Num))++"\r\n"++Num++"\r\n",
            S=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
                case S of
                    {ok, Sock} ->
                        gen_tcp:send(Sock, Msg),
                        gen_tcp:close(Sock); 
                    _Any2 -> ok
                end,
            two(0, N, Ip, Port, Rlist, 0)
    end.

three(0, N, Ip, Port, Rlist, _) -> 
    receive
    {gogo, T} ->
        Zero=os:system_time(),
        Diff=Zero-T,
        case Diff < 666000 of
            true ->
                three(1, N, Ip, Port, Rlist, T);
            false ->
                Num=integer_to_list(rand:uniform(N)),
            Msg="*3\r\n$5\r\nRPUSH\r\n$"++integer_to_list(length(Rlist))++"\r\n"++Rlist++"\r\n$"++integer_to_list(length(Num))++"\r\n"++Num++"\r\n",
            S=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
                case S of
                    {ok, Sock} ->
                        gen_tcp:send(Sock, list_to_binary(Msg)),
                        gen_tcp:close(Sock); 
                    _Any2 -> ok
                end,
                three(0, N, Ip, Port, Rlist, 0)
        end;
    stop -> exit(kill);
    _Any -> three(0, N, Ip, Port, Rlist, 0)
    end;
three(1, N, Ip, Port, Rlist, T) ->
    Zero=os:system_time(),
    Diff=Zero-T,
    case Diff < 666000 of
        true ->
            three(1, N, Ip, Port, Rlist, T);
        false ->
            Num=integer_to_list(rand:uniform(N)),
            Msg="*3\r\n$5\r\nRPUSH$"++integer_to_list(length(Rlist))++"\r\n"++Rlist++"\r\n$"++integer_to_list(length(Num))++"\r\n"++Num++"\r\n",
            S=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
                case S of
                    {ok, Sock} ->
                        gen_tcp:send(Sock, list_to_binary(Msg)),
                        receive
                            _D -> ok
                        end,                       
                        gen_tcp:close(Sock); 
                    _Any2 -> ok
                end,
            three(0, N, Ip, Port, Rlist, 0)
    end.

sendme(N, Ip, Port, Rlist) ->
    receive
        gogo ->
            Num=integer_to_list(rand:uniform(N)),
            Msg="*3\r\n$5\r\nRPUSH$"++integer_to_list(length(Rlist))++"\r\n"++Rlist++"\r\n$"++integer_to_list(length(Num))++"\r\n"++Num++"\r\n",
            S=gen_tcp:connect(Ip, Port, [list, {active, true}], 1),
                case S of
                    {ok, Sock} ->
                        gen_tcp:send(Sock, list_to_binary(Msg)),
                        gen_tcp:close(Sock); 
                    _Any2 -> ok
                end,
                sendme(N, Ip, Port, Rlist);
        stop ->
            exit(kill);
        _Any -> sendme(N, Ip, Port, Rlist)
    end. 




