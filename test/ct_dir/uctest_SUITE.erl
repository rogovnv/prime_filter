-module(uctest_SUITE).
-author("mt").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
%% -include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, check_interval}]. 

groups() ->
    [
        {check_interval, [], [p1, p2]}
    ].

init_per_suite(Config) ->
    Config.

init_per_group(check_interval, Config) ->
    Config.

end_per_group(check_interval, Config) ->
    ok,
    Config.

end_per_suite(Config) ->
    ok,
    Config.

different(List, Acc) -> 
    case length(List) of
        0 -> List;
        1 -> Acc;
        _Any ->
            A=hd(List),
            B=hd(tl(List)),
            different(tl(List), [A-B|Acc])
    end.

p1(Config) -> %% test satel:handler, the primes's filter
    L=satel:erat(),
    Num=[5678903, 2064919, 2510537, 4024021, 1046529, 1358027, 5000000, 1604941, 3000017, 2240, 9001, 2089873, 1585697, 30151081, 2964171, 2525],
    Pid1=spawn(uctest_SUITE, test_h, [Num, [], self()]),
    Pid2=spawn(satel, handler, [L, Pid1]),
    receive
        {prms, Prms} ->
            ct:pal("~nPrimes ~p",[Prms])
    end,
    exit(Pid2, kill),
    exit(Pid1, kill),
    Config.

test_h([], Prms, Host) ->
    Host!{prms, Prms};
test_h(Num, Prms, Host) ->
    receive
        {gimme, Pid} ->
            Pid!{takeit, integer_to_list(hd(Num))},
            test_h(tl(Num), Prms, Host);
        {sendprime, N} ->
            test_h(Num, [N|Prms], Host)
    end.

p2(Config) -> %% test ca:ca, the number's generator
    Pid1=spawn(uctest_SUITE, iamserver, [self(), 1000, []]),
    Pid2=spawn(uctest_SUITE, iamserver, [self(), 1000, []]),
    Pid3=spawn(uctest_SUITE, iamserver, [self(), 1000, []]),
    ct:sleep(300),
    CA=spawn(ca, ca, [{init_, {900000000000, Pid1, Pid2, Pid3}}]), 
    ct:print("~np2 ~p", [CA]),
 %% if ca:ca will foll down, test will crashes koz rndogen is not registered 
    D1=receive
        {data, Data1} -> Data1
        after 3000 -> []
    end,
    D2=receive
        {data, Data2} -> Data2
        after 3000 -> []
    end,
    D3=receive
        {data, Data3} -> Data3
        after 3000 -> []
    end,
    CA!stop,
    exit(Pid1, kill),
    exit(Pid2, kill),
    exit(Pid3, kill),
    Full=lists:reverse(lists:sort(lists:flatten([D1, D2, D3]))),
    ct:pal("~n~nOne-second diff~n~p~n", [different(Full, [])]),
    Config.
 
iamserver(Pid, N, Acc) ->
    receive
        {faster, _D, T} ->
            NewAcc=[T|Acc],
            case N==0 of
                true ->
                    Pid!{data, NewAcc};
                false ->
                    ok
            end,
            iamserver(Pid, N-1, NewAcc)
    end.

