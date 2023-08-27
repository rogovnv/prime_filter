%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. апр. 2023 20:55
%%%-------------------------------------------------------------------
-module(prime_filter).
-author("mt").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  %% Path= code:priv_dir(prime_filter),
  PathP="/home/mt/prime-filter/priv"++"/conn.conf",
  F=file:read_file(PathP),
  Fn=fun(X) ->
    Y=string:split(X, " "),
    case Y of
    [A, B] ->
      {A, re:replace(B, "[\s\n\r\t]+", "", [global, {return, list}])};
    [_Alone] ->
      {"nop", 2}
    end
  end,
  case F of
    {ok, Bin} ->
      Data=binary:bin_to_list(Bin),
      L=string:split(Data, "\n", all),
      Params=lists:map(Fn, L),
      Ip=case proplists:get_value("ip", Params, nop) of
        nop -> nop;
        Str ->
            case string:split(Str, ".", all) of
              [A, B, C, D] -> {list_to_integer(A), list_to_integer(B), list_to_integer(C), list_to_integer(D)};
              _Not_full -> nop
            end
          end,
      Port=case proplists:get_value("port", Params, nop) of
        nop -> nop;
        Str2 ->
            list_to_integer(Str2)
        end,
      R_db=case proplists:get_value("redis_db", Params, nop) of
        nop -> nop;
        Str4 ->
          list_to_integer(Str4)
        end,
      Interval= case proplists:get_value("interval", Params, nop) of
        nop -> nop;
        Str3 ->
            list_to_integer(Str3)
        end,
      R_list=proplists:get_value("redis_list", Params, nop),
      R_set=proplists:get_value("redis_set", Params, nop),
      Bool=Ip/=nop andalso Port/=nop andalso R_db/=nop andalso Interval/=nop andalso R_list/=nop andalso R_set/=nop,
      case Bool of
        true ->
          case top_sup:start_link({Ip, Port, R_db, Interval, R_list, R_set}) of
            {ok, Pid} ->
              {ok, Pid};
            Error ->
              io:format("~nError ~p", [Error])
          end;
        false ->
          io:format("~nWrong params in /priv/conn.conf"),
          exit(self(),kill)
        end;
    Other ->
      io:format("~nNo params in /priv : ~p", [Other]),
      exit(self(),kill)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
