%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. апр. 2023 20:56
%%%-------------------------------------------------------------------
-module(top_sup).
-author("mt").

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(any()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Data) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Data]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([{Ip, Port, R_db, Interval, R_list, R_set}]) ->
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild = #{id => rndogen,
    start => {rndogen, start_link, [{Ip, Port, R_db, Interval, R_list, R_set}]},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => []},

  {ok, {SupFlags, [AChild]}}.

stop() ->
  gen_server:cast(rndogen, stop),
  timer:sleep(30000),
  init:stop().

%%%===================================================================
%%% Internal functions
%%%===================================================================
