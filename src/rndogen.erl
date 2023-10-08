%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. апр. 2023 20:58
%%%-------------------------------------------------------------------
-module(rndogen).
-author("mt").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -export([ca/5, one/4, two/5, three/5]).
-define(SERVER, ?MODULE).

-record(rnd_state, {ip, interval, port, r_db, r_list, r_set, ca, fa, go, handlers, mca, mfa, mgo, monitors}). 

%% Ip, Port, R_db, Interval, R_list, R_set  - external parameters

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Data) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Data], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #rnd_state{}} | {ok, State :: #rnd_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([{Ip, Port, R_db, Interval, R_list, R_set}]) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), afterinit),
  {ok, #rnd_state{ip=Ip, port=Port, r_db=R_db, r_list=R_list, r_set=R_set, interval=Interval}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #rnd_state{}) ->
  {reply, Reply :: term(), NewState :: #rnd_state{}} |
  {reply, Reply :: term(), NewState :: #rnd_state{}, timeout() | hibernate} |
  {noreply, NewState :: #rnd_state{}} |
  {noreply, NewState :: #rnd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #rnd_state{}} |
  {stop, Reason :: term(), NewState :: #rnd_state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages

handle_cast(afterinit, State) ->
	#rnd_state{ interval = N, ip=Ip, port=Port, r_db = Rdb, r_list = Rlist, r_set = Rset}=State,
	Params1=[Ip, Port, Rlist, Rset, Rdb, {0,0}],
	Params2=[Ip, Port, Rlist, Rset, Rdb, 0],
	FA=spawn(satel, fa, Params1),
	Pa=spawn(ca, gogo, Params2),
	Pb=spawn(ca, gogo, Params2),
	Pc=spawn(ca, gogo, Params2),
	Mfa=erlang:monitor(process, FA),
	Mgo=lists:map(fun(X) -> erlang:monitor(process, X) end, [Pa, Pb, Pc]),
	timer:sleep(3000),
	Rca={N, Pa, Pb, Pc},
	CA=spawn(ca, ca, [{init_, Rca}]),
	Idx=case length(integer_to_list(N)) <7 of
		true ->
			2; 
		false -> 
			4 
	end,
	Hs=spawn(satel, filter, [{{start, Idx}, FA}]),
	Ms=erlang:monitor(process, Hs),
	Mca=erlang:monitor(process, CA),
	{noreply, State#rnd_state{ca=CA, fa=FA, handlers=Hs, mca=Mca, mfa=Mfa, monitors=Ms, go=[Pa, Pb, Pc], mgo=Mgo}};

handle_cast(fadown, State) ->
	io:format("~nFA / CA / DOWN...~n"),
	top_sup:stop(),
	{noreply, State};

handle_cast(stop, State) ->
	#rnd_state{ca=CA, fa=FA, handlers = Hs, monitors=Mons, mca = Mca, mfa=Mfa, mgo=Mgo, go=GOGO}=State,
	erlang:demonitor(Mons, [flush]),
	erlang:demonitor(Mca, [flush]),
	erlang:demonitor(Mfa, [flush]),
	[erlang:demonitor(X, [flush])||X <- Mgo],
	CA!stop,
	exit(Hs, kill),
	FA!stop,
	[X!stop||X <- GOGO],
	{stop, normal, State#rnd_state{handlers=nop}};

handle_cast(_Any, State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #rnd_state{}) ->
  {noreply, NewState :: #rnd_state{}} |
  {noreply, NewState :: #rnd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #rnd_state{}}).

handle_info({'DOWN', Ref, prosess, _Pid, _Reason}, State) -> %% oh, use supervisors, Luke
	#rnd_state{monitors=Mons, mca = Mca,  mfa=Mfa,go=GOGO, mgo=Mgo, ip = Ip, port = Port, r_set = Rset, r_list=Rlist, fa=FA, interval = N, r_db = Rdb}=State,
	case Ref==Mons of
		true ->
			erlang:demonitor(Ref, [flush]),
			Idx=case length(integer_to_list(N)) <7 of
				true ->
					2; 
				false -> 
					4 
			end,
			Rsat={{start, Idx}, FA},
			NewPid=spawn(satel, filter, [Rsat]),
			NewMon=erlang:monitor(process, NewPid),
			{noreply, State#rnd_state{monitors=NewMon, handlers=NewPid}};
		false ->
			case Ref==Mca orelse lists:member(Ref, Mgo) of
				true ->
					erlang:demonitor(Mca, [flush]),
					[erlang:demonitor(X, [flush])||X <-Mgo],
					case Ref==Mca of
						false -> 
							F=fun(X) -> try exit(X, kill) catch _:_ -> ok end end,
							NewGo=[F(X)||X <- GOGO],
							{noreply, State#rnd_state{go=NewGo, mgo=[erlang:demonitor(X, [flush])||X <-NewGo]}};
						true -> 
							NewCA=spawn(ca, ca, [{init_, {N, GOGO, 0}}]),
							NewMca=erlang:monitor(process, NewCA),
							{noreply, State#rnd_state{ca=NewCA, mca=NewMca}}
						end;
				false ->
					case Ref=Mfa of
						true->
							erlang:demonitor(Ref, [flush]),
							Lfa=[Ip, Port, Rlist, Rset, Rdb, {0,0}],
							NewFA=spawn(satel, fa, Lfa),
							NewMfa=erlang:monitor(process, NewFA),
							{noreply, State#rnd_state{fa=NewFA, mfa=NewMfa}};
						false ->
							wtf,
							{noreply, State}
					end
			end
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #rnd_state{}) -> term()).
terminate(_Reason, State) ->
	#rnd_state{ca=CA, fa=FA, handlers = Hs, monitors=Mons, mca = Mca, mgo=Mgo, go=GOGO}=State,
	case is_pid(Hs) of
		true ->
			erlang:demonitor(Mons, [flush]),
			erlang:demonitor(Mca, [flush]),
			[erlang:demonitor(X, [flush])||X <- Mgo],
			CA!stop,
			Hs!stop,
			FA!stop,
			[X!stop||X <- GOGO];
		false -> ok
	end.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #rnd_state{},
    Extra :: term()) ->
  {ok, NewState :: #rnd_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #rnd_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
			


