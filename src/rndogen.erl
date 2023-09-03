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

-record(rnd_state, {ip, interval, port, r_db, r_list, r_set, ca, fa, noconncnt, handlers, mca, monitors}). 

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
  {ok, #rnd_state{ip=Ip, port=Port, r_db=R_db, r_list=R_list, r_set=R_set, interval=Interval, noconncnt=0}}.

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
	%% RN=[13, 10],
	IpS=integer_to_list(element(1, Ip))++"."++integer_to_list(element(2, Ip))++"."++integer_to_list(element(3, Ip))++"."++integer_to_list(element(4, Ip)),
	case eredis:start_link(IpS, Port, Rdb) of
		{ok, FA} ->
			L=satel:erat(),
			%% FA=spawn(satel, fa, [Ip, Port, Rlist]),
			Rca={N, Ip, Port, Rlist, Rdb, 0},
			CA=spawn(ca, ca, [{-1, Rca}]),
			Rsat={Ip, Port, Rset, L, FA, Rlist},
			%% Idx=case length(integer_to_list(N)) <7 of
			%% 	true ->
			%% 		3;
			%% 	false -> 
			%% 		5
			%%	end,
			Hs=lists:map(fun(_X) -> spawn(satel, filter, [Rsat]) end, lists:seq(1, 3)),
			Ms=[erlang:monitor(process, X)||X <- Hs],
			Mca=erlang:monitor(process, CA),
			{noreply, State#rnd_state{ca=CA, fa=FA, handlers=Hs, mca=Mca, monitors=Ms}};
		_Error ->
			io:format("~nNo connection to DB.~n"),
			top_sup:stop(),
			{noreply, State}
		end;

handle_cast(noconn, #rnd_state{noconncnt = N}=State) ->
	N==1 andalso top_sup:stop(),
	{noreply, State#rnd_state{noconncnt=N+1}};

handle_cast(stop, State) ->
	#rnd_state{ca=CA, fa=FA, handlers = Hs, monitors=Mons, mca = Mca}=State,
	[erlang:demonitor(X)||X<-Mons],
	erlang:demonitor(Mca),
	CA!stop,
	[exit(X)||X <-Hs],
	exit(FA, kill),
	{stop, normal, State#rnd_state{handlers=nop}};

handle_cast(_Any, State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #rnd_state{}) ->
  {noreply, NewState :: #rnd_state{}} |
  {noreply, NewState :: #rnd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #rnd_state{}}).


handle_info({'DOWN', Ref, prosess, Pid, _Reason}, State) ->
	#rnd_state{handlers = Hs, monitors=Mons, mca = Mca, ip = Ip, port = Port, r_set = Rset, r_list=Rlist, fa=FA, interval = N, r_db = Rdb}=State,
	case lists:member(Ref, Mons) of
		true ->
			erlang:demonitor(Ref),
			L=satel:erat(),
			Rsat={Ip, Port, Rset, L, FA, Rlist},
			NewPid=spawn(satel, filter, [Rsat]),
			NewMon=erlang:monitor(process, NewPid),
			MedP=lists:delete(Ref, Mons),
			MedM=lists:delete(Pid, Hs),
			{noreply, State#rnd_state{monitors=[NewMon|MedM], handlers=[NewPid|MedP]}};
		false ->
			case Ref==Mca of
				true ->
					erlang:demonitor(Ref),
					Rca={N, Ip, Port, Rlist, Rdb},
					NewCA=spawn(ca, ca, [{-1, Rca}]),
					NewMca=erlang:monitor(process, NewCA),
					{noreply, State#rnd_state{ca=NewCA, mca=NewMca}};
				false ->
					wtf,
					{noreply, State}
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
	#rnd_state{ca=CA, fa=FA, handlers = Hs, monitors=Mons, mca = Mca}=State,
	case is_list(Hs) of
		true ->
			[erlang:demonitor(X, [flush])||X<-Mons],
			erlang:demonitor(Mca, [flush]),
			CA!stop,
			[exit(X)||X <-Hs],
			exit(FA, kill);
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

