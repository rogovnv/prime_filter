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

-record(rnd_state, {ip, interval, port, r_db, r_list, r_set, tref, ca, h1, h2, h3, h4, h5, h6}). 

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
	#rnd_state{ip=Ip, port=Port, r_db = Rdb, r_list = Rlist, r_set = Rset, interval = N}=State,
	RN=[13, 10],
	{ok, Sock}=gen_tcp:connect(Ip, Port, [list, {active, true}], 3),
	X="*2"++RN++"$6"++RN++"SELECT"++RN++"$"++integer_to_list(length(Rdb))++RN++Rdb++RN,
	gen_tcp:send(Sock, list_to_binary(X)),
	receive
		_Data ->
			ok
	end,
	gen_tcp:close(Sock),
	L=satel:erat(),
	CA=spawn(ca, ca, [-1, N, Ip, Port, Rlist, 0, 0]),
	H1=spawn(satel, filter, [Ip, Port, Rset, Rlist, L]), 
	H2=spawn(satel, filter, [Ip, Port, Rset, Rlist, L]),
	H3=spawn(satel, filter, [Ip, Port, Rset, Rlist, L]),
	H4=spawn(satel, filter, [Ip, Port, Rset, Rlist, L]),
	%% H5=spawn(satel, filter, [Ip, Port, Rset, Rlist, L]),
	%% H6=spawn(satel, filter, [Ip, Port, Rset, Rlist, L]),
    %% FA=spawn(satel, fa, [Ip, Port, Rlist, Nman, -1]),
	{noreply, State#rnd_state{ca=CA, h1=H1, h2=H2, h3=H3, h4=H4,h5=nop, h6=nop}};

handle_cast({tref, Tr}, State) ->
	Old=State#rnd_state.tref,
	{noreply, State#rnd_state{tref = [Tr|Old]}};

handle_cast(stop, State) ->
	#rnd_state{tref = Tr, ca=CA, h1=H1, h2=H2, h3=H3, h4=H4}=State,
	timer:cancel(Tr),
	CA!stop,
	exit(H1, kill),
	exit(H2, kill),
	exit(H3, kill),
	exit(H4, kill),
	%% exit(H5, kill),
	%% exit(H6, kill),
	{stop, normal, State#rnd_state{h1=nop}};

handle_cast(_Any, State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #rnd_state{}) ->
  {noreply, NewState :: #rnd_state{}} |
  {noreply, NewState :: #rnd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #rnd_state{}}).
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
	#rnd_state{tref = Tr, ca=CA,h1=H1, h2=H2, h3=H3, h4=H4}=State,
	case is_pid(H1) of
		true ->
			timer:cancel(Tr),
			CA!stop,
			exit(H1, kill),
			exit(H2, kill),
			exit(H3, kill),
			exit(H4, kill);
			%% exit(H5, kill),
			%% exit(H6, kill);
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

