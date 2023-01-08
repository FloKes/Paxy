-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(acc_node, 'paxy-acc@127.0.0.1').
-define(prop_node, 'paxy-prop@127.0.0.1').

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  spawn(?acc_node, fun() -> start_remote_acceptors() end),
  spawn(?prop_node, fun() -> start_remote_proposers(Sleep) end).
  
  
start_remote_proposers(Sleep) ->
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
  AccRegisterRemote = [{a, ?acc_node}, {b, ?acc_node}, {c, ?acc_node}, {d, ?acc_node}, {e, ?acc_node}],
  {gui, ?acc_node} ! {reqState, self()},
  receive
	{reqState, State} ->
		{_, PropIds} = State,
		start_proposers(PropIds, PropInfo, AccRegisterRemote, Sleep, self()),
		wait_proposers(length(PropIds))
  end.
  
start_remote_acceptors() ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e"],
  AccRegister = [a, b, c, d, e],
  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
	{reqState, State} ->
		{AccIds, _} = State,
		start_acceptors(AccIds, AccRegister)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      % // TODO this and that
	    io:format("Registered an acceptor with name ~w and id ~w ~n",[RegName,AccId]),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
    {Name, ?acc_node} ! stop,
      ok;
    Pid ->
      Pid ! stop
  end.

 
