-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(acc_node, 'paxy-acc@127.0.0.1').
-define(prop_node, 'paxy-prop@127.0.0.1').

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e"],
  AccRegister = [a, b, c, d, e],
  AccRegisterRemote = [{a, ?acc_node}, {b, ?acc_node}, {c, ?acc_node}, {d, ?acc_node}, {e, ?acc_node}],

  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],

  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  io:format("Registered gui self:~w ~n",[self()]),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
        spawn(?acc_node, fun() -> start_acceptors(AccIds, AccRegister) end),
        % start_acceptors(AccIds, AccRegister),
        spawn(fun() -> 
            Begin = erlang:monotonic_time(),
            spawn(?prop_node, fun() -> start_proposers(PropIds, PropInfo, AccRegisterRemote, Sleep, self()) end),
            % start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
            wait_proposers(length(PropIds)),
            End = erlang:monotonic_time(),
            Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
            io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
        end)
  end.

    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),

	%   io:format("[Acceptor ~w] registered with Gui id: ~w, self:~w ~n",[RegName, AccId, self()]),
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
    %   io:format("[Proposer ~w] started with Gui id: ~w, self:~w ~n",[RegName, PropId, self()]),
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

 
