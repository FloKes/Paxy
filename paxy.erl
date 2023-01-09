-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {255,255,0}).
-define(MAGENTA, {255,0,255}).
-define(PURPLE, {159, 43, 104}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  % ------ OLD------
  % AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
  %                  "Acceptor e"],
  % AccRegister = [a, b, c, d, e],

  %   % ------ NEW max ------
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e", "Acceptor f", "Acceptor g", "Acceptor h"],
  AccRegister = [a, b, c, d, e, f, g, h],

  %% ------ NEW ------
  % AcceptorNames = ["Acceptor a"],
  % AccRegister = [a],

  % ------ OLD ------
  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],

  % ------ NEW ------
  % ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
  %                  {"Proposer willard", ?BLUE}, {"Proposer  florian", ?YELLOW}, {"Proposer kesten", ?MAGENTA}, 
  %                  {"Proposer stjepan", ?PURPLE}, {"Proposer marco", ?PURPLE}, {"Proposer gordon", ?PURPLE}],
  % PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {florian, ?YELLOW}, {kesten, ?MAGENTA},
  %          {stjepan, ?PURPLE}, {marco, ?PURPLE}, {gordon, ?PURPLE}],
  

  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
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
  % NEW
  stop(f),
  stop(g),
  stop(h),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.

 
