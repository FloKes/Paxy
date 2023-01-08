-module(acceptor).
-export([start/2]).


-define(delay, 4000).
-define(drop, -1).

  
% paxy:start([1000, 3000, 2000]).
start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  case PanelId of 
    na ->
      {Pro, Vot, Val, Pan} = read_state(Name),
      io:format("--- Acceptor ~w Recovering from crash --- Promised: ~w, Voted: ~w, Value: ~w,  PID:~w~n",
         [Name, Pro, Vot, Val, Pan]),

      % Update gui
      Colour = case Val of na -> {0,0,0}; _ -> Val end,
      Pan ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Vot]), 
                  "Promised: " ++ io_lib:format("~p", [Pro]), Colour},
      acceptor(Name, Pro, Vot, Val, Pan);
    _ ->
      Promised = order:null(), 
      Voted = order:null(),
      Value = na,
      save_state(Name, Promised, Voted, Value, PanelId),
      acceptor(Name, Promised, Voted, Value, PanelId)
  end.
  

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          P = rand:uniform(10),
          if P =< ?drop ->
              io:format("message dropped~n");
            true ->
              Message = {promise, Round, Voted, Value},
              % basic
              % Proposer ! Message,               

              % With delay
              T = rand:uniform(?delay),
              timer:send_after(T, Proposer, Message)
          end,

          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          save_state(Name, Round, Voted, Value, PanelId),
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          P = rand:uniform(10),
          if P =< ?drop ->
              io:format("message dropped~n");
            true ->
              Message = {vote, Round},
              % basic
              %Proposer ! Message,

              % delay
              T = rand:uniform(?delay),
              timer:send_after(T, Proposer, Message)    
          end,
          
          case order:goe(Round, Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              save_state(Name, Promised, Round, Proposal, PanelId),
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      pers:delete(Name),
      ok
  end.

save_state(Name, Promised, Voted, Value, PanelId) ->
  pers:open(Name),
  pers:store(Name, Promised, Voted, Value, PanelId),
  pers:close(Name).

read_state(Name) ->
  pers:open(Name),
  {Promised, Voted, Value, PanelId} = pers:read(Name),
  pers:close(Name),
  {Promised, Voted, Value, PanelId}.