%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013 
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : [ADD HERE]
%%% KU-ID   : [ADD HERE]
%%%---------------------------------------------------------------------
%%% Student : [ADD HERE]
%%% KU-ID   : [ADD HERE]
%%%---------------------------------------------------------------------

-module(swarm).
-export([run/2, newFish/2,attraRepul/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run(Bound, Limit) -> 
    {Qtree, FishCoordinator} = fishInit(Bound, Limit),
    {Qtree, FishCoordinator}.

newFish(FishCoordinator, Pos) ->
    rpc(FishCoordinator, {create_fish, Pos}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fishInit(Bound, Limit) ->
    {ok, Qtree} = quadtree:start(Bound, Limit),
    Loop = spawn(fun() -> fishLoop(Qtree) end),    
    FishCoordinator = spawn(fun() -> fishCoordinator(0, Qtree, Loop) end),
    {Qtree, FishCoordinator}.
    
fishCoordinator(NextID, Qtree, Loop) -> 
    receive
      {From, {create_fish, Pos}} ->
        quadtree:addElement(Qtree, Pos, {NextID, {0,0},{0,0},0}),
        reply_ok(From),
        fishCoordinator(NextID + 1, Qtree, Loop);
       A -> erlang:display(A),
            fishCoordinator(NextID, Qtree, Loop)
    end.

fishLoop(Qtree) -> 
    timer:sleep(50),
    %moveFishes(Qtree),
    %attraRepul(Qtree),
    fishLoop(Qtree).
    
moveFishes(Qtree) ->
    MoveFish = 
        fun(Fish) ->
            moveFish(Fish)
        end,
    quadtree:mapTreeFunction(Qtree, MoveFish).

attraRepul(Qtree) ->
    Iterator = 
        fun(Fish1) -> 
            {_,{X,Y},{ID1,_,_,_}} = Fish1   , 
            Test = 
                fun(Fish2) ->
                    {_,_,{ID2,_,_,_}} = Fish2,
                    Distance = distanceFish(Fish1,Fish2),
                    if ID1 == ID2 -> %Same fish
                        Fish2;
                    Distance =< 3 ->
                        Repul = calculateRepulsion(Fish2,Fish1),
                        Fish2;
                    true -> 
                        Fish2
                    end
                end,
            {_,{X,Y},_} = Fish1   ,       
            quadtree:mapFunction(Qtree,Test,{X-10,Y-10,X+10,Y+10}),
            Fish1
    end,
    quadtree:mapFunction(Qtree,Iterator,universe).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% synchronous communication
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
    {Pid, Response} ->
        Response
    end.
    
reply(From, Msg) ->
    From ! {self(), Msg}.    
   
reply_ok(From) ->
    reply(From, ok).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculates the attraction velocity between the fish at the centre and
% a fish that is expected to be in the attraction zone.
% There is no check for attraction zone in this function.
updateChangeVector(Fish, VelocityCV) ->
    {element, Pos, Prop} = Fish,
    {ID, Velocity, FishVCV, Nd} = Prop,
    {element, Pos, {ID, Velocity, addVectors(FishVCV, VelocityCV), Nd + 1}}.

% Calculates the attraction velocity between the fish at the centre and
% a fish that is expected to be in the attraction zone.
% There is no check for attraction zone in this function.
calculateAttraction(FishCentre, FishInAttractionZone) ->
    {element, PosCF, _} = FishCentre,
    {element, PosAF, _} = FishInAttractionZone,
    toUnitVector(subtractVectors(PosCF, PosAF)).

% Calculates the repulsion velocity between the fish at the centre and
% a fish that is expected to be in the repulsion zone.
% There is no check for repulsion zone in this function.
calculateRepulsion(FishCentre, FishInRepulsionZone) ->
    {element, PosCF, _} = FishCentre,
    {element, PosRF, _} = FishInRepulsionZone,
    toUnitVector(subtractVectors(PosRF, PosCF)).

% Updates the velocity vector of a fish with the velocity difference vector.
% The moves the fish it according to the new velocity vector,
% and finally returns the updated fish with the reset difference vector.
% Maximum speed is 3.
moveFish(Fish) ->
    {element, Pos, Prop} = Fish,
    {ID, {Vx, Vy}, {Dvx, Dvy}, Nd} = Prop,
    NewV = toMaxLenVector({Vx + Dvx/Nd, Vy + Dvy/Nd},3),
    {element, addVectors(Pos, NewV), {ID, NewV, {0,0}, 0}}.

% Returns the distance between two fish
distanceFish(Fish1, Fish2) ->
    {element, Pos1, _} = Fish1,
    {element, Pos2, _} = Fish2,
    distance(Pos1, Pos2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper vector-functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adds two vectors and returns their sum
addVectors({X1,Y1},{X2,Y2}) ->
    {X1+X2, Y1+Y2}.

% Adds two vectors and returns their sum
subtractVectors({X1,Y1},{X2,Y2}) ->
    {X1-X2, Y1-Y2}.

% Transforms a vector to a unit length (vector of length = 1)
toUnitVector(Vector) ->
    Len = vectorLength(Vector),
    {X,Y} = Vector,
    {X/Len, Y/Len}.

% Returns the length of a vector
vectorLength(Vector) ->
    {X,Y} = Vector,
    math:sqrt(math:pow(X,2)+math:pow(Y,2)).

% Returns a vector of maximum MaxLen length.
toMaxLenVector(Vector,MaxLen) ->
    Len = vectorLength(Vector),
    {X,Y} = Vector,
    case Len > MaxLen of
        true  -> {X*MaxLen/Len, Y*MaxLen/Len};
        false -> Vector
    end.

% Returns the distance between two positions
distance(Pos1, Pos2) ->
    {X1,Y1} = Pos1,
    {X2,Y2} = Pos2,
    math:sqrt(math:pow(X1-X2,2)+math:pow(Y1-Y2,2)).
