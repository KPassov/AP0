%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013
%%% Updated : Aug 2013
%%% Usage   : Assignemnt for Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student : [ADD HERE]
%%% KU-ID   : [ADD HERE]
%%%---------------------------------------------------------------------
%%% Student : [ADD HERE]
%%% KU-ID   : [ADD HERE]
%%%---------------------------------------------------------------------
-module(quadtree).
-export([start/2, stop/1, addElement/3, mapFunction/3, mapTreeFunction/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Starts a quadtree with 
%   Bound: Eucledean size of tree
%   Limit: Maximum elements in each leaf
start(Bound, Limit) ->
    {ok, spawn(fun() -> quadtreeCoordinatorInit(Bound, Limit) end)}.
% Stops a tree
stop(Qtree) -> 
    send_stop(Qtree).
% Adds and element 
addElement(Qtree, Pos, Property) ->
    send_add(Qtree, {element, Pos, Property}).
    
mapFunction(Qtree, MapFun, Bound) ->
    send_mapFun(Qtree, MapFun, Bound).
    
mapTreeFunction(Qtree, MapTreeFun) ->
    send_mapTree(Qtree, MapTreeFun).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Communication functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Asynchronous communication
info(Pid, Msg) ->
    Pid ! Msg.
    
send_add(Pid, Element) ->
    info(Pid, {addElement,Element}).
    
send_mapTree(Pid, MapTreeFun) ->
    info(Pid, {mapTreeFun, MapTreeFun}).
    
send_mapFun(Pid, MapFun, MapBound) ->
    info(Pid, {mapFun, {MapFun, MapBound}}).
    
send_stop(Pid) ->
    info(Pid, stop).
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
    
reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quadtree Coordinator
quadtreeCoordinatorInit(Bound, Limit) ->
    Top = spawn(fun () -> qtreeLeaf(Bound, Limit, self(), []) end), 
    quadtreeCoordinator(Top, Bound).
    
quadtreeCoordinator (QtreeTop, Bound) ->
    receive
        {addElement, {element, Pos, Prop}} ->
            info(QtreeTop, {addElement, {element, Pos, Prop}}),
            quadtreeCoordinator (QtreeTop, Bound);
        {mapTreeFun, MapTreeFun} -> 
            send_mapTree(QtreeTop, MapTreeFun),
            quadtreeCoordinator(QtreeTop,Bound);
        {mapFun, {MapFun, MapBound}} -> 
            send_mapFun(QtreeTop, MapFun, MapBound),
            quadtreeCoordinator(QtreeTop,Bound);
        stop ->
            send_stop(QtreeTop);
        A -> erlang:display({A,quadtreeCordinator_failed})
            
    end.
%% Quadtree node
qtreeNode(Bound, Limit, Parent, Children) ->
        receive
            {addElement, {element, Pos, Prop}} ->
                checkandsendBounds({element, Pos, Prop}, Children, Parent),
                qtreeNode(Bound, Limit, Parent, Children);
            {SenderPID, {Pos, inBound}} ->
                reply (SenderPID, not outOfBound(Bound, {element, Pos, 0})),
                qtreeNode(Bound, Limit, Parent, Children);
            {mapFun, {MapFun, MapBound}} -> 
                [C1,C2,C3,C4] = Children,
                {B1, B2, B3, B4} = {rpc(C1, returnBound),rpc(C2, returnBound),
                                    rpc(C3, returnBound),rpc(C4, returnBound)},
                sendIfIntersect(C1, MapFun, B1, MapBound),
                sendIfIntersect(C2, MapFun, B2, MapBound),
                sendIfIntersect(C3, MapFun, B3, MapBound),
                sendIfIntersect(C4, MapFun, B4, MapBound),
                qtreeNode(Bound, Limit, Parent, Children);
            {SenderPid, returnBound} ->
                reply(SenderPid, Bound),
                qtreeNode(Bound, Limit, Parent, Children);
            {mapTreeFun, MapTreeFun} ->
                [C1,C2,C3,C4] = Children,
                send_mapTree(C1, MapTreeFun),
                send_mapTree(C2, MapTreeFun),
                send_mapTree(C3, MapTreeFun),
                send_mapTree(C4, MapTreeFun),
                qtreeNode(Bound, Limit, Parent, Children);
            stop ->
                [C1,C2,C3,C4] = Children,
                send_stop(C1),
                send_stop(C2),
                send_stop(C3),
                send_stop(C4);
            A -> erlang:display(A,qtreeNode_failed)                
        end.
%% Quadtree leaf
qtreeLeaf(Bound, Limit, Parent, Data) ->
    receive
        {addElement, {element, Pos, Prop}} ->
            case {outOfBound(Bound, {element, Pos, Prop}), length(Data) < Limit} of 
                {false,true} -> qtreeLeaf(Bound, Limit, Parent, Data ++ [{element,Pos, Prop}]);
                {false,false}-> Children = spawnChildren(Bound, Limit, Parent, 
                                                         Data ++ [{element,Pos,Prop}]),
                                qtreeNode(Bound, Limit, Parent, Children);
                {true,_} ->     send_add(Parent, {element, Pos, Prop}),
                                qtreeLeaf(Bound, Limit, Parent, Data)
            end;
        {mapTreeFun, MapTreeFun} ->
            apply(MapTreeFun,[Bound]),
            qtreeLeaf(Bound, Limit, Parent, Data);
        {mapFun, {MapFun, MapBound}} -> 
            NewData = mapFunList(Data, MapBound, MapFun),
            qtreeLeaf(Bound, Limit, Parent, NewData);
        stop ->
            stopped_leaf;

        %helpers
        {SenderPid, {Pos, inBound}} ->
            reply (SenderPid, not outOfBound(Bound, {element, Pos, 0})),
            qtreeLeaf(Bound, Limit, Parent, Data);
        {SenderPid, returnBound} ->
            reply(SenderPid, Bound),
            qtreeLeaf(Bound, Limit, Parent, Data);
        
        A -> erlang:display(A,qtreeLeaf_failed)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following is suggestions for useful helpler function,
% which can make your loops easier to follow.
% Organization of quadtree:
% +---------+-- -> X
% | NW | NE |
% |----+----|
% | SW | SE |
% +---------+
% |
% v
% Y

%runs Fun on data if it is within Bound. Returns true if it evaluated
mapFunList([], Bound, Fun)   -> [];
mapFunList(Data, Bound, Fun) -> 
    Element = hd(Data),
    case outOfBound(Bound, Element) of
        false -> NewData = apply(Fun, [Element]),
                [NewData] ++ mapFunList(tl(Data),Bound,Fun);
        true  -> [Element] ++ mapFunList(tl(Data),Bound,Fun)
    end.
    
sendIfIntersect(Pid, MapFun, PidBound, MapBound) ->
    case intersectBounds(PidBound, MapBound) of
        empty -> no_intersect;
        A       -> send_mapFun(Pid, MapFun, MapBound)
    end.

    
% Helper function that checks what Quater Pos is in and adds it to that child, if no child is
% found it is send to the parrent
checkandsendBounds(Element, Children, Parent) -> 
    [C1,C2,C3,C4] = Children,
    {_, Pos, Prop} = Element,
    {C1B, C2B, C3B, C4B} = {rpc(C1,{Pos, inBound}), rpc(C2,{Pos, inBound}), 
                            rpc(C3,{Pos, inBound}), rpc(C4,{Pos, inBound})},
    if
        C1B -> send_add(C1, {element, Pos, Prop});
        C2B -> send_add(C2, {element, Pos, Prop});
        C3B -> send_add(C3, {element, Pos, Prop});
        C4B -> send_add(C4, {element, Pos, Prop});
        true -> send_add(Parent, {element, Pos, Prop})
    end.
    
% Returns a list of newly spawned leaf PID's
spawnChildren(Bound, Limit, Parent, Data) -> 
    C1 = spawn(fun() -> qtreeLeaf(createBound(nw, Bound), Limit, Parent, []) end),
    C2 = spawn(fun() -> qtreeLeaf(createBound(ne, Bound), Limit, Parent, []) end),
    C3 = spawn(fun() -> qtreeLeaf(createBound(sw, Bound), Limit, Parent, []) end),
    C4 = spawn(fun() -> qtreeLeaf(createBound(se, Bound), Limit, Parent, []) end),
    Bounds = {createBound(nw, Bound),createBound(ne, Bound),
              createBound(sw, Bound),createBound(se, Bound)},
    distributData(Data, [C1,C2,C3,C4]).
    
%distributes the data in the tree after the new children are spawned
distributData([], Children) -> Children;
distributData(Data, Children) ->
    case hd(Data) of
        {element, Pos, Prop} -> send_add(self(), {element, Pos, Prop})
    end,
    distributData(tl(Data), Children).
    
% List with quarters in the quadtree.
qTreeQuarters() -> [nw,ne,sw,se].
% Returns the 1/4 of Bound which is defined by Quarter from the list above.
createBound(_, universe) -> universe;
createBound(_, empty) -> empty;
createBound(Quarter,Bound) -> 
    {X1, Y1, X2, Y2} = Bound,
    Xmid = X1 + ((X2 - X1) / 2),
    Ymid = Y1 + ((Y2 - Y1) / 2),
    case Quarter of
        nw -> {X1,   Y1,   Xmid, Ymid};
        ne -> {Xmid, Y1,   X2,   Ymid};
        sw -> {X1,   Ymid, Xmid, Y2  };
        se -> {Xmid, Ymid, X2,   Y2  }
    end.
% Predicate that returns true if an Element is outside Bound.
outOfBound(universe, _) -> false;
outOfBound(empty, _) -> true;
outOfBound(Bound, Element) ->
    {X1, Y1, X2, Y2} = Bound,
    {element, Pos, _} = Element,
    {X,Y} = Pos,
    (X < X1) or (X >= X2) or (Y < Y1) or (Y >= Y2).
% Finds the intersection between Bound1 and Bound2.
intersectBounds(universe, Bound)    -> Bound;
intersectBounds(Bound,    universe) -> Bound;
intersectBounds(empty,    _    )    -> empty;
intersectBounds(_,        empty)    -> empty;
intersectBounds(Bound,    Bound)    -> Bound;
intersectBounds(Bound1,   Bound2)   ->
    {X1_1, Y1_1, X1_2, Y1_2} = Bound1,
    {X2_1, Y2_1, X2_2, Y2_2} = Bound2,
    X1 = lists:max([X1_1, X2_1]),
    Y1 = lists:max([Y1_1, Y2_1]),
    X2 = lists:min([X1_2, X2_2]),
    Y2 = lists:min([Y1_2, Y2_2]),
    case (X1 >= X2) or (Y1 >= Y2) of
        true  -> empty;
        false -> {X1, Y1, X2, Y2}
    end.
