%%%---------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% Created : Jan 2013 
%%%---------------------------------------------------------------------

% Provided the is a very small example of how the printing works.
% It prints 4 SVG-files with increasing number of elements and a small 
% function map.
%
% NOTE: This is no way near to a test.

-module(swarmTest).

-export([run/0]).

print(Qtree, Printer) ->
    MapFun = 
        fun (Element) ->
            swarmPrinter:addCircle(Printer, element(2,Element)),
            Element 
        end,
    quadtree:mapFunction(Qtree, MapFun, universe).

printGrid(Qtree, Printer) ->
    MapFun = 
        fun (Bound) ->
            swarmPrinter:addBox(Printer, Bound)
        end,
    quadtree:mapTreeFunction(Qtree, MapFun).


run() ->
    {ok,Printer} = printer:start(),
    {Qtree,Swarm} = swarm:run({0,0,64,64},1),

    swarm:newFish(Swarm, {5,10}),
    swarm:newFish(Swarm, {5,10}),
    swarm:newFish(Swarm, {6,11}),
    swarm:newFish(Swarm, {10,10}),
    swarm:newFish(Swarm, {13,25}),
    swarm:newFish(Swarm, {19,23}), 
    swarm:newFish(Swarm, {31,51}),
    swarm:newFish(Swarm, {33,45}),
    swarm:newFish(Swarm, {33,65}),
    swarm:newFish(Swarm, {35,60}),
    swarm:newFish(Swarm, {35,49}),
    swarm:newFish(Swarm, {39,43}),
    swarm:newFish(Swarm, {39,63}),

    swarm:attraRepul(Qtree),
    
    timer:sleep(100),
    %swarmPrinter:reset(Printer,"swarm1.svg"),
    %printGrid(Qtree,Printer),
    %print(Qtree,Printer),
    timer:sleep(100).
    


    











