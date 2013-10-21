-module(swarmTest).

-export([run/0, stop/2]).

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

    swarm:newFish(Swarm, {32,32}),
    swarm:newFish(Swarm, {30,30}),
    swarm:newFish(Swarm, {30,32}),
    swarm:newFish(Swarm, {32,30}),
    swarm:newFish(Swarm, {31,31}),
    swarm:newFish(Swarm, {33,33}),
    swarm:newFish(Swarm, {33,32}),
    swarm:newFish(Swarm, {32,33}),

    timer:sleep(100),
    Loop = spawn(fun() -> theloop(Printer, Qtree) end). 

stop(Swarm, Loop) ->
    swarm:stop(Swarm), 
    exit(Loop, normal).

theloop(Printer, Qtree) -> 
    printer:reset(Printer,"swarm.svg"),
    print(Qtree, Printer),
    timer:sleep(1000),
    theloop(Printer, Qtree).

    











