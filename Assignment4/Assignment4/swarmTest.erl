-module(swarmTest).

-export([run/0, stop/2]).

print(Qtree, Printer) ->
    MapFun = 
        fun (Element) ->
            swarmPrinter:addCircle(Printer, element(2,Element)),
            Element 
        end,
    quadtree:mapFunction(Qtree, MapFun, universe).

run() ->
    {ok,Printer} = printer:start(),
    {Qtree,Swarm} = swarm:run({0,0,64,64},9),

    swarm:newFish(Swarm, {31,31}),
    swarm:newFish(Swarm, {31,32}),
    swarm:newFish(Swarm, {31,33}),
    swarm:newFish(Swarm, {32,31}),
    swarm:newFish(Swarm, {32,32}),
    swarm:newFish(Swarm, {32,33}),
    swarm:newFish(Swarm, {33,31}),
    swarm:newFish(Swarm, {33,32}),
    swarm:newFish(Swarm, {33,33}),
    swarm:newFish(Swarm, {43,43}),
    swarm:newFish(Swarm, {31,22}),

    timer:sleep(100),
    Loop = spawn(fun() -> theloop(Printer, Qtree) end), 
    {Swarm, Loop}.

stop(Swarm, Loop) ->
    swarm:stop(Swarm),
    exit(Loop, normal).

theloop(Printer, Qtree) -> 
    printer:reset(Printer,"swarm.svg"),
    print(Qtree, Printer),
    timer:sleep(100),
    theloop(Printer, Qtree). 

    











