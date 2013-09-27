%%%---------------------------------------------------------------------
%%% @authors: Michael Kirkedal Thomsen <shapper@diku.dk>
%%%           Erik Partridge 
%%% @copyright (C) 2013, Michael Kirkedal Thomsen, Erik Partridge
%%% Created : Aug 2013 for exercise in Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student: [INSERT]
%%% KU-Id:   [INSERT]
%%% Student: [INSERT]
%%% KU-Id:   [INSERT]
%%%---------------------------------------------------------------------
upd :- consult('nats.pl').

%%%---------------------------------------------------------------------
%%% Part 1:
%%%---------------------------------------------------------------------

% less (X,Y) => X < Y
less(z, s(X)).
less(s(X),s(Y)) :- less(X,Y). 


% add (X,Y,Z) => Z = X + Y
add(z,X,X).
add(s(X),Y,s(Z)) :- add(X, Y, Z).

% minus (X,Y,Z) => Z = X - Y
minus(X,Y,Z) :- add(Y,Z,X).

% mult(X,Y,Z) => Z = X * Y
mult(z,Y,z).
mult(s(X),Y,Z) :-
    less(P,Z)
    mult(X,Y,P),
    add(P,Y,Z).

% mod(X,Y,Z) => Z = X mod Y
mod(Z,Y,Z) :- less(Z,Y).
mod(X,Y,Z) :- less(z,Y), minus(X,Y,H), mod(H,Y,Z).

%%%---------------------------------------------------------------------
%%% Part 2:
%%%---------------------------------------------------------------------

% notprime(X) => X is not a prime
notprime(s(z)).
notprime(s(X)) :- prime(s(X)), notprime(X).
notprime(s(X)) :- less(s(z),Y), less(Y,X), mod(X,Y,z), n2i(X,P), write(P).

% prime(X) => X is a prime
prime(X) :- notdividable(X,X), n2i(X,P), write(P).

%notdividable(X,Y) => X / Y = False 
notdividable(X,s(s(z))).
notdividable(X,s(Y)) :- less(Z,X), less(z,Z), mod(X,Y,Z), notdividable(X,Y).

% listPrimes(L,X) => L is the list of primes small than or equal to X
listPrimes([P], T, z).
listPrimes([P], T, X) :-
    prime(X),
    minus(X,z,S),
    listPrimes([X|P],T,S).
    
    
    
%listPrimes([],X).


% superprime(X) => X is a super-prime


%%%---------------------------------------------------------------------
%%% Part 3:
%%%---------------------------------------------------------------------


%%%---------------------------------------------------------------------
%%% Tests: 
%%%---------------------------------------------------------------------


%%%---------------------------------------------------------------------
%%% Help of convert to and from unary numbers
%%%---------------------------------------------------------------------

% Binary to unary
i2n(X,s(Y)) :- X > 0, Z is (X-1), i2n(Z,Y).
i2n(0,z).

% Unary to binary
n2i(s(X),Y) :- n2i(X,Z), Y is (Z+1).
n2i(z,0).
