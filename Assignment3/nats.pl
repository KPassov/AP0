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
% mult(X,Y,Z) :- 

% mod(X,Y,Z) => Z = X mod Y
mod(Z,Y,Z) :- less(Z,Y).
mod(X,Y,Z) :- less(z,Y), minus(X,Y,H), mod(H,Y,Z).

%%%---------------------------------------------------------------------
%%% Part 2:
%%%---------------------------------------------------------------------

% notprime(X) => X is not a prime
notprime(s(z)).
notprime(X) :- less(s(z),Y), less(Y,X), mod(X,Y,z).

% prime(X) => X is a prime
%prime(

% listPrimes(L,X) => L is the list of primes small than or equal to X
%listPrimes( ... ) :- ...

% superprime(X) => X is a super-prime
%superprime( ... ) :- ...


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
