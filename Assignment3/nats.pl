%%%---------------------------------------------------------------------
%%% @authors: Michael Kirkedal Thomsen <shapper@diku.dk>
%%%           Erik Partridge 
%%% @copyright (C) 2013, Michael Kirkedal Thomsen, Erik Partridge
%%% Created : Aug 2013 for exercise in Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student: [Simon Maibom]
%%% KU-Id:   [xvm226]
%%% Student: [Kasper Passov]
%%% KU-Id:   [pvx884]
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

% minus (X,Y,Z) => Z = X - Y => Z + Y = X
minus(X,Y,Z) :- add(Y,Z,X).

% mult(X,Y,Z) => Z = X * Y 
mult(z,Y,z).
mult(s(X),Y,Z) :- 
    mult(X,Y,P), 
    add(P,Y,Z).

% mult2(X,Y,Z) => Z = X * Y => Y+Mult(X-1,Y)
mult2(z,Y,z).
mult2(s(X),Y,Z) :- 
    less(P,Z),
    mult2(X,Y,P), 
    add(P,Y,Z).

    

% mod(X,Y,Z) => Z = X mod Y
mod(Z,Y,Z) :- less(Z,Y).
mod(X,Y,Z) :- less(z,Y), minus(X,Y,H), mod(H,Y,Z).

%%%---------------------------------------------------------------------
%%% Part 2:
%%%---------------------------------------------------------------------

% notprime(X) => X is not a prime
notprime(s(z)).
notprime(X) :- 
    less(s(z),Y), 
    less(Y,X), 
    mod(X,Y,z).
    


% prime(X) => X is a prime
prime(X) :- notdividable(X,X).

%notdividable(X,Y) => X / Y = False 
notdividable(X,s(s(z))).
notdividable(X,s(Y)) :- 
    less(Z,X), 
    less(z,Z), 
    mod(X,Y,Z), 
    notdividable(X,Y).

% listPrimes(L,X) => L is the list of primes small than or equal to X
listPrimes([],z).
listPrimes([s(X)|L],s(X)) :-
    prime(s(X)),
    listPrimes(L,X).
listPrimes(L,s(X)) :-
    listPrimes(L,X).
    
    
%superprime(X) => 
superprime(X) :- 
    listPrimes(Y,X),
    length1(Y,P),
    prime(P).

% length1(L,X) => Size X of list L
length1([],z).
length1([S|L],s(X)) :-
    length1(L,X).

%%%---------------------------------------------------------------------
%%% Part 3:
%%%---------------------------------------------------------------------


%%%---------------------------------------------------------------------
%%% Tests: 
%%%---------------------------------------------------------------------

less_tests :-
    less(z,s(z)),
    less(z,s(_)),
    \+ less(z,z),
    \+ less(s(s(z)),s(z)).
    

add_tests :- 
    n2i(X,9), n2i(Y,12), n2i(Z,21), add(X,Y,Z),
    add(z,z,z),
    n2i(A,8), n2i(B,3), n2i(C,12), \+ add(A,B,C).

minus_tests :-
    n2i(X,9), n2i(Y,5), n2i(Z,4), minus(X,Y,Z),
    minus(z,z,z),
    n2i(A,5), n2i(B,9), \+ minus(A,B,_),
    minus(s(s(z)),s(s(z)),z),
    minus(P,s(s(s(z))),s(s(z))),n2i(Q,5), P = Q.
    
mod_tests :-
    n2i(X,9), n2i(Y,3), mod(X,Y,z),
    n2i(P,5), mod(P,s(s(z)),Z), Z = s(z).

prime_success_tests :-
    prime(s(s(z))),
    n2i(X,7), prime(X),
    n2i(Y,11), prime(Y),
    n2i(Z,83), prime(Z).
    
    
prime_fail_tests :-    
    n2i(Y,4), \+ prime(Y),
    \+ prime(s(z)),
    n2i(X,9), \+ prime(X),
    n2i(Z,85), \+ prime(Z). 
    
notprime_tests :-
    notprime(s(z)),
    \+ notprime(s(s(z))).
    
listprimes_tests :-
    n2i(X,11), listPrimes(Y,X),
    n2i(A,2), n2i(B,3), n2i(C,5), n2i(D,7), n2i(E,11),
    F = [E,D,C,B,A], Y = F.
    

start_tests :-
        write('\nless: '),
        (less_tests -> print(pass) ; print(failure)),
        write('\nadd: '),
        (add_tests -> print(pass) ; print(failure)),
        write('\nminus: '),
        (minus_tests -> print(pass) ; print(failure)),
        write('\nsucces prime: '),
        (prime_success_tests -> print(pass) ; print(failure)),
        write('\nfail prime: '),
        (prime_fail_tests -> print(pass) ; print(failure)),
        write('\nnotprime: '),
        (notprime_tests -> print(pass) ; print(failure)).


%%%---------------------------------------------------------------------
%%% Help of convert to and from unary numbers
%%%---------------------------------------------------------------------

% Binary to unary
i2n(X,s(Y)) :- X > 0, Z is (X-1), i2n(Z,Y).
i2n(0,z).

% Unary to binary
 n2i(z,0).
  n2i(s(X),Y) :- nonvar(Y), !, Y > 0, Z is (Y-1), i2n(Z,X).
  n2i(s(X),Y) :- n2i(X,Z), Y is (Z+1).
