%---------------------------------------------------------%
% PROBLEM 1
% define a predicate incr(P1,P2)
% such that P2 is P1+1

% pterm accepts any term of a non-negative integer
% in binary form
% eg 6 becomes bitstring 110 and pterm f0(f1(f1(null)))
pterm(null).
pterm(f0(X)) :- pterm(X).
pterm(f1(X)) :- pterm(X).

incr(f1(null), f0(f1(null))).
incr(f0(X),f1(X)). % covers f0(null), f1(null)
incr(f1(X),f0(Y)) :- incr(X,Y).

%---------------------------------------------------------%
% PROBLEM 2
% define a predicate legal(X) such that X is 'legal'
% ie. has no leading 0s

legal(f0(null)).
legal(Y) :- legal(X), incr(X,Y).

% define incrR such that X and Y are legal and Y is X+1
incrR(X,Y) :- legal(X), incr(X,Y).


%---------------------------------------------------------%
% PROBLEM 3
% define a predicate add(X,Y,Ans) such that Ans is X+Y

%base case
add(f0(null), Y, Y).
add(X, f0(null), X).
add(X,Y,Ans) :-	incr(X,NewX), incr(NewY,Y), add(NewX,NewY,Ans).

%---------------------------------------------------------%
% PROBLEM 4
% define a predicate mult(X,Y,Ans) such that Ans is X*Y

mult(X, f0(null), f0(null)).
mult(f0(null), X, f0(null)).
mult(X, f1(null), X).
mult(f1(null), X, X).

mult(X,Y,Ans) :- mult(X, Y, f0(null), Ans).
mult(f0(null), Y, Ans, Ans).
mult(X,Y,RunningTotal,Ans):-
	incr(NewX,X),
	add(Y, RunningTotal, NewTotal),
	mult(NewX, Y, NewTotal, Ans).

%---------------------------------------------------------%
% PROBLEM 5
% define a predicate revers(P, RevP) that takes a pterm P
% and reverses it to RevP

revers(null, null).
revers(f0(null), f0(null)).
revers(f1(null), f1(null)).
revers(f1(P),RevP) :- revers(P, RevP, f1(null)).
revers(f0(P),RevP) :- revers(P, RevP, f0(null)).

revers(f1(P), RevP, RunningRev) :- revers(P, RevP, f1(RunningRev)).
revers(f0(P), RevP, RunningRev) :- revers(P, RevP, f0(RunningRev)).
revers(P, RevP, RevP).

%---------------------------------------------------------%
% PROBLEM 6
% define a predicate normalize(P, Pn) that takes P and Pn
% such that legal(Pn) is true and P and Pn encode the same
% number

normalize(null, f0(null)).
%normalize(Pn, Pn).
normalize(P, Pn) :- revers(P, RevP), leading0s(Pn, RevP).

leading0s(Pn,f0(RevP)) :- leading0s(Pn,RevP).
leading0s(Pn,f1(RevP)) :- f1Add(RevP, X), revers(X, Pn).

f1Add(X, f1(X)).

%---------------------------------------------------------%
% TESTING

% test add inputting numbers N1 and N2
testAdd(N1,N2,T1,T2,Sum,SumT) :- numb2pterm(N1,T1), numb2pterm(N2,T2), add(T1,T2,SumT), pterm2numb(SumT,Sum).

% test mult inputting numbers N1 and N2
testMult(N1,N2,T1,T2,N1N2,T1T2) :- numb2pterm(N1,T1), numb2pterm(N2,T2),
mult(T1,T2,T1T2), pterm2numb(T1T2,N1N2).

% test revers inputting list L
testRev(L,Lr,T,Tr) :- ptermlist(T,L), revers(T,Tr), ptermlist(Tr,Lr).

% test normalize inputting list L
testNorm(L,T,Tn,Ln) :- ptermlist(T,L), normalize(T,Tn), ptermlist(Tn,Ln).

% make a pterm T from a number N numb2term(+N,?T)
numb2pterm(0,f0(null)).
numb2pterm(N,T) :- N>0, M is N-1, numb2pterm(M,Temp), incr(Temp,T).

% make a number N from a pterm T pterm2numb(+T,?N)
pterm2numb(null,0).
pterm2numb(f0(X),N) :- pterm2numb(X,M), N is 2*M.
pterm2numb(f1(X),N) :- pterm2numb(X,M), N is 2*M +1.

% reversible ptermlist(T,L)
ptermlist(null,[]).
ptermlist(f0(X),[0|L]) :- ptermlist(X,L).
ptermlist(f1(X),[1|L]) :- ptermlist(X,L).
