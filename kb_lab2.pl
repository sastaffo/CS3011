%-------------------------------------------------------------------%
% PROBLEM 1
% DCG, accepts strings in the form u2v where u and v are both strings
% over alphabet {0,1} such that #0 in u = 2(#1 in v)
% eg s([*0*,1,*0*,1,2,0,0,*1*,0],[]).
u2v --> [2].
u2v --> left, u2v, right.

left --> ones,[0],ones,[0],ones.
ones--> [].
ones--> [1],ones.

right --> zeros,[1],zeros.
zeros --> [].
zeros --> [0], zeros.
%-------------------------------------------------------------------%
% PROBLEM 2
% colours: red, blue, green
% nationalities: english, spanish, japanesejn. .
% pets: jaguar, snail, zebra
% output: [CA, NA, PA, CB, NB, PB, CC, NC, PC]
street -->  house(ColA,NatA,PetA), house(ColB,NatB,PetB),
			house(ColC,NatC,PetC),
			{ColA\=ColB}, {ColB\=ColC}, {ColA\=ColC},
			{NatA\=NatB}, {NatB\=NatC}, {NatA\=NatC},
			{PetA\=PetB}, {PetB\=PetC}, {PetA\=PetC}.
house(Col,Nat,Pet) -->	[Col, Nat, Pet], {lex(Col, col)},
						{lex(Nat, nat)}, {lex(Pet, pet)}.

lex(red,   col).
lex(blue,  col).
lex(green, col).

lex(english,  nat).
lex(spanish,  nat).
lex(japanese, nat).

lex(jaguar, pet).
lex(snail,  pet).
lex(zebra,  pet).

%-------------------------------------------------------------------%
% PROBLEM 3
% Sum, accepts a list of integer >= 1

% sum(3,L,[])
% | ?- s(3,L,[]).
% L = [3];
% L = [2,1];
% L = [1,2];
% L = [1,1,1];

% mkList(+Num, ?List) returns a list of ints from Num to 1
% | ?- mkList(3,L).
% L = [3,2,1];


%-------------------------------------------------------------------%
