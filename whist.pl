%==========================================================================
% Implementace hry Whist 
% Stanislav Nowak
% 2010
% Zapoctovy program na predmet Rocnikovy projekt
%==========================================================================

%==========================================================================
% gs(Hands,Table,Trick,Dealingturner,Actualturner,PairPoins)
% popisuje aktualni stav hry

%Hands - obsah rukou hracu
%Table - karty vylozene na stole pri aktualni sehravce
%Trick - ulozene sehravky u hracu
%Dealingturner - hrac ktery prave rozdava
%Actualturner - hrac ktery je prave na tahu
%PairPoins - dosud ziskane body

%==========================================================================
:-dynamic gs/4.

%==========================================================================
%  suit(-X) popisuje barvu karet
%==========================================================================
suit(c). %clubs 
suit(d). %diamonds
suit(h). %hearts
suit(s). %spades

%==========================================================================
%  rank(-X) popisuje figury/hodnoty karet
%==========================================================================
rank(2).  rank(3). 
rank(4).  rank(5). 
rank(6).  rank(7).
rank(8).  rank(9). 
rank(10).
rank(j). %jack
rank(q). %queen
rank(k). %king
rank(a). %ace

%==========================================================================
%  turners(-X) popisuje hrace
%==========================================================================
turners(p1). 
turners(p2). 
turners(p3). 
turners(p4). 

%==========================================================================
% turner_pairs(-X) urci pary hracu, kteri hraju spolu
%==========================================================================
turner_pairs(p1,p2).
turner_pairs(p2,p1).
turner_pairs(p3,p4).
turner_pairs(p4,p3).

%==========================================================================
% split(+N,+L1,-L2) vrati prvnich N prvku ze seznamu L1 v seznamu L2
%========================================================================== 
split(0,_,[]).
split(N,[H1|L1],[H1|L2]):-N1 is N-1,split(N1,L1,L2).

%==========================================================================
% split_by_pair(+L1,-L2,-L3) rozdeli seznam dvojic L1 do seznamu L2 a L3
%========================================================================== 
split_by_pair([],[],[]).
split_by_pair([[A,B]|L1],[A|L2],[B|L3]):-split_by_pair(L1,L2,L3).

%==========================================================================
%  enumerate(+L1,-L2) ocisluje prvky v seznamu L1 pridanim n: a vrati je v 
% L2
%==========================================================================
enumerate([],[], _).
enumerate([X|L1],[N-X|L2], N):-
    N1 is N+1,
    enumerate(L1,L2,N1).
enumerate(L1,L2):-enumerate(L1,L2,1).
 
%==========================================================================
%  cards(?C) popisuje hraci kartu jako dvojci barvy a figury
%==========================================================================
cards(S-R) :- suit(S),rank(R).

%==========================================================================
%  new_deck(-D) vygeneruje 52 karet potrebnych ke hre
%==========================================================================
new_deck(Deck) :- bagof(C,cards(C),Deck).

%==========================================================================
%  sel_nth1(+Nth, +List, -E, -Rest) vybere n-ty prvek ze seznamu List a vrati
% zbytek Rest
%==========================================================================
sel_nth1(1, [X|Xs], X, Xs):-!.
sel_nth1(N, [X|Xs], E, [X|Ys]) :-   
    N1 is N-1,
    sel_nth1(N1, Xs, E, Ys).

%==========================================================================
%  random_elem(+List, -E, -Rest)  nahodne vybere prvek E ze seznamu a vrati 
% zbytek seznamu Rest
%==========================================================================
random_elem(List, E, Rest) :-
    length(List, L),
    L > 0,
    R is random(L),
    Rand is R + 1,
    sel_nth1(Rand, List, E, Rest).

%==========================================================================
%  reshuffle(+D1,-D2) zamicha nahodne zadany balicek D1 a
% vysledek vrati v D2
%==========================================================================
reshuffle([],[]).
reshuffle(D,[C|D2]):-
    random_elem(D, C, D1),
    reshuffle(D1, D2).

%==========================================================================
%  deal_cards(+D,-S) rozdani karet
%==========================================================================
deal_card(D,H):-
    length(H1,13),
    append(H1,D1,D),

    length(H2,13),
    append(H2,D2,D1),

    length(H3,13),
    append(H3,D3,D2),
    
    H=[1-h(H1),
      2-h(H2),
      3-h(H3),
      4-h(D3)].  

prepare_board(gs(H,T,TR,DP,AP,PP)):-
    H=[],
    T=[],
    TR=[],
    DP=p1,
    AP=p2,
    PP=[1-pp(0),2-pp(0)].
        

%==========================================================================
%  go zahaji herni opakovaci smycku
%==========================================================================
go:-writenl('Vitejte ve Whist!'),
    prepare_board(GS),
    asserta(GS),
    loop.

%==========================================================================
%  loop herni opakovaci smycka
%==========================================================================
loop:-
    gs(H,T,TR,DP,AP,PP),
    new_deck(D),
    reshuffle(D,DSh),
    deal_cards(DSh,H),
    %vyber trumf
    tricks(gs(H,T,TR,DP,AP,PP),gs(H1,T1,TR1,DP1,AP1,PP1)),
    count_points(TR1,PP1,PP2)),
    get_next_turner(AP1,DP2),
    retract(gs(_,_,_,_,_,_)),
    asserta(gs([],[],[],AP1,DP2,PP2)),
    loop.

loop:-
    writeln('Vyhral prvni par'),
    gs(_,_,_,_,_,PP),
    select(1-pp(X),PP,_),X>6.

loop:-
    writeln('Vyhral druhy par'),
    gs(_,_,_,_,_,PP),
    select(2-pp(X),PP,_),X>6.

%==========================================================================
%  trick odehrani jednoho triku
%==========================================================================
trick(gs(H,T,TR,_,AP,_),gs(H5,T5,TR5,_,AP5,_)):-
    writenl('Nova sehravka'),
    turn(gs(H,T,TR,_,AP,_), gs(H1,T1,TR1,_,AP1,_)),
    resolve_turn(gs(H1,T1,TR1,_,AP1,_), gs(H2,T2,TR2,_,AP2,_)),
    turn(gs(H1,T1,TR1,_,AP1,_), gs(H2,T2,TR2,_,AP2,_)),
    resolve_turn(gs(H1,T1,TR1,_,AP1,_), gs(H2,T2,TR2,_,AP2,_)),
    turn(gs(H2,T2,TR2,_,AP2,_), gs(H3,T3,TR3,_,AP3,_)),
    resolve_turn(gs(H1,T1,TR1,_,AP1,_), gs(H2,T2,TR2,_,AP2,_)),
    turn(gs(H3,T3,TR3,_,AP3,_), gs(H4,T4,TR4,_,AP4,_)),
    resolve_turn(gs(H1,T1,TR1,_,AP1,_), gs(H2,T2,TR2,_,AP2,_)),
    trick(gs(H4,T4,TR4,_,AP4,_),gs(H5,T5,TR5,_,AP5,_)).

trick(gs(H,_,_,_,_,_),_):-
    select(1-h([]),H,_),
    select(2-h([]),H,_),
    select(3-h([]),H,_),
    select(4-h([]),H,_).

%==========================================================================
%  loop herni opakovaci smycka
%==========================================================================
count_points(TR,PP,PP1):-


resolve_turn(AP1,DP2),
    %presunout ziskany zdrvi ke hraci
    %ten kdo vyhral je dalsi hrajici
    %vyvstit stul
%==========================================================================
%  loop herni opakovaci smycka
%==========================================================================
turn(gs(H,T,TR,DP,AP,PP), gs(H2,T2,TR2,DP2,AP2,PP2)):-
    writenl('Na tahu je hrac X'),
    write_board(gs(H,T,TR,DP,AP,PP)),nl,
    bagof([gs(H1,T1,TR1,DP1,AP1,PP1),Turn],
          turn(gs(H,T,TR,DP,AP,PP),gs(H1,T1,TR1,DP1,AP1,PP1),Turn),BList),
    write('Mozne tahy'),
    split_by_pair(BList,Boards,Turns),
    enumerate(Turns, TurnsN),
    write(TurnsN),nl,
    read(Nth),
    nth2(Nth,Boards,gs(H2,T2,TR2,DP2,AP2,PP2)).

