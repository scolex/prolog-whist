%==========================================================================
% Implementace hry Whist 
% Stanislav Nowak
% 2010
% Zapoctovy program na predmet Rocnikovy projekt
%==========================================================================

%==========================================================================
% gs(Hands,Table,Trick,ActualPlayer,PairScore, Trump,TM)
% popisuje aktualni stav hry

%Hands - obsah rukou hracu
%Table - karty vylozene na stole pri aktualni sehravce
%Trick - ulozene sehravky u hracu
%Actualplayer - hrac ktery je prave na tahu
%PairScore - dosud ziskane body
%Trump - trumf

%==========================================================================
:-dynamic gs/6.

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
%  players(-X) popisuje hrace
%==========================================================================
player(1-p).
player(2-p).
player(3-p).
player(4-p).

%==========================================================================
% players_pairs(-X) urci pary hracu, kteri hraju spolu
%==========================================================================
players_pairs(p1,p2).
players_pairs(p2,p1).
players_pairs(p3,p4).
players_pairs(p4,p3).

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
deal_cards(D,H):-
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

%==========================================================================
%  go zahaji herni opakovaci smycku
%==========================================================================
go:-writeln('Vitejte ve Whist!'),
    asserta(gs([],[],[],null,[1-ps(0),2-ps(0)],null)),
    loop.

%==========================================================================
%  loop herni opakovaci smycka
%==========================================================================
loop:-
    gs(_,_,_,_,PS,_),
    new_deck(D),
    reshuffle(D,DSh),
    deal_cards(DSh,H),
    dealing_player(DP),
    trump(DP,H,TM),
    next_player(DP,AP), %rozdavajici hrac
    TR=[1-tr([]),2-tr([]),3-tr([]),4-tr([])],
    tricks(gs(H,T,TR,AP,PS,TM),gs(H1,T1,TR1,AP1,PS,TM)),
    count_points(TR1,PS,PS1),
    retract(gs(_,_,_,_,_,_,_)),
    asserta(gs([],[],[],AP1,PS1,TM)),
    loop.

loop:-
    writeln('Vyhral prvni par'),
    gs(_,_,_,_,PS,_),
    select(1-pp(X),PS,_),X>6.

loop:-
    writeln('Vyhral druhy par'),
    gs(_,_,_,_,PS,_),
    select(2-pp(X),PS,_),X>6.

%==========================================================================
%  tricks - odehrani jednoho triku, celkem by se melo za kolo odehrat 13
%==========================================================================
tricks(gs(H,T,TR,AP,PS,TM),gs(H8,T8,TR8,AP8,PS,TM)):-
    writeln('Nova sehravka'),
    player_turn(gs(H,T,TR,AP,_,TM), gs(H1,T1,TR1,AP1,_,TM)),
    player_turn(gs(H1,T1,TR1,AP1,_,TM), gs(H2,T2,TR2,AP2,_,TM)),
    player_turn(gs(H2,T2,TR2,AP2,_,TM), gs(H3,T3,TR3,AP3,_,TM)),
    player_turn(gs(H3,T3,TR3,AP3,_,TM), gs(H4,T4,TR4,AP4,_,TM)),
    resolve_trick(gs(H4,T4,TR4,AP4,_,TM), gs(H5,T5,TR5,AP5,_,TM)).
    tricks(gs(H5,T5,TR5,AP5,_,TM), gs(H6,T6,TR6,AP6,_,TM)).

tricks(GS,GS):-
    gs(H,_,_,_,_,_,_)=GS,
    select(1-h([]),H,_),
    select(2-h([]),H,_),
    select(3-h([]),H,_),
    select(4-h([]),H,_).

%==========================================================================
%  loop herni opakovaci smycka
%==========================================================================
count_points(TR,PS,PSO):-
    select(1-ps(PS1),PS,_),
    select(2-ps(PS2),PS,_),


    select(1-tr(TR1),TR,_),
    select(2-tr(TR2),TR,_),
    select(3-tr(TR3),TR,_),
    select(4-tr(TR4),TR,_),

    length(TR1,TL1),
    length(TR2,TL2),
    length(TR3,TL3),
    length(TR4,TL4),

    S1 is TL1 + TL3 - 6,
    S2 is TL2 + TL4 - 6,
    
    max(S1,0,SC1),
    max(S2,0,SC2),

    SCC1 is PS1 + SC1,
    SCC2 is PS2 + SC2,

    PSO=[1-ps(SCC1), 2-ps(SCC2)].

%==========================================================================
%  loop herni opakovaci smycka
%==========================================================================
player_turn(gs(H,T,TR,DP,AP,PS,TM), gs(H2,T2,TR2,DP2,AP2,PS2,TM)):-
    write('Na tahu je hrac '),
    writeln(AP),
    write_board(gs(H,T,TR,DP,AP,PS,TM)),nl,
    bagof([gs(H1,T1,TR1,DP1,AP1,PS1,TM),Turn],
          turn(gs(H,T,TR,DP,AP,PS,TM),
               gs(H1,T1,TR1,DP1,AP1,PS1,TM),Turn),BList),
    write('Mozne tahy'),
    split_by_pair(BList,Boards,Rules),
    enumerate(Rules,RulesN),
    writeln(RulesN),
    read(Nth),
    nth2(Nth,Boards,gs(H2,T2,TR2,DP2,AP2,PS2,TM)).


%==========================================================================
%  dealing_player nahodne vybere rozdavajicho hrace
%==========================================================================
turn(gs(H,T,_,DP,AP,_,TM), gs(H1,T1,_,DP1,AP1,_,TM), Des):-
    T=[],
    N-P=AP,
    select(N-h(PH),H,Hs),
    select(S-R,PH,PHs),
    next_player(AP,AP2),
    T1=[S-R],
    H1=[N-h([PHs])|Hs],
    concat_atom(['(',S,'-',R,')'],Des).

turn(gs(H,[S-_|Ts],_,DP,AP,_,TM), gs(H1,T1,_,DP1,AP1,_,TM)):-
    N-P=AP,
    select(N-h(PH),H,Hs),
    select(S-R,PH,PHs),
    next_player(AP,AP2),
    T1=[S-R|Ts],
    H1=[N-h([PHs])|Hs],
    concat_atom(['(',S,'-',R,')'],Des).

turn(gs(H,T,_,DP,AP,_,TM), gs(H1,T1,_,DP1,AP1,_,TM)):-
    [C|_]=T,
    N-P=AP,
    select(N-h(PH),H,Hs),
    \+ member(C,PH),
    select(S-R,PH,PHs),
    next_player(AP,AP2),
    T1=[S-R|Ts],
    H1=[N-h([PHs])|Hs],
    concat_atom(['(',S,'-',R,')'],Des).

dealing_player(DP):-
    bagof(P,player(P),Players),
    random_elem(Players, DP, _).

next_player(CP,NP):-
    N-p=CP,
    N1 is (N mod 4) + 1,
    NP=N1-p.

trump(CP,H,S):-
    N-p=CP,
    select(N-h([S-_|_]),H,_).
    
resolve_trick(gs(_,T,TR,AP,_,TM), gs(_,T1,TR1,AP1,_,TM)):-
    N-p=AP,
    find_win_card(T,TM,N1),
    N2 is ((N + N1 - 1) mod 4)+ 1,
    AP1=N2-p,
    select(N2-tr(X),TR,TRs),
    TR1=[N2-tr([T|X])|TRs],
    T1=[].

find_win_card(T,TR,N):-
    find_max_card(null,T,TR,0,0,N),
    N=\=0.

find_win_card([S-R|Ts],TR,N1):-
    find_max_card(null,[S-R|Ts],TR,0,0,N),
    N==0,
    find_max_card(null,[S-R|Ts],S,0,0,N1).
    
     
find_max_card(M,[C1|T],S,N,_,NO):-
    C1=S-R,
    lt_card_rank(R,M),
    N1 is N + 1,
    find_max_card(R,T,S,N1,N1,NO).

find_max_card(M,[C1|T],S,N,NM,NO):-
    C1=S-R,
    \+  lt_card_rank(R,M),
    N1 is N + 1,
    find_max_card(M,T,S,N1,NM,NO).

find_max_card(M,[C1|T],S,N,NM,NO):-
    C1 \= S-R,
    N1 is N + 1,
    find_max_card(M,T,S,N1,NM,NO).

find_max_card(_,[],_,_,NM,NO):-
    NO=NM.

lt_card_rank(R1,R2):-
    L=[0-null,1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10,10-j,11-q,12-k,13-a],
    member(N1-R1,L),
    member(N2-R2,L),
    N1>N2.

max(X,Y,X) :- X>=Y.
max(X,Y,Y) :- X<Y.
