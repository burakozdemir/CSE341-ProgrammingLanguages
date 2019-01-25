%Burak Ozdemir 141044027
%CSE 341 HOMEWORK-2

%%%%%%%%PART1

%knowledgebase
f(istanbul,izmir,3).
%f(izmir,istanbul,3).

f(istanbul,ankara,5).
%f(ankara,istanbul,5).

f(istanbul,trabzon,3).
%f(trabzon,istanbul,3).

f(izmir,ankara,6).
%f(ankara,izmir,6).

f(ankara,konya,8).
%f(konya,ankara,8).

f(trabzon,ankara,2).
%f(ankara,trabzon,2).

%f(konya,diyarbakir,1).
f(diyarbakir,konya,1).

f(izmir,antalya,1).
%f(antalya,izmir,1).

f(antalya,diyarbakir,5).
%f(diyarbakir,antalya,5).

f(konya,kars,5).
%f(kars,konya,5).

f(kars,gaziantep,3).
%f(gaziantep,kars,3).

f(edirne,edremit,5).
%f(edremit,edirne,5).

f(edremit,erzincan,7).
%f(erzincan,edremit,7).


%%%%%%%%%%%%NOT%%%%%%%%%%%%
%Kod yukarıdan asagı sehirler icin calısıyor.
%Tersi istikametteki yollari ekledigimzde sonsuz donguye giriyor.

%rules

%internetten alındı
addList([],L,L).
addList(Element,L,[Element|L]).

%internetten alındı.
member(X,[X|_]).
member(X,[_|Tail]):- member(X,Tail).

route(A,B,C):- route(A,B,C,[]).
route(A, B, C, _) :- f(A, B, C).
route(A, B, C, V) :- addList(A,V,LIST), 
                        %write(V), 
                        f(A,Z, COST1),
                        route(Z, B, COST2, LIST),
                            C is COST1 + COST2. 
                            
%%%%%%%PART2


head([Head|T],Head).
findmin(X,Y,D) :- route(X,Y,D).
croute(X,Y,RES) :- findall(M,findmin(X,Y,M),N) , sort(N,Sirali), write(Sirali), head(Sirali,RES). 


%%%%%%%PART3

enrollment(1,sessionA).
enrollment(1,sessionB).
enrollment(2,sessionA).
enrollment(3,sessionB).
enrollment(4,sessionC).
enrollment(5,sessionD).
enrollment(6,sessionD).
enrollment(6,sessionA).
enrollment(7,sessionA).
enrollment(7,sessionD).
enrollment(8,sessionA).
enrollment(8,sessionB).
enrollment(9,sessionD).
enrollment(9,sessionA).
enrollment(10,sessionF).
enrollment(10,sessionG).
enrollment(11,sessionF).
enrollment(11,sessionG).

when(sessionA,10).
when(sessionB,12).
when(sessionC,11).
when(sessionD,16).
when(sessionE,17).
when(sessionF,10).
when(sessionG,15).

where(sessionA,101).
where(sessionB,104).
where(sessionC,102).
where(sessionD,103).
where(sessionE,103).
where(sessionF,101).
where(sessionG,102).


usage(P,T) :- where(T,P).
schedule(S,P,T) :- enrollment(S,P) , when(P,T).

conflict(X,Y) :-not(X = Y),
( when(Y,TEMP2),when(X,TEMP1), (TEMP1 = TEMP2 ; abs(TEMP1-TEMP2,K) , K < 2) ; where(X,PER1), where(Y,PER2), PER1 = PER2).

meet(X,Y) :- enrollment(Y,T2),enrollment(X,T1),
not(X = Y),T1 = T2,
write('Session:'),write(T2),write(' ,'),
write('Start Time:'),when(T2,TEMP),write(TEMP),write(' ,'),
write('Room:'),where(T2,PER),write(PER).

%%%%%%PART4



%intersection
intersect([],Y,[]).
intersect([HEAD|TAIL],Y,[HEAD|RES]) :- member(HEAD,Y), intersect(TAIL,Y,RES).
intersect([HEAD|TAIL],Y,RES) :- \+ member(HEAD,Y), intersect(TAIL,Y,RES).
%union
union([],Y,Y).
union([HEAD|TAİL],Y,RES) :- member(HEAD,Y),union(TAİL,Y,RES).
union([HEAD|TAİL],Y,[HEAD|RES]) :- \+ member(HEAD,Y), 
union(TAİL,Y,RES).
%flatten
%icice 1den fazla listte calismiyor
mylist([]).
mylist([_|K]) :- mylist(K).
flatten([],[]).
flatten([HEAD|TAİL], RES) :- flatten(TAİL, ALTFLAT), 
mylist(HEAD), !, append(HEAD,ALTFLAT, RES).
flatten([HEAD|TAİL], RES) :- flatten(TAİL, ALTFLAT), 
append([HEAD],ALTFLAT, RES).
