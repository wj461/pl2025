r3(L, R) :- append(R,[_ , _ , _], L).
g2(L, R) :- append([_, _, _], R, T), append(T, [_, _, _], L).

last(Item, List) :- append(_, [Item], List).

last2(Item, [Item]).
last2(Item, [_|T]) :- last2(Item, T).

reverse([H|L], R) :- reverse[L, RL] , append(RL, [H], R)

sumList([], 0).
sumList([H|T], S) :- sumList(T, ST), S is H + ST.

# L = [1,2,3,4,5,6,7,8,9], R = [2,3,4,5,6,7,8]

