/* YOUR CODE HERE (Problem 1, delete the following line) */
range(S,E,M) :- M >= S, M =< E.

?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Problem 2, delete the following line) */
reverseL_helper([],[]).
reverseL_helper([H|T], Rem) :- reverseL_helper(T, RevT), append(RevT, [H], Rem).

reverseL(X,RevX) :- reverseL_helper(X,RevX).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 3, delete the following line) */
memberL_helper(X, [X|_]).
memberL_helper(X, [_|T]) :- memberL_helper(X, T).

memberL(X,L) :- memberL_helper(X,L).

?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Problem 4, delete the following line) */

zip(Xs, Ys, XYs) :- false.

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
insert_helper(X, [], [X]). 
insert_helper(X, [Y|Zs], [X,Y|Zs]) :- X < Y.
insert_helper(X, [Y|Ys], [Y|Zs]) :- insert_helper(X, Ys, Zs).

insert(X, Ys, Zs) :- insert_helper(X, Ys, Zs).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Problem 6, delete the following line) */
remove_duplicates(L1,L2) :- false.

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Problem 7, delete the following line) */
intersectionL_helper([], _, []).
intersectionL_helper([H|T], L2, L3) :- memberL(H,L2), L3 = [H|T3], intersectionL_helper(T,L2,T3).
intersectionL_helper([_|T], L2, L3) :- intersectionL_helper(T,L2,L3).

intersectionL(L1,L2,L3) :- intersectionL_helper(L1,L2,L3).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

/* YOUR CODE HERE (Problem 8, delete the following line) */
partition_helper([],[],[]).
partition_helper([H],[H],[]). 
partition_helper(L, P, S) :- length(L, N), P_length is div(N,2), S_length is N - div(N,2), 
length(P, P_length), length(S, S_length), prefix(P, L), suffix(S, L).

partition(L,P,S) :- partition_helper(L,P,S).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 9, delete the following line) */
merge_helper(List, List, []).
merge_helper(List, [], List).
merge_helper([], List, List).
merge_helper([X|Xs], [Y|Ys], [X|Zs]) :- X =< Y, merge_helper(Xs, [Y|Ys], Zs).
merge_helper([X|Xs], [Y|Ys], [Y|Zs]) :- Y =< X, merge_helper([X|Xs], Ys, Zs).

merge(X,Y,Z) :- merge_helper(X,Y,Z).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 10, delete the following line) */
merge_sort([], []).
merge_sort([X], [X]).
merge_sort([X,Y|Zs], Ys) :- partition([X,Y|Zs], L, R), merge_sort(L,Ls), merge_sort(R,Rs), merge(Ls,Rs,Ys).                          
mergesort(L,SL) :- merge_sort(L,SL).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
