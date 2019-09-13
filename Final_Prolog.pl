response(Q,PQ,PR,R

:-discontiguous(prop/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% VEGETABLES %%%%

prop(tomato,is,vegetable).
prop(onion,is,vegetable).
prop(bell_pepper,is,vegetable).
prop(chili_pepper,is,vegetable).
prop(carrot,is,vegetable).
prop(pea,is,vegetable).
prop(artichoke,is,vegetable).
prop(eggplant,is,vegetable).
prop(cucumber,is,vegetable).
prop(lettuce,is,vegetable).
prop(okra,is,vegetable).
prop(cauliflower,is,vegetable).
prop(cabbage,is,vegetable).
prop(broccoli,is,vegetable).
prop(mushroom,is,vegetable).
prop(potato,is,vegetable).
prop(zucchini,is,vegetable).
prop(broccoli,is,vegetable).
prop(spinach,is,vegetable).
prop(corn,is,vegetable).

%%%% FRUITS %%%%

prop(strawberry,is,fruit).
prop(blackberry,is,fruit).
prop(blueberry,is,fruit).
prop(banana,is,fruit).
prop(orange,is,fruit).
prop(grape,is,fruit).
prop(pineapple,is,fruit).
prop(apple,is,fruit).
prop(kiwi,is,fruit).
prop(peaches,is,fruit).
prop(guava,is,fruit).
prop(pear,is,fruit).
prop(mango,is,fruit).
prop(apricot,is,fruit).
prop(avocado,is,fruit).
prop(cherry,is,fruit).
prop(fig,is,fruit).
prop(coconut,is,fruit).
prop(lemon,is,fruit).
prop(watermelon,is,fruit).
prop(cantaloupe,is,fruit).

%%%% DIARY %%%%

prop(cheese,is,diary).
prop(milk,is,diary).
prop(yogurt,is,diary).

%%%% CARBS %%%%

prop(flour,is,carb).
prop(rice,is,carb).
prop(pasta,is,carb).
prop(chocolate,is,carb).

%%%% FATS %%%%

prop(oil,is,fat).
prop(butter,is,fat).

%%%% PROTEINS %%%%

prop(egg,is,protein).
prop(fish,is,protein).
prop(chicken,is,protein).
prop(meat,is,protein).
prop(shrimp,is,protein).
prop(minced_meat,is,protein).

%%%% DRESSING %%%%

prop(mayonnaise,is,dressing).
prop(vinegar,is,dressing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(chicken_caesar_salad,contain,chicken).
prop(chicken_caesar_salad,contain,oil).
prop(chicken_caesar_salad,contain,lettuce).
prop(chicken_caesar_salad,contain,cheese).
prop(chicken_caesar_salad,contain,mayonnaise).
prop(chicken_caesar_salad,contain,vinegar).
prop(chicken_caesar_salad,contain,bread).

prop(green_salad,contain,carrot).
prop(green_salad,contain,bell_pepper).
prop(green_salad,contain,lettuce).
prop(green_salad,contain,onion).
prop(green_salad,contain,tomato).
prop(green_salad,contain,cucumber).

prop(coleslaw_salad,contain,carrot).
prop(coleslaw_salad,contain,cabbage).
prop(coleslaw_salad,contain,mayonnaise).
prop(coleslaw_salad,contain,oil).

prop(pasta_salad,contain,bell_pepper).
prop(pasta_salad,contain,mayonnaise).
prop(pasta_salad,contain,pasta).
prop(pasta_salad,contain,corn).

prop(fruit_salad,contain,strawberry).
prop(fruit_salad,contain,banana).
prop(fruit_salad,contain,orange).
prop(fruit_salad,contain,apple).

prop(croissant,contain,butter).
prop(croissant,contain,flour).
prop(croissant,contain,milk).
prop(croissant,contain,oil).
prop(croissant,contain,egg).

prop(spanish_omelette,contain,egg).
prop(spanish_omelette,contain,oil).
prop(spanish_omelette,contain,potato).

prop(boiled_egg,contain,egg).

prop(grilled_chicken,contain,chicken).
prop(grilled_chicken,contain,lemon).
prop(grilled_chicken,contain,onion).

prop(fried_chicken,contain,chicken).
prop(fried_chicken,contain,oil).
prop(fried_chicken,contain,onion).
prop(fried_chicken,contain,flour).

prop(cake,contain,flour).
prop(cake,contain,butter).
prop(cake,contain,milk).
prop(cake,contain,egg).

prop(chocolate_cake,contain,cake).
prop(chocolate_cake,contain,chocolate).

prop(white_rice,contain,rice).
prop(white_rice,contain,butter).

prop(mexican_rice,contain,rice).
prop(mexican_rice,contain,oil).
prop(mexican_rice,contain,onion).
prop(mexican_rice,contain,tomato).

prop(ratatouille,contain,zucchini).
prop(ratatouille,contain,eggplant).
prop(ratatouille,contain,tomato).
prop(ratatouille,contain,bell_pepper).
prop(ratatouille,contain,onion).
prop(ratatouille,contain,lemon).
prop(ratatouille,contain,oil).
prop(ratatouille,contain,vinegar).

prop(lasagne,contain,pasta).
prop(lasagne,contain,milk).
prop(lasagne,contain,flour).
prop(lasagne,contain,butter).
prop(lasagne,contain,minced_meat).
prop(lasagne,contain,cheese).

prop(pasta_white_sauce,contain,pasta).
prop(pasta_white_sauce,contain,milk).
prop(pasta_white_sauce,contain,flour).
prop(pasta_white_sauce,contain,butter).

prop(pasta_red_sauce,contain,pasta).
prop(pasta_red_sauce,contain,tomato).
prop(pasta_red_sauce,contain,oil).

prop(pasta_alfredo,contain,pasta).
prop(pasta_alfredo,contain,milk).
prop(pasta_alfredo,contain,flour).
prop(pasta_alfredo,contain,butter).
prop(pasta_alfredo,contain,chicken).

prop(pasta_negresco,contain,pasta).
prop(pasta_negresco,contain,milk).
prop(pasta_negresco,contain,flour).
prop(pasta_negresco,contain,butter).
prop(pasta_negresco,contain,chicken).
prop(pasta_negresco,contain,cheese).

prop(shrimp_pasta,contain,pasta).
prop(shrimp_pasta,contain,shrimp).
prop(shrimp_pasta,contain,butter).
prop(shrimp_pasta,contain,milk).

prop(pizza,contain,tomato).
prop(pizza,contain,cheese).
prop(pizza,contain,flour).
prop(pizza,contain,oil).

prop(bread,contain,milk).
prop(bread,contain,flour).
prop(bread,contain,butter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(tomato,contain,11,cal).
prop(onion,contain,44,cal).
prop(cheese,contain,431,cal).
prop(egg,contain,78,cal).
prop(pasta,contain,131,cal).
prop(bell_pepper,contain,24,cal).
prop(chili_pepper,contain,18,cal).
prop(carrot,contain,25,cal).
prop(pea,contain,81,cal).
prop(artichoke,contain,120,cal).
prop(eggplant,contain,25,cal).
prop(cucumber,contain,32,cal).
prop(lettuce,contain,15,cal).
prop(okra,contain,33,cal).
prop(cauliflower,contain,25,cal).
prop(cabbage,contain,25,cal).
prop(broccoli,contain,31,cal).
prop(mushroom,contain,5,cal).
prop(potato,contain,163,cal).
prop(zucchini,contain,33,cal).
prop(spinach,contain,23,cal).
prop(corn,contain,86,cal).
prop(strawberry,contain,33,cal).
prop(blackberry,contain,43,cal).
prop(blueberry,contain,57,cal).
prop(banana,contain,89,cal).
prop(orange,contain,47,cal).
prop(grape,contain,62,cal).
prop(pineapple,contain,42,cal).
prop(apple,contain,92,cal).
prop(kiwi,contain,42,cal).
prop(peaches,contain,59,cal).
prop(guava,contain,38,cal).
prop(pear,contain,85,cal).
prop(mango,contain,99,cal).
prop(apricot,contain,48,cal).
prop(avocado,contain,160,cal).
prop(cherry,contain,50,cal).
prop(fig,contain,107,cal).
prop(coconut,contain,283,cal).
prop(lemon,contain,24,cal).
prop(watermelon,contain,30,cal).
prop(cantaloupe,contain,34,cal).
prop(milk,contain,124,cal).
prop(yogurt,contain,218,cal).
prop(flour,contain,364,cal).
prop(rice,contain,150,cal).
prop(oil,contain,240,cal).
prop(butter,contain,204,cal).
prop(fish,contain,305,cal).
prop(chicken,contain,335,cal).
prop(meat,contain,250,cal).
prop(shrimp,contain,85,cal).
prop(minced_meat,contain,332,cal).
prop(mayonnaise,contain,188,cal).
prop(vinegar,contain,3,cal).
prop(chocolate,contain,137,cal).
%prop(,contain,,cal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(cheese,not,lunch).
prop(yogurt,not,lunch).
prop(boiled_egg,not,lunch).
prop(boiled_egg,not,dinner).
prop(spanish_omelette,not,lunch).
prop(spanish_omelette,not,dinner).
prop(croissant,not,lunch).
prop(chicken_caesar_salad,not,breakfast).
prop(chicken_caesar_salad,not,dinner).
prop(pizza,not,breakfast).
prop(shrimp_pasta,not,breakfast).
prop(shrimp_pasta,not,dinner).
prop(pasta_negresco,not,breakfast).
prop(pasta_negresco,not,dinner).
prop(pasta_alfredo,not,breakfast).
prop(pasta_alfredo,not,dinner).
prop(pasta_red_sauce,not,breakfast).
prop(pasta_red_sauce,not,dinner).
prop(pasta_white_sauce,not,breakfast).
prop(pasta_white_sauce,not,dinner).
prop(fried_chicken,not,breakfast).
prop(fried_chicken,not,dinner).
prop(grilled_chicken,not,breakfast).
prop(grilled_chicken,not,dinner).
prop(lasagne,not,breakfast).
prop(lasagne,not,dinner).
prop(ratatouille,not,breakfast).
prop(ratatouille,not,dinner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




isValid(Q):-
            Q = [how,many,calories,does,_,contain];
            Q = [what,does,_,contain];
            Q = [can,i,have,_,for,_];
            Q = [what,is,_];
            Q = [how,many,calories,do,i,have,left];
            Q = [what,kind,of,_,does,_,contain];
            Q = [is,_,a,_,in,_];
            Q = [what,can,i,have,for,that,_,contains,_];
            Q = [i,ate,_,for,_];
            Q = [i,do,not,eat,_].

filterProp(X,R):-
             setof((M,N),prop(M,X,N),R).
             
matchFirst(_,[],[]).
matchFirst(T1,[(Lf1,Lf2)|Tf],[(E-Occ)|Tm]):-
                 E=Lf2,
                                 T1=Lf1,
                                 Occ = 1,
                                 matchFirst(T1,Tf,Tm).
matchFirst(T1,[(Lf1,Lf2)|Tf],[(E-Occ)|Tm]):-
                 E=Lf2,
                                 T1\=Lf1,
                                 Occ = 0,
                                 matchFirst(T1,Tf,Tm).

matchSecond(_,[],[]).
matchSecond(T1,[(Lf1,Lf2)|Tf],[(E-Occ)|Tm]):-
                 E=Lf1,
                                 T1=Lf2,
                                 Occ = 1,
                                 matchSecond(T1,Tf,Tm).
matchSecond(T1,[(Lf1,Lf2)|Tf],[(E-Occ)|Tm]):-
                 E=Lf1,
                                 T1\=Lf2,
                                 Occ = 0,
                                 matchSecond(T1,Tf,Tm).

mergeMatchLists(ML1,ML2,R):-
                 removee(ML1,L1),
                                 removee(ML2,L2),
                                 unione(L1,L2,U),
                                 helper1(ML1,U,Q1),
                                 helper1(ML2,U,Q2),
                                 helper(Q1,Q2,R).
removee([],[]).
removee([(X-_)|T],R):-
                        member((X-_),T),
                        removee(T,R).
removee([(X-_)|T],R):-
                        \+member((X-_),T),
                        removee(T,R1),
                        R =[(X-0)|R1].

unione([],L2,L2).
unione([(X-_)|T],L2,U):-
                                                \+member((X-_),L2),
                                                unione(T,L2,U1),
                                                U =[(X-0)|U1].
unione([(X-_)|T],L2,U):-
                                                member((X-_),L2),
                                                unione(T,L2,U).

helper1(_,[],[]).
helper1(L,[H|T],Q):-
        helper2(L,H,H1),
                helper1(L,T,Q1),
                Q=[H1|Q1].

helper2([],X,X).
helper2([(X-Y)|T],(X-W),H):-
                 H1 is Y+W,
                 helper2(T,(X-H1),H).
helper2([(X-_)|T],(Z-W),H):-
                Z\=X,
                 helper2(T,(Z-W),H).


re(_,[],[]).
re((X-Y),[(X-_)|T],R):-
                                re((X-Y),T,R).
re((X-Y),[(Z-W)|T],R):-
                                X\=Z,
                                re((X-Y),T,R1),
                                R =[(Z-W)|R1].

helper([],X,X).
helper([H|T],L,Q):-
          h(H,L,H1),
                  re(H,L,L0),
                  helper(T,L0,Q1),
                  Q=[H1|Q1].

h(X,[],X).
h((X-W),[(X-Y)|T],H):-
                        H1 is Y+W,
                        h((X-H1),T,H).

h((X-W),[(Z-_)|T],H):-
          X\=Z,
                  h((X-W),T,H).


bestMatches(Z,Y):-
                           getmax(Z,M),
                           helper20(Z,M,Y).
helper20([],_,[]).
helper20([(X-Y)|T],M,[X|T1]):-
                       Y==M,
                       helper20(T,M,T1).
helper20([(_-Y)|T],M,S):-
                       Y\=M,
                       helper20(T,M,S).
bestMatchesMin([],_,[]).
bestMatchesMin([(X-Y)|T],Z,[X|B]):-
                             Y==Z,
                             bestMatchesMin(T,Z,B).
bestMatchesMin([(_-Y)|T],Z,B):-
                             Y\=Z,
                             bestMatchesMin(T,Z,B).

getmax(X,Y):-
             getmax1(X,0,Y).
getmax1([],Z,Z).
getmax1([(_-Y)|T],Z,M):-
                   Y>=Z,
                   getmax1(T,Y,M).
getmax1([(_-Y)|T],Z,M):-
                       Y<Z,
                   getmax1(T,Z,M).
foodCal(F,C):-
              prop(F,contain,C,cal).
foodCal(F,M):-
              setof(C,prop(F,contain,C),List),
              h20(List,0,M).
h20([],Z,Z).
h20([X|T],Y,Z):-
          prop(X,contain,W,cal),
          Y1 is Y+W,
          h20(T,Y1,Z).
foodCalL(X,Y):-
               foodCalList(X,0,Y).
foodCalList([],Y,Y).
foodCalList([X|T],Z,Y):-
                setof(C,prop(X,contain,C),List),
                 h20(List,0,M),
                 Z1 is Z+M,
                 foodCalList(T,Z1,Y).
totalCal(1800).

calcCalories(F,M,R,X):-
             totalCal(Y),
             helper1(F,M,R,Y,X).
             
helper1(F,[],[],B,X):-
                      foodCal(F,M),
                      X is B-M.
helper1(F,[[i,ate,Z,for,_]|T],[["Ok"]|W],B,X):-
                                 foodCal(Z,M),
                                 V is B-M,
                                 helper1(F,T,W,V,X).
helper1(F,[[i,ate,_,for,_]|T],[Q|W],B,X):-
                                 Q\=["Ok"],
                                 helper1(F,T,W,B,X).


helper1(F,[[can,i,have,Z,for,_]|T],[["You",can,have,Z,for,_]|W],B,X):-
                                 foodCal(Z,M),
                                 V is B-M,
                                 helper1(F,T,W,V,X).

helper1(F,[[can,i,have,_,for,_]|T],[Q|W],B,X):-
                                 Q\=["You",can,have,_,for,_],
                                 helper1(F,T,W,B,X).
helper1([how,many,calories,do,i,have,left],[],[],M,M).

helper1([how,many,calories,do,i,have,left],[[how,many,calories,do,i,have,left]|T],[[Z,_]|W],_,X):-
                                 helper1([how,many,calories,do,i,have,left],T,W,Z,X).

getDiffAnswer(Q,PQ,PR,CR,R):-
                               getDiff1(Q,PQ,PR,PR1),
                               getDiff2(PR1,CR,R).
getDiff1(_,[],[],[]).
getDiff1(Q,[H|T],[[R1]|T2],[[R1]|PR1]):-
                               H = Q,
                               getDiff1(Q,T,T2,PR1).
getDiff1(Q,[H|T],[[_]|T2],PR1):-
                               H \= Q,
                               getDiff1(Q,T,T2,PR1).


%getDiff2(_,[],[]).
getDiff2(PR1,[H1|_],[H1]):-
                        \+member([H1],PR1).
getDiff2(PR1,[H1|T1],R):-
                         member([H1],PR1),
                         getDiff2(PR1,T1,R).
                         
perm([],[]).
perm([H|T],L) :- perm(T,P), insert(H,P,L).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).


listOrderDesc(X,Sorted):-
                    perm(X,Sorted),is_sorted(Sorted).

is_sorted([]).
is_sorted([_]).
is_sorted([(_-X),(Q-Y)|T]):-
                            X>=Y,
                            is_sorted([(Q-Y)|T]).
                            
getUnlikedIngredients([],[]).
getUnlikedIngredients([[i,do,not,eat,M]|T],[M|R]):-
                                                getUnlikedIngredients(T,R).
getUnlikedIngredients([H|T],R):-
                                                H\= [i,do,not,eat,_],
                                                getUnlikedIngredients(T,R).
foodFromHistory([],[]).

foodFromHistory([H|T],FL):-
                                        (H=[i,ate,X,for,_];
                                        H=[you,can,have,X,for,_]),
                                        foodFromHistory(T,FL1),
                                        FL=[X|FL1].
foodFromHistory([H|T],FL):-
                                        H\=[i,ate,X,for,_],
                                        H\=[you,can,have,X,for,_],
                                        foodFromHistory(T,FL).

% ------------------------------------------------------------------------------
response(Q,_,_,["I",do,not,know]):-
                                   Q=[how,many,calories,does,F,contain],
                                   \+prop(F,contain,_),
                                   \+prop(F,_,_,_).

response(Q,_,_,R):-
                     Q=[how,many,calories,does,F,contain],
                     \+prop(F,contian,_),
                     prop(F,contain,R1,cal),
                     R=[R1,"Calories"].
response(Q,_,_,R):-
                     Q=[how,many,calories,does,F,contain],
                     \+prop(F,_,_,_),
                     foodCal(F,Z),
                     R= [Z,"Calories"].
% ------------------------------------------------------------------------------

response(Q,PQ,PR,R):-
                     Q=[what,does,F,contain],
                     setof(C,prop(F,contain,C),CR),
                     getDiffAnswer(Q,PQ,PR,CR,R), R \= [].

response(Q,_,_,["I",do,not,know]):-
                              Q=[what,does,F,contain],
                              \+prop(F,contain,_).

response(Q,PQ,PR,["I",told,you,that,before]):-
                                 Q=[what,does,F,contain],
                                 setof(C,prop(F,contain,C),CR),
                                  \+getDiffAnswer(Q,PQ,PR,CR,_).
                     
% ------------------------------------------------------------------------------

we(Q,[H|_],[X|_],X):-
                 Q=H.
we(Q,[H|T],[_|Y],Z):-
                    Q\=H,
                    we(Q,T,Y,Z).


bol(F,M):-
              setof(C,prop(F,contain,C),M).
% ------------------------------------------------------------------------------
response([is,F,a,G,in ,H],PQ,_,["YES"]) :-
                        \+member([is,F,a,G,in ,H],PQ),
                        prop(F,is,G),
                        bol(H,M),
                        member(F,M).
response([is,F,a,G,in ,H],PQ,_,["NO"]) :-
                              \+member([is,F,a,G,in ,H],PQ),
                              prop(F,is,G),
                              bol(H,M),
                              \+member(F,M).
response([is,F,a,G,in ,H],PQ,_,["I",do,not,know]) :-
                              \+member([is,F,a,G,in ,H],PQ),
                              \+prop(F,is,G).
response([is,F,a,G,in ,H],PQ,_,["I",told,you,before]) :-
                              member([is,F,a,G,in ,H],PQ).

% ------------------------------------------------------------------------------

boll(F,C,O):-
          setof(M,(prop(M,contain,F),\+prop(M,not,C)),O).
                                               
response(F,_,_,["I",can,not,understand,you]) :-

                  \+isValid(F).

%response(Q,PQ,PR,["I",told,you,before]):-
 %                   Q\=
  %                  Q\=[what,does,_,contain],
   %                 member(Q,PQ).
% ------------------------------------------------------------------------------
 response(Q,_,_,["Ok"]):-
                    Q=[i,ate,_,for,_].
% ------------------------------------------------------------------------------
response(Q,PQ,PR,R):-
                     Q=[what,is,F],
                     \+member(Q,PQ),
                     prop(F,is,R1),
                     R=[R1].
response(Q,PQ,PR,R):-
                       Q=[what,is,F],
                       \+prop(F,is,_),
                       R=["I",do, not,know].
response(Q,PQ,PR,R):-
                      Q=[what,is,F],
                      prop(F,is,_),
                      member(Q,PQ),
                      R=["I",told,you,that,before].
% ------------------------------------------------------------------------------
lko([],[],0).
lko([H|T],[R|T1],B):-
                                                H=[i,ate,X,for,_],
                                                R=["Ok"],
                                                foodCal(X,Z),
                                                lko(T,T1,B1),
                                                B is B1+Z.
lko([H|T],[R|T1],B):-
                                                H=[can,i,have,X,for,Y],
                                                R=["You",can,have,X,for,Y],
                                                foodCal(X,Z),
                                                lko(T,T1,B1),
                                                B is B1+Z.

lko([H|T],[R|T1],B):-

                                                (H\= [i,ate,X,for,Y],
                                                H\=[can,i,have,X,for,Y]),
                                                lko(T,T1,B).

lko([H|T],[R|T1],B):-

                                                H=[can,i,have,X,for,Y],
                                                R\=["You",can,have,X,for,Y],
                                                lko(T,T1,B).
lko([H|T],[R|T1],B):-
                                                H=[i,ate,X,for,_],
                                                R\=["Ok"],
												lko(T,T1,B).
response(Q,PQ,PR,R):-
                      Q=[How,many,calories,do,i,have,left],
                      lko(PQ,PR,Z),
                      \+checkD(PQ),
                      R1 is 1800-Z,
                      R=[R1,"Calories"].
response(Q,PQ,PR,R):-
                                        Q=[How,many,calories,do,i,have,left],
                                        checkD(PQ),
                                        R=["I",do,not,know].

checkD([H|T]):-
                                H=[i,ate,X,for,_],
                                \+prop(X,_,_).

checkD([H|T]):-
                                H\=[i,ate,X,for,_],
                                checkD(T).
checkD([H|T]):-
                                H=[i,ate,X,for,_],
                                prop(X,_,_),
                                checkD(T).

% ------------------------------------------------------------------------------
whatcontain(F,R):-
                setof(M,prop(F,contain,M),R).

whatkind([],_,[]).
whatkind([H|T],K,[[H]|Z1]):-
                     prop(H,is,K),
                     whatkind(T,K,Z1).
whatkind([H|T],K,Z):-
                     \+prop(H,is,K),
                     whatkind(T,K,Z).
rr([],_,[]).
rr([H|T],Y,Z):-
                     member(H,Y),
                     rr(T,Y,Z).
rr([H|T],Y,[H|Z]):-
                     \+member(H,Y),
                     rr(T,Y,Z).



response(Q,_,_,["I",do,not,know]) :-
Q = [what,kind,of,FC,does,F,contain],
((\+ prop(_,_,FC));
(\+prop(F,_,_))).

response(Q,PQ,PR,["Nothing", from, what, i, know]) :-
                Q = [what,kind,of,FC,does,F,contain],
                prop(_,_,FC),
                prop(F,_,_),
                whatcontain(F,L1),
                whatkind(L1,FC,L2),
                L2=[].

response(Q,PQ,PR,R) :-
                Q = [what,kind,of,FC,does,F,contain],
                prop(_,_,FC),
                prop(F,_,_),
                whatcontain(F,L1),
                whatkind(L1,FC,L2),
                rr(L2,PR,L3),
                L3\=[],
                L3=[R|_].
response(Q,PQ,PR,["I",told,you,that,before]) :-
                Q = [what,kind,of,FC,does,F,contain],
                prop(_,_,FC),
                prop(F,_,_),
                filterProp(contain,L1),
                filterProp(is,L2),
                matchFirst(F,L1,R1),
                matchSecond(FC,L2,R2),
                mergeMatchLists(R1,R2,L3),
                bestMatchesMin(L3,2,CR),
                length(CR,N),
                N >= 1,
                \+getDiffAnswer(Q,PQ,PR,CR,_).
% ------------------------------------------------------------------------------
response(Q,PQ,PR,R):-
                                      Q=[can,i,have,F,for,M],
                                      R1=["You",can,have,F,for,M],
									                    checkll(Q,PQ,PR),
                                      R=["You",can,have,F,for,M].

response(Q,PQ,PR,R):-
                                      Q=[can,i,have,F,for,M],
                                      prop(F,not,_),
                                      \+prop(F,not,M),
									                    \+checkll(Q,PQ,PR),
                                      calcCalories(F,PQ,PR,X),
                                      X>=0,
                                      R=["You",can,have,F,for,M].
response(Q,PQ,PR,R):-
                     Q=[can,i,have,F,for,M],
                     prop(F,not,M),
                     R=[F,is,not,suitable,for,M].

response(Q,PQ,PR,R):-
                     Q=[can,i,have,F,for,M],
                     prop(F,not,_),
                     \+prop(F,not,M),
                     \+checkll(Q,PQ,PR),
					 calcCalories(F,PQ,PR,X),
                     X<0,
                     R=["NO"].
response(Q,PQ,PR,R):-
                     Q=[can,i,have,F,for,M],
                     \+prop(F,not,_),
                     R=["I",do,not,know].
response(Q,PQ,PR,R):-
                    Q=[can,i,have,F,for,M],
                    checkD(PQ),
                    R=["I",do,not,know].
checkll(Q,[],[]).
checkll(Q,[A|T],[R|T1]):-
						A=Q,
						R=["You",can,have,F,for,M].


% ------------------------------------------------------------------------------

readInputTillQuit():-
                                        ws(["Welcome",to,your,personal,assistant,"\n"]),
                                        res(I),
                                        readInputTillQuith([],[],I).

removalOfLast(Q,Z):-
                     last(Q,M),
                     append(Z,[M],Q).

readInputTillQuith(PQ,PR,I):-
                                        I=[quit,.],
                                         report(PQ,PR),
                                        ws(["Bye"]).


readInputTillQuith(PQ,PR,I):-
                                        I\=[quit,.],

                                        removalOfLast(I, II),
                                        response(II,PQ,PR,R),
                                        %write("gawab\n"),
                                        
										ws(R),
                                        write("\n"),
                                        %write(PR),
                                        %write("\n"),
                                        %write(PQ),
                                        %write("\n"),
                                        res(X),
                                        readInputTillQuith([II|PQ],[R|PR],X).

report([],[]).
report(PQ,PR):-
                                                herepB(PQ,PR),
                                                herepL(PQ,PR),
                                                herepD(PQ,PR).
herepB([],[]).
herepB([H|T],[R|T1]):-
                                        ((H= [i,ate,X,for,breakfast],
                                                R=["Ok"]);(H= [can,i,have,X,for,breakfast],
                                                R=["You",can,have,X,for,breakfast])),
                                                ws(["You",had,X,for,breakfast,"\n"]),
                                                herepB(T,T1).
herepB([H|T],[R|T1]):-
                                        ((H\= [i,ate,X,for,breakfast];
                                                R\=["Ok"]);(H\= [can,i,have,X,for,breakfast];
                                                R\=["You",can,have,X,for,breakfast])),

                                                herepB(T,T1).

herepL([],[]).
herepL([H|T],[R|T1]):-
                                        ((H= [i,ate,X,for,lunch],
                                                R=["Ok"]);
												(H= [can,i,have,X,for,lunch],
                                                R=["You",can,have,X,for,lunch])),
                                                ws(["You",had,X,for,lunch,"\n"]).
herepL([H|T],[R|T1]):-
                                        ((H\=[i,ate,X,for,lunch];
                                                R\=["Ok"]);(H\= [can,i,have,X,for,lunch];
                                                R\=["You",can,have,X,for,lunch])),

                                                herepL(T,T1).

herepD([],[]).
herepD([H|T],[R|T1]):-
                                        ((H= [i,ate,X,for,dinner],
                                                R=["Ok"]);(H=[can,i,have,X,for,dinner],
                                                R=["You",can,have,X,for,dinner])),
                                                ws(["You",had,X,for,dinner,"\n"]).
herepD([H|T],[R|T1]):-
                                       (( H\= [i,ate,X,for,dinner];
                                                R\=["Ok"]);(H\= [can,i,have,X,for,dinner];
                                                R\=["You",can,have,X,for,dinner])),

                                                herepD(T,T1).

%-------------------------------------------------------------
% res(-Sentence)
%-------------------------------------------------------------

res([FirstWord|RestOfSentence]) :-
  reSe([FirstWord|RestOfSentence]).

reSe([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord(Char,FirstWord,NextChar),
  readRestOfSentence(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res -------------------------
   readRestOfSentence(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence(_,Char,[NextWord|RestOfSentence]) :-
     readWord(Char,NextWord,NextChar),
     readRestOfSentence(NextWord,NextChar,RestOfSentence).

   readWord(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord(Char,Word,NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord(_,Word,NextChar) :-
     get0(TempChar),
     readWord(TempChar,Word,NextChar).

   restWord(Char,[NewChar|RestWord],NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar).
     restWord(Char,[],Char).

   singleCharWord(44).  /* , */
   singleCharWord(59).  /* ; */
   singleCharWord(58).  /* : */
   singleCharWord(63).  /* ? */
   singleCharWord(33).  /* ! */
   singleCharWord(46).  /* . */

   componentChar(Char,Char) :- Char>96,Char<123.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar(Char,Char) :- Char>47,Char<58.
   componentChar(39,39).  /* ' */
   componentChar(45,45).  /* - */
   componentChar(95,95).  /* _ */

    endOfSentenceWord(X) :- X='.';X='?';X='!'.
    %endOfSentenceWord('!').
    %endOfSentenceWord('?').

%-------------------------------------------------------------
% res_pc(-Sentence)
%-------------------------------------------------------------

res_pc([FirstWord|RestOfSentence]) :-
  reSe_pc([FirstWord|RestOfSentence]).

reSe_pc([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord_pc(Char,FirstWord,NextChar),
  readRestOfSentence_pc(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res_pc -------------------------
   readRestOfSentence_pc(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence_pc(_,Char,[NextWord|RestOfSentence]) :-
     readWord_pc(Char,NextWord,NextChar),
     readRestOfSentence_pc(NextWord,NextChar,RestOfSentence).

   readWord_pc(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord_pc(Char,Word,NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord_pc(_,Word,NextChar) :-
     get0(TempChar),
     readWord_pc(TempChar,Word,NextChar).

   restWord_pc(Char,[NewChar|RestWord],NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar).
     restWord_pc(Char,[],Char).

   componentChar_pc(Char,Char) :- Char>96,Char<123.

   componentChar_pc(Char,Char) :- Char>64,Char<91.

   componentChar_pc(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar_pc(Char,Char) :- Char>47,Char<58.
   componentChar_pc(39,39).  /* ' */
   componentChar_pc(45,45).  /* - */
   componentChar_pc(95,95).  /* _ */

%-------------------------------------------------------------
% ws(+Sentence)
%-------------------------------------------------------------

ws([F|R]) :-
   write(F),
   wrs(R).

   %--- ancillaries to ws ------------------------
   wrs([F|R]) :-
     write(' '),
     write(F),
     wrs(R).
   wrs([]).

%-------------------------------------------------------------
% space/0
%-------------------------------------------------------------

space :- write(' ').

%-------------------------------------------------------------
% rs(-String)
%-------------------------------------------------------------

rs(S) :-
   get0(C),
   (
      C == -1,  S = [], !, fail;
      C == 10,  S = [], ! ;
      C == 32, !, rs(S);
      !, rs(C,S)
   ).

rs(C,[C|Cs]) :-
   get0(D),
   (
      D == -1,  Cs = [], !, fail;
      D == 10,  Cs = [], ! ;
      D == 32,  Cs = [], ! ;
      !, rs(D,Cs)
   ).


%-------------------------------------------------------------
% wrst(+String)
%-------------------------------------------------------------

wrst([]) :- !.
% wrst([C|Cs]) :- put(C), wrst(Cs).
						
                   	