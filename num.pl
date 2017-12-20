% Russian numbers

:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(library(func)).
:- use_module(library(list_util)).
:- set_prolog_flag(double_quotes, chars).

:- [num_agree].

%% number_write(+N)
% number_write/1 is an impure predicate that writes the
% Russian graphical representation of the number N as a
% string to the prompt.
number_write(N) :-
  ( N #< 10^36
  ; N #>= 10^36,
    format('N has to be lower than 10^36~n'),
    fail),
  rep_agreed(N, Rep),
  maplist(flipf(append, " "), Rep, Rep_spaced),
  foldl(flipf(append), Rep_spaced, "", Chars),
  format('~w~n', [string_chars(~, Chars)]),
  !.


%% flipf(+Pred, ?B, ?A)
%% flipf(+Pred, ?B, ?A, ?C)
% flipf/2 and flipf/3 call Pred with the remaining
% arguments, but flips the first two. As an example,
% flipf(append, "2", "1", "12") = append("1", "2", "12").
flipf(Pred, B, A) :- call(Pred, A, B).
flipf(Pred, B, A, C) :- call(Pred, A, B, C).


%% rep_naive(+N, -Rep)
% rep_naive/2 describes the relationship between the number N
% and Rep - Rep being a list of strings, representing the
% "naive" Russian graphical representation of that number.
% The representation is "naive" because it lacks proper case
% agreement.
rep_naive(N, Rep) :-
  number_digits_asc(N, Digits_asc),
  reverse(Digits_asc, Digits_desc),
  length(Digits_desc, StartingPlace),
  phrase(representation(StartingPlace, Digits_desc), Rep).


%% rep_agreed(+N, -Rep)
% Via rep_naive/2 and agreed/4, rep_agreed/2 describes the
% relationship between the number N and Rep, a list of
% strings that when combined make up the complete and
% correct Russian name for that number.
rep_agreed(N, Rep) :-
  rep_naive(N, Rep_naive),
  Rep_naive = [First | _],
  phrase(agreed(First, Rep_naive), Rep).


%% representation(+Place, +Digits) --> Rep

% representation/3 is a DCG representing a list of strings
% that when combined make up the "naive" (that is, without
% proper case agreement) name for the number whose digits,
% in order, are elements of the list Digits, where the first
% digit's place (units, tens, hundreds, etc.) is equal to
% the variable Place.

% Special cases are described for the places 1, 2 and 3
% (that is, from units up to hundreds). The last two clauses
% of representation/3 handle all other places (that is,
% thousands and above).

representation(_, []) --> [].

representation(Place, [0 | Digits]) --> % skip zeros
  { succ(Place0, Place) },
  representation(Place0, Digits).

representation(1, [Unit]) --> % 1-9
  { place_digits_representation(1, [Unit], Word) },
  [Word].

representation(2, [1, Unit]) --> % 10-19
  { place_digits_representation(2, [1, Unit], Word) },
  [Word].

representation(2, [Ten | Digits]) --> % 2n-9n
  { dif(Ten, 1), % exclude 10-19
    place_digits_representation(2, [Ten], Word) },
  [Word],
  representation(1, Digits).

representation(3, [Hundred | Digits]) --> % 1nn-9nn
  { place_digits_representation(3, [Hundred], Word) },
  [Word],
  representation(2, Digits).

representation(Place, [1 | Digits]) --> % 1 thousand, 1 million, 1 milliard, ...
  { Place #> 3,
    succ(Place0, Place),
    Place0 rem 3 #= 0,
    place_digits_representation(Place, [1], Word) },
  [Word],
  representation(Place0, Digits).

representation(Place, Digits) --> % X thousand, X million, ... (X > 1)
  { Place #> 3,
    Digits = [Digit | _],
    Digit #> 0,
    (Place - 1) rem 3 #= R,
    if_(R = 0        % if X = Digit and Digit =< 9
        , Digit #> 1 % then exclude 1 thousand, 1 million, ...
        , R #> 0),   % otherwise X > 9
    Place0 #= Place - R,
    succ(R, Place_prefix),
    split_at(Place_prefix, Digits, Digits_prefix, Digits_suffix0),
    append([1], Digits_suffix0, Digits_suffix) },
  representation(Place_prefix, Digits_prefix), % X
  representation(Place0, Digits_suffix). % [one] thousand/million/... + rest


%% place_digits_representation(+Place, +DigitList, -Word)

% place_digits_representation/3 describes the relationship
% between the place of a given digit and its name. For
% example, for the number 300, the digit is 3, its place 2
% and its name in Russian "триста".

% In almost all cases, the digit is represented by the first
% element of DigitList. Only in the case of the numbers 10
% through 19 does DigitList consist of more than one digit,
% namely the ten and the unit - this in order to separate
% the numbers 10-19 from the rest of the tens (20-90).

% Units:

place_digits_representation(1, [1], "один").
place_digits_representation(1, [2], "два").
place_digits_representation(1, [3], "три").
place_digits_representation(1, [4], "четыре").
place_digits_representation(1, [5], "пять").
place_digits_representation(1, [6], "шесть").
place_digits_representation(1, [7], "семь").
place_digits_representation(1, [8], "восемь").
place_digits_representation(1, [9], "девять").

% Tens:

place_digits_representation(2, [1,0], "десять").
place_digits_representation(2, [1,2], "двенадцать").
place_digits_representation(2, [1,4], "четырнадцать").
place_digits_representation(2, [1, Unit], Word_10) :- % 10-20
  maplist(dif(Unit), [0,2,4]), % except 10, 12 and 14
  place_digits_representation(1, [Unit], Word_1),
  without_soft_sign(Word_1, Prefix),
  append(Prefix, "надцать", Word_10).

place_digits_representation(2, [4], "сорок").
place_digits_representation(2, [9], "девяносто").
place_digits_representation(2, [Ten], Word_10) :-
  place_digits_representation(1, [Ten], Prefix),
  (
    between(2, 3, Ten),
    append(Prefix, "дцать", Word_10)
  ;   between(5, 8, Ten),
      append(Prefix, "десят", Word_10)
  ).

% Hundreds:

place_digits_representation(3, [1], "сто").
place_digits_representation(3, [2], "двести").
place_digits_representation(3, [Hundred], Word_100) :-
  place_digits_representation(1, [Hundred], Prefix),
  ( between(3, 4, Hundred),
    append(Prefix, "ста", Word_100)
  ; between(5, 9, Hundred),
    append(Prefix, "сот", Word_100)).

% Thousands, millions, milliards, ... : 10 ^ (1 + Place)

place_digits_representation(4, [1], "тысяча").
place_digits_representation(7, [1], "миллион").
place_digits_representation(10, [1], "миллиард").
place_digits_representation(13, [1], "биллион").
place_digits_representation(16, [1], "биллиард").
place_digits_representation(19, [1], "триллион").
place_digits_representation(22, [1], "триллиард").
place_digits_representation(25, [1], "квадриллион").
place_digits_representation(28, [1], "квадриллиард").
place_digits_representation(31, [1], "квинтиллион").
place_digits_representation(34, [1], "квинтиллиард").


% Auxiliary predicates:

%% number_digits_asc(+N, -Ds)
% number_digits_asc/2 uses the DCG reverse_digits/3 to
% generate for the number N the list D of all its digits, in
% ascending order sorted by the their place values.
number_digits_asc(N, Ds) :-
  N #> 0,
  phrase(reverse_digits(N, Ds), Ds).

reverse_digits(0, _) --> [].
reverse_digits(N, [_|Ds]) -->
  { N #> 0,
    D #= N mod 10,
    N1 #= N div 10 }, % e.g., 43 = 436 (6 = D)
  [D],
  reverse_digits(N1, Ds).


%% without_soft_sign(+In, -Out)
% without_soft_sign/2 is a simple predicate that describes
% the relationship between the word In and the word Out,
% where Out is the exact same word as In, except that any
% potential soft sign (ь) has been chopped off its end.
without_soft_sign(In, In) :-
  \+ append(_, "ь", In).
without_soft_sign(In, Out) :-
  append(Out, "ь", In).
