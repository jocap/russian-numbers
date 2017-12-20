%% :- module(num_agree,
%%           [agreed/4
%%           ]).

/** <module> Russian number agreement

@author John Ankarström
*/

% Agreement

%% agreed(+Head, +Rep) --> Rep_agreed
% From the inputs Head (the agreed version of Rep's first element) and Rep (a
% list of strings representing the "naive" -- that is, unagreed -- Russian name
% for a number), the DCG agreed/3 describes a new version of Rep, Rep_agreed,
% with correct case agreement, accomplished via two_words_agreed/2.
agreed(W0, [_]) --> [W0].
agreed(W0, [V1, V2 | Vs]) -->
  { two_words_agreed([V1, V2], [_, W2]) },
  [W0],
  agreed(W2, [V2 | Vs]).

%% two_words_agreed(+Head_Dependant_naive, -Head_Dependant_agreed)
%% two_words_agreed(-Head_Dependant_naive, +Head_Dependant_agreed)
% two_words_agreed/2 describes the relationship between two pairs of words,
% potentially head-dependant pairs, the first of which is "naive" and the second
% of which has proper case agreement. Note that the two pairs may be equivalent.

% два is the only ordinal with a feminine form.
two_words_agreed(["два", "тысяча"], ["две", "тысячи"]).

% Defines all the [Head, Dependant] pairs, for each of which
% the dependant is declined in either the nominative
% singular, genitive singular or genitive plural case.
two_words_agreed([Head, Dependant], [Head, Dependant_agreed]) :-
  if_(Head = "два", dif(Dependant, "тысяча"), true), % exclude ["два", "тысяча"]

  place_digits_representation(Place_head, Digits_head, Head),
  place_digits_representation(Place_dependant, _, Dependant),

  Place_head #< Place_dependant, % "million billions" is fine, but not "billion millions"
  Place_dependant #> 3, % dependant has to be at least a thousand
  (Place_dependant - 1) rem 3 #= 0, % dependant has to be thousand / million / milliard / ...

  ( % head = 1-9 -> nominative / genitive singular / genitive plural
    Place_head #= 1,
    Digits_head = [X],
    ( X #= 1,
      Dependant = Dependant_agreed % nominative
    ; between(2, 4, X),
      features_undeclined_declined([g, sg], Dependant, Dependant_agreed) % genitive singular
    ; X #> 4,
      features_undeclined_declined([g, pl], Dependant, Dependant_agreed)) % genitive plural
  % head > 9 -> genitive plural
  ; Place_head #> 1,
    features_undeclined_declined([g, pl], Dependant, Dependant_agreed)). % genitive plural


% Defines all the [Word1, Word2] pairs that exhibit no improper case agreement
% in their default, "naive" form -- that is, they are in the nominative case and
% they *should* be, because they are not actual head-dependant pairs.
two_words_agreed([Word1, Word2], [Word1, Word2]) :-
  place_digits_representation(Place1, _, Word1),
  place_digits_representation(Place2, _, Word2),
  ( Place1 #>= Place2
  ; Place1 #< Place2,
    ( Place2 #=< 3
    ; Place2 #> 3,
      (Place2 - 1) rem 3 #> 0)).

%% features_undeclined_declined(+Features, +Undeclined, -Declined)
%% features_undeclined_declined(+Features, -Undeclined, +Declined)
%% features_undeclined_declined(-Features, ?Undeclined, +Declined)

% features_undeclined_declined/3 describes the relationship between the
% undeclined word Undeclined and the word Declined that is declined according to
% values for the grammatical categories described in Features.

% Features = [Case, Number], where Number is either sg (singular) or pl (plural)
% and Case is one of:
% - n = nominative
% - a = accusative
% - g = genitive
% - l = locative (prepositional)
% - d = dative
% - i = instrumental

% тысяча:
features_undeclined_declined([Case, Number], "тысяча", Declined) :-
  suffix(3, [Case, Number], Suffix),
  append("тысяч", Suffix, Declined).

% -ллион, -ллиард
features_undeclined_declined([Case, Number], Undeclined, Declined) :-
  ( append(_, "ллион", Undeclined)
  ; append(_, "ллиард", Undeclined)),
  suffix(1, [Case, Number], Suffix),
  append(Undeclined, Suffix, Declined).

%% suffix(+Declination, +Features, -Suffix)
% suffix/3 describes the proper Suffix for a given noun Declination category (1
% or 3) with a given list of grammatical Features.

suffix(1, [n, sg], "").
suffix(1, [a, sg], "").
suffix(1, [g, sg], "а").
suffix(1, [l, sg], "е").
suffix(1, [d, sg], "у").
suffix(1, [i, sg], "ом").
suffix(1, [n, pl], "ы").
suffix(1, [a, pl], "ы").
suffix(1, [g, pl], "ов").
suffix(1, [l, pl], "ах").
suffix(1, [d, pl], "ам").
suffix(1, [i, pl], "ами").

suffix(3, [n, sg], "").
suffix(3, [a, sg], "у").
suffix(3, [g, sg], "и").
suffix(3, [l, sg], "е").
suffix(3, [d, sg], "е").
suffix(3, [i, sg], "ью").
suffix(3, [n, pl], "и").
suffix(3, [a, pl], "и").
suffix(3, [g, pl], "").
suffix(3, [l, pl], "ах").
suffix(3, [d, pl], "ам").
suffix(3, [i, pl], "ами").
