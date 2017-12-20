The Prolog file `num.pl` provides a way to get from
`198374628243618` to

> сто девяносто восемь биллионов триста семьдесят четыре миллиарда
> шестьсот двадцать восемь миллионов двести сорок три тысячи
> шестьсот восемнадцать

– that is, it describes the relationship between a number and its
Russian name. At the moment, it doesn't seem to work as reliably
in the other direction – for example, an input of `["две"
"тысячи"]`, solving for the numeral representation, seems to
result in non-termination.

### Usage

Consult `[num]` and use the `number_write(N)` predicate to write
to the prompt the Russian name of any integer N (0 < N < 10^36).

    ?- [num].
    true.
    
    ?- number_write(1).
    один 
    true.
    
    ?- number_write(12).
    двенадцать 
    true.
    
    ?- number_write(47).
    сорок семь 
    true.
    
    ?- number_write(288).
    двести восемьдесят восемь 
    true.
    
    ?- number_write(2002001).
    два миллиона два тысячи один 
    true.
