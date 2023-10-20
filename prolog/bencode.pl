:- module(bencode, [bencode/2]).
:- use_module(library('dcg/basics'), [integer/3]).
:- use_module(library(plunit), [begin_tests/1, end_tests/1]).
:- use_module(library(when), [when/2]).

%%	bencode(?Term, ?Codes) is semidet.
%
%	True if Codes is the bencoding of Term.  In Term, atoms represent
%	byte strings.  Lists of =|Key-Value|= pairs, sorted by
%	=Key= represent	dictionaries.  Integers and lists represent
%	themselves.
bencode(Term, Codes) :-
    phrase(bval(Term), Codes).

bstring(Atom) -->
    { freeze(Atom, atom(Atom)) },
    { when(ground(Atom);ground(Bytes), atom_codes(Atom,Bytes)) },
    { when(ground(Bytes);ground(Length), length(Bytes,Length)) },
    integer(Length), ":", Bytes,
    !.

bval(I) -->
    { freeze(I, integer(I)) },
    "i", integer(I), "e",
    !.
bval(L) -->
    "l", bvals(L), "e",
    !.
bval(Atom) -->
    bstring(Atom),
    !.

bval(Dict) -->
    { when(ground(Dict), keys_sorted(Dict)) },
    "d",
    bpairs(Dict),
    "e".

bvals([X|Xs]) --> bval(X), bvals(Xs), !.
bvals([]) --> "".

bpairs([K-V|Pairs]) --> bstring(K), bval(V), bpairs(Pairs), !.
bpairs([]) --> "".

keys_sorted(L) :-
    is_list(L),
    keysort(L, L).


:- begin_tests(bencode).
test(spam_encode) :-
    bencode(spam, X),
    X = `4:spam`.
test(spam_decode) :-
    bencode(X, `4:spam`),
    X = spam.

% TODO test arbitrary byte strings

test(int_encode) :-
    bencode(42, X),
    X = `i42e`.
test(int_decode) :-
    bencode(X, `i42e`),
    X = 42.

test(negative_encode) :-
    bencode(-3, X),
    X = `i-3e`.
test(negative_decode) :-
    bencode(X, `i-9e`),
    X = -9.

test(list_encode) :-
    bencode([spam, eggs], X),
    X = `l4:spam4:eggse`.
test(list_decode) :-
    bencode(X, `l4:spam4:eggse`),
    X = [spam, eggs].

test(dictionary_encode) :-
    bencode([cow-moo, spam-eggs], X),
    X = `d3:cow3:moo4:spam4:eggse`.
test(dictionary_decode) :-
    bencode(X, `d3:cow3:moo4:spam4:eggse`),
    X = [cow-moo, spam-eggs].
test(dictionary_list_value) :-
    bencode([spam-[a,b]], `d4:spaml1:a1:bee`).
test(dictionary_longer) :-
    bencode([ publisher-bob
            , 'publisher-webpage'-'www.example.com'
            , 'publisher.location'-home
            ],
            `d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee`
           ).
test(dictionary_nested) :-
    bencode([spam-[a-b,c-d]], `d4:spamd1:a1:b1:c1:dee`).
test(dictionary_nested_broken_encode) :-
    catch(bencode([spam-[a-b,c]], _), error(E, _), true),
    E =@= type_error(pair, c).
test(dictionary_nested_broken_decode, [fail]) :-
    bencode(_, `d4:spamd1:a1:b1:cee`).

test(dictionary_integer_key, [fail]) :-
    bencode([5-spam], `di5e4:spame`).
test(dictionary_list_key, [fail]) :-
    bencode([[a,b]-spam], `dl1:a1:be4:spame`).

:- end_tests(bencode).
