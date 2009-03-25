-module(test).
-export([primes/1]).

primes(N) -> sieve(lists:seq(2,N)).

sieve([H|T]) -> [H|sieve(remdups(H, T))];
sieve([]) -> [].

remdups(N, H) -> remdups(N, H, []).
remdups(N, [H|T], R) when H rem N =:= 0 -> remdups(N, T, R);
remdups(N, [H|T], R) -> remdups(N, T, [H|R]);
remdups(_, [], R) -> lists:reverse(R).

%% EOF
