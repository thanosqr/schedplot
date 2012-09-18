-module(fib).

-export([fib/2]).

-spec fib(pos_integer(), pos_integer()) -> 'ok'.

fib(N, M) ->
    L = lists:seq(1, M),
    Self = self(),
    _ = [spawn(fun() -> fib_w(Self, N) end) || _ <- L],
    _ = [receive ok -> ok end || _ <- L],
    ok.

fib_w(PID, N) ->
    schedplot:print(pid_to_list(self())),
    fib(N),
    schedplot:print(pid_to_list(self())),
    PID ! ok.

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).
