-module(typechecker_test_panic_ffi).
-export([catch_panic/1]).

catch_panic(Fun) ->
    try
        {ok, Fun()}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.
