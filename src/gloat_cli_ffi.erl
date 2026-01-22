-module(gloat_cli_ffi).
-export([write_file/2]).

write_file(Path, Data) ->
  case file:write_file(Path, Data) of
    ok -> {ok, nil};
    {error, Reason} -> {error, Reason}
  end.
