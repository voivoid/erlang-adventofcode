-module(problem5a).
-export([solve/1]).

jump(_, Len, Idx, Counter) when Idx < 0; Idx >= Len -> Counter;
jump(Offsets, Len, Idx, Counter) ->
    Offset = array:get( Idx, Offsets ),
    UpdatedOffsets = array:set( Idx, Offset + 1, Offsets ),
    jump( UpdatedOffsets , Len, Idx + Offset, Counter + 1 ).
    

solve(Input) ->
    Offsets = array:from_list( lists:map( fun list_to_integer/1, string:tokens( Input, "\n" ) ) ),
    jump(Offsets, array:size(Offsets), 0, 0).
