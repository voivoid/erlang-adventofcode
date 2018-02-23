-module(problem7).
-export([solve1/1, solve2/1]).

parseTowerAndItsChildren( [ Name, Weight ] ) ->
    { Name, { Weight, [] } };
parseTowerAndItsChildren( [ Name, Weight | Children ] ) ->
    { Name, { Weight, Children } }.

parseInputLine( Line ) ->
    parseTowerAndItsChildren( string:tokens( Line, "()->, " ) ).

makeTowerMaps( Input ) ->
    TowerStrs = string:tokens( Input, "\n" ),
    lists:foldl( 
      fun ( Line, { PtC, CtP, Weights } ) ->
              { Parent, { Weight, Children } } = parseInputLine( Line ),
              NewPtC = PtC#{ Parent => Children },
              NewCtP = lists:foldl( fun( Child, ChildrenAcc ) -> ChildrenAcc#{ Child => Parent }
                                    end,
                                    CtP,
                                    Children ),
              NewWeights = Weights#{ Parent => erlang:list_to_integer( Weight ) },
              { NewPtC, NewCtP, NewWeights } end,
      { #{}, #{}, #{} },
      TowerStrs ).

findTopParentImpl( Child, ChildToParentMap ) ->
    case maps:get( Child, ChildToParentMap, nokey ) of
        nokey -> Child;
        Parent -> findTopParentImpl( Parent, ChildToParentMap )
    end.

findTopParent( ParentToChildrenMap, ChildToParentMap ) ->
    findTopParentImpl( erlang:hd( maps:keys( ParentToChildrenMap ) ), ChildToParentMap ).

solve1( Input ) ->
    { ParentToChildrenMap, ChildToParentMap, _ } = makeTowerMaps( Input ),
    findTopParent( ParentToChildrenMap, ChildToParentMap ).





findMismatch( [{_,Y},{_,Y},{_,Y}|T] ) -> findMismatch( [Y,Y|T] );
findMismatch( [{E,X},{_,Y},{_,Y}|_] ) -> E - ( X - Y );
findMismatch( [{_,Y},{E,X},{_,Y}|_] ) -> E - ( X - Y );
findMismatch( [{_,Y},{_,Y},{E,X}|_] ) -> E - ( X - Y );
findMismatch( _ ) -> none.


getChildren(Parent, ParentToChildrenMap) ->
    maps:get( Parent, ParentToChildrenMap ).

getWeight(Tower, Weights) ->
    maps:get( Tower, Weights ).

findWeightMismatch( Parent, ParentToChildrenMap, Weights ) ->
    ParentWeight = getWeight( Parent, Weights ),
    case getChildren( Parent, ParentToChildrenMap ) of
        [] -> { ParentWeight, 0 };
        Children -> 
            ChildrenWeights = lists:map( fun( Child ) ->
                                                 { ChildWeight, SubWeight } = findWeightMismatch( Child, ParentToChildrenMap, Weights ),
                                                 { ChildWeight, ChildWeight + SubWeight } end,
                                         Children ),
            case findMismatch( ChildrenWeights ) of
                none -> { ParentWeight, lists:sum( [ TotalWeight  || { _, TotalWeight } <- ChildrenWeights ] ) };
                Correct -> throw(Correct)
            end
    end.

solve2( Input ) ->
    { ParentToChildrenMap, ChildToParentMap, Weights } = makeTowerMaps ( Input ),
    Top = findTopParent( ParentToChildrenMap, ChildToParentMap ),
    catch findWeightMismatch( Top, ParentToChildrenMap, Weights ).
