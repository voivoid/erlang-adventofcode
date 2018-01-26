-module(problem7).
-export([solve1/1, solve2/1]).

parseTower( Line ) ->
    [ Name, Weight ] = string:tokens( Line, " ()" ),
    { Name, Weight }.

parseTowerWithChildren( [ Tower ] ) ->
    parseTowerWithChildren( [ Tower, "" ] );
parseTowerWithChildren( [ Parent, ChildrenStr ] ) ->
    { Name, Weight } = parseTower( Parent ),
    Children = string:tokens( ChildrenStr, " ," ),
    { Name, { Weight, Children } }.

parseInputLine( Line ) ->
    parseTowerWithChildren( string:tokens( Line, "->" ) ).

makeTowerMaps( Input ) ->
    TowerStrs = string:tokens( Input, "\n" ),
    lists:foldl( 
      fun ( Line, { PtWC, CtP } ) ->
              { Parent, { Weight, Children } } = parseInputLine( Line ),
              NewPtWC = PtWC#{ Parent => { Weight, Children } },
              NewCtp = lists:foldl( fun( Child, ChildrenAcc ) -> ChildrenAcc#{ Child => Parent }
                                    end,
                                    CtP,
                                    Children ),
              { NewPtWC , NewCtp } end,
      { #{}, #{} },
      TowerStrs ).

findTopParent( ChildToParentMap, Child ) ->
    case maps:get( Child, ChildToParentMap, nokey ) of
        nokey -> Child;
        Parent -> findTopParent( ChildToParentMap, Parent )
    end.

findTopParent( ChildToParentMap ) ->
    FirstTower = hd( maps:keys( ChildToParentMap ) ),
    findTopParent( ChildToParentMap, FirstTower ).


solve1( Input ) ->
    { _, ChildToParentMap } = makeTowerMaps ( Input ),
    findTopParent( ChildToParentMap ).

solve2( _Input ) ->
    0.
