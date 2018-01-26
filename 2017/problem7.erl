-module(problem7).
-export([solve1/1, solve2/1]).

parseTower( Line ) ->
    [Name, Weight] = string:tokens( Line, " ()" ),
    { Name, Weight }.

parseTowerData( [ Tower ] ) ->
    parseTowerData( [ Tower, "" ] );
parseTowerData( [ Parent, ChildrenStr ] ) ->
    { Name, Weight } = parseTower( Parent ),
    Children = string:tokens( ChildrenStr, " ," ),
    { Name, { Weight, Children } }.

parseInputLine( Line ) ->
    parseTowerData( string:tokens( Line, "->" ) ).

makeParentToChildrenMap( Input ) ->
    TowerStrs = string:tokens( Input, "\n" ),
    Towers = lists:foldl( 
                      fun ( Line, Acc ) ->
                              { Tower, TowerData } = parseInputLine( Line ),
                              Acc#{ Tower => TowerData } end,
                      #{},
                      TowerStrs ),
    Towers.

makeChildToParentMap( ParentToChildrenMap ) ->
    maps:fold( fun ( Parent, { _, Children }, Acc ) ->
                       lists:foldl( fun( Child, Acc ) ->
                                            Acc#{ Child => Parent }
                                    end,
                                    Acc,
                                    Children )
               end,
               #{},         
               ParentToChildrenMap ).

findTopParent( ChildToParentMap, Tower ) ->
    Parent = maps:get( Tower, ChildToParentMap, no_parent ),
    if Parent == no_parent -> Tower;
       Parent /= no_parent -> findTopParent( ChildToParentMap, Parent )
    end.

findTopParent( ChildToParentMap ) ->
    FirstTower = hd( maps:keys( ChildToParentMap ) ),
    findTopParent( ChildToParentMap, FirstTower ).


solve1( Input ) ->
    ParentToChildrenMap = makeParentToChildrenMap ( Input ),
    ChildToParentMap = makeChildToParentMap( ParentToChildrenMap ),
    findTopParent( ChildToParentMap ).

findWeightMismatch( ParentToChildrenMap, Parent ) ->
    Children = maps:get( ParentToChildrenMap, Parent ),
    0.
    

solve2(Input ) ->
    ParentToChildrenMap = makeParentToChildrenMap ( Input ),
    ChildToParentMap = makeChildToParentMap( ParentToChildrenMap ),
    TopParent = findTopParent( ChildToParentMap ),
    findWeightMismatch( ParentToChildrenMap, TopParent ) .
