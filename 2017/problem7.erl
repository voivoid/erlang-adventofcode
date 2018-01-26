-module(problem7).
-export([solve1/1, solve2/1]).

parseTower( Line ) ->
    [ Name, Weight ] = string:tokens( Line, " ()" ),
    { Name, Weight }.

parseTowerChildren( [ Tower ] ) ->
    parseTowerChildren( [ Tower, "" ] );
parseTowerChildren( [ Parent, ChildrenStr ] ) ->
    { Name, Weight } = parseTower( Parent ),
    Children = string:tokens( ChildrenStr, " ," ),
    { Name, { Weight, Children } }.

parseInputLine( Line ) ->
    parseTowerChildren( string:tokens( Line, "->" ) ).

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

findTopParent( ChildToParentMap, Parent ) ->
    NextParent = maps:get( Parent, ChildToParentMap, no_parent ),
    if NextParent == no_parent -> Parent;
       NextParent /= no_parent -> findTopParent( ChildToParentMap, NextParent )
    end.

findTopParent( ChildToParentMap ) ->
    FirstTower = hd( maps:keys( ChildToParentMap ) ),
    findTopParent( ChildToParentMap, FirstTower ).


solve1( Input ) ->
    { _, ChildToParentMap } = makeTowerMaps ( Input ),
    findTopParent( ChildToParentMap ).

makeWeightsMap( ParentToChildrenMap, Parent ) ->
    case maps:get( Parent, ParentToChildrenMap ) of
        { Weight, [] } -> #{Parent => { Weight, [] } };
        { Weight, Children } -> { ChildrenWeights, WeightsMap } = lists:mapfoldl( fun( Child, WeightsMap ) ->
                                                                                          
                                                                                  end,
                                                                                  #{},
                                                                                  Children ),
                                WeightsMap#{ Parent => { Weight, ChildrenWeights } }
    end.

findWeightMismatch( _WeightsMap, _Parent ) ->
    0.
    

solve2( Input ) ->
    { ParentToChildrenMap, ChildToParentMap } = makeTowerMaps ( Input ),
    TopParent = findTopParent( ChildToParentMap ),
    WeightsMap = makeWeightsMap( ParentToChildrenMap, TopParent ),
    findWeightMismatch( WeightsMap, TopParent ).
