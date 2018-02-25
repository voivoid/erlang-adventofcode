-module(problem2017_07).
-export([solve1/1, solve2/1]).

-type tower_name() :: nonempty_string().
-type tower_weight() :: integer().
-type tower() :: { tower_name(), { tower_weight(), [ tower_name() ] } }.
-type parent_to_children_map() :: #{ tower_name() := [ tower_name() ] }.
-type child_to_parent_map() :: #{ tower_name() := tower_name() }.
-type tower_to_weight_map() :: #{ tower_name() := tower_weight() }.


-spec parse_tower_and_its_children( [ string() ] ) -> tower().
parse_tower_and_its_children( [ Name, Weight ] ) ->
    { Name, { erlang:list_to_integer( Weight ), [] } };
parse_tower_and_its_children( [ Name, Weight | Children ] ) ->
    { Name, { erlang:list_to_integer( Weight ), Children } }.

-spec parse_input_line( string() ) -> tower().
parse_input_line( Line ) ->
    parse_tower_and_its_children( string:tokens( Line, "()->, " ) ).


-spec make_tower_maps( string() ) -> { parent_to_children_map(), child_to_parent_map(), tower_to_weight_map() }.
make_tower_maps( Input ) ->
    TowerStrs = string:tokens( Input, "\n" ),
    lists:foldl( 
      fun ( Line, { PtC, CtP, Weights } ) ->
              { Parent, { Weight, Children } } = parse_input_line( Line ),
              NewPtC = PtC#{ Parent => Children },
              NewCtP = lists:foldl( fun( Child, ChildrenAcc ) -> ChildrenAcc#{ Child => Parent }
                                    end,
                                    CtP,
                                    Children ),
              NewWeights = Weights#{ Parent => Weight },
              { NewPtC, NewCtP, NewWeights } end,
      { #{}, #{}, #{} },
      TowerStrs ).

-spec find_top_parent_impl( tower_name(), child_to_parent_map() ) -> tower_name().
find_top_parent_impl( Child, ChildToParentMap ) ->
    case maps:get( Child, ChildToParentMap, nokey ) of
        nokey -> Child;
        Parent -> find_top_parent_impl( Parent, ChildToParentMap )
    end.

-spec find_top_parent( parent_to_children_map(), child_to_parent_map() ) -> tower_name().
find_top_parent( ParentToChildrenMap, ChildToParentMap ) ->
    find_top_parent_impl( erlang:hd( maps:keys( ParentToChildrenMap ) ), ChildToParentMap ).

-spec solve1( nonempty_string() ) -> nonempty_string().
solve1( Input ) ->
    { ParentToChildrenMap, ChildToParentMap, _ } = make_tower_maps( Input ),
    find_top_parent( ParentToChildrenMap, ChildToParentMap ).





-spec find_mismatch( [ { integer(), integer() } ] ) -> integer() | none.
find_mismatch( [ { _, Y }, { _, Y } = A, { _, Y } = B | T ] ) -> find_mismatch( [ A, B | T ] );
find_mismatch( [ { E, X }, { _, Y },     { _, Y }     | _ ] ) -> E - ( X - Y );
find_mismatch( [ { _, Y }, { E, X },     { _, Y }     | _ ] ) -> E - ( X - Y );
find_mismatch( [ { _, Y }, { _, Y },     { E, X }     | _ ] ) -> E - ( X - Y );
find_mismatch( _ ) -> none.

-spec get_children( tower_name(), parent_to_children_map() ) -> [ tower_name() ].
get_children( Parent, ParentToChildrenMap ) ->
    maps:get( Parent, ParentToChildrenMap ).

-spec get_weight( tower_name(), tower_to_weight_map() ) -> tower_weight().
get_weight( Tower, Weights ) ->
    maps:get( Tower, Weights ).

-spec find_weight_mismatch( tower_name(), parent_to_children_map(), tower_to_weight_map() ) -> any().
find_weight_mismatch( Parent, ParentToChildrenMap, Weights ) ->
    ParentWeight = get_weight( Parent, Weights ),
    case get_children( Parent, ParentToChildrenMap ) of
        [] -> { ParentWeight, 0 };
        Children ->
            ChildrenWeights = lists:map( fun( Child ) ->
                                                 { ChildWeight, SubWeight } = find_weight_mismatch( Child, ParentToChildrenMap, Weights ),
                                                 { ChildWeight, ChildWeight + SubWeight } end,
                                         Children ),
            case find_mismatch( ChildrenWeights ) of
                none -> { ParentWeight, lists:sum( [ TotalWeight  || { _, TotalWeight } <- ChildrenWeights ] ) };
                Correct -> throw( Correct )
            end
    end.

-spec solve2( nonempty_string() ) -> integer().
solve2( Input ) ->
    { ParentToChildrenMap, ChildToParentMap, Weights } = make_tower_maps ( Input ),
    Top = find_top_parent( ParentToChildrenMap, ChildToParentMap ),
    catch find_weight_mismatch( Top, ParentToChildrenMap, Weights ).
