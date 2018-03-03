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

              {
                PtC#{ Parent => Children },
                lists:foldl( fun( Child, ChildrenAcc ) -> ChildrenAcc#{ Child => Parent } end, CtP, Children ),
                Weights#{ Parent => Weight } 
              }
      end,
      { #{}, #{}, #{} },
      TowerStrs ).

-spec find_base_tower_impl( tower_name(), child_to_parent_map() ) -> tower_name().
find_base_tower_impl( Child, ChildToParentMap ) ->
    case maps:get( Child, ChildToParentMap, nokey ) of
        nokey -> Child;
        Parent -> find_base_tower_impl( Parent, ChildToParentMap )
    end.

-spec find_base_tower( parent_to_children_map(), child_to_parent_map() ) -> tower_name().
find_base_tower( ParentToChildrenMap, ChildToParentMap ) ->
    find_base_tower_impl( erlang:hd( maps:keys( ParentToChildrenMap ) ), ChildToParentMap ).

-spec solve1( nonempty_string() ) -> nonempty_string().
solve1( Input ) ->
    { ParentToChildrenMap, ChildToParentMap, _ } = make_tower_maps( Input ),
    find_base_tower( ParentToChildrenMap, ChildToParentMap ).



-spec find_mismatch( [ { integer(), integer() } ] ) -> integer() | none.
find_mismatch( [ { _, Y }, { A, Y }, { B, Y } | T ] ) -> find_mismatch( [ { A, Y }, { B, Y } | T ] );
find_mismatch( [ { E, X }, { _, Y }, { _, Y } | _ ] ) -> E + Y - X;
find_mismatch( [ { _, Y }, { E, X }, { _, Y } | _ ] ) -> E + Y - X;
find_mismatch( [ { _, Y }, { _, Y }, { E, X } | _ ] ) -> E + Y - X;
find_mismatch( _ ) -> none.

-spec get_children( tower_name(), parent_to_children_map() ) -> [ tower_name() ].
get_children( Parent, ParentToChildrenMap ) ->
    maps:get( Parent, ParentToChildrenMap ).

-spec get_weight( tower_name(), tower_to_weight_map() ) -> tower_weight().
get_weight( Tower, Weights ) ->
    maps:get( Tower, Weights ).

-spec get_children_load_weights( [ tower_name() ], parent_to_children_map(), tower_to_weight_map() ) -> [ { tower_weight(), tower_weight() } ].
get_children_load_weights( Children, ParentToChildrenMap, Weights ) ->
    lists:map( fun( Child ) -> calc_tower_load_weight( Child, ParentToChildrenMap, Weights ) end, Children ).


-spec calc_tower_load_weight( tower_name(), parent_to_children_map(), tower_to_weight_map() ) -> { tower_weight(), tower_weight() }.
calc_tower_load_weight( Tower, ParentToChildrenMap, Weights ) ->
    TowerWeight = get_weight( Tower, Weights ),
    case get_children( Tower, ParentToChildrenMap ) of
        [] -> { TowerWeight, 0 };
        Children ->
            LoadWeights = get_children_load_weights( Children, ParentToChildrenMap, Weights ),
            TotalWeights = lists:map( fun ( { ChildWeight, LoadWeight } ) -> { ChildWeight, ChildWeight + LoadWeight } end, LoadWeights ),
            case find_mismatch( TotalWeights ) of
                none -> { TowerWeight, lists:sum( [ TotalWeight  || { _, TotalWeight } <- TotalWeights ] ) };
                Correct -> throw( Correct )
            end
    end.

-spec solve2( nonempty_string() ) -> integer().
solve2( Input ) ->
    { ParentToChildrenMap, ChildToParentMap, Weights } = make_tower_maps ( Input ),
    Base = find_base_tower( ParentToChildrenMap, ChildToParentMap ),
    catch calc_tower_load_weight( Base, ParentToChildrenMap, Weights ).


-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "pbga (66)
     xhth (57)
     ebii (61)
     havc (66)
     ktlj (57)
     fwft (72) -> ktlj, cntj, xhth
     qoyq (66)
     padx (45) -> pbga, havc, qoyq
     tknk (41) -> ugml, padx, fwft
     jptl (61)
     ugml (68) -> gyxo, ebii, jptl
     gyxo (61)
     cntj (57)".

solve1_test_() ->
    [ ?_assertEqual( "tknk" , solve1( test_input() ) ) ].


solve2_test_() ->
    [ ?_assertEqual( 60, solve2( test_input() ) ) ].
