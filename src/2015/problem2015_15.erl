-module(problem2015_15).
-export([solve1/1, solve2/1]).

-type property_type() :: capacity | durability | flavor | texture | calories.
-type property_value() :: integer().
-type property() :: { property_type(), property_value() }.
-type properties() :: [ property() ].
-type component() :: { string(), properties() }.
-type volume() :: non_neg_integer().
-type recipe() :: [ { component(), volume() } ].

%%% COMMON

-spec parse_property( string() ) -> property_type().
parse_property( "capacity" ) -> capacity;
parse_property( "durability" ) -> durability;
parse_property( "flavor" ) -> flavor;
parse_property( "texture" ) -> texture;
parse_property( "calories" ) -> calories.

-spec parse_properties( [ string() ] ) -> properties().
parse_properties( [] ) -> [];
parse_properties( [ Name, Value | Rest ] ) ->
    Property = { parse_property( Name ), erlang:list_to_integer( Value ) },
    [ Property | parse_properties( Rest ) ].

-spec parse_component( string() ) -> component().
parse_component( Input ) ->
    [ Name | PropertiesStr ] = string:tokens( Input, " :," ),
    Properties = parse_properties( PropertiesStr ),
    { Name, Properties }.

-spec parse_components( string() ) -> [ component() ].
parse_components( Input ) ->
    Lines = string:tokens( Input, "\n" ),
    lists:map( fun parse_component/1, Lines ).

-spec calc_recipe_total_volume( recipe() ) -> volume().
calc_recipe_total_volume( Recipe ) ->
    lists:sum( [ Volume || { _, Volume } <- Recipe ] ).


-spec generate_recipes( [ component() ], volume() ) -> [ recipe() ].
generate_recipes( [], _ ) -> [ [] ];
generate_recipes( [ X | XS ], Sum ) ->
    [ [ { X, N } | Recipe ] || N <- lists:seq( 1, Sum ), Recipe <- generate_recipes( XS, Sum - N ), N + calc_recipe_total_volume( Recipe ) == Sum ].

-spec sum_property_values( { component(), volume() }, properties() ) -> properties().
sum_property_values( { { _Name, Properties }, Volume }, AccProperties ) ->
    UpdatedProperties = [ { Type, Volume * Property } || { Type, Property } <- Properties ],
    case AccProperties of
        [] -> UpdatedProperties;
        _ -> lists:zipwith( fun( { Type, P1 }, { Type, P2 } ) -> { Type, P1 + P2 } end, UpdatedProperties, AccProperties )
    end.

-spec calc_recipe_score( recipe() ) -> non_neg_integer().
calc_recipe_score( Recipe ) ->
    SummedProperties = lists:foldl( fun sum_property_values/2,
                                    [],
                                    Recipe ),
    Score = lists:foldl( fun ( { calories, _ }, Acc )                   -> Acc;
                             ( { _, Property }, _   ) when Property < 0 -> 0;
                             ( { _, Property }, Acc )                   -> Acc * Property
                         end,
                         1,
                         SummedProperties ),
    Score.

-spec find_max_score_recipe( [ recipe() ] ) -> non_neg_integer().
find_max_score_recipe( Recipes ) ->
    lists:max( [ calc_recipe_score( Recipe ) || Recipe <- Recipes ] ).

%%% PART 1

-spec solve1( string() ) -> non_neg_integer().
solve1( Input ) ->
    Components = parse_components( Input ),
    Recipes = generate_recipes( Components, 100 ),
    find_max_score_recipe( Recipes ).

%%% PART 2

solve2( Input ) ->
    0.

%%% TESTS

-include_lib("eunit/include/eunit.hrl").

test_input() ->
    "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
     Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3".

solve1_test_() ->
    [ ?_assertEqual( 62842880, solve1( test_input() ) ) ].

solve2_test_() ->
    [ ?_assertEqual( 57600000, solve2( test_input() ) ) ].
