-module(problem2015_15).
%% -export([solve1/1]).
%% -compile([export_all, nowarn_export_all]).

%% -type property() :: integer().
%% -record( properties, { capacity :: property(), durability :: property(), flavor :: property(), texture :: property(), calories :: property() } ).
%% -type properties() :: #properties{}.
%% -type component() :: { string(), properties() }.
%% -type volume() :: non_neg_integer().
%% -type recipe() :: [ { component(), volume() } ].

%% %%% COMMON

%% -spec parse_properties( [ string() ], #{ string() := property() } ) -> #{ string() := property() }.
%% parse_properties( [], PropertiesMap ) -> PropertiesMap;
%% parse_properties( [ Name, Value | Rest ], PropertiesMap ) ->
%%     UpdatedPropertiesMap = PropertiesMap#{ Name => erlang:list_to_integer( Value ) },
%%     parse_properties( Rest, UpdatedPropertiesMap ).

%% -spec parse_properties( [ string() ] ) -> #{ string() := property() }.
%% parse_properties( Properties ) ->
%%     parse_properties( Properties, #{} ).

%% -spec parse_component( string() ) -> component().
%% parse_component( Input ) ->
%%     [ Name | PropertiesStr ] = string:tokens( Input, " :," ),

%%     #{ "capacity" := Capacity,
%%        "durability" := Durability,
%%        "flavor" := Flavor,
%%        "texture" := Texture,
%%        "calories" := Calories } = parse_properties( PropertiesStr ),

%%     Properties = #properties{ capacity = Capacity, durability = Durability, flavor = Flavor, texture = Texture, calories = Calories },
%%     { Name, Properties }.

%% -spec parse_components( string() ) -> [ component() ].
%% parse_components( Input ) ->
%%     Lines = string:tokens( Input, "\n" ),
%%     lists:map( fun parse_component/1, Lines ).

%% calc_recipe_total_volume( Recipe ) ->
%%     lists:sum( [ Volume || { _, Volume } <- Recipe ] ).


%% -spec generate_recipes( [ component() ], volume() ) -> [ recipe() ].
%% generate_recipes( [], _ ) -> [ [] ];
%% generate_recipes( [ X | XS ], Sum ) ->
%%     [ [ { X, N } | Recipe ] || N <- lists:seq( 1, Sum ), Recipe <- generate_recipes( XS, Sum - N ), N + calc_recipe_total_volume( Recipe ) == Sum ].

%% calc_component_score( { { _Name, #properties{ capacity = Capacity, durability = Durability, flavor = Flavor, texture = Texture } }, Volume },
%%                       #properties{ capacity = AccCapacity, durability = AccDurability, flavor = AccFlavor, texture = AccTexture }
%%                     ) ->
%%     #properties{ capacity = ( Volume * Capacity ) + AccCapacity,
%%                  durability = ( Volume * Durability ) + AccDurability,
%%                  flavor = ( Volume * Flavor ) + AccFlavor,
%%                  texture = ( Volume * Texture ) + AccTexture }.

%% calc_recipe_score( Recipe ) ->
%%     lists:foldl( fun calc_component_score/2,
%%                  #properties{},
%%                  Recipe ),

%% find_max_score_recipe( Recipes ) ->
%%     lists:max( [ calc_recipe_score( Recipe ) || Recipe <- Recipes ] ).

%% %%% PART 1

%% solve1( Input ) ->
%%     Components = parse_components( Input ),
%%     Recipes = generate_recipes( Components, 100 ),
%%     find_max_score_recipe( Recipes ).

%% %%% TESTS

%% -include_lib("eunit/include/eunit.hrl").

%% test_input() ->
%%     "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
%%      Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3".

%% solve1_test_() ->
%%                      [ ?_assertEqual( 62842880, solve1( test_input() ) ) ].
