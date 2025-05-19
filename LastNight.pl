% Zombie Survival Game
% Based on "Letzte Nacht" scenario

:- dynamic(at/2).
:- dynamic(holding/1).
:- dynamic(alive/1).
:- dynamic(health/2).
:- dynamic(zombie_at/2).
:- dynamic(ammo/1).
:- dynamic(food/1).
:- dynamic(water/1).
:- dynamic(visited/1).
:- dynamic(connected/3).

% Initial game state
initialize :-
    retractall(at(_, _)),
    retractall(holding(_)),
    retractall(alive(_)),
    retractall(health(_, _)),
    retractall(zombie_at(_, _)),
    retractall(ammo(_)),
    retractall(food(_)),
    retractall(water(_)),
    retractall(visited(_)),
    retractall(connected(_, _, _)),

    % Set up player
    assertz(at(player, abandoned_house)),
    assertz(alive(player)),
    assertz(health(player, 100)),
    assertz(water(50)), % Half-full water bottle
    assertz(ammo(0)),
    assertz(food(0)),

    % Set up locations
    assertz(connected(abandoned_house, street, door)),
    assertz(connected(street, abandoned_house, door)),
    assertz(connected(street, car, walk)),
    assertz(connected(car, street, walk)),
    assertz(connected(street, next_street, walk)),
    assertz(connected(next_street, safe_building, door)),
    assertz(connected(next_street, street, walk)),
    assertz(connected(safe_building, next_street, door)),
    assertz(connected(street, high_point, climb)),
    assertz(connected(high_point, street, climb)),


    % Set up objects
    assertz(at(backpack, abandoned_house)),
    assertz(at(hunting_knife, backpack)),
    assertz(at(pistol, backpack)),
    assertz(at(canned_food, backpack)),
    assertz(at(water_bottle, backpack)),

    % Set up zombies
    assertz(zombie_at(zombie1, street)),
    assertz(health(zombie1, 50)),

    assertz(visited(abandoned_house)).

% Start the game
start :-
    initialize,
    instructions,
    look.

% Display game instructions
instructions :-
    write('Welcome to "Letzte Nacht" - Zombie Survival Game'), nl,
    write('=============================================='), nl, nl,
    write('You wake up in an abandoned house. The sun shines weakly through the cracks of the windows.'), nl,
    write('It\'s quiet, but the silence feels uncomfortable. Outside, you can hear the distant rumbling of a pack of zombies.'), nl,
    write('Your stomach growls, and you realize you urgently need food and water.'), nl,
    write('Your backpack lies on the floor. It\'s time to make decisions.'), nl, nl,
    write('Available commands:'), nl,
    write('look.                 -- to look around you.'), nl,
    write('examine(Object).      -- to examine an object closely.'), nl,
    write('go(Direction).        -- to move in a direction.'), nl,
    write('take(Object).         -- to pick up an object.'), nl,
    write('drop(Object).         -- to put down an object.'), nl,
    write('use(Object).          -- to use an object.'), nl,
    write('attack(Enemy, Weapon).-- to attack an enemy with a weapon.'), nl,
    write('inventory.            -- to list what you are carrying.'), nl,
    write('status.               -- to check your health and resources.'), nl,
    write('wait.                 -- to wait and see what happens.'), nl,
    write('help.                 -- to see this message again.'), nl,
    write('quit.                 -- to end the game.'), nl, nl.

% Look around the current location
look :-
    at(player, Location),
    format('Your location: ~w.~n~n', [Location]),
    (visited(Location) -> true ; assertz(visited(Location))),
    describe_location(Location),
    list_objects_at(Location),
    list_connections(Location).

% Describe locations
describe_location(abandoned_house) :-
    write('You are in an abandoned house. The sun shines weakly through the cracks of the windows.'), nl,
    write('It\'s quiet, but the silence feels uncomfortable.'), nl.
describe_location(backpack) :-
    write('You are examining your backpack. It\'s not full, but you have a few things that might help you.'), nl.
describe_location(street) :-
    write('You are on the street. The air smells of smoke and decay.'), nl,
    write('You see several streetlights, but they are all broken.'), nl.
describe_location(car) :-
    write('You are at an abandoned car. It looks like it might still be in good condition.'), nl,
    write('Maybe it\'s your ticket to survival.'), nl.
describe_location(next_street) :-
    write('You are on another street. The buildings around you look abandoned.'), nl.
describe_location(safe_building) :-
    write('You found an abandoned building that seems like a safe shelter.'), nl,
    write('There are supplies and an old bed.'), nl,
    write('But as you settle in, you hear the hiss of flickering lights and the sound of creaking floors.'), nl.
describe_location(high_point) :-
    write('You are at a high point overlooking the area.'), nl,
    write('From here, you can see the extent of the zombie infestation.'), nl.
describe_location(_) :-
    write('You are in an unknown location.'), nl.

% List objects at current location
list_objects_at(Location) :-
    findall(Object, (at(Object, Location), Object \= player), Objects),
    (Objects = [] ->
        write('There are no objects here.'), nl
    ;
        write('You see the following objects:'), nl,
        forall(member(Object, Objects), format('- ~w~n', [Object]))
    ).

% List connections from current location
list_connections(Location) :-
    findall(Destination-Method, connected(Location, Destination, Method), Connections),
    (Connections = [] ->
        write('There are no connections from here.'), nl
    ;
        write('You can go to the following locations:'), nl,
        forall(member(Destination-Method, Connections),
               format('- ~w via ~w~n', [Destination, Method]))
    ).

% Check for zombies
check_for_zombies(Location) :-
    zombie_at(Zombie, Location),
    health(Zombie, Health),
    format('⚠️ DANGER: A ~w with health ~w is approaching you!~n~n', [Zombie, Health]),
    fail.
check_for_zombies(_).

% Examine an object
examine(Object) :-
    at(player, Location),
    at(Object, Location),
    examine_object(Object),
    !.
examine(Object) :-
    holding(Object),
    examine_object(Object),
    !.
examine(_) :-
    write('You don\'t see that here.'), nl.

% Object descriptions
examine_object(backpack) :-
    write('A sturdy backpack that can hold your supplies.'), nl,
    write('You should check what\'s inside.'), nl.
examine_object(hunting_knife) :-
    write('A sharp hunting knife. Good for close combat with zombies.'), nl.
examine_object(pistol) :-
    write('A semi-automatic pistol. It\'s empty, but could be useful if you find ammo.'), nl.
examine_object(canned_food) :-
    write('Some canned food. It will help you survive longer.'), nl.
examine_object(water_bottle) :-
    write('A half-full water bottle. Essential for survival.'), nl.
examine_object(_) :-
    write('Nothing special about this object.'), nl.

% Take an object
take(Object) :-
    at(player, Location),
    at(Object, Location),
    retract(at(Object, Location)),
    assertz(holding(Object)),
    format('You took the ~w.~n', [Object]),
    !.
take(Object) :-
    at(Object, backpack),
    at(player, Location),
    at(backpack, Location),
    retract(at(Object, backpack)),
    assertz(holding(Object)),
    format('You took the ~w from your backpack.~n', [Object]),
    !.
take(_) :-
    write('You can\'t take that.'), nl.

% Drop an object
drop(Object) :-
    holding(Object),
    at(player, Location),
    retract(holding(Object)),
    assertz(at(Object, Location)),
    format('You dropped the ~w.~n', [Object]),
    !.
drop(_) :-
    write('You don\'t have that.'), nl.

% Use an object
use(Object) :-
    holding(Object),
    use_object(Object),
    !.
use(_) :-
    write('You don\'t have that item or can\'t use it now.'), nl.

% Object usage
use_object(water_bottle) :-
    water(CurrentWater),
    (CurrentWater > 0 ->
        NewWater is max(0, CurrentWater - 10),
        retract(water(CurrentWater)),
        assertz(water(NewWater)),
        write('You drink some water. It was refreshing.'), nl,
        format('Water remaining: ~w%~n', [NewWater])
    ;
        write('The water bottle is empty.'), nl
    ).
use_object(canned_food) :-
    food(CurrentFood),
    NewFood is CurrentFood + 30,
    retract(food(CurrentFood)),
    assertz(food(NewFood)),
    write('You eat some canned food. You feel more energetic.'), nl,
    format('Food energy: ~w~n', [NewFood]).
use_object(_) :-
    write('You can\'t use that item right now.'), nl.

% Attack a zombie
attack(Zombie, Weapon) :-
    at(player, Location),
    zombie_at(Zombie, Location),
    (holding(Weapon) ; at(Weapon, Location)),
    attack_with_weapon(Zombie, Weapon),
    !.
attack(_, _) :-
    write('You can\'t attack that.'), nl.

% Attack with different weapons
attack_with_weapon(Zombie, hunting_knife) :-
    health(Zombie, Health),
    Damage is 20,
    NewHealth is max(0, Health - Damage),
    retract(health(Zombie, Health)),
    assertz(health(Zombie, NewHealth)),
    format('You attacked ~w with your hunting knife for ~w damage!~n', [Zombie, Damage]),
    check_zombie_death(Zombie, NewHealth).
attack_with_weapon(Zombie, pistol) :-
    ammo(Ammo),
    (Ammo > 0 ->
        NewAmmo is max(0, Ammo - 1),
        retract(ammo(Ammo)),
        assertz(ammo(NewAmmo)),
        health(Zombie, Health),
        Damage is 50,
        NewHealth is max(0, Health - Damage),
        retract(health(Zombie, Health)),
        assertz(health(Zombie, NewHealth)),
        format('You shot ~w with your pistol for ~w damage! Ammo remaining: ~w~n', [Zombie, Damage, NewAmmo]),
        check_zombie_death(Zombie, NewHealth)
    ;
        write('Your pistol is out of ammo!'), nl
    ).
attack_with_weapon(_, _) :-
    write('You can\'t attack with that.'), nl.

% Check if a zombie is dead
check_zombie_death(Zombie, Health) :-
    (Health =< 0 ->
        retract(zombie_at(Zombie, _)),
        format('You killed the ~w!~n', [Zombie])
    ;
        format('The ~w now has ~w health.~n', [Zombie, Health])
    ).

% Move to a new location
go(Destination) :-
    at(player, Location),
    connected(Location, Destination, _),
    retract(at(player, Location)),
    assertz(at(player, Destination)),
    format('You went to ~w.~n', [Destination]),
    look,
    !.
go(_) :-
    write('You can\'t go there from here.'), nl.

% Show inventory
inventory :-
    write('You are carrying:'), nl,
    list_inventory.

% List inventory items
list_inventory :-
    holding(Object),
    format('- ~w~n', [Object]),
    fail.
list_inventory :-
    \+ holding(_),
    write('Nothing.'), nl.
list_inventory.

% Show player status
status :-
    (health(player, Health) -> true ; Health = 0),
    (water(Water) -> true ; Water = 0),
    (food(Food) -> true ; Food = 0),
    (ammo(Ammo) -> true ; Ammo = 0),
    nl,
    write('===== Your Status ====='), nl,
    format('Health: ~w%~n', [Health]),
    format('Water: ~w%~n', [Water]),
    format('Food: ~w~n', [Food]),
    format('Ammo: ~w~n', [Ammo]),
    write('======================'), nl, nl.

% Wait and see what happens
wait :-
    write('You wait for a while. Nothing happens.'), nl.

% Custom commands for the game's specific mechanics
check_backpack :-
    at(player, Location),
    at(backpack, Location),
    retract(at(player, Location)),
    assertz(at(player, backpack)),
    look,
    !.
check_backpack :-
    write('There\'s no backpack here to check.'), nl.

% Look out window
look_window :-
    at(player, abandoned_house),
    write('You cautiously peek out the window.'), nl,
    write('There are a few zombies on the street outside.'), nl,
    write('They seem to be heading in your direction, but they are still far away.'), nl,
    write('You notice an abandoned car by the roadside that still looks in good condition.'), nl,
    write('Maybe it\'s your ticket to survival.'), nl,
    !.
look_window :-
    write('There\'s no window here to look out of.'), nl.

% Add ammo to the game
find_ammo(Amount) :-
    ammo(CurrentAmmo),
    NewAmmo is CurrentAmmo + Amount,
    retract(ammo(CurrentAmmo)),
    assertz(ammo(NewAmmo)),
    format('You found ~w bullets! Total ammo: ~w~n', [Amount, NewAmmo]).

% Helper predicates
help :- instructions.

% End the game
quit :-
    write('Thanks for playing!'), nl.

% Aliases for common commands
n :- go(north).
s :- go(south).
e :- go(east).
w :- go(west).
i :- inventory.
l :- look.
x(Object) :- examine(Object).