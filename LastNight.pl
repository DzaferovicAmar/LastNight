% Enhanced Zombie Survival Game - Fixed Version
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
:- dynamic(companion/1).
:- dynamic(time/1).
:- dynamic(trust/2).
:- dynamic(skill/2).

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
    retractall(companion(_)),
    retractall(time(_)),
    retractall(trust(_, _)),
    retractall(skill(_, _)),

    % Set up player
    assertz(at(player, abandoned_house)),
    assertz(alive(player)),
    assertz(health(player, 100)),
    assertz(water(50)),
    assertz(ammo(0)),
    assertz(food(0)),
    assertz(time(day)),

    % Set up companions
    assertz(at(toni_montana, safe_building)),
    assertz(alive(toni_montana)),
    assertz(health(toni_montana, 90)),
    assertz(skill(toni_montana, lockpicking)),
    assertz(trust(toni_montana, 50)),
    
    assertz(at(vin_disl, police_station)),
    assertz(alive(vin_disl)),
    assertz(health(vin_disl, 85)),
    assertz(skill(vin_disl, firearms)),
    assertz(trust(vin_disl, 50)),

    % Set up locations and connections
    assertz(connected(abandoned_house, street, door)),
    assertz(connected(street, abandoned_house, door)),
    assertz(connected(street, next_street, walk)),
    assertz(connected(next_street, street, walk)),
    assertz(connected(next_street, safe_building, door)),
    assertz(connected(safe_building, next_street, door)),
    assertz(connected(next_street, police_station, door)),
    assertz(connected(police_station, next_street, door)),
    assertz(connected(next_street, pharmacy, door)),
    assertz(connected(pharmacy, next_street, door)),
    assertz(connected(street, high_point, climb)),
    assertz(connected(high_point, street, climb)),
    assertz(connected(police_station, armory, locked_door)),
    assertz(connected(armory, police_station, door)),
    assertz(connected(high_point, evacuation_point, climb_down)),
    assertz(connected(evacuation_point, high_point, climb_up)),

    % Set up objects
    assertz(at(backpack, abandoned_house)),
    assertz(at(hunting_knife, backpack)),
    assertz(at(pistol, backpack)),
    assertz(at(canned_food, backpack)),
    assertz(at(water_bottle, backpack)),
    assertz(at(radio, backpack)),
    assertz(at(ammo_box, police_station)),
    assertz(at(first_aid_kit, pharmacy)),
    assertz(at(lockpick, safe_building)),
    assertz(at(shotgun, armory)),
    assertz(at(escape_vehicle_keys, high_point)),
    assertz(at(gas_can, street)),

    % Set up zombies
    assertz(zombie_at(zombie1, street)),
    assertz(health(zombie1, 50)),
    assertz(zombie_at(zombie2, next_street)),
    assertz(health(zombie2, 70)),

    assertz(visited(abandoned_house)).

% Start the game
start :-
    initialize,
    instructions,
    intro_story,
    look.

% Display game instructions
instructions :-
    write('Welcome to "Last Night" - Zombie Survival Game'), nl,
    write('=========================================='), nl, nl,
    write('Commands:'), nl,
    write('look.                  -- look around'), nl,
    write('examine(Object).       -- examine an object'), nl,
    write('go(Destination).       -- move to a location'), nl,
    write('take(Object).          -- pick up an object'), nl,
    write('use(Object).           -- use an object'), nl,
    write('attack(Enemy, Weapon). -- attack an enemy'), nl,
    write('inventory.             -- list your items'), nl,
    write('status.                -- check your status'), nl,
    write('talk(Person).          -- talk to someone'), nl,
    write('recruit(Person).       -- recruit a companion'), nl,
    write('help.                  -- show commands'), nl,
    write('quit.                  -- end game'), nl, nl.

% Intro story
intro_story :-
    write('You wake up in an abandoned house. Zombies roam the streets.'), nl,
    write('You need to find the evacuation point and escape before nightfall.'), nl,
    write('Your backpack is on the floor - check it for supplies.'), nl, nl.

% Look around
look :-
    at(player, Location),
    format('Location: ~w~n', [Location]),
    describe_location(Location),
    list_objects_at(Location),
    list_people_at(Location),
    list_connections(Location),
    check_for_zombies(Location).

% Location descriptions
describe_location(abandoned_house) :-
    write('A run-down house with broken windows. Your starting point.'), nl.
describe_location(street) :-
    write('A debris-filled street. Zombies wander around.'), nl.
describe_location(next_street) :-
    write('Another street with abandoned buildings.'), nl.
describe_location(safe_building) :-
    write('A secured building where Toni Montana is hiding.'), nl.
describe_location(police_station) :-
    write('An abandoned police station. Vin Disl is here.'), nl.
describe_location(pharmacy) :-
    write('A looted pharmacy with some medical supplies left.'), nl.
describe_location(armory) :-
    write('The police station armory with weapons.'), nl.
describe_location(high_point) :-
    write('A high building with a view of the evacuation point.'), nl.
describe_location(evacuation_point) :-
    write('The military evacuation point! A transport vehicle awaits.'), nl.
describe_location(backpack) :-
    write('Inside your backpack:'), nl,
    findall(Item, at(Item, backpack), Items),
    forall(member(Item, Items), format('- ~w~n', [Item])).

% List objects at location
list_objects_at(Location) :-
    findall(Object, (at(Object, Location), Object \= player, \+ alive(Object)), Objects),
    (Objects = [] ->
        write('No objects here.'), nl
    ;
        write('Objects here:'), nl,
        forall(member(Object, Objects), format('- ~w~n', [Object]))
    ).

% List people at location
list_people_at(Location) :-
    findall(Person, (at(Person, Location), Person \= player, alive(Person)), People),
    (People = [] ->
        true
    ;
        write('People here:'), nl,
        forall(member(Person, People), format('- ~w~n', [Person]))
    ).

% List connections
list_connections(Location) :-
    findall(Destination-Method, connected(Location, Destination, Method), Connections),
    (Connections = [] ->
        write('No exits.'), nl
    ;
        write('You can go to:'), nl,
        forall(member(Destination-Method, Connections),
               format('- ~w via ~w~n', [Destination, Method]))
    ).

% Check for zombies
check_for_zombies(Location) :-
    findall(Zombie, zombie_at(Zombie, Location), Zombies),
    (Zombies = [] ->
        true
    ;
        write('DANGER: Zombies here!'), nl,
        forall(member(Zombie, Zombies), format('- ~w~n', [Zombie]))
    ).

% Examine objects
examine(Object) :-
    (at(player, Location), at(Object, Location) ; holding(Object)),
    examine_object(Object),
    !.
examine(_) :-
    write('You don\'t see that here.'), nl.

examine_object(backpack) :-
    write('Your survival backpack. Contains:'), nl,
    findall(Item, at(Item, backpack), Items),
    forall(member(Item, Items), format('- ~w~n', [Item])).
examine_object(hunting_knife) :-
    write('A sharp knife for close combat.'), nl.
examine_object(pistol) :-
    write('A pistol. Needs ammo.'), nl.
examine_object(ammo_box) :-
    write('A box with 20 bullets.'), nl.
examine_object(lockpick) :-
    write('Tools for picking locks.'), nl.
examine_object(escape_vehicle_keys) :-
    write('Keys to the escape vehicle!'), nl.
examine_object(gas_can) :-
    write('Fuel for the escape vehicle.'), nl.
examine_object(_) :-
    write('Nothing special.'), nl.

% Take objects
take(Object) :-
    at(player, Location),
    (at(Object, Location) ; at(Object, backpack)),
    retract(at(Object, _)),
    assertz(holding(Object)),
    format('Took ~w.~n', [Object]),
    check_special_item(Object),
    !.
take(_) :-
    write('Can\'t take that.'), nl.

% Special item effects
check_special_item(ammo_box) :-
    ammo(CurrentAmmo),
    NewAmmo is CurrentAmmo + 20,
    retract(ammo(CurrentAmmo)),
    assertz(ammo(NewAmmo)),
    write('Added 20 bullets to inventory.'), nl.
check_special_item(_).

% Use objects
use(Object) :-
    holding(Object),
    use_object(Object),
    !.
use(_) :-
    write('Don\'t have that item.'), nl.

use_object(water_bottle) :-
    water(W), W > 0,
    NewWater is W - 10,
    retract(water(W)),
    assertz(water(NewWater)),
    health(player, H),
    NewHealth is min(100, H + 5),
    retract(health(player, H)),
    assertz(health(player, NewHealth)),
    format('Drank water. Health: ~w, Water: ~w~n', [NewHealth, NewWater]).
use_object(canned_food) :-
    food(F),
    NewFood is F + 30,
    retract(food(F)),
    assertz(food(NewFood)),
    format('Ate food. Food level: ~w~n', [NewFood]).
use_object(lockpick) :-
    at(player, police_station),
    connected(police_station, armory, locked_door),
    retract(connected(police_station, armory, locked_door)),
    assertz(connected(police_station, armory, door)),
    write('Unlocked the armory!'), nl.
use_object(gas_can) :-
    at(player, evacuation_point),
    holding(escape_vehicle_keys),
    write('You fuel up the vehicle and start the engine!'), nl,
    win_game.
use_object(_) :-
    write('Can\'t use that now.'), nl.

% Move between locations
go(Destination) :-
    at(player, Location),
    connected(Location, Destination, Method),
    (Method = locked_door ->
        write('That door is locked.'), nl
    ;
        move_player(Location, Destination)
    ),
    !.
go(_) :-
    write('Can\'t go there.'), nl.

move_player(OldLocation, NewLocation) :-
    retract(at(player, OldLocation)),
    assertz(at(player, NewLocation)),
    % Move companions too
    forall(companion(Companion), (
        at(Companion, OldLocation) ->
        (retract(at(Companion, OldLocation)),
         assertz(at(Companion, NewLocation))) ; true
    )),
    format('Went to ~w.~n', [NewLocation]),
    look.

% Attack zombies
attack(Zombie, Weapon) :-
    at(player, Location),
    zombie_at(Zombie, Location),
    (holding(Weapon) ; at(Weapon, Location)),
    attack_with_weapon(Zombie, Weapon),
    !.
attack(_, _) :-
    write('Can\'t attack that.'), nl.

attack_with_weapon(Zombie, hunting_knife) :-
    health(Zombie, Health),
    NewHealth is max(0, Health - 30),
    retract(health(Zombie, Health)),
    assertz(health(Zombie, NewHealth)),
    format('Attacked ~w with knife!~n', [Zombie]),
    check_zombie_death(Zombie, NewHealth).
attack_with_weapon(Zombie, pistol) :-
    ammo(Ammo),
    (Ammo > 0 ->
        NewAmmo is Ammo - 1,
        retract(ammo(Ammo)),
        assertz(ammo(NewAmmo)),
        health(Zombie, Health),
        NewHealth is max(0, Health - 50),
        retract(health(Zombie, Health)),
        assertz(health(Zombie, NewHealth)),
        format('Shot ~w! Ammo: ~w~n', [Zombie, NewAmmo]),
        check_zombie_death(Zombie, NewHealth)
    ;
        write('No ammo!'), nl
    ).

check_zombie_death(Zombie, Health) :-
    (Health =< 0 ->
        retract(zombie_at(Zombie, _)),
        format('Killed ~w!~n', [Zombie])
    ;
        format('~w has ~w health left.~n', [Zombie, Health])
    ).

% Talk to people
talk(Person) :-
    at(player, Location),
    at(Person, Location),
    talk_to(Person),
    !.
talk(_) :-
    write('That person isn\'t here.'), nl.

talk_to(toni_montana) :-
    write('Toni: "I can pick locks and find supplies. Want to team up?"'), nl.
talk_to(vin_disl) :-
    write('Vin: "Former military. I know weapons. We should work together."'), nl.

% Recruit companions
recruit(Person) :-
    at(player, Location),
    at(Person, Location),
    \+ companion(Person),
    assertz(companion(Person)),
    format('~w joined your group!~n', [Person]),
    !.
recruit(Person) :-
    companion(Person),
    format('~w is already with you.~n', [Person]).
recruit(_) :-
    write('Can\'t recruit that person.'), nl.

% Show inventory
inventory :-
    write('Carrying:'), nl,
    forall(holding(Object), format('- ~w~n', [Object])),
    (\+ holding(_) -> write('Nothing.'), nl ; true).

% Show status
status :-
    health(player, Health),
    water(Water),
    food(Food),
    ammo(Ammo),
    format('Health: ~w, Water: ~w, Food: ~w, Ammo: ~w~n', [Health, Water, Food, Ammo]).

% Win condition
win_game :-
    write('======================================'), nl,
    write('CONGRATULATIONS! YOU ESCAPED!'), nl,
    write('======================================'), nl,
    write('You drive away from the zombie-infested city.'), nl,
    forall(companion(C), format('~w escaped with you!~n', [C])),
    write('THE END'), nl.

% Utility commands
help :- instructions.
quit :- write('Thanks for playing!'), nl.
restart :- start.

% Shortcuts
i :- inventory.
l :- look.