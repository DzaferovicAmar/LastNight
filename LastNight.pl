% Enhanced Zombie Survival Game
% Based on "Letzte Nacht" scenario
% Expanded with characters from the story sketch

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
:- dynamic(companion/1).    % Track companions (Toni, Vin)
:- dynamic(time/1).         % Track game time (day/night cycle)
:- dynamic(trust/2).        % Track trust levels with companions
:- dynamic(skill/2).        % Track special skills of companions

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
    assertz(water(50)), % Half-full water bottle
    assertz(ammo(0)),
    assertz(food(0)),
    assertz(time(day)),

    % Set up companions (not with player initially)
    assertz(at(toni_montana, safe_building)),
    assertz(alive(toni_montana)),
    assertz(health(toni_montana, 90)),
    assertz(skill(toni_montana, lockpicking)),
    assertz(skill(toni_montana, scavenging)),
    assertz(trust(toni_montana, 50)),
    
    assertz(at(vin_disl, police_station)),
    assertz(alive(vin_disl)),
    assertz(health(vin_disl, 85)),
    assertz(skill(vin_disl, firearms)),
    assertz(skill(vin_disl, tactics)),
    assertz(trust(vin_disl, 50)),

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
    assertz(connected(next_street, police_station, door)),
    assertz(connected(police_station, next_street, door)),
    assertz(connected(next_street, pharmacy, door)),
    assertz(connected(pharmacy, next_street, door)),
    assertz(connected(police_station, armory, locked_door)),
    assertz(connected(armory, police_station, door)),

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

    % Set up zombies
    assertz(zombie_at(zombie1, street)),
    assertz(health(zombie1, 50)),
    assertz(zombie_at(zombie2, next_street)),
    assertz(health(zombie2, 70)),
    assertz(zombie_at(zombie_horde, pharmacy)),
    assertz(health(zombie_horde, 150)),

    assertz(visited(abandoned_house)).

% Start the game
start :-
    initialize,
    instructions,
    intro_story,
    look.

% Display game instructions
instructions :-
    write('Welcome to "Last Night" - Enhanced Zombie Survival Game'), nl,
    write('=============================================='), nl, nl,
    write('The world has fallen into chaos. A deadly virus has turned most of the population into mindless zombies.'), nl,
    write('You need to escape the city before nightfall, when zombies become even more aggressive.'), nl,
    write('Your decisions will determine who survives and how far you get.'), nl,
    write('Every choice matters—will you fight, sneak, or negotiate with other survivors?'), nl,
    write('Trust is fragile, and danger lurks around every corner.'), nl, nl,
    write('Available commands:'), nl,
    write('look.                  -- to look around you.'), nl,
    write('examine(Object).       -- to examine an object closely.'), nl,
    write('go(Direction).         -- to move in a direction.'), nl,
    write('take(Object).          -- to pick up an object.'), nl,
    write('drop(Object).          -- to put down an object.'), nl,
    write('use(Object).           -- to use an object.'), nl,
    write('attack(Enemy, Weapon). -- to attack an enemy with a weapon.'), nl,
    write('inventory.             -- to list what you are carrying.'), nl,
    write('status.                -- to check your health and resources.'), nl,
    write('talk(Person).          -- to talk to a person.'), nl,
    write('recruit(Person).       -- to try to recruit a companion.'), nl,
    write('wait.                  -- to wait and see what happens.'), nl,
    write('advance_time.          -- to advance time (day->night or night->day).'), nl,
    write('check_companions.      -- to check your companions\' status.'), nl,
    write('help.                  -- to see this message again.'), nl,
    write('quit.                  -- to end the game.'), nl, nl.

% Intro story
intro_story :-
    write('You wake up in an abandoned house. The sun shines weakly through the cracks of the windows.'), nl,
    write('It\'s quiet, but the silence feels uncomfortable. Outside, you can hear the distant rumbling of zombies.'), nl,
    write('Your radio crackles, picking up a garbled transmission:'), nl,
    write('"This is... evacuation center... military convoy leaving at... midnight..."'), nl,
    write('The message cuts off. You check your watch: it\'s early morning. You have limited time to find the evacuation point.'), nl,
    write('Your stomach growls, and you realize you urgently need food and water.'), nl,
    write('Your backpack lies on the floor. It\'s time to make decisions.'), nl,
    write('The city is dangerous, especially at night when the zombies become more aggressive.'), nl,
    write('You remember hearing about other survivors in the area - maybe they can help?'), nl, nl.

% Look around the current location
look :-
    at(player, Location),
    format('Your location: ~w.~n~n', [Location]),
    (visited(Location) -> true ; assertz(visited(Location))),
    describe_location(Location),
    list_objects_at(Location),
    list_people_at(Location),
    list_connections(Location),
    check_for_zombies(Location).

% Describe locations
describe_location(abandoned_house) :-
    write('You are in an abandoned house. The sun shines weakly through the cracks of the windows.'), nl,
    write('It\'s quiet, but the silence feels uncomfortable.'), nl,
    write('There\'s evidence of hasty departure - half-packed bags and scattered belongings.'), nl.
describe_location(backpack) :-
    write('You are examining your backpack. It\'s not full, but you have a few things that might help you.'), nl.
describe_location(street) :-
    time(Time),
    write('You are on the street. The air smells of smoke and decay.'), nl,
    write('You see several streetlights, but they are all broken.'), nl,
    (Time = night -> 
        write('In the darkness, the shadows seem to move. Strange noises echo from all directions.'), nl
    ;
        write('The daylight reveals the devastation of the city. Abandoned cars and debris litter the road.'), nl
    ).
describe_location(car) :-
    write('You are at an abandoned car. It looks like it might still be in good condition.'), nl,
    write('Maybe it\'s your ticket to survival, if you can find the keys.'), nl.
describe_location(next_street) :-
    write('You are on another street. The buildings around you look abandoned.'), nl,
    write('A pharmacy sign hangs loosely from one building. A police station is nearby.'), nl.
describe_location(safe_building) :-
    write('You found a building that appears to have been secured as a shelter.'), nl,
    write('There are supplies and an old bed. Someone has reinforced the windows and doors.'), nl,
    write('As you settle in, you hear the hiss of flickering lights and the sound of creaking floors.'), nl.
describe_location(high_point) :-
    write('You are at a high point overlooking the area.'), nl,
    write('From here, you can see the extent of the zombie infestation.'), nl,
    write('In the distance, you spot what might be military vehicles at the city outskirts.'), nl,
    write('That must be the evacuation point mentioned in the radio transmission.'), nl.
describe_location(police_station) :-
    write('You are in an abandoned police station. Papers are scattered everywhere.'), nl,
    write('Most equipment has been taken, but there might still be useful items.'), nl,
    write('A heavy door marked "Armory" is locked with a keypad.'), nl.
describe_location(pharmacy) :-
    write('You are in a ransacked pharmacy. Most shelves are empty, but a few items remain.'), nl,
    write('The back area looks like it hasn\'t been completely looted.'), nl,
    write('There\'s dried blood on the floor and signs of a struggle.'), nl.
describe_location(armory) :-
    write('You are in the police station armory. Most weapons are gone, but a few remain.'), nl,
    write('This could significantly improve your chances of survival.'), nl.
describe_location(_) :-
    write('You are in an unknown location.'), nl.

% List objects at current location
list_objects_at(Location) :-
    findall(Object, (at(Object, Location), Object \= player, \+ alive(Object)), Objects),
    (Objects = [] ->
        write('There are no objects here.'), nl
    ;
        write('You see the following objects:'), nl,
        forall(member(Object, Objects), format('- ~w~n', [Object]))
    ).

% List people at current location
list_people_at(Location) :-
    findall(Person, (at(Person, Location), Person \= player, alive(Person)), People),
    (People = [] ->
        write('There are no other people here.'), nl
    ;
        write('You see the following people:'), nl,
        forall(member(Person, People), format('- ~w~n', [Person]))
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
    findall(Zombie-Health, (zombie_at(Zombie, Location), health(Zombie, Health)), Zombies),
    (Zombies = [] ->
        true
    ;
        write('⚠️ DANGER: Zombies present!'), nl,
        forall(member(Zombie-Health, Zombies),
               format('- A ~w with health ~w is approaching you!~n', [Zombie, Health])),
        nl
    ).

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
examine_object(radio) :-
    write('A small emergency radio. It occasionally picks up transmissions.'), nl,
    write('You might hear useful information if you keep it with you.'), nl.
examine_object(ammo_box) :-
    write('A box of ammunition. It contains 20 rounds for a pistol.'), nl.
examine_object(first_aid_kit) :-
    write('A medical first aid kit. It can heal injuries and improve health.'), nl.
examine_object(lockpick) :-
    write('A professional lockpick set. Useful for opening locked doors without keys.'), nl,
    write('Toni would be much better at using this than you are.'), nl.
examine_object(shotgun) :-
    write('A pump-action shotgun. Extremely effective against zombies, but loud.'), nl,
    write('Using this will alert more zombies to your location, but it\'s powerful.'), nl.
examine_object(escape_vehicle_keys) :-
    write('Keys to what appears to be a military vehicle. These might be your ticket out of the city.'), nl.
examine_object(_) :-
    write('Nothing special about this object.'), nl.

% Take an object
take(Object) :-
    at(player, Location),
    at(Object, Location),
    retract(at(Object, Location)),
    assertz(holding(Object)),
    format('You took the ~w.~n', [Object]),
    check_special_item(Object),
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

% Special item effects
check_special_item(ammo_box) :-
    ammo(CurrentAmmo),
    NewAmmo is CurrentAmmo + 20,
    retract(ammo(CurrentAmmo)),
    assertz(ammo(NewAmmo)),
    write('You found 20 bullets! Added to your inventory.'), nl,
    !.
check_special_item(_).

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
        health(player, CurrentHealth),
        NewHealth is min(100, CurrentHealth + 5),
        retract(health(player, CurrentHealth)),
        assertz(health(player, NewHealth)),
        write('You drink some water. It was refreshing.'), nl,
        format('Water remaining: ~w%~n', [NewWater]),
        format('Health: ~w%~n', [NewHealth])
    ;
        write('The water bottle is empty.'), nl
    ).
use_object(canned_food) :-
    food(CurrentFood),
    NewFood is CurrentFood + 30,
    retract(food(CurrentFood)),
    assertz(food(NewFood)),
    health(player, CurrentHealth),
    NewHealth is min(100, CurrentHealth + 10),
    retract(health(player, CurrentHealth)),
    assertz(health(player, NewHealth)),
    write('You eat some canned food. You feel more energetic.'), nl,
    format('Food energy: ~w~n', [NewFood]),
    format('Health: ~w%~n', [NewHealth]).
use_object(radio) :-
    write('You turn on the radio and listen carefully.'), nl,
    random(1, 4, Message),
    radio_message(Message).
use_object(first_aid_kit) :-
    health(player, CurrentHealth),
    (CurrentHealth < 100 ->
        NewHealth is min(100, CurrentHealth + 30),
        retract(health(player, CurrentHealth)),
        assertz(health(player, NewHealth)),
        write('You used the first aid kit to treat your injuries.'), nl,
        format('Health increased to: ~w%~n', [NewHealth])
    ;
        write('You\'re already at full health. Saving the kit for later.'), nl
    ).
use_object(lockpick) :-
    at(player, Location),
    (Location = police_station ->
        write('You attempt to pick the lock to the armory...'), nl,
        (companion(toni_montana) ->
            write('Toni steps in: "Let me handle this, I\'m good with locks."'), nl,
            write('With impressive skill, Toni picks the lock in seconds.'), nl,
            retract(connected(police_station, armory, locked_door)),
            assertz(connected(police_station, armory, door)),
            write('The armory door is now unlocked!')
        ;
            random(1, 10, Success),
            (Success > 7 ->
                write('After several minutes of struggling, you manage to pick the lock!'), nl,
                retract(connected(police_station, armory, locked_door)),
                assertz(connected(police_station, armory, door)),
                write('The armory door is now unlocked!')
            ;
                write('You struggle with the lock but can\'t get it open.'), nl,
                write('Maybe someone with more experience could help?')
            )
        )
    ;
        write('There\'s nothing here that needs lockpicking.')
    ).
use_object(_) :-
    write('You can\'t use that item right now.'), nl.

% Radio messages
radio_message(1) :-
    write('*static* "...evacuation at military checkpoint... east side of the city... hurry..." *static*'), nl.
radio_message(2) :-
    write('*static* "...zombies more active after dark... stay indoors... *static*'), nl.
radio_message(3) :-
    write('*static* "...survivors at the old police station... armed and dangerous..." *static*'), nl.

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
    check_zombie_death(Zombie, NewHealth),
    handle_combat_consequences.
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
        check_zombie_death(Zombie, NewHealth),
        handle_combat_consequences
    ;
        write('Your pistol is out of ammo!'), nl
    ).
attack_with_weapon(Zombie, shotgun) :-
    ammo(Ammo),
    (Ammo >= 2 ->
        NewAmmo is max(0, Ammo - 2),
        retract(ammo(Ammo)),
        assertz(ammo(NewAmmo)),
        health(Zombie, Health),
        Damage is 100,
        NewHealth is max(0, Health - Damage),
        retract(health(Zombie, Health)),
        assertz(health(Zombie, NewHealth)),
        format('You blasted ~w with the shotgun for ~w damage! Ammo remaining: ~w~n', [Zombie, Damage, NewAmmo]),
        check_zombie_death(Zombie, NewHealth),
        write('The loud noise might attract more zombies!'), nl,
        maybe_spawn_zombie
    ;
        write('You need at least 2 ammo to use the shotgun!'), nl
    ).
attack_with_weapon(_, _) :-
    write('You can\'t attack with that.'), nl.

% Handle combat consequences
handle_combat_consequences :-
    at(player, Location),
    time(Time),
    (Time = night ->
        random(1, 10, ZombieChance),
        (ZombieChance > 6 ->
            maybe_spawn_zombie
        ; true)
    ; true).

% Maybe spawn a new zombie
maybe_spawn_zombie :-
    at(player, Location),
    format('More zombies heard the commotion and are approaching ~w!~n', [Location]),
    assertz(zombie_at(new_zombie, Location)),
    assertz(health(new_zombie, 40)).

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
    connected(Location, Destination, Method),
    (Method = locked_door ->
        write('That door is locked. You need to find a way to unlock it.'), nl
    ;
        move_player(Location, Destination)
    ),
    !.
go(_) :-
    write('You can\'t go there from here.'), nl.

% Move player and companions
move_player(OldLocation, NewLocation) :-
    % Move player
    retract(at(player, OldLocation)),
    assertz(at(player, NewLocation)),
    format('You went to ~w.~n', [NewLocation]),
    
    % Move companions
    forall(companion(Companion), (
        retract(at(Companion, OldLocation)),
        assertz(at(Companion, NewLocation))
    )),
    
    look.

% Talk to a person
talk(Person) :-
    at(player, Location),
    at(Person, Location),
    talk_to(Person),
    !.
talk(_) :-
    write('That person isn\'t here.'), nl.

% Dialogue for characters
talk_to(toni_montana) :-
    (companion(toni_montana) ->
        write('Toni looks at you with tired eyes: "What\'s up? We should keep moving if we want to make it to the evacuation point."'), nl,
        trust(toni_montana, Trust),
        (Trust >= 70 ->
            write('Toni adds: "By the way, I\'ve been thinking about our escape plan. I might know a shortcut through the sewers."'), nl,
            write('Toni marks a potential route on your map.')
        ; true)
    ;
        write('Toni eyes you suspiciously: "Who are you? What do you want?"'), nl,
        write('After explaining your situation, Toni seems less tense.'), nl,
        write('"I\'m a good scavenger. I know how to find supplies and pick locks. Maybe we can help each other survive."'), nl,
        write('"If you\'re interested in teaming up, just let me know. Safety in numbers, right?"')
    ).
talk_to(vin_disl) :-
    (companion(vin_disl) ->
        write('Vin nods at you: "We need to stay focused. My military training tells me that nightfall will bring more danger."'), nl,
        trust(vin_disl, Trust),
        (Trust >= 70 ->
            write('Vin shares: "When I was in the special forces, we dealt with situations like this. Always have an exit strategy."'), nl,
            write('Vin shows you some tactical movements that might help you avoid zombies.')
        ; true)
    ;
        write('Vin stands at attention when you approach: "State your business."'), nl,
        write('After explaining your situation, Vin relaxes slightly.'), nl,
        write('"Former military. Special forces. I know how to handle weapons and tactics."'), nl,
        write('"If you\'re looking for someone who can watch your back, I\'m your man. What do you say?"')
    ).

% Recruit a companion
recruit(Person) :-
    at(player, Location),
    at(Person, Location),
    \+ companion(Person),
    recruit_companion(Person),
    !.
recruit(Person) :-
    companion(Person),
    format('~w is already your companion.~n', [Person]),
    !.
recruit(_) :-
    write('That person isn\'t here or can\'t be recruited.'), nl.

% Recruitment logic
recruit_companion(toni_montana) :-
    write('You ask Toni to join you on your escape mission.'), nl,
    (holding(canned_food) ->
        write('You offer some food as a gesture of goodwill.'), nl,
        write('Toni smiles: "Smart move. Alright, I\'m in. My lockpicking skills might come in handy."'), nl,
        assertz(companion(toni_montana)),
        increase_trust(toni_montana, 10),
        write('Toni Montana has joined your group!')
    ;
        write('Toni looks hesitant: "What\'s in it for me? Do you have any food or supplies to share?"'), nl,
        write('Perhaps you should find something to offer before asking again.')
    ).
recruit_companion(vin_disl) :-
    write('You ask Vin to join your escape mission.'), nl,
    (holding(pistol), ammo(Ammo), Ammo > 0 ->
        write('You mention you have a weapon and ammo to share.'), nl,
        write('Vin nods approvingly: "Good to meet someone prepared. I\'ll join you. Two guns are better than one."'), nl,
        assertz(companion(vin_disl)),
        increase_trust(vin_disl, 10),
        write('Vin Disl has joined your group!')
    ;
        write('Vin frowns: "You don\'t look equipped for survival. Get yourself armed, then we\'ll talk."'), nl,
        write('Perhaps you should find a weapon and ammo before asking again.')
    ).

% Increase trust with companion
increase_trust(Companion, Amount) :-
    trust(Companion, CurrentTrust),
    NewTrust is min(100, CurrentTrust + Amount),
    retract(trust(Companion, CurrentTrust)),
    assertz(trust(Companion, NewTrust)).

% Decrease trust with companion
decrease_trust(Companion, Amount) :-
    trust(Companion, CurrentTrust),
    NewTrust is max(0, CurrentTrust - Amount),
    retract(trust(Companion, CurrentTrust)),
    assertz(trust(Companion, NewTrust)).

% Check companions status
check_companions :-
    write('Your companions:'), nl,
    check_companion_status.

% List companion status
check_companion_status :-
    companion(Companion),
    health(Companion, Health),
    trust(Companion, Trust),
    format('~w - Health: ~w%, Trust: ~w%~n', [Companion, Health, Trust]),
    write('Skills: '),
    findall(Skill, skill(Companion, Skill), Skills),
    print_skills(Skills),
    nl,
    fail.
check_companion_status :-
    \+ companion(_),
    write('You have no companions.'), nl.
check_companion_status.

% Print skills
print_skills([]).
print_skills([Skill]) :-
    format('~w', [Skill]).
print_skills([Skill|Rest]) :-
    format('~w, ', [Skill]),
    print_skills(Rest).

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
    (time(Time) -> true ; Time = day),
    nl,
    write('===== Your Status ====='), nl,
    format('Health: ~w%~n', [Health]),
    format('Water: ~w%~n', [Water]),
    format('Food: ~w~n', [Food]),
    format('Ammo: ~w~n', [Ammo]),
    format('Time: ~w~n', [Time]),
    write('======================'), nl, nl.

% Wait and see what happens
wait :-
    at(player, Location),
    write('You wait for a while...'), nl,
    random(1, 10, Event),
    (Event > 8 ->
        random_event(Location)
    ;
        write('Nothing significant happens.')
    ),
    nl.

% Random events that might occur when waiting
random_event(Location) :-
    random(1, 6, EventType),
    random_event_type(EventType, Location).

random_event_type(1, Location) :-
    write('You hear a distant scream, then silence.'), nl.
random_event_type(2, Location) :-
    write('The wind shifts, bringing the putrid smell of zombies closer.'), nl.
random_event_type(3, Location) :-
    write('You find a small cache of supplies someone left behind!'), nl,
    random(1, 4, FindType),
    random_find(FindType).
random_event_type(4, Location) :-
    time(day),
    write('The sun is getting lower in the sky. Night will come soon.'), nl.
random_event_type(4, Location) :-
    time(night),
    write('You notice the faint glow of dawn on the horizon. Day is approaching.'), nl.
random_event_type(5, Location) :-
    random(1, 3, ZombieAppears),
    (ZombieAppears = 1 ->
        format('A zombie wanders into ~w!~n', [Location]),
        assertz(zombie_at(random_zombie, Location)),
        assertz(health(random_zombie, 30))
    ;
        write('You thought you heard something, but it was just the wind.')
    ).

% Random finds from searching
random_find(1) :-
    water(CurrentWater),
    Added is 10,
    NewWater is min(100, CurrentWater + Added),
    retract(water(CurrentWater)),
    assertz(water(NewWater)),
    format('You found some water! Water increased by ~w to ~w%~n', [Added, NewWater]).
random_find(2) :-
    food(CurrentFood),
    Added is 20,
    NewFood is CurrentFood + Added,
    retract(food(CurrentFood)),
    assertz(food(NewFood)),
    format('You found some food! Food increased by ~w to ~w~n', [Added, NewFood]).
random_find(3) :-
    ammo(CurrentAmmo),
    Added is 5,
    NewAmmo is CurrentAmmo + Added,
    retract(ammo(CurrentAmmo)),
    assertz(ammo(NewAmmo)),
    format('You found some ammunition! Ammo increased by ~w to ~w~n', [Added, NewAmmo]).

% Day/Night cycle
advance_time :-
    time(CurrentTime),
    (CurrentTime = day ->
        retract(time(day)),
        assertz(time(night)),
        handle_nightfall
    ;
        retract(time(night)),
        assertz(time(day)),
        handle_daybreak
    ).

% Handle changes when night falls
handle_nightfall :-
    write('The sun has set. Darkness envelops the city.'), nl,
    write('Zombies become more active and dangerous at night.'), nl,
    increase_zombie_strength,
    maybe_spawn_night_zombie.

% Handle changes when day breaks
handle_daybreak :-
    write('The sun rises, bringing light to the devastated city.'), nl,
    write('Zombies are less active during the day, but still dangerous.'), nl,
    decrease_zombie_strength.

% Increase zombie strength at night
increase_zombie_strength :-
    forall(health(Zombie, Health), (
        (zombie_at(Zombie, _) ->
            NewHealth is Health * 1.5,
            retract(health(Zombie, Health)),
            assertz(health(Zombie, NewHealth))
        ; true)
    )).

% Decrease zombie strength during day
decrease_zombie_strength :-
    forall(health(Zombie, Health), (
        (zombie_at(Zombie, _) ->
            NewHealth is Health / 1.5,
            retract(health(Zombie, Health)),
            assertz(health(Zombie, NewHealth))
        ; true)
    )).

% Maybe spawn additional zombie at night
maybe_spawn_night_zombie :-
    at(player, Location),
    random(1, 10, ZombieChance),
    (ZombieChance > 6 ->
        format('The darkness brings more zombies to ~w!~n', [Location]),
        assertz(zombie_at(night_zombie, Location)),
        assertz(health(night_zombie, 60))
    ; true).

% Search the current location
search :-
    at(player, Location),
    format('You search ~w carefully...~n', [Location]),
    (companion(toni_montana) ->
        write('Toni helps you search, increasing your chances of finding something.'), nl,
        random(1, 10, FindChance),
        (FindChance > 3 ->
            search_location(Location),
            increase_trust(toni_montana, 5)
        ;
            write('You don\'t find anything useful.')
        )
    ;
        random(1, 10, FindChance),
        (FindChance > 6 ->
            search_location(Location)
        ;
            write('You don\'t find anything useful.')
        )
    ).

% Location-specific search results
search_location(abandoned_house) :-
    random(1, 3, Find),
    (Find = 1 ->
        water(CurrentWater),
        NewWater is min(100, CurrentWater + 20),
        retract(water(CurrentWater)),
        assertz(water(NewWater)),
        write('You found a sealed bottle of water!'), nl,
        format('Water increased to ~w%~n', [NewWater])
    ;
        food(CurrentFood),
        NewFood is CurrentFood + 15,
        retract(food(CurrentFood)),
        assertz(food(NewFood)),
        write('You found some preserved food in a cabinet!'), nl,
        format('Food increased to ~w~n', [NewFood])
    ).
search_location(police_station) :-
    ammo(CurrentAmmo),
    Added is random(5, 15),
    NewAmmo is CurrentAmmo + Added,
    retract(ammo(CurrentAmmo)),
    assertz(ammo(NewAmmo)),
    write('You found some ammunition in a desk drawer!'), nl,
    format('Ammo increased by ~w to ~w~n', [Added, NewAmmo]).
search_location(pharmacy) :-
    health(player, CurrentHealth),
    NewHealth is min(100, CurrentHealth + 25),
    retract(health(player, CurrentHealth)),
    assertz(health(player, NewHealth)),
    write('You found some medical supplies and treated your wounds.'), nl,
    format('Health increased to ~w%~n', [NewHealth]).
search_location(car) :-
    (at(gas_can, _) ; holding(gas_can)) ->
        write('You\'ve already searched this car thoroughly.'), nl
    ;
        assertz(at(gas_can, car)),
        write('You found a gas can under the seat! This might be useful for the escape vehicle.'), nl.
search_location(_) :-
    random(1, 3, GenericFind),
    generic_find(GenericFind).

% Generic finds for non-specific locations
generic_find(1) :-
    water(CurrentWater),
    Added is random(5, 15),
    NewWater is min(100, CurrentWater + Added),
    retract(water(CurrentWater)),
    assertz(water(NewWater)),
    write('You found some water!'), nl,
    format('Water increased by ~w to ~w%~n', [Added, NewWater]).
generic_find(2) :-
    food(CurrentFood),
    Added is random(10, 25),
    NewFood is CurrentFood + Added,
    retract(food(CurrentFood)),
    assertz(food(NewFood)),
    write('You found some food!'), nl,
    format('Food increased to ~w~n', [NewFood]).

% Check if player can escape and win the game
check_escape :-
    at(player, evacuation_point),
    holding(escape_vehicle_keys),
    (holding(gas_can) ; at(gas_can, evacuation_point)),
    win_game.

% Win the game
win_game :-
    write('======================================'), nl,
    write('CONGRATULATIONS! YOU HAVE ESCAPED!'), nl,
    write('======================================'), nl,
    write('With the escape vehicle keys and gas, you start up the military transport vehicle.'), nl,
    write('The engine roars to life as you navigate through the zombie-infested streets.'), nl,
    nl,
    (companion(toni_montana), companion(vin_disl) ->
        write('Both Toni and Vin made it with you. Your teamwork paid off.'), nl,
        write('Vin navigates while Toni watches for threats. Together, you\'ve survived "Last Night".'), nl
    ; companion(toni_montana) ->
        write('Toni made it out with you. His scavenging skills were invaluable.'), nl,
        write('Together, you escaped the city just as dawn breaks.'), nl
    ; companion(vin_disl) ->
        write('Vin made it out with you. His military training helped you survive.'), nl,
        write('With his tactical knowledge, you navigate safely through danger zones.'), nl
    ;
        write('You made it out alone, relying only on your wits and determination.'), nl,
        write('As the sun rises, you leave the nightmare city behind.'), nl
    ),
    nl,
    write('THE END'), nl,
    nl,
    write('(Type restart. to play again)'), nl.

% Restart the game
restart :- start.

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
    time(Time),
    (Time = day ->
        write('There are a few zombies on the street outside.'), nl,
        write('They seem to be heading in your direction, but they are still far away.'), nl,
        write('You notice an abandoned car by the roadside that still looks in good condition.'), nl,
        write('Maybe it\'s your ticket to survival.')
    ;
        write('In the darkness, you can barely make out shadowy figures moving on the street.'), nl,
        write('There seem to be more zombies at night, and they move with more purpose.'), nl,
        write('It might be safer to wait until morning before venturing outside.')
    ),
    nl,
    !.
look_window :-
    write('There\'s no window here to look out of.'), nl.

% Listen for sounds
listen :-
    at(player, Location),
    write('You stand still and listen carefully...'), nl,
    time(Time),
    listen_at_location(Location, Time).

% Location and time specific listening results
listen_at_location(abandoned_house, day) :-
    write('You hear distant moaning and shuffling feet outside.'), nl,
    write('Occasionally there\'s a crash or bang from far away - perhaps other survivors?'), nl.
listen_at_location(abandoned_house, night) :-
    write('The zombie moans are louder at night, and seem to be coming from multiple directions.'), nl,
    write('You also hear occasional screams in the distance. The city is not safe after dark.'), nl.
listen_at_location(street, _) :-
    write('The sounds of zombies are clearer here - moaning, shuffling, and occasional growls.'), nl,
    write('You can pinpoint several groups of them within a few blocks of your position.'), nl.
listen_at_location(police_station, _) :-
    write('The police station is eerily quiet, but you hear faint scratching from somewhere inside.'), nl,
    write('It might be rats... or something worse.'), nl.
listen_at_location(high_point, _) :-
    write('From this vantage point, the sounds of the city carry clearly.'), nl,
    write('You hear zombie hordes moving through the streets below, and what might be gunfire far to the east.'), nl,
    write('That could be the military evacuation point mentioned in the radio transmission.'), nl.
listen_at_location(_, _) :-
    write('You hear the typical sounds of a dead city - wind, creaking buildings, and distant zombies.'), nl.

% Add new locations
add_new_locations :-
    assertz(connected(next_street, hospital, door)),
    assertz(connected(hospital, next_street, door)),
    assertz(connected(high_point, helipad, walk)),
    assertz(connected(helipad, high_point, walk)),
    assertz(connected(street, sewer_entrance, manhole)),
    assertz(connected(sewer_entrance, street, ladder)),
    assertz(connected(sewer_entrance, sewer_tunnel, tunnel)),
    assertz(connected(sewer_tunnel, sewer_entrance, tunnel)),
    assertz(connected(sewer_tunnel, evacuation_point, ladder)),
    assertz(connected(evacuation_point, sewer_tunnel, ladder)),
    assertz(connected(high_point, evacuation_point, climb_down)).

% Description for new locations
describe_location(hospital) :-
    write('You are in an abandoned hospital. Medical equipment is scattered everywhere.'), nl,
    write('This place might have medical supplies, but it also seems particularly dangerous.'), nl,
    write('The floors are stained with dried blood and the emergency generators are off.'), nl.
describe_location(helipad) :-
    write('You\'re at a helipad on a building rooftop. There\'s no helicopter here now.'), nl,
    write('From radio transmissions, you know that helicopters occasionally make rescue attempts.'), nl,
    write('But timing and signaling would be critical to catch their attention.'), nl.
describe_location(sewer_entrance) :-
    write('You\'re at a sewer entrance. The air is damp and smells terrible.'), nl,
    write('This could provide a safer path through the city, away from zombies above ground.'), nl,
    write('However, who knows what might be lurking in the darkness below?'), nl.
describe_location(sewer_tunnel) :-
    write('You\'re in a dark, wet sewer tunnel. Your footsteps echo against the concrete walls.'), nl,
    write('The water level is low, making movement possible, but it\'s hard to see far ahead.'), nl,
    write('You notice some markings on the wall that might be from other survivors.'), nl.
describe_location(evacuation_point) :-
    write('You\'ve reached what appears to be the military evacuation point!'), nl,
    write('There are signs of recent activity - tire tracks, boot prints, and empty supply crates.'), nl,
    write('A large transport vehicle is parked nearby, but you\'ll need keys and fuel to use it.'), nl,
    write('This could be your way out of the city if you can get it running.'), nl.

% Helper predicates
help :- instructions.

% End the game
quit :-
    write('Thanks for playing "Last Night"!'), nl,
    write('You survived for a time in the zombie apocalypse.'), nl,
    write('(Type restart. to play again)'), nl.

% Aliases for common commands
n :- go(north).
s :- go(south).
e :- go(east).
w :- go(west).
i :- inventory.
l :- look.
x(Object) :- examine(Object).

% Special items for the expanded game
examine_object(gas_can) :-
    write('A red metal gas can with some fuel still inside. Perfect for the escape vehicle.'), nl.
examine_object(flare_gun) :-
    write('A bright orange flare gun with a single flare. Could be used to signal for help.'), nl.
examine_object(walkie_talkie) :-
    write('A military-grade walkie-talkie. It occasionally picks up transmissions from other survivors.'), nl.
examine_object(escape_vehicle_keys) :-
    write('Keys to the military transport vehicle at the evacuation point. Your ticket out of the city.'), nl.

% Add use cases for new items
use_object(gas_can) :-
    at(player, evacuation_point),
    write('You pour the gas into the military transport vehicle\'s tank.'), nl,
    write('The fuel gauge rises - it should be enough to get you out of the city.'), nl,
    retract(holding(gas_can)),
    assertz(at(gas_can, evacuation_point)),
    check_escape.
use_object(gas_can) :-
    write('You need to be at the evacuation point to use this.'), nl.
use_object(flare_gun) :-
    at(player, helipad),
    time(day),
    write('You fire the flare gun into the sky. The bright red flare arcs high above the city.'), nl,
    write('After a few minutes, you hear the distant sound of helicopter rotors!'), nl,
    write('A military helicopter approaches and lands on the helipad.'), nl,
    write('You\'ve been rescued! You\'ve survived "Last Night"!'), nl,
    write('CONGRATULATIONS! YOU WIN!'), nl,
    write('(Type restart. to play again)'), nl.
use_object(flare_gun) :-
    at(player, helipad),
    time(night),
    write('You fire the flare gun into the night sky. The red flare briefly illuminates the darkness.'), nl,
    write('The bright light attracts nearby zombies! You hear them converging on your position.'), nl,
    assertz(zombie_at(zombie_horde2, helipad)),
    assertz(health(zombie_horde2, 120)),
    write('No helicopter responds to your signal.'), nl.
use_object(flare_gun) :-
    write('You need to be at the helipad to use this effectively.'), nl.
use_object(walkie_talkie) :-
    write('You turn on the walkie-talkie and listen for transmissions.'), nl,
    random(1, 4, Transmission),
    walkie_talkie_message(Transmission).

% Walkie-talkie messages
walkie_talkie_message(1) :-
    write('*static* "...military evacuation continuing for next 12 hours only... coordinates are..." *static*'), nl.
walkie_talkie_message(2) :-
    write('*static* "...helipad extractions suspended due to weather... ground transports only..." *static*'), nl.
walkie_talkie_message(3) :-
    write('*static* "...sewers provide safe passage... marking tunnels with blue paint... follow to extraction..." *static*'), nl.