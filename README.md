## Combat & Research simulator for Warzone 2100

#### How to build

TODO opam/dune instructions
```
dune build
```

Usage:
```
>> ./run.sh research find "assault gun"
(Assault Gun Hardpoint) R-Defense-Wall-RotMg enables: [Wall-RotMg]
(Twin Assault Gun Hardpoint) R-Defense-WallTower-TwinAGun enables: [WallTower-TwinAssaultGun]
(Assault Gun) R-Wpn-MG4 enables: [MG4ROTARYMk1 MG4ROTARY-VTOL CyborgRotMG]
(Twin Assault Gun) R-Wpn-MG5 enables: [MG5TWINROTARY]

>> ./run.sh research tree R-Wpn-MG4
 R-Wpn-MG1Mk1 (Machinegun: 5; 5; start @ 0)
 R-Sys-Engineering01 (Engineering: 86; 86; start @ 0)
 R-Wpn-MG-Damage01 (Hardened MG Bullets: 43; 48; start @ 5)
... 
total_time;used_labs;total_cost 32min55sec;3;1121

>> ./run.sh template info "heavy cannon python tracks"
armour_heat: 9 -> 41
armour_kinetic: 38 -> 74
hitpoints: 2470 -> 4810
damage: 270 -> 390
...


>> /run.sh fight 1vs1 "heavy cannon" "lancer mantis half-tracks"
weapon;template;long_min_upgrades_salvo;short_min_upgrades_salvo;long_min_upgrades_sec;short_min_upgrades_sec
Heavy Cannon;Half-tracks Mantis Lancer;6;5;35;29

>> cat tk-mc.ini 
[aggressor_weapon]
Tank Killer
Heavy Cannon

[victim_weapon]
Tank Killer
Heavy Cannon

[victim_body]
Python

[victim_propulsion]
Tracks

>> ./run.sh fight weapon-vs-template $PWD/tk-mc.ini 
weapon;template;long_min_upgrades_salvo;short_min_upgrades_salvo;long_min_upgrades_sec;short_min_upgrades_sec
Heavy Cannon;Tracks Python Heavy Cannon;19;15;112;88
Heavy Cannon;Tracks Python Tank Killer;10;8;59;47
Tank Killer;Tracks Python Heavy Cannon;4;5;53;71
Tank Killer;Tracks Python Tank Killer;2;3;17;35
```