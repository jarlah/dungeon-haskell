# dungeon-haskell

A terminal-based roguelike dungeon crawler written in Haskell.

Procedurally generated rooms, fog-of-war line-of-sight, turn-based
combat, inventory, NPC quests, a 10-floor descent capped by a dragon
boss fight, persistent saves, and background music. The game logic
is a pure Haskell library with a thin [Brick](https://hackage.haskell.org/package/brick)
rendering shell on top, and is covered by 140+ HSpec / QuickCheck
tests.

```
####################
#........#.........#
#..@..r..#....g....#
#........+.........#
#####+####.........#
    #..............#
    #.....>........#
    ####............
       #############
```

## Features

- **Procedural dungeons.** BSP room generation, doors that can be
  open or closed, and stairs between floors. Each descent rolls a
  fresh floor; ascending returns you to exactly the layout and
  monster state you left.
- **Field of view.** Symmetric shadowcasting FOV with persistent
  exploration — tiles you've seen stay rendered in fog color after
  they fall out of sight.
- **Turn-based combat.** Hit / miss / crit / kill rolls with per-
  monster stats, damage, and XP. Level up heals you and bumps your
  stat line.
- **Inventory.** Potions, weapons, armor, and loot drops; equip,
  unequip, use, and drop via an in-game modal.
- **NPCs and quests.** Friendly NPCs hand out kill, depth, and
  boss-slaying quests. A quest log modal tracks progress; quests
  can be turned in for XP or abandoned.
- **Boss fight.** The tenth floor holds a 2×2 dragon in its own
  room, with its own music cue that crossfades in the moment you
  step into line of sight.
- **Audio.** SDL-backed SFX for hit / miss / crit / kill / hurt /
  level up / death, plus a dungeon music loop that swaps to a boss
  track on the boss floor. All audio is CC0.
- **Persistent saves.** Quicksave / quickload (F5 / F9), a
  six-slot save picker, and a launch menu with *Continue* (loads
  the most recent save). Save files are compact binary with a
  magic+version header so casual editing is deliberately hard.
- **Wizard console.** A slash-command prompt (`/`) for debugging:
  reveal the map, heal, teleport, spawn monsters, jump floors,
  grant XP. Nice for iterating on balance without replaying from
  scratch.

## Building

The project uses [Stack](https://docs.haskellstack.org/) with a
pinned LTS and (on NixOS) a Nix integration that pulls in SDL2 and
pkg-config so `proteaaudio-sdl` can link against `libSDL2`.

```sh
stack build
```

First build compiles the full dependency closure (Brick, vty,
proteaaudio-sdl, …), so expect a wait. Subsequent builds are
incremental.

### Dependencies

- GHC 9.10.3 (pinned via `stack.yaml`)
- `libSDL2` for audio playback (on NixOS, handled automatically by
  the Nix integration; on other Linux distros install
  `libsdl2-dev`; on macOS `brew install sdl2`)
- A terminal that supports Unicode box-drawing and 256 colors

## Running

```sh
stack run
```

You'll land on the title screen. Pick *New Game* to start a fresh
run, or *Continue* to load the most recent save.

## Testing

```sh
stack test
```

140+ examples covering movement, FOV, combat rolls, progression,
command parsing, inventory, loot, quest advancement, save/load
(with a temp-directory XDG override so tests can't touch real
saves), and GameState event emission.

## Controls

### Movement

Vi keys for eight-way movement:

```
 y  k  u
  \ | /
 h -@- l
  / | \
 b  j  n
```

| key | action                         |
|-----|--------------------------------|
| `.` | wait a turn                    |
| `g` | pick up the item under you     |
| `>` | descend stairs                 |
| `<` | ascend stairs                  |

### Modals

| key   | action                                          |
|-------|-------------------------------------------------|
| `i`   | open inventory                                  |
| `Q`   | open quest log                                  |
| `?`   | open help / key reference                       |
| `/`   | open the wizard command prompt                  |
| `q`   | quit (asks to confirm)                          |
| `Esc` | close the current modal                         |

### Save / load

| key  | action                                          |
|------|-------------------------------------------------|
| `F5` | quicksave                                       |
| `F9` | quickload                                       |
| `F2` | open the save picker (pick a slot, overwrite)   |
| `F3` | open the load picker                            |

Save files live under the XDG data directory
(`$XDG_DATA_HOME/dungeon-haskell/saves` on Linux,
`~/Library/Application Support/dungeon-haskell/saves` on macOS,
`%APPDATA%\dungeon-haskell\saves` on Windows). Each save is a
compact binary blob prefixed with the magic header `DHSAVE01` so
old or corrupted files are rejected cleanly instead of silently
decoding into garbage.

### Wizard commands

Open the prompt with `/` and type one of:

| command                | effect                                   |
|------------------------|------------------------------------------|
| `/reveal`              | reveal the whole map                     |
| `/heal`                | refill HP to max                         |
| `/kill-all`            | kill every monster on the floor          |
| `/teleport X Y`        | jump to a tile                           |
| `/spawn rat\|goblin\|orc` | spawn a monster next to you              |
| `/xp N`                | grant N experience points                |
| `/descend`             | jump to the next floor                   |
| `/ascend`              | jump to the previous floor               |

## Project layout

```
app/Main.hs             Brick event loop, modal dispatch, audio + save wiring
src/Game/Types.hs       core ADTs (Tile, Monster, Item, Stats, ...)
src/Game/GameState.hs   the whole-world snapshot + applyAction / applyCommand
src/Game/Logic/         pure game logic (Movement, Combat, FOV, Dungeon,
                        Inventory, Loot, Quest, Progression, Command, MonsterAI)
src/Game/Render.hs      Brick widgets for the map, HUD, and every modal
src/Game/Input.hs       key → GameAction mapping
src/Game/Audio.hs       proteaaudio-sdl shell (best-effort, degrades to silent)
src/Game/Save.hs        binary save/load with a magic+version header
src/Game/Save/Types.hs  shared save types (factored out to break a cycle)
test/                   HSpec + QuickCheck suite, auto-discovered
assets/music            CC0 dungeon + boss music loops
assets/sfx              CC0 combat and UI sound effects
docs/plan.md            milestone-by-milestone planning doc
```

The library is deliberately factored so every game rule is testable
in isolation: `applyAction` is a pure `GameState -> GameState`
function, Brick and the audio shell are the only places that touch
`IO`, and the save layer is the single module that knows about
`Data.Binary`.

## Credits

- Audio: all tracks and effects under [assets/](assets/) are CC0
  (public domain). Full sourcing in [`assets/CREDITS.md`](assets/CREDITS.md).
- Code: see [`LICENSE`](LICENSE).
