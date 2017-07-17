# Fudge

Fudge is a text-based puzzle game about a radioactive dog seeking
vengeance. It is a clone of the 1995 game
[Pudge](https://archive.org/details/Pudge_1020) for Windows 3.11,
created by Damon Chaplin based on a previous game designed by
A. J. Cook for the Atari ST. This clone is written in Fortran and its
purpose is to demnostrate how modern Fortran with its C
interoperability features can be used for things other than numerical
computation. Fudge is distributed under the GNU/GPL license, version 3
(see LICENSE).

Fudge has been developed for Linux. You will need a Fortran compiler
(gcc/gfortran works) and the ncurses library. On debian, you can get
both by doing:

  ```
  sudo apt-get install libncurses-dev gfortran
  ```

To compile, run:

  ```
  make clean
  make
  ```
### Backstory

(Shamelessly copied from the Pudge help page).

After many years of constant use as a nuclear dumping-ground, Fudge's
home planet, Stacia, is on the brink of disaster. So Fudge, being very
environmentally-aware, sets forth on a quest to cleanse the planet and
finally rid it of the perpetrators of this thoughtless deed, the
greedy and selfish Naggons. But what can one Fudge do against the evil
hordes? Fudge must journey through levels, making each one safe by:

1) Clearing up all the **radioactive earth** (**~**).

2) Collecting all the anti-rad **crystals** (**%**) (these also give
Fudge valuable extra time).

3) De-fusing the **bombs** (**B**) left by the Naggons, by surrounding
them with 4 **rocks** (**o**).

4) Killing any **naggons** (**N**) lurking about, by surrounding them
with radiation canisters (too good for 'em!).

5) Destroying the radiation **canisters** (**O**) by feeding them to
the deadly **chompers** (**X**). 

Each level has a password (in yellow, at the top of the screen) which
may be used in future games to skip the previous screens (**fudge -p
PASSWD**).

### Controls

* Move Fudge with the **arrow keys**.

* Restart a level with **r**.

* Quit the game with **q**.

* Help screen with **h**.

See the command-line options by doing **fudge -h**.

### Level design

You can design your own levels and then run them by doing **fudge -f
FILE**, where FILE is a text file containing the level
description. A level file uses the same symbols for the in-game
objects as the game itself (in fact, you can copy and paste the
terminal output). For instance, a simple level is:

  ```
  #####
  #@o #
  #oo #
  #o ~#
  #####
  ```

There are no limitations regarding the size of the level or the number
of objects in it.

### Sokoban mode

Fudge can double as a sokoban game. To beat a sokoban level, rocks
must be moved onto all **+** symbols in the map. A ***** character in
the map denotes a **+** with a rock on it. Try the first level
in classic sokoban by doing:

  ```
  fudge -f sokoban-1.lvl
  ```

More sokoban levels can be obtained from
[sokoban.org](http://www.sokoban.org) or from [Jonathan Schaeffer's
webpage at the the UofA](https://webdocs.cs.ualberta.ca/~games/Sokoban/). The
ASCII maps in the former can be converted to fudge format using:

  ```
  sed 's/[-_]/ /g;s/\$/o/g;s/\./+/g'
  ```

