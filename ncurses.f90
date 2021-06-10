! Copyright (c) 2017 Alberto Otero de la Roza <aoterodelaroza@gmail.com>.
!
! fudge is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or (at
! your option) any later version.
!
! fudge is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
module ncurses
  use iso_c_binding
  implicit none

  public

  logical(c_bool), parameter :: true  = .true.
  logical(c_bool), parameter :: false = .false.

  integer(c_int),parameter :: KEY_DOWN  = int(O'402',c_int)
  integer(c_int),parameter :: KEY_UP    = int(O'403',c_int)
  integer(c_int),parameter :: KEY_LEFT  = int(O'404',c_int)
  integer(c_int),parameter :: KEY_RIGHT = int(O'405',c_int)

  integer(c_short), parameter :: COLOR_BLACK   = 0
  integer(c_short), parameter :: COLOR_RED     = 1
  integer(c_short), parameter :: COLOR_GREEN   = 2
  integer(c_short), parameter :: COLOR_YELLOW  = 3
  integer(c_short), parameter :: COLOR_BLUE    = 4
  integer(c_short), parameter :: COLOR_MAGENTA = 5
  integer(c_short), parameter :: COLOR_CYAN    = 6
  integer(c_short), parameter :: COLOR_WHITE   = 7

  integer(c_long), parameter :: A_BLINK        =  524288_c_long
  integer(c_long), parameter :: A_BOLD         =  2097152_c_long
  integer(c_long), parameter :: A_INVIS        =  8388608_c_long
  integer(c_long), parameter :: A_NORMAL       =  0_c_long
  integer(c_long), parameter :: A_PROTECT      =  16777216_c_long
  integer(c_long), parameter :: A_REVERSE      =  262144_c_long
  integer(c_long), parameter :: A_UNDERLINE    =  131072_c_long
  integer(c_long), parameter :: A_DIM          =  1048576_c_long
  integer(c_long), parameter :: A_HORIZONTAL   =  33554432_c_long
  integer(c_long), parameter :: A_LEFT         =  67108864_c_long
  integer(c_long), parameter :: A_LOW          =  134217728_c_long
  integer(c_long), parameter :: A_RIGHT        =  268435456_c_long
  integer(c_long), parameter :: A_STANDOUT     =  65536_c_long
  integer(c_long), parameter :: A_TOP          =  536870912_c_long
  integer(c_long), parameter :: A_VERTICAL     =  1073741824_c_long

  interface
     ! window *initscr(void)
     function initscr() bind(c)
       import :: c_ptr
       type(c_ptr) :: initscr
     end function initscr
     ! int endwin(void);
     function endwin() bind(c)
       import :: c_int
       integer(c_int) :: endwin
     end function endwin
     ! int start_color(void);
     function start_color() bind(c)
       import :: c_int
       integer(c_int) :: start_color
     end function start_color
     ! int cbreak(void);
     function cbreak() bind(c)
       import :: c_int
       integer(c_int) :: cbreak
     end function cbreak
     ! int keypad(window *,bool)
     function keypad(win,bool) bind(c)
       import :: c_int, c_bool, c_ptr
       type(c_ptr), value :: win
       logical(c_bool), value :: bool
       integer(c_int) :: keypad
     end function keypad
     ! int nodelay(window *,bool)
     function nodelay(win,bool) bind(c)
       import :: c_int, c_bool, c_ptr
       type(c_ptr), value :: win
       logical(c_bool), value :: bool
       integer(c_int) :: nodelay
     end function nodelay
     ! int noecho()
     function noecho() bind(c)
       import :: c_int
       integer(c_int) :: noecho
     end function noecho
     ! int init_pair(short pair,short color,short color)
     function init_pair(pair,color1,color2) bind(c)
       import :: c_short, c_int
       integer(c_short), value :: pair, color1, color2
       integer(c_int) :: init_pair
     end function init_pair
     ! int getch()
     function getch() bind(c)
       import :: c_int
       integer(c_int) :: getch
     end function getch
     ! int clear()
     function clear() bind(c)
       import :: c_int
       integer(c_int) :: clear
     end function clear
     ! int move(y,x)
     function move(y,x) bind(c)
       import :: c_int
       integer(c_int), value :: y, x
       integer(c_int) :: move
     end function move
     ! int mvaddch(int y,int x,const chtype c)
     function mvaddch(y,x,c) bind(c)
       import :: c_int, c_char, c_ptr, c_long
       integer(c_int), value :: y, x
       integer(c_long), value :: c
       integer(c_int) :: mvaddch
     end function mvaddch
     ! int mvaddstr(int y,int x,const char *c)
     function mvaddstr(y,x,c) bind(c)
       import :: c_int, c_char, c_ptr
       integer(c_int), value :: y, x
       type(c_ptr), value :: c
       integer(c_int) :: mvaddstr
     end function mvaddstr
     ! int mvaddstr(int y,int x,const char *c)
     function mvinsstr(y,x,c) bind(c)
       import :: c_int, c_char, c_ptr
       integer(c_int), value :: y, x
       type(c_ptr), value :: c
       integer(c_int) :: mvinsstr
     end function mvinsstr
     ! int mvaddstr(int y,int x,const char *c)
     function mvinsnstr(y,x,c,l) bind(c)
       import :: c_int, c_char, c_ptr
       integer(c_int), value :: y, x
       type(c_ptr), value :: c
       integer(c_int), value :: l
       integer(c_int) :: mvaddstr
     end function mvinsnstr
     ! int refresh()
     function refresh() bind(c)
       import :: c_int
       integer(c_int) :: refresh
     end function refresh
     ! int color_set(int pair,null)
     function color_set(pair,void) bind(c)
       import :: c_short, c_ptr, c_int
       integer(c_short), value :: pair
       type(c_ptr), value :: void
       integer(c_int) :: color_set
     end function color_set
     ! int refresh()
     function attrset(lattr) bind(c)
       import :: c_long, c_int
       integer(c_long), value :: lattr
       integer(c_int) :: attrset
     end function attrset
     ! int curs_set(int)
     function curs_set(n) bind(c)
       import :: c_int
       integer(c_int), value :: n
       integer(c_int) :: curs_set
     end function curs_set
  end interface

end module ncurses
