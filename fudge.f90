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
program fudge
  use global
  use levelmod
  use ncurses
  use iso_c_binding
  implicit none

  type(c_ptr) :: screen
  integer :: istat, ilvl, iexit, i
  type(level) :: lvl
  integer, parameter :: arglen = 1024
  integer :: argc, iarg
  character(len=arglen) :: argv
  character(len=:), allocatable :: file, passinp
  character(len=40) :: title

  file = ""
  passinp = ""
  argc = command_argument_count()
  if (argc > 0) then
     iarg = 0
     do while (iarg < argc)
        iarg = iarg + 1
        call getarg(iarg,argv)
        if (argv(1:2) == "-h") then
           write (*,*)
           write (*,'("Fudge: a text-based puzzle game about a radioactive dog seeking vengeance.")')
           write (*,*)
           write (*,'("Usage: fudge [options]")')
           write (*,*)
           write (*,'("Options:")')
           write (*,*)
           write (*,'(" -h        This help message")')
           write (*,'(" -f FILE   Load the map in file")')
           write (*,'(" -p PASSWD Skip to a level using a password")')
           write (*,*)
           return
        elseif (argv(1:2) == "-f") then
           iarg = iarg + 1
           call getarg(iarg,argv)
           file = trim(argv)
        elseif (argv(1:2) == "-p") then
           iarg = iarg + 1
           call getarg(iarg,argv)
           passinp = trim(argv)
           call upper(passinp)
        end if
     end do
  end if

  screen = initscr()
  if (.not.c_associated(screen)) then
     write (*,*) "could not initialize screen"
     stop 1
  end if

  istat = start_color()
  istat = cbreak()
  istat = keypad(screen,true)
  istat = nodelay(screen,true)
  istat = noecho()
  istat = curs_set(0)
  istat = init_pair(col_white_black,COLOR_WHITE,COLOR_BLACK);
  istat = init_pair(col_yellow_black,COLOR_YELLOW,COLOR_BLACK);
  istat = init_pair(col_green_black,COLOR_GREEN,COLOR_BLACK);
  istat = init_pair(col_cyan_black,COLOR_CYAN,COLOR_BLACK);
  istat = init_pair(col_red_black,COLOR_RED,COLOR_BLACK);
  istat = init_pair(col_magenta_black,COLOR_MAGENTA,COLOR_BLACK);
  istat = init_pair(col_blue_black,COLOR_BLUE,COLOR_BLACK);
  istat = init_pair(col_white_green,COLOR_WHITE,COLOR_GREEN);
  istat = init_pair(col_blue_blue,COLOR_BLUE,COLOR_BLUE);
  istat = init_pair(col_red_red,COLOR_RED,COLOR_RED);

  iexit = 0
  if (len_trim(file) > 0) then
     do while(.true.)
        call lvl%end()
        call lvl%read(file,"",file=file)
        if (.not.lvl%isinit) then
           istat = endwin()
           write (*,*) "error: ", lvl%errmsg
           return
        end if
        call lvl%run(iexit)
        if (iexit /= iexit_redo) exit
     end do
  else
     ilvl = 0
     if (len_trim(passinp) > 0) then
        do i = 1, ndeflvl
           if (passinp == trim(passwd(i))) then
              ilvl = i-1
              exit
           end if
        end do
     end if
     do while(.true.)
        if (.not. iexit == iexit_redo)&
           ilvl = ilvl + 1
        call lvl%end()
        write (title,'("Level ",I2.2)') ilvl
        call lvl%read(title,passwd(ilvl),idef=ilvl)
        if (.not.lvl%isinit) then
           istat = endwin()
           write (*,*) "error: ", lvl%errmsg
           return
        end if
        call lvl%run(iexit)
        if (iexit == iexit_failed .or. iexit == iexit_userexit) exit
        if (ilvl == ndeflvl .and. iexit == iexit_beaten) exit
     end do
  end if
  istat = endwin()

  if (iexit == iexit_beaten) then
     write (*,'("Well done!")')
  elseif (iexit == iexit_failed) then
     write (*,'("You died!")')
  elseif (iexit == iexit_userexit) then
     write (*,'("Good-bye!")')
  end if

end program fudge
