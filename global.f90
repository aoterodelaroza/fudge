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

! The levels in this file come from the shareware version of Pudge, by
! Damon Chaplin (1995).
module global
  use iso_c_binding
  use ncurses
  implicit none

  public

  integer*8 :: nframe
  integer, parameter :: fps = 10
  integer, parameter :: fpidle = 2
  integer, parameter :: fpnaggon = 10
  integer, parameter :: maxtime = 240 ! in seconds
  integer, parameter :: maxtime_bomb = 240 ! in seconds
  integer, parameter :: addtime_crystal = 30 ! in seconds
  logical :: showhelp = .false.

  integer, parameter :: dxg = 2, dyg = 2 ! delta-x and delta-y
  integer, parameter :: borx = 1, bory = 1 ! border thickness

  integer, parameter :: c_empty = 1       ! empty space ( )
  integer, parameter :: c_wall = 2        ! wall (#)
  integer, parameter :: c_player = 3      ! player (@)
  integer, parameter :: c_chomper = 4     ! chomper (X)
  integer, parameter :: c_rock = 5        ! rock (*)
  integer, parameter :: c_arrowl = 6      ! arrow left (<)
  integer, parameter :: c_arrowr = 7      ! arrow right (>)
  integer, parameter :: c_arrowu = 8      ! arrow up (^)
  integer, parameter :: c_arrowd = 9      ! arrow down (v)
  integer, parameter :: c_teleport0 = 10  ! teleport (0)
  integer, parameter :: c_teleport1 = 11  ! teleport (1)
  integer, parameter :: c_teleport2 = 12  ! teleport (2)
  integer, parameter :: c_teleport3 = 13  ! teleport (3)
  integer, parameter :: c_teleport4 = 14  ! teleport (4)
  integer, parameter :: c_teleport5 = 15  ! teleport (5)
  integer, parameter :: c_teleport6 = 16  ! teleport (6)
  integer, parameter :: c_teleport7 = 17  ! teleport (7)
  integer, parameter :: c_teleport8 = 18  ! teleport (8)
  integer, parameter :: c_teleport9 = 19  ! teleport (9)
  integer, parameter :: c_cornerup = 20   ! corner up (/)
  integer, parameter :: c_cornerdn = 21   ! corner down (\)
  integer, parameter :: c_earth = 22      ! earth (.)
  integer, parameter :: c_crystal = 23    ! crystal (%)
  integer, parameter :: c_naggon = 24     ! naggon (N)
  integer, parameter :: c_canister = 25   ! canister (O)
  integer, parameter :: c_bomb = 26       ! bomb (B)

  integer(c_short), parameter :: col_white_black = 1
  integer(c_short), parameter :: col_yellow_black = 2
  integer(c_short), parameter :: col_green_black = 3
  integer(c_short), parameter :: col_cyan_black = 4
  integer(c_short), parameter :: col_red_black = 5
  integer(c_short), parameter :: col_magenta_black = 6
  integer(c_short), parameter :: col_blue_black = 7
  integer(c_short), parameter :: col_white_green = 8
  integer(c_short), parameter :: col_blue_blue = 9
  integer(c_short), parameter :: col_red_red = 10

  character(len=1), parameter :: symbol(26) = (/" ","#","@","X","o",&
     "<",">","^","v","0","1","2","3","4","5","6","7","8","9","/","\",& ! " 
     "~","%","N","O","B"/) 
  integer(kind=c_short), parameter :: symcol(26) = (/&
     col_white_black,col_white_black,col_magenta_black,col_red_black,&
     col_cyan_black,col_blue_black,col_blue_black,col_blue_black,col_blue_black,&
     col_white_green,col_white_green,col_white_green,col_white_green,&
     col_white_green,col_white_green,col_white_green,col_white_green,&
     col_white_green,col_white_green,col_white_black,col_white_black,&
     col_red_black,col_magenta_black,col_green_black,col_yellow_black,&
     col_red_black/)

  integer(kind=c_long), parameter :: symatt(26) = (/A_NORMAL,A_NORMAL,A_BOLD,&
     A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,&
     A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_NORMAL,A_NORMAL,A_BOLD,&
     A_BOLD,A_BOLD,A_BOLD,A_BOLD/)

  integer, parameter :: mov_up = KEY_UP
  integer, parameter :: mov_down = KEY_DOWN
  integer, parameter :: mov_right = KEY_RIGHT
  integer, parameter :: mov_left = KEY_LEFT

  integer, parameter :: spr_none = 0
  integer, parameter :: spr_explosion = 1

  integer, parameter :: spr_explosion_nstep = 7
  integer, parameter :: spr_explosion_nframe(7) = (/1,1,1,2,3,4,4/)
  character(len=1), parameter :: spr_explosion_char(7) = (/"O","X","X","+",";",";","."/)
  integer(c_short), parameter :: spr_explosion_color(7) = &
     (/col_red_red,col_red_red,col_red_black,col_red_black,col_red_black,&
     col_yellow_black,col_white_black/)
  integer(kind=c_long), parameter :: spr_explosion_attr(7) = &
     (/A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD,A_BOLD/)
  logical, parameter :: spr_explosion_repeat = .false.

  integer, parameter :: ndeflvl = 32
  character(len=209), parameter :: deflvl(ndeflvl) = (/&
"/ O O O X~ ~ ~~  %_        # ### ### _~  ~ ~~ #  #o oo#~_~~~ ~@ ~# o  Ooo#~_       ~# o oo~ # _ O/ \ ~~  ~oo   #~_   B    ##  ooo # _~ \ / ~  # o    #~_    O    ########~_  O~  ~  2 o o 2 %_X    ~~  1  oo 1  _",&
"   v  #o# oo     %_ ##v#     oo o  o _vO\v/ ### #   ~B~ _X  X  ### # o  ~~~_  /1\  ~O    ooo~~_ O#o##^ #### #o~ ~_O #~~> ~O<      ~ _ O# ###~#### #####_ ~O   # #~ o@o o ~_  ### #1# ~o ooOOX_%  >  o o~ o o  o _",&
" ~\# %  o ~ o~o   _ / >o ~ #^# O   % _   ###### ##X#### _~X  <    ~#   <   _  ~###^##  ~#####%_o/ ##  ### ###~ ##_ ~ #  OX####ooO o@_o ## O  O #oB   oo_/ ##   oO #o   X  _O #N~     #    Oo _ %# ~o o o< o o   _",&
"N      oo  o o o o_      o  o  o oOoo_ O     oo oo o    _  B   #######v###v_O   @ #  ooo  #o %_        ooo O # o _        o~  o # o _  #O  # Oo oO #o o_ #  #O#  ooo #  oo_    #     oo  #oo _X                 _",&
"  O  O ~\#/ O O~ ~_  # /#\    ~#^#^#v_X < # # %X% # #~# _O# o<o  X X ~    ~_  ~ >o    O  /####_## /####/o\\//\# %_%# \/~\/  O\/     _    ~ ~  X   O/O /_O# ##o\\O  /\ %\  _O# # O~#\~O\#\ O %_X# ~ @ >>  ~\#\/\/_",&
"  OO   \     \    _ ####\  % /    O O_   o #^##v# ##  # _ B@   O <~# ## #  _    o  ##v#  O # /_##v###   O# Oo # %_ / O # o%/X  O #~N_o  O #\  \/    #  _    O<        #   _ OXo #   /#O  # / _  o  # \          _",&
"~ /% # O\/ ~   ~ ~_ v  O  ~   ##~## ~_o>o~# O  O # O2#~#_  o# o  ~%~X1o #~\_  #o~~ /vvv#v#v#  _~# ~~/v#ooo#v#>>O _   o >1<   #v#### _######^/~~~#>o ~#~_X O o#~  ~ #2o  > _ o  o<~@ B ###~## _~ o~ #  ~ ~    ~~ _",&
"~      > ~ ~    o _  oOOO####### ~o o_  o~~ < >>> ##ooo _\ ~ O~#%# oO~ ~o o_/ X###   #o ~# o  _ @  >1o2X1~2 ~~oo _~~X### o##ooO#Xo  _#  ~#o  > o o #  ~_  # oB o# #% #~ ~ _~N  #o###^# ~O~~O _ #  N  < %   ~ ~~ _",&
"  > >>>o~  #%#%#~N_o X^XX#  O ~o~  ~ _  o^X/\   # #o# #~_ oX^X\/   #  ~o~  _o X^XX# ## #o#~#  _  X^<<< ~Oo  oo#  _ %XXXX# #  o #    _######~ ~##O##  @ _ o o o    X XoB  ~_o o o   O       ~ _ ~  o O   /#\   ~N_",&
" O  XX~O~## o #  %_^#~XOOOOO ~  oo ~~_  o~oXXXX##oo  ###_ XXX     #%   o   _     XXXv#### ## #_#oXX  XO o ~# # o#_X#  X   o ~ # #% #_% # #~#####v# ##o#_#~  #  o %# #oo   _~o#  # #^## #~# #v_%  # ~  o    o~@#~_",&
"          #      %_ #  #  # #  # N#  _  N  # N          _           #      _ #  #  # #O #  #  _  #  N  @         _            O #   _#N     O     #    _    #    O        _  #  # #       #  _            X    %_",&
"~   >O\~ 1 X#o~   _o1o >O oO O##~o # _  oo#oo o  >Oo o ~_ @  #   # #>O    o_~oO###### ## #####_o    \   B%     O _##~     # ##X o   _~~   X#^# # ## Oo _~O## #XOO~O o ##  _ Oo o<OO  ~   o #v_~ o ~#   oo o o   _",&
"% o     >N<    # X_% o ###v# #1#/  ~ _##^ #>>v~o~^<  /~ _  O   #2#~#^O /## _v# ##  o> #  // o _ # @  # # >O//~o  _ >  # # O #~/ ~ o _ ##  o   %    /\O _~##o#v# # # ~ ##\ _o#o >o ~ ~ B  ^2# _1#  # \ ~~ ~~  O  _",&
" o     \\//\\#   %_%~o O   \/  \/ # \_##v###\         oO_  O > #\~~#~#v##^^_ # # o~XX~X~~O~#  _ # #v~oO>vXO~~~#  _  Oo oOX^<~~~@~## _# ^^^  X XX~O~O<  _  #### #O  Xo~~# o_#v#   oo oB~~~~#o _~  o~# #   >~~~~  _",&
"% # #  o # o<   >2_ o# <~o o< o# o #v_ 2  #o o # o<oo  %_##v#####^##v#^##X#_X~Oo~#1%  O~#  o~ _~~ ~~>  @O~O#~  oo_~/ \~#   O  #~o O1_~oB~~<N    X###^##_~\~/~#####v## o > _~o~o~#o o o > oo  _~~~~~>~o   ~# o  ~_",&
"  o<   //  o  o ~~_ oOXo O< oo  o   ~_  ~#~ //  o o    ~_oOO# //  O Oo ~O~N_   # X Oo###   ~  _  o#    12 ####^##_#v#  @ # # # ~~~~ _ o  B  ooo #~2~oX~_###    # # #~1~ ~~_%%O  o# # oo#~~~~%_ o      >  ~# O/%%_",&
"%~###      ~X X  #_~~#   # # # # >O %_o1# ~    ~   O>~O#_###ooo#~B # # >O  _~~~      ~  # #   _~O~#####o#vv# ##o#_~~~< <~       # ~#_####~\ ##~##~##  <_#  # O \#o##o## 1%_~@O> O ~#  ~  O  \_# ~X O/ <  o ## OX_",&
"X%~###  #  / \%#  _   #  O      ~  O _ #O  /2\ /O\ /oO  _ OO o>   O@O >  o _     \1/ \O/ \#v#v_v##>~ ~ X       o _   # / \ /o\ /#O#v_ O~  ~B  1N2 < ~ O_ #   \ /o\ / ##  /_>o ~  %  ~  ~# oo _>^^##   # ~~/#>  X_",&
"~ ~~  X  % ~  ~ ~ _ OOo~ #   ~  O~ ~N_~O~##~ #O# O ~  ~~_~  ##  O1 #o~ OO ~_v#~~~ ## 2   ~  ~ _o# O~#  1 #o######_ #~~ # # 2O   O \%_  ###   #O#^#X    _  o   #^o  ^#   O _ B o@ # #%#N#~ O  _    o   #X# ~     _",&
"XX/  O        \  %_X~  ##v###o#\  ~~ _/ ~~#   ~<  #  ###_ ~ ~ B # <N #% #  _ ~O   ##o####  o  _  \ O  O@~#### #N _~ \^^^##~o~  # ###_ ~ ~  #%o# ~ #    _ ~ ~ o >^#^#voo##v_#1#^#1## ~ # ~ ~  _~~~> ~%# # ~  #   _",&
"~<~~>~~> o% 1#~<~%_o##^#~O##~####~##~_~~# #~OX#o> o~~~~~_o~#2#####~##v#~##~_~~#o##~~~o~~o###~~_~~~ ~~@O~B~~~~~~~#_~##~##~~~~~~~#v#v#_~####~2#~##v## o  _~~~X~  O~#1 # o## _\~O#v##^###o>o O  _~~~>~~    ~ <  o X_",&
" ~ O O O  X~ ~~~ ~_~ O O O   # ~~O~~ _o#^#####  #~~#~#~~_~# <~<~< 2#~O N~O~_ 1 #~# #v##~~#~# ~_##o~@~O#%%# ~~O~~~_~~~#v# ##v## ~  ~ _~X  o  o   # ~~2o _~O#O   o   #####^#_~~~ooO o B >o o   _1~~o  o    >o  o ~_",&
"N##~  //~       ~ _ ## ~//     O o  ~_ O  ///Xo\O# # oo _ ##  OX^>>O N O   _ ## oX#^#/O# # o  _ ~  ~>>>X   O     _  O  XXXo     B  #_1  ~~~~ /~ o    ##_  @  ~ //  ~o  ##~_ O  O // N ~  ##~ _ \~O //% ~  ~##~ 1_",&
" O\\   ~ X ~ <  o~_ ~ \\ O   o //~ o _~ o \\ o o //   o _ ~ o \\# #// o##^^_X\~ o \#O#/ ~ # o _1< B @%%N%% o <%2~_X/  o /# #\~ ~# o _   o //#%#\\ ~##v#_ oo //     \\  oo _~~ //  o2   \\O  ~_ O/1  o O /  < o~ _",&
"%    > #N  #%N # N_~#O##  ##~####~#~#_ #   @  ~~  ~~ ~ %_ o  o~ X##########_~#~~  ~#       ~  _~O   ## ~  O  #  ~_ # o   ~ /\    B~ _~#~  #~  \/  O~ ~~_~oo   ~  O  // X~ _~#~####O####/ O   _~    ~~    <      _",&
"X  O<~    o     ~ _ ~  #   #  oO # #~_~#O#2 ~#1# o #~~  _ O  # # o #X# #~# _~#O# # o o # N #  _~   #  ~B~~2#~#  #_ #1#%# o@oO#%     _ \# ~ #   X #%#  \_   # ~ #v# O # #  _ OO  oo# #O O     _~   oo~< >O    /  _",&
"o  o >o  o ~# ~ o~_  o  >o o o # oB  _ O O X oo o #~Ooo~_v##^##vvvvvv##X#^#_~  ~ XO o oo#~~o~~_ O O~# o~O  #~OoO~_oOo O#o o ~ #~~~~~_   o #vvvvvv##^###_#^##X# o o o>o  #~_ ~O  #~ o~o # o~~~_@ o~ # o o  >o  #~_",&
"   o<  o# o # o <~_ X  # o <o o< oo# _  O <o~o# o #o ~<~_ o o# o #~ o< ~o#~_o~ o<o o<oo # o~<@_ BO # o #  o<o  # _~O o<o o<oo # o <~_ o~ #~o #~ o<o~o#~_~o o<o o# ~ #~ o<~_~ O~# o <o o< o~# _ ~ o< ~o# ~ #  o<~_",&
"     # < ~#~> #1#2_~ooo #O#~ ~ #~#~#~_~o o   ##^#v# #v#v_  ~# o~#~o ~# o o _o~B # /#^##v##v#v#_@O ~ O> o # ~ OOO _o ~ ~ \## ## o N ~_ ~ ~ ~ XO~#%~ oO ~_ #~ ##\o# ####~ ~ _v~ ~O\# o~ < # ~ X_2#\ ~ ~1< o# ~  XX_",&
"  N   ~  #       N_ #### %# ## ##%## _ # #  ##   O ###~ _ # O~    ###  # ~ _   ### X  #  ~### _ # N#  ~  # o     _~~  o    ~    # ##_ ##   ~  N### # ~ _ O  ## ~~   #~ O~#_# #  # #~## # ##  _% #@   #  ~~~ #  %_",&
"/\#/\~/\#     #XO~_\//1@2  # #v#v##~~_/\\~o~\ # #2o ~##~_\/#\/#/ # #v#~1   _/\/\/\\ #   < ## o_X~ O o< ###^## oo _  o O < ##%  ~~O  _ ~ o o<o ##~O o o _ /\/##/ \##o O o o_v/\/~o B    o o O _ ~~~ /\o/#X O~ o  _",&
"XOOOOO > <%###  ~%_##~~## # #  >O ~ ~_XO~#%>      # O ~ _o~## ###o## >O ~ ~_ v#~1 #   # # # ##_oo~~# oN@No O # ##_  #~2##   # # >  %_~ ######o##~#^#o##_~2O \/     B    ~ _# XO   / \  o~# #~_% >~ ~/~1~\~   ~O%_"/)
  character(len=8), parameter :: passwd(ndeflvl) = (/"        ","ZARG    ","NAGGON  ",&
     "BOOGER  ","GORK    ","ZORCH   ","FREEM   ","VOID    ","WHAP    ","ZOKBAR  ",&
     "SLINKY  ","ZOUNDS  ","FLEA    ","ZINGG   ","TWINKY  ","TAILS   ","HOOPA   ",&
     "FWOOSH  ","BLAHHH  ","FANGS   ","SLOOP   ","EPOCH   ","CHOMP   ","SHEESH  ",&
     "BINK    ","MOK     ","SQUID   ","STOMP   ","TOXIC   ","SHOOF   ","GOOEY   ",&
     "KABLOOIE"/)

  integer, parameter :: iexit_beaten = 1
  integer, parameter :: iexit_failed = 2
  integer, parameter :: iexit_userexit = 3
  integer, parameter :: iexit_redo = 4

contains
  subroutine cstring(fstr,cstr)
    character(len=*), intent(in) :: fstr
    character(kind=c_char,len=1), allocatable, intent(inout) :: cstr(:)
    integer :: i, il

    il = len(fstr)
    if (allocated(cstr)) deallocate(cstr)
    allocate(cstr(il+1))
    do i = 1, il
       cstr(i) = fstr(i:i)
    end do
    cstr(il+1) = c_null_char
  end subroutine cstring

  subroutine upper(a)
    character*(*), intent(inout) :: a
    character(*), parameter :: lo = 'abcdefghijklmnopqrstuvwxyz'
    character(*), parameter :: up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    integer :: i, idx

    do i = 1, len_trim(a)
       idx = index(lo,a(i:i))
       if (idx > 0) a(i:i) = up(idx:idx)
    end do
  end subroutine upper

  subroutine helppanel(y0,x0)
    integer, intent(in) :: y0, x0

    character(kind=c_char,len=1), allocatable, target :: msg(:)
    integer :: istat, y, x

    y = y0
    x = max(x0,27)
    call cstring(symbol(c_player),msg)
    istat = attrset(symatt(c_player))
    istat = color_set(symcol(c_player),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Fudge: a puppy with a grudge. Move him with the arrow keys.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y0+1
    call cstring("   Clear earth, crystals, bombs, naggons, and canisters to win.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))
    
    y = y + 2
    call cstring(symbol(c_earth),msg)
    istat = attrset(symatt(c_earth))
    istat = color_set(symcol(c_earth),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Earth: clean it by walking over it.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_crystal),msg)
    istat = attrset(symatt(c_crystal))
    istat = color_set(symcol(c_crystal),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Crystal: gives you extra time.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_bomb),msg)
    istat = attrset(symatt(c_bomb))
    istat = color_set(symcol(c_bomb),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Bomb: surround it with 4 rocks to deactivate.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_naggon),msg)
    istat = attrset(symatt(c_naggon))
    istat = color_set(symcol(c_naggon),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Naggon: surround it with 4 canisters to kill it.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_canister),msg)
    istat = attrset(symatt(c_canister))
    istat = color_set(symcol(c_canister),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Canister: push them into a chomper (several at a time).",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_chomper),msg)
    istat = attrset(symatt(c_chomper))
    istat = color_set(symcol(c_chomper),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Chomper: eats canisters and dogs.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_rock),msg)
    istat = attrset(symatt(c_rock))
    istat = color_set(symcol(c_rock),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Rock: push them one at a time.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_teleport1),msg)
    istat = attrset(symatt(c_teleport1))
    istat = color_set(symcol(c_teleport1),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Teleport: move to the matching pair.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_arrowr),msg)
    istat = attrset(symatt(c_arrowr))
    istat = color_set(symcol(c_arrowr),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Arrow: pushes fudge in a direction.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

    y = y + 2
    call cstring(symbol(c_cornerup),msg)
    istat = attrset(symatt(c_cornerup))
    istat = color_set(symcol(c_cornerup),c_null_ptr)
    istat = mvinsstr(y,x,c_loc(msg))
    call normalfont()
    call cstring(" - Corner: you can push canisters around corners.",msg)
    istat = mvinsstr(y,x+1,c_loc(msg))

  contains
    subroutine normalfont()
      istat = attrset(A_NORMAL)
      istat = color_set(col_white_black,c_null_ptr)
    end subroutine normalfont
  end subroutine helppanel

end module global
