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
module levelmod
  use ncurses
  use global
  implicit none

  private
  
  type sprite
     integer :: id = 0
     integer :: typ = 0
     integer :: x = 0
     integer :: y = 0
     integer*8 :: nf = 0
     integer*8 :: mf = 0
     logical :: repeat = .false.
     character(len=1), allocatable :: char(:)
     integer(c_short), allocatable :: color(:)
     integer(c_long), allocatable :: attr(:)
   contains
     procedure :: render => render_sprite
  end type sprite

  type char
     integer :: typ = 0
     integer :: x = 0
     integer :: y = 0
   contains
     procedure :: render => render_char
     procedure :: push => push_char
     procedure :: idle => idle_char
     procedure :: kill => kill_char
  end type char

  type level
     logical :: isinit = .false. ! is initialized
     character(len=:), allocatable :: title 
     character(len=:), allocatable :: passwd
     character(len=:), allocatable :: errmsg ! error message
     integer :: nx = 0 ! map size
     integer :: ny = 0
     type(char) :: pl ! player
     integer, allocatable :: map(:,:) ! map
     integer, allocatable :: mapobj(:,:) ! active objects on the map
     integer :: nnpc = 0 ! number of npc
     type(char), allocatable :: npc(:) ! npc active objects
     integer :: xtep(2,0:9) ! teleport positions
     integer :: ytep(2,0:9) ! teleport positions
     integer*8 :: ntime = 0 ! number of frames (blue timer)
     integer*8 :: ntime_bomb = 0 ! number of frames (bomb timer)
     integer :: nspr = 0
     type(sprite), allocatable :: spr(:)
   contains
     procedure :: end => end_level
     procedure :: run => run_level
     procedure :: read => read_level
     procedure :: isbeaten
     procedure :: isfailed
     procedure :: render => render_level
     procedure :: update_player
     procedure :: update_npcs
     procedure :: addframe_timers
     procedure :: addsprite
  end type level
  public :: level

contains

  subroutine end_level(l)
    class(level), intent(inout) :: l
    l%isinit = .false.
    l%errmsg = ""
    l%nx = 0
    l%ny = 0
    l%pl%typ = 0
    l%pl%x = 0
    l%pl%y = 0
    if (allocated(l%map)) deallocate(l%map)
    if (allocated(l%mapobj)) deallocate(l%mapobj)
    l%nnpc = 0
    if (allocated(l%npc)) deallocate(l%npc)
    l%ntime = 0
    l%ntime_bomb = 0
    l%nspr = 0
    if (allocated(l%spr)) deallocate(l%spr)
  end subroutine end_level

  subroutine run_level(l,iexit)
    class(level), intent(inout) :: l
    integer, intent(out) :: iexit
    
    integer*8 :: irate, imax, ilast, icur
    integer :: ich, ich0, istat

    iexit = 0
    call system_clock(count_rate=irate,count_max=imax)
    call system_clock(ilast)
    icur = ilast
    nframe = 0
    do while (.true.)
       ! wait for the next tic and read input
       ich = 0
       do while (real(icur-ilast,8) / irate * fps < 1d0)
          ich0 = getch()
          if (ich0 > 0) ich = ich0
          call system_clock(icur)
          if (icur < ilast) ilast = ilast - imax
       end do
       ilast = icur 
       nframe = nframe + 1

       call l%addframe_timers()
       call l%update_player(ich)
       call l%update_npcs()
       call l%render()
       istat = refresh()
       if (ich == ichar('q')) then
          iexit = iexit_userexit
       elseif (ich == ichar('r')) then
          iexit = iexit_redo
       elseif (ich == ichar('h')) then
          showhelp = .not.showhelp
       elseif (l%isbeaten()) then
          iexit = iexit_beaten
       elseif (l%isfailed()) then
          iexit = iexit_failed
       end if
       if (iexit > 0) exit
    end do

  end subroutine run_level

  subroutine read_level(l,title,passwd,file,idef)
    class(level), intent(inout) :: l
    character*(*), intent(in) :: title, passwd
    character*(*), intent(in), optional :: file
    integer, intent(in), optional :: idef

    integer, parameter :: lu = 7

    character(len=1) :: c
    logical :: ok
    integer :: ios, nnx, nny, nobj, lp
    integer :: ntep(0:9), id

    l%title = trim(title)
    l%passwd = trim(passwd)
    if (present(file)) then
       inquire(file=file,exist=ok)
       if (.not.ok) then
          l%errmsg = "file not found"
          return
       end if
       open(unit=lu,file=file,status='old',iostat=ios,form="formatted",access="stream")
       if (ios /= 0) then
          l%errmsg = "could not open file"
          goto 99
       end if
    elseif (present(idef)) then
       open(unit=lu,status='scratch',form="formatted",iostat=ios)
       if (ios /= 0) then
          l%errmsg = "could not open temporary file"
          goto 99
       end if
       lp = 1
       id = index(deflvl(idef)(lp:),"_")
       do while(id > 0)
          write (lu,'(A)') deflvl(idef)(lp:lp+id-2)
          lp = lp+id
          id = index(deflvl(idef)(lp:),"_")
       end do
       rewind(lu)
    end if

    ! first pass, count rows, columns, objects
    l%nx = 0
    l%ny = 0
    nnx = 0
    nobj = 0
    ntep = 0
    do while(.true.)
       read(lu,'(A)',advance="no",iostat=ios) c
       if (is_iostat_eor(ios)) then
          l%nx = max(l%nx,nnx)
          l%ny = l%ny + 1
          nnx = 0
       else
          nnx = nnx + 1
       end if
       if (is_iostat_end(ios)) exit 
       if (c=="X".or.c=="o".or.c=="N".or.c=="O".or.c=="B") &
          nobj = nobj + 1
    end do 

    ! read the map information
    allocate(l%map(l%nx+2*borx,l%ny+2*bory))
    allocate(l%mapobj(l%nx+2*borx,l%ny+2*bory))
    l%map = c_empty
    l%mapobj = 0
    l%nnpc = nobj
    allocate(l%npc(nobj))

    ! border wall
    l%map(:,1:bory) = c_wall
    l%map(:,l%ny+bory+1:l%ny+2*bory) = c_wall
    l%map(1:borx,:) = c_wall
    l%map(l%nx+borx+1:l%nx+2*borx,:) = c_wall
    l%nx = l%nx + 2 * borx
    l%ny = l%ny + 2 * bory

    nnx = 0 + borx
    nny = 1 + bory
    nobj = 0
    rewind(lu)
    do while(.true.)
       read(lu,'(A)',advance="no",iostat=ios) c
       if (is_iostat_eor(ios)) then
          nny = nny + 1
          nnx = 0 + borx
          if (is_iostat_end(ios)) exit 
          cycle
       else
          nnx = nnx + 1
       end if
       if (is_iostat_end(ios)) exit 
       select case (c)
       case(" ")
          l%map(nnx,nny) = c_empty
       case("#")
          l%map(nnx,nny) = c_wall
       case("@")
          l%pl%typ = c_player
          l%pl%x = nnx
          l%pl%y = nny
          l%map(nnx,nny) = c_empty
          l%mapobj(nnx,nny) = -1
       case("~")
          l%map(nnx,nny) = c_earth
       case("0")
          l%map(nnx,nny) = c_teleport0
       case("1")
          l%map(nnx,nny) = c_teleport1
       case("2")
          l%map(nnx,nny) = c_teleport2
       case("3")
          l%map(nnx,nny) = c_teleport3
       case("4")
          l%map(nnx,nny) = c_teleport4
       case("5")
          l%map(nnx,nny) = c_teleport5
       case("6")
          l%map(nnx,nny) = c_teleport6
       case("7")
          l%map(nnx,nny) = c_teleport7
       case("8")
          l%map(nnx,nny) = c_teleport8
       case("9")
          l%map(nnx,nny) = c_teleport9
       case("<")
          l%map(nnx,nny) = c_arrowl
       case(">")
          l%map(nnx,nny) = c_arrowr
       case("^")
          l%map(nnx,nny) = c_arrowu
       case("v")
          l%map(nnx,nny) = c_arrowd
       case("/")
          l%map(nnx,nny) = c_cornerup
       case("\") ! " stupid emacs
          l%map(nnx,nny) = c_cornerdn
       case("%")
          l%map(nnx,nny) = c_crystal
       case("X","o","N","O","B")
          nobj = nobj + 1
          l%npc(nobj)%x = nnx
          l%npc(nobj)%y = nny
          if (c=="X") then
             l%npc(nobj)%typ = c_chomper
          elseif (c=="o") then
             l%npc(nobj)%typ = c_rock
          elseif (c=="N") then
             l%npc(nobj)%typ = c_naggon
          elseif (c=="O") then
             l%npc(nobj)%typ = c_canister
          elseif (c=="B") then
             l%npc(nobj)%typ = c_bomb
          end if
          l%map(nnx,nny) = c_empty
          l%mapobj(nnx,nny) = nobj
       case("+")
          l%map(nnx,nny) = c_hole
       case default
          l%errmsg = "unknown symbol"
          goto 99
       end select

       if (l%map(nnx,nny) >= c_teleport0 .and. l%map(nnx,nny) <= c_teleport9) then
          id = l%map(nnx,nny) - c_teleport0
          ntep(id) = ntep(id) + 1
          if (ntep(id) > 2) then
             l%errmsg = "teleports must come in pairs"
             goto 99
          end if
          l%xtep(ntep(id),id) = nnx
          l%ytep(ntep(id),id) = nny
       end if
    end do 

    if (any((ntep /= 0) .and. (ntep /= 2))) then
       l%errmsg = "teleports must come in pairs"
       goto 99
    end if
    if (l%isbeaten()) then
       l%errmsg = "level is already completed"
       goto 99
    end if
    if (l%pl%typ == 0) then
       l%errmsg = "no player found in the map"
       goto 99
    end if

    l%nspr = 4
    allocate(l%spr(4))
    l%isinit = .true.
    l%errmsg = ""

99  continue
    close(lu)

  end subroutine read_level

  logical function isbeaten(l)
    class(level), intent(in) :: l

    integer :: i

    isbeaten = all(l%map /= c_earth .and. l%map /= c_crystal)
    do i = 1, l%nnpc
       isbeaten = isbeaten .and..not.(l%npc(i)%typ == c_crystal.or.&
          l%npc(i)%typ == c_naggon.or.l%npc(i)%typ == c_canister.or.&
          l%npc(i)%typ == c_bomb)
    end do
    if (any(l%map == c_hole)) then
       where (l%map == c_hole)
          isbeaten = isbeaten .and. (l%mapobj == c_rock)
       end where
    end if
    isbeaten = isbeaten .and.all(l%spr(1:l%nspr)%typ /= spr_explosion)

  end function isbeaten

  logical function isfailed(l)
    class(level), intent(in) :: l

    isfailed = .not.(l%pl%typ > 0)
    isfailed = isfailed .or. (l%ntime >= maxtime * fps)
    isfailed = isfailed .or. (l%ntime_bomb >= maxtime_bomb * fps)
    isfailed = isfailed .and.all(l%spr(1:l%nspr)%typ /= spr_explosion)

  end function isfailed

  subroutine render_level(l)
    class(level), intent(inout) :: l
    
    integer :: ix, iy, istat, id, i
    character(kind=c_char,len=1), target :: c(2)
    character(kind=c_char,len=1), allocatable, target :: msg(:)

    istat = clear()

    do ix = 1, l%nx
       do iy = 1, l%ny
          id = l%map(ix,iy)
          if (id < 0) cycle
          istat = attrset(symatt(id))
          istat = color_set(symcol(id),c_null_ptr)
          c = (/symbol(id),c_null_char/)
          istat = mvaddstr(dyg+iy,dxg+ix,c_loc(c))
       end do
    end do

    do i = 1, l%nspr
       if (l%spr(i)%typ == 0) cycle
       call l%spr(i)%render()
    end do

    call l%pl%render(l)

    do i = 1, l%nnpc
       call l%npc(i)%render(l)
    end do

    call cstring(l%title,msg)
    istat = attrset(A_BOLD)
    istat = color_set(col_white_black,c_null_ptr)
    istat = mvaddstr(dyg-1,dxg+3,c_loc(msg))
    if (len_trim(l%passwd) > 0) then
       call cstring("(" // l%passwd // ")",msg)
       istat = attrset(A_BOLD)
       istat = color_set(col_yellow_black,c_null_ptr)
       istat = mvaddstr(dyg-1,dxg+4+len_trim(l%title),c_loc(msg))
    end if

    call cstring("Bomb",msg)
    istat = attrset(A_BOLD)
    istat = color_set(col_red_black,c_null_ptr)
    istat = mvaddstr(dyg+1,dxg+l%nx+3,c_loc(msg))
    call render_timepanel(dyg+3,dxg+l%nx+3,l%ny-4,4,col_red_red," ",&
       l%ntime_bomb,int(maxtime_bomb*fps,8))

    call cstring("Time",msg)
    istat = attrset(A_BOLD)
    istat = color_set(col_blue_black,c_null_ptr)
    istat = mvaddstr(dyg+1,dxg+l%nx+8,c_loc(msg))
    call render_timepanel(dyg+3,dxg+l%nx+8,l%ny-4,4,col_blue_blue," ",&
       l%ntime,int(maxtime*fps,8))

    call cstring("q:quit r:restart h:help",msg)
    istat = attrset(A_BOLD)
    istat = color_set(col_white_black,c_null_ptr)
    istat = mvaddstr(dyg+l%ny+2,dxg,c_loc(msg))

    if (showhelp) call helppanel(dyg,dxg+l%nx+14)

  end subroutine render_level

  subroutine update_player(l,ich)
    class(level), intent(inout) :: l
    integer, intent(in) :: ich

    logical :: ok
    logical, save :: hadch = .false.

    if (l%pl%typ /= c_player) return
    if (ich > 0) then
       hadch = .true.
       ok = l%pl%push(l,ich)
    elseif (modulo(nframe,fpidle) == 0) then
       if (.not.hadch) call l%pl%idle(l)
       hadch = .false.
    end if

  end subroutine update_player

  subroutine update_npcs(l)
    class(level), intent(inout) :: l

    integer :: i, j, id, dx, dy
    logical :: lexp, ok
    integer :: xd(4) = (/1,-1, 0, 0/)
    integer :: yd(4) = (/0, 0, 1,-1/)

    do i = 1, l%nnpc
       if (l%npc(i)%typ == c_bomb .or. l%npc(i)%typ == c_naggon) then
          lexp = .true.
          do j = 1, 4
             id = l%mapobj(l%npc(i)%x+xd(j),l%npc(i)%y+yd(j))
             lexp = lexp .and. (id > 0)
             if (id > 0) lexp = lexp .and. &
                (l%npc(id)%typ == c_rock .and. l%npc(i)%typ == c_bomb .or.&
                 l%npc(id)%typ == c_canister .and. l%npc(i)%typ == c_naggon)
          end do
          if (lexp) call l%npc(i)%kill(l,.true.)
       end if
       if (l%npc(i)%typ == c_naggon .and. l%pl%typ /= 0 .and. &
          modulo(nframe,fpnaggon) == 0) then
          dx = l%npc(i)%x - l%pl%x
          dy = l%npc(i)%y - l%pl%y
          if (abs(dx) > abs(dy) .and. abs(dx) > 0) then
             if (dx > 0) then
                ok = l%npc(i)%push(l,KEY_LEFT)
             elseif (dx < 0) then
                ok = l%npc(i)%push(l,KEY_RIGHT)
             end if
             if (.not.ok) then
                if (dy > 0) then
                   ok = l%npc(i)%push(l,KEY_UP)
                elseif (dy < 0) then
                   ok = l%npc(i)%push(l,KEY_DOWN)
                end if
             end if
          elseif (abs(dy) >= abs(dx) .and. abs(dy) > 0) then
             if (dy > 0) then
                ok = l%npc(i)%push(l,KEY_UP)
             elseif (dy < 0) then
                ok = l%npc(i)%push(l,KEY_DOWN)
             end if
             if (.not.ok) then
                if (dx > 0) then
                   ok = l%npc(i)%push(l,KEY_LEFT)
                elseif (dx < 0) then
                   ok = l%npc(i)%push(l,KEY_RIGHT)
                end if
             end if
          else
             call l%pl%kill(l,.true.)
          end if
       end if
    end do

  end subroutine update_npcs

  subroutine addframe_timers(l)
    class(level), intent(inout) :: l
    
    l%ntime = l%ntime + 1
    if (any(l%npc(1:l%nnpc)%typ == c_bomb)) &
       l%ntime_bomb = l%ntime_bomb + 1
    if (l%ntime >= maxtime * fps .or. l%ntime_bomb >= maxtime_bomb * fps) &
       call l%pl%kill(l,.true.)

  end subroutine addframe_timers

  subroutine addsprite(l,y,x,tspr)
    class(level), intent(inout) :: l
    integer, intent(in) :: x, y
    integer, intent(in) :: tspr

    integer :: i, j, n, id

    id = 0
    do i = 1, l%nspr
       if (l%spr(i)%typ == 0) then
          id = i
          exit
       end if
    end do
    if (id == 0) then
       call realloc_sprite(l%spr,2*l%nspr)
       id = l%nspr + 1
       l%nspr = 2 * l%nspr
    end if
    
    associate (s => l%spr(id))
      s%id = id
      s%typ = tspr
      s%x = x
      s%y = y
      s%nf = 0
      if (allocated(s%char)) deallocate(s%char)
      if (allocated(s%color)) deallocate(s%color)
      if (allocated(s%attr)) deallocate(s%attr)
      if (tspr == spr_explosion) then
         s%repeat = spr_explosion_repeat
         s%mf = sum(spr_explosion_nframe)
         allocate(s%char(s%mf),s%color(s%mf),s%attr(s%mf))
         n = 0
         do i = 1, spr_explosion_nstep
            do j = 1, spr_explosion_nframe(i)
               n = n + 1
               s%char(n) = spr_explosion_char(i)
               s%color(n) = spr_explosion_color(i)
               s%attr(n) = spr_explosion_attr(i)
            end do
         end do
      end if
    end associate

  end subroutine addsprite

  subroutine render_sprite(s)
    class(sprite), intent(inout) :: s

    character(kind=c_char,len=1), target :: c(2)
    integer :: istat

    if (s%typ == 0) return
    s%nf = s%nf + 1
    if (s%nf > s%mf) then
       if (s%repeat) then
          s%nf = s%nf - s%mf
       else
          s%id = 0
          s%typ = 0
          deallocate(s%char,s%color,s%attr)
          return
       end if
    end if

    istat = attrset(s%attr(s%nf))
    istat = color_set(s%color(s%nf),c_null_ptr)
    c = (/s%char(s%nf),c_null_char/)
    istat = mvaddstr(dyg+s%y,dxg+s%x,c_loc(c))

  end subroutine render_sprite

  subroutine render_char(ch,l)
    class(char), intent(in) :: ch
    type(level), intent(in) :: l

    integer :: id, istat

    character(kind=c_char,len=1), target :: c(2)

    id = ch%typ
    if (id == 0) return
    if (id == c_rock .and. l%map(ch%x,ch%y) == c_hole) then
       istat = attrset(symatt(c_hole))
       istat = color_set(symcol(c_hole),c_null_ptr)
    else
       istat = attrset(symatt(id))
       istat = color_set(symcol(id),c_null_ptr)
    end if
    c = (/symbol(id),c_null_char/)
    istat = mvaddstr(dyg+ch%y,dxg+ch%x,c_loc(c))

  end subroutine render_char

  recursive function push_char(ch,l,dir) result(ok)
    class(char), intent(inout) :: ch
    type(level), intent(inout) :: l
    integer, intent(in) :: dir

    logical :: ok, tailtel
    integer :: ityp0, ityp1, nx0, nx1, ny0, ny1, iobj0, iobj1
    integer :: id
    
    ityp0 = ch%typ
    nx0 = ch%x
    ny0 = ch%y
    iobj0 = l%mapobj(nx0,ny0)

    ok = .false.
    tailtel = .false.
    if (dir == mov_up) then
       nx1 = ch%x
       ny1 = ch%y - 1
    elseif (dir == mov_down) then
       nx1 = ch%x
       ny1 = ch%y + 1
    elseif (dir == mov_right) then
       nx1 = ch%x + 1
       ny1 = ch%y
    elseif (dir == mov_left) then
       nx1 = ch%x - 1
       ny1 = ch%y
    else
       return
    end if
    ityp1 = l%map(nx1,ny1)
    iobj1 = l%mapobj(nx1,ny1)
    if (iobj1 > 0) then
       ityp1 = l%npc(iobj1)%typ
    elseif (iobj1 < 0) then
       ityp1 = c_player
    end if

    if (ityp1 == c_empty .or. ityp1 == c_hole) then
       ok = .true.
    elseif (ityp0 == c_player .and. ityp1 == c_arrowl) then
       ok = (dir /= mov_right)
    elseif (ityp0 == c_player .and. ityp1 == c_arrowr) then
       ok = (dir /= mov_left)
    elseif (ityp0 == c_player .and. ityp1 == c_arrowu) then
       ok = (dir /= mov_down)
    elseif (ityp0 == c_player .and. ityp1 == c_arrowd) then
       ok = (dir /= mov_up)
    elseif (ityp0 == c_player .and. ityp1 >= c_teleport0 .and. ityp1 <= c_teleport9) then
       id = ityp1 - c_teleport0
       if (nx1 == l%xtep(1,id) .and. ny1 == l%ytep(1,id)) then
          nx1 = l%xtep(2,id)
          ny1 = l%ytep(2,id)
       else
          nx1 = l%xtep(1,id)
          ny1 = l%ytep(1,id)
       end if
       ok = .true.
       tailtel = .true.
    elseif (ityp0 == c_player .and. ityp1 == c_earth) then
       l%map(nx1,ny1) = c_empty
       ok = .true.
    elseif (ityp0 == c_player .and. ityp1 == c_rock) then
       ok = l%npc(iobj1)%push(l,dir)
    elseif (ityp0 == c_player .and. ityp1 == c_canister) then
       ok = l%npc(iobj1)%push(l,dir)
    elseif (ityp0 == c_canister .and. ityp1 == c_canister) then
       ok = l%npc(iobj1)%push(l,dir)
    elseif (ityp0 == c_canister .and. ityp1 == c_cornerup) then
       if (dir == mov_up) then
          ok = (l%map(nx0+1,ny0) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0+1,ny0) == 0)
          ok = ok .and. (l%map(nx0+1,ny0-1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0+1,ny0-1) == 0)
          nx1 = nx0 + 1
          ny1 = ny0 - 1
       elseif (dir == mov_down) then
          ok = (l%map(nx0-1,ny0) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0-1,ny0) == 0)
          ok = ok .and. (l%map(nx0-1,ny0+1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0-1,ny0+1) == 0)
          nx1 = nx0 - 1
          ny1 = ny0 + 1
       elseif (dir == mov_right) then
          ok = (l%map(nx0,ny0-1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0,ny0-1) == 0)
          ok = ok .and. (l%map(nx0+1,ny0-1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0+1,ny0-1) == 0)
          nx1 = nx0 + 1
          ny1 = ny0 - 1
       elseif (dir == mov_left) then
          ok = (l%map(nx0,ny0+1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0,ny0+1) == 0)
          ok = ok .and. (l%map(nx0-1,ny0+1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0-1,ny0+1) == 0)
          nx1 = nx0 - 1
          ny1 = ny0 + 1
       end if
    elseif (ityp0 == c_canister .and. ityp1 == c_cornerdn) then
       if (dir == mov_up) then
          ok = (l%map(nx0-1,ny0) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0-1,ny0) == 0)
          ok = ok .and. (l%map(nx0-1,ny0-1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0-1,ny0-1) == 0)
          nx1 = nx0 - 1
          ny1 = ny0 - 1
       elseif (dir == mov_down) then
          ok = (l%map(nx0+1,ny0) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0+1,ny0) == 0)
          ok = ok .and. (l%map(nx0+1,ny0+1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0+1,ny0+1) == 0)
          nx1 = nx0 + 1
          ny1 = ny0 + 1
       elseif (dir == mov_right) then
          ok = (l%map(nx0,ny0+1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0,ny0+1) == 0)
          ok = ok .and. (l%map(nx0+1,ny0+1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0+1,ny0+1) == 0)
          nx1 = nx0 + 1
          ny1 = ny0 + 1
       elseif (dir == mov_left) then
          ok = (l%map(nx0,ny0-1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0,ny0-1) == 0)
          ok = ok .and. (l%map(nx0-1,ny0-1) == c_empty.or.l%map(nx0+1,ny0) == c_hole).and.(l%mapobj(nx0-1,ny0-1) == 0)
          nx1 = nx0 - 1
          ny1 = ny0 - 1
       end if
    elseif (ityp0 == c_canister .and. ityp1 == c_chomper) then
       call l%npc(iobj0)%kill(l,.false.)
       ok = .true.
       return
    elseif (ityp0 == c_player .and. ityp1 == c_crystal) then
       ok = .true.
       l%map(nx1,ny1) = c_empty
       l%ntime = max(l%ntime - addtime_crystal*fps,0_8)
    elseif (ityp0 == c_player .and. (ityp1 == c_chomper .or. ityp1 == c_naggon) .or.&
            ityp0 == c_naggon .and. ityp1 == c_player) then
       call l%pl%kill(l,.true.)
       ok = .true.
       return
    else
       ok = .false.
    end if

    if (ok) then
       l%mapobj(nx1,ny1) = l%mapobj(nx0,ny0)
       l%mapobj(nx0,ny0) = 0
       ch%x = nx1
       ch%y = ny1
    end if
    
    if (tailtel) then
       ok = ch%push(l,dir)
    end if

  end function push_char

  subroutine idle_char(ch,l)
    class(char), intent(inout) :: ch
    type(level), intent(inout) :: l

    logical :: ok

    if (ch%typ == c_player) then
       if (l%map(ch%x,ch%y) == c_arrowl) then
          ok = ch%push(l,KEY_LEFT)
       elseif (l%map(ch%x,ch%y) == c_arrowr) then
          ok = ch%push(l,KEY_RIGHT)
       elseif (l%map(ch%x,ch%y) == c_arrowu) then
          ok = ch%push(l,KEY_UP)
       elseif (l%map(ch%x,ch%y) == c_arrowd) then
          ok = ch%push(l,KEY_DOWN)
       end if
    end if

  end subroutine idle_char

  subroutine kill_char(ch,l,explode)
    class(char), intent(inout) :: ch
    type(level), intent(inout) :: l
    logical, intent(in) :: explode

    ch%typ = 0
    l%mapobj(ch%x,ch%y) = 0
    if (explode) &
       call l%addsprite(ch%y,ch%x,spr_explosion)

  end subroutine kill_char

  subroutine render_timepanel(y,x,yc,xc,color,char,nf,mf)
    integer, intent(in) :: y, x, yc, xc
    integer(c_short), intent(in) :: color
    character(len=1), intent(in) :: char
    integer*8, intent(in) :: nf, mf
    
    integer :: ntot, ndraw, iy, ix, idraw, istat
    character(kind=c_char,len=1), target :: c(2)

    c = (/char,c_null_char/)

    ntot = yc * xc
    ndraw = int(ntot - (nf * ntot) / mf,4)
    idraw = 0
    do ix = 1, xc
       do iy = 1, yc
          idraw = idraw + 1
          if (idraw <= ndraw) then
             istat = color_set(color,c_null_ptr)
             istat = mvaddstr(y+iy-1,x+ix-1,c_loc(c))
          else
             return
          end if
       end do
    end do

  end subroutine render_timepanel

  !> Adapt the size of an allocatable 1D type(atom) array
  subroutine realloc_sprite(a,nnew)
    type(sprite), intent(inout), allocatable :: a(:)
    integer, intent(in) :: nnew
    
    type(sprite), allocatable :: temp(:)
    integer :: nold
    
    if (.not.allocated(a)) then
       allocate(a(nnew))
       return
    end if
    nold = size(a)
    if (nold == nnew) return
    allocate(temp(nnew))
    
    temp(1:min(nnew,nold)) = a(1:min(nnew,nold))
    call move_alloc(temp,a)

  end subroutine realloc_sprite

end module levelmod

