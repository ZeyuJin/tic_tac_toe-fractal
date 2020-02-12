    module some_type
    type returnmove
         integer :: index
         integer :: score
    end type returnmove
    end module some_type

program tic_tac_toe
    use some_type
    implicit none

    integer, dimension(1:9) :: board = 0
    integer :: playerx, move, result, firstplay, nextturn

    type(returnmove) :: aimove
    integer, external :: readmove, checkwinner, determinemove
    type(returnmove), external :: findmove_zeyu

    print *, 'Now it starts the game tic-tac-toe!'

    ! choose where to start
    do 
       print *, 'please choose start first or second (1 first and 2 second)'
       print *
       read *, playerx
       if (playerx == 1 .or. playerx == 2) then
          exit
       else
          print *, 'You enter the wrong number to start!'
       endif
    enddo

    firstplay = playerx
    call printboard(board, firstplay)

    if (playerx == 1) then
       print *, 'The user starts first'
       print *
       move = readmove(board)
       board(move) = 1
    else 
       print *, 'The computer starts first'
       print *
       ! aimove = findmove(board,2)
       ! board(aimove%index) = 2
       ! move = 5  ! stand the middle first
       ! board(move) = 2
       aimove = findmove_zeyu(board,2)
       board(aimove%index) = 2
       move = aimove%index
    endif

    call printboard(board, firstplay)

    do
       ! check the winner or drawn first
       result = checkwinner(board)
       if (result == 1) then
           print *, 'The user has won! Congratulations!'
           return
       else if (result == 2) then
           print *, 'The computer has won! Never mind!'
           return
       else if (result == 3) then
           print *, 'The game is drawn. Come on again!'
           return
       else
           print *
       endif

       nextturn = determinemove(move, playerx)
       if (nextturn == 1) then
          print *, 'The last move is computer. Now it is your turn!'
          print *
          playerx = nextturn
          move = readmove(board)
          board(move) = 1
       else ! nextturn == 2
          print *, 'You have moved this time, Now let computer moves!'
          print *
          playerx = nextturn
          aimove = findmove_zeyu(board,2)
          board(aimove%index) = 2
          move = aimove%index
       endif
       
       ! the firstplay is constant which can not be changed
       ! the playerx is changed between 1 and 2 every turn
       call printboard(board, firstplay)
    enddo

end program tic_tac_toe

integer function checkmove(board, move)
    implicit none
    integer, dimension(9) :: board
    integer :: move
    if (move > 9 .or. move < 1) then
        checkmove = 0
    else if (board(move) == 1 .or. board(move) == 2) then
        checkmove = 0
    else
        checkmove = 1
    endif
end function checkmove

integer function readmove(board)
    implicit none
    integer, dimension(9) :: board
    integer :: move, result
    integer, external :: checkmove

    do
        print *, "Please enter a move number: (0~9)"
        read *, move
        result = checkmove(board, move)

        if (result == 0) then
            print *, "Invalid move! Please reenter the move."
        else
            readmove = move
            exit
        endif
    enddo
end function readmove

integer function checkwinner(board)
   implicit none
   integer, dimension(1:9) :: board
   integer, dimension(8,3) :: win
   integer :: i, count

   win(1, 1:3) = (/ 1, 2, 3 /)
   win(2, 1:3) = (/ 4, 5, 6 /)
   win(3, 1:3) = (/ 7, 8, 9 /)
   win(4, 1:3) = (/ 1, 4, 7 /)
   win(5, 1:3) = (/ 2, 5, 8 /)
   win(6, 1:3) = (/ 3, 6, 9 /)
   win(7, 1:3) = (/ 1, 5, 9 /)
   win(8, 1:3) = (/ 3, 5, 7 /)

   do i = 1, 8
      if (board(win(i,1)) == 1 .and. board(win(i,2)) == 1 .and. board(win(i,3)) == 1) then
         checkwinner = 1
         return
      else if (board(win(i,1)) == 2 .and. board(win(i,2)) == 2 .and. board(win(i,3)) == 2) then
         checkwinner = 2
         return
      endif
   enddo

   count = 0
   do i = 1, 9
      if (board(i) /= 0) then
         count = count + 1
      else
         checkwinner = 0 ! means the game is still on
         return
      endif
   enddo

   if (count == 9) checkwinner = 3 ! the game is drawn
end function checkwinner

!integer function findmove(board)
!   implicit none
!   integer, dimension(9) :: board
!   integer :: i, count, index
!   integer, dimension(9) :: possible_move = 0 ! record all possible moves
!   real :: xran
!
!   count = 0
!   do i = 1, 9
!      if (board(i) == 0) then
!         count = count + 1
!         possible_move(count) = i
!      endif
!   enddo
!
!   call random_seed()
!   call random_number(xran)
!   index = ceiling(xran * count)
!   findmove = possible_move(index) ! return a random valid move
!
!end function findmove

recursive function findmove_zeyu(board, player) result(moveresult)
   ! player here 1 for the user, 2 for the computer
   ! for the findmove function in main program, player == 2
   use some_type
   implicit none
   integer, dimension(9) :: board, possible_move = 0
!   type returnmove
!        integer :: index
!        integer :: score
!   end type returnmove
   type(returnmove) :: interesult, moveresult

   integer, dimension(:), allocatable :: availmove  ! record all possible moves
   integer :: i, j, count, res, player, num, bestscore, bestmove
   type(returnmove), dimension(9) :: movestr
   integer, external :: checkwinner

   count = 0
   do i = 1, 9
      if (board(i) == 0) then
         count = count + 1  ! count the number of available spots to move
         possible_move(count) = i
      endif
   enddo

   allocate(availmove(count))
   do j = 1, count
      availmove(j) = possible_move(j)
   enddo

   res = checkwinner(board)
   if (res == 1) then
      moveresult%score = -10
      return
   else if (res == 2) then
      moveresult%score = 10
      return
   else if (res == 3) then
      moveresult%score = 0
      return
   endif

   if (count == 9) then  ! the empty board
      moveresult%index = 5
      moveresult%score = 10
      return
   endif

   num = 0
   do i = 1, count
      num = num + 1
      moveresult%index = availmove(i)
      board(availmove(i)) = player

      if (player == 2) then  ! the computer sets
         interesult = findmove_zeyu(board, 1)
         moveresult%score = interesult%score
      else
         interesult = findmove_zeyu(board, 2)
         moveresult%score = interesult%score
      endif
      board(availmove(i)) = 0
      movestr(num) = moveresult
   enddo

   if (player == 2) then
      bestscore = -1000
      do i = 1, num
         if (movestr(i)%score > bestscore) then
            bestscore = movestr(i)%score
            bestmove = i
         endif
      enddo

   else
      bestscore = 1000
      do i = 1, num
         if (movestr(i)%score < bestscore) then
            bestscore = movestr(i)%score
            bestmove = i
         endif
      enddo
   endif

   moveresult = movestr(bestmove)
end function findmove_zeyu

! determine who moves next
integer function determinemove(move, playerx)
   implicit none
   integer :: move, playerx
   if (playerx == 1) then
      print *, 'The user has moved into number ', move
      print *
      determinemove = 2

   else ! playex == 2
      print *, 'The computer has moved into number ', move
      print *
      determinemove = 1

   endif
end function determinemove

