subroutine printboard(board, playerx)
    implicit none
    integer, dimension(9) :: board
    character (len=9) :: cboard
    integer :: i, playerx

    do i = 1,9
        if (board(i) == 0) then
            cboard(i:i) = ' '
        else if (board(i) == 1) then
             if (playerx == 1) then
                cboard(i:i) = 'x'
             else if (playerx == 2) then
                cboard(i:i) = 'o'
             else
                 print *, "There is something wrong with player order!"
             endif
        else if (board(i) == 2) then
             if (playerx == 1) then
                cboard(i:i) = 'o'
             else if (playerx == 2) then
                cboard(i:i) = 'x'
             else
                 print *, "There is something wrong with player order!"
             endif
        else
             print *, "There is wrong number in board matrix!"
        endif
    enddo

print *, ' ', cboard(1:1), ' | ', cboard(2:2), ' | ', cboard(3:3), ' '
print *, '---+---+---'
print *, ' ', cboard(4:4), ' | ', cboard(5:5), ' | ', cboard(6:6), ' '
print *, '---+---+---'
print *, ' ', cboard(7:7), ' | ', cboard(8:8), ' | ', cboard(9:9), ' '
end subroutine printboard
