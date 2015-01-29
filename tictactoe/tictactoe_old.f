c-------------------------------------------------------------------------------------------------c
      program tictactoe
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This program generates all the unique tic tac toe boards in order to create a neural network   c
c  that will find the optimum path without teaching the rules of tic tac toe to the computer.     c
c  It should be quite interesting.                                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c variables to generate the tic tac toe boards      
      integer num_boards, num_boards_per_move(0:9), num_terminal(0:9), test_board(9), index, index_old, index_new
      logical board_match, check_terminal, unique, new_next_move, terminal_board(1000), x_win(1000), o_win(1000)
      integer x1, x2, x3, x4, winner
      integer*8 counter, start

c neural network variables
      integer beads(1000,7), num_beads(1000), move(9), nn_board(0:9), n_moves, idum, offset
      logical done
      double precision rand, random
      character*10 words(7)

c boards common block      
      integer boards(1000,9), num_moves(1000), next_moves(1000,9), num_next_moves(1000)
      common/boards/ boards, num_moves, next_moves, num_next_moves
      
c initialize some variables
      do x1=1,1000
        do x2=1,9
          boards(x1,x2) = 0
          next_moves(x1,x2) = 0
        enddo
        num_moves(x1) = 0
        num_next_moves(x1) = 0
        terminal_board(x1) = .false.
        x_win(x1) = .false.
        o_win(x1) = .false.
      enddo
      do x1=0,9
        num_terminal(x1) = 0
      enddo

c initialize the number of boards per move with 0 moves (i.e. 1 board which is blank)
      num_boards = 1
      index_old = 1
      index_new = 1
      num_boards_per_move(0) = index_new-index_old+1
      
c loop over the number of moves
      do x1=1,9
c loop over the number of boards with x1-1 moves
        do index=index_old, index_new
c loop over the potential empty spaces on the board
          do x2=1,9
c make sure that the board isn't a terminal board and has an empty space at location x2
            if ((.not.terminal_board(index)).and.(boards(index,x2).eq.0)) then
              do x3=1,9
                test_board(x3) = boards(index,x3)
              enddo

c If x1 is odd then place an X (=1) in the open space, if x1 is even then place an O (=2)
              if (((x1/2)*2).ne.x1) then
                test_board(x2) = 1
              else
                test_board(x2) = 2
              endif

c loop over all the boards to see if this one is unique
              unique = .true.
        
              do x3=1,num_boards
                if ((unique).and.(num_moves(x3).eq.x1)) then
c The first non-unique board that matches is added to the list of next moves
                  if (board_match(x3,test_board)) then
                    unique = .false.
                    new_next_move = .true.
                    do x4=1,num_next_moves(index)
                      if (next_moves(index,x4).eq.x3) new_next_move = .false.
                    enddo
                    
                    if (new_next_move) then
                      num_next_moves(index) = num_next_moves(index) + 1
                      next_moves(index,num_next_moves(index)) = x3
                    endif
                  endif
                endif
              enddo

c if it is unique add it to the list of boards
              if (unique) then
                num_boards = num_boards + 1
                do x3=1,9
                  boards(num_boards,x3) = test_board(x3)
                enddo
                num_moves(num_boards) = x1
                num_next_moves(index) = num_next_moves(index) + 1
                next_moves(index,num_next_moves(index)) = num_boards

c check to see if the board is an end state
                terminal_board(num_boards) = check_terminal(test_board,winner)
                if (terminal_board(num_boards)) num_terminal(x1) = num_terminal(x1) + 1
                if (winner.eq.1) x_win(num_boards) = .true.
                if (winner.eq.2) o_win(num_boards) = .true.

              endif
            endif
          enddo
        enddo

c set the new boundaries for the next group of boards
        index_old = index_new + 1
        index_new = num_boards
        num_boards_per_move(x1) = index_new-index_old+1
      enddo

c write out the number of unique boards per number of moves
      do x1=0,9
        write(*,*) x1, num_boards_per_move(x1), num_terminal(x1)
      enddo

c write out the move information and unique boards to a file
      open(unit=10,file='move_info.txt')
      open(unit=42,file='boards.txt')
      do x1=1,num_boards
        write(10,fmt='(10(I4,1X))') x1, num_moves(x1), num_next_moves(x1), (next_moves(x1,x2), x2=1,num_next_moves(x1))
        do x2=1,9
          test_board(x2) = boards(x1,x2)
        enddo
        write(42,fmt='(I4)') x1
        call print_board(test_board,42)
      enddo
      close(10)
      close(42)
      read(*,*)

c-------------------------------------------------------------------------------------------------c
c set up the neural network
      idum = -42
      do x1=1,765
        num_beads(x1) = 100*num_next_moves(x1)
        do x2=1,num_next_moves(x1)
          beads(x1,x2) = 100
        enddo
      enddo

c loop over the starting point of the neural network
c We only start at a point that has more than one option
      do start=765,1,-1
        write(*,*) start
        if (num_next_moves(start).ge.2) then

c we create this offset because we can start the neural network at any board
          if (((num_moves(start)/2)*2).eq.num_moves(start)) then
            offset = 0
          else
            offset = 1
          endif
        
c loop over a random number of trials
          do counter=1,1000000
c reset the random moves that are made
            do x1=1,9
              move(x1) = 0
              nn_board(x1) = -1
            enddo
            done = .false.
        
c the first neural network board is board 1
            n_moves = 0
            nn_board(n_moves) = start

            do while(.not.done)
              rand=random(idum)*num_beads(nn_board(n_moves))
c              write(*,*) rand

              do x1=1,num_next_moves(nn_board(n_moves))
                if ((rand.gt.0.d0).and.(rand.lt.beads(nn_board(n_moves),x1))) then
                  move(n_moves) = x1
                endif
                rand = rand - beads(nn_board(n_moves),x1)
              enddo

c now that a board has been chosen add it to the list
              nn_board(n_moves+1) = next_moves(nn_board(n_moves),move(n_moves))
              n_moves = n_moves + 1
          
c if we reach a terminal board then we check to see who won          
              if (terminal_board(nn_board(n_moves))) then
                done = .true.
c add to the X beads and remove the O beads
                if (x_win(nn_board(n_moves))) then
c                  write(*,*) 'X won!'
                  do x1=0,n_moves

c here is where we need the offset just in case we start the neural network with an O move
                    if (((((x1+offset)/2)*2).eq.(x1+offset)).and.(num_beads(nn_board(x1)).gt.0)) then
                      num_beads(nn_board(x1)) = num_beads(nn_board(x1)) + 1
                      beads(nn_board(x1),move(x1)) = beads(nn_board(x1),move(x1)) + 1

                    else if (((((x1+offset)/2)*2).ne.(x1+offset)).and.(num_beads(nn_board(x1)).gt.0)) then
                      num_beads(nn_board(x1)) = num_beads(nn_board(x1)) - 1
                      beads(nn_board(x1),move(x1)) = beads(nn_board(x1),move(x1)) - 1

c if any of boards runs out of beads then we have effectively found a new terminal board where X wins
                      if (num_beads(nn_board(x1)).eq.0) then
                        terminal_board(nn_board(x1)) = .true.
                        x_win(nn_board(x1)) = .true.
                      endif

C c if any of the options goes down to 1 then increase the total number of beads for each choice by 10 unless
C c the total number of beads exceeds 10000 in which case let the failing option fail
C                       if ((beads(nn_board(x1),move(x1)).eq.1).and.(num_beads(nn_board(x1)).lt.10000)) then
C                         num_beads(nn_board(x1)) = num_beads(nn_board(x1)) + 10*num_next_moves(nn_board(x1))
C                         do x2=1,num_next_moves(nn_board(x1))
C                           beads(nn_board(x1),x2) = beads(nn_board(x1),x2) + 10
C                         enddo
C                       endif

                    endif
                  enddo
              
c add to the O beads and remove the X beads
                else if (o_win(nn_board(n_moves))) then
c                  write(*,*) 'O won!'
                  do x1=0,n_moves

c again we need the offset just in case we start off with an O move              
                    if (((((x1+offset)/2)*2).eq.(x1+offset)).and.(num_beads(nn_board(x1)).gt.0)) then
                      num_beads(nn_board(x1)) = num_beads(nn_board(x1)) - 1
                      beads(nn_board(x1),move(x1)) = beads(nn_board(x1),move(x1)) - 1

c if any of boards runs out of beads then we have effectively found a new terminal board where O wins
                      if (num_beads(nn_board(x1)).eq.0) then
                        terminal_board(nn_board(x1)) = .true.
                        o_win(nn_board(x1)) = .true.
                      endif
                                        
C c if any of the options goes down to 1 then increase the total number of beads for each choice by 10 unless
C c the total number of beads exceeds 10000 in which case let the failing option fail
C                       if ((beads(nn_board(x1),move(x1)).eq.1).and.(num_beads(nn_board(x1)).lt.10000)) then
C                         num_beads(nn_board(x1)) = num_beads(nn_board(x1)) + 10*num_next_moves(nn_board(x1))
C                         do x2=1,num_next_moves(nn_board(x1))
C                           beads(nn_board(x1),x2) = beads(nn_board(x1),x2) + 10
C                         enddo
C                       endif

                    else if (((((x1+offset)/2)*2).ne.(x1+offset)).and.(num_beads(nn_board(x1)).gt.0)) then
                      num_beads(nn_board(x1)) = num_beads(nn_board(x1)) + 1
                      beads(nn_board(x1),move(x1)) = beads(nn_board(x1),move(x1)) + 1
                    endif
                  enddo
                else
c                  write(*,*) 'Noone won :('
                endif
              endif
            enddo
        
C             if (((counter/1000000)*1000000).eq.counter) then
C               do x1=1,num_boards
C                 write(*,fmt='(10(I8,1X))') x1, num_beads(x1), (beads(x1,x2), x2=1,num_next_moves(x1))
C               enddo
C               read(*,*)
C             endif
          enddo
        endif
      enddo

c write out the results to a file
      open(unit=42,file='results.txt')
      do x1=1,num_boards
        write(42,fmt='(10(I8,1X))') x1, num_next_moves(x1), num_beads(x1), (beads(x1,x2), x2=1,num_next_moves(x1))
      enddo
      close(42)
      
c convert the results to a verbal format 
      open(unit=42,file='results_verbose.txt')
      do x1=1,num_boards
        if (num_next_moves(x1).ge.1) then
          do x2=1,num_next_moves(x1)
            if (beads(x1,x2).lt.10) then
              if (((num_moves(x1)/2)*2).eq.num_moves(x1)) then
                words(x2) = 'O wins    '
              else
                words(x2) = 'X wins    '
              endif
            else if (beads(x1,x2).lt.10000) then
              words(x2) = 'tie       '
            else
              if (((num_moves(x1)/2)*2).eq.num_moves(x1)) then
                words(x2) = 'X wins   '
              else
                words(x2) = 'O wins   '
              endif
            endif
          enddo
          write(42,fmt='(2(I7,1X),3X,7(A10))') x1, num_moves(x1), (words(x2), x2=1,num_next_moves(x1))
        else
          if (x_win(x1)) then
            write(42,fmt='(2(I7,1X),3X,A10)') x1, num_moves(x1),'X wins    '
          else if (o_win(x1)) then
            write(42,fmt='(2(I7,1X),3X,A10)') x1, num_moves(x1),'O wins    '
          else
            write(42,fmt='(2(I7,1X),3X,A10)') x1, num_moves(x1),'tie       '
          endif
        endif
      enddo
      close(42)
         
      end
      
          

c-------------------------------------------------------------------------------------------------c
      function board_match(index,board)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine checks to see if two tic tac toe boards are the same up to rotational symmetry c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      
c input parameters
      integer index, board(9)
      logical board_match

c boards common block      
      integer boards(1000,9), num_moves(1000), next_moves(1000,9), num_next_moves(1000)
      common/boards/ boards, num_moves, next_moves, num_next_moves

c parameters used in this function only
      integer test(9), x1, x2, dummy
      
c initalize some parameters
      do x1=1,9
        test(x1) = board(x1)
      enddo

c loop over 4 rotations checking after each one to see if there is a match
      do x1=1,4
      
c check if the boards match exactly
        board_match = .true.
        do x2=1,9
          if (test(x2).ne.boards(index,x2)) board_match = .false.
        enddo
      
        if (board_match) return
        
        call rotate_board(test)
      enddo
      
c flip the board about the vertical axis
c 1 and 3
      dummy = test(1)
      test(1) = test(3)
      test(3) = dummy
c 4 and 6
      dummy = test(4)
      test(4) = test(6)
      test(6) = dummy
c 7 and 9
      dummy = test(7)
      test(7) = test(9)
      test(9) = dummy

c loop over 4 more rotations checking after each one to see if there is a match
      do x1=1,4
      
c check if the boards match exactly
        board_match = .true.
        do x2=1,9
          if (test(x2).ne.boards(index,x2)) board_match = .false.
        enddo
      
        if (board_match) return
        
        call rotate_board(test)
      enddo

c if it makes it this far then the board is not a match      
      board_match = .false.
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      subroutine rotate_board(test)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine will rotate a tic tac toe board by a quarter turn counter clockwise.           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      
c input parameters
      integer test(9)
      
c parameters used in this subroutine only
      integer dummy(9), x1

c set the dummy board      
      do x1=1,9
        dummy(x1) = test(x1)
      enddo

c rotate!      
      test(1) = dummy(3)
      test(2) = dummy(6)
      test(3) = dummy(9)
      test(4) = dummy(2)
      test(6) = dummy(8)
      test(7) = dummy(1)
      test(8) = dummy(4)
      test(9) = dummy(7)
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      subroutine print_board(board,fileunit)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine prints out a board just to see what it looks like.                             c
c  If the fileunit it zero it prints to the screen, else it prints to a file.                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      
c input parameters
      integer board(9), fileunit

c parameters used in this subroutine only
      character chars(9)
      integer x1

c Change the number board to characters      
      do x1=1,9
        if (board(x1).eq.0) chars(x1) = ' '
        if (board(x1).eq.1) chars(x1) = 'X'
        if (board(x1).eq.2) chars(x1) = 'O'
      enddo

c print out the board to the screen
      if (fileunit.eq.0) then
        write(*,*) ' ',chars(1),' | ',chars(2),' | ',chars(3)
        write(*,*) '-----------'
        write(*,*) ' ',chars(4),' | ',chars(5),' | ',chars(6)
        write(*,*) '-----------'
        write(*,*) ' ',chars(7),' | ',chars(8),' | ',chars(9)
        write(*,*)
c print the board to the user supplied file unit
      else
        write(fileunit,*) ' ',chars(1),' | ',chars(2),' | ',chars(3)
        write(fileunit,*) '-----------'
        write(fileunit,*) ' ',chars(4),' | ',chars(5),' | ',chars(6)
        write(fileunit,*) '-----------'
        write(fileunit,*) ' ',chars(7),' | ',chars(8),' | ',chars(9)
        write(fileunit,*)
      endif
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      function check_terminal(board, winner)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function checks to see if a tic tac toe board has three in a row.                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c input parameters
      integer board(9), winner
      logical check_terminal
      
c parameters used in this function only
      integer x1
      
c initialize the function
      check_terminal = .true.
      winner = 0
      do x1=1,9
        if (board(x1).eq.0) check_terminal = .false.
      enddo
      
c vertical boards
      if ((board(1).eq.board(4)).and.(board(1).eq.board(7)).and.(board(1).ne.0)) then
        check_terminal = .true.
        winner = board(1)
      endif
      if ((board(2).eq.board(5)).and.(board(2).eq.board(8)).and.(board(2).ne.0)) then
        check_terminal = .true.
        winner = board(2)
      endif
      if ((board(3).eq.board(6)).and.(board(3).eq.board(9)).and.(board(3).ne.0)) then
        check_terminal = .true.
        winner = board(3)
      endif

c horizontal boards
      if ((board(1).eq.board(2)).and.(board(1).eq.board(3)).and.(board(1).ne.0)) then
        check_terminal = .true.
        winner = board(1)
      endif
      if ((board(4).eq.board(5)).and.(board(4).eq.board(6)).and.(board(4).ne.0)) then
        check_terminal = .true.
        winner = board(4)
      endif
      if ((board(7).eq.board(8)).and.(board(7).eq.board(9)).and.(board(7).ne.0)) then
        check_terminal = .true.
        winner = board(7)
      endif

c diagonal boards
      if ((board(1).eq.board(5)).and.(board(1).eq.board(9)).and.(board(1).ne.0)) then
        check_terminal = .true.
        winner = board(1)
      endif
      if ((board(3).eq.board(5)).and.(board(3).eq.board(7)).and.(board(3).ne.0)) then
        check_terminal = .true.
        winner = board(3)
      endif
      
      return
      end