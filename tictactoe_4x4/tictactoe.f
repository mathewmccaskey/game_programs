c-------------------------------------------------------------------------------------------------c
      program tictactoe_4x4
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This program generates all the unique tic tac toe boards in order to create a neural network   c
c  that will find the optimum path without teaching the rules of tic tac toe to the computer.     c
c  It should be quite interesting.                                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c variables to generate the tic tac toe boards      
      integer num_boards, num_boards_per_move(0:16), num_terminal(0:16), test_board(16), index, index_old, index_new
      integer equivalent_boards(8,16)
      logical board_match, check_terminal, unique, new_next_move, terminal_board(2000000), eff_term(2000000)
      logical x_win(2000000), o_win(2000000)
      integer x1, x2, x3, x4, x5, winner
      integer*8 counter, start

c neural network variables
      integer beads(2000000,16), num_beads(2000000), move(16), nn_board(0:16), n_moves, idum, offset
      logical done
      double precision rand, random
      character*9 verbose_words(16), result_word, move_words(16)

c boards common block      
      integer boards(2000000,16), num_moves(2000000), next_moves(2000000,16)
      common/boards/ boards, num_moves, next_moves

c output variable
      character*2 num_string
      
c initialize some variables
      do x1=1,2000000
        do x2=1,16
          boards(x1,x2) = 0
          next_moves(x1,x2) = 0
        enddo
        num_moves(x1) = 0
        terminal_board(x1) = .false.
        x_win(x1) = .false.
        o_win(x1) = .false.
      enddo
      do x1=0,16
        num_terminal(x1) = 0
      enddo

c initialize the number of boards per move with 0 moves (i.e. 1 board which is blank)
      num_boards = 1
      index_old = 1
      index_new = 1
      num_boards_per_move(0) = index_new-index_old+1

c setup output to both the screen and to a file
      open(unit=42,file='results/num_boards_per_move.txt')
      write(42,*) 'Number of unique boards per number of moves'
      write(42,fmt='(4(I10,1X))') 0, num_boards_per_move(0), num_terminal(0), num_boards      
      write(*,*) 'Number of unique boards per number of moves'
      write(*,fmt='(4(I10,1X))') 0, num_boards_per_move(0), num_terminal(0), num_boards
      
c loop over the number of moves
      do x1=1,17
      
c set up the individual output files for each move number
        if (x1.le.10) then
          write(num_string,fmt='(I1)') x1-1
          open(unit=10,file='results/boards_'//trim(num_string)//'.txt')
          open(unit=11,file='results/move_info_'//trim(num_string)//'.txt')
        else
          write(num_string,fmt='(I2)') x1-1
          open(unit=10,file='results/boards_'//num_string//'.txt')
          open(unit=11,file='results/move_info_'//num_string//'.txt')
        endif
        
c loop over the number of boards with x1-1 moves
        do index=index_old, index_new
c loop over the potential empty spaces on the board
          do x2=1,16
c make sure that the board isn't a terminal board
            if (.not.terminal_board(index)) then
c if there is an X or an O in the chosen spot then fill in the next move variable with a place holder
              if (boards(index,x2).eq.1) then
                next_moves(index,x2) = -1
              else if (boards(index,x2).eq.2) then
                next_moves(index,x2) = -2
              else if (boards(index,x2).eq.0) then
                do x3=1,16
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
                call get_equivalent_boards(test_board,equivalent_boards)
                
                x3 = index_new+1
                do while(unique.and.(x3.le.num_boards))
                  x4 = 1
                  do while(unique.and.(x4.le.8))
                    board_match = .true.
                    x5 = 1
                    do while(board_match.and.(x5.le.16))
                      if (boards(x3,x5).ne.equivalent_boards(x4,x5)) board_match = .false.
                      x5 = x5 + 1
                    enddo

c The first non-unique board that matches is added to the list of next moves                    
                    if (board_match) then
                      unique = .false.
                      next_moves(index,x2) = x3
                    endif
                    x4 = x4 + 1
                  enddo
                  x3 = x3 + 1
                enddo

c if it is unique add it to the list of boards
                if (unique) then
                  num_boards = num_boards + 1
                  do x3=1,16
                    boards(num_boards,x3) = test_board(x3)
                  enddo
                  num_moves(num_boards) = x1
                  next_moves(index,x2) = num_boards
                  if (((num_boards/1000)*1000).eq.num_boards) then
                    write(*,*) num_boards
                  endif

c check to see if the board is an end state
                  terminal_board(num_boards) = check_terminal(test_board,winner)
                  eff_term(num_boards) = terminal_board(num_boards)
                  if (terminal_board(num_boards)) num_terminal(x1) = num_terminal(x1) + 1
                  if (winner.eq.1) x_win(num_boards) = .true.
                  if (winner.eq.2) o_win(num_boards) = .true.
                endif
              endif

c if it is a terminal board then we can still adjust the next moves appropriately
            else
              if (boards(index,x2).eq.1) then
                next_moves(index,x2) = -1
              else if (boards(index,x2).eq.2) then
                next_moves(index,x2) = -2
              else if (boards(index,x2).eq.0) then
                next_moves(index,x2) = -3
              endif
            endif
          enddo

c write the boards and move information to their respective files
          do x2=1,16
            test_board(x2) = boards(index,x2)
          enddo
          write(10,fmt='(I10)') index
          call print_board(test_board,10)
          write(11,fmt='(18(I10,1X))') index, num_moves(index), (next_moves(index,x2), x2=1,16)
        enddo

c set the new boundaries for the next group of boards
        index_old = index_new + 1
        index_new = num_boards
        num_boards_per_move(x1) = index_new-index_old+1

c write stuff to the output files
        write(*,fmt='(4(I10,1X))') x1, num_boards_per_move(x1), num_terminal(x1), num_boards
        write(42,fmt='(4(I10,1X))') x1, num_boards_per_move(x1), num_terminal(x1), num_boards
        close(10)
        close(11)
      enddo

c write out the number of unique boards per number of moves
      write(*,*) 'Number of unique boards per number of moves'
      do x1=0,16
        write(*,*) x1, num_boards_per_move(x1), num_terminal(x1)
      enddo

c-------------------------------------------------------------------------------------------------c
c set up the neural network
      idum = -42
      do x1=1,num_boards
        num_beads(x1) = 0
        do x2=1,16
          if (next_moves(x1,x2).gt.0) then
            beads(x1,x2) = 10
          else
            beads(x1,x2) = 0
          endif
          num_beads(x1) = num_beads(x1) + beads(x1,x2)
        enddo
      enddo

c loop over the starting point of the neural network
c We only start at a point that has more than one option
      do start=num_boards,1,-1

c write out some output so that we know how much progress has gone
        if (((start/10000)*10000).eq.start) then
          write(*,*) start
        endif
        
        if (num_beads(start).gt.0) then
        
c we create this offset because we can start the neural network at any board
          if (((num_moves(start)/2)*2).eq.num_moves(start)) then
            offset = 0
          else
            offset = 1
          endif
        
c loop over a random number of trials
          do counter=1,1000
c reset the random moves that are made
            do x1=1,16
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

              do x1=1,16
                if ((rand.gt.0.d0).and.(rand.lt.beads(nn_board(n_moves),x1))) then
                  move(n_moves) = x1
                endif
                rand = rand - beads(nn_board(n_moves),x1)
              enddo

c now that a board has been chosen add it to the list
              nn_board(n_moves+1) = next_moves(nn_board(n_moves),move(n_moves))
              n_moves = n_moves + 1
          
c if we reach a terminal board then we check to see who won          
              if (terminal_board(nn_board(n_moves)).or.eff_term(nn_board(n_moves))) then
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
                        eff_term(nn_board(x1)) = .true.
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
                        eff_term(nn_board(x1)) = .true.
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

c write out the bead results to the output files
      do x1=0,16
        if (x1.lt.10) then
          write(num_string,fmt='(I1)') x1
          open(unit=x1,file='results/beads_'//trim(num_string)//'.txt')
        else
          write(num_string,fmt='(I2)') x1
          open(unit=x1,file='results/beads_'//num_string//'.txt')
        endif
      enddo

      do x1=1,num_boards
        write(num_moves(x1),fmt='(18(I10,1X))') x1, num_beads(x1), (beads(x1,x2), x2=1,16)
      enddo
      
      do x1=0,16
        close(x1)
      enddo
      
c open up the files for the verbose results
      do x1=0,16
        if (x1.lt.10) then
          write(num_string,fmt='(I1)') x1
          open(unit=x1,file='results/verbose_'//trim(num_string)//'.txt')
        else
          write(num_string,fmt='(I2)') x1
          open(unit=x1,file='results/verbose_'//num_string//'.txt')
        endif
        write(x1,fmt='(2X,A5,6X,A4,17X,A11,38X,A8,24X,A6)') 'board','move','next boards','strategy','result'
      enddo
      
c convert the results to a verbal format
      do x1=1,num_boards
        do x2=1,16
          if (next_moves(x1,x2).gt.0) then

c go through the number of beads and set up the verbose results          
            if (beads(x1,x2).lt.5) then
              if (((num_moves(x1)/2)*2).eq.num_moves(x1)) then
                if (.not.terminal_board(next_moves(x1,x2))) then
                  verbose_words(x2) = '  adv. O '
                else
                  verbose_words(x2) = '  O wins '
                endif
              else
                if (.not.terminal_board(next_moves(x1,x2))) then
                  verbose_words(x2) = '  adv. X '
                else
                  verbose_words(x2) = '  X wins '
                endif
              endif
            else if (beads(x1,x2).lt.100) then
              verbose_words(x2) = '   tie   '
            else
              if (((num_moves(x1)/2)*2).eq.num_moves(x1)) then
                if (.not.terminal_board(next_moves(x1,x2))) then
                  verbose_words(x2) = '  adv. X '
                else
                  verbose_words(x2) = '  X wins '
                endif
              else
                if (.not.terminal_board(next_moves(x1,x2))) then
                  verbose_words(x2) = '  adv. O '
                else
                  verbose_words(x2) = '  O wins '
                endif
              endif
            endif

c set up the next move strings
            write(move_words(x2),fmt='(I7)') next_moves(x1,x2)
            move_words(x2) = ' '//move_words(x2)//' '

c strings for moves that have already been made for a particular board          
          else if (next_moves(x1,x2).eq.-1) then
            verbose_words(x2) = '    X    '
            move_words(x2) = '    X    '
          else if (next_moves(x1,x2).eq.-2) then
            verbose_words(x2) = '    O    '
            move_words(x2) = '    O    '
          else if (next_moves(x1,x2).eq.-3) then
            verbose_words(x2) = '         '
            move_words(x2) = '         '
          endif

c setting up the result string for the verbose results
          if (terminal_board(x1).and.x_win(x1)) then
            result_word = '  X wins '
          else if (terminal_board(x1).and.o_win(x1)) then
            result_word = '  O wins '
          else if (terminal_board(x1)) then
            result_word = '   tie   '
          else if (((num_moves(x1)/2)*2).eq.num_moves(x1)) then
            result_word = '  X move '
          else
            result_word = '  O move '
          endif
        enddo
        
c write the verbose 
        write(num_moves(x1),fmt='(I7,7X,I2,4X,3(A9,A1),A9,10X,3(A9,A1),A9,5X,A9)') x1, num_moves(x1), 
     .    move_words(1), '|', move_words(2), '|', move_words(3), '|', move_words(4),
     .    verbose_words(1), '|', verbose_words(2), '|', verbose_words(3), '|', verbose_words(4),
     .    result_word
        write(num_moves(x1),fmt='(20X,A39,10X,A39)') '---------------------------------------',
     .    '---------------------------------------'
        write(num_moves(x1),fmt='(20X,3(A9,A1),A9,10X,3(A9,A1),A9)') move_words(5), '|', move_words(6), '|', 
     .    move_words(7), '|', move_words(8), 
     .    verbose_words(5), '|', verbose_words(6), '|', verbose_words(7), '|', verbose_words(8)
        write(num_moves(x1),fmt='(20X,A39,10X,A39)') '---------------------------------------',
     .    '---------------------------------------'
        write(num_moves(x1),fmt='(20X,3(A9,A1),A9,10X,3(A9,A1),A9)') move_words(9), '|', move_words(10), '|', 
     .    move_words(11), '|', move_words(12),
     .    verbose_words(9), '|', verbose_words(10), '|', verbose_words(11), '|', verbose_words(12)
        write(num_moves(x1),fmt='(20X,A39,10X,A39)') '---------------------------------------',
     .    '---------------------------------------'
        write(num_moves(x1),fmt='(20X,3(A9,A1),A9,10X,3(A9,A1),A9)') move_words(13), '|', move_words(14), '|', 
     .    move_words(15), '|', move_words(16),
     .    verbose_words(13), '|', verbose_words(14), '|', verbose_words(15), '|', verbose_words(16)
        write(num_moves(x1),*)

      enddo
      
c close the files
      do x1=0,16
        close(x1)
      enddo
         
      end
      
          

c-------------------------------------------------------------------------------------------------c
      subroutine get_equivalent_boards(board,equiv)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine checks to see if two tic tac toe boards are the same up to rotational symmetry c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      
c input parameters
      integer board(16), equiv(8,16)

c parameters used in this function only
      integer test(16), x1, x2, dummy, index
      
c initalize some parameters
      index = 1
      do x1=1,16
        test(x1) = board(x1)
      enddo

c loop over 4 rotations checking after each one to see if there is a match
      do x1=1,4      
        do x2=1,16
          equiv(index,x2) = test(x2)
        enddo
        index = index + 1
        call rotate_board(test)
      enddo
      
c flip the board about the vertical axis
c 1 and 4
        dummy = test(1)
        test(1) = test(4)
        test(4) = dummy
c 2 and 3
        dummy = test(2)
        test(2) = test(3)
        test(3) = dummy
c 5 and 8
        dummy = test(5)
        test(5) = test(8)
        test(8) = dummy
c 6 and 7
        dummy = test(6)
        test(6) = test(7)
        test(7) = dummy
c 9 and 12
        dummy = test(9)
        test(9) = test(12)
        test(12) = dummy
c 10 and 11
        dummy = test(10)
        test(10) = test(11)
        test(11) = dummy
c 13 and 16
        dummy = test(13)
        test(13) = test(16)
        test(16) = dummy
c 14 and 15
        dummy = test(14)
        test(14) = test(15)
        test(15) = dummy

c loop over 4 more rotations checking after each one to see if there is a match
      do x1=1,4
        do x2=1,16
          equiv(index,x2) = test(x2)
        enddo
        index = index + 1
        call rotate_board(test)
      enddo

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
      integer test(16)
      
c parameters used in this subroutine only
      integer dummy(16), x1

c set the dummy board      
      do x1=1,16
        dummy(x1) = test(x1)
      enddo

c rotate!      
      test(1) = dummy(4)
      test(2) = dummy(8)
      test(3) = dummy(12)
      test(4) = dummy(16)
      test(5) = dummy(3)
      test(6) = dummy(7)
      test(7) = dummy(11)
      test(8) = dummy(15)
      test(9) = dummy(2)
      test(10) = dummy(6)
      test(11) = dummy(10)
      test(12) = dummy(14)
      test(13) = dummy(1)
      test(14) = dummy(5)
      test(15) = dummy(9)
      test(16) = dummy(13)  
      
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
      integer board(16), fileunit

c parameters used in this subroutine only
      character chars(16)
      integer x1

c Change the number board to characters      
      do x1=1,16
        if (board(x1).eq.0) chars(x1) = ' '
        if (board(x1).eq.1) chars(x1) = 'X'
        if (board(x1).eq.2) chars(x1) = 'O'
      enddo

c print out the board to the screen
      if (fileunit.eq.0) then
        write(*,*) ' ',chars(1),' | ',chars(2),' | ',chars(3),' | ',chars(4)
        write(*,*) '---------------'
        write(*,*) ' ',chars(5),' | ',chars(6),' | ',chars(7),' | ',chars(8)
        write(*,*) '---------------'
        write(*,*) ' ',chars(9),' | ',chars(10),' | ',chars(11),' | ',chars(12)
        write(*,*) '---------------'
        write(*,*) ' ',chars(13),' | ',chars(14),' | ',chars(15),' | ',chars(16)
        
        write(*,*)
c print the board to the user supplied file unit
      else
        write(fileunit,*) ' ',chars(1),' | ',chars(2),' | ',chars(3),' | ',chars(4)
        write(fileunit,*) '---------------'
        write(fileunit,*) ' ',chars(5),' | ',chars(6),' | ',chars(7),' | ',chars(8)
        write(fileunit,*) '---------------'
        write(fileunit,*) ' ',chars(9),' | ',chars(10),' | ',chars(11),' | ',chars(12)
        write(fileunit,*) '---------------'
        write(fileunit,*) ' ',chars(13),' | ',chars(14),' | ',chars(15),' | ',chars(16)
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
      integer board(16), winner
      logical check_terminal
      
c parameters used in this function only
      integer x1
      
c initialize the function
      check_terminal = .true.
      winner = 0
      do x1=1,16
        if (board(x1).eq.0) check_terminal = .false.
      enddo
      
c vertical boards
      if ((board(1).eq.board(5)).and.(board(1).eq.board(9)).and.(board(1).eq.board(13)).and.(board(1).ne.0)) then
        check_terminal = .true.
        winner = board(1)
      endif
      if ((board(2).eq.board(6)).and.(board(2).eq.board(10)).and.(board(2).eq.board(14)).and.(board(2).ne.0)) then
        check_terminal = .true.
        winner = board(2)
      endif
      if ((board(3).eq.board(7)).and.(board(3).eq.board(11)).and.(board(3).eq.board(15)).and.(board(3).ne.0)) then
        check_terminal = .true.
        winner = board(3)
      endif
      if ((board(4).eq.board(8)).and.(board(4).eq.board(12)).and.(board(4).eq.board(16)).and.(board(4).ne.0)) then
        check_terminal = .true.
        winner = board(4)
      endif
      
c horizontal boards
      if ((board(1).eq.board(2)).and.(board(1).eq.board(3)).and.(board(1).eq.board(4)).and.(board(1).ne.0)) then
        check_terminal = .true.
        winner = board(1)
      endif
      if ((board(5).eq.board(6)).and.(board(5).eq.board(7)).and.(board(5).eq.board(8)).and.(board(5).ne.0)) then
        check_terminal = .true.
        winner = board(5)
      endif
      if ((board(9).eq.board(10)).and.(board(9).eq.board(11)).and.(board(9).eq.board(12)).and.(board(9).ne.0)) then
        check_terminal = .true.
        winner = board(9)
      endif
      if ((board(13).eq.board(14)).and.(board(13).eq.board(15)).and.(board(13).eq.board(16)).and.(board(13).ne.0)) then
        check_terminal = .true.
        winner = board(13)
      endif
      
c diagonal boards
      if ((board(1).eq.board(6)).and.(board(1).eq.board(11)).and.(board(1).eq.board(16)).and.(board(1).ne.0)) then
        check_terminal = .true.
        winner = board(1)
      endif
      if ((board(4).eq.board(7)).and.(board(4).eq.board(10)).and.(board(4).eq.board(13)).and.(board(4).ne.0)) then
        check_terminal = .true.
        winner = board(4)
      endif
      
      return
      end