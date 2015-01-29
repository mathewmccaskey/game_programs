c-------------------------------------------------------------------------------------------------c
      program piglet
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This is a program to analyze the a coin flipping game Piglet.  This is a simpler game than Pig c
c  and will be used as an instructional tool for the algorithm that solves Pig.                   c
c                                                                                                 c
c  The game goes as follows:                                                                      c
c                                                                                                 c
c  Each turn, a player filps a coin until either tails is flipper or the player decides to "hold" c                                                                                       c
c                                                                                                 c
c  - If the player flips tails, they score nothing and it becomes the next player's turn.         c
c  - If the player flips heads, 1 is added to their turn total is and the player's turn continues.c                                                                                  c
c  - If a player chooses to "hold", their turn total is added to their score, and it becomes the  c
c    next player's turn.                                                                          c
c                                                                                                 c
c  The first player to score 100 or more points wins.                                             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c parameters used in this program only
      double precision states(0:10,0:10,0:10), probs(0:10,0:10,0:10), diff, max_diff, P_roll, P_hold
      integer goal, total, s1, s2, min_s1, max_s1, roll_yes_int(0:10,0:10,0:10)
      logical roll_yes(0:10,0:10,0:10)
      character*2 num_str2
      character*1 num_str1

c counters
      integer x1, x2, x3

c goal to reach
      goal = 10
      
c initialize the results
      do x1=0,goal
        do x2=0,goal
          do x3=0,goal
            probs(x1,x2,x3) = 0.d0
            states(x1,x2,x3) = 0.d0
          enddo
        enddo
      enddo

c some of the states we know the probability to win is 1
c  (i.e. player 1 will have the correct number of points if they hold)
      do x1=0,goal
        do x2=0,goal-1
          do x3=goal-x1,goal
            probs(x1,x2,x3) = 1.d0
            roll_yes(x1,x2,x3) = .false.
            roll_yes_int(x1,x2,x3) = 0
          enddo
        enddo
      enddo  
          
c loop over the sum of the two players' scores
      do total=2*(goal-1),0,-1
        write(*,*) total

c loop over how how many points go to p1 (only need to go halfway because of symmetry)
        if (total.ge.goal) then
          min_s1 = total - (goal-1)
c if the total is less than the goal then player 1 can have 0 points without losing
        else 
          min_s1 = 0
        endif
        max_s1 = total/2
        
        do s1=min_s1, max_s1
          s2=total-s1
          
c if s1=s2 then we only have one set of states to calculate
          if (s1.eq.s2) then
          
            max_diff = 1.d0
            do while(max_diff.gt.1.0d-12)

c reset the max_diff
              max_diff = 0.d0

c loop over the turn totals that put the player just under the goal (else prob = 1)
              do x1=0,goal-(s1+1)
                P_roll = ((1.d0-probs(s2,s1,0))+probs(s1,s2,x1+1))/2.d0
                if (x1.eq.0) then
                  P_hold = 0.d0
                else
                  P_hold = 1.d0-probs(s2,s1+x1,0)
                endif
                states(s1,s2,x1) = max(P_roll,P_hold)

c check to see what is the optimal procedure
                if (P_roll.gt.P_hold) then
                  roll_yes(s1,s2,x1) = .true.
                  roll_yes_int(s1,s2,x1) = 1
                else
                  roll_yes(s1,s2,x1) = .false.
                  roll_yes_int(s1,s2,x1) = 0
                endif
                
c compare with the previous results
                diff = dabs(states(s1,s2,x1)-probs(s1,s2,x1))
                if (diff.gt.max_diff) max_diff = diff
              enddo
              
c transfer the new states to the old probabilities
              do x1=0,goal-(s1+1)
                probs(s1,s2,x1) = states(s1,s2,x1)
              enddo              
            enddo
                
c if s1!=s2 then we have two sets of states to calculate
          else

            max_diff = 1.d0
            do while(max_diff.gt.1.0d-12)

c reset the max_diff
              max_diff = 0.d0
              
c loop over the turn totals that put player 1 just under the goal
              do x1=0,goal-(s1+1)
                P_roll = ((1.d0-probs(s2,s1,0))+probs(s1,s2,x1+1))/2.d0
                if (x1.eq.0) then
                  P_hold = 0.d0
                else
                  P_hold = 1.d0-probs(s2,s1+x1,0)
                endif
                states(s1,s2,x1) = max(P_roll,P_hold)

c check to see what is the optimal procedure
                if (P_roll.gt.P_hold) then
                  roll_yes(s1,s2,x1) = .true.
                  roll_yes_int(s1,s2,x1) = 1
                else
                  roll_yes(s1,s2,x1) = .false.
                  roll_yes_int(s1,s2,x1) = 0
                endif
                
c compare with the previous results
                diff = dabs(states(s1,s2,x1)-probs(s1,s2,x1))
                if (diff.gt.max_diff) max_diff = diff                
              enddo
              
c loop over the turn totals that put player 2 just under the goal
              do x1=0,goal-(s2+1)
                P_roll = ((1.d0-probs(s1,s2,0))+probs(s2,s1,x1+1))/2.d0
                if (x1.eq.0) then
                  P_hold = 0.d0
                else
                  P_hold = 1.d0-probs(s1,s2+x1,0)
                endif
                states(s2,s1,x1) = max(P_roll,P_hold)

c check to see what is the optimal procedure
                if (P_roll.gt.P_hold) then
                  roll_yes(s2,s1,x1) = .true.
                  roll_yes_int(s2,s1,x1) = 1
                else
                  roll_yes(s2,s1,x1) = .false.
                  roll_yes_int(s2,s1,x1) = 0
                endif

c compare with the previous results
                diff = dabs(states(s2,s1,x1)-probs(s2,s1,x1))
                if (diff.gt.max_diff) max_diff = diff
              enddo              

c transfer the new results to the old probabilities
              do x1=0,goal-(s1+1)
                probs(s1,s2,x1) = states(s1,s2,x1)
              enddo
              do x1=0,goal-(s2+1)
                probs(s2,s1,x1) = states(s2,s1,x1)
              enddo
            enddo
          endif
        enddo
      enddo
      
C       write(*,*) 'Here are some probabilities'
C       do x1=0,goal-1
C         do x2=0,goal-1
C           do x3=0,goal-(x1+1)
C             write(*,fmt='(A2,2(I1,A1),I1,A4,SE16.8,5X,L1)') 'P(',x1,',',x2,',',x3,') = ',
C      .            probs(x1,x2,x3),roll_yes(x1,x2,x3)
C           enddo
C         enddo
C       enddo

c print out the probabilities to some files
      do s2=0,10
        if (s2.eq.10) then
          write(num_str2,fmt='(I2)') s2
          open(unit=10,file='piglet_results/probs_'//num_str2//'.txt')
          open(unit=11,file='piglet_results/flip_'//num_str2//'.txt')
        else
          write(num_str1,fmt='(I1)') s2
          open(unit=10,file='piglet_results/probs_0'//num_str1//'.txt')
          open(unit=11,file='piglet_results/flip_0'//num_str1//'.txt')
        endif
        
c write to the files
        do x1=10,0,-1
          write(10,fmt='(11(SE16.8,2X))') (probs(s1,s2,x1), s1=0,10)
          write(11,fmt='(11(I1,1X))') (roll_yes_int(s1,s2,x1), s1=0,10)
        enddo
        
        close(10)
        close(11)
      enddo
      
      end