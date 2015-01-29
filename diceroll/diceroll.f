c-------------------------------------------------------------------------------------------------c
      program diecroll
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  PLaying around with some more dice rolling statistics                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c parameters for this program only
      integer*8 dice1_1(6), dice1_2(6), dice2_2(2:12), dice2_3(2:12), dice3_3(3:18), dice3_4(3:18), index
      double precision prob1_1(6), prob1_2(6), prob2_2(2:12), prob2_3(2:12), prob3_3(3:18), prob3_4(3:18)
      integer*8 ave1_int, ave2_int, ave3_int
      double precision ave1, ave2, ave3, sd1, sd2, sd3
      
      integer*8 x1, x2, x3, x4
      
c initialize parameters
      do x1=1,6
        dice1_1(x1) = 0
        dice1_2(x1) = 0
        prob1_1(x1) = 0.d0
        prob1_2(x1) = 0.d0
      enddo
      do x1=2,12
        dice2_2(x1) = 0
        dice2_3(x1) = 0
        prob2_2(x1) = 0.d0
        prob2_3(x1) = 0.d0
      enddo
      do x1=3,18
        dice3_3(x1) = 0
        dice3_4(x1) = 0
        prob3_3(x1) = 0.d0
        prob3_4(x1) = 0.d0
      enddo
      ave1 = 0.d0
      ave2 = 0.d0
      ave3 = 0.d0
      ave1_int = 0
      ave2_int = 0
      ave3_int = 0
      sd1 = 0.d0
      sd2 = 0.d0
      sd3 = 0.d0
      
c loops for the 2 dice case
      do x1=1,6
        dice1_1(x1) = dice1_1(x1) + 1
        do x2=1,6
          index = x1+x2
          dice2_2(index) = dice2_2(index) + 1
          index = x1+x2 - min(x1,x2)
          dice1_2(index) = dice1_2(index) + 1
          do x3=1,6
            index = x1+x2+x3
            dice3_3(index) = dice3_3(index) + 1
            index = x1+x2+x3 - min(min(x1,x2),x3)
            dice2_3(index) = dice2_3(index) + 1
            do x4=1,6
              index = x1+x2+x3+x4 - min(min(min(x1,x2),x3),x4)
              dice3_4(index) = dice3_4(index) + 1
            enddo
          enddo
        enddo
      enddo
      
      do x1=1,6
        prob1_1(x1) = dice1_1(x1)/6.d0
        prob1_2(x1) = dice1_2(x1)/36.d0
        write(*,fmt='(3(I5,1X),2(2X,SE16.10))') x1, dice1_1(x1), dice1_2(x1), prob1_1(x1), prob1_2(x1)        
      enddo
      write(*,*)
      
      do x1=2,12
        prob2_2(x1) = dice2_2(x1)/36.d0
        prob2_3(x1) = dice2_3(x1)/216.d0
        write(*,fmt='(3(I5,1X),2(2X,SE16.10))') x1, dice2_2(x1), dice2_3(x1), prob2_2(x1), prob2_3(x1)
      enddo
      write(*,*)
      
      do x1=3,18
        prob3_3(x1) = dice3_3(x1)/216.d0
        prob3_4(x1) = dice3_4(x1)/1296.d0
        write(*,fmt='(3(I5,1X),2(2X,SE16.10))') x1, dice3_3(x1), dice3_4(x1), prob3_3(x1), prob3_4(x1)
      enddo
        
c calulate averages and standard deviations
      do x1=1,6
        ave1_int = ave1_int + x1*dice1_2(x1)
      enddo
      ave1 = ave1_int/36.d0
      do x1=1,6
        sd1 = sd1 + (x1-ave1)**2
      enddo
      sd1 = sd1/36.d0
      write(*,*) ave1_int, ave1, sd1
      
      do x1=2,12
        ave2_int = ave2_int + x1*dice2_3(x1)
      enddo
      ave2 = ave2_int/216.d0
      do x1=2,12
        sd2 = sd2 + (x1-ave2)**2
      enddo
      sd2 = sd2/216.d0
      write(*,*) ave2_int, ave2, sd2
      
      do x1=3,18
        ave3_int = ave3_int + x1*dice3_4(x1)
      enddo
      ave3 = ave3_int/1296.d0
      do x1=3,18
        sd3 = sd3 + (x1-ave3)**2
      enddo
      sd3 = sd3/1296.d0
      write(*,*) ave3_int, ave3, sd3
      
      end      