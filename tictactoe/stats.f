c-------------------------------------------------------------------------------------------------c
      program stats
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  After running the tic tac toe program I wanted to write a quick program that did some          c
c  statistical analysis on some of the results.... you know... for funsies.                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c parameters used in this progra, only
      integer*8 num_boards_3x3(0:9), num_boards_4x4(0:16), total, n, combo
      double precision sum_sq3, sum_sq4, ax, bx, cx, p_min, tol

c counters
      integer*8 x1

c probability data
      double precision probs_3x3(0:9), probs_4x4(0:16)
      common/data/ probs_3x3, probs_4x4

      external sum_sq3, sum_sq4
      
c initialize data
      num_boards_3x3(0) = 1
      num_boards_3x3(1) = 3
      num_boards_3x3(2) = 12
      num_boards_3x3(3) = 38
      num_boards_3x3(4) = 108
      num_boards_3x3(5) = 174
      num_boards_3x3(6) = 204
      num_boards_3x3(7) = 153
      num_boards_3x3(8) = 57
      num_boards_3x3(9) = 15

c tally up the boards      
      total = 0
      do x1=0,9
        total = total + num_boards_3x3(x1)
      enddo

c calculate the porbabilities to fit
      do x1=0,9
        probs_3x3(x1) = dble(num_boards_3x3(x1))/dble(total)
      enddo
      
      n = 9
      
c minimize the function of least squares
      ax = 0.01d0
      bx = 0.5d0
      cx = 0.99d0
      tol = 1.0d-10
      
      call brent(ax,bx,cx,sum_sq3,tol,p_min)

c write out the results
      write(*,*) 'p = ',p_min
      open(unit=42,file='fit_3x3.txt')
      do x1=0,9
        write(42,fmt='(I2,1X,2(SE16.8,1X))') x1, probs_3x3(x1), (combo(n,x1)*p_min**x1*(1.d0-p_min)**(n-x1))
      enddo
      close(42)

c NOW DO THE SAME FOR THE 4x4 BOARDS        
c initialize data
      num_boards_4x4(0) = 1
      num_boards_4x4(1) = 3
      num_boards_4x4(2) = 33
      num_boards_4x4(3) = 219
      num_boards_4x4(4) = 1413
      num_boards_4x4(5) = 5514
      num_boards_4x4(6) = 20122
      num_boards_4x4(7) = 50215
      num_boards_4x4(8) = 112379
      num_boards_4x4(9) = 179510
      num_boards_4x4(10) = 245690
      num_boards_4x4(11) = 245690
      num_boards_4x4(12) = 193318
      num_boards_4x4(13) = 110452
      num_boards_4x4(14) = 41870
      num_boards_4x4(15) = 10489
      num_boards_4x4(16) = 1059
      
c tally up the boards      
      total = 0
      do x1=0,16
        total = total + num_boards_4x4(x1)
      enddo

c calculate the porbabilities to fit
      do x1=0,16
        probs_4x4(x1) = dble(num_boards_4x4(x1))/dble(total)
      enddo
      
      n = 16
    
c minimize the function of least squares
      ax = 0.01d0
      bx = 0.5d0
      cx = 0.99d0
      tol = 1.0d-15
      
      call brent(ax,bx,cx,sum_sq4,tol,p_min)

c write out the results
      write(*,*) 'p = ',p_min
      open(unit=42,file='fit_4x4.txt')
      do x1=0,16
        write(42,fmt='(I2,1X,2(SE16.8,1X))') x1, probs_4x4(x1), (combo(n,x1)*p_min**x1*(1.d0-p_min)**(n-x1))
      enddo
      close(42)

      end



c-------------------------------------------------------------------------------------------------c
      function sum_sq3(p)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the sum of squares for us to minimize
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c input parameters and function definition
      double precision sum_sq3, p
      
c parameters used in this function only
      integer*8 n, x1
      double precision combo

c probability data
      double precision probs_3x3(0:9), probs_4x4(0:16)
      common/data/ probs_3x3, probs_4x4
      
      n = 9
      sum_sq3 = 0.d0
      do x1=0,9
        sum_sq3 = sum_sq3 + (combo(n,x1)*(p**x1)*((1.d0-p)**(n-x1))-probs_3x3(x1))**2
      enddo
      
      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      function sum_sq4(p)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the sum of squares for us to minimize
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c input parameters and function definition
      double precision sum_sq4, p
      
c parameters used in this function only
      integer*8 n, x1
      double precision combo

c probability data
      double precision probs_3x3(0:9), probs_4x4(0:16)
      common/data/ probs_3x3, probs_4x4
      
      n = 16
      sum_sq4 = 0.d0
      do x1=0,16
        sum_sq4 = sum_sq4 + (combo(n,x1)*(p**x1)*((1.d0-p)**(n-x1))-probs_4x4(x1))**2
      enddo
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      function combo(n,r)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the combination of n things taken r at a time                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c input parameters
      integer*8 combo, n, r

c parameters used in this function only
      logical used(1000)
      integer*8 denom_index, numer_index

c counters
      integer*8 x1, x2
      
c initialize some parameters
      do x1=1,1000
        used(x1) = .false.
      enddo

c check to see what kind of index we're looking for in our denominator
      if (r.le.(n/2)) then
        denom_index = r
        numer_index = n-r+1
      else
        denom_index = n-r
        numer_index = r+1
      endif

      combo = 1
      do x1=n,numer_index,-1
        combo = combo*x1
        do x2=1,denom_index
          if ((.not.used(x2)).and.(((combo/x2)*x2).eq.combo)) then
            combo = combo/x2
            used(x2) = .true.
          endif
        enddo
      enddo

      return
      end