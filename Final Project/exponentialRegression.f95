program exponentialRegression
  implicit none
  INTEGER :: nR, opt, count, nX, nY
  CHARACTER (len=150):: insFile, endFile
  REAL:: x(150), y(150), addX, addY, addX2, addXY, xMed, yMed, la0, a0, a1, sT, sR, r, r2

  PRINT *, "Please enter the name of the file frome where the data will be obtained"
  PRINT *, "Keep in mind the name you enter must contain the type of document '.txt' at the end of the chosen name"

  PRINT *, " "

  read (*,*)insFile

  OPEN(10, file = insFile)
  READ(10, *)nR

  READ(10, *)(x(count), count = 1, nR)
  READ(10, *)(y(count), count = 1, nR)
  CLOSE(10)

  !---CLOSE INSERTED PROGRAM NAME---!

  print *, "Please enter the name of the file where the results will be printed"
  PRINT *, "Do remember to add the identifier '.txt' at the end of the document's name"

  PRINT *, " "
  read (*,*)endFile

  OPEN(10, file = endFile)

  !-----------OPERATIONS-----------!

  do count = 1, nR

    addX = addX + x(count)
    addY = addY + log(y(count))
    addX2 = addX2 + x(count)**2
    addXY = addXY + x(count)*log(y(count))

  end do

  xMed = addX/nR
  yMed = addY/nR
  a1 = (nR*addXY-addX*addY) / (nR*addX2-(addX**2))
  la0 = yMed - a1*xMed

  do count = 1, nR

    sT = sT + (log(y(count)) - yMed)**2
    sR = sR + (log(y(count)) - (la0+a1*x(count)))**2

  end do

  r2 = (sT - sR)/sT
  r =  SQRT(r2)

  a0 = EXP(la0)

  PRINT *, "The results from the equation are: "
  WRITE (*,*) "ln(a0) -> ", la0
  WRITE (*,*) "a0 -> ", a0
  WRITE (*,*) "a1 -> ", a1
  WRITE (*,*) "Sr -> ", sR
  WRITE (*,*) "St -> ", sT
  WRITE (*,*) "R -> ", r
  WRITE (*,*) "R^2 -> ", r2

  !---END ORIGINAL OPERATIONS---!

  !---USER CHOSEN X VALUE---!

  Do WHILE(opt == 0)
    PRINT *, "If you would like to calculate regression on another point press 0, if not press any other numerical key"

      READ *, opt

      if ( opt == 0 ) then

        PRINT *, "Please enter the new value"
        READ *, nX

          nY = a0 * exp(a1*nX)

          WRITE(*,*) "The new value of x = ", nX," results in ", nY

          WRITE (10, *)"The new value of x = ", nX," results in ", nY

      end if

    END DO
  !-----INSERTION OF RESULTS-----!


  WRITE (10, *) "ln(a0) -> ", la0
  WRITE (10, *) "a0 -> ", a0
  WRITE (10, *) "a1 -> ", a1
  WRITE (10, *) "Sr -> ", sR
  WRITE (10, *) "St -> ", sT
  WRITE (10, *) "R -> ", r
  WRITE (10, *) "R^2 -> ", r2

  CLOSE(10)


end program
