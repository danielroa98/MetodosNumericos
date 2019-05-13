subroutine logarithmicRegression
  IMPLICIT NONE
  INTEGER :: nR, opt, count, nX, nY
  CHARACTER (len = 150) :: insFile, endFile
  REAL :: x(150), y(150), rCount(150), addLogX, addLogY, addLogX2, addLogXLogY, xMed, yMed, la0, a0, a1, sT, sR, r, r2


  PRINT *, "Please enter the name of the file frome where the data will be obtained"
  PRINT *, "Keep in mind the name you enter must contain the type of document '.txt' at the end of the chosen name"

  PRINT *, " "

  read (*,*)insFile

  OPEN(10, file = insFile) !---Document from which data will be extracted from
  READ(10, *)nR

  READ(10, *)(x(count), count = 1, nR)
  READ(10, *)(y(count), count = 1, nR)
  CLOSE(10)

  print *, "Please enter the name of the file where the results will be printed"
  PRINT *, "Do remember to add the identifier '.txt' at the end of the document's name"

  PRINT *, " "

  read (*,*)endFile

  OPEN(10, file = endFile) !---Document where results will be printed upon

  !-----------OPERATIONS-----------!

  do count = 1, nR

    addLogX = addLogX + log10(x(count))
    addLogX2 = addLogX2 + (log10(x(count))**2)
    addLogY = addLogY + log10(y(count))
    addLogXLogY = addLogXLogY + log10(x(count)) * log10(y(count))

  end do

  xMed = addLogX/nR
  yMed = addLogY/nR

  a1 = (nR*addLogXLogY-addLogX*addLogY)/(nR*(addLogX2**2))
  la0 = yMed - a1*xMed

  do count = 1, nR

    rCount(count) = la0 + a1*log10(x(count))

  END DO

  do count = 1, nR

    sT = sT + (log10(y(count)) - yMed)**2
    sR = sR + (log10(y(count)) - rCount(count))**2

  end do

  a0 = 10**la0

  r2 = (sT - sR)/sT
  r =  SQRT(r2)

  PRINT *, "The results from the equation are: "
  PRINT *, "log(a0) -> ", la0
  PRINT *, "a0 -> ", a0
  PRINT *, "a1 -> ", a1
  PRINT *, "Sr -> ", sR
  PRINT *, "St -> ", sT
  PRINT *, "R -> ", r
  PRINT *, "R^2 -> ", r2

  Do WHILE(opt == 0)
    PRINT *, "If you would like to calculate regression on another point press 0, if not press any other numerical key"

      READ *, opt

      if (opt == 0) then

        PRINT *, "Please enter the new value"
        READ *, nX

          nY = a0 * a1*nX

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

end subroutine 
