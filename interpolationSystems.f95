

program interpolationSystems

implicit none
integer :: opt

	PRINT*, "Welcome!"

	DO
    	PRINT*, "1 --> Power Series"
        PRINT*, "2 --> Lagrange Polynomials"
        PRINT*, "3 --> Newton Divided Differences"

        WRITE(*,*) "What method do you want to use?"
        READ(*,*)opt

        SELECT CASE(opt)
          CASE(1)	!Power series
          	CALL PowerSeries
            !PRINT*, "Power series works"

          CASE(2)	!Lagrange Polynomials
          	CALL Lagrange
            !PRINT*, "LAGRANGE works"

          CASE(3)	
          	CALL NewtonDD
            !PRINT*, "NDD works"

          CASE DEFAULT
          	PRINT*, "ERROR"

         END SELECT

	END DO
END PROGRAM interpolationSystems

	subroutine PowerSeries

	IMPLICIT NONE
	INTEGER :: x, y, row, column
	CHARACTER (len=50):: firstFileP, finalFileP

	real, dimension(:,:), allocatable:: A, C
	REAL, DIMENSION(:), ALLOCATABLE::B, I   
     
	PRINT*, "You chose: Power Series"

	PRINT*, "DISCLAIMER: Please insert the resulting data into the program named systemOfLinearEquation"

	Print*,"Insert the file name"
    READ(*,*)firstFileP

	open(10, file = firstFileP)
    read(10, *)column, row
	!------OPERATIONS-------!

	DO x=1, row
		A(x,1) = 1
    END DO

    DO x = 1, row
      DO y = 1, row
        A(x, y) = C(x,1)**(y-1)
      END DO
    END DO

    DO x = 1, row
      B(x)=C(x,column)
      I(x)=0
   	END DO

    

	!-------END FILE--------!
	WRITE(*,*)"Insert the file name where the results will be written"
   	READ(*,*)finalFileP
    
    open(15, file = finalFileP)

	WRITE(15, *) "The following system is the one that will be solved..."
    	DO x=1, row
        	WRITE(15, *)(A(x,y), y = 1, row), B(x)
        END DO

	end subroutine PowerSeries

SUBROUTINE Lagrange
	
	IMPLICIT NONE
    INTEGER :: columnL, rowL, cont, dec
    CHARACTER (len=50):: firstFileL, finalFileL
	REAL :: x(50), y(50), outcome, xOriginal, total = 0
    real, dimension(:,:), allocatable :: a

	PRINT*, "You chose: Lagrange Polynomials"

	Print*,"Please insert the file name from where the data will be obtained"
    READ(*,*)firstFileL
    
   	open(10, file = firstFileL)
    read(10, *)columnL, rowL

	allocate(a (columnL, rowL))

	read(10, *)xOriginal

    read(10, *)(x(cont), cont=1, columnL)
    read(10, *)(y(cont), cont=1, columnL)
    
	!------OPERATIONS--------!
    
	print*, "                 x                y" !5 espacios
 
    DO cont=1, columnL
    	PRINT*, cont, " ",x(cont), " ", y(cont)
    END DO
    
    DO cont = 1, columnL
      outcome = y(cont)

      	  DO dec = 1, columnL
        	IF(cont /= dec)THEN
          	outcome = outcome * (xOriginal - x(dec))/(x(cont) - x(dec))
    		END IF
    	  END DO
    
		total = total + outcome
	
	END DO

    !-------WRITE IN DOC-------!
 	
	Print*,""

	PRINT*,"The outcome for f(",xOriginal,") is = ",total
	

 	WRITE(*,*)"Insert the file name where the results will be written"
    READ(*,*)finalFileL   
    
    open(10, file = finalFileL)
	
	WRITE(10, *)"The outcome for f(", xOriginal,") is= ", total

    

END SUBROUTINE Lagrange

SUBROUTINE NewtonDD
	
	IMPLICIT NONE
    INTEGER :: column, row, cont, cont2, num, d
    CHARACTER (len=50):: firstFileN, finalFileN
	REAL :: x(50), y(50), xOriginal, r(50), aux(50), res, ra(50)
    real, dimension(:,:), allocatable :: a

	PRINT*, "You chose: Newton Divided Differences"

	Print*,"Insert the file name"
    READ(*,*)firstFileN
  
    open(10, file = firstFileN)
    read(10, *)column, row

	allocate(a (column, row))

	read(10, *)xOriginal

    read(10, *)(x(cont), cont=1, column)
    read(10, *)(y(cont), cont=1, column)
    
	!------OPERATIONS--------!
    
	print*, "                    x                y"
 
    DO cont=1, column
    	PRINT*,cont, " ", x(cont), " ", y(cont)
    END DO
   
	aux(1:column) = y(1:column)
    num = column - 1
    d = 1

	PRINT *, "---------------RESULTS---------------"

	DO cont = 1, 3
    	DO cont2 = 1, num
      		ra(cont2) = (aux(cont2+1)-aux(cont2))/(x(cont2+d)- x(cont2)) 
    		PRINT *, " ", ra(cont2)
    	END DO
        aux(1:num) = ra(1:num)
        r(cont) = ra(1)
        num = num - 1
        d = d + 1
    END DO
       
    res = y(1) + ((xOriginal - x(1))*r(1)) + ((xOriginal - x(2))*(xOriginal - x(1))*r(2)) &
   			   + ((xOriginal - x(2))*(xOriginal - x(1))*(xOriginal - x(3))*r(3))
    PRINT *, "Analitical value: f(", xOriginal, ") = ", res
	
    !-------WRITE IN DOC-------!
    
    WRITE(*,*)"Insert the file name where the results will be written"	
    READ(*,*)finalFileN   
	WRITE(15,*) "Analitical value: f(", xOriginal, ") = ", res
    

END SUBROUTINE NewtonDD