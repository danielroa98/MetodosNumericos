program FinalMetodos
  implicit none

  write(*,*) "Final Project Métodos Numéricos"
  call Menu()

end program

subroutine Menu

  integer:: Case

  write(*,*)"                                                   "
  write(*,*)"1-----Solution of Non Linear Equations"
  write(*,*)"2-----Solution of Systems of Linear Equations"
  write(*,*)"3-----Interpolation"
  write(*,*)"4-----Regression"
  write(*,*)"5-----Numerical Integration"
  write(*,*)"6-----Solution of Ordinary Differential Equations"
  write(*,*)"7-----Create a Matrix"
  write(*,*)"8-----Exit"
  write(*,*)"                                                   "

  write(*,*)"Choose a option 1-8"
  read(*,*)Case

  select case (Case)

  case(1)
  call nonLinearEq()

  case(2)
  call linearEqu()

  case(3)
  call interpolation()

  case(4)
  call regression()

  case(5)
  call integration()

  case(6)
  call ordinaryDiff()

  case(7)
  call creatMatrix()

  case(8)
  Stop

 case default
   write(*,*)"Please choose a valid option"
  end select

  end subroutine Menu

  subroutine nonLinearEq


  implicit none
  integer ::it, caso
  real :: tol, f

  DO
      print*, "---Solution of Non Linear Equations---"
    	print *, "1 => Bisection"
    	print *, "2 => False Position"
   	print *, "3 => Newton-Raphson"
  	print *, "4 => Secant"

  	write (*,*) "What method do you want to use?"
      read (*,*) caso

  	write (*,*) "What is the tolerance?"
      read (*,*) tol
      write (*,*) "How many iterations are you requesting?"
  	read (*,*) it

      select case(caso)
        case (1)
         call Bisection(tol,it)

        case (2)
        	call FalsePosition(tol,it)

  	  case (3)
  		call NewtonRaphson(tol,it)

    	  case (4)
        	call Secant(tol,it)

  	case default
        	print *, "ERROR"

  	END SELECT


  	END DO
  end subroutine  nonLinearEq


  	subroutine Bisection(tol, it)

      		real :: x, y, z, err
  			logical :: interval
  			interval = .FALSE.


      		DO WHILE(interval .EQV. .FALSE.)

  				print *, "Write the first interval's number"
          		read *, x
          		print *, "Write the second interval's number"
  				read *, y

  				write(*,*) f(x)
              	write(*,*) f(y)

          		if((f(x)*f(y)) .LT. 0)then
                  	print *, "It's an interval"
                      interval = .TRUE.
  				else
                  	print *, "There is no interval"

                  end if
              end DO

  		call operations_BISECTION(x,y,z,tol,it)

  	end subroutine Bisection


    subroutine FalsePosition(tol, it)

      real :: x,y,z, err

      logical :: interval
      interval = .FALSE.

      do while ( interval .EQV. .FALSE. )

        print *, "Write the first interval's number (LOWER)"
        read *, x

        print *, "Write the second interval's number (BIG)"
        read *, y

        write (*,*) f(x)
        write (*,*) f(y)

        if((f(x)*f(y)) .LT. 0)then
              print *, "It's an interval"
                interval = .TRUE.

        else
              print *, "There is no interval"
            end if
        end do

        call operations_False(x,y,z,tol,it)


    end subroutine FalsePosition


    subroutine operations_False(x,y,z,tol,it)

      integer :: i=0

      do
        z = x +((f(x)*(y-x))/(f(x)-f(y)))
        if ( f(z) .GT. 0 ) then
          x=z
        else
          y=z
        end if

        if ( (x-y) .LT. tol .AND. (y-x) .GE. 0) then

          write (*,*) "Result: ",z
          write (*,*) "Iterations: ", i
          EXIT

        end if
        i =i+1

      end do

    end subroutine operations_False


  	subroutine NewtonRaphson(tol, it)

  		real::x,f,f_PRIME,beg

  			DO
          		write(*,*)"Insert value of x: "
                  	read(*,*)x
                      beg = 0

                  write(*,*)f(x)
                  write(*,*)f_PRIME(x)

  					if(f(x) .LE. tol .AND. f(x) .GE. 0)then
          				print*, "The outcome is: ",x
                          print*, "Amount of iterations made: ",it
                          EXIT
                     END if

              		if(beg == it)then
                      	print*, "The outcome is: ",x
                          print*, "Iterations made: ",it
                       EXIT
                     	END if

                  x= x-(f(x)/f_PRIME(x))
                  	print*, "New x = ",x
                      print*, "Iterations made: ",it
                      beg = beg + 1
                  EXIT
                  END DO


      end subroutine NewtonRaphson



  	subroutine operations_BISECTION(x,y,z,tol,it)

  	integer::i = 0

          DO
            z = (x+y)/2.0
          	if(f(Z).GT.0)then
              	x=z
  			else
              	y=z
              end if
            if((y-x).LT.(tol) .AND. (y-x).GT.0)then
              write (*,*) "Result: ",z
              write (*,*) "Iterations: ",i
  			EXIT
            end if
  			if(i == it)then
              	write (*,*) "Result: ",z
              	write (*,*) "Iterations: ",i
  				EXIT
              end IF
              i=i+1

         end DO
      end subroutine operations_BISECTION

      REAL function f(x)
      	implicit none
          real :: x

          f = x**3-20
   end function f

   	REAL function f_PRIME(x)
  		implicit none
          real::x
          f_PRIME = (3*x**2) + (14*x) + 14
      end function f_PRIME

      subroutine Secant(tol, it)

		real::ans, xS,x0, x1, beg, arc

		print*, "Enter your first 'x' value"
        read(*,*) xS
        x0 = xS
        beg = 0

        x1 = x0
        x0 = x0 * 1.02

		print*, "X: ", x0
        print*, "f(x) ", f(x0)

     	DO
            if(f(x0) .LE. tol .AND. f(x0) .GE. 0)then
              print*, "The function's result is: ",x0
              print*, "Iterations: ",it
              EXIT
           	end if

        if (it ==beg)then

      		print *, "The result was not found"
        	write (*,*) "Equation result: ", x0
        	write (*,*) "Iterations: ", beg
        	EXIT
        end if

        arc = x0
        x0 = x0 - (f(x0)*(x0-x1))/(f(x0)-f(X1))
        x1 = arc


		beg=beg+1
        END DO


    end subroutine Secant


subroutine linearEqu

	integer:: caso

      print *, "----Solution of Systems of Linear Equations----"
	  	print *, "1 => LU decomposition"
	  	print *, "2 => Gaussian Elimination"
	 	  print *, "3 => Gauss Seidel"
      print *, "4 => Create a matrix"
      print*, "5=> Menu"

		write (*,*) "which method do you want to use?"
	   read (*,*) caso

	    select case(caso)
	      case (1)
	       call luDecomposition()

	     	case (2)
	      call gaussian_elimination()

				case (3)
				call	NgaussS()

      case (4)
        call creatMatrix()

      case (5)
        call Menu()

	8	case default
	      	print *, "ERROR"

		END SELECT

	end subroutine linearEqu


subroutine luDecomposition
    implicit none
    !Matrix dimensions
    integer ::Da

    real, allocatable,dimension(:,:):: Am
    real, allocatable,dimension(:,:):: Lt
    real, allocatable,dimension(:,:):: Up
    real, allocatable,dimension(:):: X

    logical:: luDecompositionS, comp
    integer:: row,column

    character (len=10) :: file_name
    print *, 'Give me the name of the file where the matrix is (end in .txt)'
    read (*,*) file_name
    open (10,file=file_name)
    open(10 , file = file_name)
    open(20 , file = "RESULTS.txt")

    read(10,*) Da
    allocate( Am(Da,Da+1), Up(Da,Da+1) , Lt(Da,Da+1), X(Da) )

    read(10,*) (  ( Am(row,column) , column=1 , Da+1 ) , row=1 , Da)
    100 format(1X,F10.3)
    write(20,*) "[Matrix from the file A]"
    do row = 1,Da
        do column = 1,Da+1
        write(20,100,advance='no') Am(row,column)
        end do
        write(20,*)
    end do

      comp = luDecompositionS(Da,Am,Up,Lt,X)

    if( comp .EQV. .TRUE. ) then
      write(20,*) "Lower matrix (L)"
      do row = 1,Da
        do column = 1,Da
          write(20,100,advance='no') Lt(row,column)
        end do
        write(20,*)
      end do

      write(20,*) "Upper Matrix"
      do row = 1,Da
        do column = 1,Da
          write(20,100,advance='no') Up(row,column)
      end do
        write(20,*)
      end do

      write(20,*) "Result x ="

      do row=1,Da
        write(20,100,advance='no') X(row)
      end do

      write(20,*)
    else
      write(20,*) "This system cant be solved"
    end if

    deallocate(Am, Lt, Up, X)
    close(10)
    close(20)

    call Menu()

  end subroutine


  logical function luDecompositionS(Da,Am,Up,Lt,X)
    IMPLICIT NONE
    integer,intent(IN)::Da
    real,intent(IN),dimension(Da,Da+1)::Am
    real,intent(OUT),dimension(Da,Da+1)::Up
    real,intent(OUT),dimension(Da,Da+1)::Lt
    real,intent(OUT),dimension(Da)::X
    real,dimension(Da)::Zr
    integer::i

      Lt(1,1) = Am(1,1)
    if ( Lt(1,1) == 0 ) then
            luDecompositionS = .FALSE.
          return
    end if

        Up(1,1) = 1.0
        Up(1,2) = Am(1,2) / Lt(1,1)
        Zr(1) = Am(1,Da+1) / Lt(1,1)

    do i=2,Da-1
            Lt(i,i-1) = Am(i,i-1)
            Lt(i,i) = Am(i,i) - Lt(i,i-1) * Up(i-1,i)

            if ( Lt(i,i) == 0 ) then

                luDecompositionS = .FALSE.
              return
            end if

          Up(1,1) = 1.0
          Up(i,i+1) = Am(i,i+1) / Lt(i,i)
          Zr(i) = ( Am(i,Da+1) - Lt(i,i-1) * Zr(i-1) ) / Lt(i,i)
        end do

      Up(Da,Da) = 1.0
      Lt(Da,Da-1) = Am(Da,Da-1)
      Lt(Da,Da) = Am(Da,Da) - Lt(Da,Da-1) * Up(Da-1,Da)

      if (Lt(Da,Da) == 0 ) then

          luDecompositionS = .FALSE.

      return
    end if

    Zr(Da) = ( Am(Da,Da+1) - Lt(Da,Da-1) * Zr(Da-1) ) / Lt(Da,Da)

    X(Da) = Zr(Da)
  do i = Da-1,1,-1
        X(i) = Zr(i) - Up(i,i+1) * X(i+1)
  end do
    luDecompositionS = .TRUE.
  return

  call Menu()

  end function

	subroutine gaussian_elimination
	    implicit none

	    integer,parameter::r = 3
	    integer::b,c
	    REAL::s
	    real,dimension(r,r+1)::a
	    real,dimension(r)::x

	    character (len=10) :: file_name
	    print *, 'Give me the name of the file where the matrix is (end in .txt)'
	    read (*,*) file_name

	    print *, 'Give me the number of rows'

	    open (10,file=file_name)
	    open(2,file='gaussian_eliminationR.txt')

	    READ(10,*)((a(b,c),c=1,r+1),b=1,r)

	    write(2,8)"Matrix from de file",((a(b,c),c=1,r+1),b=1,c)

	    do c=1,r

	        do b=c+1,r
	            a(b,:)=a(b,:)-a(c,:)*a(b,c)/a(c,c)
	        end do
	      end do

	    write(2,8)" Gaussian Elimination",((a(b,c),c=1,r+1),b=1,r)

	    do b=r,1,-1
	        s=a(b,r+1)
	        do c=b+1,r
	            s=s-a(b,c)*x(c)
	        end do
	        x(b)=s/a(b,b)
	    end do

	    write(2,9)"Result of X=",(x(b),b=1,r)

	    8 format(a,/,3(4(f7.2,3x),/))
	    9 format(a,/,3(f7.2,/))

	print*, "The result was printed in the file gaussian_eliminationR.txt"
  call Menu()

	end subroutine

	subroutine NgaussS

	  implicit none

	  Integer n,n1,a,b,c,d
	  Double precision, dimension (:,:), allocatable ::M
	  Double precision, dimension (:), allocatable ::x
	  Double precision, dimension (:), allocatable ::oldX
	  Double precision, dimension (:), allocatable ::z
	  Double precision:: temp, sum, e, tol
	  integer:: it

	!  character (len=10) :: file_name
	!  print *, 'Give me the name of the file where the matrix is (end in .txt)'
	!  read (*,*) file_name
	!  open (10,file=file_name)

	open(unit=10, file="data.txt")
	read(10,*)n
	n1 = n+1

	  allocate (M(n,n))
	  allocate (x(n))
	  allocate(z(n))

	  do a=1,n
	    read(10,*)M(a,:),z(a)
	    write(*,*)M(a,:)
	  end do

	    x(:) = 0
	    write(*,*) x(:)

	    close (10)

	    write (*,*)"Tolerance: "
	    read(*,*)tol

	    do
	      do a =1,n
	    !    write(*,*) "P1"
	        temp = 0
	  !      write(*,*) "temp"
	        oldX = x
	  !      write(*,*) "oldx"

	        do b=1,n
	        !  write(*,*) "P2"
	          if (b/=a) then

	            temp = temp + M(a,b)*x(b)
	          end if
	        end do
	        x(a)= (z(a)-temp)/M(a,a)
	      end do

	      sum = 0

	      do a=1,n
	      !  write(*,*) "err"
	        e=x(a)-oldX(a)
	        sum = sum + e
	        it=it+1
	      end do


	      if(abs(sum)<=tol)then
	        EXIT
	      end if

	      if (it>100)then
	        EXIT
	      endif

	    end do

	  open(20 , file = "RESULTS.txt")
	  write (20,*)"Results from the iterations"
	  write(20,*)it
	  write (20,*)"Xs",(",",b,b=1,n)
	  write (20,*)" ",(",",x(a),a=1,n)
	close(20)
	close(10)

	print*,"The results were written on the file call RESULTS.txt"
  call Menu()

end subroutine

subroutine creatMatrix

  ! this program creates an nxm matrix [A] with real random numbers, double precision, between 0 and 100
  ! it read the values "n and m" from keyboard and writes the data in a new file and on screen
  ! it also creates vectors
  	Implicit none
      Integer, parameter :: dp = kind(1.0d0)
      integer i,j,k,row,col,cont,p!size of matrix A,number of rows and columns, k=1 for vector, 2 for matrix
      real (kind=dp),dimension (:,:),allocatable :: A
      real (kind=dp), dimension(:),allocatable :: V
      character (len=20) :: file_name


  		cont =1
  	!	if ( p==0 ) then
  		!call Menu
  		!end if

      do while (cont==1)
      	print *, 'Write 1 to create a VECTOR or 2 to create a MATRIX, the entries will have random numbers between 0 and 100'
      	read(*,*) k
      	If (k==1) then
      		print *,'Give me the number of elements in the VECTOR'
        		read (*,*)row
        		allocate (V(row))
        		call random_number (V)
        		V = V*100
              print *, 'Give me the name of the file to write the vector (end with .txt and less than 20 characters total)'
              read (*,*) file_name
          	open (20,file=file_name)
              do i = 1,row
        			write (*,20) V(i)
        			write (20,20) V(i)
  			end do
  10			format (2x,F5.2)
      	else if (k==2) then
      		print *,'Give me the size of the MATRIX (n(rows) x m(columns)) include the part of the result'
      		read (*,*) row,col
      		allocate (a(row,col))
      		call random_number (A)
     			A = A*100
  !    print *,row,col
              print *, 'Give me the name of the file to write the matrix (end with .txt and less than 20 characters total)'
              read (*,*) file_name
          	open (10,file=file_name)
          	write (10,*) row !writing in the first line the number of equations
      		do i = 1,row
        			write (*,20) (A(i,j),j=1,col)
        			write (10,20) (A(i,j),j=1,col)
      		end do
  20			format (200(2x,F5.2,2x))
  		else
      		print *, 'WRONG number, it has to be 1 or 2'
      	endif
          print *,'Press 1 to write another matrix, 0 to end matrix creation'
          read (*,*) cont

  				close(10)
  				close(20)

  				call Menu()

  			end do

end subroutine creatMatrix


subroutine integration


    implicit none
    integer caso

    write (*,*)"------Numerical Integration-------"
    write (*,*)"1---Simpson 3/8"
    write (*,*)"2---Simpson 1/3"
    write (*,*)"3---Trapezoidal Rule"

    write(*,*)'Select the method'
    READ(*,*)caso

    select case (caso)
    case (1)
      call Simp()

    case (2)
      call simpsOneT()

    case (3)
      call trapezoRule()
    end select

  end subroutine integration


  subroutine Simp

        character (len=10) :: file_name

      write (*,*)"---Simpson 3/8---"
      write(*,*)'Give me the lower limit'
      READ(*,*)a

      write(*,*)'Give me the upper limit'
      READ(*,*)b

      write(*,*)'Number of partitions'
      READ(*,*)n

      h=(b-a)/n

      sum=j(b)+j(a)+3*j(b-h)+3*j(b-2*h)

      do 10 I=1,n-5, 3
        sum=sum+3*j(a+I*h)+3*j(a+(I+1)*h)+2*j(a+(I+2)*h)
  10     continue
       S=3*(sum*h)/8

      write(*,*)"The value is = ",S

      open(2,file = "ResultsSimpson.txt")
      write(2,*)"The function is f(X)=1/X"
      write(2,*)"The value is = ",S
      write(2,*)"a = ",a
      write(2,*)"b = ",b
      write(2,*)"h = ",h
      close(2)
      write (*,*)"The results were written in the file called ResultsSimpson.txt"
      call Menu()

     STOP

   end subroutine

      FUNCTION j(X)
        j = 1/X
      RETURN
      END


      subroutine simpsOneT

      character (len=20) :: file_name

      write (*,*)"---Simpson 1/3---"
      write(*,*)'Give me the lower limit (a)'
      READ(*,*)a

      write(*,*)'Give me the upper limit (b)'
      READ(*,*)b

      write(*,*)'Number of interval (n)'
      READ(*,*)n

      h=(b-a)/n

      S= G(b)+G(a)+4*G(b-h)

        DO 10 I=1,n-3,2

          S=S+4*G(a+I+h)+2*G(a+(I+1)*h)
    10 CONTINUE

           S=(S*h)/3

             write(*,*)"The value is = ",S

           open(2,file = "ResultsSimpson13.txt")
           write(2,*)"Simpson 1/3"
           write(2,*)"The function is: F(x)=x^3/(1+x^0.5)"
           write(2,*)"The value is = ",S
           write(2,*)"a = ",a
           write(2,*)"b = ",b
           write(2,*)"h = ",h
           close(2)
           write (*,*)"The results were written in the file called ResultsSimpson13.txt"
           call Menu()

           stop

      end subroutine

      function G(x)
            G=(1/sqrt(2*3.1416))*exp(-0.5*x**2)
         return
       END function

       subroutine trapezoRule
         real,external::f
         real::a,b,h,p,result,sumat
         integer::n,i
         sumat = 0

         write (*,*)"---Trapezoidal Rule---"
         write(*,*)'Give me the lower limit'
         READ(*,*)a

         write(*,*)'Give me the upper limit'
         READ(*,*)b

         write(*,*)'Number of partitions'
         READ(*,*)n

         h=(b-a)/n

         p=(h/2.)*(z(a)+z(b))

         do i=1,n-1
         sumat=sumat+h*z(a+i*h)
         end do

         result = sumat + p

         write(*,*)"The value is = ",result

       open(2,file = "Trapezoresult.txt")
       write(2,*)"Trapezoidal Rule"
       write(2,*)"The function is: f=(1/sqrt(2*3.1416))*exp(-0.5*x^2)"
       write(2,*)"The value is = ",result
       write(2,*)"a = ",a
       write(2,*)"b = ",b
       write(2,*)"h = ",h
       close(2)
       write (*,*)"The results were written in the file called Trapezoresult.txt"
       call Menu()

    end subroutine

    function z(x)
          z=(1/sqrt(2*3.1416))*exp(-0.5*x**2)
       return
     END

!end




subroutine interpolation

  implicit none
  integer :: opt

  	PRINT*, "-----Interpolation-----"


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


end subroutine interpolation

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
              call Menu()

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

    call Menu()


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

        call Menu()


  END SUBROUTINE NewtonDD

  subroutine ordinaryDiff

    implicit none
    integer :: opcion

    print *, "----Solution of Ordinary Differential Equations----"
    print *, "1 => Euler Method & Modified Euler"
    print *, "2 => Runge Kutta 3rd order"
    print *, "3 => Runge Kutta 4rd order"

    print *, "Select a option"
    read(*,*)opcion

    select case (opcion)

    case(1)
      call meuler()

    case(2)
      call kutta3()

    case(3)
       call kutta4()

    CASE DEFAULT
      PRINT*, "Please select a valid option"

end select
end subroutine ordinaryDiff


subroutine kutta3

  implicit none
  real::x,y,xp,yp,h,k1,k2,k3,fx,n,it

  write(*,*) "Input  the range of values (smallest-largest)"
  read (*,*) x,y
  write(*,*)"Input value of x0 and y0(x0 must be the same value as x"
  read(*,*)xp,yp
  write(*,*) "Input the number of iterations"
  read (*,*) h
  n=(y-x)/h
  x=xp
  it=0

  open(2,file="kutta3Results.txt")
  write(2,*) "Runge Kutta grade 3"
  write(2,*)""
  write(2,*)"Iterations     x       y"
  write(2,*)""

  do while(it<=h)
    write(*,*)"x= ", xp, "y= ",yp
    it=it+1
    k1=fx(xp,yp)
    k2=fx(xp+n/2,y+(n*k1)/2)
    k3=fx(xp+n,yp-n*k1+2*n*k2)
    xp=xp+n
    yp=yp+(n/6)*(k1+4*k2+k3)

    write(2,*)it, "    ", xp,"    ",yp

  enddo

  write(2,*)""
  write(2,*)"The result is: ",xp, " ",yp
  write(2,*)"Numer of iterations: ", it
  close(2)

  call Menu()

end subroutine kutta3


subroutine kutta4

  implicit none
  real::x,y,xp,yp,h,k1,k2,k3,k4,fx,it,n


  write(*,*) "Input the interval of values (smallest-largest)"
  read (*,*) x,y
  write(*,*)"Input value of x0 and y0(x0 must be the same value as x)"
  read(*,*)xp,yp
  write(*,*) "Input the number of iterations"
  read (*,*) h
  x=xp
  n=(y-x)/h
  it=0

  open(2,file="kutta4Results.txt")
  write(2,*) "Runge Kutta grade 4"
  write(2,*)""
  write(2,*)"Iterations     x       y"
  write(2,*)""

  do while(it<=h)
    write(*,*)"x= ", xp, "y= ",yp
    it=it+1
    k1=fx(xp,yp)
    k2=fx(xp+n/2,yp+n*k1/2)
    k3=fx(xp+n/2,yp+n*k2/2)
    k4=fx(xp+n,yp+k3)
    xp=xp+n
    yp=yp+(n/6)*(k1+(2.0*k2)+(2.0*k3)+k4)

    write(2,*)it, "    ", xp,"    ",yp
  enddo

  write(2,*)""
  write(2,*)"The result is: ",xp, " ",yp
  write(2,*)"Numer of iterations: ", it
  close(2)



end subroutine kutta4


real function fx(x,y)
real::x,y
!Function definition
!f=x-y+1
fx= -509.2958/x

endfunction



subroutine meuler

    IMPLICIT NONE
  !  INTEGER,PARAMETER :: it=10
    INTEGER ::i,it
    REAL::a,b,h,x,y,y1E,y2EM,fuc,df
    character (len=2) :: fileN

    it = 0

    !OPEN(2,file=fileN)
    !open(2,file='input.txt')
    write(*,*)"Give me a:"
    read(*,*)a

    write(*,*)"Give me b:"
    read(*,*)b

    write(*,*)"Give me y:"
    read(*,*)y

    write(*,*)"How many iteratios: "
    read(*,*)it

    OPEN(10,file='ResultsEuler.txt')

    !READ(2,*)a,b,y

    x=a
    h=(b-a)/it
    y1E=y
    y2EM=y

    WRITE(10,*)"Iter |  x  |   y |   Euler  | error Euler |   Modified Euler  | error Modified Euler"

    DO i=0,it
        WRITE(10,7)i,x,fuc(x),y1E,ABS(fuc(x)-y1E),y2EM,ABS(fuc(x)-y2EM)
        y1E=df(x,y1E)*h+y1E!Euler method
        y2EM=(df(x,y2EM)+df(x+h,df(x,y2EM)*h+y2EM))*h/2+y2EM! modified Euler
        x=x+h
    END DO

    7 FORMAT(i4,3x,f5.2,5(3x,f12.8))

    write(*,*)"The results where printed in the file called ResultsEuler.txt"
    close(2)
    close(10)


END subroutine meuler

FUNCTION fuc(x)
    IMPLICIT NONE
    REAL::fuc,x
    fuc=x**2+3*x
END FUNCTION

FUNCTION df(x,y)
    IMPLICIT NONE
    REAL::df,x,y
    df=2*x+3
END FUNCTION

subroutine regression

  implicit none
  integer opt

write(*,*)"-----Regression-----"
write(*,*)" "
write(*,*)"1=> Polynomial Regression"
write(*,*)"2=> Exponential Regression"
write(*,*)"3=> Logarithmic Regression"
write(*,*)" "


write(*,*)"Choose a option"
  read(*,*)opt

  select case (opt)

  case (1)
    call polyregression

  case (2)
    call exponentialRegression

  case (3)
    PRINT *, "It works LR"

  case default
    PRINT *, "Please choose a valid option"

  end select

end subroutine regression

subroutine polyregression

IMPLICIT NONE
      INTEGER :: tx, ty, degree, i, j, sumx = 0, sumy = 0
      REAL :: ymean
      CHARACTER (len=50):: firstFileN

      integer, dimension(:), allocatable :: x
      integer, dimension(:), allocatable :: y

	  integer, dimension(:,:), allocatable :: matrix
      integer, dimension(:), allocatable :: rside
      integer, dimension(:,:), allocatable :: expx
	  integer, dimension(:,:), allocatable :: expyx
      integer, dimension(:), allocatable :: sumexpx
      integer, dimension(:), allocatable :: sumexpyx

      PRINT*, "You chose: Polynomial Regression"

  	  Print*,"Insert the file name"
      READ(*,*)firstFileN

      open(10, file = firstFileN)
      read(10, *)tx, ty, degree

      allocate(x(tx+1))
      allocate(y(ty+1))

	  allocate(matrix(degree+1, degree+1))
      allocate(rside(degree+1))
      allocate(expx((degree*2)+1, tx+1))
	  allocate(expyx(degree+1, tx))
      allocate(sumexpx((degree*2)+1))
      allocate(sumexpyx(degree+1))

	  read(10, *)(x(i), i=1, tx)
      read(10, *)(y(i), i=1, ty)

	  do i = 1, tx, 1
      	sumx = x(i) + sumx
        sumy = y(i) + sumy
      end do

      ymean = sumy / ty

      do i = 1, degree*2, 1
        do j = 1, tx, 1
          expx(i, j) = x(1)**i
        end do
      end do

	  do i = 1, degree, 1
        do j = 1, tx, 1
          expyx(i, j) = y(j) * expx(i, j)
        end do
      end do

	  do i = 1, degree*2, 1
        do j = 1, tx, 1
          sumexpx(i) = sumexpx(i) + expx(i, j)
        end do
      end do

      do i = 1, degree, 1
        do j = 1, tx, 1
          sumexpyx(i) = y(j) * expyx(i, j)
        end do
      end do

      !do i = 0, degree, 1
      !  do j = 0, degree, 1
      !    if (i == 0 .AND. j == 0) then
      !      matrix(i, j) = tx
      !    else if ((i == 0 .AND. j == 1) .OR. (i == 1 .AND. j == 0)) then
      !      matrix(i, j) = sumx
      !    else
      !      matrix(i, j) =
      !    end if
      !  end do
      !end do

      do i = 0, degree, 1
        if (i == 0) then
          rside(i) = sumy
        else
          rside(i) = sumexpyx(i)
        end if
      end do



end subroutine polyregression

subroutine exponentialRegression
  implicit none
  INTEGER :: nR, opt, count, nX, nY
  CHARACTER (len=150):: insFile, endFile
  REAL:: x(150), y(150), addX, addY, addX2, addXY, xMed, yMed
  REAL :: la0, a0, a1, xIn, yIn, sT, sR, r, r2

  PRINT *, "Please enter the name of the file frome where the data will be obtained"
  PRINT *, "Keep in mind the name you enter must contain the type of document '.txt' at the end of the chosen name"
  read (*,*)insFile

  OPEN(10, file = insFile)
  READ(10, *)nR

  READ(10, *)(x(count), count = 1, nR)
  READ(10, *)(y(count), count = 1, nR)
  CLOSE(10)

  !---CLOSE INSERTED PROGRAM NAME---!

  print *, "Please enter the name of the file where the results will be printed"
  PRINT *, "Do remember to add the identifier '.txt' at the end of the document's name"
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
  a1 = (nR*addXY-addX*addY) / (nR*addX2-addX**2)
  la0 = yMed - a1*xMed

  do count = 1, nR

    sT = sT + (log(y(count)) - yMed)**2
    sR = sR + (log(y(count)) - (la0+a1*x(count)))**2

  end do

  r2 = (sT - sR)/sT
  r =  SQRT(r2)

  a0 = EXP(la0)

  PRINT *, "The results from the equation are: "
  PRINT *, "ln(a0) -> ", la0
  PRINT *, "a0 -> ", a0
  PRINT *, "a1 -> ", a1
  PRINT *, "Sr -> ", sR
  PRINT *, "St -> ", sT
  PRINT *, "R -> ", r
  PRINT *, "R^2 -> ", r2

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

  call Menu()

end subroutine

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
