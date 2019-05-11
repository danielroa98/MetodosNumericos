! this program creates an nxm matrix [A] with real random numbers, double precision, between 0 and 100
! it read the values "n and m" from keyboard and writes the data in a new file and on screen
! it also creates vectors
Program systemOfLinearEquations
	implicit none
	integer ::caso
	DO
		 print *, "1->Create a Matrix"
	  print *, "2->Solve a Matrix"
		print *, "3->End program"


		write (*,*) "What do you want to do?"
	    read (*,*) caso


	    select case(caso)
	      case (1)
	       call Matrix_Creator()

	      case (2)
					call Menu()

				case(3)
					stop "END"

		case default
	      	print *, "ERROR"
		END SELECT
		END DO


end program systemOfLinearEquations

subroutine Matrix_Creator
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
	      print *,'Give me the size of the MATRIX (n(rows) x m(columns)) include the column of the result'
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

				if ( cont.eq.0 ) then
					 call Menu()
				end if

	      close(10)
	      close(20)



	    end do

end subroutine Matrix_Creator

subroutine Menu

	integer:: caso
		print *, "Choose a method to solve the matrix"
	  	print *, "1 => LU Dpuecomposition"
	  	print *, "2 => Gaussian Elimination"
	 	  print *, "3 => Gauss Seidel"

		write (*,*) "which method do you want to use?"
	   read (*,*) caso

	    select case(caso)
	      case (1)
	       call luDecomposition()

	     	case (2)
	      call gaussian_elimination()

				case (3)
				call	NgaussS()

!			call NewtonRaphson

		case default
	      	print *, "ERROR"

		END SELECT

	end subroutine Menu


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
    open(20 , file = "RESULTS-LU.txt")

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

	 !   print *, 'Give me the number of rows'

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

	open(unit=10, file="prueba.txt")
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

end subroutine
