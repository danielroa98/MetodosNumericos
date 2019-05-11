
program PrimeraEntrega/*CORRECT VERSION

implicit none
integer ::it, caso
real :: tol

	print *, " "

DO
  	print *, "1 => Bisection"
  	print *, "2 => False Position"
 	print *, "3 => Newton-Raphson"
	print *, "4 => Secant"
    print *, "5 => Exit program"

	print *, ""

	write (*,*) "What method do you want to use?"
    read (*,*) caso

    if(caso == 5) then
      EXIT
    END IF
    
	write (*,*) "What is the tolerance?"
    read (*,*) tol
    write (*,*) "How many iterations are you requesting?"
	read (*,*) it

    select case(caso)
      case (1)
       /*print *, "Bisection"
       call Bisection(tol,it)
       
      case (2)
        /*print *, "False Position"
      	call FalsePosition(tol,it)
    	
	  case (3)
        /*print *, "Newton-Raphson"
		call NewtonRaphson(tol,it)
		
  	  case (4)
  		/*print *, "Secant"
      	call Secant(tol,it)

	case default
      	print *, "ERROR"

	END SELECT


	END DO
End program PrimeraEntrega

  subroutine Bisection(tol, it)

    		real :: x, y, z
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

  end subroutine Bisection/*ENDS BISECTION SUBROUTINE
    
  subroutine operations_BISECTION(x,y,z,tol,it)/*BEGINS SUBROUTINE FOR BISECTION OPERATIONS


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
            write (*,*) "Evaluated function: ", f(z)
            write (*,*) "Iterations: ",i
			EXIT
          end if
          
		if(i == it)then
        	write (*,*) "Result not found"
			write (*,*) "Evaluated function: ", f(z)
            write (*,*) "f(x): ", f(z)
           	write (*,*) "Iterations: ",i
			EXIT
        end IF
        
       i=i+1

       end DO
  end subroutine operations_BISECTION


  subroutine FalsePosition(tol, it)

    real :: x,y,z,a

    logical :: interval
    interval = .FALSE.

    do while ( interval .EQV. .FALSE. )

      print *, "Write the first interval's number"
      read *, x

      print *, "Write the second interval's number"
      read *, y

      write (*,*) f(x)
      write (*,*) f(y)

      if((f(x)*f(y)) .LT. 0)then
            print *, "It's an interval"
              interval = .TRUE.

      else
            print *, "There is no interval"
          end if

      if(f(x) .LT. 0)then
        a=x
        x=y
        y=a
      end if
      
    end do

    call operations_False(x,y,z,tol,it)


  end subroutine FalsePosition


  subroutine operations_False(x,y,z,tol,it)

    integer :: i=0

    do
      z = x +((f(x)*(y-x))/(f(x)-f(y)))

      if (ABS(y-z) .LT. tol) then

        write (*,*) "Result: ",z
        write (*,*) "Evaluated function: ", f(z)
        write (*,*) "Iterations: ", i
        EXIT

      end if

      if (it == i) then
        
		write (*,*) "Result not found"
        write (*,*) "Max iterations result: ",z
        write (*,*) "Evaluated function: ", f(z)
        write (*,*) "Iterations: ", i
        EXIT
        
      END IF
      y = z
      i = i+1

    end do

  end subroutine operations_False


	subroutine NewtonRaphson(tol, it)

		real::x,f,f_PRIME,beg,error

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


    end subroutine NewtonRaphson/*ENDS N-R SUBROUTINE

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


    REAL function f(x)
    	implicit none
        real :: x
        /*f = x**3 - 7*x**2 +14*x - 6
        f = x**2 - 3

 	end function f

 	REAL function f_PRIME(x)
		implicit none
        real::x
        /*f_PRIME = (3*x**2) + (14*x) + 14
		f_PRIME = 2*x

    end function f_PRIME
