program FinalDelivery

  implicit none
  integer :: option

DO
    print *, "Please choose an option to solve an equation or to exit the program."

    print *, "1) System of Non Linear Equations"
    print *, "2) System of Linear Equations"
    print *, "3) An Interpolation System"
    print *, "4) Regression"
    print *, "5) Numerical Integration"
    print *, "6) Solutions of Ordinary Differential Equations"

    print *, "7) End program"

    write(*,*) "What do you want to do?"
    read(*,*) option

    SELECT CASE(option)
    case(1)
      print*, "System of non linear equations"

    case(2)
      print*, "System of linear equations"

    case(3)
      print*, "Interpolation"

    case(4)
      print*, "Regression"

    case(5)
      print*, "Numerical integration"

    case(6)
      print*, "Ordinary Differential Equations"

    case(7)
      stop "GOODBYE"

END select
END DO

end program FinalDelivery
