program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), X(:)
    real(mp) :: resid
    integer :: n, i

    n = 3

    allocate(A(n,n), B(n), X(n))

    A(1,:) = (/1, 2, 1/)
    A(2,:) = (/9, 2, 0/)
    A(3,:) = (/1, 2, 9/)

    B(:) = (/7, 7, 7/)

end program
