program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), X(:)
    real(mp) :: resid
    integer :: n, row

    n = 3

    allocate(A(n,n), B(n), X(n))

    A(1,:) = (/1, 1, 1/)
    A(2,:) = (/3, 2, 0/)
    A(3,:) = (/1, 2, 3/)
    B(:) = (/7, 7, 7/)

    X = solve_triangle(A, B, n)
    write(*,*) X(:)

    X = solve_diagonal(A, B, n)
    write(*,*) X(:)

end program
