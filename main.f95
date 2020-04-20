program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), AB(:,:), X(:)
    real(mp) :: resid
    integer :: n, i

    n = 3

    allocate(A(n,n), B(n), X(n), AB(0:n, 1:n+1))

    A(1,:) = (/1.0, 2.0, 0.00001/)
    A(2,:) = (/3.0, 10000.0, 0.0/)
    A(3,:) = (/-32.0, 5.0, 99.0/)

    B(:) = (/7.0, 7.0001, 7.0/)

    X(:) = solve_linear_system(A, B, n, 'G')
    write(*,*) 'G-method: ', X(:)
    resid = get_resid(A, B, X, n)
    write(*,*) 'resid: ', resid

    X(:) = solve_linear_system(A, B, n, 'J')
    write(*,*) 'J-method: ', X(:)
    resid = get_resid(A, B, X, n)
    write(*,*) 'resid: ', resid
    
    X(:) = solve_linear_system(A, B, n, 'C')
    write(*,*) 'C-method: ', X(:)
    resid = get_resid(A, B, X, n)
    write(*,*) 'resid: ', resid
end program
