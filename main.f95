program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), AB(:,:), X(:)
    real(mp) :: resid
    integer :: n, i

    open(1, file = './in-output/data.dat')
    read(1, '(2x, i12)') n

    open(2, file = './in-output/result.dat')
    write(2, *) '# ', n

    allocate(A(n,n), B(n), X(n), AB(0:n, 1:n+1))

    read(1, *) A
    A = transpose(A)
    read(1, *) B

    X(:) = solve_linear_system(A, B, n, 'G')
    write(2,*) 'G-method: ', X(:)
    resid = get_resid(A, B, X, n)
    write(2,*) 'resid: ', resid

    X(:) = solve_linear_system(A, B, n, 'J')
    write(2,*) 'J-method: ', X(:)
    resid = get_resid(A, B, X, n)
    write(2,*) 'resid: ', resid

    X(:) = solve_linear_system(A, B, n, 'C')
    write(2,*) 'C-method: ', X(:)
    resid = get_resid(A, B, X, n)
    write(2,*) 'resid: ', resid

end program
