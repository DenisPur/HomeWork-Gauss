program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), AB(:,:), X(:)
    real(mp) :: resid
    integer :: n, row

    n = 3

    allocate(A(n,n), B(n), AB(n,n+1), X(n))

    A(1,:) = (/1, 1, 1/)
    A(3,:) = (/3, 2, 0/)
    A(2,:) = (/1, 2, 3/)
    B(:)   = (/7, 7, 7/)
    AB(:,:) = merge(A, B, n)

    write(*,*) (AB(row, :), new_line('c'), row = 1,n)

    AB(:,:) = up_triangle(AB,n)

    write(*,*) (AB(row, :), new_line('c'), row = 1,n)

    X(:) = write_rec(A, B, n)
    write(*,*) X(:)

    write(*,*) 'добавил комментарий для git'
    write(*,*) 'git делает ping!!'

end program
