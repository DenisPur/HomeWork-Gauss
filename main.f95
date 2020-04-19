program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), AB(:,:)
    real(mp) :: resid
    integer :: n, i

    n = 3

    allocate(A(n,n), B(n), AB(0:n, 1:n+1))

    A(1,:) = (/1, 2, 1/)
    A(2,:) = (/9, 2, 0/)
    A(3,:) = (/1, 2, 9/)

    B(:) = (/7, 7, 7/)

    AB = merge(A, B, 3)

    write(*,*) (AB(i, :), new_line('c'), i = 0,n)

    call make_up_triangle(AB, n)

    write(*,*) (AB(i, :), new_line('c'), i = 0,n)

    call make_down_triangle(AB, n)

    write(*,*) (AB(i, :), new_line('c'), i = 0,n)
end program
