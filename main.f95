program gauss_linear_system 
use func_mod
use prec_mod
    implicit none
    real(mp), allocatable :: A(:,:), B(:), X(:), AB_Logs(:,:)
    real(mp) :: resid
    integer :: n, i

    n = 3

    allocate(A(n,n), B(n), X(n), AB_Logs(0:n,n+1))

    A(1,:) = (/1, 2, 1/)
    A(2,:) = (/9, 2, 0/)
    A(3,:) = (/1, 2, 9/)
    ! A(4,:) = (/4, 3, 2, 2/)
    B(:) = (/7, 7, 7/)

    X = solve_triangle(A, B, n)
    write(*,*) X(:)

    write(*,*) '==='

    X = solve_diagonal(A, B, n)
    write(*,*) X(:)

    write(*,*) '==='

    X = solve_wichmax(A, B, n)
    write(*,*) X(:)

    write(*,*) '==='

    do i = 1, n+1
        AB_Logs(0,i) = i
    end do
    AB_Logs(1:n,1:n+1) = merge(A, B, n)
    write(*,*) (AB_Logs(i, :), new_line('c'), i = 0,n)

    AB_Logs = up_max_triangle(AB_Logs, n)
    write(*,*) (AB_Logs(i, :), new_line('c'), i = 0,n)


end program
