module func_mod
use prec_mod
contains


function solve_wichmax(A, B, n) result(X)
    implicit none
    real(mp) :: A(n,n), B(n), AB_Logs(0:n,1:n+1), X(n)
    integer :: n, i

    do i = 1, n+1
        AB_Logs(0,i) = i
    end do
    AB_Logs(1:n,1:n+1) = merge(A, B, n)

    AB_Logs = up_max_triangle(AB_Logs, n)

    X(:) = 0
    do i = n, 1, -1 
        X(i) = AB_Logs(i,n+1) - sum( X(:)*AB_Logs(i,1:n) )
    end do
end function

function solve_triangle(A, B, n) result(X)
    implicit none
    real(mp) :: A(n,n), B(n), X(n), AB(n,n+1)
    integer :: n, i

    X(:) = 0
    AB = merge(A, B, n)
    AB = up_triangle(AB, n)
    do i = n, 1, -1 
        X(i) = AB(i,n+1) - sum( X(:)*AB(i,1:n) )
    end do
end function

function solve_diagonal(A, B, n) result(X)
    implicit none
    real(mp) :: A(n,n), B(n), X(n), AB(n,n+1)
    integer :: n, i

    AB = merge(A, B, n)
    AB = up_triangle(AB, n)
    AB = down_triangle(AB, n)
    do i = 1, n
        X(i) = AB(i,n+1)
    end do
end function

function merge(A, B, n) result(AB)
    implicit none
    real(mp) :: A(n,n), B(n), AB(n,n+1)
    integer :: n, i

    do i = 1, n
        AB(i, 1:n) = A(i,1:n)
        AB(i, n+1) = B(i)
    end do
end function

recursive function up_max_triangle(AB_Logs, n) result(ABr)
    implicit none
    real(mp) :: AB_Logs(0:n,1:n+1), X(1:n+1), Y(0:n)
    real(mp) :: ABr(0:n,1:n+1), AB_res(0:n-1,1:n), AB_step(0:n-1,1:n)
    real(mp) :: max
    integer :: n, i, p, j, q

    AB_Logs(:,:) = make_first_max(AB_Logs, n)

    AB_Logs(1,:) = AB_Logs(1,:) / AB_Logs(1,1)
    do i = 2, n
        AB_Logs(i,:) = AB_Logs(i,:) - AB_Logs(1,:)*AB_Logs(i,1)
    end do

    if(n /= 1) then
        AB_step(0,     1:n) = AB_Logs(0,   2:n+1)
        AB_step(1:n-1, 1:n) = AB_Logs(2:n, 2:n+1)

        AB_res = up_max_triangle(AB_step, n-1)

        AB_Logs(0,   2:n+1) = AB_res(0,     1:n)
        AB_Logs(2:n, 2:n+1) = AB_res(1:n-1, 1:n)
    end if
    ABr = AB_Logs
end function

recursive function up_triangle(AB, n) result(ABr)
    implicit none
    real(mp) :: AB(n,n+1), ABr(n,n+1)
    integer :: n, i

    AB(:,:) = make_first_nonzero(AB, n)
    AB(1,:) = AB(1,:) / AB(1,1)
    
    do i = 2, n
        AB(i,:) = AB(i,:) - AB(1,:)*AB(i,1)
    end do

    if(n /= 1) then
        AB(2:n, 2:n+1) = up_triangle(AB(2:n, 2:n+1), n-1)
    end if
    ABr = AB
end function

recursive function down_triangle(AB, n) result(ABr)
    implicit none
    real(mp) :: AB(n,n+1), ABr(n,n+1), AB_step(n-1,n), AB_res(n-1,n)
    integer :: n, i

    ! AB(:,:) = make_last_nonzero(AB, n)
    AB(n,:) = AB(n,:) / AB(n,n)

    do i = n-1, 1, -1
        AB(i,:) = AB(i,:) - AB(n,:)*AB(i,n)
    end do

    if(n /= 1) then
        AB_step(1:n-1,1:n-1) = AB(1:n-1,1:n-1)
        Ab_step(1:n-1,    n) = AB(1:n-1,  n+1)

        AB_res = down_triangle(AB_step, n-1)

        AB(1:n-1,1:n-1) = AB_res(1:n-1,1:n-1)
        AB(1:n-1,  n+1) = AB_res(1:n-1,    n)
    end if
    ABr = AB
end function

function make_first_nonzero(AB, n) result (ABr)
    implicit none
    real(mp) :: AB(n,n+1), ABr(n,n+1), X(n)
    integer :: n, i

    do i = 1, n
        if(AB(i,1) /= 0) then
            X(:) = AB(1,:)
            AB(1,:) = AB(i,:)
            AB(i,:) = X(:)
            exit
        end if
    end do
    ABr = AB
end function

function make_first_max(AB_Logs, n) result (ABr)
    implicit none
    real(mp) :: AB_Logs(0:n,1:n+1), X(1:n+1), Y(0:n), ABr(0:n,1:n+1)
    integer :: n, i, j, p, q, max

    max = 0
    do i = 1, n
        do j = 1, n
            if( abs(AB_Logs(i,j)) > max) then
                max = abs(AB_Logs(i,j))
                p = i
                q = j
            end if
        end do
    end do

    Y(0:n) = AB_Logs(:, q)
    AB_Logs(:, q) = AB_Logs(:, 1)
    AB_Logs(:, 1) = Y(0:n)

    X(1:n+1) = AB_Logs(p, :)
    AB_Logs(p, :) = AB_Logs(1, :)
    AB_Logs(1, :) = X(1:n+1)

    ABr = AB_Logs
end function

end module