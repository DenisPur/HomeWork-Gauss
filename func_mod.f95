module func_mod
use prec_mod
contains


function merge(A, B, n) result(AB)
    implicit none
    real(mp) :: A(n,n), B(n), AB(n,n+1)
    integer :: n, i

    do i = 1, n
        AB(i, 1:n) = A(i,1:n)
        AB(i, n+1) = B(i)
    end do
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

    AB(:,:) = make_last_nonzero(AB, n)
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


function make_last_nonzero(AB, n) result (ABr)
    implicit none
    real(mp) :: AB(n,n+1), ABr(n,n+1), X(n)
    integer :: n, i

    do i = n, 1, -1
        if(AB(i,n) /= 0) then
            X(:) = AB(n,:)
            AB(n,:) = AB(i,:)
            AB(i,:) = X(:)
            exit
        end if
    end do
    ABr = AB
end function


end module