module func_mod
use prec_mod
contains 

recursive function write_rec(A, B, n) result(X)
    !test input/output - complete
    implicit none
    real(mp) :: A(n,n), B(n), X(n)
    integer :: n

    X(1) = A(1,1)
    if(n /= 1) then
        x(2:n) = write_rec(A(2:n, 2:n), B(2:n), n-1)
    end if
end function

!recursive function solve(A, B, n) result(X)
    !comment
!    implicit none
!    real(mp) :: A(n,n), B(n), X(n)
!    integer :: n
    !...
    !...
!end function

recursive function up_triangle(AB, n) result(ABr)
    implicit none
    real(mp) :: AB(n,n+1), ABr(n,n+1)
    integer :: n, i

    AB(1,:) = AB(1,:) / AB(1,1)
    do i = 2, n
        AB(i,:) = AB(i,:) - AB(1,:)*AB(i,1)
    end do
    
    if(n /= 1) then
        AB(2:n, 2:n+1) = up_triangle(AB(2:n, 2:n+1), n-1)
    end if

    ABr(:,:) = AB(:,:)
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

end module