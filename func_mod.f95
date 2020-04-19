module func_mod
use prec_mod
contains

function merge(A, B, n) result(AB)
    implicit none
    real(mp) :: A(n,n), B(n), AB(0:n, 1:n+1)
    integer :: n, i

    do i = 1, n+1
        AB(0, i) = i
    end do

    AB(1:n, n+1) = B(:)
    AB(1:n, 1:n) = A(:,:)
end function

subroutine move_pq(AB, n, p, q, s1, s2)
    !exchange (p,q) and (s1, s2) 
    !with their rows and columns
    implicit none
    real(mp) :: AB(0:n, 1:n+1), Swap(n+1)
    integer :: n, p, q, s1, s2

    if(s2 /= q) then
        Swap(:) = AB(:, s2)
        AB(:, s2) = AB(:, q)
        AB(:,  q) = Swap(:)
    end if

    if(s1 /= p) then
        Swap(:) = AB(s1, :)
        AB(s1, :) = AB(p, :)
        AB( p, :) = Swap
    end if
    return
end subroutine

subroutine make_nonzero(AB, n, s1, s2)
    implicit none
    real(mp) :: AB(0:n, 1:n+1), Swap(n+1)
    integer :: n, p, s1, s2

    if(AB(s1, s2) == 0) then
        do p = s1+1, n
            if(AB(p, s2) /= 0) then
                call move_pq(AB, n, p, s2, s1, s2)  !exch. (p,s2) & (s1, s2) == move rows
                exit
            end if
        end do
    end if
    return
end subroutine

subroutine make_max(AB, n, s1, s2)
    implicit none
    real(mp) :: AB(0:n, 1:n+1)
    integer :: n, s1, s2, p, q, i, j, max
    max = 0
    do i = s1, n
        do j = s2, n
            if(abs(AB(i,j)) > max) then
                p = i
                q = j
            end if
        end do
    end do
    call move_pq(AB, n, p, q, s1, s2)
    return
end subroutine

subroutine make_up_triangle_with_leader(AB, n)
    implicit none
    real(mp) :: AB(0:n, 1:n+1)
    integer :: s, p, n

    do s = 1, n-1
        call make_max(AB, n, s, s)
        AB(s, s:n+1) = AB(s, s:n+1) / AB(s,s)
        do p = s+1, n
            AB(p, s:n+1) = AB(p, s:n+1) - AB(s, s:n+1) * AB(p,s)
        end do
    end do
    AB(n, n:n+1) = AB(s, n:n+1) / AB(n,n)
    return
end subroutine 

subroutine make_up_triangle(AB, n)
    implicit none
    real(mp) :: AB(0:n, 1:n+1)
    integer :: s, p, n

    do s = 1, n-1
        call make_nonzero(AB, n, s, s)
        AB(s, s:n+1) = AB(s, s:n+1) / AB(s,s)
        do p = s+1, n
            AB(p, s:n+1) = AB(p, s:n+1) - AB(s, s:n+1) * AB(p,s)
        end do
    end do
    AB(n, n:n+1) = AB(s, n:n+1) / AB(n,n)
    return
end subroutine

subroutine make_down_triangle(AB, n)
    implicit none
    real(mp) :: AB(0:n, 1:n+1)
    integer :: s, p, n

    do s = n, 2, -1
        do p = s-1, 1, -1
            AB(p, n+1) = AB(p, n+1) - AB(s, n+1) * AB(p,s)
            AB(p, s) = 0
        end do
    end do
    AB(1, n+1) = AB(1, n+1) - AB(1, n+1) * AB(1,1)
    return
end subroutine 

end module