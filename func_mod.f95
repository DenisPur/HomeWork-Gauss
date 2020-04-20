module func_mod
use prec_mod
contains

function solve_linear_system(A, B, n, method) result(X)
    implicit none
    real(mp) :: A(n,n), B(n), AB(0:n, 1:n+1), X(n)
    integer :: n, i
    character :: method ! J-Jordan, G-Gauss, C-gauss with Choise

    AB = merge(A, B, n)

    select case(method)
    case('G')
        call make_up_triangle_with_choise(AB, n, .false.)
        X(:) = solve_triangle_with_choise(AB, n, .false.)
    case('C')
        call make_up_triangle_with_choise(AB, n, .true.)
        X(:) = solve_triangle_with_choise(AB, n, .true.)
    case('J')
        call make_up_triangle_with_choise(AB, n, .false.)
        call make_down_triangle(AB, n)
        X(:) = solve_diagonal(AB, n)
    end select
end function

function get_resid(A, B, X, n) result(resid)
    implicit none
    real(mp) :: A(n,n), B(n), X(n), M(n)
    real(mp) :: resid
    integer :: i, n

    M(:) = matmul(A, X) - B(:)
    resid = sqrt( sum(M*M) )
end function

function solve_diagonal(AB, n) result(X)
    implicit none
    real(mp) :: AB(0:n, 1:n+1), X(1:n)
    integer :: n, i

    do i = 1, n
        X(i) = AB(i, n+1) / AB(i,i)
    end do
end function

function solve_triangle_with_choise(AB, n, with_choise) result(X)
    implicit none
    real(mp) :: AB(0:n, 1:n+1), X(1:n)
    integer :: n, i
    logical :: with_choise

    X(:) = 0
    do i = n, 1, -1 
        X(i) = AB(i,n+1) - sum( X(i:n)*AB(i,i:n) )
    end do

    if(with_choise .eqv. .true.) then
        X(:) = write_with_order(X, AB(0,1:n), n)
    end if
end function

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

function write_with_order(Xin, Ord, n) result(X)
    implicit none
    real(mp) :: Xin(n), X(n), Ord(n)
    integer :: n, i

    do i = 1, n
        X(Ord(i)+eps) = Xin(i)
    end do
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
    max = 0.0
    
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

subroutine make_up_triangle_with_choise(AB, n, with_choise)
    implicit none
    real(mp) :: AB(0:n, 1:n+1)
    integer :: s, p, n
    logical :: with_choise

    do s = 1, n-1
        if(with_choise .eqv. .true.)then
            call make_max(AB, n, s, s)
        else 
            call make_nonzero(AB, n, s, s)
        end if

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
    ! write(*,*) (AB(p,:), new_line('c'), p = 0, n)
    ! AB(1, n+1) = AB(1, n+1) - AB(1, n+1) * AB(1,1)
    return
end subroutine 

end module