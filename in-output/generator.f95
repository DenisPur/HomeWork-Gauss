program main
! генератор случайной матрицы nxn
! и столбика 1xn
    integer :: n
    real(4), allocatable :: x(:,:), y(:)

    write(*, '(a40)', advance = 'no') 'Enter the N - number of rows: '
    read(*, *) n

    open(1, file = 'data.dat') !файл, куда они выводятся 

    write(1, '(a, 1x, i6)') "#", n

    allocate(x(n,n), y(n))

    call random_number(x)
    write(1, *) (x(i , :), new_line('c'), i = 1,n)

    call random_number(y)
    write(1, *) (y(i), new_line('c'), i = 1,n)

end program