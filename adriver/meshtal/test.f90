! A program to test the meshtal module
program meshtal_test
    use meshtal, only: get_nps, get_next_tally
    implicit none

    integer (kind=8):: nps
    integer n
    character (len=1000) :: fn='meshtal'
    real, allocatable:: x(:), y(:), z(:), e(:), v(:, :, :, :, :)

    ! get meshtal filename from the command line
    n = nargs()
    if (n > 1) then
        call getarg(1, fn)
    end if
    write(*, *) 'Reading meshtal from ', trim(fn)
    open(1, file=fn)
    nps = get_nps(1)
    write(*, *)' NPS:', nps

    do while (.not. eof(1))
        call get_next_tally(1, n, x, y, z, e, v)
        write(*, *) 'Read tally', n
        write(*, *) 'X bins', size(x), x
        write(*, *) 'Y bins', size(y), y
        write(*, *) 'Z bins', size(z), z
        write(*, *) 'E bins', size(e), e
        write(*, *) 'Vals:', size(v)
        do n = 1, size(z) - 1
            write(*, *) v(1, 1, 1, n, 1), v(1, 1, 1, n, 2)
        end do
    end do


end program meshtal_test
