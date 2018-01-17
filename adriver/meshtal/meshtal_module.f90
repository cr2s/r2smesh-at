! Module to read meshtal.
module meshtal
    use gen, only: get_line, read_line
    integer, parameter:: nt = 1
    private
    public:: get_nps, get_next_tally, write_tally_log

    contains
        function get_nps(fn) result(nps)
            ! read nps from meshtal file opened as unit fn
            implicit none
            integer, intent(in):: fn
            integer (kind=8):: nps

            ! local variables
            character (len=100):: l  ! 100 chould be enough to search for nps
            integer i
            nps = -1
            rewind(fn)
            l = get_line(fn, 'of histories used for', 100)
            i = index(l, '=')
            if (i > 0) then
                read(l(i+1:), *) nps
                return
            end if
            return
        end function get_nps

        subroutine get_next_tally(fn, n, x, y, z, e, v)
            ! Read from the meshtal file opened as unit fn the next tally.
            implicit none
            integer, intent(in):: fn  ! meshtal unit number
            integer, intent(out):: n  ! tally number
            real, allocatable, intent(out):: x(:), y(:), z(:), e(:)
            real, allocatable, intent(out):: v(:, :, :, :, :)

            integer i
            character (len=50):: l

            ! find where the next tally starts
            l = get_line(fn, 'Mesh Tally Number', len(l))
            i = index(l, 'r')
            if (i > 0) then
                read(l(i+1:), *) n
            else
                write(*, *) 'Cannot read tally number from line'
                write(*, *) l
                stop
            end if

            ! read bin boundaries
            l = get_line(fn, 'Tally bin boun', 50)
            x = read_bin_boundaries(fn, 'X direction', 10000)
            y = read_bin_boundaries(fn, 'Y direction', 10000)
            z = read_bin_boundaries(fn, 'Z direction', 10000)
            e = read_bin_boundaries(fn, 'Energy bin',  10000)
            v = read_values(fn, size(x), size(y), size(z), size(e))

            ! Check total values, if any.
            return

        end subroutine get_next_tally

        subroutine write_tally_log(title, n, x, y, z, e, v)
            ! Print out information about the meshtally
            implicit none
            character (len=*), intent(in):: title
            integer, intent(in):: n
            real, intent(in):: x(:), y(:), z(:), e(:), v(:, :, :, :, :)

            write(*, *) title
            write(*, *) 'Mesh tally number', n
            write(*, *) 'X boundaries', size(x), x(1), ' ... ', x(size(x))
            write(*, *) 'Y boundaries', size(y), y(1), ' ... ', y(size(y))
            write(*, *) 'Z boundaries', size(z), z(1), ' ... ', z(size(z))
            write(*, *) 'E boundaries', size(e), e(1), ' ... ', e(size(e))
            write(*, *) 'Shape of v:', shape(v)

        end subroutine write_tally_log

        function read_values(fn, Nx, Ny, Nz, Ne) result(v)
            ! Read values from the meshtal file opened as unit fn.
            ! Nx, Ny, Nz and Ne -- number of bin boundaries in respective directions.
            implicit none
            integer, intent(in):: fn, Nx, Ny, Nz, Ne
            real, allocatable:: v(:, :, :, :, :)

            character (len=50):: l
            integer ie, ix, iy, iz
            integer nei
            real d
            ! If there are more than 2 energy bins, there is additional, total
            if (Ne .gt. 2) then
                nei = Ne
            else
                nei = Ne - 1
            end if
            allocate(v(nei, Nx - 1 , Ny - 1, Nz - 1, 2))
            ! Position file to the values header
            l = get_line(fn, 'X         Y         Z', 50)
            write(*, *) 'Heaader line:' 
            write(*, *) '|' // l // '|'
            write(*, *) '|' // l(:10) // '|'
            ! Define wether energy values are given
            if (l(:10) .eq. "   Energy ") then
                ! There are energy bins and values in the table
                write(*, *) 'Mesh tally table contains energy'
                do ie = 1, Ne - 1
                    do ix = 1, Nx - 1
                        do iy = 1, Ny - 1
                            do iz = 1, Nz - 1
                                read(fn, *) d, d, d, d, &
                                            v(ie, ix, iy, iz, 1), &
                                            v(ie, ix, iy, iz, 2)
                            end do
                        end do
                    end do
                end do
                if (Ne .gt. 2) then
                    write(*, *) 'Mesh tally table contains total energy bin'
                    do ix = 1, Nx - 1
                        do iy = 1, Ny - 1
                            do iz = 1, Nz - 1
                                read(fn, *) l(:9), d, d, d, &
                                            v(Ne, ix, iy, iz, 1), &
                                            v(Ne, ix, iy, iz, 2)
                            end do
                        end do
                    end do
                end if
            else
                ! There are no energy bins and no energy values in the table.
                write(*, *) 'Mesh tally table does not contain energy'
                do ix = 1, Nx - 1
                    do iy = 1, Ny - 1
                        do iz = 1, Nz - 1
                            read(fn, *) d, d, d, &
                                        v(1, ix, iy, iz, 1), &
                                        v(1, ix, iy, iz, 2)
                        end do
                    end do
                end do
            end if
            return
        end function read_values


        function read_bin_boundaries(fn, ss, Nmax) result(bl)
            ! Read bin boundaries from the next line in meshtal opened as unit fn.
            implicit none
            integer, intent(in):: fn, Nmax
            character (len=*), intent(in):: ss
            real, allocatable:: bl(:)

            character (len=:), allocatable:: ll
            character c
            integer i, j, k, n
            real lbl(Nmax), dummy

            ll = read_line(fn)
            i = index(ll, ss)
            if (i .eq. 0) then
                ! Type mismatch. Return empty array
                write(*, *) ss, ' not in ', ll(:50)
                allocate(bl(0))
                return
            else
                i = index(ll, ':') + 1
                read(ll(i:), *) dummy
                dummy = dummy - 1.0
                lbl = dummy
                read(ll(i:), *, iostat=j) lbl
                n = count(lbl /= dummy)
                allocate(bl(n))
                bl(:n) = lbl(:n)
                return
            end if
        end function read_bin_boundaries



end module meshtal
