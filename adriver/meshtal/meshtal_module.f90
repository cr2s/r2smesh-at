! Module to read meshtal.
module meshtal
    use r2senv
    use proc
    use gen, only: get_line, read_line

    real, allocatable:: xf(:), yf(:), zf(:), ef(:), vf(:, :, :, :, :), & ! fine mesh
                        xc(:), yc(:), zc(:), ec(:), vc(:, :, :, :, :)    ! coarse mesh

    private
    public:: xf, yf, zf, ef, vf, &
             xc, yc, zc, ec, vc, &
             get_fluxes, get_mesh_volume 

    contains
        subroutine get_fluxes
            ! Populate meshtally arrays for flux intensity and spectra
            if (pr_id .eq. 0) then
                call print_log('Read ' // r2s_neutronintensity // ' ... ')
                open(pr_scr, file=r2s_neutronintensity)
                call get_next_tally(pr_scr, nf, xf, yf, zf, ef, vf)
                close(pr_scr)

                ! Read neutron flux spectra
                call print_log('Read ' // r2s_neutronspectra // ' ... ')
                open(pr_scr, file=r2s_neutronspectra)
                call get_next_tally(pr_scr, nc, xc, yc, zc, ec, vc)
                close(pr_scr)

                ! Write header for DGS files
                call print_log('Writing ' // r2s_out // '/dgs.header ... ')
                open(pr_scw, file= r2s_out // '/dgs.header')
                write(pr_scw, '(3i6)') size(xf), size(yf), size(zf)
                n = size(xf)
                write(pr_scw, '(1p<n>e15.6)') xf
                n = size(yf)
                write(pr_scw, '(1p<n>e15.6)') yf
                n = size(zf)
                write(pr_scw, '(1p<n>e15.6)') zf
                close(pr_scw)
            end if
            ! Broadcast read data to all processes
            call mpi_bcast(nf, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)
            call mpi_bcast(nc, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)
            call broadcast_1r(xf)
            call broadcast_1r(yf)
            call broadcast_1r(zf)
            call broadcast_1r(ef)
            call broadcast_5r(vf)

            call broadcast_1r(xc)
            call broadcast_1r(yc)
            call broadcast_1r(zc)
            call broadcast_1r(ec)
            call broadcast_5r(vc)
            ! Print log to check that all processes have the same set of data
            call write_tally_log('Fine   meshtally:', nf, xf, yf, zf, ef, vf)
            call write_tally_log('Coarse meshtally:', nc, xc, yc, zc, ec, vc)
        end subroutine

        function get_mesh_volume(i, j, k) result(v)
            ! Return volume of mesh element i, j, k
            implicit none
            integer, intent(in):: i, j, k
            real:: v

            v = (xf(i + 1) - xf(i)) * & 
                (yf(j + 1) - yf(j)) * &
                (zf(k + 1) - zf(k))
            return 
        end function get_mesh_volume

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

            integer:: s(5), sx, sy, sz, se
            s = shape(v)
            sx = size(x)
            sy = size(y)
            sz = size(z)
            se = size(e)

            call print_log(title)
            write(pr_log, *) 'Mesh tally number', n
            write(pr_log, 100) 'X', sx, x(1), x(sx), sum(x(:sx/2)),  sum(x(sx/2+1:))
            write(pr_log, 100) 'Y', sy, y(1), y(sy), sum(y(:sy/2)),  sum(y(sy/2+1:))
            write(pr_log, 100) 'Z', sz, z(1), z(sz), sum(z(:sz/2)),  sum(z(sz/2+1:))
            write(pr_log, 100) 'E', se, e(1), e(se), sum(e(:se/2)),  sum(e(se/2+1:))
            100 format (a3, " boundaries:", i5, 1pe12.4, '---', 1pe12.4, 1p2e12.4)
            write(pr_log, *) 'Shape of v:', s
            s = s / 2
            write(pr_log, 101) '    Sum v(:, :, :, :, :)', sum(v)
            write(pr_log, 101) '    Sum v(l, :, :, :, :)', sum(v(  :s(1), :, :, :, :))
            write(pr_log, 101) '    Sum v(r, :, :, :, :)', sum(v(s(1)+1:, :, :, :, :))
            write(pr_log, 101) '    Sum v(:, l, :, :, :)', sum(v(:,   :s(2), :, :, :))
            write(pr_log, 101) '    Sum v(:, r, :, :, :)', sum(v(:, s(2)+1:, :, :, :))
            write(pr_log, 101) '    Sum v(:, :, l, :, :)', sum(v(:, :,   :s(3), :, :))
            write(pr_log, 101) '    Sum v(:, :, r, :, :)', sum(v(:, :, s(3)+1:, :, :))
            write(pr_log, 101) '    Sum v(:, :, :, l, :)', sum(v(:, :, :,   :s(4), :))
            write(pr_log, 101) '    Sum v(:, :, :, r, :)', sum(v(:, :, :, s(4)+1:, :))
            write(pr_log, 101) '    Sum v(:, :, :, :, l)', sum(v(:, :, :, :,   :s(5)))
            write(pr_log, 101) '    Sum v(:, :, :, :, r)', sum(v(:, :, :, :, s(5)+1:))
            101 format (a, 1pe12.4)

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
            ! Define wether energy values are given
            if (l(:10) .eq. "   Energy ") then
                ! There are energy bins and values in the table
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
                write(pr_log, *) ss, ' not in ', ll(:50)
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
