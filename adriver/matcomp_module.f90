! Module to represent material compositions
module matcomp  
    use r2senv
    use proc
    implicit none
    integer, allocatable:: &
      mc_zaid(:),  & ! ZAIDs for all materials
      mc_i(:),     & ! Adresses of the last element in mc_zaid and mc_frac
      na_a(:),     & ! A numbers of elements, for natural abundancies
      na_i(:)        ! Adresses of the last element in na_a and na_f

    real, allocatable:: &
      mc_frac(:), &   ! Fractions for all materials
      na_f(:)         ! Natural abundancies for all elements


    private
    public:: get_mat, get_zaid, get_frac, get_naa, get_naf, split_za

    contains
        subroutine get_mat()
            ! Populate arrays for all processes
            implicit none


            if (pr_id .eq. 0) then
                call print_log('Reading ' // r2s_matcomposition // ' ... ')
                call p_read_mat(r2s_matcomposition)

                call print_log('Reading ' // r2s_natab // ' ... ')
                call p_read_na(r2s_natab)
            end if

            call broadcast_1i(mc_i, 0)
            call broadcast_1i(mc_zaid, 1)
            call broadcast_1r(mc_frac)

            call broadcast_1i(na_i, 0)
            call broadcast_1i(na_a, 1)
            call broadcast_1r(na_f)

            call p_write_mat_log
            call p_write_nat_log
        end subroutine get_mat

        function get_zaid(i) result(zaa)
            ! Return za array for material with index i
            implicit none
            integer, intent(in):: i
            integer, allocatable:: zaa(:)

            allocate(zaa(mc_i(i) - mc_i(i-1)))
            zaa = mc_zaid(mc_i(i-1)+1: mc_i(i))
            return
        end function get_zaid

        function get_frac(i) result(fa)
            ! Return array of fractions for material with index i
            implicit none
            integer, intent(in):: i
            real, allocatable:: fa(:)

            allocate(fa(mc_i(i) - mc_i(i-1)))
            fa = mc_frac(mc_i(i-1)+1: mc_i(i))
            return
        end function get_frac

        function get_naa(z) result(aa)
            ! For element z return list of A in natural composition
            ! The order is the same as returned by get_naf.
            implicit none
            integer, intent(in):: z
            integer, allocatable:: aa(:)

            allocate(aa(na_i(z) - na_i(z-1)))
            aa = na_a(na_i(z-1)+1: na_i(z))
            return
        end function get_naa

        function get_naf(z) result(fa)
            ! For element z return list of natural fractions of isotopes A.
            ! The order is the same as returned by get_naa.
            implicit none
            integer, intent(in):: z
            real, allocatable:: fa(:)

            allocate(fa(na_i(z) - na_i(z-1)))
            fa = na_f(na_i(z-1)+1: na_i(z))
            return
        end function get_naf

        subroutine p_write_mat_log
            ! Dump material compositions from all processes to log files.
            implicit none
            integer:: n, i
            write(pr_log, *) 'Material compositions:'
            do i = 1, size(mc_i) - 1
                write(pr_log, *) i
                n = mc_i(i) - mc_i(i-1)
                write(pr_log, '(<n>i12)') get_zaid(i)
                write(pr_log, '(1p<n>e12.4)') get_frac(i)
            end do
        end subroutine p_write_mat_log

        subroutine p_read_mat(fname)
            ! Read the file containing print table FIS
            implicit none
            character (len=*), intent(in):: fname

            ! local vars
            integer:: n, i, j, mi, mn, nn, istart
            character (len=6):: nam

            open(pr_scr, file=fname)

            ! First run through the file: get number of zaids and fractions to store
            read(pr_scr, *)
            read(pr_scr, *) n  ! number of materials
            allocate(mc_i(0: n))
            mc_i = 0
            do i = 1, n
                read(pr_scr, *), nam, &  ! Dummy string
                                 mi,  &  ! Material index
                                 mn,  &  ! Material name
                                 nn      ! number of entries
                mc_i(mi) = nn
                do j = 1, nn
                    read(pr_scr, *)
                end do
            end do

            ! Convert from number of entries to positions
            do i = 1, n
                mc_i(i) = mc_i(i-1) + mc_i(i)
            end do

            allocate(mc_zaid(mc_i(n)))
            allocate(mc_frac(mc_i(n)))

            ! Second run: read all zaids and fractions
            rewind(pr_scr)
            read(pr_scr, *)
            read(pr_scr, *)
            do i = 1, n
                read(pr_scr, *) nam, mi, mn, nn
                istart = mc_i(mi-1)
                do j = 1, nn
                    read(pr_scr, *), mc_zaid(istart + j),  &  ! ZAID
                                     nam,                  &  ! dummy string
                                     mc_frac(istart + j)      ! fraction
                end do
            end do
            close(pr_scr)
            return
        end subroutine p_read_mat 

        subroutine split_za(za, z, a)
            implicit none
            integer, intent(in):: za
            integer, intent(out):: z, a
            z = za / 1000
            a = za - z*1000
            return
        end subroutine split_za

        subroutine p_read_na(fname)
            implicit none
            character (len=*), intent(in):: fname
            integer:: i, na, nz, ios, za, z, a
            integer:: i1, i2
            real:: f
            ! read natural.txt
            open(pr_scr, file=fname)

            ! First run: get dimensions
            na = 0  ! number of entries in the files
            nz = 0  ! maximal Z
            do while (.not. eof(pr_scr))
                read(pr_scr, *, iostat=ios) za, f
                if (ios .eq. 0) then 
                    na = na + 1
                    call split_za(za, z, a)
                    if (nz .lt. z) nz = z
                end if
            end do
            allocate(na_i(0:nz))
            allocate(na_a(na))
            allocate(na_f(na))
            na_i = 0
            na_a = 0
            na_f = 0.0

            ! Second run: get dimensions for every Z
            rewind(pr_scr)
            na = 0  ! number of isotopes for current Z
            nz = 0  ! current Z
            do while (.not. eof(pr_scr))
                read(pr_scr, *, iostat=ios), za, f
                if (ios .eq. 0) then
                    call split_za(za, z, a)
                    na_i(z) = na_i(z) + 1
                end if
            end do
            ! Convert na_i from number of isotopes to addresses
            do i = 1, size(na_i) -1
                na_i(i) = na_i(i - 1) + na_i(i)
            end do

            ! Third run: read data into arrays
            rewind(pr_scr)
            do while (.not. eof(pr_scr))
                read(pr_scr, *, iostat=ios), za, f
                if (ios .eq. 0) then
                    call split_za(za, z, a)
                    i1 = na_i(z - 1) + 1
                    i2 = na_i(z)
                    na = count(na_a(i1: i2) .gt. 0)  ! number of isotopes allready in z
                    na_a(i1 + na) = a
                    na_f(i1 + na) = f
                end if
            end do
            close(pr_scr)
        end subroutine p_read_na

        subroutine p_write_nat_log
            ! Write natural abundancies to log file, to check that all processes has the sate data set
            implicit none
            integer:: z, i1, i2, n
            write(pr_log, *) 'Natural abundancies'
            do z = 1, size(na_i) - 1
                i1 = na_i(z-1) + 1
                i2 = na_i(z)
                n = i2 - i1 + 1
                if (n .gt. 0) then
                    write(pr_log, *) 'Element Z=', z
                    write(pr_log, '(<n>i12)') na_a(i1:i2)
                    write(pr_log, '(1p<n>e12.4)') na_f(i1:i2)
                end if
            end do
        end subroutine p_write_nat_log



end module matcomp      
