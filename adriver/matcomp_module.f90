! Module to represent material compositions
module matcomp  
    use proc
    use gen, only: print_log
    implicit none
    integer, allocatable:: &
      mc_zaid(:),  & ! ZAIDs for all materials
      mc_i(:),     & ! Adresses of the last element in mc_zaid and mc_frac
      na_a(:),     & ! A numbers of elements, for natural abundancies
      na_i(:)        ! Adresses of the last element in na_a and na_f

    real, allocatable:: &
      mc_frac(:), &   ! Fractions for all materials
      na_f(:)         ! Natural abundancies for all elements

    character (len=2), dimension(112), parameter:: f_name =            & 
         (/' H', 'He', 'Li', 'Be', ' B', ' C', ' N', ' O', ' F', 'Ne', & 
           'Na', 'Mg', 'Al', 'Si', ' P', ' S', 'Cl', 'Ar', ' K', 'Ca', & 
           'Sc', 'Ti', ' V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', & 
           'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', ' Y', 'Zr', & 
           'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', & 
           'Sb', 'Te', ' I', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', & 
           'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', & 
           'Lu', 'Hf', 'Ta', ' W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', & 
           'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', & 
           'Pa', ' U', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', & 
           'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 'Ds', & 
           'Rg', 'Cn' /)


    private
    public:: get_mat, get_zaid, get_frac, write_mat_fispact

    contains
        subroutine get_mat(fname)
            ! Populate arrasy for all processes
            implicit none
            character (len=*), intent(in):: fname

            if (pr_id .eq. 0) then
                call print_log('Reading FIS table from ' // fname // ' ... ')
                call p_read_mat(fname)
                call print_log('Reading natural.txt')
                call p_read_na
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

        function f_get_name(z, a) result(s)
            ! Return FISPACT name of nuclide za
            implicit none
            integer, intent(in):: z, a
            character (len=6):: s

            write(s, '(a2,i3.3)') f_name(z:z), a 
            return
        end function f_get_name

        function get_frac(i) result(fa)
            ! Return array of fractions for material with index i
            implicit none
            integer, intent(in):: i
            real, allocatable:: fa(:)

            allocate(fa(mc_i(i) - mc_i(i-1)))
            fa = mc_frac(mc_i(i-1)+1: mc_i(i))
            return
        end function get_frac

        subroutine write_mat_fispact(funit, mindex, amount, non)
            ! Write material with index mindex to unit funit
            implicit none
            integer, intent(in):: funit       ! file unit, should be already opened
            integer, intent(in):: mindex      ! Material index
            real, intent(in):: amount         ! Amount of material to be irradiated, and density
            integer, intent(out):: non        ! Number of entries in the material

            ! local vars
            integer:: i, j, z, a
            non = 0
            do i = mc_i(mindex-1)+1, mc_i(mindex)
                call split_za(mc_zaid(i), z, a)
                if (a .eq. 0) then
                    do j = na_i(z-1)+1, na_i(z)
                        write(funit, *) f_get_name(z, na_a(j)), na_f(j) * mc_frac(i) * amount  !Total amount of material
                        non = non + 1
                    end do 
                else
                    write(funit, *) f_get_name(z, a), mc_frac(i) * amount 
                    non = non + 1
                end if
            end do
            return
        end subroutine

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

            open(pr_inp, file=fname)

            ! First run through the file: get number of zaids and fractions to store
            read(pr_inp, *)
            read(pr_inp, *) n  ! number of materials
            allocate(mc_i(0: n))
            mc_i = 0
            do i = 1, n
                read(pr_inp, *), nam, &  ! Dummy string
                                 mi,  &  ! Material index
                                 mn,  &  ! Material name
                                 nn      ! number of entries
                mc_i(mi) = nn
                do j = 1, nn
                    read(pr_inp, *)
                end do
            end do

            ! Convert from number of entries to positions
            do i = 1, n
                mc_i(i) = mc_i(i-1) + mc_i(i)
            end do

            allocate(mc_zaid(mc_i(n)))
            allocate(mc_frac(mc_i(n)))

            ! Second run: read all zaids and fractions
            rewind(pr_inp)
            read(pr_inp, *)
            read(pr_inp, *)
            do i = 1, n
                read(pr_inp, *) nam, mi, mn, nn
                istart = mc_i(mi-1)
                do j = 1, nn
                    read(pr_inp, *), mc_zaid(istart + j),  &  ! ZAID
                                     nam,                  &  ! dummy string
                                     mc_frac(istart + j)      ! fraction
                end do
            end do
            close(pr_inp)
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

        subroutine p_read_na
            implicit none
            integer:: i, na, nz, ios, za, z, a
            integer:: i1, i2
            real:: f
            ! read natural.txt
            open(pr_inp, file='natural.txt')

            ! First run: get dimensions
            na = 0  ! number of entries in the files
            nz = 0  ! maximal Z
            do while (.not. eof(pr_inp))
                read(pr_inp, *, iostat=ios) za, f
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
            rewind(pr_inp)
            na = 0  ! number of isotopes for current Z
            nz = 0  ! current Z
            do while (.not. eof(pr_inp))
                read(pr_inp, *, iostat=ios), za, f
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
            rewind(pr_inp)
            do while (.not. eof(pr_inp))
                read(pr_inp, *, iostat=ios), za, f
                if (ios .eq. 0) then
                    call split_za(za, z, a)
                    i1 = na_i(z - 1) + 1
                    i2 = na_i(z)
                    na = count(na_a(i1: i2) .gt. 0)  ! number of isotopes allready in z
                    na_a(i1 + na) = a
                    na_f(i1 + na) = f
                end if
            end do
            close(pr_inp)
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
