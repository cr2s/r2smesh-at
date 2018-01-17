! Fispact driver module
module fispactdriver
    use ifport
    use gen, only: mcomp, to_lower, read_line, get_line, ijk2str
    use r2senv, only: r2s_fwd, r2s_scratch
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

    type ena
        ! Type to represent natural abundance of chemical element
        integer, allocatable:: a(:)  ! list of a numbers
        real, allocatable:: f(:)     ! list of atom fractions
    end type ena

    type(ena), allocatable:: nat_abund(:)  ! index corresponds to z number

   

    private
    public:: f_name, f_get_name, run_condense, run_collapse, run_inventory, &
             read_tab4, write_gi
    contains
    subroutine read_tab4(funit, ijkf, gi)
        ! Read 2-nd column in the tab4 file for each time step
        implicit none
        integer, intent(in):: funit
        real, intent(out), allocatable:: gi(:, :)
        integer, intent(in):: ijkf(:)
        ! local vars
        character (len=:), allocatable:: l
        character (len=35):: cmnd
        real:: v, tgi(1:30, 0:500)  ! number of groups, number of time intervals. i_t = 0 is for the case when 1-st interval contains no spectrum
        integer:: i_emax, n
        integer:: i_e, i_t 

        i_e = 0
        i_t = 0
        i_emax = 0
        tgi = 0.0
        n = size(ijkf)
        write(cmnd, '(a, <n>(".", i0))') '/tab4', ijkf
        open(funit, file=r2s_scratch // cmnd)
        do while (.not. eof(funit))
            l = read_line(funit)
            if (index(l, 'INTERVAL') .gt. 0) then
                ! This line contains interval index. 
                ! Read the new interval index
                read(l(index(l, 'INTERVAL') + 9:), *) i_t
                ! In Fispact-II tab4, spectrum is given only when it differs from the previous step. Therefore, by default, use
                ! spectrum from the previuos step for the new one.
                tgi(:, i_t) = tgi(:, i_t - 1)
                ! Identify the maximal number of energy groups (must be the same in all time intervals)
                if (i_e .gt. i_emax) i_emax = i_e
                ! Reset group index
                i_e = 0
            else if (index(l, 'MeV)') .gt. 0) then
                ! This is line containing values for the next energy interval
                i_e = i_e + 1
                read(l(index(l, 'MeV)') + 5:), *) v, tgi(i_e, i_t)
            end if
        end do
        ! Prepare output array
        allocate(gi(i_emax, i_t))
        gi(1:i_emax, 1:i_t) = tgi(1:i_emax, 1:i_t)
        close(funit)
        ! ! print out for check
        ! write(*, *) 'Read from tab4', funit, fname
        ! do i_t = 1, size(gi(1, :))
        !     do i_e = 1, size(gi(:, 1))
        !         write(*, *) i_t, i_e, gi(i_e, i_t)
        !     end do
        ! end do
        return
    end subroutine read_tab4

    subroutine write_gi(funit, ijk, gi)
        implicit none
        integer, intent(in):: funit
        integer, intent(in):: ijk(3)
        real, intent(in):: gi(:, :)

        integer:: i, n
        character (len=:), allocatable:: fname
        
        fname = get_file_name(r2s_fwd, '/gi/gi', ".", ijk)
        call report_file_name('Writing gamma intensity to', fname)
        open(funit,   file=fname)
        n = size(gi(:, 1))
        do i = 1, size(gi(1, :))
            write(funit,   '(i, 3i, 1p<n>e13.5)') i, ijk, gi(:, i)
        end do
        close(funit)
        return
    end subroutine write_gi

    function f_get_name(z, a) result(s)
        ! Return FISPACT name of nuclide za
        implicit none
        integer, intent(in):: z, a
        character (len=6):: s

        write(s, '(a2,i3.3)') f_name(z:z), a 
        return
    end function f_get_name

    subroutine split_za(za, z, a)
        implicit none
        integer, intent(in):: za
        integer, intent(out):: z, a
        z = za / 1000
        a = za - z*1000
        return
    end subroutine split_za

    subroutine get_nat_abund(z, a, f)
        implicit none
        integer, intent(in):: z
        integer, intent(out), allocatable:: a(:)
        real, intent(out), allocatable:: f(:)

        integer:: ios, zaid, nz, na
        real:: af
        integer:: zarray(3000), aarray(3000), i
        real:: farray(3000)

        if (size(nat_abund) .eq. 0) then
            ! read natural.txt
            zarray = 0
            aarray = 0
            farray = 0.0
            open(201, file='natural.txt')
            i = 0
            do while (.not. eof(201))
                read(201, *, iostat=ios) zaid, af
                if (ios .eq. 0) then 
                    i = i + 1
                    call split_za(zaid, nz, na)
                    zarray(i) = nz
                    aarray(i) = na
                    farray(i) = af
                end if
            end do
            close(201)
            nz = maxval(zarray) 
            allocate(nat_abund(nz))
            ! Assign fractions to elements
            do i = 1, size(nat_abund)
                na = count(zarray .eq. i)  ! number of isotopes for element z=i
                allocate(nat_abund(i)%a(na))
                allocate(nat_abund(i)%f(na))
                nat_abund(i)%a(:) = pack(aarray, zarray .eq. i)  
                nat_abund(i)%f(:) = pack(farray, zarray .eq. i)  
            end do
        end if
        na = size(nat_abund(z)%a)
        allocate(a(na))
        allocate(f(na))
        a = nat_abund(z)%a
        f = nat_abund(z)%f
        return 
    end subroutine get_nat_abund

    function get_file_name(prefix, base, delimiter, indices) result(fname)
        ! Compose file name from prefix, basename and indices
        character (len=*), intent(in):: prefix
        character (len=*), intent(in):: base
        character (len=*), intent(in):: delimiter
        integer, intent(in):: indices(:)
        character (len=:), allocatable:: fname

        ! local vars
        integer:: n

        character (len=600):: cind  ! string representation of indices
        n = size(indices)
        write(cind, '(<n>(a, i0))') (delimiter, indices(i), i = 1, n)
        ! n = len(prefix // trim(base) // trim(cind))
        ! allocate(character (len=n):: fname)
        fname = prefix // trim(base) // trim(cind)
        return
    end function get_file_name

    subroutine report_file_name(cmnt, fname)
        character (len=*), intent(in):: cmnt, fname

        integer:: n
        integer, parameter:: nmax = 40

        n = len(cmnt)
        if (n .gt. nmax) n = nmax
        write(*, '(a<nmax>, ": ", a)') cmnt(:n), fname
    end subroutine report_file_name


    subroutine run_inventory(funit, f, mat, den, amount, ijkf, ijkc)
        ! Run inventory calc with flux intensity f for 
        ! material with nuclide composition mat at density d.
        ! Name of the folder for inventory calculations is 
        ! defined by nam. The collapsed xs are taken from the
        ! folder defined by namc.
        implicit none
        integer, intent(in):: funit
        real, intent(in):: f, den, amount
        type(mcomp), intent(in):: mat
        integer, intent(in):: ijkf(:), ijkc(:)

        ! local vars
        integer:: ist, nz, na, i, j, non, ni
        real:: v
        character (len=4):: kw
        character (len=:), allocatable:: l
        character (len=:), allocatable:: fname  ! max len: 20 for script name, 7*6 for indices
        logical:: flux_normalized

        integer, allocatable:: aa(:)
        real, allocatable:: fa(:)

        ! write material definition to the standard place
        fname = get_file_name(r2s_scratch, '/mat.content', ".", ijkf)
        call report_file_name('Writing composition to', fname)
        open(funit, file=fname)
        non = 0
        do i = 1, size(mat%f)
            call split_za(mat%za(i), nz, na)
            if (na .eq. 0) then
                call get_nat_abund(nz, aa, fa)
                do j = 1, size(aa)
                    write(funit, *) f_get_name(nz, aa(j)), fa(j) * mat%f(i) * amount  !Total amount of material
                    non = non + 1
                end do 
            else
                write(funit, *) f_get_name(nz, na), mat%f(i) * amount 
                non = non + 1
            end if
        end do
        close(funit)
        fname = get_file_name(r2s_scratch, '/mat.title', ".", ijkf)
        call report_file_name('Writing mat title to', fname)
        open(funit, file=fname)
        write(funit, *) 'DENSITY', den
        write(funit, *) 'FUEL', non
        close(funit)

        ! Write irradiation scenario
        open(funit, file='inv_input.footer')  ! irradiation scenario template
        fname = get_file_name(r2s_scratch, '/scenario', ".", ijkf)
        call report_file_name('Writing scenario to', fname)
        open(funit+1, file=fname)
        flux_normalized = .FALSE.
        do while(.not. eof(funit))
            l = read_line(funit)
            read(l, *, iostat=ist) kw, v
            if (ist .eq. 0 .and. to_lower(kw) .eq. "flux") then
                ! This line contain FLUX keyword
                ist = index(l, kw)
                write(funit+1, '(a, 1pe)') l(:ist+4), v*f
                flux_normalized = .TRUE.
            else
                write(funit+1, '(a)') l
            end if
        end do
        close(funit)
        if (flux_normalized) then
            write(funit+1, '("<< Flux normalized by", 1pe12.5, " >>" )') f 
        end if
        close(funit+1)
        fname = get_file_name('', './inventory1.sh', " ", (/ijkf, ijkc/))
        call report_file_name('Calling script: ', fname)
        ist = system(fname)
    end subroutine run_inventory

    subroutine run_condense()
        ! prepare folder to start fispact collapse with spectrum s
        implicit none
        integer:: ist
        ! integer:: system
        ! Condense does not require any case-specific into. Simply
        ! call the script
        write(*, *) 'Calling ./condense.sh '
        ist = system("./condense.sh ")
    end subroutine run_condense

    subroutine run_collapse(funit, s, ijk)      
        ! prepare folder to start fispact collapse with spectrum s
        implicit none
        integer, intent(in):: funit  ! unit number for IO operations
        real, intent(in):: s(:)
        integer, intent(in):: ijk(:)  ! indices of the coarse mesh where spectrum is collapsed
        ! local vars
        integer:: ist, n
        character (len=:), allocatable:: fname

        ! Write spectrum s to standard place 
        n = size(s)
        ! standard spectrum filename
        fname = get_file_name(r2s_scratch, '/fluxes', ".", ijk)
        call report_file_name('Writing spectrum to', fname)
        open(funit, file=fname)
        write(funit, *) s(n-1:1:-1)  ! s contains also the total value, which is not needed here
        write(funit, *) 1.0          ! First wall loading. Does not matter (?)
        write(funit, *) 'Spectrum for ', ijk, ' written' 
        close(funit)

        ! Prepare the script command:
        fname = get_file_name('', './collapse1.sh', " ", ijk)
        call report_file_name('Calling script', fname)
        ! Call external script to create the working folder
        ist = system(fname)
        return

    end subroutine run_collapse


end module fispactdriver      
