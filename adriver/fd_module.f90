! Fispact driver module
module fispactdriver
    use ifport
    use proc
    use matcomp
    use matall
    use meshtal, only: vf, vc
    use gen, only: to_lower, read_line, get_line, ijk2str
    use r2senv, only: r2s_fwd, r2s_scratch
   

    private
    public:: f_name, f_get_name, run_condense, run_collapse, run_collapse_clean, run_inventory, &
             read_tab4, write_cgi, run_inventory2
    contains
    subroutine read_tab4(ijk, gi)
        ! Read 2-nd column in the tab4 file for each time step
        implicit none
        real, intent(out), allocatable:: gi(:, :)
        integer, intent(in):: ijk(:)
        ! local vars
        character (len=:), allocatable:: fname, l
        real:: v, tgi(1:30, 0:500)  ! number of groups, number of time intervals. i_t = 0 is for the case when 1-st interval contains no spectrum
        integer:: i_emax
        integer:: i_e, i_t

        i_e = 0
        i_t = 0
        i_emax = 0
        tgi = 0.0

        fname = get_file_name(r2s_scratch, '/tab4', ".", ijk)
        call report_file_name('Reading gamma intensity from', fname)
        open(pr_inp, file=fname)

        do while (.not. eof(pr_inp))
            l = read_line(pr_inp)
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
        call report_file_name('                        Read', fname)
        close(pr_inp)
        ! Prepare output array
        allocate(gi(i_emax, i_t))
        gi(1:i_emax, 1:i_t) = tgi(1:i_emax, 1:i_t)
        return
    end subroutine read_tab4

    subroutine write_cgi(ijk, i1, j1, k1, cgi)
        implicit none
        integer, intent(in):: ijk(:)  ! indices of the coarse mesh element
        integer, intent(in):: i1, j1, k1 ! Starting indices of fine mesh elements
        real, intent(in):: cgi(i1:, j1:, k1:, :, :) ! gamma intensities in all fine mesh elements in the coarse mesh element

        ! local vars
        integer:: i, j, k, l, m, ne, nt, i2, j2, k2
        character (len=:), allocatable:: fname
        
        ne = size(cgi(i1, j1, k1, :, 1))  ! number of energy groups
        nt = size(cgi(i1, j1, k1, 1, :))  ! number of time intervals
        i2 = size(cgi(:,  j1, k1, 1, 1)) + i1 - 1
        j2 = size(cgi(i1,  :, k1, 1, 1)) + j1 - 1
        k2 = size(cgi(i1, j1,  :, 1, 1)) + k1 - 1

        do l = 1, nt
            ! Write when at least one of fine mesh elements has non-zero gamma intensity
            if (sum(cgi(:, :, :, :, l)) .gt. 0.0) then
                fname = get_file_name(r2s_fwd, '/gi/cgi', ".", (/l, ijk(1), ijk(2), ijk(3)/))
                call report_file_name('Writing gamma intensity to', fname)
                open(pr_out,   file=fname)
                do i = i1, i2
                do j = j1, j2
                do k = k1, k2
                    if (sum(cgi(i, j, k, :, l)) .gt. 0.0) then 
                        write(pr_out,   '(i, 3i, 1p<ne>e13.5)') l, i, j, k, (cgi(i, j, k, m, l), m = 1, ne)
                    end if
                end do
                end do
                end do
                close(pr_out)
            end if
        end do
        return
    end subroutine write_cgi

    subroutine write_gi(ijk, gi)
        implicit none
        integer, intent(in):: ijk(:)
        real, intent(in):: gi(:, :)

        ! local vars
        integer:: i, n
        character (len=:), allocatable:: fname
        
        fname = get_file_name(r2s_fwd, '/gi/gi', ".", ijk(1:3))
        call report_file_name('Writing gamma intensity to', fname)
        open(pr_out,   file=fname)
        n = size(gi(:, 1))
        do i = 1, size(gi(1, :))
            write(pr_out,   '(i, 3i, 1p<n>e13.5)') i, ijk(1:3), gi(:, i)
        end do
        close(pr_out)
        return
    end subroutine write_gi

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
        write(pr_log, '(a<nmax>, ": ", a)') cmnt(:n), fname
    end subroutine report_file_name

    subroutine run_inventory2(i, j, k, istat, ic, jc, kc, cstat, gi, dryrun)
        ! Run inventory calculation for a mixture of materials in the fine mesh element.
        implicit none
        integer, intent(in):: i, j, k, ic, jc, kc  ! indices of the fine and coarse meshes
        integer, intent(out):: istat               ! Status of inventory run. 0 -- not run, 1 -- run
        integer, intent(inout):: cstat             ! If 0 -- run collapse when necessary and return 1. Otherwise dont run collapse and return 1
        real, allocatable, intent(out):: gi(:, :)  ! Gamma intensities in time intervals
        logical, intent(in):: dryrun               ! Count runs, but do actually nothing

        ! local vars
        integer, allocatable:: cind(:), mind(:)
        real, allocatable:: frac(:), dens(:), conc(:)
        real:: v, evol, f, den
        integer:: nmats  ! number of materials in the fine mesh element
        integer:: tnon, non
        integer:: i_c, ist
        character (len=:), allocatable:: fname  ! max len: 20 for script name, 7*6 for indices
        character (len=:), allocatable:: l
        character (len=4):: kw
        logical:: flux_normalized

        ! Get properties of cells in the current mesh element
        call get_ma_properties(i, j, k, cind, frac, mind, dens, conc, evol)
        conc = conc * 1e24  ! convert from MCNP 1/cm/barn to 1/cm3
        nmats = count(mind .gt. 0)

        
        ! Get flux intensity in the fine mesh element
        f = vf(1, i, j, k, 1)

        if (.not. dryrun) write(pr_log, 100) i, j, k, nmats, f
        100 format ("Fine mesh element: ", 3i6, " contains", i4, " materials at flux ", 1pe12.4)

        if (f .gt. 0 .and. nmats .gt. 0) then
            ! Non-zero flux in non-void element: run fispact
            if (dryrun) then
                istat = 1
                cstat = 1
                return
            end if

            ! Run collapse, if not done previously
            if (cstat .eq. 0) then
                call run_collapse(ic, jc, kc)
                cstat = 1
            end if

            ! Prepare material composition
            fname = get_file_name(r2s_scratch, '/mat.content', ".", (/i, j, k/))
            call report_file_name('Writing composition to', fname)
            open(pr_out, file=fname)
            tnon = 0
            do i_c = 1, size(cind)
                if (mind(i_c) .gt. 0) then 
                    call write_mat_fispact(pr_out,                 &  ! unit
                                           mind(i_c),              &  ! material index
                                           evol*frac(i_c)*conc(i_c),  &  ! amount of material in cell i_c
                                           non)                       ! out, number of entries in this material
                    tnon = tnon + non
                end if
            end do
            close(pr_out)
            fname = get_file_name(r2s_scratch, '/mat.title', ".", (/i, j, k/))
            call report_file_name('Writing mat title to', fname)
            open(pr_out, file=fname)
            write(pr_out, *) 'DENSITY', sum(dens*frac)
            write(pr_out, *) 'FUEL', tnon
            close(pr_out)

            ! Write irradiation scenario
            open(pr_inp, file='inv_input.footer')  ! irradiation scenario template
            fname = get_file_name(r2s_scratch, '/scenario', ".", (/i, j, k/))
            call report_file_name('Writing scenario to', fname)
            open(pr_out, file=fname)
            flux_normalized = .FALSE.
            do while(.not. eof(pr_inp))
                l = read_line(pr_inp)
                read(l, *, iostat=ist) kw, v
                if (ist .eq. 0 .and. to_lower(kw) .eq. "flux") then
                    ! This line contain FLUX keyword
                    ist = index(l, kw)
                    write(pr_out, '(a, 1pe)') l(:ist+4), v*f
                    flux_normalized = .TRUE.
                else
                    write(pr_out, '(a)') l
                end if
            end do
            close(pr_inp)
            if (flux_normalized) then
                write(pr_out, '("<< Flux normalized by", 1pe12.5, " >>" )') f 
            end if
            close(pr_out)
            fname = get_file_name('', './inventory.sh', " ", (/i, j, k, ic, jc, kc/))
            call report_file_name('Calling script: ', fname)
            ist = system(fname)
            if (ist .eq. 0) then
                istat = 1
                call read_tab4((/i, j, k/), gi)
                ! Density in invenoty input is the mean density in the fine mesh element
                gi = gi * evol

                ! Call cleaning script
                fname = get_file_name('', './inventory_clean.sh', " ", (/i, j, k/))
                call report_file_name('Calling script: ', fname)
                ist = system(fname)
            else
                write(pr_log, *) "WARNING: Non-zero exit status for " // fname, ist
            end if

        else
            if (nmats .gt. 0) write(pr_log, *) 'WARNING: Zero flux in non-void fine mesh element', i, j, k
            istat = 0
            cstat = cstat
        end if
        return 
    end subroutine run_inventory2


    subroutine run_inventory(f, mindex, den, amount, ijkf, ijkc)
        ! Run inventory calc with flux intensity f for 
        ! material with nuclide composition mindex at density d.
        ! Name of the folder for inventory calculations is 
        ! defined by nam. The collapsed xs are taken from the
        ! folder defined by namc.
        implicit none
        real, intent(in):: f, den, amount
        integer, intent(in):: mindex
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

        integer, allocatable:: zaid(:)
        real, allocatable:: frac(:)

        ! write material definition to the standard place
        fname = get_file_name(r2s_scratch, '/mat.content', ".", ijkf)
        call report_file_name('Writing composition to', fname)
        open(pr_out, file=fname)
        call write_mat_fispact(pr_out, mindex, amount, non)
        close(pr_out)
        fname = get_file_name(r2s_scratch, '/mat.title', ".", ijkf)
        call report_file_name('Writing mat title to', fname)
        open(pr_out, file=fname)
        write(pr_out, *) 'DENSITY', den
        write(pr_out, *) 'FUEL', non
        close(pr_out)

        ! Write irradiation scenario
        open(pr_inp, file='inv_input.footer')  ! irradiation scenario template
        fname = get_file_name(r2s_scratch, '/scenario', ".", ijkf)
        call report_file_name('Writing scenario to', fname)
        open(pr_out, file=fname)
        flux_normalized = .FALSE.
        do while(.not. eof(pr_inp))
            l = read_line(pr_inp)
            read(l, *, iostat=ist) kw, v
            if (ist .eq. 0 .and. to_lower(kw) .eq. "flux") then
                ! This line contain FLUX keyword
                ist = index(l, kw)
                write(pr_out, '(a, 1pe)') l(:ist+4), v*f
                flux_normalized = .TRUE.
            else
                write(pr_out, '(a)') l
            end if
        end do
        close(pr_inp)
        if (flux_normalized) then
            write(pr_out, '("<< Flux normalized by", 1pe12.5, " >>" )') f 
        end if
        close(pr_out)
        fname = get_file_name('', './inventory.sh', " ", (/ijkf, ijkc/))
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
        call report_file_name('Calling script', './condense.sh ')
        ist = system("./condense.sh ")
    end subroutine run_condense

    subroutine run_collapse(i, j, k)      
        ! prepare folder to start fispact collapse with spectrum s
        implicit none
        integer, intent(in):: i, j, k  ! indices of the coarse mesh where spectrum is collapsed
        ! local vars
        integer:: ist, n, l
        character (len=:), allocatable:: fname

        n = size(vc(:, i, j, k, 1))
        ! standard spectrum filename
        fname = get_file_name(r2s_scratch, '/fluxes', ".", (/i, j, k/))
        call report_file_name('Writing spectrum to', fname)
        open(pr_out, file=fname)
        write(pr_out, *) (vc(l, i, j, k, 1), l = n-1, 1, -1)  ! vc contains also the total value, which is not needed here
        write(pr_out, *) 1.0          ! First wall loading. Does not matter (?)
        write(pr_out, *) 'Spectrum for ', i, j, k, ' written' 
        close(pr_out)

        ! Prepare the script command:
        fname = get_file_name('', './collapse.sh', " ", (/i, j, k/))
        call report_file_name('Calling script', fname)
        ! Call external script to create the working folder
        ist = system(fname)
        return

    end subroutine run_collapse

    subroutine run_collapse_clean(i, j, k)      
        ! prepare folder to start fispact collapse with spectrum s
        implicit none
        integer, intent(in):: i, j, k  ! indices of the coarse mesh where spectrum is collapsed
        ! local vars
        integer:: ist, n, l
        character (len=:), allocatable:: fname

        ! Prepare the script command:
        fname = get_file_name('', './collapse_clean.sh', " ", (/i, j, k/))
        call report_file_name('Calling script', fname)
        ist = system(fname)
        return

    end subroutine run_collapse_clean


end module fispactdriver      
