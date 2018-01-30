! Fispact driver module
module fispactdriver
    use ifport
    use r2senv
    use proc
    use matcomp
    use matall
    use meshtal, only: vf, vc
    use gen, only: to_lower, read_line, get_line, get_file_name
   

    private
    public:: f_name, f_get_name, run_condense, run_collapse, run_collapse_clean, run_inventory, &
             read_tab4, write_cgi, check_cgi, run_inventory2
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

        fname = r2s_scratch // ijk2str('/r2s_r/tab4', '.', ijk)
        open(pr_scr, file=fname)

        do while (.not. eof(pr_scr))
            l = read_line(pr_scr)
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
        call report_file_name('Read gamma intensity from', fname // ' , last time interval: ' // i2str(i_t))
        close(pr_scr)
        ! Prepare output array
        allocate(gi(i_emax, i_t))
        gi(1:i_emax, 1:i_t) = tgi(1:i_emax, 1:i_t)
        return
    end subroutine read_tab4

    function check_cgi(ijk) result(cgi_exists)
        implicit none
        integer, intent(in):: ijk(:)  ! indices of the coarse mesh element
        logical:: cgi_exists

        ! local vars
        character (len=:), allocatable:: fname
        
        ! Check cgi for time interval 3.
        fname = r2s_out // ijk2str('/cgi', ".", ijk)
        ! TODO actually read cgi file and check if it is complete.
        inquire(file=fname, exist=cgi_exists)
        return
    end function check_cgi

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

        fname = r2s_scratch // ijk2str('/r2s_w/cgi', ".", ijk)
        call report_file_name('Writing gamma intensity to ', fname)
        do l = 1, nt
            ! Write when at least one of fine mesh elements has non-zero gamma intensity
            if (sum(cgi(:, :, :, :, l)) .gt. 0.0) then
                open(pr_scw,   file=fname)
                do i = i1, i2
                do j = j1, j2
                do k = k1, k2
                    if (sum(cgi(i, j, k, :, l)) .gt. 0.0) then 
                        write(pr_scw,   '(4i10, 1p<ne>e11.4)') l, i, j, k, (cgi(i, j, k, m, l), m = 1, ne)
                    end if
                end do
                end do
                end do
            end if
        end do
        close(pr_scw)
        return
    end subroutine write_cgi

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

        if (.not. dryrun) write(pr_log, 100) i, j, k, ic, jc, kc, nmats, f
        100 format ("Fine mesh element: ", 3i6, " (in coarse mesh element ", 3i5, ")  contains", i4, " materials at flux ", 1pe12.4)

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
            fname = r2s_scratch // ijk2str('/r2s_w/mat.content', ".", (/i, j, k/))
            call report_file_name('Writing composition to ', fname)
            open(pr_scw, file=fname)
            tnon = 0
            do i_c = 1, size(cind)
                if (mind(i_c) .gt. 0) then 
                    call write_mat_fispact(pr_scw,                 &  ! unit
                                           mind(i_c),              &  ! material index
                                           evol*frac(i_c)*conc(i_c),  &  ! amount of material in cell i_c
                                           non)                       ! out, number of entries in this material
                    tnon = tnon + non
                end if
            end do
            close(pr_scw)
            fname = r2s_scratch // ijk2str('/r2s_w/mat.title', '.', (/i, j, k/))
            call report_file_name('Writing mat title to', fname)
            open(pr_scw, file=fname)
            write(pr_scw, *) 'DENSITY', sum(dens*frac)
            write(pr_scw, *) 'FUEL', tnon
            close(pr_scw)

            ! Write irradiation scenario
            ! TODO: read footer only once, broadcast to all processes
            open(pr_scr, file=r2s_scratch // '/r2s_r/footer')  ! irradiation scenario template
            fname = r2s_scratch // ijk2str('/r2s_w/scenario', ".", (/i, j, k/))
            call report_file_name('Writing scenario to', fname)
            open(pr_scw, file=fname)
            flux_normalized = .FALSE.
            do while(.not. eof(pr_scr))
                l = read_line(pr_scr)
                read(l, *, iostat=ist) kw, v
                if (ist .eq. 0 .and. to_lower(kw) .eq. "flux") then
                    ! This line contain FLUX keyword
                    ist = index(l, kw)
                    write(pr_scw, '(a, 1pe)') l(:ist+4), v*f
                    flux_normalized = .TRUE.
                else
                    write(pr_scw, '(a)') l
                end if
            end do
            close(pr_scr)
            if (flux_normalized) then
                write(pr_scw, '("<< Flux normalized by", 1pe12.5, " >>" )') f 
            end if
            close(pr_scw)
            fname = r2s_inventory_s1 // ijk2str(" ", " ", (/i, j, k, ic, jc, kc/))
            call pr_runp(fname, ist, pr_log) 
            if (ist .eq. 0) then
                istat = 1
                call read_tab4((/i, j, k/), gi)
                ! Density in inventory input is the mean density in the fine mesh element
                gi = gi * evol

                ! Clean inventory after tab4 has been read
                fname = r2s_inventory_s2 // ijk2str(" ", " ", (/i, j, k/))
                call pr_runp(fname, ist, pr_log) 
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

    subroutine run_condense()
        implicit none
        integer:: ist

        ! Condense does not require any case-specific into. Simply
        ! call the script
        call pr_run1(r2s_condense_s1, ist)
        call pr_run1(r2s_condense_s2, ist)

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
        fname = r2s_scratch // ijk2str('/r2s_w/fluxes', ".", (/i, j, k/))
        call report_file_name('Writing spectrum to', fname)
        open(pr_scw, file=fname)
        write(pr_scw, *) (vc(l, i, j, k, 1), l = n-1, 1, -1)  ! vc contains also the total value, which is not needed here
        write(pr_scw, *) 1.0          ! First wall loading. Does not matter (?)
        write(pr_scw, *) 'Spectrum for ', i, j, k, ' written' 
        close(pr_scw)

        ! Prepare the script command:
        fname = r2s_collapse_s1 // ijk2str(" ", " ", (/i, j, k/))
        ! Call external script to create the working folder
        call pr_runp(fname, ist, pr_log) 
        return

    end subroutine run_collapse

    subroutine run_collapse_clean(i, j, k)      
        ! prepare folder to start fispact collapse with spectrum s
        implicit none
        integer, intent(in):: i, j, k  ! indices of the coarse mesh where spectrum is collapsed
        ! local vars
        integer:: ist
        character (len=:), allocatable:: fname

        ! Prepare the script command:
        fname = r2s_collapse_s2 // ijk2str(" ", " ", (/i, j, k/))
        ! Call external script to create the working folder
        call pr_runp(fname, ist, pr_log) 
        return

    end subroutine run_collapse_clean


end module fispactdriver      
