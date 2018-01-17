program meshtal_test
    use mpi
    use gen, only: print_log, get_cmi, mcomp, get_mat, get_fine_mesh_content, cvf, ijk2str, &
                   check_fine_mesh_content
    use meshtal, only: get_next_tally, write_tally_log
    use fispactdriver, only: run_condense, run_collapse, run_inventory, read_tab4, write_gi
    use r2senv, only: env_init
    ! use iounits, only: io_co, io_po, io_pl, io_puo
    implicit none

    real, allocatable:: xf(:), yf(:), zf(:), ef(:), vf(:, :, :, :, :), & ! fine mesh
                        xc(:), yc(:), zc(:), ec(:), vc(:, :, :, :, :)    ! coarse mesh

    integer, allocatable:: cnm(:, :)     ! cell names and material indices
    real, allocatable:: rho(:, :)        ! cell density and conc

    type(mcomp), allocatable:: mats(:)
    type(cvf), allocatable:: a(:, :, :)

    integer:: i_c, j_c, k_c ! indices for coarse mesh elements
    integer:: nfir, &  ! number of fispact inventory runs
              nfir1, & ! number of fispact inventory runs in coarse mesh element
              nfcr     ! number of fispact collapse runs

    integer:: cmax  ! maximal number of cells in fine mesh element

    ! variables for mpi
    integer:: ierr, np, pid, st(MPI_STATUS_SIZE)
    integer, allocatable:: pstat(:)  ! Status of slave processes
    integer, parameter:: &
        nb=4,            &   ! length of the buffer between the master and slaves
        tag1=1,          &   ! tag to continue job
        tag2=2,          &   ! tag telling that no more jobs
        master=0             ! sender or recepient
    integer:: buf(nb)  ! Message buffer between the master and slaves

    integer:: nc  ! current coarse mesh element
    real:: nctot, &  ! Total number of coarse mesh elements (real to compute %)
           nfirtot   ! Total number of inventory runs (real to compute %)

    character (len=200):: message

    ! Get all necessary environment variables
    call env_init

    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD, np, ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, pid, ierr)

    nfir = 0  ! number of inventory runs

    ! Initialization
    call initialize(pid)
    ! Compute number of inventory runs
    call print_log('Computing number of inventory runs ...')
    do i_c = 1, size(xc) - 1
        do j_c = 1, size(yc) - 1
            do k_c = 1, size(zc) - 1 
                nc = nc + 1
                call process_coarse_element(i_c, j_c, k_c, pid, nfir1, .TRUE.)
                if (nfir1 .gt. 0) nfcr = nfcr + 1
                nfir = nfir + nfir1
            end do
        end do
    end do
    nfirtot = float(nfir) * 1e-2
    write(message, '(a, i)') 'Number of inventory runs in this case:', nfir
    call print_log(trim(message))

    nfir = 0  ! number of inventory runs
    nctot = float((size(xc) - 1)*(size(yc) - 1)*(size(zc) - 1)) * 1e-2 

    if (np .eq. 1) then 
        ! Only one process, do sequentially
        call run_condense()
        nc = 0
        do i_c = 1, size(xc) - 1
            do j_c = 1, size(yc) - 1
                do k_c = 1, size(zc) - 1 
                    nc = nc + 1
                    ! All details of coarse mesh processing are in a separate subroutine
                    write(message, '("Process ", i3, "starts coarse ", 3i4, f9.4" %. Completed inventory runs:", i6, f9.4" %")') st(MPI_SOURCE), i_c, j_c, k_c, float(nc)/nctot, nfir, float(nfir)/nfirtot
                    call print_log(trim(message))
                    call process_coarse_element(i_c, j_c, k_c, pid, nfir1, .FALSE.)
                    if (nfir1 .gt. 0) nfcr = nfcr + 1
                    nfir = nfir + nfir1
                end do
            end do
        end do
        write(*, *) 'Number of  collapse runs:', nfcr
        write(*, *) 'Number of inventory runs:', nfir
    else
        if (pid .eq. 0) then
            ! Code for master
            call run_condense()
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
            allocate(pstat(np - 1))
            pstat = 0
            nc = 0
            do i_c = 1, size(xc) - 1
                do j_c = 1, size(yc) - 1
                    do k_c = 1, size(zc) - 1 
                        nc = nc + 1
                        ! Get the next free process and supply it with the next job
                        call MPI_Recv(buf, nb, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, st, ierr)   ! Get I_am_ready message
                        ! In the buf received from slave -- number nfir1
                        nfir1 = buf(1)
                        if (nfir1 .gt. 0) nfcr = nfcr + 1
                        nfir = nfir + nfir1
                        ! In the buffer sent to a slave -- indices of the coarse mesh element
                        buf(1:nb) = (/i_c, j_c, k_c, nc/)
                        write(message, '("Process ", i3, "starts coarse ", 3i4, f9.4" %. Completed inventory runs:", i6, f9.4" %")') st(MPI_SOURCE), i_c, j_c, k_c, float(nc)/nctot, nfir, float(nfir)/nfirtot
                        call print_log(trim(message))
                        call MPI_Send(buf, nb, MPI_INTEGER, st(MPI_SOURCE), tag1, MPI_COMM_WORLD, ierr)  ! Send work_definition message
                        pstat(st(MPI_SOURCE)) = pstat(st(MPI_SOURCE)) + 1  ! compute the number of jobs each processor receives 
                    end do
                end do
            end do
            write(*, *) 'Number of jobs accomplshed by each slave process:'
            write(*, *) pstat
            ! When all coarse meshes are ready, inform all slaves that there is no work
            do while (count(pstat > 0) .gt. 0)
                call MPI_Recv(buf, nb, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, st, ierr)   ! Get I_am_ready message
                call MPI_Send(0, 1, MPI_INTEGER, st(MPI_SOURCE), tag2, MPI_COMM_WORLD, ierr)                 ! Send end_of_work message
                write(*, *) 'Sent end-of-job message to process', st(MPI_SOURCE)
                pstat(st(MPI_SOURCE)) = 0
            end do
            write(*, *) 'Number of  collapse runs:', nfcr
            write(*, *) 'Number of inventory runs:', nfir
        else
            ! Code for slaves
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
            ! Send "I'm ready" signal to the master
            buf = 0
            call MPI_Send(buf, nb, MPI_INTEGER, master, tag1, MPI_COMM_WORLD, ierr)    ! Send I_am_ready message
            call MPI_Recv(buf, nb, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, st, ierr)    ! Get work_definition message
            do while (st(MPI_TAG) .eq. tag1)
                i_c = buf(1)
                j_c = buf(2)
                k_c = buf(3)
                nc =  buf(4)
                call process_coarse_element(i_c, j_c, k_c, pid, nfir1, .FALSE.)
                buf(1) = nfir1
                buf(2:nb) = 0
                call MPI_Send(buf, nb, MPI_INTEGER, master, tag1, MPI_COMM_WORLD, ierr)    ! Send I_am_ready message
                call MPI_Recv(buf, nb, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, st, ierr)    ! Get work_definition or end_of_work message
            end do
        end if
        call print_log(ijk2str('Completed pid: ', (/pid/)))
    end if
    call mpi_finalize(ierr)
    call print_log('Program completed')

    contains

    subroutine initialize(pid)
        implicit none
        integer, intent(in):: pid
        ! local vars
        integer:: nf, nc  ! meshtal numbers for the fine and coarse meshes
        integer, allocatable:: mn(:)  ! material names
        integer:: funit
        character (len=12):: cpid

        integer:: n

        write(cpid, '("Process", i4)'), pid
        funit = 1000 + pid  ! file unit for current process
        call print_log(cpid // 'Read meshtal.fine ... ')
        open(funit, file='meshtal.fine')
        call get_next_tally(funit, nf, xf, yf, zf, ef, vf)
        call write_tally_log(cpid // 'Fine meshtally:', nf, xf, yf, zf, ef, vf)
        close(funit)
        call print_log(cpid // 'Read meshtal.coarse ... ')
        open(funit, file='meshtal.coarse')
        call get_next_tally(funit, nc, xc, yc, zc, ec, vc)
        call write_tally_log(cpid // 'Coarse meshtally:', nc, xc, yc, zc, ec, vc)
        close(funit)

        ! write header for dgs files 
        if (pid .eq. 0) then
            open(1000 + pid, file='fine_mesh_def')
            write(1000 + pid, '(3i6)') size(xf), size(yf), size(zf)
            n = size(xf)
            write(1000 + pid, '(1p<n>e15.6)') xf
            n = size(yf)
            write(1000 + pid, '(1p<n>e15.6)') yf
            n = size(zf)
            write(1000 + pid, '(1p<n>e15.6)') zf
            close(1000 + pid)
        end if

        ! Get cell and material indices etc. from the CMI print table
        call print_log(cpid // 'Readnig CMI table from outp ... ')
        call get_cmi(funit, 'cmi_table', cnm, mn, rho)

        ! Read material compositions
        call print_log(cpid // 'Reading the print table FIS ... ')
        call get_mat(funit, 'mat_table', mats)
        write(*, *) cpid, 'There are ', size(mats), ' materials'

        ! Read fine_mesh_content
        call print_log(cpid // 'Reading fne_mesh_content ... ')
        call get_fine_mesh_content(funit, 'fine_mesh_content', xf, yf, zf, a, cmax)

        ! Print out cell and material masses and volumes
        call check_fine_mesh_content(a, xf, yf, zf, rho, cnm) 

        ! Loop for fispact runs. 
        call print_log(cpid // 'Loop over coarse mesh elements ... ')
        nfir = 0  
        nfcr = 0  
        return
    end subroutine initialize

    subroutine process_coarse_element(ic, jc, kc, pid, nfr, dryrun)
        implicit none
        integer, intent(in):: ic, jc, kc
        integer, intent(in):: pid         ! process id. To avoid reading from the same units.
        integer, intent(out):: nfr ! number of fispact inventory runs in the coarse mesh element
        logical, intent(in):: dryrun ! If TRUE, fispact not actually started, only numer of runs is computed

        ! local variables
        logical collapse_completed
        integer:: i, j, k  ! indices for fine mesh elements
        integer:: if1, if2, jf1, jf2, kf1, kf2  ! index bounds of fine mesh elements in a coarse one
        integer:: nmf      ! number of materials in fine mesh element
        character (len=:), allocatable:: fnamec, fnamef

        integer, parameter:: nts=150  ! max number of time steps to read from tab4 files
        real, allocatable:: fmgi(:, :) ! fine mesh gamma intensity for each time step
        real, allocatable:: t4gi(:, :) ! Current tab4 results for each time step
        integer:: fmci, fmmi  ! cell and mat. indices in the fine mesh
        real:: cc1, cc2
        integer:: i_m ! index for material in fine mesh element
        integer:: funit
        integer:: nfrf

        funit = 1000 + pid*2   ! *2 to let more than one unit number be used in subroutines
        ! Collapse should run only when there are materials in the coarse mesh element.
        ! THerefore, trigger only after having found any of them below.
        collapse_completed = .FALSE.

        ! Get subset of finemesh element in the current coarse mesh element
        if1 = count(xf .le. xc(ic))
        if2 = count(xf .le. xc(ic + 1))
        jf1 = count(yf .le. yc(jc))
        jf2 = count(yf .le. yc(jc + 1))
        kf1 = count(zf .le. zc(kc))
        kf2 = count(zf .le. zc(kc + 1))
        if (.not. dryrun) then
            write(*, *) 'Fine mesh index i:', if1, if2 - 1
            write(*, *) 'Fine mesh index j:', jf1, jf2 - 1
            write(*, *) 'Fine mesh index k:', kf1, kf2 - 1
        end if
        nfr = 0  ! number of fispact runs for the current coarse mesh element
        do i = if1, if2 - 1
            do j = jf1, jf2 - 1
                do k = kf1, kf2 - 1
                    if (.not. dryrun) write(*, *) 'Fine mesh element', i, j, k
                    ! In mcnp, the nmt array containing material names starts from 0, and initially set
                    ! to 0. Thus I assume that the material index 0 always corresponds to material 0 and
                    ! can be used to identify void cells: cell with index i is void when mat(i) is 0.
                    nmf = count(cnm(a(i, j, k)%c, 2) .gt. 0)  ! number of non-zero materials in the current fine mesh
                    if (vf(1, i, j, k, 1) .ne. 0) then 
                        ! run inventory only when non-zero flux
                        nfrf = 0  ! number of fispact inventory runs for the current fine mesh element
                        if (.not. dryrun) then
                            write(*, *) 'Number of non-zero materials:', nmf
                            write(*, *) '                Cell indices:', a(i, j, k)%c
                            write(*, *) '                   Materials:', cnm(a(i, j, k)%c, 2)
                        end if
                        if (nmf .gt. 0) then 
                            if (.not. collapse_completed) then
                                ! Get spectrum: run fispact collapse once for the current coarse mesh element
                                if (.not. dryrun) then
                                    fnamec = ijk2str('_col', (/ic, jc, kc/))
                                    call run_collapse(funit, vc(:, ic, jc, kc, 1), (/ic, jc, kc/))
                                end if
                                collapse_completed = .TRUE.
                            end if
                            ! Run inventory calculations for all non-void cells in the fine mesh element
                            cc1 = (xf(i+1) - xf(i)) * (yf(j+1) - yf(j)) * (zf(k+1) - zf(k))  ! fine mesh element volume
                            do i_m = 1, size(a(i, j, k)%c)
                                fmci = a(i, j, k)%c(i_m)  ! cell index
                                fmmi = cnm(fmci, 2)       ! material index
                                if (fmmi .gt. 0) then    ! run fispact only for non-void cells
                                    nfrf = nfrf + 1
                                    if (.not. dryrun) then
                                        fnamef = ijk2str('_inv', (/i, j, k, fmci/))
                                        cc2 = cc1 * a(i, j, k)%f(i_m) / sum(a(i, j, k)%f)  ! volume of the cell in the fine mesh element
                                        call run_inventory(funit,                   & ! file unit to be used for i/o
                                                           vf(1, i, j, k, 1),       & ! flux intensity
                                                           mats(fmmi),              & ! material composition 
                                                           rho(fmci, 1),            & ! Cell density, g/cm3, passed to "DENSITY" keyword
                                                           rho(fmci, 2)*cc2*1e24,   & ! Total amount of material. tab1 and tab2 are normalized to this value.
                                                           (/i, j, k, fmci/),       & ! Indides identifying fine mesh element and the cell
                                                           (/ic, jc, kc/))            ! Indices identifying correspondent collapse run
                                        if (nfrf .eq. 1) then
                                            ! After the first non-void cell, read tab4 directly into fmgi
                                            call read_tab4(funit, (/i, j, k, fmci/), fmgi)
                                            fmgi = fmgi * cc2   ! tab4 normalized per 1 cm3. Multiply by cell volume to get 1/s
                                        else
                                            ! read tab4 into temporary array and add to fmgi
                                            call read_tab4(funit, (/i, j, k, fmci/), t4gi)
                                            fmgi = fmgi + t4gi * cc2 
                                        end if
                                    end if
                                end if
                            end do
                            ! Write decay gamma source for current fine mesh element
                            if (.not. dryrun) call write_gi(funit, (/i, j, k/), fmgi)

                            ! Combine fispact results into single gamma spectrum for the current fine mesh element.  For this one needs
                            ! total amount of materials, i.e. vol. fractions, volume of the mesh element and cell densities.
                            !
                            ! TAB4 output files contain two columns: 1) gamma power in energy group, MeV/s, which is proportional to the
                            ! amount of material specified in the FUEL card, and 2) Gammas per group per cc and per second, 1/s/cc,
                            ! which depends on the dinsity given in the DENSITY card. For the decay gamma source, the number of gammas
                            ! per group is needed, i.e. 2-nd column. It must be normalized to the total volume of the irradiated
                            ! material in the fine mesh element. 
                            !
                            ! Here -- DGS format based on my DGS subroutine. What is needed: (1) fine mesh geometry, (2) gammas per
                            ! group in fine mesh element. 
                            ! 
                            ! Complete sourse cannot be formed untill all inventory runs are complete. However, at this point one can
                            ! write dgs entry for the current fine mesh element. THey can be later contatenated by system utilities.
                        end if
                        nfr = nfr + nfrf
                    else
                        ! Report that for a non-void fine mesh element a zero flux detected
                        if (.not. dryrun .and. nmf .gt. 0) write(*, *) 'WARNING: zero flux in fine mesh element', i, j, k
                    end if

                end do
            end do
        end do
        return

    end subroutine process_coarse_element
end program meshtal_test


