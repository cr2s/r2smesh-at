program adriver
    use proc  ! mpi module is available from here
    use matcomp, only: get_mat
    use gen, only: print_log, ijk2str
    use meshtal
    use matall
    use fispactdriver, only: run_condense, run_collapse, run_inventory, read_tab4, write_gi
    use r2senv, only: env_init
    implicit none

    integer:: i_c, j_c, k_c ! indices for coarse mesh elements
    integer:: nfir, &  ! number of fispact inventory runs
              nfir1, & ! number of fispact inventory runs in coarse mesh element
              nfcr     ! number of fispact collapse runs

    integer:: cmax  ! maximal number of cells in fine mesh element

    ! variables for mpi
    integer, allocatable:: pstat(:)  ! Status of slave processes
    integer, parameter:: &
        nb=4,            &   ! length of the buffer between the master and slaves
        tag1=1,          &   ! tag to continue job
        tag2=2,          &   ! tag telling that no more jobs
        master=0             ! sender or recepient
    integer:: buf(nb)  ! Message buffer between the master and slaves

    real:: nfirtot   ! Total number of inventory runs (real to compute %)

    ! Get all necessary environment variables
    call env_init


    call pr_initialize

    nfir = 0  ! number of inventory runs

    ! Initialization
    call initialize()
    ! Compute number of inventory runs
    call print_log('Computing number of inventory runs ...')
    do i_c = 1, size(xc) - 1
        do j_c = 1, size(yc) - 1
            do k_c = 1, size(zc) - 1 
                call process_coarse_element(i_c, j_c, k_c, nfir1, .TRUE.)
                if (nfir1 .gt. 0) nfcr = nfcr + 1
                nfir = nfir + nfir1
            end do
        end do
    end do
    nfirtot = float(nfir) * 1e-2
    call print_log(ijk2str('Number of collapse/inventory runs:', (/nfcr, nfir/)))

    nfir = 0  ! number of inventory runs
    nfcr = 0  ! Number of collapse runs (at most once per coarse element)

    if (pr_np .eq. 1) then 
        ! Only one process, do sequentially
        call run_condense()
        do i_c = 1, size(xc) - 1
            do j_c = 1, size(yc) - 1
                do k_c = 1, size(zc) - 1 
                    nfir1 = nfir
                    call process_coarse_element(i_c, j_c, k_c, nfir1, .FALSE.)
                    if (nfir1 .gt. 0) nfcr = nfcr + 1
                    nfir = nfir + nfir1
                end do
            end do
        end do
        write(pr_log, *) 'Number of  collapse runs:', nfcr
        write(pr_log, *) 'Number of inventory runs:', nfir
    else
        if (pr_id .eq. 0) then
            ! Code for master
            call run_condense()
            call MPI_Barrier(MPI_COMM_WORLD, pr_er)
            allocate(pstat(pr_np - 1))
            pstat = 0
            do i_c = 1, size(xc) - 1
                do j_c = 1, size(yc) - 1
                    do k_c = 1, size(zc) - 1 
                        ! Get the next free process and supply it with the next job
                        call MPI_Recv(buf, nb, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, pr_st, pr_er)   ! Get I_am_ready message

                        ! In the buf received from slave -- number nfir1
                        nfir1 = buf(1)
                        if (nfir1 .gt. 0) nfcr = nfcr + 1
                        nfir = nfir + nfir1
                        ! In the buffer sent to a slave -- indices of the coarse mesh element, and number of already completed fispact runs (for logging)
                        buf(1:nb) = (/i_c, j_c, k_c, nfir/)
                        call MPI_Send(buf, nb, MPI_INTEGER, pr_st(MPI_SOURCE), tag1, MPI_COMM_WORLD, pr_er)  ! Send work_definition message
                        pstat(pr_st(MPI_SOURCE)) = pstat(pr_st(MPI_SOURCE)) + 1  ! compute the number of jobs each processor receives 
                    end do
                end do
            end do
            write(pr_log, *) 'Number of jobs accomplshed by each slave process:'
            write(pr_log, *) pstat
            ! When all coarse meshes are ready, inform all slaves that there is no work
            do while (count(pstat > 0) .gt. 0)
                call MPI_Recv(buf, nb, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, pr_st, pr_er)   ! Get I_am_ready message
                nfir1 = buf(1)
                if (nfir1 .gt. 0) nfcr = nfcr + 1
                nfir = nfir + nfir1
                buf = 0.0
                call MPI_Send(buf, nb, MPI_INTEGER, pr_st(MPI_SOURCE), tag2, MPI_COMM_WORLD, pr_er)                 ! Send end_of_work message
                write(pr_log, *) 'Sent end-of-job message to process', pr_st(MPI_SOURCE)
                pstat(pr_st(MPI_SOURCE)) = 0
            end do
            write(pr_log, *) 'Number of  collapse runs:', nfcr
            write(pr_log, *) 'Number of inventory runs:', nfir
        else
            ! Code for slaves
            call MPI_Barrier(MPI_COMM_WORLD, pr_er)
            ! Send "I'm ready" signal to the master
            buf = 0
            call MPI_Send(buf, nb, MPI_INTEGER, master, tag1, MPI_COMM_WORLD, pr_er)    ! Send I_am_ready message
            call MPI_Recv(buf, nb, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, pr_st, pr_er)    ! Get work_definition message
            do while (pr_st(MPI_TAG) .eq. tag1)
                i_c = buf(1)
                j_c = buf(2)
                k_c = buf(3)
                nfir1 =  buf(4)
                call process_coarse_element(i_c, j_c, k_c, nfir1, .FALSE.)
                buf(1) = nfir1
                buf(2:nb) = 0
                call MPI_Send(buf, nb, MPI_INTEGER, master, tag1, MPI_COMM_WORLD, pr_er)    ! Send I_am_ready message
                call MPI_Recv(buf, nb, MPI_INTEGER, master, MPI_ANY_TAG, MPI_COMM_WORLD, pr_st, pr_er)    ! Get work_definition or end_of_work message
            end do
        end if
        call print_log('Process completed')
    end if
    call print_log('Program completed')
    call pr_finalize()

    contains

    subroutine initialize()
        implicit none
        ! local vars
        integer:: nf, nc  ! meshtal numbers for the fine and coarse meshes
        integer, allocatable:: mn(:)  ! material names

        integer:: n

        ! Read material compositions
        call get_mat('mat_table')

        call get_fluxes

        call get_mcnp_names('cmi_table')
        call get_mat_allocation('fine_mesh_content')

        ! Loop for fispact runs. 
        call print_log('Loop over coarse mesh elements ... ')
        nfir = 0  
        nfcr = 0  
        return
    end subroutine initialize

    subroutine process_coarse_element(ic, jc, kc, nfr, dryrun)
        implicit none
        integer, intent(in):: ic, jc, kc
        integer, intent(inout):: nfr ! number of fispact inventory runs in the coarse mesh element
        logical, intent(in):: dryrun ! If TRUE, fispact not actually started, only numer of runs is computed

        ! local variables
        logical collapse_completed
        integer:: i, j, k  ! indices for fine mesh elements
        integer:: ijkf(4), ijkc(3)
        integer:: if1, if2, jf1, jf2, kf1, kf2  ! index bounds of fine mesh elements in a coarse one
        integer:: nmf      ! number of materials in fine mesh element

        real, allocatable:: fmgi(:, :) ! fine mesh gamma intensity for each time step
        real, allocatable:: t4gi(:, :) ! Current tab4 results for each time step
        real:: volc
        integer:: i_m ! index for material in fine mesh element
        integer:: nfrf
        character (len=200):: msg

        integer, allocatable:: cind(:), mind(:)
        real, allocatable:: frac(:), dens(:), conc(:)
        real:: v


        ! Write log
        if (.not. dryrun) then 
            write(msg, '("Starting coarse element ", 3i5, ". Completed fispact inventory runs: ", i6, f9.4" %")') ic, jc, kc, nfr, float(nfr)/nfirtot
            call print_log(trim(msg))
        end if

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
        ijkc = (/ic, jc, kc/)
        if (.not. dryrun) then
            write(pr_log, '(a, 3(5x, 2i5))') 'Fine mesh indices:', if1, if2-1, jf1, jf2-1, kf1, kf2-1
        end if
        nfr = 0  ! number of fispact runs for the current coarse mesh element
        do i = if1, if2 - 1
            do j = jf1, jf2 - 1
                do k = kf1, kf2 - 1
                    if (.not. dryrun) write(pr_log, *) 'Fine mesh element', i, j, k
                    ! In mcnp, the nmt array containing material names starts from 0, and initially set
                    ! to 0. Thus I assume that the material index 0 always corresponds to material 0 and
                    ! can be used to identify void cells: cell with index i is void when mat(i) is 0.

                    ! get cell properties for the current mesh element
                    call get_ma_properties(i, j, k, cind, frac, mind, dens, conc, v)
                    nmf = count(mind .gt. 0)  ! number of non-zero materials in the current fine mesh
                    ! run inventory only when non-zero flux
                    if (vf(1, i, j, k, 1) .eq. 0) then 
                        ! Report that for a non-void fine mesh element a zero flux detected
                        if (.not. dryrun .and. nmf .gt. 0) write(pr_log, *) 'WARNING: zero flux in fine mesh element', i, j, k
                        cycle
                    end if
                    nfrf = 0  ! number of fispact inventory runs for the current fine mesh element
                    if (nmf .gt. 0) then 
                        if (.not. dryrun) then
                            write(pr_log, *) 'Number of non-zero materials:', nmf
                            write(pr_log, *) '                Cell indices:', cind 
                            write(pr_log, *) '            Material indices:', mind
                        end if
                        if (.not. collapse_completed) then
                            ! Get spectrum: run fispact collapse once for the current coarse mesh element
                            if (.not. dryrun) then
                                call run_collapse(vc(:, ic, jc, kc, 1), ijkc)
                            end if
                            collapse_completed = .TRUE.
                        end if
                        ! Run inventory calculations for all non-void cells in the fine mesh element
                        do i_m = 1, size(cind) 
                            if (mind(i_m) .gt. 0) then   ! run fispact only for non-void cells
                                ijkf = (/i, j, k, cind(i_m)/)
                                nfrf = nfrf + 1
                                if (.not. dryrun) then
                                    volc = v * frac(i_m)                           ! volume of the cell in the fine mesh element
                                    call run_inventory(vf(1, i, j, k, 1),       & ! flux intensity
                                                       mind(i_m),               & ! material index, for composition 
                                                       dens(i_m),               & ! Cell density, g/cm3, passed to "DENSITY" keyword
                                                       conc(i_m)*volc*1e24,     & ! Total amount of material. tab1 and tab2 are normalized to this value.
                                                       ijkf,                    & ! Indides identifying fine mesh element and the cell
                                                       ijkc)                      ! Indices identifying correspondent collapse run
                                    if (nfrf .eq. 1) then
                                        ! After the first non-void cell, read tab4 directly into fmgi
                                        call read_tab4(ijkf, fmgi)
                                        fmgi = fmgi * volc   ! tab4 normalized per 1 cm3. Multiply by cell volume to get 1/s
                                    else
                                        ! read tab4 into temporary array and add to fmgi
                                        call read_tab4(ijkf, t4gi)
                                        fmgi = fmgi + t4gi * volc 
                                    end if
                                end if
                            end if
                        end do
                        ! Write decay gamma source for current fine mesh element
                        if (.not. dryrun) call write_gi(ijkf, fmgi)

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
                end do
            end do
        end do
        return

    end subroutine process_coarse_element
end program adriver


