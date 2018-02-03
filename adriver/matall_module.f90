! Module representing material allocation in the mesh
module matall
    use r2senv
    use proc
    use gen, only: get_line
    use meshtal, only: xf, yf, zf, get_mesh_volume

    implicit none
    integer, allocatable::  &
        ma_ci(:),  &     ! Cell indices for all mesh elements
        ma_ch(:),  &     ! number of hits in cells for all mesh elements
        ma_3(:, :, :), & ! Mapping (i, j, k) -> l, where ma_i(l) -- address in ma_ci and ma_hc
        ma_i(:)          ! ma_i(ma_3(i, j, k)) -- address of element (i, j, k) in ma_ci and ma_hc

    integer, allocatable:: & 
        cnam(:), cmat(:),  &    ! cnam(i), cmat(:) -- Cell name and filling material index for cell with index i
        mnam(:)                 ! mnam(i) -- material name of material with index i
    real, allocatable::    &
        cden(:), ccon(:)        ! cden(i), ccon(i) -- cell density and concentration for cell with index i

    public get_mat_allocation, get_mcnp_names, get_ma_adress

    contains
        subroutine get_mat_allocation()
            implicit none

            if (pr_id .eq. 0) then
                call print_log('Reading ' // r2s_matallocation // ' ... ')
                call p_read_matall(r2s_matallocation)
            end if

            call broadcast_1i(ma_ci, 1)
            call broadcast_1i(ma_ch, 1)
            call broadcast_1i(ma_i, 0)
            call broadcast_3i(ma_3)

            call p_write_matall_log

            ! Print out cell and material masses and volumes
            call print_log('Checking fne_mesh_content ... ')
            call check_fine_mesh_content
        end subroutine get_mat_allocation

        subroutine get_mcnp_names()
            implicit none

            if (pr_id .eq. 0) then
                call print_log('Reading ' // r2s_cellsmaterials // ' ... ')
                call p_read_cmi(r2s_cellsmaterials)
            end if

            call  broadcast_1i(cnam, 1)
            call  broadcast_1i(cmat, 1)
            call  broadcast_1i(mnam, 0)
            call  broadcast_1r(cden)
            call  broadcast_1r(ccon)

            call p_write_cmi_log
        end subroutine get_mcnp_names

        subroutine get_ma_adress(i, j, k, i1, i2)
            ! For mesh element i, j, k return start and end indices, i1 and i2,  in ma_* arrays
            implicit none
            integer, intent(in):: i, j, k
            integer, intent(out):: i1, i2
            
            ! local vars
            integer:: l
            l = ma_3(i, j, k)
            if (l .gt. 0) then 
                i1 = ma_i(l - 1) + 1
                i2 = ma_i(l)
            else
                i1 = 2
                i2 = 1
            end if
            return
        end subroutine get_ma_adress

        subroutine get_ma_properties(i, j, k, cindices, fractions, mindices, densities, concentrations, vol)
            ! For mesh element i, j, k, return arrays specifying cells in it.
            implicit none
            integer, intent(in):: i, j, k
            integer, allocatable, intent(out):: cindices(:), mindices(:)
            real, allocatable, intent(out):: fractions(:), densities(:), concentrations(:)
            real, intent(out):: vol

            ! local vars
            integer:: i1, i2, l, n
            call get_ma_adress(i, j, k, i1, i2)
            n = i2 - i1 + 1
            allocate(cindices(n))
            allocate(mindices(n))
            allocate(fractions(n))
            allocate(densities(n))
            allocate(concentrations(n))

            cindices(:) = ma_ci(i1: i2)
            mindices(:) = cmat(cindices)
            fractions(:) = float(ma_ch(i1: i2)) 
            if (sum(fractions) .gt. 0) fractions = fractions / sum(fractions)
            densities(:) = cden(cindices)
            concentrations(:) = ccon(cindices)
            vol = get_mesh_volume(i, j, k)
            return 
        end subroutine get_ma_properties

        subroutine p_read_cmi(fname)
            ! Read information from the print table CMI
            implicit none
            character (len=*), intent(in):: fname  ! filename to read

            ! local variables
            integer:: ci, cn, i, mi, mn
            real:: cd, cc
            integer:: mimax, cimax  ! maximal mateial and cell indices. Used to get number of materials and cells
            logical:: e
            character (len=30):: l

            inquire(file=fname, exist=e)
            if (.not. e) then
                write(*, *) 'File with CMI does not exist:', fname
                stop
            end if

            ! First run: get line numbers and number of lines
            open(pr_scr, file=fname)
            l = get_line(pr_scr, 'Cell_index', 30)
            mimax = 0
            cimax = 0
            do while (.not. eof(pr_scr))
                read(pr_scr, *) ci, cn, i, i, mi, mn, cd, cc
                if (mimax .lt. mi) mimax = mi
                if (cimax .lt. ci) cimax = ci
            end do
            allocate(cnam(cimax))
            allocate(cmat(cimax))
            allocate(cden(cimax))
            allocate(ccon(cimax))
            allocate(mnam(0: mimax))

            rewind(pr_scr)
            l = get_line(pr_scr, 'Cell_index', 30)
            do while (.not. eof(pr_scr))
                read(pr_scr, *) ci, cn, i, i, mi, mn, cd, cc
                cnam(ci) = cn
                cmat(ci) = mi
                cden(ci) = cd
                ccon(ci) = cc
                mnam(mi) = mn
            end do
            close(pr_scr)
            return
        end subroutine p_read_cmi

        subroutine p_write_cmi_log
            implicit none
            call print_log('Cell/matetial indices, names and densities:')

            call print_array_i("Cell names          ", cnam, pr_log, 50)
            call print_array_i("Cell mat. indices   ", cmat, pr_log, 50)
            call print_array_r("Cell densities      ", cden, pr_log, 50)
            call print_array_r("Cell concentrations ", ccon, pr_log, 50)
            call print_array_i("Material names      ", mnam, pr_log, 50)
        end subroutine

        subroutine print_array_i(title, a, funit, nmax)
            implicit none
            character (len=*), intent(in):: title
            integer, intent(in):: a(:)
            integer, intent(in):: funit, nmax

            integer:: n, n1, n2, i
            integer, allocatable:: ind(:)

            n = size(a)
            allocate(ind(n))
            do i = 1, n
                ind(i) = i
            end do
            if (n .le. nmax) then
                write(funit, '(a, ": ", 4x, <n>i12)') title, a
            else
                n1 = nmax / 2 - 1
                n2 = n - n1 + 1
                write(funit, '(a, ": ", 4x, <n1>i12, " ... ", <n1>i12)') title, a(1:n1), a(n2:n)
            end if
            return
        end subroutine print_array_i

        subroutine print_array_r(title, a, funit, nmax)
            implicit none
            character (len=*), intent(in):: title
            real, intent(in):: a(:)
            integer, intent(in):: funit, nmax

            integer:: n, n1, n2, i
            integer, allocatable:: ind(:)

            n = size(a)
            allocate(ind(n))
            do i = 1, n
                ind(i) = i
            end do
            if (n .le. nmax) then
                write(funit, '(a, ": ", 4x, 1p<n>e12.4)') title, a
            else
                n1 = nmax / 2 - 1
                n2 = n - n1 + 1
                write(funit, '(a, ": ", 4x, 1p<n1>e12.4, " ... ", 1p<n1>e12.4)') title, a(1:n1), a(n2:n)
            end if
            return
        end subroutine print_array_r

        subroutine p_write_matall_log
            ! Dump material allocation arrays to the log files, to ckeck that all processes
            ! have the same set of data
            implicit none
            integer:: i, j, k, i1, i2, n, l
            integer:: di, dj, dk, ni, nj, nk

            ! Print out every di-, dj- and dk-th element to keep output fast and small
            ni = size(ma_3(:, 1, 1))
            nj = size(ma_3(1, :, 1))
            nk = size(ma_3(1, 1, :))
            di = 1
            dj = 1
            dk = 1
            if (ni .gt. 10) then
                di = ni / 10
            end if
            if (nj .gt. 10) then
                dj = nj / 10
            end if
            if (nk .gt. 10) then
                dk = nk / 10
            end if

            write(pr_log, *) 'Material allocation:'
            do i = 1, ni, di
                do j = 1, nj, dj
                    do k = 1, nk, dk
                        l = ma_3(i, j, k)
                        if (l .gt. 0) then
                            i1 = ma_i(l-1) + 1
                            i2 = ma_i(l)
                            n = i2 - i1 + 1
                            write(pr_log, '(3i5, \)') i, j, k
                            write(pr_log, '(<n>i9)') ma_ci(i1: i2)
                            write(pr_log, '(15x, <n>i9)') ma_ch(i1: i2)
                        end if
                    end do
                end do
            end do
        end subroutine p_write_matall_log

        subroutine p_read_matall(fname)
            ! Read fine_mesh_content file. It contains only mesh elements with non-void
            ! cells. Therefore, the number of mesh elements can be considerably smaller
            ! than the nuber of elements in the whole fine mesh.
            implicit none
            character (len=*), intent(in):: fname

            ! local vars
            integer:: l, m, i, j, k, n
            real:: xc, yc, zc
            integer:: dum(301)

            ! TODO consider case when fname does not exist
            ! a has the same form as the meshtally. Its size can be deduced from xb, yb and zb

            ! First run: get number of non-void elements and max number of cells per element
            l = 0    ! 1-d index of the mesh element
            m = 0    ! Number of entries in the whole file
            open(pr_scr, file=fname)
            do while (.not. eof(pr_scr))
                read(pr_scr, *) i, j, k, xc, yc, zc, n
                l = l + 1
                m = m + n
            end do
            allocate(ma_3(size(xf) - 1, size(yf) - 1, size(zf) - 1))
            allocate(ma_ci(m))
            allocate(ma_ch(m))
            allocate(ma_i(0: l))
            ma_3 = 0
            ma_i = 0
            ma_ci = 0
            ma_ch = 0

            ! Second run: read cell indices and hits
            rewind(pr_scr)
            l = 0
            do while(.not. eof(pr_scr))
                read(pr_scr, *) i, j, k, xc, yc, zc, n, dum(1:3*n + 1)
                l = l + 1
                ma_3(i, j, k) = l
                ma_i(l) = n + ma_i(l - 1)
                ma_ci(ma_i(l-1)+1: ma_i(l)) = dum(2:3*n+1:3)
                ma_ch(ma_i(l-1)+1: ma_i(l)) = dum(3:3*n+1:3)
            end do
            close(pr_scr)
            return
        end subroutine p_read_matall

        subroutine check_fine_mesh_content
            ! Compute total masses of materials and cells from the fine_mesh_content, mesh geometry, 
            ! cell materials and cell densities
            implicit none

            ! local vars
            real:: v ! mesh element volume
            integer:: &
                i, j, k, ic, &  ! loop variables
                i1, i2, l, &
                cindex, &       ! cell index
                mindex, &       ! material index
                cimin, cimax, & ! range of cell indices
                mimin, mimax    ! range of material indices
            real:: cfract, &  ! vol.fraction of cell in the fine mesh element
                   cdenst, &  ! cell density
                   thits      ! Total number of hits to material in the fine mesh element (converted from integer)

            real, allocatable:: mmass(:, :), &   ! mass and volume of materials
                                cmass(:, :)      ! mass and volume of cells


            ! Get the range of cell indices in the fine_mesh_content
            cimin = -1  ! minimal cell index
            cimax = -1  ! maximal cell index
            mimin = -1  ! minimal material index
            mimax = -1  ! maximal material index
            do i = 1, size(ma_3(:, 1, 1))
                do j = 1, size(ma_3(1, :, 1))
                    do k = 1, size(ma_3(1, 1, :))
                        call get_ma_adress(i, j, k, i1, i2)
                        do ic = i1, i2
                            cindex = ma_ci(ic) 
                            mindex = cmat(cindex)
                            if (mindex .gt. 0) then
                                if (cindex .gt. cimax) cimax = cindex
                                if (mindex .gt. mimax) mimax = mindex
                                if (cindex .lt. cimin .or. cimin .eq. -1) cimin = cindex
                                if (mindex .lt. mimin .or. mimin .eq. -1) mimin = mindex
                            end if
                        end do
                    end do
                end do
            end do
            call print_log('Range of cell and material indices, found in the mesh:')
            write(pr_log, *) cimin, cimax
            write(pr_log, *) mimin, mimax

            allocate(mmass(mimin:mimax, 2))
            allocate(cmass(cimin:cimax, 2))

            mmass = 0.0
            cmass = 0.0

            do i = 1, size(ma_3(:, 1, 1))
                do j = 1, size(ma_3(1, :, 1))
                    do k = 1, size(ma_3(1, 1, :))
                        ! fine mesh element volume
                        v = get_mesh_volume(i, j, k) 
                        call get_ma_adress(i, j, k, i1, i2)
                        thits = float(sum(ma_ch(i1: i2)))
                        do ic = i1, i2 
                            cindex = ma_ci(ic)      ! Cell index
                            mindex = cmat(cindex)   ! Material index
                            if (mindex .gt. 0) then
                                ! Compute volumes and masses only of non-void cells. Voided cells
                                ! are not represented in the fine_mesh_content file completely.
                                cfract = ma_ch(ic) / thits              ! vol.frac of the cell
                                cdenst = cden(cindex)                   ! cell density

                                cmass(cindex, 2) = cmass(cindex, 2) + v * cfract           ! cell volume
                                cmass(cindex, 1) = cmass(cindex, 1) + v * cfract * cdenst  ! cell mass
                                mmass(mindex, 2) = mmass(mindex, 2) + v * cfract           ! material volume
                                mmass(mindex, 1) = mmass(mindex, 1) + v * cfract * cdenst  ! material mass
                            end if
                        end do
                    end do
                end do
            end do

            ! print out
            write(pr_log, '(3a12)') 'Cell_index', 'Cell_vol', 'Cell_mass'
            do i = cimin, cimax
                if (cmass(i, 2) .gt. 0) write(pr_log, '(i12, 1p2e12.5)') i, cmass(i, 2), cmass(i, 1)
            end do
            write(pr_log, '(3a12)') 'Mat_index', 'Mat_vol', 'Mat_mass'
            do i = mimin, mimax
                if (mmass(i, 2) .gt. 0) write(pr_log, '(i12, 1p2e12.5)') i, mmass(i, 2), mmass(i, 1)
            end do
        end subroutine check_fine_mesh_content

end module      
