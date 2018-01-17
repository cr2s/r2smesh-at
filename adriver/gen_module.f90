! Module containing geenral functions
module gen
    ! Type to store material composition
    type mcomp
        integer, allocatable:: za(:)  ! za of the component
        real, allocatable:: f(:)      ! fraction of za
    end type mcomp

    ! Type to store cell volumetric fractions in a mesh element
    type cvf
        integer, allocatable:: f(:)  ! number of hits in the cell
        integer, allocatable:: c(:)  ! cell indices
    end type cvf

    private
    public:: getts, get_line, get_cmi, mcomp, get_mat, get_fine_mesh_content,  &
             cvf, read_line, print_log, ijk2str, to_lower, check_fine_mesh_content, &
             aiindex
    contains

        function ijk2str(p, i) result(s)
            character (len=*), intent(in):: p
            integer, intent(in):: i(:)
            character (len=:), allocatable:: s

            character (len=120):: d
            integer:: ll, n
            n = size(i)
            write(d, '(a, <n>("_", i0))') p, i
            ll = len(trim(d))
            allocate(character (len=ll):: s)
            s(:) = trim(d)
            return
        end function ijk2str

        subroutine print_log(message)
            character (len=*), intent(in):: message
            ! local vars
            character (len=19):: ts
            integer:: n
            ts = getts()
            n = len(message) + 1
            write(*, '(a11, a20, a<n>)') repeat('*', 10), ts, message
            return
        end subroutine print_log
        
        function getts() result(ts)
            character ts*19
            integer d(8)
            character (len = 12) rc(3)
            call DATE_AND_TIME(rc(1), rc(2), rc(3), d)
            write(ts, '(I4.4"/"I2.2"/"I2.2" "I2.2":"I2.2":"I2.2, a)')   &
                         d(1), d(2),  d(3),   d(5), d(6),  d(7)
            return
        end function getts

        subroutine get_fine_mesh_content(funit, fname, xb, yb, zb, a, nmax)
            ! Read fine_mesh_content file. It contains only mesh elements with non-void
            ! cells. Therefore, the number of mesh elements can be considerably smaller
            ! than the nuber of elements in the whole fine mesh.
            implicit none
            integer, intent(in):: funit
            character (len=*), intent(in):: fname
            real, intent(in) :: xb(:), yb(:), zb(:)  ! mesh boundary coordinates are used to get i, j and k for the fine mesh element
                                                     ! identified by its center coordinates
            type(cvf), allocatable, intent(out):: a(:, :, :)
            integer, intent(out):: nmax  ! maximal number of cells in fine mesh element

            ! local vars
            integer:: n, i, j, k
            integer:: ir, jr, kr  ! mesh element indices, as read from fine_mesh_content file
            real:: xc, yc, zc
            integer:: dum(301)
            ! character (len=:), allocatable:: line

            ! TODO consider case when fname does not exist
            ! a has the same form as the meshtally. Its size can be deduced from xb, yb and zb
            allocate(a(size(xb) - 1, size(yb) - 1, size(zb) - 1))

            nmax = 0
            open(funit, file=fname)
            do while ( .not. eof(funit))
                ! line = read_line(funit)
                ! read(line, *) ir, jr, kr, xc, yc, zc, n, dum(1:3*n + 1)
                read(funit, *) i, j, k, xc, yc, zc, n, dum(1:3*n + 1)
                if (nmax .lt. n) nmax = n
                allocate(a(i, j, k)%f(n))
                allocate(a(i, j, k)%c(n))
                a(i, j, k)%c(1:n) = dum(2:n*3:3)
                a(i, j, k)%f(1:n) = dum(3:n*3:3)
                ! ! extra check indices
                ! ir = count(xb .lt. xc)               
                ! jr = count(yb .lt. yc)               
                ! kr = count(zb .lt. zc)               
                ! if (i .ne. ir .or. j .ne. jr .or. k .ne. kr) then
                !     write(*, *) 'Inconsistent indices in fine_mesh_content:'
                !     write(*, *) i, j, k
                !     write(*, *) ir, jr, kr
                !     stop
                ! end if
                ! if (sum(a(i, j, k)%f) - dum(1) .ne. 0) then
                !     ! Sum of hits must be equal to the total number of hits
                !     write(*, *) 'in f.m. element', i, j, k
                !     write(*, *) '    cells:', a(i, j, k)%c
                !     write(*, *) '     hits:', a(i, j, k)%f
                !     write(*, *) ' tot.hits:', dum(1)
                !     write(*, *) 'Number of total hits differs from the sum in cells'
                !     stop
                ! end if 
            end do
            return

        end subroutine get_fine_mesh_content

        subroutine check_fine_mesh_content(a, xb, yb, zb, cden, cmat)
            ! Compute total masses of materials and cells from the fine_mesh_content, mesh geometry, 
            ! cell materials and cell densities
            implicit none
            type(cvf), intent(in):: a(:, :, :)
            real, intent(in):: xb(:), yb(:), zb(:)
            real, intent(in):: cden(:, :)
            integer, intent(in):: cmat(:, :)

            ! local vars
            real:: v ! mesh element volume
            integer:: &
                i, j, k, ic, i1, i2, &  ! loop variables
                cindex, &       ! cell index
                mindex          ! material index
            real:: cfract, &  ! vol.fraction of cell in the fine mesh element
                   cdenst, &  ! cell density
                   thits      ! Total number of hits to material in the fine mesh element (converted from integer)
            integer, parameter:: nmats=100000, &   ! number of different materials in the region covered by fine mesh
                                 ncels=100000      ! number of different cells     in the region covered by fine mesh
            integer:: imt(nmats), &  ! material index in order, as appears in fine_mesh_content
                      icl(ncels)     ! cell     index in order, as appears in fine_mesh_content
            real:: mmass(nmats, 2), &   ! mass and volume of materials
                   cmass(ncels, 2)      ! mass and volume of cells

            imt = -1
            icl = -1 
            mmass = 0.0
            cmass = 0.0
            do i = 1, size(a(:, 1, 1))
                do j = 1, size(a(1, :, 1))
                    do k = 1, size(a(1, 1, :))
                        ! fine mesh element volume
                        v = (xb(i+1) - xb(i)) * (yb(j+1) - yb(j)) * (zb(k+1) - zb(k))
                        thits = float(sum(a(i, j, k)%f))
                        do ic = 1, size(a(i, j, k)%c)
                            cindex = a(i, j, k)%c(ic)                      ! cell index
                            mindex = cmat(cindex, 2)                       ! material index
                            cfract = a(i, j, k)%f(ic) / thits              ! vol.frac of the cell
                            cdenst = cden(cindex, 1)                       ! cell density

                            if (count(icl .eq. cindex) .eq. 0) then
                                ! Cell cindex appears for the 1-st time
                                i1 = aiindex(icl, -1)   ! index of the fisrt default value
                                icl(i1) = cindex
                                write(*, *) 'Cell index', i1, cindex
                            else
                                ! Cell cindex was already found previously 
                                i1 = aiindex(icl, cindex)
                            end if
                            if (count(imt .eq. mindex) .eq. 0) then
                                ! Material mindex appears for the 1-st time
                                i2 = aiindex(imt, -1)
                                imt(i2) = mindex
                                write(*, *) 'Mat  index', i2, mindex
                            else 
                                ! Material mindex was already found before
                                i2 = aiindex(imt, mindex)
                            end if
                            cmass(i1, 2) = cmass(i1, 2) + v * cfract           ! cell volume
                            cmass(i1, 1) = cmass(i1, 1) + v * cfract * cdenst  ! cell mass
                            mmass(i2, 2) = mmass(i2, 2) + v * cfract           ! material volume
                            mmass(i2, 1) = mmass(i2, 1) + v * cfract * cdenst  ! material mass
                        end do
                    end do
                end do
            end do

            ! print out
            write(*, '(3a12)') 'Cell_index', 'Cell_vol', 'Cell_mass'
            i1 = count(icl .ne. -1)
            i2 = count(imt .ne. -1)
            do i = 1, i1
                write(*, '(i12, 1p2e12.5)') icl(i), cmass(i, 2), cmass(i, 1)
            end do
            write(*, '(3a12)') 'Mat_index', 'Mat_vol', 'Mat_mass'
            do i = 1, i2
                write(*, '(i12, 1p2e12.5)') imt(i), mmass(i, 2), mmass(i, 1)
            end do
        end subroutine check_fine_mesh_content

        subroutine get_mat(funit, fname, mats)
            ! Read the file containing print table FIS
            implicit none
            integer, intent(in):: funit
            character (len=*), intent(in):: fname
            type(mcomp), allocatable, intent(out):: mats(:)

            ! local vars
            integer:: n, i, j, mi, mn, nn
            character (len=6):: nam

            open(funit, file=fname)

            read(funit, *)
            read(funit, *) n  ! number of materials
            write(*, *) 'There should be n materials', n
            allocate(mats(n))
            write(*, *) 'mats allocated'
            do i = 1, n
                read(funit, *) nam, mi, mn, nn
                allocate(mats(mi)%za(nn))
                allocate(mats(mi)%f(nn))
                do j = 1, nn
                    read(funit, *), mats(mi)%za(j), nam, mats(mi)%f(j)
                end do
            end do
            close(funit)
            return
        end subroutine get_mat 

        subroutine get_cmi(funit, fname, cells, mats, densities)
            ! Read information from the print table CMI
            implicit none
            character (len=*), intent(in):: fname  ! filename to read
            integer, intent(in):: funit            ! Ensure different units in different processes
            integer, allocatable, intent(out):: & 
                cells(:, :), &  ! Cell name, material index
                mats(:)         ! material name
            real, allocatable, intent(out):: densities(:, :)  ! dens and conc for a cell

            ! local variables
            integer:: ci, cn, i, mi, mn
            real:: cd, cc
            integer:: mimax  ! number of materials
            logical:: e
            character (len=30):: l

            inquire(file=fname, exist=e)
            if (.not. e) then
                write(*, *) 'File with CMI does not exist:', fname
                stop
            end if

            ! First run: get line numbers and number of lines
            open(funit, file=fname)
            l = get_line(funit, 'Cell_index', 30)
            mimax = 0
            do while (.not. eof(funit))
                read(funit, *) ci, cn, i, i, mi, mn, cd, cc
                if (mimax .lt. mi) mimax = mi
            end do
            write(*, *) 'CMI table contains '
            write(*, *) '        cells', ci
            write(*, *) '        mats ', mimax
            allocate(cells(ci, 2))
            allocate(densities(ci, 2))
            allocate(mats(mimax))

            rewind(funit)
            l = get_line(funit, 'Cell_index', 30)
            do while (.not. eof(funit))
                read(funit, *) ci, cn, i, i, mi, mn, cd, cc
                cells(ci, 1) = cn
                cells(ci, 2) = mi
                densities(ci, 1) = cd
                densities(ci, 2) = cc
                mats(mi) = mn
            end do
            close(funit)

            ! Print out arrays for check
            write(*, *) 'Cell_index Cell_name Material_index Density Conc'
            do i = 1, ci
                write(*, *) i,                 & ! cell index
                            cells(i, 1),       & ! cell name
                            cells(i, 2),       & ! material index
                            densities(i, 1),   & ! cell density
                            densities(i, 2)      ! cell concentration
            end do
            write(*, *) 'Material_index Material_name'
            do i = 1, mimax
                write(*, *) i, mats(i)
            end do
            return
        end subroutine get_cmi


        pure function to_lower(si) result (so)
            ! based on https://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90
            implicit none
            character (len=*), intent(in)::  si
            character (len(si)):: so

            integer:: i, j
            character(26), parameter:: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
            character(26), parameter:: low = 'abcdefghijklmnopqrstuvwxyz'
            
            so = si
            do i = 1, len_trim(si)
                j = index(cap, si(i:i))
                if (j > 0) so(i:i) = low(j:j)
            end do
        end function to_lower

        function get_line(fn, ss, n) result(l)
            ! Reads lines from unit fn until find the line containing ss.
            implicit none
            integer, intent(in):: fn  ! file unit
            character (len=*), intent(in):: ss  ! substring to search for
            integer, intent(in):: n   ! length of the returned line
            character (len=n):: l

            integer i

            do while (.not. eof(fn))
                read(fn, '(a<n>)') l
                ! l = to_lower(l)
                i = index(l, ss)
                if (i > 0) then
                    return
                end if
            end do
            return
        end function get_line

        pure function aiindex(a, n) result(i)
            ! return index i of element n in the array a.
            implicit none
            integer, intent(in):: a(:)
            integer, intent(in):: n
            integer:: i
            do i = 1, size(a)
                if (a(i) .eq. n) return
            end do
        end function aiindex

        function read_line(fn) result(l)
            ! Read next line in fn into character string l.
            implicit none
            integer, intent(in):: fn
            character (len=:), allocatable:: l

            integer i, n
            character (len=10000)::  c
            ! get the line length
            n = 1
            i = 0
            do while (i .eq. 0)
                read(fn, '(a)', advance='no', iostat=i) c(n:n)
                n = n + 1
            end do
            n = n - 1
            allocate(character (len=n):: l)
            l(:n) = c(:n)
            return
        end function read_line

end module gen      
