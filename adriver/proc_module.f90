module proc
    ! Module to store task-specific values.
    use mpi
    use ifport
    use r2senv
    use gen, only: getts, i2str, ijk2str, str2int

    implicit none

    integer:: pr_id, &  ! Process ID. 
              pr_np, &  ! number of MPI processes
              pr_nodecomm, &  ! Communicator within a node
              pr_nid,      &  ! Rank in the node communicator
              pr_er     ! Error status for communications

    integer:: &
        pr_log = 6, &  ! Unit for log
        pr_scr,     &  ! scratch unit for files to read
        pr_scw         ! scratch unit for files to write
        ! pr_cgi         ! Unit for the cgi file containing gamma intensities for all fine mesh elements processed with the current process

    integer:: pr_st(MPI_STATUS_SIZE)

    public

    contains
        subroutine pr_run1(cl, istat, funit)
            ! Run commad line cl only on the master process
            character (len=*), intent(in):: cl    ! command line to execute
            integer, intent(in), optional:: funit
            integer, intent(out):: istat

            integer:: fu
            if (present(funit)) then
                fu = funit
            else
                fu = pr_log
            end if

            if (pr_id .eq. 0) then 
                call pr_runp(cl, istat, fu)
            else
                istat = 0
            end if
            ! To ensure that all other processes wait untill the script is finished.
            call mpi_barrier(MPI_COMM_WORLD, pr_er)
            return 
        end subroutine pr_run1

        subroutine pr_runp(cl, istat, funit)
            ! Run command cl on each process. 
            character (len=*), intent(in):: cl    ! command line to execute
            integer, intent(out):: istat          ! Exit status of the command cl
            integer, intent(in), optional:: funit ! Where log should be written

            integer:: fu
            character*19:: t1, t2
            if (present(funit)) then
                fu = funit
            else
                fu = pr_log
            end if

            t1 = getts()
            istat = system(cl) 
            t2 = getts()
            call report_file_name(t1 // ' Running script', cl // " #  " // t2 // " exit status " // i2str(istat), fu) 
            return
        end subroutine pr_runp

        subroutine pr_runn(cl, istat)
            ! Run command cl on each node only once. 
            ! Report with comment cmnt
            character (len=*), intent(in):: cl    ! command line to execute
            integer, intent(out):: istat

            if (pr_nid .eq. 0) then 
                call pr_runp(cl, istat)
            else
                istat = 0
            end if
            call mpi_barrier(MPI_COMM_WORLD, pr_er)
        end subroutine pr_runn

        subroutine report_file_name(cmnt, fname, funit)
            character (len=*), intent(in):: cmnt, fname
            integer, intent(in), optional:: funit

            integer:: n
            integer, parameter:: nmax = 40


            integer:: fu
            if (present(funit)) then
                fu = funit
            else
                fu = pr_log
            end if

            n = len(cmnt)
            if (n .gt. nmax) n = nmax
            write(fu, '(a<nmax>, ": ", a)') cmnt(:n), fname
        end subroutine report_file_name

        subroutine pr_initialize
            integer:: pnamelen, ipname
            character*(MPI_MAX_PROCESSOR_NAME):: pname_tmp
            character (len=:), allocatable:: pname
            integer:: c

            call mpi_init(pr_er)
            call mpi_comm_size(MPI_COMM_WORLD, pr_np, pr_er)
            call mpi_comm_rank(MPI_COMM_WORLD, pr_id, pr_er)

            ! to split by nodes
            call mpi_get_processor_name(pname_tmp, pnamelen, pr_er)
            pname = trim(pname_tmp)
            ipname = str2int(pname)

            call mpi_comm_split(MPI_COMM_WORLD, ipname, pr_id, pr_nodecomm, pr_er) 
            call mpi_comm_rank(pr_nodecomm, pr_nid, pr_er)

            select case (pr_np)
            case (1:99)    
                c = 100
            case (100:999) 
                c = 1000
            case (1000:9999) 
                c = 10000
            case default 
                c = 100000
            end select
            ! log and cgi files are opened once for each process
            pr_log = 1 * c + pr_id 
            ! pr_cgi = 2 * c + pr_id
            ! Scratch files should be opened and closed in subroutines where they are used
            pr_scw = 3 * c + pr_id 
            pr_scr = 4 * c + pr_id 
        end subroutine pr_initialize

        subroutine pr_open_files()
            open(pr_log, file=r2s_log // '/log.' // i2str(pr_id), status='NEW')
        end subroutine pr_open_files

        subroutine pr_inquire_files(funit)
            integer, intent(in):: funit
            logical:: i_opened
            character:: i_action
            character (len=1000):: i_name
            inquire(unit=funit, opened=i_opened, name=i_name, action=i_action)
            write(*, 100) funit, i_opened, trim(i_name), i_action
            100 format ('Inquire for unit', i4, l2, a50, a3)
        end subroutine pr_inquire_files



        subroutine pr_finalize
            call mpi_finalize(pr_er)
            close(pr_log)
        end subroutine pr_finalize

        subroutine print_log(message)
            character (len=*), intent(in):: message
            ! local vars
            character (len=19):: ts
            integer:: n
            ts = getts()
            n = len(message) + 1
            write(pr_log, '(a11, a20, a<n>)') '==========', ts, message
            return
        end subroutine print_log
        
        subroutine broadcast_1i(a, lb)
            integer, allocatable, intent(inout):: a(:)
            integer, intent(in):: lb  ! lower bound
            integer:: l
            if (pr_id .eq. 0) then
                l = size(a)
            end if
            call mpi_bcast(l, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)
            if (pr_id .ne. 0) then
                allocate(a(lb: lb + l - 1))
            end if
            call mpi_bcast(a, l, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)
        end subroutine broadcast_1i

        subroutine broadcast_3i(a)
            integer, allocatable, intent(inout):: a(:, :, :)
            integer:: l(4)
            integer, allocatable:: a1(:)

            ! Define size and shape of the array on the root process
            if (pr_id .eq. 0) then
                l(1:3) = shape(a)
                l(4) = size(a)
            end if

            ! Broadcast array shape
            call mpi_bcast(l, 4, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)

            ! Allocate temporary 1-dim array, which will be broadcast
            allocate(a1(l(4)))
            if (pr_id .eq. 0) then
                a1 = reshape(a, (/l(4)/))
            end if
            call mpi_bcast(a1, l(4), MPI_REAL, 0, MPI_COMM_WORLD, pr_er)

            if (pr_id .ne. 0) then
                allocate(a(l(1), l(2), l(3)))
                a = reshape(a1, l(1:3))
            end if
        end subroutine broadcast_3i

        subroutine broadcast_1r(a)
            real, allocatable, intent(inout):: a(:)
            integer:: l
            if (pr_id .eq. 0) then
                l = size(a)
            end if
            call mpi_bcast(l, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)
            if (pr_id .ne. 0) then
                allocate(a(l))
            end if
            call mpi_bcast(a, l, MPI_REAL, 0, MPI_COMM_WORLD, pr_er)
        end subroutine broadcast_1r

        subroutine broadcast_5r(a)
            real, allocatable, intent(inout):: a(:, :, :, :, :)
            integer:: l(6)
            real, allocatable:: a1(:)

            ! Define size and shape of the array on the root process
            if (pr_id .eq. 0) then
                l(1:5) = shape(a)
                l(6) = size(a)
            end if

            ! Broadcast array shape
            call mpi_bcast(l, 6, MPI_INTEGER, 0, MPI_COMM_WORLD, pr_er)

            ! Allocate temporary 1-dim array, which will be broadcast
            allocate(a1(l(6)))
            if (pr_id .eq. 0) then
                a1 = reshape(a, (/l(6)/))
            end if
            call mpi_bcast(a1, l(6), MPI_REAL, 0, MPI_COMM_WORLD, pr_er)

            if (pr_id .ne. 0) then
                allocate(a(l(1), l(2), l(3), l(4), l(5)))
                a = reshape(a1, l(1:5))
            end if
        end subroutine broadcast_5r


end module proc        
