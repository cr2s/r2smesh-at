module proc
    ! Module to store task-specific values.
    use mpi

    implicit none

    integer:: pr_id, &  ! Process ID. 
              pr_np, &  ! number of MPI processes
              pr_er     ! Error status for communications

    integer:: &
        pr_log = 1, &  ! UNit for log
        pr_inp = 2, &  ! UNit for files to read
        pr_out = 3     ! UNit for files to write

    integer:: pr_st(MPI_STATUS_SIZE)

    public

    contains
        subroutine pr_initialize
            integer:: c = 10000
            call mpi_init(pr_er)
            call mpi_comm_size(MPI_COMM_WORLD, pr_np, pr_er)
            call mpi_comm_rank(MPI_COMM_WORLD, pr_id, pr_er)

            select case (pr_np)
            case (1:99)    
                c = 100
            case (100:999) 
                c = 1000
            case default 
                c = 10000
            end select

            pr_log = 1 * c + pr_id 
            pr_out = 2 * c + pr_id 
            pr_inp = 3 * c + pr_id 
            open(pr_log)
        end subroutine

        subroutine pr_finalize
            call mpi_finalize(pr_er)
            close(pr_log)
        end subroutine pr_finalize

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
