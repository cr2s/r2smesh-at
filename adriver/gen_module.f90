! Module containing geenral functions
module gen
    ! Type to store material composition
    use proc, only: pr_log, pr_inp, pr_out 

    implicit none


    private
    public:: getts, get_line, &
             read_line, print_log, ijk2str, to_lower

    contains

        function ijk2str(p, i, delimiter) result(s)
            character (len=*), intent(in):: p
            integer, intent(in):: i(:)
            character (len=*), optional:: delimiter
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
            write(pr_log, '(a11, a20, a<n>)') '==========', ts, message
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
