! Module containing geenral functions
module gen

    implicit none

    private
    public:: getts, get_line, get_file_name, i2str, ijk2str, &
             read_line, to_lower, str2int

    contains

        function get_file_name(prefix, base, delimiter, indices) result(fname)
            ! Compose file name from prefix, basename and indices
            character (len=*), intent(in):: prefix
            character (len=*), intent(in):: base
            character (len=*), optional:: delimiter
            integer, optional:: indices(:)
            character (len=:), allocatable:: fname

            character (len=:), allocatable:: dlm, sfx

            if (present(delimiter)) then
                dlm = delimiter
            else
                dlm = ""
            end if
            if (present(indices)) then
                sfx = ijk2str(trim(base), dlm, indices)
            else
                sfx = trim(base)
            end if
            fname = prefix // sfx
            return
        end function get_file_name


        function str2int(s) result(res)
            ! taken from https://fortrandev.wordpress.com/2013/07/06/fortran-hashing-algorithm/
            implicit none
            character (len=*), intent(in):: s
            integer:: res

            ! local
            integer:: i
            res = 5381
            do i = 1, len(s) 
                res = (ishft(res, 5) + res) + ichar(s(i:i))
            end do
            return
        end function str2int


        function ijk2str(prefix, delimiter, iarray) result(s)
            character (len=*), intent(in):: prefix
            character (len=*), intent(in):: delimiter
            integer, intent(in):: iarray(:)
            character (len=:), allocatable:: s

            ! local vars
            integer:: n, i
            character (len=600):: cind  ! string representation of indices

            n = size(iarray)
            write(cind, '(<n>(a, i0))') (delimiter, iarray(i), i = 1, n)
            s = prefix // trim(cind)
            return 
        end function ijk2str

        function i2str(i) result(s)
            integer, intent(in):: i
            character (len=:), allocatable:: s

            ! local vars
            character (len=50):: cind  ! string representation of i

            write(cind, '(i0)') i
            s = trim(cind)
            return 
        end function i2str

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
