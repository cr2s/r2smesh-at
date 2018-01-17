! Module containing io unit numbers
module iounits
    implicit none
    integer, parameter:: &
             io_co = 6,  & ! common output
             io_ci = 5     ! common input
    integer:: & 
             io_po, &  ! process output
             io_pl, &  ! process log
             io_puo    ! offset for unit numbers for current process.
end module
    
