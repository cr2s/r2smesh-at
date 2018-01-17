! module to get environment variables relevant for r2s
module r2senv
    character (len=:), allocatable::  & 
        r2s_fwd, &       ! fispact working directory
        r2s_fsp_data, &  ! Folder with fispact data 
        r2s_scratch      ! Folder for small files to write

    contains
        subroutine env_init()
            ! Prepare all necessary variables, so that they are 
            ! available in the other modules
            r2s_fwd      = get_var('r2s_fwd',      'fispact_wd')
            r2s_fsp_data = get_var('r2s_fsp_data', 'FISPACT-II')
            r2s_scratch  = get_var('r2s_scratch',  'SCRATCH')
            return
        end subroutine env_init

        function get_var(var, dflt) result(l)
            character (len=*), intent(in):: var
            character (len=*), intent(in):: dflt
            character (len=:), allocatable:: l

            character (len=1000):: raw
            integer:: n, s

            call get_environment_variable(name=var, value=raw, & 
                                          length=n, status=s)
            if (s .eq. 0) then
                ! value var exists and either has no value or its value is 
                ! assigned to raw
                if (n .gt. 0) then 
                    allocate(character (len=n):: l)
                    l = trim(raw)
                    return
                end if
            end if
            ! in case the env.var not set or empty, return its default value
            n = len(dflt)
            allocate(character (len=n):: l)
            l = dflt
            return
        end function get_var


end module r2senv      
