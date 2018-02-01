! module to get environment variables relevant for r2s

! The following places to consider:
!   * Folder with the case data: meshtallies, scenario, fispact scripts, material allocation
!   * Original place of the fispact data and fispact executable
!   * Common folder for condense run 
!   * Local folders, process-specific, to prepare input files for fispact and to organize fispact workplaces
!   * Local folder to write gamma intensities, computed by each process.
!   * Common place, where gamma intensities are copied from the local folder at the end of calculation.

module r2senv
    character (len=:), allocatable::  & 
        r2s_natab,              & ! file, where natural abundancies to read
        r2s_condense_s1,        & ! Script to start fispact condense
        r2s_condense_s2,        & ! script to clean condense run
        r2s_collapse_s1,        & ! script to start collapse run
        r2s_collapse_s2,        & ! script to clean collapse run
        r2s_inventory_s1,       & ! script to start inventory run
        r2s_inventory_s2,       & ! script to clean inventory run
        r2s_matallocation,     & ! File describing material allocation
        r2s_neutronintensity,   & ! Meshtal containing neutron intensity. Must have the same geometry as r2s_matallocation
        r2s_neutronspectra,     & ! Meshtal containing neutron spectra. Covers the neutron intensity mesh and has boundary coordinates
        r2s_matcomposition,     & ! File containing material compositions
        r2s_cellsmaterials,     & ! File containing a table with cell and material indices and names
        r2s_cgiheader,          & ! File, where the header containing mesh boundaries is written (to be copied to the DGS file)
        r2s_scratch,            & ! Path to the scratch folders for reading and writing
        r2s_out,                & ! Folder where gi files are written by each process.
        r2s_init_1,             & ! Script name that is called at the begin only once
        r2s_init_n,             & ! Script name that is called at the begin on each node 
        r2s_finalize_1,         & ! Script name that is called at the end only once
        r2s_finalize_n,         & ! Script name that is called at the end on each node
        r2s_log                   ! Folder for process logs

    contains
        subroutine env_init()
            ! Prepare all necessary variables, so that they are 
            ! available in the other modules
            r2s_natab              = get_var('r2s_natab',            ':undefined:')
            r2s_condense_s1        = get_var('r2s_condense_s1',      ':undefined:')
            r2s_condense_s2        = get_var('r2s_condense_s2',      ':undefined:')
            r2s_collapse_s1        = get_var('r2s_collapse_s1',      ':undefined:')
            r2s_collapse_s2        = get_var('r2s_collapse_s2',      ':undefined:')
            r2s_inventory_s1       = get_var('r2s_inventory_s1',     ':undefined:')
            r2s_inventory_s2       = get_var('r2s_inventory_s2',     ':undefined:')
            r2s_matallocation      = get_var('r2s_matallocation',    ':undefined:')
            r2s_neutronintensity   = get_var('r2s_neutronintensity', ':undefined:')
            r2s_neutronspectra     = get_var('r2s_neutronspectra',   ':undefined:')
            r2s_matcomposition     = get_var('r2s_matcomposition',   ':undefined:')
            r2s_cellsmaterials     = get_var('r2s_cellsmaterials',   ':undefined:')
            r2s_cgiheader          = get_var('r2s_cgiheader',        ':undefined:')
            r2s_scratch            = get_var('r2s_scratch',          ':undefined:')
            r2s_out                = get_var('r2s_out',              ':undefined:')
            r2s_init_1             = get_var('r2s_init_1',           ':undefined:')
            r2s_init_n             = get_var('r2s_init_n',           ':undefined:')
            r2s_finalize_1         = get_var('r2s_finalize_1',       ':undefined:')
            r2s_finalize_n         = get_var('r2s_finalize_n',       ':undefined:')
            r2s_log                = get_var('r2s_log',              ':undefined:')
            ! TODO: put here file existence checks
            return
        end subroutine env_init

        subroutine r2senv_report(n)
            ! Print out all environment variables to unit n
            write(n, 100) 'r2s_natab',              r2s_natab
            write(n, 100) 'r2s_condense_s1',        r2s_condense_s1
            write(n, 100) 'r2s_condense_s2',        r2s_condense_s2
            write(n, 100) 'r2s_collapse_s1',        r2s_collapse_s1
            write(n, 100) 'r2s_collapse_s2',        r2s_collapse_s2
            write(n, 100) 'r2s_inventory_s1',       r2s_inventory_s1
            write(n, 100) 'r2s_inventory_s2',       r2s_inventory_s2
            write(n, 100) 'r2s_matallocation',      r2s_matallocation
            write(n, 100) 'r2s_neutronintensity',   r2s_neutronintensity
            write(n, 100) 'r2s_neutronspectra',     r2s_neutronspectra
            write(n, 100) 'r2s_matcomposition',     r2s_matcomposition
            write(n, 100) 'r2s_cellsmaterials',     r2s_cellsmaterials
            write(n, 100) 'r2s_cgiheader',          r2s_cgiheader
            write(n, 100) 'r2s_scratch',            r2s_scratch
            write(n, 100) 'r2s_out',                r2s_out
            write(n, 100) 'r2s_init_1',             r2s_init_1
            write(n, 100) 'r2s_init_n',             r2s_init_n
            write(n, 100) 'r2s_finalize_1',         r2s_finalize_1
            write(n, 100) 'r2s_finalize_n',         r2s_finalize_n
            write(n, 100) 'r2s_log',                r2s_log
            100 format (a35, ": ", a)
        end subroutine r2senv_report

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
