!> Description:
!>  Module to manage input climate forcing data.
module climate_forcing

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use climate_forcing_config
    use climate_forcing_io
    use print_routines

    implicit none

    contains

    !> Description:
    !>  Initializes the climate forcing object, including the allocation
    !>  of variables, and opens the climate files for forcing data.
    !>  Resumes states from the climate forcing state file, if enabled.
    !>
    !> Input/output variables.
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  ii1: Start index in the GAT vector.
    !*  ii2: Stop index in the GAT vector.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    function climate_module_init(fls, shd, ii1, ii2, cm) result(ENDDATA)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        !> model_dates: 'ic' counter variable.
        !> model_variables: 'vs' variable.
        !> FLAGS: 'SAVERESUMEFLAG'.
        use model_files_variables
        use shd_variables
        use model_dates
        use model_variables
        use FLAGS

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams) shd
        integer, intent(in) :: ii1, ii2

        !> Input/output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer vid, iun, iskip, isteps1, isteps2, t, s, k, j, i, ierr
        character(len = DEFAULT_LINE_LENGTH) line

        ENDDATA = .false.

        !> Allocate the climate forcing variable.
!?        cm%nclim = ck%nn
!?        if (allocated(cm%dat)) deallocate(cm%dat)
!?        allocate(cm%dat(cm%nclim))

        !> Set the default file name and map the climate GRD/GAT/GRU variables to 'vs'.
        cm%dat(ck%FB)%fname = 'basin_shortwave'
        cm%dat(ck%FB)%GRD => vs%grid%fsin(1:shd%NA)
        cm%dat(ck%FB)%GAT => vs%tile%fsin(1:shd%lc%NML)
        cm%dat(ck%FB)%GRU => vs%gru%fsin(1:shd%lc%NTYPE)
        cm%dat(ck%FI)%fname = 'basin_longwave'
        cm%dat(ck%FI)%GRD => vs%grid%flin(1:shd%NA)
        cm%dat(ck%FI)%GAT => vs%tile%flin(1:shd%lc%NML)
        cm%dat(ck%FI)%GRU => vs%gru%flin(1:shd%lc%NTYPE)
        cm%dat(ck%RT)%fname = 'basin_rain'
        cm%dat(ck%RT)%GRD => vs%grid%pre(1:shd%NA)
        cm%dat(ck%RT)%GAT => vs%tile%pre(1:shd%lc%NML)
        cm%dat(ck%RT)%GRU => vs%gru%pre(1:shd%lc%NTYPE)
        cm%dat(ck%TT)%fname = 'basin_temperature'
        cm%dat(ck%TT)%GRD => vs%grid%ta(1:shd%NA)
        cm%dat(ck%TT)%GAT => vs%tile%ta(1:shd%lc%NML)
        cm%dat(ck%TT)%GRU => vs%gru%ta(1:shd%lc%NTYPE)
        cm%dat(ck%UV)%fname = 'basin_wind'
        cm%dat(ck%UV)%GRD => vs%grid%uv(1:shd%NA)
        cm%dat(ck%UV)%GAT => vs%tile%uv(1:shd%lc%NML)
        cm%dat(ck%UV)%GRU => vs%gru%uv(1:shd%lc%NTYPE)
        cm%dat(ck%P0)%fname = 'basin_pres'
        cm%dat(ck%P0)%GRD => vs%grid%pres(1:shd%NA)
        cm%dat(ck%P0)%GAT => vs%tile%pres(1:shd%lc%NML)
        cm%dat(ck%P0)%GRU => vs%gru%pres(1:shd%lc%NTYPE)
        cm%dat(ck%HU)%fname = 'basin_humidity'
        cm%dat(ck%HU)%GRD => vs%grid%qa(1:shd%NA)
        cm%dat(ck%HU)%GAT => vs%tile%qa(1:shd%lc%NML)
        cm%dat(ck%HU)%GRU => vs%gru%qa(1:shd%lc%NTYPE)
        cm%dat(ck%N0)%fname = 'WR_runoff'
        cm%dat(ck%O1)%fname = 'WR_recharge'

        !> Read from file to override default configuration.
        call open_config(cm)

        !> Allocate GRD/GAT/GRU variables because no equivalent 'vs' variables exist for 'MET'.
        if (cm%dat(ck%MET)%factive) then
            allocate(cm%dat(ck%MET)%GRD(shd%NA), cm%dat(ck%MET)%GAT(shd%lc%NML), cm%dat(ck%MET)%GRU(shd%lc%NTYPE))
        end if

        !> Initialize climate variables.
        call print_message('READING: Climate forcing variables')
        do vid = 1, cm%nclim

            !> Cycle if the variable is not active.
            if (.not. cm%dat(vid)%factive) cycle

            !> Assign a unit number to the file.
            cm%dat(vid)%fiun = cm%basefileunit + vid

            !> Open the file.
            if (open_data(shd, cm, vid)) goto 999

            !> Print field to screen.
            call print_message(cm%dat(vid)%fpath)

            !> Check if the file is in the legacy binary format.
            if (cm%dat(vid)%ffmt == 0) then
                call print_error('Forcing data in the legacy binary format (*.bin) are no longer supported.')
                call print_message('These data must be converted to one of the supported formats.')
                call program_abort()
            end if

            !> Check that the forcing record is not less than the model time-step.
!todo: Could probably find a way to accommodate this (e.g., accumulating/averaging/etc...).
            if (cm%dat(vid)%hf < ic%dtmins) then
                write(line, FMT_GEN) ic%dtmins
                call print_error('The forcing data time-step is less than the model time-step: ' // trim(adjustl(line)) // ' mins')
                call print_message('Aggregate the data to the model time-step.')
                call program_abort()
            end if

            !> Check if the time-step is divisible by the model time-step.
            if (mod(cm%dat(vid)%hf, ic%dtmins) /= 0) then
                call print_error('The forcing data time-step must be divisible by the model time-step.')
                write(line, FMT_GEN) cm%dat(vid)%hf
                call print_message_detail('Data time-step: ' // trim(adjustl(line)) // ' mins')
                write(line, FMT_GEN) ic%dtmins
                call print_message_detail('Model time-step: ' // trim(adjustl(line)) // ' mins')
                call program_abort()
            end if

            !> Warn of unsupprted interpolation flag option.
            if (cm%dat(vid)%ipflg > 1) then
                write(line, FMT_GEN) cm%dat(vid)%ipflg
                call print_warning('INTERPOLATIONFLAG ' // trim(adjustl(line)) // ' is not supported and has no effect.', PAD_3)
                cm%dat(vid)%ipflg = 0
            end if

            !> Remark on INTERPOLATIONFLAG if the data and model use the same time-step.
            if (cm%dat(vid)%ipflg == 1 .and. cm%dat(vid)%hf == ic%dtmins) then
                line = 'INTERPOLATIONFLAG is active but has no effect. The climate forcing data and model have the same time-step.'
                call print_remark(line, PAD_3)
                cm%dat(vid)%ipflg = 0
            end if

            !> Preserve the last record skipped with INTERPOLATIONFLAG 2.
!?            if (INTERPOLATIONFLAG == 2) nrs = nrs - 1

            !> Activate fields for INTERPOLATIONFLAG.
            if (cm%dat(vid)%ipflg == 1) then
                if (allocated(cm%dat(vid)%ipdat)) deallocate(cm%dat(vid)%ipdat)
                allocate(cm%dat(vid)%ipdat(size(cm%dat(vid)%blocks, 1), 2))
            end if

        !> Allocate and initialize the alpha coefficient for the default series.
!            allocate(cm%dat(vid)%alpha(cm%dat(vid)%nseries))
!            cm%dat(vid)%alpha = 1.0 / cm%dat(vid)%nseries

        !> Special case two sources of precipitation with alpha constant.
!todo generalize this
!?            if (vid == ck%RT .and. cm%dat(ck%RT)%ffmt == 6) then
!?                call Init_clim_data(ck%RT, 921, cm)
!?                call Init_clim_data(8, 922, cm)
!?                return
!?            end if

            !> Allocate the data series.
!-            allocate(cm%dat(vid)%GRD(shd%NA), cm%dat(vid)%GAT(shd%lc%NML), cm%dat(vid)%GRU(shd%lc%NTYPE))

            !> Skip records in the file to the simulation start date.
            isteps1 = jday_to_tsteps( &
                cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%jday, cm%dat(vid)%start_date%hour, &
                cm%dat(vid)%start_date%mins, cm%dat(vid)%hf)
            isteps2 = jday_to_tsteps(ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins, cm%dat(vid)%hf)
            if (isteps2 < isteps1) then
                call print_error('The first record occurs after the simulation start date.')
                call print_message( &
                    'The record must start on or after the simulation start date.')
                write(line, "(i5, i4)") cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%jday
                call print_message_detail('First record occurs on: ' // trim(line))
                write(line, "(i5, i4)") ic%start%year, ic%start%jday
                call print_message_detail('Simulation start date: ' // trim(line))
                call program_abort()
            end if
            iskip = (isteps2 - isteps1)
            if (iskip > 0) then
                write(line, FMT_GEN) iskip
                call print_message_detail('Skipping ' // trim(adjustl(line)) // ' records.')
                if (update_data(shd, cm, vid, iskip)) goto 999
            end if
        end do

        !> Print summary of climate forcing variables.
        if (DIAGNOSEMODE) then
            write(line, FMT_GEN) 'Variable', 'Name', 'File format', 'Frame length', 'Blocks in-mem.', 'No. series'
            call print_message_detail(line)
            do i = 1, cm%nclim
                if (cm%dat(i)%factive) then
                    write(line, FMT_GEN) &
                        cm%dat(i)%id_var, cm%dat(i)%fname, cm%dat(i)%ffmt, cm%dat(i)%hf, cm%dat(i)%nblocks, cm%dat(i)%nseries
                    call print_message_detail(line)
                end if
            end do
            call print_message('')
        end if

        !> Resume states from file.
        if (RESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open( &
                iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat', action = 'read', status = 'old', &
                form = 'unformatted', access = 'sequential', iostat = ierr)
            if (ierr /= 0) then
                call print_error('Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat' // ' to resume states.')
                call program_abort()
            end if

            !> Stop if the state file does not contain the expected number of climate variables.
            read(iun) ierr
            if (ierr /= 7) then
                call print_error('Incompatible ranking in climate state file.')
                write(line, FMT_GEN) ierr
                call print_message_detail('Number of clim. variables read: ' // trim(adjustl(line)))
                write(line, FMT_GEN) 7
                call print_message_detail('Number of clim. variables expected: ' // trim(adjustl(line)))
                call program_abort()
            end if

            !> Loop through variables in the climate forcing object and read the states from file.
            do vid = 1, 7

                !> Read the state of the climate variable (in case reading into memory).
                read(iun) cm%dat(vid)%blocks
                read(iun) cm%dat(vid)%iblock

                !> Read the last time-step read from file.
                read(iun) cm%dat(vid)%itimestep

                !> Read the interpolation state (if active).
                read(iun) cm%dat(vid)%ipflg
                if (cm%dat(vid)%ipflg == 1) then
                    read(iun) cm%dat(vid)%ipdat

                    !> INTERPOLATIONFLAG 1 requires an additional frame be read from the next time-step.
                    if (cm%dat(vid)%itimestep == 0) then
                        if (update_data(shd, cm, vid, 0)) goto 999
                        cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                    end if
                end if
            end do

            !> Close the file to free the unit.
            close(iun)
        end if

        return

999     ENDDATA = .true.

    end function

    !> Description:
    !>  Updates climate forcing data, either from memory or from file.
    !>
    !> Input/output variables:
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  ii1: Start index in the GAT vector.
    !*  ii2: Stopp index in the GAT vector.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    function climate_module_update_data(fls, shd, ii1, ii2, cm) result(ENDDATA)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        !> model_dates: 'ic' counter variable.
        !> FLAGS: 'SAVERESUMEFLAG'.
        use model_files_variables
        use shd_variables
        use model_dates
        use FLAGS

        !> Required for 'value' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams) shd
        integer, intent(in) :: ii1, ii2

        !> Input/output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer ierr, vid, t, s, k, j, i
        real rt, alpha

        ENDDATA = .false.

        !> Loop through variables in the climate forcing object.
        do vid = 1, cm%nclim

            !> Update data if the climate variable is active.
            if (cm%dat(vid)%factive) then

                !> INTERPOLATIONFLAG 1 requires an additional frame be read in the first time-step.
                if (ic%ts_count == 1 .and. cm%dat(vid)%ipflg == 1) then
                    if (update_data(shd, cm, vid, 0)) goto 999
                    cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                end if

                !> Grab data from file.
                if (cm%dat(vid)%itimestep == 0) then

                    !> Update the input forcing data.
                    if (update_data(shd, cm, vid, 0)) goto 999

                    !> Apply conditions to the series of data is such conditions exist.
                    if (cm%dat(vid)%nseries > 0) then
                        do s = 1, cm%dat(vid)%nseries
                            select case (cm%dat(vid)%series(s)%attrtype)
                                case ('gru')
                                    call value(cm%dat(vid)%series(s)%attr(1), j, ierr)
                                    call value(cm%dat(vid)%series(s)%attr(2), alpha, ierr)
                                    forall (k = ii1:ii2, shd%lc%JLMOS(k) == j)
                                        cm%dat(vid)%GAT(k) = cm%dat(vid)%GAT(k)*alpha
                                    end forall
                            end select
                        end do
                    end if

                    !> Update interpolation fields.
                    if (cm%dat(vid)%ipflg == 1) then
                        cm%dat(vid)%ipdat(:, 1) = cm%dat(vid)%ipdat(:, 2)
                        cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                    end if

                end if

                !> Interpolate intermediate values.
                if (cm%dat(vid)%ipflg == 1) then
                    cm%dat(vid)%blocks(:, cm%dat(vid)%iblock) = cm%dat(vid)%ipdat(:, 1) + &
                        min(1.0, real(cm%dat(vid)%itimestep)/cm%dat(vid)%hf)*(cm%dat(vid)%ipdat(:, 2) - cm%dat(vid)%ipdat(:, 1))
                end if

                !> Extract data from the climate variable.
                select case (cm%dat(vid)%blocktype)

                    case (1)

                        !> Block type: GRD (Grid).
                        cm%dat(vid)%GRD = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        do k = ii1, ii2
                            cm%dat(vid)%GAT(k) = cm%dat(vid)%GRD(shd%lc%ILMOS(k))
                        end do
                        do k = ii1, ii2
                            cm%dat(vid)%GRU(shd%lc%JLMOS(k)) = cm%dat(vid)%GAT(k)
                        end do

                    case (2)

                        !> Block type: GRU.
                        cm%dat(vid)%GRU = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        cm%dat(vid)%GRD = 0.0
                        do k = ii1, ii2
                            j = shd%lc%JLMOS(k)
                            i = shd%lc%ILMOS(k)
                            cm%dat(vid)%GAT(k) = cm%dat(vid)%GRU(j)
                            cm%dat(vid)%GRD(i) = cm%dat(vid)%GRD(i) + shd%lc%ACLASS(i, j)*cm%dat(vid)%GRU(j)
                        end do

                    case (3)

                        !> Block type: GAT (Land element).
                        cm%dat(vid)%GAT = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        cm%dat(vid)%GRD = 0.0
                        do k = ii1, ii2
                            j = shd%lc%JLMOS(k)
                            i = shd%lc%ILMOS(k)
                            cm%dat(vid)%GRD(i) = cm%dat(vid)%GRD(i) + shd%lc%ACLASS(i, j)*cm%dat(vid)%GAT(k)
                            cm%dat(vid)%GRU(j) = cm%dat(vid)%GAT(k)
                        end do

                    case default
                        call print_error('Unable to read blocks from ' // trim(cm%dat(vid)%fpath) // '.')
                        call program_abort()

                end select

                !> Increment the time-step of the variable.
                cm%dat(vid)%itimestep = cm%dat(vid)%itimestep + ic%dtmins
                if (cm%dat(vid)%itimestep >= cm%dat(vid)%hf) then
                    cm%dat(vid)%itimestep = 0
                end if

                !> Update the count of the current block.
                if (cm%dat(vid)%nblocks > 1 .and. cm%dat(vid)%itimestep == 0) then
                    cm%dat(vid)%iblock = cm%dat(vid)%iblock + 1
                    if (cm%dat(vid)%iblock > cm%dat(vid)%nblocks) then
                        cm%dat(vid)%iblock = 1
                    end if
                end if
            end if
        end do

        !> Advance line if 'met' format file is active (special condition).
        if (cm%dat(ck%MET)%factive) then
            if (update_data(shd, cm, ck%MET, 1)) goto 999
        end if

        return

999     ENDDATA = .true.

    end function

    !> Description:
    !>  Saves states to the climate forcing state file, if enabled.
    !>
    !> Input/output variables:
    !*  fls: Contains file unit information.
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and unit.
    subroutine climate_module_finalize(fls, shd, cm)

        !> model_files_variables: 'fls' variable.
        !> shd_variables: 'shd' variable.
        !> FLAGS: 'SAVERESUMEFLAG'.
        use model_files_variables
        use shd_variables
        use FLAGS

        !> mpi_module: 'ipid' variable to identify node.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams) shd

        !> Input/output variables.
        type(clim_info) cm

        !> Local variables.
        integer vid, ierr, iun

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Save states to file.
        if (SAVERESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open( &
                iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat', action = 'write', status = 'replace', &
                form = 'unformatted', access = 'sequential', iostat = ierr)
            if (ierr /= 0) then
                call print_error('Unable to open ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat' // ' to save states.')
                call program_abort()
            end if

            !> Write the number of climate variables.
            write(iun) 7

            !> Loop through variables in the climate forcing object and write the states to file.
            do vid = 1, 7

                !> Save the state of the climate variable (in case reading into memory).
                write(iun) cm%dat(vid)%blocks
                write(iun) cm%dat(vid)%iblock

                !> Save the current time-step read from file.
                write(iun) cm%dat(vid)%itimestep

                !> Save the interpolation state (if active).
                write(iun) cm%dat(vid)%ipflg
                if (cm%dat(vid)%ipflg == 1) then
                    write(iun) cm%dat(vid)%ipdat
                end if
            end do

            !> Close the file to free the unit.
            close(iun)
        end if

    end subroutine

end module
