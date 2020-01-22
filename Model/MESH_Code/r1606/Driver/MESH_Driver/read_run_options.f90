subroutine READ_RUN_OPTIONS(fls, shd, cm, ierr)

    use mpi_module
    use strings
    use model_files_variables
    use sa_mesh_common
    use model_dates
    use climate_forcing
    use output_files

    use FLAGS
    use save_basin_output, only: BASINAVGWBFILEFLAG, BASINAVGEBFILEFLAG, STREAMFLOWOUTFLAG, REACHOUTFLAG
    use RUNCLASS36_variables
    use RUNCLASS36_save_output
    use runsvs_mesh
    use baseflow_module
    use cropland_irrigation_variables
    use WF_ROUTE_config
    use rte_module
    use SA_RTE_module, only: SA_RTE_flgs
    use SIMSTATS_config, only: mtsflg
    use PBSM_module

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd
    type(CLIM_INFO) cm

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer CONFLAGS, IROVAL, iun, nargs, n, j, i, z
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args

    !> Initialize the return status.
    ierr = 0

    !>
    !> SET RUN OPTIONS
    !> READ THE RUN_OPTIONS.INI INPUT FILE TO SET OR RESET ANY CONTROL
    !> FLAGS AND READ THE GRID OUTPUT DIRECTORIES.
    !>
    !>    * IF IDISP = 0, VEGETATION DISPLACEMENT HEIGHTS ARE IGNORED,
    !>    * BECAUSE THE ATMOSPHERIC MODEL CONSIDERS THESE TO BE PART OF THE
    !>    * "TERRAIN".
    !>    * IF IDISP = 1, VEGETATION DISPLACEMENT HEIGHTS ARE CALCULATED.
    IDISP = 1

    !>    * IF IZREF = 1, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN TO
    !>    * LIE AT THE GROUND SURFACE.
    !>    * IF IZREF = 2, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN TO
    !>    * LIE AT THE LOCAL ROUGHNESS HEIGHT.
    IZREF = 1

    !>    * IF ISLFD = 0, DRCOEF IS CALLED FOR SURFACE STABILITY
    !>    * CORRECTIONS AND THE ORIGINAL GCM SET OF SCREEN-LEVEL DIAGNOSTIC
    !>    * CALCULATIONS IS DONE.
    !>    * IF ISLFD = 1, DRCOEF IS CALLED FOR SURFACE STABILITY
    !>    * CORRECTIONS AND SLDIAG IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC
    !>    * CALCULATIONS.
    !>    * IF ISLFD = 2, FLXSURFZ IS CALLED FOR SURFACE STABILITY
    !>    * CORRECTIONS AND DIASURF IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC
    !>    * CALCULATIONS.
    ISLFD = 2

    !>    * IF IPCP = 1, THE RAINFALL-SNOWFALL CUTOFF IS TAKEN TO LIE AT
    !>    * 0 dC.
    !>    * IF IPCP = 2, A LINEAR PARTITIONING OF PRECIPITATION BETWEEEN
    !>    * RAINFALL AND SNOWFALL IS DONE BETWEEN 0 dC AND 2 dC.
    !>    * IF IPCP = 3, RAINFALL AND SNOWFALL ARE PARTITIONED ACCORDING TO
    !>    * A POLYNOMIAL CURVE BETWEEN 0 dC AND 6 dC.
    !>    * IF IPCP=4, THE RAINFALL, SNOWFALL AND TOTAL PRECIPITATION RATES
    !>    * ARE READ IN DIRECTLY.
    IPCP = 1

    !>    * ITC, ITCG AND ITG ARE SWITCHES TO CHOOSE THE ITERATION SCHEME
    !>    * TO BE USED IN CALCULATING THE CANOPY OR GROUND SURFACE
    !>    * TEMPERATURE RESPECTIVELY.  IF THE SWITCH IS SET TO 1, A
    !>    * COMBINATION OF SECANT AND BISECTION METHODS IS USED; IF TO 2,
    !>    * THE NEWTON-RAPHSON METHOD IS USED.
    ITC = 2
    ITCG = 2
    ITG = 2

    !>    * IF IWF = 0, ONLY OVERLAND FLOW AND BASEFLOW ARE MODELLED, AND
    !>    * THE GROUND SURFACE SLOPE IS NOT MODELLED.
    !>    * IF IWF = 1, THE MODIFIED CALCULATIONS OF OVERLAND
    !>    * FLOW AND INTERFLOW ARE PERFORMED.
    !>    * IF IWF = 2, SAME AS IWF = 0 EXCEPT THAT OVERLAND FLOW IS
    !>    * MODELLED AS FILL AND SPILL PROCESS FROM A SERIES OF POTHOLES.
    !>    * DEFAULT VALUE IS 1.
    RUNCLASS36_flgs%INTERFLOWFLAG = 1

    !>    * IF IPAI, IHGT, IALC, IALS AND IALG ARE ZERO, THE VALUES OF
    !>    * LEAF ARE INDEX, VEGETATION HEIGHT, CANOPY ALBEDO, SNOW ALBEDO
    !>    * AND SOIL ALBEDO RESPECTIVELY CALCULATED BY CLASS ARE USED.
    !>    * IF ANY OF THESE SWITCHES IS SET TO 1, THE VALUE OF THE
    !>    * CORRESPONDING PARAMETER CALCULATED BY CLASS IS OVERRIDDEN BY
    !>    * A USER-SUPPLIED INPUT VALUE.
    IPAI = 0
    IHGT = 0
    IALC = 0
    IALS = 0
    IALG = 0

    !>    * ICTEMMOD IS SET TO 1 IF CLASS IS BEING RUN IN CONJUNCTION WITH
    !>    * THE CANADIAN TERRESTRIAL ECOSYSTEM MODEL "CTEM"; OTHERWISE
    !>    * ICTEMMOD IS SET TO 0.
    ICTEMMOD = 0

    !> DAN * IF RELFLG = 0, ANY CONFIGURATION FILE IS READ THAT MATCHES
    !> DAN * THE FILE NAME IN THE OPEN STATEMENT.
    !> DAN * IF RELFLG = 1, ONLY CONFIGURATION FILES WHOSE VERSION MATCHES
    !> DAN * THE RELEASE OF MESH_DRIVER ARE READ.  THE PROGRAM STOPS IF THE
    !> DAN * TWO STRINGS DO NOT MATCH.
    !> DAN * THIS FLAG IS NOT APPLICABLE TO RUN_OPTIONS.INI, WHERE THIS FLAG
    !> DAN * MAY BE RESET).
    RELFLG = 1

    !> set RESUMEFLAG to 0 just as a default
    !> this will be set later by reading the run_options file
    !* if RESUMEFLAG is 0, the user doesn't want to use the resume file
    !* if RESUMEFLAG is 1, the user wants to run the resume file.
    !* if RESUMEFLAG is 2, the user wants to run the r2c resume file.
    RESUMEFLAG = 0

    !* if SAVERESUMEFLAG is 0, the user doesn't want to make the resume file.
    !* if SAVERESUMEFLAG is 1, the user wants to make the resume file.
    !* if SAVERESUMEFLAG is 2, the user wants to make the r2c resume file.
    !* if SAVERESUMEFLAG is 3, the user wants to make the only class prognostic variables resume file.
    SAVERESUMEFLAG = 0

    !> SOIL INITIALIZATION  FLAG - DEFAULT = STOP SIMULATION IF SUM OF SOIL PERCENTAGES EXCEEDS 100%
    !> If SOILINIFLAG is 0, stop simulation if the sum of soil percentages is greater than 100%
    !> If SOILINIFLAG is 1, no adjustment to soil percentages even if the sum is greater than 100%
    !> If SOILINIFLAG is 2, adjust soil percentages in favor of sand
    !> If SOILINIFLAG is 3, adjust soil percentages in favor of clay
    !> If SOILINIFLAG is 4, adjust soil percentages proportionally
    !> If SOILINIFLAG is 5, directly read soil parameter values from soil.ini file.
    SOILINIFLAG = 0

    !> NUDGESDEPFLAG: Nudge SDEP of the tile to the nearest boundary in the discretized soil profile.
    !*  NUDGESDEPFLAG 0: No effect (default).
    !*  NUDGESDEPFLAG 1: Nudge SDEP of the tile to the nearest boundary in the discretized soil profile.
    NUDGESDEPFLAG = 0

    !> If OBJFNFLAG is 0 {DEFAULT} = SAE - SUM OF ABSOLUTE VALUE OF ERRORS
    !> If OBJFNFLAG is 1, SAESRT - SUM OF ABSOLUTE VALUE OF ERRORS AFTER SORTING
    !> If OBJFNFLAG is 2, SAEMSRT - SUM OF ABSOLUTE VALUE OF MOVING ERRORS AFTER SORTING
    !> If OBJFNFLAG is 3, NSE - MEAN NASH-SUTCLIFFE MODEL EFFICIENCY INDEX (+ve FOR MAXIMIZATION)
    !> IF OBJFNFLAG is 4, NSE - MEAN NASH-SUTFLIFFE MODEL EFFICIENCY INDEX (-ve FOR MINIMIZATION)
    OBJFNFLAG = 0

    WINDOWSIZEFLAG = 1
    WINDOWSPACINGFLAG = 1

    METRICSSTATSOUTFLAG = 1
    METRICSFILTEROBSFLAG = 1

    !> METRICSSPINUP specifies the starting day from which to calculate metrics.
    !> The starting day is relative to the beginning of the simulation; Day 1 is
    !> the first day of the simulation, regardless of the date or its Julian date
    !> in the year. If METRICSINCLUDESPINUP is set to 1, METRICSSPINUP is not used.
    METRICSSPINUP = 1

    !> If METRICSINCLUDESPINUP is set to 1 then metrics are calculated from the
    !> first day of the simulation (1:ndsim).
    !> If METRICSINCLUDESPINUP is set to 0 then metrics are calculated from
    !> METRICSSPINUP (METRICSSPINUP:ndsim).
    METRICSINCLUDESPINUP = 0

    !> If FROZENSOILINFILFLAG is 0, all snow melt infiltrates.
    !> If FROZENSOILINFILFLAG is 1, snow melt is partitioned to frozen soil infiltration
    !> and direct runoff based on the parameteric equation developed by Gray et al, 2001.
    FROZENSOILINFILFLAG = 0

    !* If SUBBASINFLAG is 1, calculations will only be done for grid squares that are
    !* in the watersheds of the locations listed in the streamflow files.
    !* If SUBBASINFLAG is 0, calculations will be made for all grid squares.
    SUBBASINFLAG = 0

    !* If R2COUTPUTFLAG is 1, R2C ascii file will be written for user specified
    !* variables.
    !* If R2COUTPUTFLAG is 2, R2C binary will be written for user specified
    !* variables (list of variables will be read from r2c_output.txt file).
    R2COUTPUTFLAG = 0

    !* If FROZENSOILINFILFLAG is 0, all snow melt infiltrates.
    !* If FROZENSOILINFILFLAG is 1, snow melt is partitioned to frozen soil infiltration
    !* and direct runoff based on the parameteric equation developed by Gray et al, 2001.
    FROZENSOILINFILFLAG = 0

    !> FLAGS FOR GEOTHERMAL FLUX FOR THE BOTTOM OF THE LAST SOIL LAYER
    !* If GGEOFLAG is GT 0,  READ UNIQUE VALUE FROM MESH_ggeo.INI FILE
    GGEOFLAG = 0

    !> BASIN SWE OUTPUT FLAG
    !> If enabled, saves the SCA and SWE output files.
    !>     0 = Create no output.
    !>     1 = Save the SCA and SWE output files.
    BASINSWEOUTFLAG = 0

    !> The above parameter values are defaults, to change to a different
    !> value, use the MESH_input_run_options.ini file

    !> open the file and print an error if unable to open the file.
    call reset_tab()
    call print_screen('READING: ' // trim(fls%fl(mfk%f53)%fn))
    call increase_tab()
    iun = fls%fl(mfk%f53)%iun
    open(iun, file = fls%fl(mfk%f53)%fn, status = 'old', action = 'read', iostat = ierr)

    !> Return if an error occured.
    if (ierr /= 0) then
        ECHOTXTMODE = .false.
        call print_error('Unable to open the file. Check if the file exists.')
        return
    end if

    !> Begin reading the control flags.
    do i = 1, 3
        read(iun, *, err = 98)
    end do
    read(iun, '(i5)', err = 98) CONFLAGS

    !> Read and parse the control flags.
    if (CONFLAGS > 0) then

        !> Control flags are parsed by space.
        do i = 1, CONFLAGS

            !> Read and parse the entire line.
            call readline(iun, line, ierr)
            if (ierr /= 0) goto 98
            if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
            if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
            call compact(line)
            call parse(line, ' ', args, nargs)
            if (.not. nargs > 0) then
                write(line, FMT_GEN) i
                call print_screen('WARNING: Error reading control flag ' // trim(adjustl(line)))
                cycle
            end if

            !> Determine the control flag and parse additional arguments.
            z = 0
            select case (trim(adjustl(args(1))))

                case ('IDISP')
                    call value(args(2), IDISP, z)
                case ('IZREF')
                    call value(args(2), IZREF, z)
                case ('ISLFD')
                    call value(args(2), ISLFD, z)
                case ('IPCP')
                    call value(args(2), IPCP, z)
                case ('ITC')
                    call value(args(2), ITC, z)
                case ('ITCG')
                    call value(args(2), ITCG, z)
                case ('ITG')
                    call value(args(2), ITG, z)
                case ('IWF')
                    call value(args(2), RUNCLASS36_flgs%INTERFLOWFLAG, z)
                case ('IPAI')
                    call value(args(2), IPAI, z)
                case ('IHGT')
                    call value(args(2), IHGT, z)
                case ('IALC')
                    call value(args(2), IALC, z)
                case ('IALS')
                    call value(args(2), IALS, z)
                case ('IALG')
                    call value(args(2), IALG, z)
                case ('RESUMEFLAG')
                    call value(args(2), RESUMEFLAG, z)
                case ('SAVERESUMEFLAG')
                    call value(args(2), SAVERESUMEFLAG, z)

                !> Basin forcing time-step flag.
                case ('HOURLYFLAG')
                    call value(args(2), IROVAL, z)
                    if (z == 0) then
                        do j = 1, cm%nclim
                            cm%dat(j)%hf = IROVAL
                        end do
                    end if

                !> Model time-step.
                case ('TIMESTEPFLAG')
                    call value(args(2), ic%dtmins, z)
                    ic%dts = ic%dtmins*60

                case ('RELFLG')
                    call value(args(2), RELFLG, z)

                !> Message output options.
                case (PRINTSIMSTATUS_NAME, 'VERBOSEMODE')
                    call parse_options(PRINTSIMSTATUS_NAME, args(2:nargs))
                case (DIAGNOSEMODE_NAME)
                    call parse_options(DIAGNOSEMODE_NAME, args(2:nargs))
                case (ECHOTXTMODE_NAME, 'MODELINFOOUTFLAG')
                    call parse_options(ECHOTXTMODE_NAME, args(2:nargs))

                !> MPI OPTIONS
                case ('MPIUSEBARRIER')
                    call value(args(2), MPIUSEBARRIER, z)

                !> BASIN FORCING DATA OPTIONS
                !> Basin forcing data.
                case ('BASINFORCINGFLAG')
                    do j = 2, nargs
                        select case (lowercase(args(j)))
                            case ('met')

                                !> Assign attributes for the 7 variables.
                                !> To override these, respective 'BASIN' flags should list after 'BASINFORCINGFLAG'.
                                cm%dat(ck%FB)%factive = .true.
                                cm%dat(ck%FB)%id_var = 'FB'
                                cm%dat(ck%FB)%ffmt = 6
                                cm%dat(ck%FI)%factive = .true.
                                cm%dat(ck%FI)%id_var = 'FI'
                                cm%dat(ck%FI)%ffmt = 6
                                cm%dat(ck%RT)%factive = .true.
                                cm%dat(ck%RT)%id_var = 'RT'
                                cm%dat(ck%RT)%ffmt = 6
                                cm%dat(ck%TT)%factive = .true.
                                cm%dat(ck%TT)%id_var = 'TT'
                                cm%dat(ck%TT)%ffmt = 6
                                cm%dat(ck%UV)%factive = .true.
                                cm%dat(ck%UV)%id_var = 'UV'
                                cm%dat(ck%UV)%ffmt = 6
                                cm%dat(ck%P0)%factive = .true.
                                cm%dat(ck%P0)%id_var = 'P0'
                                cm%dat(ck%P0)%ffmt = 6
                                cm%dat(ck%HU)%factive = .true.
                                cm%dat(ck%HU)%id_var = 'HU'
                                cm%dat(ck%HU)%ffmt = 6

                                !> Activate the 'met' file.
                                cm%dat(ck%MET)%ffmt = 6
                                cm%dat(ck%MET)%id_var = 'CLASSMET'
                                cm%dat(ck%MET)%factive = .true.
                                exit
                        end select
                    end do
                case ('BASINSHORTWAVEFLAG')
                    call value(args(2), cm%dat(ck%FB)%ffmt, z)
                    if (z == 0) cm%dat(ck%FB)%factive = .true.
                    cm%dat(ck%FB)%id_var = 'FB'
                    if (cm%dat(ck%FB)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%FB)%ffmt, z)
                        call value(args(4), cm%dat(ck%FB)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%FB)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%FB)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINLONGWAVEFLAG')
                    call value(args(2), cm%dat(ck%FI)%ffmt, z)
                    if (z == 0) cm%dat(ck%FI)%factive = .true.
                    cm%dat(ck%FI)%id_var = 'FI'
                    if (cm%dat(ck%FI)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%FI)%ffmt, z)
                        call value(args(4), cm%dat(ck%FI)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%FI)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%FI)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINRAINFLAG')
                    call value(args(2), cm%dat(ck%RT)%ffmt, z)
                    if (z == 0) cm%dat(ck%RT)%factive = .true.
                    cm%dat(ck%RT)%id_var = 'RT'
                    if (cm%dat(ck%RT)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%RT)%ffmt, z)
                        call value(args(4), cm%dat(ck%RT)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%RT)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%RT)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINTEMPERATUREFLAG')
                    call value(args(2), cm%dat(ck%TT)%ffmt, z)
                    if (z == 0) cm%dat(ck%TT)%factive = .true.
                    cm%dat(ck%TT)%id_var = 'TT'
                    if (cm%dat(ck%TT)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%TT)%ffmt, z)
                        call value(args(4), cm%dat(ck%TT)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%TT)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%TT)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINWINDFLAG')
                    call value(args(2), cm%dat(ck%UV)%ffmt, z)
                    if (z == 0) cm%dat(ck%UV)%factive = .true.
                    cm%dat(ck%UV)%id_var = 'UV'
                    if (cm%dat(ck%UV)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%UV)%ffmt, z)
                        call value(args(4), cm%dat(ck%UV)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%UV)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%UV)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINPRESFLAG')
                    call value(args(2), cm%dat(ck%P0)%ffmt, z)
                    if (z == 0) cm%dat(ck%P0)%factive = .true.
                    cm%dat(ck%P0)%id_var = 'P0'
                    if (cm%dat(ck%P0)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%P0)%ffmt, z)
                        call value(args(4), cm%dat(ck%P0)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%P0)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%P0)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINHUMIDITYFLAG')
                    call value(args(2), cm%dat(ck%HU)%ffmt, z)
                    if (z == 0) cm%dat(ck%HU)%factive = .true.
                    cm%dat(ck%HU)%id_var = 'HU'
                    if (cm%dat(ck%HU)%ffmt == 5) then
                        call value(args(3), cm%dat(ck%HU)%ffmt, z)
                        call value(args(4), cm%dat(ck%HU)%nblocks, z)
                    end if
                    do j = 3, nargs
                        if (len_trim(args(j)) > 3) then
                            if (args(j)(1:3) == 'hf=') then
                                call value(args(j)(4:), cm%dat(ck%HU)%hf, z)
                            end if
                        end if
                        if (len_trim(args(j)) > 4) then
                            if (args(j)(1:4) == 'nts=') then
                                call value(args(j)(5:), cm%dat(ck%HU)%nblocks, z)
                            end if
                        end if
                    end do
                case ('BASINRUNOFFFLAG')
                case ('BASINRECHARGEFLAG')

                case ('STREAMFLOWFILEFLAG')
                    fms%stmg%qomeas%fls%ffmt = adjustl(args(2))
                case ('RESERVOIRFILEFLAG')
                    fms%rsvr%rlsmeas%fls%ffmt = adjustl(args(2))

                case ('SHDFILEFLAG')
                    SHDFILEFLAG = adjustl(line)

                case ('SOILINIFLAG')
                    call value(args(2), SOILINIFLAG, z)
                case ('NRSOILAYEREADFLAG')
                    call value(args(2), NRSOILAYEREADFLAG, z)
                case ('NUDGESDEPFLAG')
                    call value(args(2), NUDGESDEPFLAG, ierr)

                case ('PREEMPTIONFLAG')
                    call value(args(2), mtsflg%PREEMPTIONFLAG, z)

                !> Interpolation flag for climate forcing data.
                case ('INTERPOLATIONFLAG')
                    call value(args(2), IROVAL, z)
                    if (z == 0) then
                        cm%dat(ck%FB)%ipflg = IROVAL
                        cm%dat(ck%FI)%ipflg = IROVAL
                        cm%dat(ck%RT)%ipflg = IROVAL
                        cm%dat(ck%TT)%ipflg = IROVAL
                        cm%dat(ck%UV)%ipflg = IROVAL
                        cm%dat(ck%P0)%ipflg = IROVAL
                        cm%dat(ck%HU)%ipflg = IROVAL
                    end if

                case ('SUBBASINFLAG')
                    call value(args(2), SUBBASINFLAG, z)
                case ('R2COUTPUTFLAG')
                    call value(args(2), R2COUTPUTFLAG, z)
                case ('OBJFNFLAG')
                    call value(args(2), OBJFNFLAG, z)
                case ('AUTOCALIBRATIONFLAG')
                    call value(args(2), mtsflg%AUTOCALIBRATIONFLAG, z)
                case ('WINDOWSIZEFLAG')
                    call value(args(2), WINDOWSIZEFLAG, z)
                case ('WINDOWSPACINGFLAG')
                    call value(args(2), WINDOWSPACINGFLAG, z)
                case ('METRICSSTATSOUTFLAG')
                    call value(args(2), METRICSSTATSOUTFLAG, z)
                case ('METRICSFILTEROBSFLAG')
                    call value(args(2), METRICSFILTEROBSFLAG, z)
                case ('METRICSSPINUP')
                    call value(args(2), METRICSSPINUP, z)
                    METRICSSPINUP = max(METRICSSPINUP, 1)
                case ('METRICSINCLUDESPINUP')
                    call value(args(2), METRICSINCLUDESPINUP, z)
                case ('FROZENSOILINFILFLAG')
                    call value(args(2), FROZENSOILINFILFLAG, z)
                case ('PRINTRFFR2CFILEFLAG')
                    call value(args(2), SA_RTE_flgs%PRINTRFFR2CFILEFLAG, z)
                    SA_RTE_flgs%PROCESS_ACTIVE = (SA_RTE_flgs%PRINTRFFR2CFILEFLAG == 1)
                case ('PRINTRCHR2CFILEFLAG')
                    call value(args(2), SA_RTE_flgs%PRINTRCHR2CFILEFLAG, z)
                    SA_RTE_flgs%PROCESS_ACTIVE = (SA_RTE_flgs%PRINTRCHR2CFILEFLAG == 1)
!+                case ('PRINTLKGR2CFILEFLAG')
!+                    call value(args(2), SA_RTE_flgs%PRINTLKGR2CFILEFLAG, z)
!+                    SA_RTE_flgs%PROCESS_ACTIVE = (SA_RTE_flgs%PRINTLKGR2CFILEFLAG == 1)
                case ('ICTEMMOD')
                    call value(args(2), ICTEMMOD, z)

                !> PBSM (blowing snow).
                case ('PBSMFLAG')
                    call PBSM_parse_flag(line)

                case ('LOCATIONFLAG')
                    !> Flag has no effect.
                case ('OUTFIELDSFLAG', 'OUTFILESFLAG')
                    fls_out%PROCESS_ACTIVE = (args(2) == '1' .or. lowercase(args(2)) == 'on')
                case ('GGEOFLAG')
                    call value(args(2), GGEOFLAG, z)

                !> Basin output files.
                case ('BASINBALANCEOUTFLAG')
                    if (args(2) == '0' .or. lowercase(args(2)) == 'off' .or. lowercase(args(2)) == 'none') then
                        BASINAVGEBFILEFLAG = 'none'
                        BASINAVGWBFILEFLAG = 'none'
                    end if
                case ('BASINAVGEBFILEFLAG')
                    BASINAVGEBFILEFLAG = adjustl(line)
                case ('BASINAVGWBFILEFLAG')
                    BASINAVGWBFILEFLAG = adjustl(line)
                case ('STREAMFLOWOUTFLAG')
                    STREAMFLOWOUTFLAG = adjustl(line)
                case ('REACHOUTFLAG')
                    REACHOUTFLAG = adjustl(line)

                !> Time-averaged basin PEVP-EVAP and EVPB output.
                case ('BASINAVGEVPFILEFLAG')
                    BASINAVGEVPFILEFLAG = 0
                    do j = 2, nargs
                        select case (lowercase(args(j)))
                            case ('daily')
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 1
                            case ('monthly')
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 2
                            case ('hourly')
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 4
                            case ('ts')
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 8
                            case ('all')
                                BASINAVGEVPFILEFLAG = 1
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 2
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 4
                                BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 8
                                exit
                            case ('default')
                                BASINAVGEVPFILEFLAG = 1
                                exit
                            case ('none')
                                BASINAVGEVPFILEFLAG = 0
                                exit
                        end select
                    end do

                case ('BASINSWEOUTFLAG')
                    call value(args(2), BASINSWEOUTFLAG, z)

                !> BASEFLOW routing.
                case ('BASEFLOWFLAG')
                    call bflm_parse_flag(line)

                !> Reservoir Release function flag (Number of WF_B coefficients).
!?                    case ('RESVRELSWFB')
!?                        call value(args(2), WF_RTE_flgs%RESVRELSWFB, z)

                !> Cropland irrigation module.
                case ('CROPLANDIRRIGATION')
                    cifg%ts_flag = 0
                    do j = 2, nargs
                        select case (lowercase(args(j)))
                            case ('daily')
                                cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KDLY)**civ%fk%KDLY
                            case ('hourly')
                                cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KHLY)**civ%fk%KHLY
                            case ('ts')
                                cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KTS)**civ%fk%KTS
                            case ('all')
                                cifg%ts_flag = radix(civ%fk%KDLY)**civ%fk%KDLY
                                cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KHLY)**civ%fk%KHLY
                                cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KTS)**civ%fk%KTS
                                exit
                            case ('default')
                                cifg%ts_flag = radix(civ%fk%KDLY)**civ%fk%KDLY
                                exit
                            case ('none')
                                cifg%ts_flag = 0
                                exit
                        end select
                    end do
                    cifg%PROCESS_ACTIVE = (cifg%ts_flag > 0)

                !> Run mode.
                case ('RUNMODE')
                    do j = 2, nargs
                        select case (lowercase(args(j)))
                            case ('runsvs')
                                svs_mesh%PROCESS_ACTIVE = .true.
                                RUNCLASS36_flgs%PROCESS_ACTIVE = .false.
                            case ('runclass')
                                RUNCLASS36_flgs%PROCESS_ACTIVE = .true.
                                svs_mesh%PROCESS_ACTIVE = .false.
                            case ('nolss')
                                RUNCLASS36_flgs%PROCESS_ACTIVE = .false.
                                svs_mesh%PROCESS_ACTIVE = .false.
                            case ('runrte')
                                WF_RTE_flgs%PROCESS_ACTIVE = .false.
                                rteflg%PROCESS_ACTIVE = .true.
                            case ('noroute')
                                WF_RTE_flgs%PROCESS_ACTIVE = .false.
                                rteflg%PROCESS_ACTIVE = .false.
                                ro%RUNCHNL = .false.
                            case ('default')
                                RUNCLASS36_flgs%PROCESS_ACTIVE = .true.
                                svs_mesh%PROCESS_ACTIVE = .false.
                                WF_RTE_flgs%PROCESS_ACTIVE = .true.
                                rteflg%PROCESS_ACTIVE = .false.
                                exit
                            case ('diagnostic')
                                RUNCLASS36_flgs%PROCESS_ACTIVE = .false.
                                svs_mesh%PROCESS_ACTIVE = .false.
                                WF_RTE_flgs%PROCESS_ACTIVE = .false.
                                rteflg%PROCESS_ACTIVE = .false.
                                exit
                        end select
                    end do

                !> INPUTPARAMSFORMFLAG
                case ('INPUTPARAMSFORMFLAG')
                    INPUTPARAMSFORM = adjustl(lowercase(line))

                !> Unrecognized flag.
                case default
                    call print_screen("WARNING: '" // trim(adjustl(args(1))) // "' is not recognized as a control flag.")
            end select

            !> Check for errors.
            if (z /= 0) then
                call print_screen("WARNING: Unable to parse the options of '" // trim(adjustl(args(1))) // "'.")
            end if
        end do
    end if

    !> Empty lines.
    do i = 1, 2
        read(iun, *, err = 98)
    end do

    !> Output grid points.
    read(iun, '(i5)', err = 98) WF_NUM_POINTS
    if (WF_NUM_POINTS > 10) then
        call print_screen('REMARK: The number of folders for CLASS output is greater than ten and will impact performance.')
    end if
    read (iun, *, err = 98)
    if (WF_NUM_POINTS > 0 .and. RUNCLASS36_flgs%PROCESS_ACTIVE) then
        allocate(op%DIR_OUT(WF_NUM_POINTS), op%N_OUT(WF_NUM_POINTS), &
                 op%II_OUT(WF_NUM_POINTS), op%K_OUT(WF_NUM_POINTS), stat = ierr)

        !> Return if unable to allocate the variables.
        if (ierr /= 0) then
            ECHOTXTMODE = .false.
            call print_error('Unable to allocate variables for CLASS output.')
            write(line, FMT_GEN) WF_NUM_POINTS
            call print_message('Number of points: ' // trim(adjustl(line)))
            return
        end if
        read(iun, *, err = 98) (op%N_OUT(i), i = 1, WF_NUM_POINTS)
        read(iun, *, err = 98) (op%II_OUT(i), i = 1, WF_NUM_POINTS)
        read(iun, *, err = 98) (op%DIR_OUT(i), i = 1, WF_NUM_POINTS)
    else
        read(iun, *, err = 98)
        read(iun, *, err = 98)
        read(iun, *, err = 98)
        allocate(op%DIR_OUT(1), op%N_OUT(1), op%II_OUT(1), op%K_OUT(1))
    end if

    !> Output folder for basin/high-level model output.
    read(iun, *, err = 98)
    read(iun, *, err = 98)
    read(iun, '(a10)', err = 98) line
    call removesp(line)
    fls%GENDIR_OUT = adjustl(line)

    !> Simulation start and stop dates.
    read(iun, *, err = 98)
    read(iun, *, err = 98)
    read(iun, *, err = 98) ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
    read(iun, *, err = 98) ic%stop%year, ic%stop%jday, ic%stop%hour, ic%stop%mins

    !> Close the file.
    close(iun)

    return

98  ierr = 1
    call print_error('Unable to read the file.')
    return

end subroutine
