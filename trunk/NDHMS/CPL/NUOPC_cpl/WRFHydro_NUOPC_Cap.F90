!>
!! @mainpage NCAR's WRF-Hydro NUOPC Cap
!! @author Daniel Rosen (daniel.rosen@noaa.gov)
!! @author ESMF Support (esmf_support@ucar.edu)
!! @date 03/14/2017 WRF-Hydro NUOPC Cap Added to GitHub
!! @date 03/17/2017 Documentation Added
!!
!! @tableofcontents
!!
!! @section Overview Overview
!!
!! The Weather Research and Forecasting Hydrological (WRF-Hydro) model is a
!! hydrometerological forecasting model developed and maintained by the
!! National Center for Atmospheric Research (NCAR). The WRF-Hydro cap wraps
!! the WRF-Hydro model with NUOPC compliant interfaces. The result is a
!! WRF-Hydro model capable of coupling with other models using National
!! Unified Operational Prediction Capability (NUOPC).
!!
!! This page documents the technical design of the specialized NUOPC model and
!! the WRF-Hydro gluecode. For generic NUOPC model documentation please see
!! the NUOPC reference manual: https://www.earthsystemcog.org/projects/nuopc/refmans.
!!
!!
!! @section NuopcSpecialization NUOPC Model Specialized Entry Points
!!
!! This cap specializes the cap configuration, initialization, advertised
!! fields, realized fields, data initialization, clock, run, and finalize.
!!
!! @subsection SetServices Set Services (Register Subroutines)
!!
!! Table summarizing the NUOPC specialized subroutines registered during
!! [SetServices] (@ref WRFHYDRO_NUOPC::SetServices).  The "Phase" column says
!! whether the subroutine is called during the initialization, run, or
!! finalize part of the coupled system run.
!!
!! Phase  |     Cap Subroutine                                | Description
!! -------|---------------------------------------------------|-------------------------------------------------------------
!! Init   | [InitializeP0] (@ref WRFHYDRO_NUOPC::InitializeP0)     | Set the Initialize Phase Definition (IPD). Configure model
!! Init   | [InitializeP1] (@ref WRFHYDRO_NUOPC::InitializeP1)     | Initialize model.  Advertize import and export fields
!! Init   | [InitializeP3] (@ref WRFHYDRO_NUOPC::InitializeP3)     | Realize import and export fields
!! Init   | [DataInitialize] (@ref WRFHYDRO_NUOPC::DataInitialize) | Initialize import and export data
!! Init   | [SetClock] (@ref WRFHYDRO_NUOPC::SetClock)             | Set model clock during initialization
!! Run    | [SetRunClock] (@ref WRFHYDRO_NUOPC::SetRunClock)       | Set model clock during run
!! Run    | [CheckImport] (@ref WRFHYDRO_NUOPC::CheckImport)       | Check timestamp on import data.
!! Run    | [ModelAdvance] (@ref WRFHYDRO_NUOPC::ModelAdvance)     | Advances the model by a timestep
!! Final  | [ModelFinalize] (@ref WRFHYDRO_NUOPC::ModelFinalize)   | Releases memory
!!
!!
!! @section Initialize Initialize
!!
!! Description of the initialization phases and internal model calls.
!! - [InitializeP0] (@ref WRFHYDRO_NUOPC::InitializeP0)
!! - [InitializeP1] (@ref WRFHYDRO_NUOPC::InitializeP1)
!! - [InitializeP3] (@ref WRFHYDRO_NUOPC::InitializeP3)
!! - [DataInitialize] (@ref WRFHYDRO_NUOPC::DataInitialize)
!! - [SetClock] (@ref WRFHYDRO_NUOPC::SetClock)
!!
!! @subsection InitializeP0 InitializeP0
!!
!! During initialize phase 0 the runtime configuration is read in from model
!! attributes and the initialization phase definition version is set to
!! IPDv03.
!!
!! @subsection InitializeP1 InitializeP1
!!
!! During initialize phase 1 the model is initialized and the import and
!! export fields are advertised in a state labeled with the domain ID.
!!
!! @subsection InitializeP3 InitializeP3
!!
!! During initialize phase 3 import and export fields are realized if they are
!! connected through NUOPC. Realized fields are created on the WRF-Hydro grid.
!!
!! @subsection DataInitialize DataInitialize
!!
!! During data initialize this cap checks the timestamp of all import fields
!! dependent on a coupled model.  Once all dependent import fields have been
!! initialized this cap is marked initalized.
!!
!! @subsection SetClock SetClock
!!
!! During set clock the cap creates a new clock using the timestep configured
!! in te WRF-Hydro configuration file. The restart write time step is also
!! created and the restart write time accumulation tracker is reset to zero.
!!
!!
!! @section Run Run
!!
!! Description of the run phase(s) and internal model calls.
!! - [SetRunClock] (@ref WRFHYDRO_NUOPC::SetRunClock)
!! - [CheckImport] (@ref WRFHYDRO_NUOPC::CheckImport)
!! - [ModelAdvance] (@ref WRFHYDRO_NUOPC::ModelAdvance)
!!
!! @subsection SetRunClock SetRunClock
!!
!! During set run clock the model clock and timestep are modified.
!!
!! @subsection CheckImport CheckImport
!!
!! During check import the import data is checked to verify that it is at
!! the beginning or end of the timestep.
!!
!! @subsection ModelAdvance ModelAdvance
!!
!! Calls WRF-Hydro advance for the configured domain.
!!
!!
!! @section Finalize Finalize
!!
!! Description of the finalize phase and internal model calls.
!! - [ModelFinalize] (@ref WRFHYDRO_NUOPC::ModelFinalize)
!!
!! @subsection ModelFinalize ModelFinalize
!!
!! During model finalize WRF-Hydro finalize subroutines are called and memory
!! allocated during cap initialization is released.
!!
!!
!! @section ModelConfiguration Model Configuration
!!
!! Custom model attributes are used to configure the model.
!!
!! Attribute          | Default         | Description
!! -------------------|------------------|-----------------------------------------------------------------------------------
!! Verbosity          | 0                | String, converted into an integer. Bit 16: LIS cap information will be logged.
!! Diagnostic         | 0                | String, converted into an integer. Bit 16: LIS cap diagnostics will be written.
!! realize_all_export | false            | Realize all export fields including non connected fields.
!! config_file        | hydro.namelist   | Override the WRF-Hydro configuration file.
!! das_config_file    | namelist.hrldas  | Override the WRF-Hydro DAS configuration file.
!! forcings_directory | WRFHYDRO_FORCING | Override the WRF-Hydro forcings directory.
!! domain_id          | 1                | Set the WRF-Hydro domain identifier.
!! nest_to_nest       | false            | Turn on nest to nest coupling. Each nest will be identified with an integer.
!! import_dependency  | false            | Data initialization will loop until all import field dependencies are satisfied.
!! output_directory   | [CNAME]_OUTPUT   | Configure the WRF-Hydro Cap output directory.
!! multi_instance_hyd | false            | Run multiple instances of WRF-Hydro, each in a different directory.
!!
!!
!! @section ModelFields Model Fields
!!
!! The following tables list the import and export fields.
!!
!! @subsection ImportFields Import Fields
!!
!! Import fields are listed in the import_list parameter.
!!
!! Standard Name  | Units  | Model Variable  | Description                                | Notes
!! ---------------|--------|-----------------|--------------------------------------------|--------------------------------------
!! dummy_field_1  | Pa     | forcing_1       | field description for first import field   | |
!! dummy_field_2  | kg     | forcing_2       | field description for second import field  | |
!! dummy_field_3  | W m-2  | forcing_3       | field description for third import field   | field notes
!!
!! @subsection ExportField Export Fields
!!
!! Export fields are listed in the export_list parameter.
!!
!! Standard Name  | Units   | Model Variable  | Description                               | Notes
!! ---------------|---------|-----------------|-------------------------------------------|---------------------------
!! dummy_field_1  | m       | output_1        | field description for first export field  | field notes
!! dummy_field_2  | kg      | output_2        | field description for second export field | |
!! dummy_field_3  | m s-1   | output_3        | field description for third export field  | field notes
!!
!!
!! @section MemoryManagement Memory Management
!!
!! Model configuration is stored in a custom internal state data type. A
!! pointer to the custom internal state data type is stored in the component.
!!
!! The cap allocates new memory for each field.  This will be updated so that
!! NUOPC fields directly access the WRF-Hydro field memory.
!!
!! @section IO Input and Output
!!
!! Cap diagnostic output is written to the ESMF PET Logs. Cap diagnostic
!! output can be increased or decreased by setting the Verbosity attribute.
!!
!! NUOPC state restart write files are written depending on the
!! RestartInterval attribute. If set to 0 then NUOPC state restart write files
!! will never be written.
!!
!! WRF-Hydro diagnostics output is written to standard out. To increase the
!! diagnostic output compile WRF-Hydro with -DHYDRO_D.
!!
!! WRF-Hydro writes several output files.  Please see the
!! [WRF-Hydro documentation] (https://www.ral.ucar.edu/projects/wrf_hydro).
!!
!! @section Dependencies Dependencies
!!
!! Dependencies
!! - [ESMF v7.0.0+] (https://www.earthsystemcog.org/projects/esmf/)
!! - [NetCDF v4.3.0+] (http://www.unidata.ucar.edu/software/netcdf/docs/)
!! - [NetCDF FORTRAN] (http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html)
!!
!! @subsection ESMF ESMF
!!
!! See the [ESMF User's Guide]
!! (http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc).
!!
!! @section BuildingAndInstalling Building and Installing
!!
!! Environment Variables
!! - ESMFMKFILE
!!
!! NUOPC Makefile Targets
!! - nuopc
!! - nuopcinstall
!! - nuopcclean
!!
!! The build system in [Makefile] (@ref Makefile) wraps the WRF-Hydro build
!! system and adds the nuopc, nuopcinstall, and nuopcclean targets. Before
!! building make sure to configure the internal model.
!!
!! To build and install into the current directory run:
!!    $ make nuopc
!!
!! To install into an alternative directory run:
!!    $ make nuopcinstall DESTDIR=<INSTALL_DIR> INSTDIR=<SUBDIR>
!!
!! To build with debugging information run:
!!    $ make nuopc DEBUG=on
!!
!! @section Repository
!! The WRF-Hydro NUOPC cap is maintained in a GitHub repository:
!! https://github.com/NESII/wrfhydro_cap
!!
!! @section References
!!
!! - [WRF-Hydro] (https://www.ral.ucar.edu/projects/wrf_hydro)
!! - [ESPS] (https://www.earthsystemcog.org/projects/esps)
!! - [ESMF] (https://www.earthsystemcog.org/projects/esmf)
!! - [NUOPC] (https://www.earthsystemcog.org/projects/nuopc/)

#include "WRFHydro_NUOPC_Macros.h"

module WRFHydro_NUOPC
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock    => label_SetClock, &
    model_label_SetRunClock => label_SetRunClock, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize
  use WRFHYDRO_NUOPC_Gluecode
  use WRFHYDRO_NUOPC_Fields
  use WRFHYDRO_NUOPC_Time
  use WRFHYDRO_NUOPC_Domain
  use WRFHYDRO_NUOPC_Flags
  use WRFHYDRO_ESMF_Logging
  use WRFHydro_ESMF_Extensions

  use module_mpp_land, only: &
    HYDRO_COMM_WORLD
  use module_CPL_LAND, only: &
    cpl_outdate
  use orchestrator_base
#ifdef NOAHMP
  use module_noahmp_hrldas_driver, only: &
    noahmp_ini => land_driver_ini, &
    noahmp_exe => land_driver_exe, &
    noahmp_olddate   => olddate, &
    noahmp_newdate   => newdate, &
    noahmp_startdate => startdate, &
    noahmp_dtbl      => dtbl
  use state_module, only: &
    noahmp_state_type => state_type
#endif

  implicit none

  private

  public SetServices, SetVM

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
#ifdef NOAHMP
    type(noahmp_state_type)  :: noahmp_state
#endif
    logical                  :: realizeAllImport = .FALSE.
    logical                  :: realizeAllExport = .FALSE.
    character(len=64)        :: configFile       = 'hydro.namelist'
    character(len=64)        :: dasConfigFile    = 'namelist.hrldas'
    character(len=128)       :: forcingDir       = 'WRFHYDRO_FORCING'
    integer                  :: did              = 1
    type(cap_domain_type)    :: domain
    logical                  :: nestToNest       = .FALSE.
    type(memory_flag)        :: memr_import      = MEMORY_POINTER
    type(memory_flag)        :: memr_export      = MEMORY_POINTER
    type(fillv_flag)         :: init_import      = FILLV_MODEL
    type(fillv_flag)         :: init_export      = FILLV_MODEL
    type(checkclock_flag)    :: chck_import      = CHECKCLOCK_CURRT
    type(missingval_flag)    :: misg_import      = MISSINGVAL_FAIL
    logical                  :: reset_import     = .FALSE.
    character(len=128)       :: dirOutput        = "./HYD_OUTPUT"
    character(len=128)       :: dirInput         = "./HYD_INPUT"
    logical                  :: writeRestart     = .FALSE.
    logical                  :: multiInstance    = .FALSE.
    integer                  :: nnests           = 1
    type(ESMF_State)         :: NStateImp(1)
    type(ESMF_State)         :: NStateExp(1)
    logical                  :: lsm_forcings(1)  = .FALSE.
    real                     :: dt_cpl0 = UNINITIALIZED
    real                     :: dt_ter0 = UNINITIALIZED
    real                     :: dt_ch0 = UNINITIALIZED
    integer                  :: ft_ter0 = UNINITIALIZED
    integer                  :: ft_ch0 = UNINITIALIZED
  endtype

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of internal state memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, &
      specRoutine=SetRunClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_MethodRemove(gcomp, label=model_label_CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP0"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call WRFHydro_AttributeGet(rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! change directory for multiple instances
    if (is%wrap%multiInstance) then
      if (btest(verbosity,16)) then
        call ESMF_LogWrite(trim(cname)//": Change working directory", &
          ESMF_LOGMSG_INFO)
      endif
      call WRFHYDRO_ESMF_ChDir(trim(cname),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! prepare diagnostics folder
    if (btest(diagnostic,16) .OR. is%wrap%writeRestart) then
      call ESMF_UtilIOMkDir(pathName=trim(is%wrap%dirOutput), &
        relaxedFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine WRFHydro_AttributeGet(rc)
      integer, intent(out)  :: rc

      ! local variables
      logical                    :: configIsPresent
      type(ESMF_Config)          :: config
      type(NUOPC_FreeFormat)     :: attrFF
      character(32)              :: atName
      logical                    :: atPres
      character(32)              :: atVal
      character(ESMF_MAXSTR)     :: logMsg

      ! check gcomp for config
      call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      if (configIsPresent) then
        ! read and ingest free format component attributes
        call ESMF_GridCompGet(gcomp, config=config, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        attrFF = NUOPC_FreeFormatCreate(config, &
          label=trim(cname)//"_attributes::", relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! Realize all import fields
      atName="realize_all_import"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%realizeAllImport = (trim(atVal)=="TRUE")
      endif

      ! Realize all export fields
      atName="realize_all_export"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%realizeAllExport = (trim(atVal)=="TRUE")
      endif

      ! Determine hydro configuration filename
      atName="config_file"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%configFile = atVal
      endif

      ! Determine DAS configuration filename
      atName="das_config_file"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%dasConfigFile = atVal
      endif

      ! Forcing Directory
      atName="forcings_directory"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%forcingDir = trim(atVal)
      endif

      ! Determine Domain ID
      atName="did"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%did = ESMF_UtilString2Int(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! Connect Nest to Nest
      atName="nest_to_nest"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%nestToNest = (trim(atVal)=="TRUE")
      endif

      ! import data memory type
      atName="field_memory_import"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%memr_import = atVal
      endif

      ! export data memory type
      atName="field_memory_export"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%memr_export = atVal
      endif

      ! import data initialization type
      atName="initialize_import"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%init_import = atVal
      endif

      ! backwards compatible setting (overrides initialize_import)
      atName="import_dependency"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        if (trim(atVal)=="TRUE") is%wrap%init_import = FILLV_DEPENDENCY
      endif

      ! export data initialization type
      atName="initialize_export"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%init_export = atVal
      endif

      ! backwards compatible setting (overrides initialize_export)
      atName="read_restart"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        if (trim(atVal)=="TRUE") is%wrap%init_export = FILLV_FILE
      endif

      ! Get check import
      atName="check_import"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%chck_import = atVal
      endif

      ! Get missing import handler
      atName="missing_import"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%misg_import = atVal
      endif

      ! Get reset import handler
      atName="reset_import"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%reset_import = (trim(atVal)=="TRUE")
      endif

      ! Get component output directory
      atName="output_directory"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%dirOutput = trim(atVal)
      endif

      ! Get component input directory
      atName="input_directory"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%dirInput = trim(atVal)
      endif

      ! Write cap restart state
      atName="write_restart"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%writeRestart = (trim(atVal)=="TRUE")
      endif

      ! Determine Import Dependency
      atName="multi_instance_hyd"
      call NUOPC_CompAttributeGet(gcomp, name=atName, isPresent=atPres, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (atPres) then
        call NUOPC_CompAttributeGet(gcomp, name=atName, value=atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        atVal = ESMF_UtilStringUpperCase(atVal, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        is%wrap%multiInstance = (trim(atVal)=="TRUE")
      endif

      if (btest(verbosity,16)) then
        call ESMF_LogWrite(trim(cname)//": Settings",ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Verbosity              = ",verbosity
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Diagnostic             = ",diagnostic
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//": ", &
          "Realze All Imports     = ",is%wrap%realizeAllImport
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//": ", &
          "Realze All Exports     = ",is%wrap%realizeAllExport
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Config File            = ",trim(is%wrap%configFile)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "DAS Config File        = ",trim(is%wrap%dasConfigFile)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Forcing Directory      = ",trim(is%wrap%forcingDir)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Domain ID              = ",is%wrap%did
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//": ", &
          "Nest To Nest           = ",is%wrap%nestToNest
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        atVal = is%wrap%memr_import
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Field Memory Import    = ",trim(atVal)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        atVal = is%wrap%memr_export
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Field Memory Export    = ",trim(atVal)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        atVal = is%wrap%init_import
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Initialize Import      = ",trim(atVal)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        atVal = is%wrap%init_export
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Initialize Export      = ",trim(atVal)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        atVal = is%wrap%chck_import
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Check Imports          = ",trim(atVal)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        atVal = is%wrap%misg_import
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Missing Imports        = ",trim(atVal)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Reset Import           = ",is%wrap%reset_import
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Output Directory       = ",trim(is%wrap%dirOutput)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Input Directory        = ",trim(is%wrap%dirInput)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Write Restart          = ",is%wrap%writeRestart
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Multiple Instances     = ",is%wrap%multiInstance
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      endif

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="InitializeP1"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    type(ESMF_VM)               :: vm
    integer                     :: fIndex
    integer                     :: ntime

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! initialize wrfhydro
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    call orchestrator%init()

#ifdef NOAHMP
    call noahmp_ini(ntime, is%wrap%noahmp_state)
#else
    call wrfhydro_nuopc_ini(is%wrap%did,vm,is%wrap%forcingDir, &
      is%wrap%domain, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
#endif

    call WRFHYDRO_DomainInit(is%wrap%did,vm,is%wrap%domain,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    if (btest(verbosity,16)) then
      call WRFHYDRO_log_nlst(cname,is%wrap%did,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call WRFHYDRO_log_rtdomain(cname,is%wrap%did,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call WRFHYDRO_log_noahlsm(cname,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call WRFHYDRO_DomainLog(cname,is%wrap%domain,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
    endif

    ! add namespace
    if(.NOT.is%wrap%nestToNest) then
      is%wrap%NStateImp(1) = importState
      is%wrap%NStateExp(1) = exportState
    else
      call NUOPC_AddNestedState(importState, &
        CplSet=trim(is%wrap%domain%label), &
        nestedStateName="NestedStateImp_N"//trim(is%wrap%domain%label), &
        nestedState=is%wrap%NStateImp(1), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_AddNestedState(exportState, &
        CplSet=trim(is%wrap%domain%label), &
        nestedStateName="NestedStateExp_N"//trim(is%wrap%domain%label), &
        nestedState=is%wrap%NStateExp(1), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    call field_dictionary_add(fieldList=cap_fld_list, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    !!
    !! advertise import and export fields
    !!
    call field_advertise(fieldList=cap_fld_list, &
      importState=is%wrap%NStateImp(1), &
      exportState=is%wrap%NStateExp(1), &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (btest(verbosity,16)) call field_advertise_log(cap_fld_list,cname,rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: gcomp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP3"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    type(ESMF_Field)           :: field
    logical                    :: importConnected, exportConnected
    integer                    :: fIndex

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (btest(verbosity,16)) then
      call WRFHYDRO_ESMF_LogGrid(is%wrap%domain%grid, &
        trim(cname)//"_"//rname//"_D"//trim(is%wrap%domain%label),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! Write grid to NetCDF file.
    if (btest(diagnostic,16)) then
      call WRFHYDRO_ESMF_GridWrite(is%wrap%domain%grid, &
        trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
        rname//'_grid_D'//trim(is%wrap%domain%label)//".nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    call field_realize(fieldList=cap_fld_list, &
      importState=is%wrap%NStateImp(1), &
      exportState=is%wrap%NStateExp(1), &
      grid=is%wrap%domain%grid, did=is%wrap%did, &
      realizeAllImport=is%wrap%realizeAllImport, &
      realizeAllExport=is%wrap%realizeAllExport, &
      memr_import=is%wrap%memr_import, &
      memr_export=is%wrap%memr_export, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    is%wrap%lsm_forcings(1) = check_lsm_forcings(is%wrap%NStateImp(1),rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (btest(verbosity,16)) call field_realize_log(cap_fld_list,cname,rc=rc)
    if (btest(verbosity,16)) call LogMode()

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    !---------------------------------------------------------------------------

    subroutine LogMode()
      ! local variables
      character(ESMF_MAXSTR)     :: logMsg
      character(len=64)          :: modeStr

      if(is%wrap%lsm_forcings(1)) then
        modeStr = "WRFHYDRO_Coupled"
      else
        modeStr = "WRFHYDRO_Offline"
      endif
      write (logMsg, "(A,(A,A))") trim(cname)//": ", &
        "Mode = ",trim(modeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)                          :: cname
    character(*), parameter                :: rname="DataInitialize"
    integer                                :: verbosity, diagnostic
    character(len=64)                      :: value
    type(type_InternalState)               :: is
    type(ESMF_Clock)                       :: modelClock
    type(ESMF_Time)                        :: currTime
    type(ESMF_Time)                        :: invalidTime
    character(len=32)                      :: currTimeStr
    logical                                :: importCurrent
    logical                                :: importUpdated
    logical                                :: exportUpdated
    character(len=32)                      :: initTypeStr
    integer                                :: stat

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! set up invalid time (by convention)
    call ESMF_TimeSet(invalidTime, yy=99999999, mm=01, dd=01, &
      h=00, m=00, s=00, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! get the current time out of the clock
    call ESMF_ClockGet(modelClock, currTime=currTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! initialize import state
    if (is%wrap%init_import.eq.FILLV_MISSING) then
      call state_fill_uniform(is%wrap%NStateImp(1), &
        fillValue=ESMF_MISSING_VALUE, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_SetTimestamp(is%wrap%NStateImp(1), time=invalidTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      importUpdated = .TRUE.
    elseif (is%wrap%init_import.eq.FILLV_ZERO) then
      call state_fill_uniform(is%wrap%NStateImp(1), &
        fillValue=0.0_ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_SetTimestamp(is%wrap%NStateImp(1), time=invalidTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      importUpdated = .TRUE.
    elseif (is%wrap%init_import.eq.FILLV_DEPENDENCY) then
      importCurrent = NUOPC_IsAtTime(is%wrap%NStateImp(1), &
        time=currTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      if (importCurrent) then
        call ESMF_LogWrite( &
          trim(cname)//': '//rname//' Initialize-Data-Dependency SATISFIED!!! DID='//trim(is%wrap%domain%label), &
          ESMF_LOGMSG_INFO)
        importUpdated = .TRUE.
      else
        call ESMF_LogWrite( &
          trim(cname)//': '//rname//' Initialize-Data-Dependency NOT YET SATISFIED!!! DID='//trim(is%wrap%domain%label), &
          ESMF_LOGMSG_INFO)
        importUpdated = .FALSE.
      endif
    elseif (is%wrap%init_import.eq.FILLV_PRESCRIBE) then
      call state_fill_prescribe(is%wrap%NStateImp(1), &
        fieldList=cap_fld_list, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      importUpdated = .TRUE.
    elseif (is%wrap%init_import.eq.FILLV_FILE) then
      call state_fill_file(is%wrap%NStateImp(1), &
        filePrefix=trim(is%wrap%dirInput)//"/restart_"//trim(cname)// &
          "_imp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_SetTimestamp(is%wrap%NStateImp(1), time=currTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      importUpdated = .TRUE.
    elseif (is%wrap%init_import.eq.FILLV_MODEL) then
      if (is%wrap%memr_import.eq.MEMORY_COPY) then
        call state_copy_frhyd(is%wrap%NStateImp(1), is%wrap%did, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      if (WRFHYDRO_isRestart(is%wrap%did)) then
        call NUOPC_SetTimestamp(is%wrap%NStateImp(1), time=currTime, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      else
        call NUOPC_SetTimestamp(is%wrap%NStateImp(1), time=invalidTime, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      importUpdated = .TRUE.
    else
      initTypeStr = is%wrap%init_import
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Import data initialize routine unknown "//trim(initTypeStr), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
      importUpdated = .FALSE.
    endif

    ! initialize export state
    if (is%wrap%init_export.eq.FILLV_MISSING) then
      call state_fill_uniform(is%wrap%NStateExp(1), &
        fillValue=ESMF_MISSING_VALUE, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_SetTimestamp(is%wrap%NStateExp(1), time=invalidTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      exportUpdated = .TRUE.
    elseif (is%wrap%init_export.eq.FILLV_ZERO) then
      call state_fill_uniform(is%wrap%NStateExp(1), &
        fillValue=0.0_ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_SetTimestamp(is%wrap%NStateExp(1), time=invalidTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      exportUpdated = .TRUE.
    elseif (is%wrap%init_export.eq.FILLV_PRESCRIBE) then
      call state_fill_prescribe(is%wrap%NStateExp(1), &
        fieldList=cap_fld_list, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      exportUpdated = .TRUE.
    elseif (is%wrap%init_export.eq.FILLV_FILE) then
      call state_fill_file(is%wrap%NStateExp(1), &
        filePrefix=trim(is%wrap%dirInput)//"/restart_"//trim(cname)// &
          "_exp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call NUOPC_SetTimestamp(is%wrap%NStateExp(1), time=currTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      exportUpdated = .TRUE.
    elseif (is%wrap%init_export.eq.FILLV_MODEL) then
      if (is%wrap%memr_export.eq.MEMORY_COPY) then
        call state_copy_frhyd(is%wrap%NStateExp(1), is%wrap%did, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      if (WRFHYDRO_isRestart(is%wrap%did)) then
        call NUOPC_SetTimestamp(is%wrap%NStateExp(1), time=currTime, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      else
        call NUOPC_SetTimestamp(is%wrap%NStateExp(1), time=invalidTime, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      exportUpdated = .TRUE.
    else
      initTypeStr = is%wrap%init_export
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Export data initialize routine unknown "//trim(initTypeStr), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
      exportUpdated = .FALSE.
    endif

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    if (importUpdated.AND.exportUpdated) then
      call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      ! Write initialization files
      if (btest(diagnostic,16)) then
        call NUOPC_Write(is%wrap%NStateImp(1), &
          fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
            rname//"_imp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr)//"_", &
          overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_Write(is%wrap%NStateExp(1), &
          fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
            rname//"_exp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr)//"_", &
          overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="SetClock"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: startTime
    type(ESMF_TimeInterval)    :: modelTimeStep
    character(len=19)          :: startTimeStr
    logical                    :: checkStartTime

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the Component for time
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_ClockGet(modelClock, startTime=startTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call WRFHYDRO_time_toString(startTime, timestr=startTimeStr, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

#ifdef NOAHMP
    ! Check LSM start time
    checkStartTime = LSM_IsAtTime(startTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (.not. checkStartTime) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Driver and HRLDAS start times do not match. "// &
            "check start time configuration!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
#endif

    ! Set WRFHYDRO start time
    read (startTimeStr(1:4),"(I4)")   nlst(is%wrap%did)%START_YEAR
    read (startTimeStr(6:7),"(I2)")   nlst(is%wrap%did)%START_MONTH
    read (startTimeStr(9:10),"(I2)")  nlst(is%wrap%did)%START_DAY
    read (startTimeStr(12:13),"(I2)") nlst(is%wrap%did)%START_HOUR
    read (startTimeStr(15:16),"(I2)") nlst(is%wrap%did)%START_MIN
    nlst(is%wrap%did)%startdate(1:19) = startTimeStr(1:19)
    nlst(is%wrap%did)%olddate(1:19)   = startTimeStr(1:19)
    cpl_outdate = startTimeStr(1:19)

    ! Set Model Time Step
    call ESMF_TimeIntervalSet(modelTimeStep, &
      s_r8=real(nlst(is%wrap%did)%dt, ESMF_KIND_R8), &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call NUOPC_CompSetClock(gcomp, modelClock, modelTimeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (btest(verbosity,16)) then
      call WRFHYDRO_log_nlst(cname,is%wrap%did,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call WRFHYDRO_log_rtdomain(cname,is%wrap%did,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call WRFHYDRO_log_noahlsm(cname,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call WRFHYDRO_DomainLog(cname,is%wrap%domain,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call LogClock()
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine LogClock()
      ! local variables
      character(ESMF_MAXSTR)     :: logMsg
      type(ESMF_Time)            :: currTime
      type(ESMF_TimeInterval)    :: timestep
      character(len=64)          :: currTimeStr
      character(len=64)          :: timestepStr

      call ESMF_ClockGet(modelClock, &
        currTime=currTime,timeStep=timestep,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_TimeGet(currTime, &
        timeString=currTimeStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_TimeIntervalGet(timestep, &
        timeString=timestepStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      write (logMsg, "(A,(A,A))") trim(cname)//": ", &
        "Current Time = ",trim(currTimeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A))") trim(cname)//": ", &
        "Time Step    = ",trim(timestepStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="SetRunClock"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    type(ESMF_Clock)            :: driverClock
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    type(ESMF_TimeInterval)     :: timeStep
    character(len=19)           :: currTimeStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, driverClock=driverClock, &
      modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! get the curr time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! check and set the model clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call WRFHYDRO_time_toString(modelClock, timestr=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    nlst(is%wrap%did)%olddate(1:19) = currTimeStr(1:19)

    call ESMF_ClockGet(modelClock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    nlst(is%wrap%did)%dt = WRFHYDRO_interval_toReal(timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    if(nlst(is%wrap%did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Timestep less than 1 is not supported!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if(is%wrap%dt_cpl0 .ne. nlst(is%wrap%did)%dt) then
      call ESMF_LogWrite(cname//": Driver timestep changed.",ESMF_LOGMSG_INFO)
      if(nlst(is%wrap%did)%dtrt_ter .ge. nlst(is%wrap%did)%dt) then
         nlst(is%wrap%did)%dtrt_ter = nlst(is%wrap%did)%dt
         is%wrap%ft_ter0 = 1
      else
         is%wrap%ft_ter0 = nlst(is%wrap%did)%dt/nlst(is%wrap%did)%dtrt_ter
         if (is%wrap%ft_ter0*nlst(is%wrap%did)%dtrt_ter .lt. nlst(is%wrap%did)%dt) &
           nlst(is%wrap%did)%dtrt_ter = nlst(is%wrap%did)%dt/is%wrap%ft_ch0
      endif
      if(nlst(is%wrap%did)%dtrt_ch .ge. nlst(is%wrap%did)%dt) then
        nlst(is%wrap%did)%dtrt_ch = nlst(is%wrap%did)%dt
        is%wrap%ft_ch0 = 1
      else
        is%wrap%ft_ch0 = nlst(is%wrap%did)%dt/nlst(is%wrap%did)%dtrt_ch
        if(is%wrap%ft_ch0*nlst(is%wrap%did)%dtrt_ch .lt. nlst(is%wrap%did)%dt) &
          nlst(is%wrap%did)%dtrt_ch = nlst(is%wrap%did)%dt/is%wrap%ft_ch0
      endif
      is%wrap%dt_cpl0 = nlst(is%wrap%did)%dt
      is%wrap%dt_ter0 = nlst(is%wrap%did)%dtrt_ter
      is%wrap%dt_ch0  = nlst(is%wrap%did)%dtrt_ch
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="CheckImport"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    integer                     :: nIndex
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    logical                     :: allCurrTime

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! get the curr time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! check that Fields in the importState show correct timestamp

    allCurrTime = NUOPC_IsAtTime(is%wrap%NStateImp(1), modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (.not.allCurrTime) then
      call ESMF_LogWrite(trim(cname)//": NUOPC INCOMPATIBILITY DETECTED: "// &
        "Import Fields not at correct time", &
        ESMF_LOGMSG_WARNING)
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="ModelAdvance"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime, advEndTime
    character(len=32)           :: currTimeStr, advEndTimeStr, timeStepStr
    type(ESMF_TimeInterval)     :: timeStep
    integer(ESMF_KIND_I8)       :: advanceCount
    character(len=16)           :: misgValTypeStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the component for its clock, importState, and exportState
    call NUOPC_ModelGet(gcomp, &
      modelClock=modelClock, &
      importState=importState, &
      exportState=exportState, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the clock for its current time and timestep
    call ESMF_ClockGet(modelClock, &
      currTime=currTime, timeStep=timeStep, advanceCount=advanceCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    advanceCount = advanceCount + 1
    advEndTime = currTime + timeStep
    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_TimeGet(advEndTime, timeString=advEndTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_TimeIntervalGet(timeStep, timeString=timeStepStr,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! Write import files
    if (btest(diagnostic,16)) then
      call NUOPC_Write(is%wrap%NStateImp(1), &
        fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
          rname//"_imp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr)//"_", &
        overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    if (is%wrap%memr_import.eq.MEMORY_COPY) then
      call state_copy_tohyd(is%wrap%NStateImp(1), is%wrap%did, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    if (is%wrap%misg_import.eq.MISSINGVAL_FAIL) then
      call state_check_missing(is%wrap%NStateImp(1), did=is%wrap%did, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    elseif (is%wrap%misg_import.eq.MISSINGVAL_PRESCRIBE) then
      call state_prescribe_missing(is%wrap%NStateImp(1), did=is%wrap%did, &
        fieldList=cap_fld_list, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    elseif (is%wrap%misg_import.eq.MISSINGVAL_IGNORE) then
!     DO NOTHING
    else
      misgValTypeStr = is%wrap%misg_import
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Unknown missing value handler "//trim(misgValTypeStr), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (btest(diagnostic,16)) then
      call model_debug(is%wrap%NStateImp(1), did=is%wrap%did, &
        memflg=is%wrap%memr_import, &
        filePrefix=trim(is%wrap%dirOutput)//"/wrfhydro_"// &
          rname//"_imp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr)//"_", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    if (btest(verbosity,16)) then
      call LogAdvance()
    endif

#ifdef NOAHMP
    call noahmp_exe(int(advanceCount), is%wrap%noahmp_state)
#else
    call wrfhydro_nuopc_run(is%wrap%did,is%wrap%lsm_forcings(1), &
      modelClock,is%wrap%NStateImp(1),is%wrap%NStateExp(1),rc)
    if(ESMF_STDERRORCHECK(rc)) return
#endif

    if (is%wrap%memr_export.eq.MEMORY_COPY) then
      call state_copy_frhyd(is%wrap%NStateExp(1), is%wrap%did, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    if (is%wrap%reset_import) then
      if ((is%wrap%memr_import.eq.MEMORY_POINTER) .AND. &
          (is%wrap%memr_export.eq.MEMORY_POINTER)) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg="Cannot reset import field if pointer is shared with export.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      else
        call state_fill_uniform(is%wrap%NStateImp(1), &
          fillValue=ESMF_MISSING_VALUE, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
    endif

    ! Write export files
    if (btest(diagnostic,16)) then
      call NUOPC_Write(is%wrap%NStateExp(1), &
        fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
          rname//"_exp_D"//trim(is%wrap%domain%label)//"_"//trim(advEndTimeStr)//"_", &
        overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine LogAdvance()
      ! local variables
      character(ESMF_MAXSTR)     :: logMsg
      character(len=32)          :: nModeStr

      if (is%wrap%lsm_forcings(is%wrap%did)) then
        nModeStr = "WRFHYDRO_Coupled"
      else
        nModeStr = "WRFHYDRO_Offline"
      endif

      write (logMsg, "(A,I0)") trim(cname)//': ModelAdvance did=',is%wrap%did
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A))") trim(cname)//': ', &
        '  Mode         = ',trim(nModeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A))") trim(cname)//': ', &
        '  Current Time = ',trim(currTimeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A))") trim(cname)//': ', &
        '  Time Step    = ',trim(timestepStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
        '  NTIME        = ',advanceCount
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(gcomp,rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local Variables
    character(32)              :: cname
    character(*), parameter    :: rname="ModelFinalize"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: currTime
    character(len=32)          :: currTimeStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min    ","max    ","bit16  ","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_ClockGet(modelClock, currTime=currTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! Write export file
    if (is%wrap%writeRestart) then
      call NUOPC_Write(is%wrap%NStateExp(1), &
        fileNamePrefix=trim(is%wrap%dirOutput)//"/restart_"//trim(cname)// &
          "_exp_D"//trim(is%wrap%domain%label)//"_"//trim(currTimeStr)//"_", &
          overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    call wrfhydro_nuopc_fin(is%wrap%did,rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of internal state memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

  end subroutine

end module
