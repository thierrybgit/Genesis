# 1 "hardwareinfo.f90"
# 1 "../lib/hardwareinfo.fpp"
# 1 "<built-in>"
# 1 "<command-line>"
# 23 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1

# 17 "/usr/include/stdc-predef.h" 3 4











































# 1 "../lib/hardwareinfo.fpp"
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   hardwareinfo_mod
!> @brief   utilities for get hardware information
!! @authors Chigusa Kobayashi (CK), Motoshi Kamiya (MK)
!
!  (c) Copyright 2014 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8





# 50
module hardwareinfo_mod

  use fileio_str_mod
  use fileio_mod
  use string_mod
  use constants_mod
  use messages_mod



  use mpi

  use mpi_parallel_mod
  use, intrinsic :: iso_c_binding


  implicit none
  private

  character(13), parameter  :: cpuinfo = "/proc/cpuinfo"

# 125 "../lib/hardwareinfo.fpp"
# 125
  type, bind(C) :: f_hipDeviceArch
    integer(C_INT)        :: hasGlobalInt32Atomics
    integer(C_INT)        :: hasGlobalFloatAtomicExch
    integer(C_INT)        :: hasSharedInt32Atomics
    integer(C_INT)        :: hasSharedFloatAtomicExch
    integer(C_INT)        :: hasFloatAtomicAdd
    integer(C_INT)        :: hasGlobalInt64Atomics
    integer(C_INT)        :: hasSharedInt64Atomics
    integer(C_INT)        :: hasDoubles
    integer(C_INT)        :: hasWarpVote
    integer(C_INT)        :: hasWarpBallot
    integer(C_INT)        :: hasWarpShuffle
    integer(C_INT)        :: hasFunnelShift
    integer(C_INT)        :: hasThreadFenceSystem
    integer(C_INT)        :: hasSyncThreadsExt
    integer(C_INT)        :: hasSurfaceFuncs
    integer(C_INT)        :: has3dGrid
    integer(C_INT)        :: hasDynamicParallelism
  end type

  type, bind(C) :: f_hipDeviceProp
    character(1)          :: name(256)
    integer(C_SIZE_T)     :: totalGlobalMem
    integer(C_SIZE_T)     :: sharedMemPerBlock
    integer(C_INT)        :: regsPerBlock
    integer(C_INT)        :: warpSize
    integer(C_INT)        :: maxThreadsPerBlock
    integer(C_INT)        :: maxThreadsDim(3)
    integer(C_INT)        :: maxGridSize(3)
    integer(C_INT)        :: clockRate
    integer(C_INT)        :: memoryClockRate
    integer(C_INT)        :: memoryBusWidth
    integer(C_SIZE_T)     :: totalConstMem
    integer(C_INT)        :: major
    integer(C_INT)        :: minor
    integer(C_INT)        :: multiProcessorCount
    integer(C_INT)        :: l2CacheSize
    integer(C_INT)        :: maxThreadsPerMultiProcessor
    integer(C_INT)        :: computeMode
    integer(C_INT)        :: clockInstructionRate
    type(f_hipDeviceArch) :: arch
    integer(C_INT)        :: concurrentKernels
    integer(C_INT)        :: pciBusID
    integer(C_INT)        :: pciDeviceID
    integer(C_SIZE_T)     :: maxSharedMemoryPerMultiProcessor
    integer(C_INT)        :: isMultiGpuBoard
    integer(C_INT)        :: canMapHostMemory
    integer(C_INT)        :: gcnArch
  end type

  interface
    integer function cudaGetDeviceProperties(hipDeviceProp, dev) BIND(C, name="hipGetDeviceProperties")
      use, intrinsic :: iso_c_binding

      import

      type(f_hipDeviceProp), intent(out) :: hipDeviceProp
      integer, value,         intent(in) :: dev

    end function cudaGetDeviceProperties

    integer function cudaGetDeviceCount(count) BIND(C, name="hipGetDeviceCount")
      integer, intent(out) :: count
    end function cudaGetDeviceCount

    integer function cudaSetDevice(dev) BIND(C, name="hipSetDevice")
      integer, value, intent(in) :: dev
    end function cudaSetDevice
  end interface



  public  :: hw_information
  private :: runtime_information
  private :: build_information
  private :: genesis_information
  public  :: get_cpu_information
  public  :: get_cpu_flags

  public  :: assign_gpu
  private :: get_local_rank
  private :: get_gpu_information

  private :: get_env_information

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    hw_information
  !> @brief        write architecture and compile information
  !! @param[in]    get_arch_infomation
  !! @authors      CK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine hw_information

    call genesis_information

    call build_information

    call runtime_information

    return

  end subroutine hw_information

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    runtime_information
  !> @brief        write runtime information
  !! @authors      CK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine runtime_information

    ! local variables
    character(MaxLine)       :: ldlibrary,  rt_user, rt_host
    character(MaxLine)       :: mpiversion
    character(80)            :: cpuname
    integer                  :: tval(8)
    integer                  :: leng, ierr
    integer(8)               :: year, month, day, hour, minute, sec





    type(f_hipDeviceProp)    :: gpu

    integer                  :: itmp
    character(256)           :: gpu_modelname
    character(256)           :: gpu_major, gpu_minor
    logical                  :: gpu_ecc




    write(MsgOut,'(A)') 'Runtime_Information> Machine and Library Information'

    call date_and_time(values=tval)
    year   = int(tval(1),kind=8)
    month  = int(tval(2),kind=8)
    day    = int(tval(3),kind=8)
    hour   = int(tval(5),kind=8)
    minute = int(tval(6),kind=8)
    sec    = int(tval(7),kind=8)
    write(MsgOut,'(A,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)') &
    '  date       = ',&
      year,"/",month,"/",day," ",hour,":",minute,":", sec

    call get_cpu_information(cpuname)
    write(MsgOut,*) ' cpu model    = ',trim(cpuname)


    call get_gpu_information(gpu)
    gpu_modelname(1:256) = ' '




    gpu_ecc = .false.

    do itmp = 1, 256
      if (gpu%name(itmp) == CHAR(0)) then
        exit
      end if
      gpu_modelname(itmp:itmp) = gpu%name(itmp)
    end do
    write(gpu_major,'(i8)') gpu%major
    write(gpu_minor,'(i8)') gpu%minor
    write(MsgOut,*) ' gpu model    = ',trim(gpu_modelname), &
                    " (CC ", trim(adjustl(gpu_major)), ".", &
                    trim(adjustl(gpu_minor)), ")"
    write(MsgOut,*) ' gpu ECC      = ',gpu_ecc

    call get_env_information(ldlibrary,rt_host,rt_user)
    write(MsgOut,*) ' exec. host   = ',trim(rt_user),'@',trim(rt_host)
    write(MsgOut,*) ' LD library   = ',trim(ldlibrary)


    call MPI_GET_LIBRARY_VERSION(mpiversion, leng, ierr)
    if (trim(mpiversion) .ne. "") then
      write(MsgOut,*) 'MPI Runtime = ',trim(mpiversion)
    endif

    write(MsgOut,'(A)')

    return

  end subroutine runtime_information

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    genesis_information
  !> @brief        write GENESIS information
  !! @authors      CK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine genesis_information

    write(MsgOut,'(A)') 'GENESIS_Information> GENESIS Information'
    write(MsgOut,*) ' version      = ',1
    write(MsgOut,*) ' commit ID    = ',1
    write(MsgOut,*) ' precision    = ',precision_char

    write(MsgOut,*) ' nonbonding   = GPU'



    write(MsgOut,'(A)')

    return

  end subroutine genesis_information

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    build_information
  !> @brief        write build information
  !! @authors      CK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine build_information

    write(MsgOut,'(A)') 'Build_Information> Compiler Information'
    write(MsgOut,*) ' build host   = ',1,'@',1
    write(MsgOut,*) ' fortran      = ',1
    write(MsgOut,*) ' option       = ',1
    write(MsgOut,*) ' C            = ',1
    write(MsgOut,*) ' option       = ',1
    write(MsgOut,*) ' defined var. = ',1
    write(MsgOut,*) ' link option  = ',1

    write(MsgOut,*) ' MPI Compiler = ',1,' MPI'





    write(MsgOut,*) ' HIP          = ',1

    write(MsgOut,'(A)')

    return

  end subroutine build_information

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    get_env_information
  !> @brief        write Enviroment information
  !! @authors      CK
  !! @param[out]   ldlibrary    : LD_LIBRARY_PATH
  !! @param[out]   rt_host      : host
  !! @param[out]   rt_user      : user
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine get_env_information(ldlibrary, rt_host, rt_user)









    ! formal arguments
    character(*),            intent(out)    :: ldlibrary
    character(*),            intent(out)    :: rt_host
    character(*),            intent(out)    :: rt_user

    call getenv("LD_LIBRARY_PATH",ldlibrary)
    call getenv("USER",rt_user)
    call getenv("HOSTNAME",rt_host)

    return

  end subroutine get_env_information

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    get_cpu_information
  !> @brief        write CPU information
  !! @param[out]   cpuname    :  cpu model name
  !! @authors      CK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine get_cpu_information(cpuname)

    ! formal arguments
    character(*),            intent(out)    :: cpuname

    ! local variables
    integer       :: i, inchar, file
    character(80) :: line
    logical       :: exists


    exists=.false.





    inquire(file=cpuinfo, EXIST=exists)
    if (.not. exists) then
      cpuname="N/A"
      return
    endif

    call open_file(file, cpuinfo, IOFileInput)

    do while(.true.)
      read(file, '(A80)',end=100,err=100) line
      inchar = index(line,":")
      if (inchar > 0) then
        if (line(1:10) == 'model name') then
          read(line(inchar+2:80),'(A)') cpuname
          exit
        else if (inchar == 6 .and. line(1:3) == 'cpu') then
          read(line(inchar+2:80),'(A)') cpuname
          exit
        end if
      end if
    end do
100 continue

    call close_file(file)

    return

  end subroutine get_cpu_information

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    get_cpu_flags
  !> @brief        get CPU flags
  !! @param[out]   cpuflags    :  cpu flags
  !! @authors      NT
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine get_cpu_flags(cpuflags)

    ! formal arguments
    character(*),            allocatable   :: cpuflags(:)

    ! local variables
    integer                  :: i, inchar, file, cnt
    character(1000)          :: line, flags
    logical                  :: exists


    exists=.false.

    inquire(file=cpuinfo, EXIST=exists)
    if (.not. exists) then
      allocate(cpuflags(1))
      cpuflags(1) = "N/A"
      return
    end if

    call open_file(file, cpuinfo, IOFileInput)

    flags = ''
    do while(.true.)
      read(file, '(a)', end=100) line
      inchar = index(line,":")
      if (inchar > 0) then
        if (line(1:5) == 'flags') then
          read(line(inchar+2:),'(A)') flags
          exit
        end if
      end if
    end do

100 call close_file(file)

    if (flags == '') then
      allocate(cpuflags(1))
      cpuflags(1) = "N/A"
      return
    end if

    cnt = split_num(flags)
    allocate(cpuflags(cnt))
    call split(cnt, cnt, flags, cpuflags)

    return

  end subroutine get_cpu_flags


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    assign_gpu
  !> @brief        assign GPU for each process
  !! @authors      MK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine assign_gpu(my_device_id)

    ! formal arguments
    integer, intent(out) :: my_device_id

    ! local variables
    integer :: ret, mycount
    integer :: counter, i, not_avail




    type(f_hipDeviceProp) :: gpu

    logical, allocatable :: avail_gpus(:)


    my_device_id = 0





    ret = cudaGetDeviceCount(mycount)
    if (ret /= 0) then
      call error_msg('get_gpu_info> Error: cannot get gpu device count.')
    end if

    not_avail = 0
    if (mycount > 0) then
      allocate(avail_gpus(mycount))
      avail_gpus(:) = .true.
    end if

# 590 "../lib/hardwareinfo.fpp"

# 591
    if (main_rank) then
      write(MsgOut,'(a,i5)') "  # of GPUs    =", mycount
    end if
    ! if only 1 GPU is found, do nothing
    if (mycount == 1) return

    ! put error if no gpu found
    if (mycount == 0) then




      call error_msg('get_gpu_info> HIP enabled GPU is not available')

    endif

    call get_local_rank()
    my_device_id = mod(my_node_local_rank,mycount)

    counter = -1
    do i = 1, mycount + not_avail
      if (avail_gpus(i)) counter = counter + 1
      if (counter == my_device_id) then
        my_device_id = i - 1
        exit
      end if
      if (i == (mycount + not_avail)) then
        call error_msg('get_gpu_info> Error: unexpected error')
      end if
    end do

    ! assign device
    ret = cudaSetDevice(my_device_id)
    if (ret /= 0) then
      call error_msg('get_gpu_info> Error: cannot set gpu device.')
    end if


    return

  end subroutine assign_gpu

  !======1=========2=========3=========4=========5=========6=========7=========8  !
  !  Subroutine    get_local_rank
  !> @brief        get node local rank of this process
  !! @authors      MK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine get_local_rank()









    ! local variables
    character(MaxLine) :: myname
    integer            :: len_myname, i, ierr

    character(MaxLine), allocatable :: nodenames(:)
    integer,            allocatable :: ranks(:)



    ! check OpenMPI
    call getenv("OMPI_COMM_WORLD_LOCAL_RANK", myname)
    if (len_trim(myname) /= 0) then
      if (main_rank) then
        write(MsgOut,'(a)') &
          "get_local_rank> OpenMPI environment variable found."
        write(MsgOut,*)
      end if
      read(myname,*) my_node_local_rank
      return
    end if

    ! general case
    call MPI_Get_processor_name(myname, len_myname, ierr)

    allocate(nodenames(nproc_world),ranks(nproc_world))

    nodenames(1:nproc_world) = ""
    ranks(1:nproc_world)     = 0

    nodenames(my_world_rank+1) = myname
    ranks(my_world_rank+1)     = my_world_rank

    do i = 0, nproc_world - 1
      call MPI_Bcast(nodenames(i+1), MaxLine, MPI_CHARACTER, &
                     i, MPI_COMM_WORLD, ierr)
      call MPI_Bcast(ranks(i+1), 1, MPI_INTEGER, i, MPI_COMM_WORLD, ierr)
    end do

    my_node_local_rank = 0
    do i = 0, nproc_world - 1
      if (trim(nodenames(my_world_rank+1)) == trim(nodenames(i+1))) then
        if (my_world_rank > ranks(i+1)) then
          my_node_local_rank = my_node_local_rank + 1
        end if
      end if
    end do

    deallocate(nodenames,ranks)


    return

  end subroutine get_local_rank

  !======1=========2=========3=========4=========5=========6=========7=========8  !
  !  Subroutine    get_gpu_information
  !> @brief        get GPU information
  !! @param[out]   gpu        :  gpu info
  !! @authors      MK
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine get_gpu_information(gpu)

    ! formal arguments




    type(f_hipDeviceProp),  intent(out) :: gpu


    ! local variables
    integer :: ret, my_device_id


    call assign_gpu(my_device_id)
    ret = cudaGetDeviceProperties( gpu, my_device_id )
    if ( ret /= 0 ) then
      call error_msg('get_gpu_info> Error: cannot get gpu device info.')
    end if

    return

  end subroutine get_gpu_information



end module hardwareinfo_mod
