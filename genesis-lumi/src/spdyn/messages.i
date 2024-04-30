# 1 "messages.f90"
# 1 "../lib/messages.fpp"
# 1 "<built-in>"
# 1 "<command-line>"
# 23 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1

# 17 "/usr/include/stdc-predef.h" 3 4











































# 1 "../lib/messages.fpp"
!--------1---------2---------3---------4---------5---------6---------7---------8
! 
!  Module   messages_mod
!> @brief   utilities for taking messages to Std or Err output
!! @authors Yuji Sugita (YS)
!! @note    MsgOut and ErrOut are defined here.
!
!  (c) Copyright 2014 RIKEN. All rights reserved.
! 
!--------1---------2---------3---------4---------5---------6---------7---------8





# 51
module messages_mod

  use mpi_parallel_mod

  implicit none
  private

  ! parameters
  integer, public, parameter :: MsgOut  = 6
  integer, public, parameter :: ErrOut  = 6

  ! subroutines and functions
  public :: error_msg
  public :: error_msg_alloc
  public :: error_msg_dealloc
  public :: error_msg_fileio

contains
  
  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    error_msg
  !> @brief        write error message and stop execution
  !! @param[in]    message : Error message (optional)
  !! @authors      YS
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine error_msg(message)









    ! formal arguments
    character(*), optional,  intent(in)    :: message

    ! local variables
    character(1000)          :: msg


    if (present(message)) then
      write(msg,'(A,A12,I5)') trim(message), '  rank_no = ', my_world_rank
    else
      write(msg,'(A25,I5)') '               rank_no = ', my_world_rank
    end if

# 78 "../lib/messages.fpp"
# 78
    write(ErrOut,*) trim(msg)
    call exit(1)



  end subroutine error_msg

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    error_msg_alloc
  !> @brief        write error message in allocation and stop execution
  !! @param[in]    message : Error message
  !! @authors      YS
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine error_msg_alloc

    call error_msg('Memory allocation error')

  end subroutine error_msg_alloc

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    error_msg_dealloc
  !> @brief        write error message in deallocation and stop execution
  !! @param[in]    message : Error message
  !! @authors      YS
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine error_msg_dealloc

    call error_msg('Memory deallocation error')

  end subroutine error_msg_dealloc

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    error_msg_fileio
  !> @brief        write error message in fileio and stop execution
  !! @param[in]    message : Error message
  !! @authors      YS
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine error_msg_fileio

    call error_msg('File I/O error')

  end subroutine error_msg_fileio

end module messages_mod
