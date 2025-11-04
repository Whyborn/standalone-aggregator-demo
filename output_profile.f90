module output_profile_module

  use iso_fortran_env, only: error_unit
  use aggregator_module, only: aggregator, aggregator_1d, aggregator_2d

  implicit none

  type, abstract :: output_var
    integer :: file_id, var_id
  contains
    procedure, deferred :: write_var
  end type output_var

  abstract interface
    subroutine write_var(this)
      import output_var
      class(output_var), intent(inout) :: this
    end subroutine write_var
  end interface

  type, extends(output_var) :: output_var_1d
    type(aggregator_1d) :: var_agg
    !!! And a pointer to the decomp that maps to the lat/lon grid
  end type output_var_1d

  type, extends(output_var) :: output_var_2d
    type(aggregator_2d) :: var_agg
    !!! And a pointer to the decomp that maps to the lat/lon grid
  end type output_var_2d

  type :: output_profile
    ! Separate variables of different ranks, since they need to be retrieved
    class(output_var), allocatable, dimension(:) :: output_vars

    integer :: counter ! How long since last write
  contains
    procedure :: add_var
  end type output_profile

contains

  function new_output_profile()
    type(output_profile) :: new_output_profile
  end function new_output_profile
  
  subroutine add_var(this, var)
    type(output_profile), intent(inout) :: this
    class(output_var), intent(in) :: var

    ! Need a temporary array to hold the original data in
    class(output_var), dimension(:), allocatable :: tmp_output_vars

    tmp_output_vars = this%output_vars
    allocate(this%output_vars(ubound(tmp_output_vars) + 1))

    this%output_vars(1:ubound(tmp_output_vars)) = tmp_output_vars
    this%output_vars(ubound(this%output_vars)) = var
  end subroutine add_var

end module output_profile_module
