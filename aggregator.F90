module aggregator_module

  use iso_fortran_env, only: error_unit
  
  implicit none

  ! Define the parent aggregator and the procedures that
  ! will be deferred to rank (and type?) specific routines
  type, abstract :: aggregator
    procedure(accumulate_data), pointer :: accumulate
    procedure(reset_data), pointer :: reset

  end type aggregator

  abstract interface
    subroutine accumulate_data(this)
      import aggregator
      class(aggregator), intent(inout) :: this
    end subroutine accumulate_data
  end interface

  abstract interface
    subroutine reset_data(this)
      import aggregator
      class(aggregator), intent(inout) :: this
    end subroutine reset_data
  end interface

  ! Define the rank specific aggregators
  type, extends(aggregator) :: aggregator_1d
    real, dimension(:), allocatable :: storage
    real, dimension(:), pointer :: source_data
    procedure(get_data_1d), pointer :: get => null()
  end type aggregator_1d

  abstract interface
    function get_data_1d(this, counter)
      import aggregator_1d
      class(aggregator_1d), intent(inout) :: this
      integer, intent(in) :: counter  ! Required for mean
      real, dimension(:), allocatable :: get_data_1d
    end function get_data_1d
  end interface

  type, extends(aggregator) :: aggregator_2d
    real, dimension(:,:), allocatable :: storage
    real, dimension(:,:), pointer :: source_data
    procedure(get_data_2d), pointer :: get => null()
  end type aggregator_2d

  abstract interface
    function get_data_2d(this, counter)
      import aggregator_2d
      class(aggregator_2d), intent(inout) :: this
      integer, intent(in) :: counter  ! Required for mean
      real, dimension(:,:), allocatable :: get_data_2d
    end function get_data_2d
  end interface

  interface new_aggregator
    module procedure new_aggregator_1d
    module procedure new_aggregator_2d
  end interface

contains

  ! Define the aggregation methods
  subroutine mean_accumulate(this)
    class(aggregator), intent(inout) :: this
    select type (this)
    type is (aggregator_1d)
      this%storage = this%storage + this%source_data
    type is (aggregator_2d)
      this%storage = this%storage + this%source_data
    end select
  end subroutine mean_accumulate

  subroutine sum_accumulate(this)
    class(aggregator), intent(inout) :: this
    
    select type (this)
    type is (aggregator_1d)
      this%storage = this%storage + this%source_data
    type is (aggregator_2d)
      this%storage = this%storage + this%source_data
    end select
  end subroutine sum_accumulate

  subroutine point_accumulate(this)
    class(aggregator), intent(inout) :: this
    ! This is intentionally empty- just pull the source when we call get_data 
  end subroutine point_accumulate

  subroutine min_accumulate(this)
    class(aggregator), intent(inout) :: this
    
    select type (this)
    type is (aggregator_1d)
      this%storage = min(this%storage, this%source_data)
    type is (aggregator_2d)
      this%storage = min(this%storage, this%source_data)
    end select
  end subroutine min_accumulate

  subroutine max_accumulate(this)
    class(aggregator), intent(inout) :: this
    
    select type (this)
    type is (aggregator_1d)
      this%storage = max(this%storage, this%source_data)
    type is (aggregator_2d)
      this%storage = max(this%storage, this%source_data)
    end select
  end subroutine max_accumulate
  
  ! Define the get methods- all same except for mean and point
  ! But these do need to be rank specific, as we are returning something
  function mean_get_1d(this, counter)
    class(aggregator_1d), intent(inout) :: this
    integer, intent(in) :: counter
    real, dimension(:), allocatable :: mean_get_1d

    mean_get_1d = this%storage / real(counter)
  end function mean_get_1d

  function mean_get_2d(this, counter)
    class(aggregator_2d), intent(inout) :: this
    integer, intent(in) :: counter
    real, dimension(:,:), allocatable :: mean_get_2d

    mean_get_2d = this%storage / real(counter)
  end function mean_get_2d

  function point_get_1d(this, counter)
    class(aggregator_1d), intent(inout) :: this
    integer, intent(in) :: counter
    real, dimension(:), allocatable :: point_get_1d

    point_get_1d = this%source_data
  end function point_get_1d

  function point_get_2d(this, counter)
    class(aggregator_2d), intent(inout) :: this
    integer, intent(in) :: counter
    real, dimension(:,:), allocatable :: point_get_2d

    point_get_2d = this%source_data
  end function point_get_2d

  function other_get_1d(this, counter)
    class(aggregator_1d), intent(inout) :: this
    integer, intent(in) :: counter
    real, dimension(:), allocatable :: other_get_1d

    other_get_1d = this%storage
  end function other_get_1d

  function other_get_2d(this, counter)
    class(aggregator_2d), intent(inout) :: this
    integer, intent(in) :: counter
    real, dimension(:,:), allocatable :: other_get_2d

    other_get_2d = this%storage
  end function other_get_2d
  
  ! Define the reset methods- max and min require bespoke methods,
  ! point requires nothing
  subroutine point_reset(this)
    class(aggregator), intent(inout) :: this
  end subroutine point_reset

  subroutine min_reset(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage(:) = huge(real(0.0))
    type is (aggregator_2d)
      this%storage(:,:) = huge(real(0.0))
    end select
  end subroutine min_reset

  subroutine max_reset(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage(:) = -huge(real(0.0))
    type is (aggregator_2d)
      this%storage(:,:) = -huge(real(0.0))
    end select
  end subroutine max_reset
  
  subroutine other_reset(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage(:) = 0.0
    type is (aggregator_2d)
      this%storage(:,:) = 0.0
    end select
  end subroutine other_reset

  ! Now we can define the initialisers
  function new_aggregator_1d(source_data, method) result(agg)
    real, dimension(:), intent(in) :: source_data
    character(len=*), intent(in) :: method

    class(aggregator), allocatable :: agg

    type(aggregator_1d) :: agg_1d
    
    allocate(agg_1d%storage, source=source_data)
    agg = agg_1d

    ! Assign the rank specific methods
    if (method == "mean") then
      agg%accumulate => mean_accumulate
      agg_1d%get => mean_get_1d
      agg%reset => other_reset
    elseif (method == "sum") then
      agg%accumulate => mean_accumulate
      agg_1d%get => other_get_1d
      agg%reset => other_reset
    elseif (method == "point") then
      agg%accumulate => point_accumulate
      agg_1d%get => point_get_1d
      agg%reset => point_reset
    elseif (method == "min") then
      agg%accumulate => min_accumulate
      agg_1d%get => other_get_1d
      agg%reset => min_reset
    elseif (method == "max") then
      agg%accumulate => max_accumulate
      agg_1d%get => other_get_1d
      agg%reset => max_reset
    else
      write(error_unit, '(A)') "Aggregation method "//method//" is invalid."
      stop -1
    endif

  end function new_aggregator_1d

  function new_aggregator_2d(source_data, method) result(agg)
    real, dimension(:,:), intent(in) :: source_data
    character(len=*), intent(in) :: method

    class(aggregator), allocatable :: agg

    type(aggregator_2d) :: agg_2d
    
    allocate(agg_2d%storage, source=source_data)
    agg = agg_2d

    ! Assign the rank specific methods
    if (method == "mean") then
      agg%accumulate => mean_accumulate
      agg_2d%get => mean_get_2d
      agg%reset => other_reset
    elseif (method == "sum") then
      agg%accumulate => mean_accumulate
      agg_2d%get => other_get_2d
      agg%reset => other_reset
    elseif (method == "point") then
      agg%accumulate => point_accumulate
      agg_2d%get => point_get_2d
      agg%reset => point_reset
    elseif (method == "min") then
      agg%accumulate => min_accumulate
      agg_2d%get => other_get_2d
      agg%reset => min_reset
    elseif (method == "max") then
      agg%accumulate => max_accumulate
      agg_2d%get => other_get_2d
      agg%reset => max_reset
    else
      write(error_unit, '(A)') "Aggregation method "//method//" is invalid."
      stop -1
    endif

  end function new_aggregator_2d

end module aggregator_module
