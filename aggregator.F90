module aggregator_module

  use iso_fortran_env, only: error_unit
  
  implicit none

  ! Define the parent aggregator and the procedures that
  ! will be deferred to rank (and type?) specific routines
  type, abstract :: aggregator
  contains
    procedure, deferred :: accumulate_data
    procedure, deferred :: get_data
    procedure, deferred :: reset_data
  end type aggregator

  abstract interface
    subroutine accumulate_data(this)
      import aggregator
      class(aggregator), intent(inout) :: this
    end subroutine accumulata_data
  end interface

  abstract interface
    function get_data(this, counter)
      import aggregator
      class(aggregator), intent(inout) :: this
      integer, intent(in) :: counter  ! Required for mean
      real, dimension(..) :: get_data
    end function get_data
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
  end type aggregator_1d

  type, extends(aggregator) :: aggregator_2d
    real, dimension(:), allocatable :: storage
    real, dimension(:), pointer :: source_data
  end type aggregator_2d

contains

  ! Define the aggregation methods
  subroutine mean_aggregate(this)
    class(aggregator), intent(inout) :: this
    
    this%storage = this%storage + this%source_data
  end subroutine mean_aggregate

  subroutine sum_aggregate(this)
    class(aggregator), intent(inout) :: this
    
    this%storage = this%storage + this%source_data
  end subroutine sum_aggregate

  subroutine point_aggregate(this)
    class(aggregator), intent(inout) :: this
    ! This is intentionally empty- just pull the source when we call get_data 
  end subroutine point_aggregate

  subroutine min_aggregate(this)
    class(aggregator), intent(inout) :: this
    
    this%storage = min(this%storage, this%source_data)
  end subroutine min_aggregate

  subroutine max_aggregate(this)
    class(aggregator), intent(inout) :: this
    
    this%storage = max(this%storage, this%source_data)
  end subroutine max_aggregate
  
  ! Define the get methods- all same except for mean and point
  ! But these do need to be rank specific, as we are returning something
  function mean_get_1d(this, counter)
    type(aggregator_1d), intent(in) :: this
    real, dimension(:) :: mean_get_1d

    mean_get_1d = this%storage / real(counter)
  end function mean_get_1d

  function mean_get_2d(this, counter)
    type(aggregator_2d), intent(in) :: this
    real, dimension(:,:) :: mean_get_1d

    mean_get_2d = this%storage / real(counter)
  end function mean_get_2d

  function point_get_1d(this, counter)
    type(aggregator_1d), intent(in) :: this
    real, dimension(:) :: point_get_1d

    point_get_1d = this%source_data
  end function point_get_1d

  function point_get_2d(this, counter)
    type(aggregator_2d), intent(in) :: this
    real, dimension(:,:) :: point_get_1d

    point_get_2d = this%source_data
  end function point_get_2d

  function other_get_1d(this, counter)
    type(aggregator_1d), intent(in) :: this
    real, dimension(:) :: other_get_1d

    other_get_1d = this%storage
  end function other_get_1d

  function other_get_2d(this, counter)
    type(aggregator_2d), intent(in) :: this
    real, dimension(:,:) :: other_get_2d

    other_get_2d = this%storage
  end function other_get_1d
  
  ! Define the reset methods- max and min require bespoke methods,
  ! point requires nothing
  subroutine point_reset(this)
    class(aggregator), intent(inout) :: this
  end subroutine point_reset

  subroutine min_reset(this)
    class(aggregator), intent(inout) :: this

    this%storage(:) = huge(real(0.0))
  end subroutine min_reset

  subroutine max_reset(this)
    class(aggregator), intent(inout) :: this

    this%storage(:) = -huge(real(0.0))
  end subroutine max_reset
  
  subroutine other_reset(this)
    class(aggregator), intent(inout) :: this

    this%storage(:) = 0.0
  end subroutine other_reset

  ! Now we can define the initialiser
  function new_aggregator(source_data, method) 
    real, dimension(..), intent(in) :: source_data
    character(len=*), intent(in) :: method

    class(aggregator) :: new_aggregator

    type(aggregator_1d) :: new_agg_1d
    type(aggregator_2d) :: new_agg_2d

    select rank(source)
    rank(1)
      allocate(new_agg_1d%storage, source=source_data)
      new_aggregator = new_agg_1d

      ! Assign the rank specific get
      if (method == "mean") then
        agg%accumulate_data => mean_accumulate
        agg%get_data => mean_get_1d
        agg%reset_data => mean_reset
      elseif (method == "sum") then
        agg%accumulate_data => mean_accumulate
        agg%get_data => sum_get_1d
        agg%reset_data => other_reset
      elseif (method == "point") then
        agg%accumulate_data => point_accumulate
        agg%get_data => point_get_1d
        agg%reset_data => point_reset
      elseif (method == "min") then
        agg%accumulate_data => min_accumulate
        agg%get_data => other_get_1d
        agg%reset_data => min_reset
      elseif (method == "max") then
        agg%accumulate_data => max_accumulate
        agg%get_data => other_get_1d
        agg%reset_data => max_reset
      else
        write(error_unit, '(A)') "Aggregation method "//method//" is invalid."
        stop -1
      endif

    rank(2)
      allocate(new_agg_2d%storage, source=source_data)
      new_aggregator = new_agg_2d

      ! Assign the rank specific get
      if (method == "mean") then
        agg%accumulate_data => mean_accumulate
        agg%get_data => mean_get_2d
        agg%reset_data => mean_reset
      elseif (method == "sum") then
        agg%accumulate_data => mean_accumulate
        agg%get_data => sum_get_2d
        agg%reset_data => other_reset
      elseif (method == "point") then
        agg%accumulate_data => point_accumulate
        agg%get_data => point_get_2d
        agg%reset_data => point_reset
      elseif (method == "min") then
        agg%accumulate_data => min_accumulate
        agg%get_data => other_get_2d
        agg%reset_data => min_reset
      elseif (method == "max") then
        agg%accumulate_data => max_accumulate
        agg%get_data => other_get_2d
        agg%reset_data => max_reset
      else
        write(error_unit, '(A)') "Aggregation method "//method//" is invalid."
        stop -1
      endif

    rank default
      write(error_unit, '(A)') "Aggregation methods only implemented up to 2D"
      stop -1
    end select
