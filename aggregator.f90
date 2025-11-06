module aggregator_module

  use iso_fortran_env, only: error_unit
  
  implicit none

  ! Define the parent aggregator and the procedures that
  ! will be deferred to rank (and type?) specific routines
  type, abstract :: aggregator
    integer :: counter = 0
    procedure(accumulate_data), pointer :: accumulate
    procedure(normalise_data), pointer :: normalise
    procedure(reset_data), pointer :: reset
  end type aggregator

  abstract interface
    subroutine accumulate_data(this)
      import aggregator
      class(aggregator), intent(inout) :: this
    end subroutine accumulate_data
  end interface

  abstract interface
    subroutine normalise_data(this)
      import aggregator
      class(aggregator), intent(inout) :: this
    end subroutine normalise_data
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
    real, dimension(:,:), allocatable :: storage
    real, dimension(:,:), pointer :: source_data
  end type aggregator_2d

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

    this%counter = this%counter + 1

  end subroutine mean_accumulate

  subroutine sum_accumulate(this)
    class(aggregator), intent(inout) :: this
    
    select type (this)
    type is (aggregator_1d)
      this%storage = this%storage + this%source_data
    type is (aggregator_2d)
      this%storage = this%storage + this%source_data
    end select

    this%counter = this%counter + 1

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

    this%counter = this%counter + 1

  end subroutine min_accumulate

  subroutine max_accumulate(this)
    class(aggregator), intent(inout) :: this
    
    select type (this)
    type is (aggregator_1d)
      this%storage = max(this%storage, this%source_data)
    type is (aggregator_2d)
      this%storage = max(this%storage, this%source_data)
    end select

    this%counter = this%counter + 1

  end subroutine max_accumulate
  
  ! Define the normalise methods- only the mean does any work for now
  subroutine mean_normalise(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage = this%storage / this%counter
    type is (aggregator_2d)
      this%storage = this%storage / this%counter
    end select

  end subroutine mean_normalise

  subroutine other_normalise(this)
    class(aggregator), intent(inout) :: this

  end subroutine other_normalise

  ! Define the reset methods- max and min require bespoke methods,
  ! point requires nothing
  subroutine point_reset(this)
    class(aggregator), intent(inout) :: this

    this%counter = 0

  end subroutine point_reset

  subroutine min_reset(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage(:) = huge(real(0.0))
    type is (aggregator_2d)
      this%storage(:,:) = huge(real(0.0))
    end select

    this%counter = 0

  end subroutine min_reset

  subroutine max_reset(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage(:) = -huge(real(0.0))
    type is (aggregator_2d)
      this%storage(:,:) = -huge(real(0.0))
    end select

    this%counter = 0

  end subroutine max_reset
  
  subroutine other_reset(this)
    class(aggregator), intent(inout) :: this

    select type (this)
    type is (aggregator_1d)
      this%storage(:) = 0.0
    type is (aggregator_2d)
      this%storage(:,:) = 0.0
    end select

    this%counter = 0

  end subroutine other_reset

  ! Now we can define the initialisers
  function new_aggregator_1d(source_data, method) result(agg)
    real, dimension(:), intent(in) :: source_data
    character(len=*), intent(in) :: method

    class(aggregator), allocatable :: agg

    type(aggregator_1d) :: agg_1d
    
    allocate(agg_1d%storage, source=source_data)
    agg = agg_1d

    ! Assign the methods
    if (method == "mean") then
      agg%accumulate => mean_accumulate
      agg%normalise => mean_normalise
      agg%reset => other_reset
    elseif (method == "sum") then
      agg%accumulate => mean_accumulate
      agg%normalise => other_normalise
      agg%reset => other_reset
    elseif (method == "point") then
      agg%accumulate => point_accumulate
      agg%normalise => other_normalise
      agg%reset => point_reset
    elseif (method == "min") then
      agg%accumulate => min_accumulate
      agg%normalise => other_normalise
      agg%reset => min_reset
    elseif (method == "max") then
      agg%accumulate => max_accumulate
      agg%normalise => other_normalise
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

    ! Assign the methods
    if (method == "mean") then
      agg%accumulate => mean_accumulate
      agg%normalise => mean_normalise
      agg%reset => other_reset
    elseif (method == "sum") then
      agg%accumulate => mean_accumulate
      agg%normalise => other_normalise
      agg%reset => other_reset
    elseif (method == "point") then
      agg%accumulate => point_accumulate
      agg%normalise => other_normalise
      agg%reset => point_reset
    elseif (method == "min") then
      agg%accumulate => min_accumulate
      agg%normalise => other_normalise
      agg%reset => min_reset
    elseif (method == "max") then
      agg%accumulate => max_accumulate
      agg%normalise => other_normalise
      agg%reset => max_reset
    else
      write(error_unit, '(A)') "Aggregation method "//method//" is invalid."
      stop -1
    endif

  end function new_aggregator_2d

end module aggregator_module
