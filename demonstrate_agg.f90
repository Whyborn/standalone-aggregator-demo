program demonstrate_aggregator

  use iso_fortran_env, only: output_unit
  use aggregator_module, only: aggregator, aggregator_1d, aggregator_2d, new_aggregator

  implicit none

  class(aggregator), dimension(:), allocatable :: timestep_aggs, three_hourly_aggs,&
      daily_aggs
  real, dimension(:), allocatable, target :: var_1, var_2, var_3, tmp_1d
  real, dimension(:,:), allocatable, target :: var_4, var_5, tmp_2d
  integer :: t, dt, secs_in_day, y, i

  ! Set up some timing things
  secs_in_day = 24 * 3600
  t = 0
  dt = 1800 ! 30min timestep

  ! Set up the aggregators
  allocate(var_1(10), var_2(10), var_3(100))
  allocate(var_4(10, 10), var_5(100, 10))

  timestep_aggs = [new_aggregator(var_1, "point"), new_aggregator(var_2, "point")]
  three_hourly_aggs = [new_aggregator(var_1, "min"), new_aggregator(var_5, "max")]
  daily_aggs = [new_aggregator(var_4, "point"), new_aggregator(var_3, "mean")]

  do y = 1, 5
    do while (t < 365 * secs_in_day)
      ! Just fill the working arrays with random numbers
      call random_number(var_1)
      call random_number(var_2)
      call random_number(var_3)
      call random_number(var_4)
      call random_number(var_5)

      t = t + dt

      ! Don't need to select type here, as accumulate is defined on the parent type
      do i = 1, size(timestep_aggs)
        call timestep_aggs(i)%accumulate()
      end do

      do i = 1, size(three_hourly_aggs)
        call three_hourly_aggs(i)%accumulate()
      end do

      do i = 1, size(daily_aggs)
        call daily_aggs(i)%accumulate()
      end do

      ! Select types are required here, as the gets are child (rank) specific
      ! Trigger every timestep- pass 1 as thats the number of timesteps in the period
      do i = 1, size(timestep_aggs)
        select type (agg => timestep_aggs(i))
        type is (aggregator_1d)
          call agg%normalise()
          tmp_1d = agg%storage
          call agg%reset()
        type is (aggregator_2d)
          call agg%normalise()
          tmp_2d = agg%storage
          call agg%reset()
        end select
      end do

      ! Trigger every 3 hours
      if (mod(t, 3600 * 3) == 0) then
        do i = 1, size(three_hourly_aggs)
          select type (agg => three_hourly_aggs(i))
          type is (aggregator_1d)
            call agg%normalise()
            tmp_1d = agg%storage
            call agg%reset()
          type is (aggregator_2d)
            call agg%normalise()
            tmp_2d = agg%storage
            call agg%reset()
          end select
        end do
      endif

      ! Trigger every day
      if (mod(t, 3600 * 24) == 0) then
        write(output_unit,'(A)') "Triggering on end of day"
        do i = 1, size(daily_aggs)
          select type (agg => daily_aggs(i))
          type is (aggregator_1d)
            call agg%normalise()
            tmp_1d = agg%storage
            call agg%reset()
          type is (aggregator_2d)
            call agg%normalise()
            tmp_2d = agg%storage
            call agg%reset()
          end select
        end do
      endif
    end do
    t = 0
  end do

end program demonstrate_aggregator
