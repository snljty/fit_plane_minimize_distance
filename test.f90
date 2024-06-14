program main
    use fit_plane_pca_module
    implicit none
    integer, parameter :: ifl_unit = 10
    character(len=10), parameter :: ifl_file = "data1.xyz"
    ! character(len=10), parameter :: ifl_file = "data2.xyz"
    ! character(len=10), parameter :: ifl_file = "data3.xyz"
    integer, parameter :: num_dims = 3
    integer :: num_points
    double precision, dimension(:, :), allocatable :: coords
    character(len=128) :: buf
    character(len=2), dimension(:), allocatable :: elems
    integer :: ind_point
    double precision, dimension(num_dims + 1) :: res

    open(ifl_unit, file = trim(ifl_file), status = "old", action = "read")
    read(ifl_unit, "(a)") buf
    read(buf, *) num_points
    read(ifl_unit, "(a)") buf
    allocate(coords(num_points, num_dims))
    allocate(elems(num_points))
    do ind_point = 1, num_points
        read(ifl_unit, "(a)") buf
        read(buf, *) elems(ind_point), coords(ind_point, :)
    end do
    close(ifl_unit)

    call fit_plane_pca(num_points, num_dims, coords, res)
    deallocate(coords)
    deallocate(elems)

    ! write(*, "(3(f0.6,1x,a1,1x,a1,1x),f0.6,1x,a,1x,i0)") res(1), "x", "+", res(2), "y", "+", res(3), "z", "+", res(4), "=", 0
    write(*, "(f0.6,a1,sp,2(f0.6,a1),f0.6,ss,1x,a1,1x,i0)") res(1), "x", res(2), "y", res(3), "z", res(4), "=", 0
    ! res(2) = res(2) / res(1)
    ! res(3) = res(3) / res(1)
    ! res(4) = res(4) / res(1)
    ! res(1) = 1.d0
    ! write(*, "(f0.6,a1,sp,2(f0.6,a1),f0.6,ss,1x,a1,1x,i0)") res(1), "x", res(2), "y", res(3), "z", res(4), "=", 0

    stop
end program main

! program main
!     use fit_plane_pca_module
!     implicit none
!     integer, parameter :: num_dims = 3
!     double precision, dimension(num_dims, num_dims) :: coords = &
!         reshape((/1.d0, 2.d0, 8.d0, 3.d0, 9.d0, 4.d0, 7.d0, 6.d0, 5.d0/), (/num_dims, num_dims/))
!     double precision, dimension(num_dims + 1) :: res

!     call calc_plane(num_dims, coords, res)

!     write(*, "(f0.6,a1,sp,2(f0.6,a1),f0.6,ss,1x,a1,1x,i0)") res(1), "x", res(2), "y", res(3), "z", res(4), "=", 0

!     stop
! end program main

