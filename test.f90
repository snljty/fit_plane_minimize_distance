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
    type(plane) :: res

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

    call fit_plane_pca(num_points, coords, res)
    deallocate(coords)
    deallocate(elems)

    ! write(*, "(3(f0.6,1x,a1,1x,a1,1x),f0.6,1x,a,1x,i0)") res%a, "x", "+", res%b, "y", "+", res%c, "z", "+", res%d, "=", 0
    write(*, "(f0.6,a1,sp,2(f0.6,a1),f0.6,ss,1x,a1,1x,i0)") res%a, "x", res%b, "y", res%c, "z", res%d, "=", 0
    ! res%b = res%b / res%a
    ! res%c = res%c / res%a
    ! res%d = res%d / res%a
    ! res%a = 1.d0
    ! write(*, "(f0.6,a1,sp,2(f0.6,a1),f0.6,ss,1x,a1,1x,i0)") res%a, "x", res%b, "y", res%c, "z", res%d, "=", 0

    stop
end program main

