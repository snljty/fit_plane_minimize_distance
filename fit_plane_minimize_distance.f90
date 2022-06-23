# define exit_success 0
# define exit_failure 1

# define stdin_unit 5
# define stdout_unit 6
# define stderr_unit 0

module fit_plane_pca_module
    implicit none

    ! private :: cross

    ! interface operator(.cross.)
    !     module procedure cross
    ! end interface

    contains

    subroutine fit_plane_pca(n, num_dims, vectors, plane_fitted)
        implicit none
        integer, intent(in) :: n, num_dims
        double precision, dimension(n, num_dims), intent(in) :: vectors
        double precision, dimension(num_dims + 1), intent(out) :: plane_fitted
        double precision, external :: ddot
        integer :: ind_point
        double precision, dimension(num_dims) :: e_points
        double precision, dimension(:, :), allocatable :: mat
        double precision, dimension(:, :), allocatable :: u ! will not actually be used
        double precision, dimension(num_dims, num_dims) :: v_t
        double precision, dimension(num_dims) :: s
        double precision, dimension(:), allocatable :: work
        integer :: lwork
        integer :: info

        if (n < num_dims) then
            write(stderr_unit, "(a)") "Error! The number of points should be at least ", num_dims, ", but got ", n, "."
            stop exit_failure
            return
        else if (n == num_dims) then ! every points are exactly on this plane
            ! call calc_plane(vectors, plane_fitted)
            continue
        else
            continue ! codes later
        end if

        allocate(mat(n, num_dims))
        e_points = sum(vectors, dim = 1) / n
        call dlacpy("A", n, num_dims, vectors, n, mat, n)
        do ind_point = 1, n
            mat(ind_point, :) = mat(ind_point, :) - e_points
        end do

        ! here we already have n > num_dims
        lwork = -1
        allocate(work(- lwork))
        call dgesvd("N", "A", n, num_dims, mat, n, s, u, 1, v_t, num_dims, work, lwork, info)
        lwork = nint(work(1))
        deallocate(work)
        allocate(work(lwork))
        call dgesvd("N", "A", n, num_dims, mat, n, s, u, 1, v_t, num_dims, work, lwork, info)
        lwork = 0
        deallocate(work)
        deallocate(mat)
        if (info < 0) then
            write(stderr_unit, "(a,i0,a)") "Error! The ", - info, "th argument had an illegal value."
            stop exit_failure
        else if (info > 0) then
            write(stderr_unit, "(a)") "Error! DBDSQR did not converge, "
            write(stderr_unit, "(i0,a)") info, " superdiagonals of an intermediate bidiagonal form B did not converge to zero."
            stop exit_failure
        else
            continue
        end if

        plane_fitted(:num_dims) = v_t(num_dims, :)
        ! plane_fitted(num_dims + 1) = - dprod(v_t(num_dims, :), e_points)
        plane_fitted(num_dims + 1) = - ddot(num_dims, v_t(num_dims, :), 1, e_points, 1)

        return
    end subroutine fit_plane_pca

    ! ! only for 3-d situation
    ! subroutine calc_plane(vectors, plane_fitted)
    !     implicit none
    !     integer, parameter :: num_dims = 3
    !     double precision, dimension(num_dims, num_dims), intent(in) :: vectors
    !     double precision, dimension(num_dims + 1), intent(out) :: plane_fitted
    !     double precision, external :: ddot
    !     double precision, dimension(num_dims) :: norm

    !     ! norm = cross(vectors(2, :) - vectors(1, :) , vectors(3, :) - vectors(1, :))
    !     norm = (vectors(2, :) - vectors(1, :)) .cross. (vectors(3, :) - vectors(1, :))
    !     norm = norm / sqrt(sum(norm ** 2))
    !     plane_fitted(:num_dims) = norm
    !     ! plane_fitted(num_dims + 1) = - dprod(norm, vectors(1, :))
    !     plane_fitted(num_dims + 1) = - ddot(num_dims, norm, 1, vectors(1, :), 1)

    !     return
    ! end subroutine calc_plane

    subroutine calc_plane(num_dims, vectors, plane_fitted)
        implicit none
        integer, intent(in) :: num_dims
        double precision, dimension(num_dims, num_dims), intent(in) :: vectors
        double precision, dimension(num_dims + 1), intent(out) :: plane_fitted
        double precision, external :: ddot
        double precision, dimension(num_dims) :: e_points
        double precision, dimension(num_dims) :: norm
        double precision, dimension(num_dims, num_dims) :: mat
        double precision, dimension(num_dims) :: scaler
        double precision, dimension(:), allocatable :: work
        integer :: lwork
        integer :: info
        integer :: i, j

        e_points = sum(vectors, dim = 1) / num_dims

        call dlacpy("A", num_dims, num_dims, vectors, num_dims, mat, num_dims)
        do i = 1, num_dims
            mat(i, :) = mat(i, :) - e_points
        end do
        
        lwork = -1
        allocate(work(- lwork))
        call dgeqrf(num_dims, num_dims, mat, num_dims, scaler, work, lwork, info)
        lwork = nint(work(1))
        deallocate(work)
        allocate(work(lwork))
        call dgeqrf(num_dims, num_dims, mat, num_dims, scaler, work, lwork, info)
        lwork = 0
        deallocate(work)

        do i = 1, num_dims
            do j = 1, i - 1
                mat(i, j) = 0.d0
            end do
        end do

        norm = 0.d0
        do i = num_dims, 1
            if (i == num_dims) then
                norm(i) = 1.d0
            else
                norm(i) = - ddot(num_dims, mat(i, :), 1, norm, 1) / mat(i, i)
            end if
        end do

        norm = norm / sqrt(sum(norm ** 2))

        plane_fitted(:num_dims) = norm
        plane_fitted(num_dims + 1) = - ddot(num_dims, norm, 1, e_points, 1)

        return
    end subroutine calc_plane

    ! function cross(vector_1, vector_2) result(ret)
    !     implicit none
    !     integer, parameter :: num_dims = 3
    !     double precision, dimension(num_dims), intent(in) :: vector_1, vector_2
    !     double precision, dimension(num_dims) :: ret

    !     ret(1) = vector_1(2) * vector_2(3) - vector_1(3) * vector_2(2)
    !     ret(2) = vector_1(3) * vector_2(1) - vector_1(1) * vector_2(3)
    !     ret(3) = vector_1(1) * vector_2(2) - vector_1(2) * vector_2(1)

    !     return
    ! end function cross

end module fit_plane_pca_module

