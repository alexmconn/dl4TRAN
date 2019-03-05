program mnist
use activations
implicit none

integer, allocatable :: train(:,:)
integer :: n, m, io, i, j, number_to_display
integer :: seed = 1234567
character(len=128) :: arg
character(len=128) :: filename

! Declaration of network weights and biases
double precision, dimension(30, 784) :: w_h1
double precision, dimension(30) :: b_h1
double precision, dimension(10,30) :: w_out
double precision, dimension(10) :: b_out, out

! Initializes all weights with gaussian normal distribution
! with 1 variance
call r8mat_normal_01(30, 784, seed, w_h1)
call r8vec_normal_01(30, seed, b_h1)
call r8mat_normal_01(10, 30, seed, w_out)
call r8vec_normal_01(10, seed, b_out)

call getarg(1, arg)
filename = arg
call getarg(2, arg)
read(arg, '(I10)') number_to_display

m = 784
n = 0
open(1, file = filename)
do
	read(1,*,iostat=io)
	if (io/=0) exit
	n = n + 1
end do
close(1)

n = n - 1 ! remove header line from count
allocate(train(n, m))

open(1, file = filename)
read(1,*) ! read header
do i = 1, n
	read(1, *) train(i,:)
end do

call display(train(number_to_display,:))
call feedforward(train(202,:), w_h1, b_h1, w_out, b_out, out)
write(*,*) out

contains

subroutine feedforward(input, w_h1, b_h1, w_out, b_out, out)
	integer, intent(in), dimension(784) :: input
	double precision, intent(in), dimension(30, 784) :: w_h1
	double precision, intent(in), dimension(30) :: b_h1
	double precision, intent(in), dimension(10,30) :: w_out
	double precision, intent(in), dimension(10) :: b_out
	double precision, dimension(30) :: h1
	double precision, intent(out), dimension(10) :: out

	h1 = sigmoid(matmul(w_h1, input) + b_h1)
	out = sigmoid(matmul(w_out, h1) + b_out)

end subroutine

subroutine display(x)
	integer, dimension(784) :: x
	character, dimension(28,28) :: d
	integer :: i, j, k
	k = 1
	do i = 1, 28
		do j = 1, 28
			if(x(k) > 128) then
				d(i,j) = 'X'
			else
				d(i,j) = ' '
			end if
			k = k +1
		end do
	end do

	do i = 1, 28
		write(*,*) d(i,:)
	end do


end subroutine

end program
