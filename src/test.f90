program test
use activations
implicit none

call test_cost()

contains

subroutine test_cost()
	double precision, dimension(5) :: out
	double precision, dimension(5) :: target
	double precision :: cost_output

	out = (/ 1.0, 2.0, 3.0, 4.0, 5.0/)
	target = (/2.0, 3.0, 4.0, 5.0, 6.0/)

	cost_output = cost(out, target)
	if(cost_output == 2.50) then
		write(*,*) "test_cost: success"
	else
		write(*,*) "test_cost: failure"
	end if

end subroutine test_cost

end program test