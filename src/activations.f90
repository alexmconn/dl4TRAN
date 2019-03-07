module activations
implicit none

contains

pure function sigmoid(z)
	double precision, intent(in) :: z(:)
	double precision, dimension(size(z)) :: sigmoid

	sigmoid = 1.0 / (1.0 + exp(-z))
end function sigmoid

pure function sigmoid_prime(z)
	double precision, intent(in) :: z(:)
	double precision, dimension(size(z)) :: sigmoid_prime

	sigmoid_prime = sigmoid(z) * (1 - sigmoid(z))
end function sigmoid_prime


! maybe we will move this to another module later
function cost(output, target)
	double precision, intent(in) :: output(:)
	double precision, intent(in) :: target(:)
	double precision :: cost

	cost = 0.5 * sum((target - output)**2)
end function cost

end module activations