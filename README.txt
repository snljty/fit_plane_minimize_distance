The codes can be easisy extented to a multi-dimensional hyper-plane fitting when num_dims >= 3, 
where num_dims is the dimension of the space.

If the number of points is smaller than num_dims, then it raises an error.
If the number of points equals num_dims, the QR decomposition is used and in this situation you 
need to make sure that the last coordinate of each points cannot be the same (while if they are, 
the plane should be orthogonal with the last axis).
If the number os points is larger than num_dims, then the Singular Value Decomposition is applied, 
to find a plane that the L-2 distance (a.k.a. the square root of the sum of the squares of each elements of the vector) 
of the distance between each points and the plane is minimized. Noth that this does not equals the Least-Square algorithm, 
whose aim is to minimize the L-2 distance of z - (a x + b y + c).

