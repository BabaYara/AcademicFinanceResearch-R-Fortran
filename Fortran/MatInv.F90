! Computes inverse of a matrix
subroutine MatInv( A, Ainv)
   implicit none
   real*8,        intent(in)   ::  A(:,:)
   real*8,        intent(out)  ::  Ainv(:,:)
   real*8, allocatable         ::  work(:)
   integer                     :: n, info
   integer, allocatable        :: ipiv(:)

   Ainv  =  A
   n     =  size( Ainv, 1)
   allocate(ipiv(n),work(n))
   
   call dgetrf(n,n,Ainv,n,ipiv,info)
   call dgetri(n,Ainv,n,ipiv,work,n,info)
   
   deallocate( ipiv, work)
end subroutine MatInv
