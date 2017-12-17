 subroutine MEAN(average, A, An)
! --------------------------------------------------------------------
! PROGRAM  Mean:
! This program reads in an array and computes the mean.
! Input
!   A   :  Array 
!   An  :  Size of Array (A(1:An)
!
! Output 
!   Average : Mean of Array A
! --------------------------------------------------------------------
    
    IMPLICIT NONE
    INTEGER, INTENT(IN)  ::  An
    REAL*8,  INTENT(OUT) ::  average, A(An)

    !Intermediary Variables
    REAL*8   :: s 
    INTEGER  :: j

    s = 0 
    do j = 1, An
        s = s + A(j)
    enddo 
    average = s / An
   
end subroutine MEAN
