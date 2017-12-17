SUBROUTINE COVAR(Cov, MyData, Nrow, Ncol)
    ! --------------------------------------------------------------------
    ! PROGRAM  COVAR:
    ! This program reads in an array and computes the covariance matrix.
    ! Input
    !   MyData :  Array 
    !   Nrow   :  Number of Rows    (Time Dimension)
    !   Ncol   :  Number of Columns (Cross Section)
    !
    ! Output 
    !   Cov    : An Nrow by Ncol Array 
    ! --------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER, INTENT(IN)   ::  Ncol, Nrow
    REAL*8,  INTENT(IN)   ::  MyData(Nrow, Ncol) 
    REAL*8,  INTENT(OUT)  ::  Cov(Ncol, Ncol)  
    
    ! Intermediary Variables
    REAL*8  :: summed, dummy(Nrow, Ncol)
    INTEGER :: i, j, k 

    dummy = MyData

    do i = 1, Ncol 
        summed = 0.0 
        do k = 1, Nrow 
            summed = summed + dummy(k, i)
        end do 
        summed = summed / Nrow 

        do k = 1, Nrow
            dummy(k, i) = dummy(k, i) - summed
        end do 

        do j = 0, i
            do k = 1, Nrow
                Cov(i, j) = Cov(i, j) + dummy(k, i) * dummy(k, j)
            end do 
            Cov(i, j) = Cov(i, j) / Nrow 
            Cov(j, i) = Cov(i, j)
        end do 
    end do 
    
END subroutine COVAR
