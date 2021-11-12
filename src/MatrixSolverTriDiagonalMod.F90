module MatrixSolverTriDiagonalMod

!!! Solve tri-diagonal matrix problem

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine MatrixSolverTriDiagonal(P, A, B, C, D, DELTA, NTOP, NSOIL, NSNOW)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: ROSR12
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------

    implicit none

! in & out variables
    integer               , intent(in)    :: NTOP          ! top layer index: soil layer starts from NTOP = 1
    integer               , intent(in)    :: NSOIL,NSNOW   ! soil and snow layers
    real(kind=kind_noahmp), allocatable, dimension(:), intent(in)    :: A, B, D
    real(kind=kind_noahmp), allocatable, dimension(:), intent(inout) :: C,P,DELTA

! local variables
    integer  :: K, KK   ! loop indices
! ----------------------------------------------------------------------

    ! initialization
    allocate( A    (-NSNOW+1:NSOIL) )
    allocate( B    (-NSNOW+1:NSOIL) )
    allocate( D    (-NSNOW+1:NSOIL) )
    allocate( C    (-NSNOW+1:NSOIL) )
    allocate( P    (-NSNOW+1:NSOIL) )
    allocate( DELTA(-NSNOW+1:NSOIL) )

    ! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)

    ! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
    DELTA (NTOP) = D (NTOP) / B (NTOP)

    ! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
    do K = NTOP+1, NSOIL
       P (K)     = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K) * DELTA (K -1)) * (1.0 / (B (K) + A (K) &
                   * P (K -1)))
    enddo

    ! SET P TO DELTA FOR LOWEST SOIL LAYER
    P (NSOIL) = DELTA (NSOIL)

    ! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
    do K = NTOP+1, NSOIL
       KK     = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    enddo

  end subroutine MatrixSolverTriDiagonal

end module MatrixSolverTriDiagonalMod
