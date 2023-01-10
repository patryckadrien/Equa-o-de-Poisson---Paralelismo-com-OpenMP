module inicializa
    ! Código contendo o algoritmo que inicializa a malha com base nos vetores iniciais
    
    implicit none
                          
    contains 
    
    !#######################################################################################################
        
        subroutine inicializacao(m, h, a, c, x, y)
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !     a                 :: parâmetro inicial
            !     c                 :: parâmetro inicial
            !     h                 :: comprimento da subdivisão da malha
            !
            ! Exemplo de uso:
            !
            !     call inicializacao(m, h, a, c, x, y)
            !
            implicit none
            
            integer :: i, j, m
            real(8) :: h, x(0:m), y(0:m), a, c
            
            h = (1.d0)/m
                
            do i=0, m
                x(i) = a + i*h
            end do
            
            do j=0, m
                y(j) = c + j*h
            end do 
    
        end subroutine inicializacao
        
    !#######################################################################################################
        
end module inicializa
