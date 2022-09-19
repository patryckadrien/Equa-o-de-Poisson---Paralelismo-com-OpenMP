module cond_con
    ! Código contendo o algoritmo que realiza o cálculo das condições de contorno
    
    implicit none
                          
    contains 

    !#######################################################################################################
    
        real(8) function g1(y, j, n)
            ! Função chamada para o calculo das condições de contorno
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = g1(y, j, n)
            !
            implicit none
            
            integer :: j, n
            real(8) :: y(0:n)
            
            g1 = (y(j))*(log(y(j)))
            
            return
        end function g1
        
        !#######################################################################################################
        
        real(8) function g2(y, j, n)
            ! Função chamada para o calculo das condições de contorno
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = g2(y, j, n)
            !
            implicit none
            
            integer :: j, n
            real(8) :: y(0:n)
            
            g2 = (2.d0)*(y(j))*(log((2.d0)*(y(j))))
            
            return
        end function g2
        
        !#######################################################################################################
        
        subroutine condicoes_contorno(n, u_ant, u_novo, y)
            ! Atribui as condições de contorno a malha
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     u_ant(0:m,0:n)    :: malha inicial discretizada
            !     u_novo(0:m,0:n)   :: malha discretizada a ser iterada
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     call condicoes_contorno(n, u_ant, u_novo, y)
            !
            implicit none
            
            integer :: j, n
            real(8) :: u_ant(0:n,0:n), u_novo(0:n,0:n), y(0:n)

            do j=0, n
                u_ant(0,j) = g1(y, j, n)
                u_ant(n,j) = g2(y, j, n)
            end do
            
            u_novo = u_ant
            
        end subroutine condicoes_contorno
    
    !#######################################################################################################
    
end module cond_con