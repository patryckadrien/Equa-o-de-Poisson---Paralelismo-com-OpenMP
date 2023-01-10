module cond_ini
    ! Código contendo o algoritmo que realiza o cálculo das condições iniciais
    
    implicit none
                          
    contains 

    !#######################################################################################################
            
        real(8) function f1(x, i, m)
            ! Função chamada para o cálculo das condições iniciais
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = f1(x, i, m)
            !
            implicit none
            
            integer :: i, m
            real(8) :: x(0:m)
            
            f1 = (x(i))*(log(x(i)))
            
            return
        end function f1

        !#######################################################################################################

        real(8) function f2(x, i, m)
            ! Função chamada para o cálculo das condições iniciais
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = f2(x, i, m)
            !
            implicit none
            
            integer :: i, m
            real(8) :: x(0:m)
            
            f2 = (2.d0)*(x(i))*(log((2.d0)*(x(i))))
            
            return
        end function f2

        !#######################################################################################################

        subroutine condicoes_iniciais(m, u_ant, x)
            ! Atribui às condições iniciais a malha
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     u_ant(0:m,0:n)    :: malha inicial discretizada
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     call condicoes_iniciais(m, u_ant, x)
            !
            implicit none
            
            integer :: i, m
            real(8) :: u_ant(0:m,0:m), x(0:m)
                
            do i=0, m
                u_ant(i,0) = f1(x, i, m)
                u_ant(i,m) = f2(x, i, m)
            end do
    
        end subroutine condicoes_iniciais
        
    !#######################################################################################################
        
end module cond_ini