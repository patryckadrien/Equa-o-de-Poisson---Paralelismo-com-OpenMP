module sol_ex
    ! Código contendo o algoritmo que calcula as soluções exatas da malha
    
    implicit none
                          
    contains 

    !#######################################################################################################
        
        subroutine solucao_exata(n, u_exato, x, y)
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     u_exato(0:m,0:n)  :: malha inicial discretizada
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     call solucao_exata(n, u_exato, x, y)
            !
            implicit none
            
            integer :: j, i, n
            real(8) :: u_exato(0:n,0:n), x(0:n), y(0:n)
                
            do i=0, n
                do j=0, n 
                    u_exato(i, j) = (x(i))*(y(j))*(log((x(i))*(y(j))))
                end do 
            end do
        
        end subroutine solucao_exata
    
    !#######################################################################################################
    
end module sol_ex