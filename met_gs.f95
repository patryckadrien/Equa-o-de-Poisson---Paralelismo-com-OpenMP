module met_gs
    ! Código contendo o algoritmo iterativo de Gauss-Seidel, em forma de subroutine.
    
    implicit none
                          
    contains 

    !#######################################################################################################
    
        real(8) function f(i, j, m, n, x, y)
            ! Função chamada para o cálculo da função f dada pela equação
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = f(i, j, m, n, x, y)
            !
            implicit none
            
            integer :: i, j, m, n
            real(8) :: x(0:m), y(0:n)
            
            f = (((x(i))/(y(j))) + ((y(j))/(x(i))))

            return
        end function f
        
        !#######################################################################################################
        
        real(8) function norma_infinita_mat(m, n, mat)
            ! Função chamada para o cálculo da norma infinita da malha, calculando
            ! a soma do módulo das linhas da matriz e obtendo o maior valor entre a soma
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     mat(0:m,0:n)      :: malha discretizada
            !
            ! Variáveis internas de relevância:
            !
            ! Tipo REAL(8):
            !
            !     somas(n)          :: somatório dos coeficientes da malha
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = norma_infinita_mat(m, n, mat)
            !
            implicit none
            
            integer :: n, i, m
            real(8) :: mat(0:m,0:n), somas(n)
            
            do i = 1, n
                somas(i) = sum(abs(mat(i,:)))
            end do
            
            norma_infinita_mat = maxval(somas(:))
            
            return
        end function norma_infinita_mat
        
        !#######################################################################################################
    
        real(8) function dif_rel(u_novo, u_exato, m, n)
            ! Função chamada para o cálculo da maior diferença relativa 
            ! entre a solução obtida e a solução exata, calculando a norma infinita
            ! da diferença entre a solução exata menos a solução obtida dividido 
            ! pela norma infinita da solução exata
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     n                 :: dimensão da malha
            !     m                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     u_novo(0:m,0:n)   :: malha com as soluções obtidas
            !     u_exato(0:m,0:n)  :: malha com as soluções exatas
            !
            ! Exemplo de uso:
            !
            !     variavel_qualquer = dif_rel(u_novo, u_exato, m, n)
            !
            implicit none
            
            integer :: m, n
            real(8) :: u_novo(0:m,0:n), u_exato(0:m,0:n)
            
            dif_rel = norma_infinita_mat(m, n, (u_exato - u_novo)) / norma_infinita_mat(m, n, u_exato)
            
            return
        end function dif_rel

        !#######################################################################################################
    
        subroutine sub_metodo_gauss_seidel(n, m, u_ant, u_novo, tol, h, x, y)
            ! Método iterativo de Gauss-Seidel para obtenção das soluções da malha
            !
            ! Variáveis de entrada de relevância:
            !
            ! Tipo INTEGER:
            !
            !     m                 :: dimensão da malha
            !     n                 :: dimensão da malha
            !
            ! Tipo REAL(8):
            !
            !     u_ant(0:m,0:n)    :: malha inicial discretizada
            !     u_novo(0:m,0:n)   :: malha discretizada a ser iterada
            !     x(0:m)            :: vetor com as discretizações iniciais da malha
            !     y(0:m)            :: vetor com as discretizações iniciais da malha
            !     tol               :: tolerância mínima para os resultados
            !     h                 :: comprimento da subdivisão da malha
            !
            ! Variáveis internas de relevância:
            !
            ! Tipo INTEGER:
            !
            !     k                 :: quantidade de iterações
            !
            ! Tipo REAL(8):
            !
            !     delta             :: valor da tolerância que será iterada
            !
            ! Exemplo de uso:
            !
            !     call sub_metodo_jacobi(n, m, u_ant, u_novo, tol, h, x, y)
            !
            implicit none
        
            integer :: n, m, k=0, i, j
            real(8) :: u_ant(0:m,0:n), u_novo(0:m,0:n), delta = 1.d5, tol, h, &
                        x(0:m), y(0:m)
        
            do while((delta >= tol))
                do i=1, m-1
                    do j=1, n-1 
                        u_novo(i, j) = (u_novo(i-1, j) + u_ant(i+1, j) + &
                                        u_novo(i, j-1) + u_ant(i, j+1) - &
                                        ((h**2)*f(i, j, m, n, x, y)))/(4.d0)
                    end do 
                end do
                delta = dif_rel(u_novo, u_ant, m, n)
                k = k+1 
                u_ant = u_novo 
            end do
            print*, 'Quantidade de iterações:', k
            
        end subroutine sub_metodo_gauss_seidel
    
    !#######################################################################################################
    
end module met_gs