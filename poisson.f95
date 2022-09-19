Program poisson
    ! Código contendo as subroutinas necessárias para a resolução da EDP de Poisson
    ! atráves do método iterativo de Gauss-Seidel
    !
    ! Variáveis de relevância:
    !
    ! Variáveis do tipo INTEGER:
    !
    !     n (parameter)     :: dimensão da malha
    !     m (parameter)     :: dimensão da malha
    !
    ! Variáveis do tipo REAL(8):
    !
    !     u_ant(0:m,0:n)    :: malha inicial discretizada 
    !     u_novo(0:m,0:n)   :: malha discretizada a ser iterada
    !     u_exato(0:m,0:n)  :: malha com as soluções exatas
    !     x(0:m)            :: vetor com as discretizações iniciais da malha
    !     y(0:n)            :: vetor com as discretizações iniciais da malha
    !     tol               :: tolerância mínima para os resultados
    !     a                 :: parâmetro inicial
    !     c                 :: parâmetro inicial
    !     h                 :: comprimento da subdivisão da malha
    !     diferenca         :: valor da maior diferença relativa entre as soluções
    !
    ! Usamos a equação de Poisson da apresentada abaixo para mostrar o resultado 
    ! após a aplicação do método iterativo de Gauss-Seidel.
    !
    ! u_xx + u_yy = x/y + y/x
    !
    ! Com as condições de contorno
    !
    ! u(x, 1) = xln(x), u(x, 2) = 2xln(2x)
    ! u(1, y) = yln(y), u(2, y) = 2yln(2y)
    !
    ! cuja solução exata é u(x, y) = xyln(xy)

    use inicializa
    use cond_ini
    use cond_con 
    use sol_ex 
    use met_gs 

    implicit none
    
    integer, parameter :: n=1000, m=1000
    real(8)            :: u_ant(0:m,0:n), u_novo(0:m,0:n), a=1.d0, &
                          c=1.d0, tol=1.d-16, h, x(0:m), y(0:n),&
                          u_exato(0:m,0:n), diferenca


    ! Inicializando a malha
    call inicializacao(m, h, a, c, x, y)
    
    ! Aplicando as condições iniciais na malha
    call condicoes_iniciais(m, u_ant, x)
    
    ! Aplicando as condições de contorno na malha
    call condicoes_contorno(n, u_ant, u_novo, y)

    ! Calculando a malha com as soluções exatas para comparação
    call solucao_exata(n, u_exato, x, y)

    print*, 'Método de Gauss-Seidel para tolerância de', tol
    print*

    ! Chamada do método de Gauss-Seidel
    call sub_metodo_gauss_seidel(n, m, u_ant, u_novo, tol, h, x, y)
    
    ! Calculando a maior diferenca relativa entre a solução obtida e a solução exata
    diferenca = dif_rel(u_novo, u_exato, m, n)
    
    print*
    print*, 'Diferença relativa:', diferenca     
        
End Program poisson