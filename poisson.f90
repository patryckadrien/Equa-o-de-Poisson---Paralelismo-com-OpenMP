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
    
    integer, parameter :: n=160, m=160
    real(8)	       :: a, c, h, diferenca, u_ant(0:m,0:n), &
			  u_novo(0:m,0:n), x(0:m), y(0:n), &
			  u_exato(0:m,0:n), u_dif(0:m,0:n)
    
    a=1.d0
    c=1.d0 
	
    print*, 'Malha ', n, 'x', m    
    print*

    ! Inicializando a malha
    call inicializacao(m, h, a, c, x, y)
    
    ! Aplicando as condições iniciais na malha
    call condicoes_iniciais(m, u_ant, x)
    
    ! Aplicando as condições de contorno na malha
    call condicoes_contorno(n, u_ant, u_novo, y)

    ! Calculando a malha com as soluções exatas para comparação
    call solucao_exata(n, u_exato, x, y)

    ! Chamada do método de Gauss-Seidel
    call sub_metodo_gauss_seidel(n, m, u_ant, u_novo, h, x, y)
    
    ! Calculando a maior diferenca relativa entre a solução obtida e a solução exata
    diferenca = dif_rel(u_novo, u_exato, m, n)
    
    print*
    print*, 'Diferença relativa:', diferenca      
    print*
   
    !u_dif = u_exato - u_novo
    
    !call print_mat(u_dif, 'f15.10')
    
    contains
     
    !#######################################################################################################
        
        subroutine print_mat(mat, num_fmt)
            ! Imprime os elementos de uma matriz mat, no formato num_fmt especificado pelo usuario, na tela. A 
            ! subrotina determina automaticamente a forma da matriz com a função intrínseca shape.
            !
            ! Variáveis de entrada:
            !
            ! Tipo REAL(8):
            !
            !     mat   (real(8), dimension(:,:))   :: matriz a ser impressa na tela
            !
            ! Tipo CHARACTER:
            !
            !     num_fmt (character(*))            :: cadeia de caracteres contendo o formato de cada elemento.
            !
            ! Variáveis internas de relevância:
            !
            ! Tipo CHARACTER:
            !
            !     ntimes (character(10))            :: número de elementos numa linha de mat, em formato de string
            !     frmt (character(:), allocatable)  :: string especificadora de formato
            !
            ! Tipo INTEGER:
            !
            !     len_fmt                           :: comprimento da string especificadora de formato
            !     dims (integer, dimension(2))      :: vetor de 2 posições que conterá a forma de mat
            !
            ! Exemplo de uso:
            !         call print_mat(a, 'f10.4')    ! Escreve a matriz em formato real com 10 caracteres, quatro 
            !                                       ! para a parte decimal,cinco para a parte inteira
          
            implicit none
            
            integer                   :: i, len_fmt, dims(2)
            character(len=10)         :: ntimes
            real(8)                   :: mat(:,:)
            character(*)              :: num_fmt
            character(:), allocatable :: frmt
            dims = shape(mat)          
          
            ! Conversão de inteiro para string do número de colunas de mat.
            write(ntimes,'(I10)') dims(2)
            ntimes = adjustl(ntimes)
            
            ! Determinação do comprimento da string para podermos alocar a memória para ela dinamicamente.
            len_fmt = len('('//trim(ntimes)//num_fmt//')')
          
            allocate(character(len_fmt) :: frmt)
            frmt = '('//trim(ntimes)//num_fmt//')'
            
            ! Impressão de mat, linha por linha.
            do i = 1, dims(1)   !nlin 
              write(*,frmt) mat(i,:)
            end do
          
            if (allocated(frmt)) deallocate(frmt)
          
        end subroutine print_mat

End Program poisson
