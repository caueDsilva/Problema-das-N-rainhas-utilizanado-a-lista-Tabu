#CODIGO FINALIZADO N-QUEENS
# VERSAO 1.4

n <- (readline("Digite o numero de rainhas: ")) # qtd de queens -----------------
n <- as.numeric(n)


vetor <- sample(1:n,n ,replace = FALSE) #vetor aleatorio
tabu <- matrix(0,nrow =n, ncol= n) #matriz tabu
QTD <- 0


#funcao que altera a posicao do vetor 
swap <- function(v,sw){ 
  
  v[c(sw[1], sw[2])] <- v[c(sw[2], sw[1])]
  return(v)
  
}

#mostra a quantidade de conflitos
#Entrada -- vetor de queens
#Saida -- numero de conflitos

sol <- function(v){
  n <- length(v) # qtd de queens ---------------
  
  v_dp <- 1:n
  v_dn <-1:n
  
  for( i in 1:n){
    
    v_dp[i] <- i - v[i] #cria valores da DP
    v_dn[i] <- i + v[i] #cria valores da DN
    
  }
  
  #conta a quantidade de numeros reptidos dentro do vetor da DP
  cont_1 <- table(v_dp) 
  soma <- sum(cont_1 - 1)
  
  
  #conta a quantidade de numeros reptidos dentro do vetor da DN  
  cont_2 <-table(v_dn)
  soma <- soma + sum(cont_2 -1)
  soma
  
  return(soma)
  
}

#Gera nova solucao e att a matriz tabu
#Entrada -- vetor de queens e matriz tabu
#saida -- novo vetor de queens e matriz att
new_sol <- function(v,m,atual){
  
  if(QTD >= 1000){ #Limite de quantidade de vezes que se pode trocar o vetor dentro da funcao
    resultado <-list(v,m) 
    return(resultado)
  }
  
  timer<-100 # tempo que a troca n podera ser feita
  n <- length(v) # qtd de queens -----------------
  
  sw <- sample(1:n,2, replace = FALSE) #vetor contendo as posicoes aleatorias a serem trocadas
  sw <- sort(sw) #coloca o vetor em ordem crescente
  
  #chama a funcao() swap e retorna o novo vetor alterado
  vet_teste <-swap(v,sw)
  
  # chama funcao sol() para retornar o novo numero de conflitos 
  novo <- sol(vet_teste)
  
  #caso o novo vetor seja melhor em relacao ao antigo ent:
  if(m[sw[1],sw[2]]==0 && novo < atual){ # caso a posicao de troca seja zero(0) ent:
    
    v <-vet_teste
    m[sw[1],sw[2]] <- m[sw[1],sw[2]] + (timer + 1) #add timer na parte superior
    m[sw[2],sw[1]] <- m[sw[2],sw[1]] + 1 #add qtd de vezes reptidas
    QTD <<- QTD + 1
    
    for (i in 1:n) {
      for (j in i:n) {
        if(m[i,j]!=0){
          m[i,j]<- m[i,j]-1 # diminui os valores acima da diagonal superior da matriz tabu
        }
      }
    }
    
    resultado <-list(v,m) #gera resultado em lista para poder att o vetor e a matriz
    return(resultado)
  }else{
    QTD <<- QTD + 1
    return(new_sol(v,m,atual)) #caso a troca esteja bloqueada, retorna a propria funcao
  }
}





# main -----------------------------------------------
colisao_atual <- sol(vetor)
cat("primeiro vetor: ",vetor,"\n")
cat("numero de colisoes: ",colisao_atual, "\n")

cat("\n")

vetor
colisao_atual


max_iteracao <- 5000  # Defina o número máximo de iterações aqui

while (colisao_atual > 0 && max_iteracao > 0) {
  resultado <- new_sol(vetor, tabu, colisao_atual)
  vetor_new <- resultado[[1]]
  tabu <- resultado[[2]]
  
  
  vetor <- vetor_new
  colisao_atual <- sol(vetor_new)
  
  max_iteracao <- max_iteracao - 1
}

cat("vetor final : ",vetor,"\n")
cat("numero de colisoes: ",colisao_atual,"\n")




#colisao_atual <- sol(vetor) #devolve o numero de colisao atual
#vetor
#colisao_atual

#resultado <- new_sol(vetor,tabu,colisao_atual) #solução com o novo vetor e matriz att

#vetor_new <- resultado[[1]] #novo vetor 

#new_coli <- sol(vetor_new) #novo numero de colisoes utilizando o novo vetor
#vetor_new
#new_coli

#tabu <- resultado[[2]]
#tabu