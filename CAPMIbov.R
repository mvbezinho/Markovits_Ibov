# Verificando o diretorio
getwd()

# Importando os dados que tem que estar nesta mesma pasta de diretorio
myData <- read.table(file="mtl_ret_2016.csv", header=TRUE, sep=';')

# Verifico o tamanho da base para setar os parametros do loop
dim(myData) # tenho 149 observacoes temporais (meses) de 419 acoes 
            # dentre elas, as duas primeiras sao a Rf e o mercado

# armazeno o valor das dimensoes da base de dados em uma variavel
L <- ncol(myData)
J <- L - 2

#ESTIMO O Rm
Rm <- myData[ ,2] - myData[ ,1]

# Substituo isso nas matrizes de armazenamento do loop
myBetas <- rep(0, J) # TRES ACOES DIFERENTES
myAlphas <- rep(0, J) # SEUS RESPECTIVOS ALPHAS
myTs <- rep(0, J) # TESTES T-STUDENT
myAver <- rep(0,J) # RETORNOS EM EXCESSO DE CADA COMPANHIA (acao - mercado)

# Criando um loop para armazenar isso de todos os datasets
# 2+j porque a primeira coluna do df sao a tx s/risco e o mercado
# altero o J no for 
for (j in 1:J){
  Rj <- myData[ ,2+j] - myData[ ,1]
  myResult <- lm(Rj ~ Rm)
  mySummary <- summary(myResult)
  myCoef <- coef(mySummary)
  
  myBetas[j] <- myCoef[2,1]
  myAlphas[j] <- myCoef[1,1]
  myTs[j] <- myCoef[1,3]
  myAver[j] <- mean(Rj)
}

# Verificando graficamente (FRONTEIRA EFICIENTE!!!)
plot(myBetas, myAver)

# Plotando uma linha diagonal que demarca quais acoes apresentam retornos excesso
x <- c(1:185)/100 # eixo x varia de acordo com meus betas
points(x, mean(Rm)*x, col="red", type="l") # plotando isso no grafico

# De acordo com o modelo de CAPM, os dados deveriam estar em torno desta reta
# Como este nao parece ser o caso, e necessario hierarquizar as acoes com maiores e menores betas
# para, assim, classificar o desvio da base

myNames <- colnames(myData) # retiro os rotulos das companhias
myNames2 <- myNames[3:L] # retiro as duas primeiras colunas

myData3 <- data.frame(myNames2, myBetas) # CRIO UM DATAFRAME (df) COM DUAS COLUNAS DISSO E OS BETAS

myOrder <- order(myBetas, decreasing=T) # filtro o df em ordem decressente em relacao aos betas

X <- myData3[myOrder, ] # armazeno este resultado em uma nova coluna em uma nova lista
Beta.high <-X[1:10, ] # crio uma coluna nela com os 10 maiores betas
Beta.low <-X[J:(J-9), ] # crio otura com os 10 menores betas

myData4 <- data.frame(myNames2, myAlphas, myTs)
