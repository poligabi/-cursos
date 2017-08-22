#############################################################
# Aula1 Explorando dados no R com Marcos Vital 10-viii-2017 #
#############################################################
data(iris) #Carrega data.frame
iris       #Mostra objeto todo (nessa caso o data.frame)
summary(iris) #Resume as estatísticas descritivas do data.frame
str(iris)   #Mostra a estrutura do objeto (qnd objeto é muito grande, como na multivariada, é +interessante que o summary)

iris$Sepal.Width   #Mostra só essa coluna
iris [,2]          #Mostra só essa colun
iris[10,]		 #Mostra só essa linha
iris[1:10,1:2]     #Mostra linha 1 a 10, e colunas 1 e 2

#Função concatenar -> c() -> 
iris[,c(2,4)] #usamos concatenas para mostrar as colunas 2 e 4
iris[c1:10,15:20), c(1,5)] #Aqui mostramos linhas 1 a 10, 15 a20 e colunas 1 e 5

#ctrl R -> executa linha do script e cursor já vai p seguinte, 
# se função tem mais de uma linha precisa selecionar ambas anter do crtl R

##############################################################################

## Graficos simples## 
 
#-> ggplot2   gg=grammar of graffics (cria uma semantica de graficos)
#(ao invez de uma função p cada grafico, ele cria codigos p cada elemento do grafico que pode ir sendo adicionada) 
# criador do ggplot2 cria padrao de escrever, exemplo função é sempre verbo, sempre te diz oq faz

plot(iris$Petal.Length, iris$Petal.Width, las=1, pch=16,
xlab="Comprimento de pétalas", ylab="Largura da pétalas",

#las = orientação dos eixos, se não especifica, o R usa argumento na forma padrão
#pch = define o tipo de símbolo, ex 16=bolinha cheia, ?pch para +detalhes
#link: r-graph-gallery.com =varios scripts d graficos
#relevel = altera ordem dos itens (que por padrão é alfabética)

## Recriando gráfico com cores
levels(iris$Species) # chama variavel categorica que escolhestes pra agrupar dados
cores<-c("red4", "green4","blue4") #link de cores no R: stat.columbia.edu/~tzheng/files/Rcolor.pdf

plot(iris$Petal.Length, iris$Petal.Width, las=1, pch=16,
xlab="Comprimento de pétalas", ylab="Largura da pétalas",
col=cores[iris$Species]) 

especies <- levels(iris$Species)
legend("topleft", legend=especies,col=cores, pch=16, text.font=3) #legenda das cores p cada especie, 
#localizada topleft do grafico
#text.font =tipo de fonte (2=negrito,3=italico,4=negrito e italico)



####################################################################
# Aula2 Gráficos de riqueza de espécies e correlações 15-VIII-2017 #
####################################################################
install.packages("vegan")

# Abrir área de trabalho (pasta contendo arquivos trabalhados)
setwd("C:/R/NEwR") #dados do livro Numerical Ecology with R baixados no link: http://adn.biol.umontreal.ca/~numericalecology/numecolR/
 dir()

# Criar 3 objetos (variáveis espaciais, ambientais e dados das espécies)

spe<-read.csv("DoubsSpe.csv", row.names=1) #row.names informa qual coluna tem os nomes delas (de cada linha, ex: 1,2,3,4...)
spa<-read.csv("DoubsSpa.csv", row.names=1)
env<-read.csv("DoubsEnv.csv", row.names=1) #read.table precisa de header=T, read.csv não

str(spe) #conferindo dados
str(spa) 
str(env)  #podes conferir usando summary(), mas não é muito pratico qnd os dados tem muitas colunas
 
plot(spa$X, spa$Y) #autor sugere ir add camadas em camadas, nesse caso, com dados espaciais, vemos só os pontos do rio

plot(spa$X, spa$Y, type="n")    #type=n gera gráfico sem nada, pontos estão lá mas invisíveis
lines(spa$X, spa$Y, col="blue4", lwd=2)     #slwd= grossura da linha, eleciona todos juntos no script e executa (ctrlR), ou add + 
text(spa$X, spa$Y, row.names(spa), col="red")   #add rotulo de cada variável

library(vegan)

# Add riqueza d sp.
riqueza<-specnumber(spe) #função do pacote q calcula riqueza através dos dados brutos

plot(spa$X, spa$Y, type="n", ylim=c(20,120),    #função concatenar foi usada p ...
xlab= "Coordenada X(km)", ylab= "Coordenada Y(km)", las=1) +
lines(spa$X, spa$Y, col="blue4") +
points(spa$X, spa$Y, cex=riqueza/3.5,pch=16, col="gray") +   #add círculos de riqueza (dividiu por 3.5 pois riqueza ficava muito grande no grafico)
points(spa$X, spa$Y, cex=riqueza/3.5,pch=1) +   #permite vermos as sobreposições dos círculos
points(spa$X, spa$Y, cex=0.5, pch=16)   #...

###########################
### Explorando Correlações

#vamos criar um objeto com as variáveis físico-químicas da agua:
fisqui<-env[ , 5:11] #todas as linhas, coluna 5 a 11

#correlações entre as variáveis:
cor(fisqui, method="pearson") # pearson é o padrão, argmento seria inutil, mas torna util p leitor

# Painel de correlações
pairs(fisqui)

#ou
# funções copiadas de help(pairs)
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) #podes modificar o method de pearson para outro em panel.cor
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 1)     #finzinho modificado pra ajeitar tamanho do número (2 grande, 1 pequeno)
}
pairs(fisqui, diag.panel=panel.hist, upper.panel=panel.cor) #parte onde insere seus dados
#o painel criado passa a ter, os índices em um triângulo, a disperção em outro, e histogramas com os rótulos na diagonal principal.


#####################################################
# Aula3 Matriz de distância e Clusters 21-viii-2017 #
#####################################################
library("vegan")
setwd("C:/R/NEwR")

# criar 3 objetos (variáveis espaciais, ambientais e de espécies)
spa<-read.csv("DoubsSpa.csv", row.names=1) 
env<-read.csv("DoubsEnv.csv", row.names=1)
spe<-read.csv("DoubsSpe.csv", row.names=1)

fisqui <- env[,5:11]
summary(fisqui)

## Criando Cluster:

# Padronização dos dados (aplique se necessario)
fisquipad<-decostand(fisqui, method="standardize")

#Criando matriz de distancia
fisquipad.euc<-dist(fisquipad, method="euclidean") #o method euclidean ja é o padrão da função dist, R faria sem especificar 

# Calculando o cluster
cluster1<-hclust(fisquipad.euc, method="average") #method q usa medias para comparar distacias
#antes de plotar, vamos calcular a correlação cofenetica pra nao tirar conclusões e se decepcionar depois
fisquipad.euc.coph<-cophenetic(cluster1)
cor(fisquipad.euc, fisquipad.euc.coph) #compara matriz cofenetica com a matriz euclidiana, mais perto de 1 = +semelhantes :D
#resultado 0.8742811 = coeficiente de correlação cofenética

#Visualização plot padrão (da classe hclust) apontando p baixo:
plot(cluster1, hang=-1) # hang-1 estica e padroniza o dendograma

# Visualização horizontal:
fisquipad.euc.dend<-as.dendrogram(cluster1) #função as. permite transformar em classe dendograma o seu cluster
plot(fisquipad.euc.dend, horiz=T)

# Plotando em janela com dimensões pré-determinadas (feche janela grafica antes)
windows(width=8,height=12, rescale="fixed")
plot(fisquipad.euc.dend, horiz=T) #copie como metafile, abra no office, botão direito, desagrupar,
                                  #e aí podes modificar qualquer elemento do gráfico

#########################################
# Dever de casa
# Pesquise no livro e calcule 2 dendogramas usando bray-curtis e jaccard:
# Função vegdist Cap3 pag:35-40
#############################################
install.packages("ade4")
library(ade4)
library(vegan) #carregado APÓS ade4 p evitar conflitos
install.packages("gclus")
library(gclus)
library(cluster)
install.packages("FD")
library(FD)

setwd("C:/R/Multivariada)

spe<-read.csv("DoubsSpe.csv", row.names=1) #row.names informa qual coluna tem nomes
spa<-read.csv("DoubsSpa.csv", row.names=1)
env<-read.csv("DoubsEnv.csv", row.names=1)

#Remover linha vazia
spe<- spe[-8,]
env<- env[-8,]
spa<- spa[-8,]

#Bray-Curtis dissimilarity matrix
spe.db<-vegdist(spe)
head(spe.db)

#Dissimilarity for binary data
#Jaccard matrix
spe.dj<-vegdist(spe, "jac",  binary=TRUE)
head(spe.dj)
head(sqrt(spe.dj))

## Mapa de Calor

library(gclus)
source("coldiss.R")
#coldiss(D=dist.object, nc=4,byrank=TRUE,diag=FALSE) # argumentos padrão
#byrank= TRUE :=sized categories 	FALSE :=length intervals
#diag= TRUE :rótulos posicionados na diagonal
#nc= numeor de cores no mapa

#Bray-curtis
coldiss(spe.db, byrank=FALSE,diag=TRUE)
#Jaccard
coldiss(spe.dj, byrank=FALSE,diag=TRUE)



######################################
# Aula 4 Resolvendo dever de casa
# 22-vii-2017
#######################################

# criar 3 objetos (variáveis espaciais, ambientais e de espécies)
spa<-read.csv("DoubsSpa.csv", row.names=1) 
env<-read.csv("DoubsEnv.csv", row.names=1)
spe<-read.csv("DoubsSpe.csv", row.names=1)

#Remover linha vazia (nao tem sp registradas)
spe<- spe[-8,] #comando arriscado pois se repetires comando ele tirará a linha -8 de novo mesmo sem ser a q queriamos tirar
               # melhor ou renomear diferente, ou tirar direto na planilha de campo se a info é tão desencessária
spe_8<- spe
env_8<- env
spa_8<- spa

#antes vamos olhar a abundância por species
abund.sp<-colSums(spe_8)
abund.sp
hist(abund.sp, col="gray")

#abundancia por unidade amostral
abund.ua<-rowSums(spe_8)
abund.ua
hist(abund.ua, col="gray") 

## Calcular cluster por Bray-Curtis:
# 1-Não precisa transformar
# 2-Calcular Matriz
spe.bray <-vegdist(spe_8, method="bray")
# 3-Calcular cluster
cluster1<-hclust(spe.bray, method="average")
# 4-Coeficiente de correlação cofenética
cluster1.coph<-cophenetic(cluster1)
cor(spe.bray, cluster1.coph)
# 5-Visualizar cluster
spe.bray.dend<-as.dendrogram(cluster1)
plot(spe.bray.dend, horiz=T) 

#Mapa de Calor:
install.packages("gclus")
library(gclus)

source("coldiss.R") 
# podes salvar arquivo (script) em uma area de trabalho / pacote e o source executa o arquivo inteiro
coldiss(spe.bray)


######################
# Aula4-p2 Ordenação
# 22-viii-2017
#######################











