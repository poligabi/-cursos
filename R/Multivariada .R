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

plot(spa$X, spa$Y, type="n")    #type=n gera gráfico sem nada, pontos estão lá mas invisíveis, dai vais add caract dele nas funções abaixo
lines(spa$X, spa$Y, col="blue4", lwd=2)     #slwd= grossura da linha, eleciona todos juntos no script e executa (ctrlR), ou add + 
text(spa$X, spa$Y, row.names(spa), col="red")   #add rotulo de cada variável

library(vegan)

# Add riqueza d sp.
riqueza<-specnumber(spe) #função do pacote q calcula riqueza através dos dados brutos

plot(spa$X, spa$Y, type="n", ylim=c(20,120),    #função concatenar foi usada p ...
xlab= "Coordenada X(km)", ylab= "Coordenada Y(km)", las=1) +
lines(spa$X, spa$Y, col="blue4") +
points(spa$X, spa$Y, cex=riqueza/3.5,pch=16, col="gray") +   #add círculos de riqueza (dividiu por 3.5 pois riqueza ficava muito grande no grafico)
points(spa$X, spa$Y, cex=riqueza/3.5,pch=1) +   #add bordas, permite vermos as sobreposições dos círculos
points(spa$X, spa$Y, cex=0.5, pch=16)   #add centros

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
install.packages("ade4") #prof nao usou
library(ade4)
library(vegan) #carregado APÓS ade4 p evitar conflitos
library(cluster)
install.packages("gclus") #usado p mapa d calor
library(gclus)
install.packages("FD") #prof nao usou
library(FD)

setwd("C:/R/Multivariada)
spe<-read.csv("DoubsSpe.csv", row.names=1)
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
#nc= numero de cores no mapa (magenta=+prox, cyan=+dist)

#Bray-curtis
coldiss(spe.db, byrank=FALSE,diag=TRUE)
#Jaccard
coldiss(spe.dj, byrank=FALSE,diag=TRUE)



######################################
# Aula 4 Resolvendo dever de casa
# 22-vii-2017
#######################################
spa<-read.csv("DoubsSpa.csv", row.names=1) 
env<-read.csv("DoubsEnv.csv", row.names=1)
spe<-read.csv("DoubsSpe.csv", row.names=1)

#Remover linha vazia (nao tem sp registradas)
not use: spe<- spe[-8,] #comando arriscado pois se repetires comando ele tirará a linha -8 de novo mesmo sem ser a q queriamos tirar
               # melhor ou renomear diferente, ou tirar direto na planilha de campo se a info é tão desencessária
spe_8<- spe[-8,]
env_8<- env[-8,]
spa_8<- spa[-8,]

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

## Calcular Jaccard:
# method="jaccard", binary=TRUE nos argumentos
# binary=TRUE excencial, tranforma dados em binários (tudo >1 vira 1), exceto se dados já forem binários.

#Mapa de Calor:
install.packages("gclus")
library(gclus)

source("coldiss.R") #nesse caso há uma função completa já salva
# podes salvar arquivo (script) em uma area de trabalho, ou pacote, e o source executa o arquivo inteiro
coldiss(spe.bray)

dmat.color(spe.bray, colors= default.dmat.color, byrank=, ...) # Modificar as cores da matriz

#############################
# Aula4-p2 Ordenação -> PCA #
# 22-viii-2017              #
#############################
setwd("C:/R/NEwR")
spa<-read.csv("DoubsSpa.csv", row.names=1) 
env<-read.csv("DoubsEnv.csv", row.names=1)
spe<-read.csv("DoubsSpe.csv", row.names=1)
fisqui <- env[,5:11]
spe_8<- spe[-8,]
library("vegan")

pca.fisqui<-rda(fisqui, scale=T) 
#scale=T é oq vai padronizar (standardization) os dados, sem ele será usado a matriz de covariancia
#só nao precisa dele qnd todas as variáveis tem a mesma unidade

summary(pca.fisqui)
#'cumulative proportion' quanta explicação é fornecida por n Componentes Principais (PC2=olhar p 2 componentes, é até onde vamos usar, par a par)
#'species scores' qnt informação cada variável tras p explica cada Componente Principal (+valor, +explica)
# valores do componente (site scores) pode ser usado como variável

biplot(pca.fisqui)
# grafico PC1 x PC2, cada linha vira um ponto
# as variáveis fisico-químicas são expressas em linhas de explicação (inclinação indicada no 'species scores')
# +prox =+correlacionadas, +perpendiculares=+independente das outras

str(pca.fisqui) #mostra resultado por dentro
pca.fisqui$CA$eig  #chama só os autovalores 'eigenvalue'
pca.fisqui$CA$v  #Valores das variáveis 'species scores'
pca.fisqui$CA$u  #Coordenadas das unidades amostrais 'site scores'

#armazenar os 2 primeiros componentes (se quiser fazer... ex regressão)
pc1<-pca.fisqui$CA$u[,1]
pc2<-pca.fisqui$CA$u[,2]
#exemplo tosco:
riqueza<-specnumber(spe)
plot(riqueza~pc1)

#######################################
PCA meus dados

library("vegan")
setwd("C:/R/Multivariada")
condi<-read.csv("consumDiario.csv", row.names=1:3) 
str(condi)
txn<-condi[,c(4:77,12:17)]
pca.txn<-rda(txn, scale=T) 
summary(pca.txn)
biplot(pca.txn)
#####################################

# Aula 5 +Ordenações (NMDS) 
# 24-viii-2017
########################################
setwd("C:/R/NEwR")
spa<-read.csv("DoubsSpa.csv", row.names=1) #row.names informa qual coluna tem 'nome" de cada variável (variavel 1,2,3,4,5...)
env<-read.csv("DoubsEnv.csv", row.names=1)
spe<-read.csv("DoubsSpe.csv", row.names=1)
fisqui <- env[,5:11]
spe_8<-spe[-8, ]
env_8<- env[-8,]
fisqui_8<-fisqui[-8,]
library("vegan")

#criando a matriz de distância
spe.jac<-vegdist(spe_8, method="jaccard", binary=T) #lembre d sempre por binary=T qnd usa jaccard

#analise NMDS
help(metaMDS)  #ver caracteristicas da função: try= repetição mínima; trymax=maximo de repetições
nmds.jac<-metaMDS(spe.jac) #ele ele avisa "New best solution' cada vez q acha um novo resultado ótimo
     #mas ele ainda continua rodando as quantias demandadas, avisa tbm de resultados similares ao ótimo
     # se acha no início ótimo, se ele demora p achar, aumente as repetições, eventualmente ocorrerá momento q as 10 ultimas soluções são inúteis, aí vc pára.

plot(nmds.jac) #grafico de disperção
plot(nmds.jac, type="t")       #type="t" troca bolinhas pelos nomes das unidades amostrais
      
#vamos olhar altitude na tentativa de agrupar os dados
summary(env)
as.data.frame(env[,2]) #usa o as(mostre-me como) p R mostrar os dados em um colunas(data.frame) ao invés da forma de vetor(tudo em linhas)

#olhamos e vamos classsificar como 1-10 (mais altas), depois 11-, 
#se vais a campo essa quebra deveria ja ser definida por conheceres as características da area 
      
#Criar classe (usar Logica)
alt.clas<-ifelse(env_8$alt>600,"alto","baixo") #if >600 =alto, else =baixo
alt.clas<-as.factor(alt.clas) #transforma o objeto em fator (variável categórica) q agora pode ser usada nas analises
      #esse é um argumento circular sem perigo, bão gera erro se executar alt.clas 2 vezes
      
cores.alt<-c("blue4","red4")

plot(nmds.jac, type="n")	#grafico em branco e vais add as características: 
points(nmds.jac, pch=16, col=cores.alt[alt.clas])   
      #no resultado vimos q as espécies de altitudes +baixas apresentam uma distribuição no grafico +heterogenea
ordihull(nmds.jac, alt.clas, col=cores.alt) #add poligonos convexos
legend("topleft", legend=levels(alt.clas), pch=16, col=cores.alt)

      fisqui_8<-fisqui[-8,]

## envfit
envfit.jac<-envfit(nmds.jac,fisqui_8) #analises sem nome, tem a logica (olhar o help)
#entao ao citar deves explicar: ela busca relações lineares em uma analise de ordenação
plot(envfit.jac) #aparece as linhas de regressões de cada variável fisico-quimicas no grafico como na PCA
envfit.jac	#mostra os resultados das regressões (relações lineares)
#no caso de muitas variáveis, ao invés de usar os dados fisico-químicos direto, poderias aplicar os resultados da PCA q já fizemos dos dados fisico-químicos

#Vamos verificar se uma das variáveis inportantes (oxigenação) tem relação com a altitude q usamos p agrupar
plot(env_8$oxy~env_8$alt, pch=16)
cor(env_8$oxy,env_8$alt)

boxplot(env_8$oxy~alt.clas, range=0)
boxplot(env_8$nit~alt.clas, range=0)
#resultados até agora, oxigênio é importante para determinar a composição
#e nitrogênio parece se conectar bem com a divisão de classes de altitude

############################################################################
# Aula 6 Permutação de Matrizes (Mantel, anosim, adonis, autocorrelograma) #
# 25-viii-2017                                                             #
############################################################################
setwd("C:/R/NEwR")
spa<-read.csv("DoubsSpa.csv", row.names=1) #row.names informa qual coluna tem 'nome" de cada variável (variavel 1,2,3,4,5...)
env<-read.csv("DoubsEnv.csv", row.names=1)
spe<-read.csv("DoubsSpe.csv", row.names=1)
fisqui <- env[,5:11]
spe_8<-spe[-8, ]
spa_8<-spa[-8, ]
env_8<- env[-8,]
fisqui_8<-fisqui[-8,]
library("vegan")
      
      
## Teste de Mantel      
      
fisqui.pad<-decostand(fisqui_8, method="standardize") 
fisqui.euc<-vegdist(fisqui.pad, method="euclidean") #matriz de distância de dados físico-químicos
      
spe.jac<-vegdist(spe_8, method="jaccard", binary=T) #matriz de distância de dados de espécies

mantel(fisqui.euc, spe.jac)      
# Mantel statistic r: 0.4199 #por volta de 43% das variações são explicadas pelas variaveis fisico-quimicas (mas a analise não dá causa e efeito)
#      Significance: 0.001 

# Sheppard plot       
plot(fisqui.euc, spe.jac, pch=16, cex=0.5)   # grafico mostra(funil) q a partir de certa distância a variação passa a ser enorme
      #na pratica dados geraram um funil invertido, ambientes com diferentes características se encontra sp. bem diferentes, 
      #mas qnd carcteristicas são próxima a presença de sp. é imprevisível, tem de muito prox a muito diferentes.

spa.euc<-vegdist(spa_8, method="euclidean") #matriz de distância de dados espaciais     

# Mantel parcial:      
mantel.partial(fisqui.euc, spe.jac, spa.euc)    
      
######################
#Anosim e Permanova(adonis)

#Criar variável categórica:
alt.clas<-ifelse(env_8$alt>600,"alto","baixo") #if >600 =alto, else =baixo
alt.clas<-as.factor(alt.clas) #transforma o objeto em fator (variável categórica)

##
#Anosim
resultado.anosim<-anosim(spe.jac,alt.clas)
resultado.anosim        #maior R maior diferença de composição entre os grupos
plot(resultado.anosim)  #mostra dif (mediana) dentro dos grupos e entre os grupos, a largura do boxplot =n, 
      #resultado mostrou diferença menor dentro dos grupos do que entre os grupos
      
##      
# adonis  
resultado.adonis<-adonis(spe.jac~alt.clas)
resultado.adonis #gera um coenficiente de determinação (R2=proporção da variável resposta explicada pela explicativa)(anosim mostra correlaçao)
   
#########################################      
## Autocorrelogramas de Mantel      
      
jac.spa<-mantel.correlog(spe.jac, spa.euc) #aqui a ordem importa, a nao ser q escreva por extenso, primeiro argumennto: d.eco= segundo: d.geo=
jac.spa	#cria classes de distância (class.index= até 9m, até 25m, até 42m...), 
#p cada classe indica o n presente nelas (qnd muito baixo o n não consegue calcular alto correlação)
#há autocorrelação qnd Pr(mantel) menor q 0.05
plot(jac.spa) #quadradinho preto autorrelação presente, branco ausente(não signif) ou não calculado
#quando a linha vermelha cruza a linha dos resultado é o ponto ideal que não há autocorrelação espacial, seria a distância entre coletores q deves usar na sua coleta de dados

##############################
## Métodos de filtros espaciais -> filtrar os resíduos

riqueza<-specnumber(spe_8)

#1-PCA:
spa.pca<-princomp(spa.euc) 	#usamos pricomp pois função rda nao aceita objeto q não seja data.frame, converter p matriz chato se tem outras q fação PCA
spa.pca

#2-Extrai os filtros:
summary(spa.pca)	#Proporção de explicação de cada componente p escolher quais usar
filtros<-spa.pca$scores[,1:5] #até o 5 já explica 99%, scores é o local dos resultados da PCA por unidade amostral

#3-Reg mpultipla
resultado.filtros<-lm(riqueza~filtros)
summary(resultado.filtros)

#4-Pegando os resíduos
riqueza.res<-resultado.filtros$residuals
riqueza.res

#5- agora podes usar a variável como desejado 
#resíduos são a parte das variáveis sem receber influencia das variáveis analisadas até o momento (espaciais)
      
      
      
      
