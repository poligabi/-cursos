#############################################################
# Aula1 Explorando dados no R com Marcos Vital 10-viii-2017 #
#############################################################




####################################################################
# Aula2 Gráficos de riqueza de espécies e correlações 15-VIII-2017 #
####################################################################

install.packages("vegan")

# Abrir área de trabalho (pasta contendo arquivos trabalhados)
setwd("C:/R/NEwR") #dados do livro Numerical Ecology with R baixados no link: http://adn.biol.umontreal.ca/~numericalecology/numecolR/
 dir()

# Criar 3 objetos (variáveis espaciais, ambientais e dados das espécies)

spe<-read.csv("DoubsSpe.csv", row.names=1) #row.names informa qual coluna tem nomes
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
points(spa$X, spa$Y, cex=riqueza/3.5,pch=16, col="gray") +   #add círculos de riqueza (dividiu por 3.5 pq a riqueza toda fica muito grande no grafico)
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


######################
# Aula3 21-viii-2017 #
######################
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
fisqui.pad<-decostand(fisqui, method="standardize")

#Criando matriz de distancia







