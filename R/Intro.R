####################################
# Aula 1 - Introdução ao uso de R  #
#        05.03.2018                #
####################################

tempo <- c(18, 14, 14, 15, 14, 34, 16, 17, 21, 26)
max(tempo)
min(tempo)
mean(tempo)

help(union)
A <- c(1,2,3,4,5)
B <- c(4,5,6,7)
union(A,B) # 1 2 3 4 5 6 7
intersect(A,B) # 4 5
setdiff(A,B) # 1 2 3
setdiff (B,A) # 6 7

abs((2^3)-(3^2))   #abs = modulo


#### Gerar repetições # rep(repita x, y vezes)
rep(1:4,2)  # 1 2 3 4 1 2 3 4
rep(1:4, each=2)  # 1 1 2 2 3 3 4 4

locais <- c(rep("A",50), rep("B",50))  #ou
locais1 <- rep(c("A","B"), c(50,50)) #primeiro palavras repetidas e depois quantas vezes repetir cada uma


#### Gerar dados
##Aleatórios # sample(x, size=1, replace=FALSE) # false é o padrão, se colocar TRUE permite repetir numeros já sorteados
sample(1:60, 6) #amostrar os numeros da megasena

seq(1,30,2)  # sequencia so de numeros ímpares seq(from=1, to=30, by=2)

a <- c("A", "B", "C")
table(sample(a, replace=T) # erro: function (..., recursive = FALSE)  .Primitive("c")

table(sample(1:6,250,replace=T)) #mostra quantas vezes cada número foi sorteado

##Aleatórios com distribuição uniforme
runif(gera n valores, min=0, max=1)

##Aleatórios com distribuição normal
rnorm( n, mean=0, sd =1)

temp <- c(9, 0, 10, 13, 15, 17, 18, 17, 22, 11, 15)
sqrt(temp)
log(temp)
log(temp, temp+1)
temp^2

q() #fecha o R, salve workspace
ls() # confere se seus objetos ainda estão salvos no workspace

########################################
# Aula 2 - Introdução ao R  - Graficos #
#        06.03.2018                    #
########################################
setwd("C:/R/intro")
dir()

sapos<- c(6,5,10,11,26,16,17,37,18,21,22,15,24,25,29,31,32,13,39,40)
umid<- c(62,24,21,30,34,36,41,48,56,74,57,46,58,61,68,76,79,33,85,86)
temp<- c(31,23,28,30,15,16,24,27,18,10,17,13,25,22,34,12,29,35,26,19)

par(mfrow=c(1,2)) #deve vir antes dos graficos se queres dividir a janela (linha, colunas)
plot(umid,sapos)
plot(temp,sapos)


aves<-c(22,8,37,34,13,24,39,5,33,32)
arvores<-c(25,26,40,30,10,20,35,8,35,28)
plot(arvores, aves)

arvores2<-c(6,17,18,11,6,15,20,16,12,15)
aves2<-c(7,15,12,14,4,14,16,60,13,16)
points(aves2,arvores2,col="blue") #add pontos ao gráfico aberto

aves.c<-c(aves,aves2) #juntou os arquivos
arvores.c<-c(arvores,arvores2)
locais1<-rep(c("A","B"),c(10,10)) #criei outro com os locais d coleta
locais<-factor(locais1)  #para usar dados categoricos no grafico dado precisa ser transformado em fator

stripchart(aves.c~locais, vertical=T, pch=c(16,17))
stripchart(arvores.c~locais, vertical=T, pch=c(16,17))

stripchart(aves.c~locais, vertical=T, pch=c(16,17), method="stack", offset=1)
stripchart(arvores.c~locais, vertical=T, pch=c(16,17), method="stack", offset=1)
#esse metodo e offset permite que pontos não fiquem sobrepostos

?par

plot(arvores, aves, main="Quantidade de aves por quantia de árvores",xlab="N arvores", ylab="N aves", type="n")  
points(arvores, aves, pch=16)
##################################### Falta ainda alterar a caixa do grafico p aparecer só os eixos

 par(mfrow=c(1,1))

x<-c(2,8)
y<-c(2,8)
plot(x,y, pch=16, xlim=c(1,10), ylim=c(1,10), xlab="1:10", ylab="1:10") #ylim tamanho do eixo y
lines(x,y, ) ########### Falta colocar linha tracejada
x2<-c(4,8)
y2<-c(2,4)
points(x2,y2)
lines(x2,y2)

A<-rnorm(100, mean=10, sd=10)
B<-rnorm(100,8, 15)
plot(A,B)
hist(A,B)

############################
# Aula 2.2 Manejo de dados #
############################

macac <-read.table(file.choose(),header=T) #escolhe arquivo .txt com mouse

a<-c(1,2,3,4)
b<-c(5,6,7,8)
c<-c("a","b","c","d")

cbind(
rbind( #tranformam em matrizes, q só pode ter valores numericos OU caracteres, nunca ambos

data.frame(a,b,c)

attach(macac) #pode digitar e acessar a qualquer momento os atributos dele
macacos
frutas #lembre que Marcos Vital pediu p evitar esse recurso
detach(macac)

mean(macac[,"macacos"])
mean(macac$macacos)
mean(macac$frutas)

tapply(dados,grupos,função) #calcula uma função nos dados em relação aos grupos
tapply(macac$macacos, macac$reserva, mean)
tapply(macac$mfrutas, macac$reserva, mean)

stripchart(macac$macacos~macac$reserva, vertical=T, pch=c(10,5))

names(mol) #nome das variaveis q estao no arquivo mol

mean(mol[,1:6]) #esse comando nao funciona mais
rowMeans(mol[1:5,1:6]) #media de cada linha dessas colunas, em cada ponto d amostragem (lin) tera a media de moluscos (col) encontrados
colMeans(mol[1:5,1:6]) #media de cada coluna usando só essas linhas, teremos a media de cada tipo de molusco (col) na reserva A (lin)
mean(rowSums(mol[1:5,1:6])) # somas as linhas (Reserva A) e tira a media geral, abundancia media na reserva A

t(mol) #transpose, info das  linhas nas colunas e colunas nas linhas

ambi<-simu[,1:6]
spp<-simu[,7:26] #separei dados de especies (cada linha 1 riacho)

pres.aus<-spp #copia p alterar
pres.aus[pres.aus>0]<-1 # tudo maior q zero substitui por 1, vira dado de presença/ausencia

riqueza<-rowSums(sp01) #soma linha = riqueza d sp por riacho
riqueza
tapply(riqueza,ambi$município, mean) # riqueza por município
colSums(pres.aus) # somando colunas = quantos riachos cada espécie ocorreu



str(minhoc)
minhoc[order(minhoc[,"Área"]]  ###### ERRO p ordenar dados por área

plot(minhoc$dens.minhocas~minhoc$Área)
plot(minhoc$dens.minhocas~minhoc$Vegetação)

minhoc$Alagado
minhoc.alag<- which(minhoc$Alagado==TRUE) #linhas dos dados d area alagada

mean(minhoc$dens.minhocas)
tapply(minhoc$dens.minhocas,minhoc$Alagado,mean) #densidade media em locais alagados e não alagados
mean(minhoc$Área)

#tapply aplica funções em uma coluna por vez, para usar em varias: aggregate by

aggregate(spp,list(simu[,1]),mean) #gera dataframe com valores p cada local
aggregate(spp,list(simu$município),mean) #igual acima com $

by(spp,simu[,1],mean) ### ERRO argumento não é numérico nem lógico: retornando NA


#################################
# aula 2.3/3 Comandos de Logica #
#		06 e 07.03.2018     #
#################################

== #iguadade
!= #diferença
a<-c(2,4,6,8,10,12,14,16,18,20)
a>10  #lista de TRUE ou FALSE 

which(a>10) #QUAIS valores são maiores que 10? responde a posição deles
a[which(a>=14)] #assim diz os valores

mol[which(mol$chuva>1650)] #seleciona só as linhas (parcelas) com chuva>1650mm
mol[which(mol$reserva=="A")] #escolheu apenas da reserva A

#add segundo criteri & (sign e) | (signf ou)
mol[which(mol$chuva>1650 & mol$reserva=="A")]
mol[which(mol$chuva>1650 | mol$reserva=="A")]

#usando p excluir uma variável ex: sp. raras
raras<-which(colSums)<=30 #decobre as colunas cuja soma é baixa, logo pouco abundantes
abundantes<-spp[,-raras]

altos<-which(ambi$alt>=1500)
plot(ambi[-altos,"pH"], spp[-altos,7])#Faz grafico ph x sp sem os locais altos

#subset = which, seleciona partes de dataframe
subset(mol, reserva=="A")
subset(mol, chuva>1650)
subset(mol, reserva=="A" & chuva>1650)

salarios<-c(1000, 400, 1200, 3500, 380, 3000, 855, 700, 1500, 500)
ifelse(salarios<1000,"pouco","muito")

#para dados de presença e ausencia
ifelse(spp>=1,1,0)

ifelse(mol[,1:6]>=1,1,0) # transforma abundancia em presença/ausencia
 
rowSums(mol[,1:6]) #quantas sp de moluscos presentes
colSums(mol[,1:6]) # quantas parcelas cada sp estava presente

#################################
# aula 2.3/3 Comandos de Logica #
#	06 e 07.03.2018		  #
#################################

== #iguadade
!= #diferença
a<-c(2,4,6,8,10,12,14,16,18,20)
a>10  #lista de TRUE ou FALSE 

which(a>10) #QUAIS valores são maiores que 10? responde a posição deles
a[which(a>=14)] #assim diz os valores

mol[which(mol$chuva>1650)] #seleciona só as linhas (parcelas) com chuva>1650mm
mol[which(mol$reserva=="A")] #escolheu apenas da reserva A

#add segundo criteri & (sign e) | (signf ou)
mol[which(mol$chuva>1650 & mol$reserva=="A")]
mol[which(mol$chuva>1650 | mol$reserva=="A")]

#usando p excluir uma variável ex: sp. raras
raras<-which(colSums)<=30 #decobre as colunas cuja soma é baixa, logo pouco abundantes
abundantes<-spp[,-raras]

altos<-which(ambi$alt>=1500)
plot(ambi[-altos,"pH"], spp[-altos,7])#Faz grafico ph x sp sem os locais altos

#subset = which, seleciona partes de dataframe
subset(mol, reserva=="A")
subset(mol, chuva>1650)
subset(mol, reserva=="A" & chuva>1650)

salarios<-c(1000, 400, 1200, 3500, 380, 3000, 855, 700, 1500, 500)
ifelse(salarios<1000,"pouco","muito")

#para dados de presença e ausencia
ifelse(spp>=1,1,0)

ifelse(mol[,1:6]>=1,1,0) # transforma abundancia em presença/ausencia
 
rowSums(mol[,1:6]) #quantas sp de moluscos presentes
colSums(mol[,1:6]) # quantas parcelas cada sp estava presente

#################################################
# aula 3 Manipulando dados e formatos de tabela #
#	07.03.2018	                        					#
#################################################

str(longa)
head(longa) # mosra 6 primeiras linhas

table(longa$parcela, longa$especie) #gera tabela de sp x locais = tabela de contigência

amb2<-read.table(file.choose(), header=T)

cbind(longa, amb2)# tinham tamanhos diferentes -> copia conteudo da menor 
# ate ficar do tamanho da maior para serem combinadas(como oq fiz com dados q renata queria)

longa.amb<-merge(longa,amb2)
str(longa.amb) # da p ver aqui que N diminuiu pois dados sem par foram excluidos

longa.amb.all<-merge(longa, amb2, all.x=T)
str(longa.amb.all)

########################
# Aula 4 e 5 - Funções #
#	08 e 09.03.2018  #
########################

minha.função<-function(lista de argumentos) {corpo da função}

jogar.dado<-function(faces,n.vezes){
sorteio<-sample(faces,n.vezes, replace=T)
return(sorteio)
}

jogar.dado(10,5) # jogei dado de 10 lados 5x

rolar.d<-function(faces,n.vezes){
sorteio<-sample(faces,n.vezes, replace=T)
tabela<-table(sorteio)		#quantas vezes fiz cada rolagem
return(tabela)
}

rolar.d(10,100)


for(i in 1:n) {comandos} #calcula os comandos para os valores i de 1 ate n
#podemos trocar essa letra i por qualquer outra

resu<-numeric() # numeric(0) = numeric() ambos criam uma matriz numerica vazia
resu
for(i in 1:5){ resu[i]<-i^2 }
resu

plot(0:10, 0:10, type="n")
for (i in 1:9) { text(i,i, paste("Passo",i)) } #foi colando texto nas coordenas i,i

plot(0:10, 0:10, type="n")
for(i in 1:9) {
text(i,i, paste("Passo",i))
Sys.sleep(1)	#retarda os passos em 1 seg
}

Fibonacci<-numeric()
Fibonacci[c(1,2)]<-1 #pois o primeiro e segundo valores devem ser 1 e 2
for (i in 3:12) {
Fibonacci[i]<-Fibonacci[i-2]+Fibonacci[i-1]}
Fibonacci

#codigo pra megasena
njogos<-20 		#quantos jogos quero produzir
numeros<-matrix(NA,6,njogos) #cria matrix que irá receber os jogos
for (i in 1:njogos) {
numeros[,i]<-sample(1:60,6)  }
numeros

#função da megasena
megasena<-function(njogos){
numeros<-matrix(NA,6,njogos)	#matriz(vazia, quantia numeros apostados, repetições)
for(i in 1:njogos){ numeros[,i]<-sample(1:60,6)  }
return(numeros)
}
megasena(100)

# Índice de diversidade de Shannom

dados<-c(20,43,21,14,34,54)
prop<-dados/sum(dados)
prop

shannon1<-function(dados){
prop<-dados/sum(dados)
resu<-numeric(0)
n<-length(prop)
for(i in 1:n) {
resu[i]<-if(prop[i]>0){prop[i]*log(prop[i])}
else{0}  
}
H<- -sum(resu)
return(H)
}

simu<-read.table(file.choose(),header=T)
ambi<-simu[,1:6]
spp<-simu[,7:26]

#calculo de indice de shannon para varios locais
shannon<-function(dados){
nl<-nrow(dados)	#numero de locais (linhas)
nc<-ncol(dados)
H<-numeric()	#variavel vazia que receberá os valores H de cada local
	for(i in 1:nl) {
	prop<-dados[i,]/sum(dados[i,])
	resu<-numeric()
		for(k in 1:nc){
		resu[k]<-if(prop[1,k]>0){prop[1,k]*log(prop[1,k])}
		else{0}
		}
	H[i]<--sum(resu)
	}
return(H)	}

shan<-shannon(spp)

library(vegan)
shan.vegan<-diversity(spp,"shannon",MARGIN=1)
cbind(shan,shan.vegan)
shan==shan.vegan

simpson<-function(dados){
nl<-nrow(dados)	#numero de locais (linhas)
nc<-ncol(dados)
D<-numeric()
for(i in 1:nl) {
	prop<-dados[i,]/sum(dados[i,])
	D[i]<-1-sum((prop)^2)
	}
return(D)	}

simp<-simpson(spp)
simp.vegan<-diversity(spp,"simpson",MARGIN=1) #MARGIN calculo por 1 linha (riachos) ou 2 colunas (especies)
cbind(simp,simp.vegan)
simp==simp.vegan

#########################WWWW índices usando só código, sem função

simpson.spp<-1-sum((spp/sum(spp))^2) #1 resultado só, deveria somar sp e diferenciar riachos?
simpson.spp

prop.s<-spp/sum(spp)
shannon.spp<- -sum(prop.s*log(prop.s))
shannon.spp

### Calcular Matrizes de Similaridade

jaccard<-function(dados.spp){
if(any(dados.spp>1))	#criar mensagem de erro caso dados nãe sejam presença-ausencia
stop("Erro: é preciso fornecer uma tabela com dados de presença e ausência")
n<-nrow(dados.spp)	#numero de locais
jac<-matrix(NA,n,n)	#matrix q recebera valores de similaridade
colnames(jac)<-rownames(dados.spp)	#dar o nome dos locais
rownames(jac)<-rownames(dados.spp)	#dar o nome dos locais
	for(i in 1:n) {
		for(j in 1:n){
		a<-sum(dados.spp[i,]==1 & dados.spp[j,]==1) #numero sp presentes ambos locais, i e j
		b<-sum(dados.spp[i,]==1 & dados.spp[j,]==0) #numero sp presentes apenas no local i
		c<-sum(dados.spp[j,]==1 & dados.spp[i,]==0) #numero sp presentes apenas local j
		jac[i,j]<- a /(a+b+c)	#Formula de Jaccard
		}
	}
return (as.dist(jac))	}

pres.aus<-spp #copia p alterar
pres.aus[pres.aus>0]<-1 # tudo maior q zero substitui por 1, vira dado de presença/ausencia

jac<-jaccard(pres.aus)
jac
jac.vegan<- 1-vegdist(pres.aus,"jaccard") 
jac.vegan	# Jaccard no vegan nao calcula similaridade e sim distâncias (dissimilaridade), por isso uso 1-
cbind(jac,jac.vegan) #coloca valores lado a lado p comparar se esta correto (foram=)


bray<-function(dados.spp){
n<-nrow(dados.spp)
BrayCurtis<-matrix(NA,n,n)
	for(i in 1:n){
		for(j in 1:n){
		numerador<-sum(abs(dados.spp[i,]-dados.spp[j,])) #somatorio da abundancia da especie no local 1 menos abundancia da especie no local2
		denominador<-sum(dados.spp[i,]+dados.spp[j,]) #somatorio da abundancia da especie no local 1 mais abundancia da especie no local2
		BrayCurtis[i,j]<-numerador / denominador
		}
	}
return(as.dist(BrayCurtis))	}

bra<-bray(spp)
bra.vegan<-vegdist(spp,"bray")
cbind(bra,bra.vegan)
bra==bra.vegan	#usa logica p ver se são iguais

hellinger<-function(dados.spp){
n<-nrow(dados.spp)
Hellinger<-matrix(NA,n,n)
	for(i in 1:n){
		for(j in 1:n){
		y1<-sqrt(dados.spp[i,]/sum(dados.spp[i,]))
		y2<-sqrt(dados.spp[j,]/sum(dados.spp[j,]))
		Hellinger[i,j]<-sqrt(sum((y1-y2)^2))
		}
	}
return(as.dist(Hellinger))	}

hel<-hellinger(spp)
hel.vegan<-vegdist(decostand(spp,"hellinger"),"euclid")
cbind(hel,hel.vegan)
hel==hel.vegan		###ok... foi cheio de trues e falses tbm...

####################################
# Desafio do jogo do Silvio Santos #
# Porta da esperança/desesperado   #
####################################

silvio.g 
portas <- sample(1:3,3)
premio <-sample(1:3,1)
escolha1 <- sample(1:3,1) #escolha qual esta o premio
monstros <- setdiff(portas,premio)# silvio escolhe uma com monstro
silvio.a<-setdiff(monstros,escolha1) # abre essa sem premio
portas2 <- setdiff(portas,silvio.a) # exclui porta aberta
escolha2<- setdiff(portas2,escolha1) #mudar? estabelece no programa que a pessoa muda de porta
ifelse(escolha2==premio,"parabéns", "você perdeu")),1000) # resultado ganhou ou perdeu

#repetir 1000 vezes -> ver no resultado final 33212a proporção que ela ganhou
paradox<-table(rep(silvio.g,1000)

paradoxo de montyhall ->melhor mudar

porta.desesper<-function(escolha1,troca){
portas <- sample(1:3,3)
premio <-sample(1:3,1)
monstros <- setdiff(portas,premio)# silvio escolhe uma com monstro
silvio.a<-setdiff(monstros,escolha1) # abre essa sem premio e diferente da escolhida
portas2 <- setdiff(portas,silvio.a) # exclui porta aberta
troca <- c("n","s")
escolha2<- setdiff(portas2,escolha1)
final <-ifelse(troca=="n", escolha1, escolha2)
resultado<-ifelse(final==premio, "Parabéns!","Corre! Olha o monstro!")
return(resultado)
}

porta.desesper(1,s) #


= é o mesmo que <-
return("Silviu escolheu porta",silvio.a, "Você quer trocar", troca)




#######################versão do professor
Po<-1:3
Pr<-sample(Po,1)
Es<-sample(Po,1)
Sobra<-Po[-c(Pr,Es)]
Abre<-sample(Sobra,1)
Po2<-Po[-Abre]
Nov<-Po2[-Es]
R[s]<-as.numeric(No==Pr) #R[S]<- estao em vermelho, são do codigo abaixo, q faz repetições


R<-numeric(1000)
tr<-numeric(1000)
for(s in 1:1000) {		#add esse inicio
Po<-1:3
Pr<-sample(Po,1)
Es<-sample(Po,1)
Sobra<-Po[-c(Pr,Es)]
Abre<-sample(Sobra,1)
Po2<-Po[-Abre] 	#ate aqui igual
troca<-sample(c(0,1),1)
tr[s]<-troca
Nov<-ifelse(troca==1,Po2[-Es],Es)
Nov<-Po2[-Es]
R[s]<-as.numeric(No==Pr)
}


