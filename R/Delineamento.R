########################################################
#   Delineamento amostral - Profa. Lucia
#	14.III.2018
#  Aula4: Amostragens, Teorema do limite central, Erro-Padrao,
#  Aula5: Testar probabilidade de encontrar diferença= ao acaso (P->H0=H1)
#	15.III.2018
#  Aula6: Função p amostragem ser feita +vezes, Teste-T,
#  Aula7: 
#


########## amostragem de comprimento de folha Ixora sp.

ixora <-read.table(file.choose(),header=T)
folhas<-ixora[,2:31]
descrit<-summary(folhas)

head(folhas)
media<-apply(folhas,2,mean) #medias de cada amostra
media
hist(media) # normal dele é fazer com frequencia (prob=F)
hist(media, prob=T) # mas se colocar prob=T faz usando probabilidades
curve(dnorm(x,mean=mean(media),sd=sd(media)),col=2,add=T) #ajusta curva normal com media e sd

mean(media) #media das medias
sd(media) # desvio padrão das medias (erro padrao)

dp<-apply(folhas,2,sd) #o 2 indica q estimativa é por coluna
erropad<-dp/sqrt(nrow(folhas))
erropad
sort(erropad) #ordena
hist(erropad)
abline(v=0.33628, col=2, lwd=4) #add linha em v =valor do erro, col=cor, lwd=expessura


#############folha aleatorizadas
min(folhas)
max(folhas)
folha.ale=(matrix(runif(150,0.31,8.39),nrow=5, ncol=30))
folha.ale
hist(folha.ale,prob=T)
media<-apply(folha.ale,2,mean)
media
hist(media, prob=T)
curve(dnorm(x,mean=mean(media),sd=sd(media)),col=2,add=T)
mean(media) 
sd(media)
dp<-apply(folha.ale,2,sd) 
dp
erropad<-dp/sqrt(nrow(folhas))
erropad
sort(erropad) #ordena
hist(erropad)
abline(v=0.9480642, col=2, lwd=4) #v=valor do erropad
median(erropad)
abline(v=1.072114, col=3, lwd=4) #v=valor da mediana


################################### Testando hipóteses
mulheres<-c(1,2,3,4,5,6,7,8,9,10)
homens<-c(11,12,13,14,15,16,17,18)
sample(1:10,4) #sorteou numero de chamada para selecionar a amostra


sexo<-c("H","H","H","H","M","M","M","M")
alt<-c(168,180,179,180,160,173,164,170) #dados da amostragem
H0<-("H","H","H","M","H","M","M","M") #sorteia q altura sera homem ou mulher)

stripchart(alt~sexo,v=T,pch=1, cex=1.5,method="stack")

h2.h<-c(168,180,180,173) #sorteamos a altura usando cara e coroa(H0) p estar hipotese nula
h2.m<-c(179,160,164,170)
mean(h2.h)
mean(h2.m)
dif<-(mean(h2.h)) - (mean(h2.m))
dif

h4.h<-c(179,173,164,170)
h4.m<-c(168,180,180,160)
mean(h4.h)
mean(h4.m)
dif<-(mean(h4.h)) - (mean(h4.m))
dif

h6.h<-c(180,160,164,170)
h6.m<-c(168,179,180,173)
mean(h6.h)
mean(h6.m)
(mean(h6.h)) - (mean(h6.m))

h8.h<-c(179,173,164,170)
h8.m<-c(168,180,180,160)
mean(h8.h)
mean(h8.m)
(mean(h8.h)) - (mean(h8.m))

h10.h<-c(180,160,173,170)
h10.m<-c(168,179,180,164)
mean(h10.h)
mean(h10.m)
(mean(h10.h)) - (mean(h10.m))

h12.h<-c(168,179,180,173)
h12.m<-c(180,160,164,170)
mean(h12.h)
mean(h12.m)
(mean(h12.h)) - (mean(h12.m))

diff<-c(7,1,0.5,7.5,6.5,0.5,0.5,7,2,1,6.5,8,10,7.5,3.5,10,4,0,6.5)
hist(diff)

##################### Função para amostrar sorteando acaso altura p cada gênero
H<-c(168,180,179,180)
M<-c(160,173,164,170) #amostragem de ontem

#Criando a rotina para teste

dmedia<-function(a,b,R) {
	C<-c(a,b)
	dmo<-mean(a)-mean(b)
	dm<-numeric(R)
	for(i in 1:R) {
		Cr<-sample(C)
		ar<-Cr[c(1:length(a))]
		br<-Cr[-c(1:length(a))]
		dm[i]<-mean(ar)-mean(br) }
	return(dm) }

#Executando o teste
mean(H)-mean(M)

Dif<-dmedia(a=H,b=M,R=1000)
hist(Dif)
hist(c(10,Dif),main="diferença das medias",prob=T) #add as barras do histograma
curve(dnorm(x,mean=mean(Dif), sd=sd(Dif)),col=2,add=T) #add curva normal
abline(v=10,col=2,lwd=4)	#add linhas linhas q darão base p comparar
abline(v=-10,col=2,lwd=4)

absDif<-abs(Dif)
Next<-length(absDif[absDif>=10]) #quantos resultados tiveram diferença >= 10
Next

p.value<- Next/1000 #Qual a probabilidade de obter um valor tão extremo?
p.value

################### Test t
H<-c(168,180,179,180)
M<-c(160,173,164,170) #amostragem de ontem
alt<-c(H,M)
alt
sexo<-c("H","H","H","H","M","M","M","M")
stripchart((alt)~sexo,v=T,pch=1, cex=2,method="stack")
points(y=166.75,x=2,col=2,pch=3) #plotar media das mulheres
points(y=176.75,x=2,col=3,pch=3) #plotar media dos homens

tapply(alt,sexo,var) # variancia de cada genero
var(M)/var(H) #ver se variancias são homogêneas, ou heterogêneas(razão>1)

t.test(H,M,var.equal=T)		#var.equal=T variâncias são homogêneas
t.test(alt~sexo,var.equal=T)		# 2 maneiras de fazer o mesmo teste

qt(0.975,6) #qt= t critico,primeiro valor=nivel de confiança, segundao= graus de liberdade n-2
		#usa 0.975 p sobrar 0.025 em cada lado, total=5%= area de rejeição
curve(dt(x,df=6),col=4,xlim=c(-4,+4))
abline(v=+2.447,col=3,lwd=1) #v= valor do qt
abline(v=-2.447,col=3,lwd=1)	#v = lugar onde posiconaremos linhas q marcam area de rejeição
points(2.416,0,col=2,pch=19) #add valor de t

############################## Teste T com as folhas de ontem
ixora <-read.table(file.choose(),header=T)
folhas<-ixora[,2:31]
folhas

t.test(folhas$A1,folhas$A2,var.equal=T) #t = -2.2086, df = 8, p-value = 0.05821
t.test(folhas$A2,folhas$A3,var.equal=T) #t = 1.1119, df = 8, p-value = 0.2985
t.test(folhas$A3,folhas$A4,var.equal=T) #t = 3.9337, df = 8, p-value = 0.004334  x
t.test(folhas$A4,folhas$A5,var.equal=T) #t = -0.48038, df = 8, p-value = 0.6438
t.test(folhas$A5,folhas$A6,var.equal=T) #t = 0.9794, df = 8, p-value = 0.3561
t.test(folhas$A6,folhas$A7,var.equal=T) #t = -0.77511, df = 8, p-value = 0.4606
t.test(folhas$A7,folhas$A8,var.equal=T) #t = 0.74745, df = 8, p-value = 0.4762
t.test(folhas$A8,folhas$A9,var.equal=T) #t = -0.12427, df = 8, p-value = 0.9042
t.test(folhas$A9,folhas$A10,var.equal=T) #t = 0.14178, df = 8, p-value = 0.8908
t.test(folhas$A10,folhas$A11,var.equal=T) #t = 0.12524, df = 8, p-value = 0.9034
t.test(folhas$A11,folhas$A12,var.equal=T) #t = -1.3254, df = 8, p-value = 0.2216
t.test(folhas$A12,folhas$A13,var.equal=T) #t = 0.13363, df = 8, p-value = 0.897
t.test(folhas$A13,folhas$A14,var.equal=T) #t = 0.9246, df = 8, p-value = 0.3822
t.test(folhas$A14,folhas$A15,var.equal=T) #t = 0, df = 8, p-value = 1
t.test(folhas$A15,folhas$A16,var.equal=T) #t = -0.32397, df = 8, p-value = 0.7543
t.test(folhas$A16,folhas$A17,var.equal=T) #t = 1.1404, df = 8, p-value = 0.2871
t.test(folhas$A17,folhas$A18,var.equal=T) #t = -1.1746, df = 8, p-value = 0.2739
t.test(folhas$A18,folhas$A19,var.equal=T) #t = 0.17747, df = 8, p-value = 0.8635
t.test(folhas$A19,folhas$A20,var.equal=T) #t = 0.72643, df = 8, p-value = 0.4883
t.test(folhas$A20,folhas$A21,var.equal=T) #t = 0.71005, df = 8, p-value = 0.4978
t.test(folhas$A21,folhas$A22,var.equal=T) #t = -1.6845, df = 8, p-value = 0.1306
t.test(folhas$A22,folhas$A23,var.equal=T) #t = 0.72932, df = 8, p-value = 0.4866
t.test(folhas$A23,folhas$A24,var.equal=T) #t = 0.22609, df = 8, p-value = 0.8268
t.test(folhas$A24,folhas$A25,var.equal=T) #t = 0.86173, df = 8, p-value = 0.4139
t.test(folhas$A25,folhas$A26,var.equal=T) #t = -2.3255, df = 8, p-value = 0.0485  x
t.test(folhas$A26,folhas$A27,var.equal=T) #t = 2.1, df = 8, p-value = 0.06894
t.test(folhas$A27,folhas$A28,var.equal=T) #t = 1.4527, df = 8, p-value = 0.1844
t.test(folhas$A28,folhas$A29,var.equal=T) #t = -1.9566, df = 8, p-value = 0.08611
t.test(folhas$A29,folhas$A30,var.equal=T) #t = -0.80033, df = 8, p-value = 0.4466
t.test(folhas$A30,folhas$A1,var.equal=T) #t = -2.0608, df = 8, p-value = 0.07326

#maioria de nós rejeitou a H0 qnd ela era verdadeira
30*0.05 #quantas vezes podíamos errar (hipoteticamente)
2/30 #qual a frequencia que realmente errei (os outros foram maior ainda)
#Correção de Bonferroni (a significancia usada é 0.05/quant de comparações 2 a 2)


############################## Teste T com as folhas aleatorizadas
min(folhas)
max(folhas)
folhaa=(matrix(runif(150,0.31,8.39),nrow=5, ncol=30))
folhaa


t.test(folhaa$A1,folhas$A2,var.equal=T) #
t.test(folhas$A2,folhas$A3,var.equal=T) #
t.test(folhas$A3,folhas$A4,var.equal=T) #
t.test(folhas$A4,folhas$A5,var.equal=T) #
t.test(folhas$A5,folhas$A6,var.equal=T) #
t.test(folhas$A6,folhas$A7,var.equal=T) #
t.test(folhas$A7,folhas$A8,var.equal=T) #
t.test(folhas$A8,folhas$A9,var.equal=T) #
t.test(folhas$A9,folhas$A10,var.equal=T) #
t.test(folhas$A10,folhas$A11,var.equal=T) #
t.test(folhas$A11,folhas$A12,var.equal=T) #
t.test(folhas$A12,folhas$A13,var.equal=T) #
t.test(folhas$A13,folhas$A14,var.equal=T) #
t.test(folhas$A14,folhas$A15,var.equal=T) #
t.test(folhas$A15,folhas$A16,var.equal=T) #
t.test(folhas$A16,folhas$A17,var.equal=T) #
t.test(folhas$A17,folhas$A18,var.equal=T) #
t.test(folhas$A18,folhas$A19,var.equal=T) #
t.test(folhas$A19,folhas$A20,var.equal=T) #
t.test(folhas$A20,folhas$A21,var.equal=T) #
t.test(folhas$A21,folhas$A22,var.equal=T) #
t.test(folhas$A22,folhas$A23,var.equal=T) #
t.test(folhas$A23,folhas$A24,var.equal=T) #
t.test(folhas$A24,folhas$A25,var.equal=T) #
t.test(folhas$A25,folhas$A26,var.equal=T) #
t.test(folhas$A26,folhas$A27,var.equal=T) #
t.test(folhas$A27,folhas$A28,var.equal=T) #
t.test(folhas$A28,folhas$A29,var.equal=T) #
t.test(folhas$A29,folhas$A30,var.equal=T) #
t.test(folhas$A30,folhas$A1,var.equal=T) #


########################################
#  Delinemamento Amostral Profa Lucia
#	16.III.2018 - #	ANOVA, Regressão Linear Simples
#	19.III.2018 - #	Transformações, Regressão Não linear, Regressão Multipla
#

veg<-c("cerrado","cerrado","cerrado","cerrado","cerrado","cerrado","mataciliar","mataciliar","mataciliar","mataciliar","mataseca","mataseca","mataseca","mataseca","mataseca","mataseca")
alt<-c(10.2,8.4,5.7,12.3,14,11.2,6.5,9.6,8,13.3,14.4,13,12.3,17.2,16,16)

as.data.frame(veg)

plot(alt~factor(veg),pch=19,cex.axis=0.8)
(medias<-tapply(alt,veg,mean))
points(y=medias,x=1:3, col=2,pch=8,cex=1)

stripchart(alt~factor(veg),pch=19,cex.axis=0.8,v=2)
(medias<-tapply(alt,veg,mean))
points(y=medias,x=1:3, col=2,pch=8,cex=1)

#2 modelos p fazer ANOVA:
lmplant<-lm(alt~factor(veg),data=)
anova(lmplant)

#ou
aovplant<-aov(alt~factor(veg),data=)
summary(aovplant)

names(aovplant) #nome de todos os dados disponíveis que podem ser chamados
aovplant$df.residual

#avaliar os residuos para buscar tendencias (q devem ser evitadas)
plot(aovplant$residuals~aovplant$fitted.values, pch=19,cex.axis=0.8)
abline(h=0,col=2)
 #ex:problema de homogeneidade de variancia: nunvem d pontos em forma de corneta=varianção aumentando com o tamanho 
#solução:dados devem ser transformados


hist(aovplant$residuals)
shapiro.test(aovplant$residuals) #normalidade dos residuos
require(lawstat)
levene.test(aovplant$ #teste de homogeneidade de variância

curve(df(x,13,2,log=F),xlim=c(0,8))		#gera o grafico assimetrico
qf(0.95,2,13)		#0.95=valor de confiança, 2=graus do fator (3-1), 13=observações - niveis do fator(16-3)
abline(v=3.806,col=3,lwd=3)		#v= valor de qf, plota F crítico
points(y=0.00,x=6.87,pch=19,col=4	 #x= valor q achamos de F

############################ Analise de Regressão Linear Simples
altpe<-read.table(file.choose(),header=T)
reg<-altpe

plot(altpe$pe~altpe$alt)

model1<-lm(reg$pe~reg$alt)
summary(model1)
#residuals= resumo de estatisticas dos residuos
#(Intercept)xEstimate= a (origem da reta, y=a+bx) t= se "a" é ou nao diferente de zero
#reg$altxEstimate = inclinação da reta (b)
#Multiple R-squared= nosso R2
# F-statistic:50.42 on 1(grau no fator 2-1) and 18(graus de liberdade das obervações20-2), p-value(da regressão)
plot(reg$pe~reg$alt)
abline(model1,col=2,lwd=3) #add linha da regressão

anova(model1)
# aparece os mesmos graus d liberdade do fator e das observações(residuos)
# e o F e o p tbm são iguais em ambos os testes, pois tem a mesma base estatistica

#Analise de Residuos
resul<-cbind(reg$pe,model1$fitted.values,model1$residuals)
colnames(resul)=c("obs","esp","residuo")
resul
mean(reg$pe)
plot(model1$residuals~model1$fitted.values)
abline(h=0, col=2)	#procurar tendencias
hist(model1$residuals)	#observar normalidade


########################### Regressão precisando de Transformação
bid<-read.table(file.choose(),header=T)
bid
plot(bid$bio~bid$dap)

model1<-lm(bid$bio~bid$dap)
summary(model1)		#há relação, altamente significatica, da pra predizer bio em função de dap
# modelo: R2=0.62 bio=-2247+445*dap -> mas esse não é o melhor modelo para predizer.

## Analise dos Resíduos:
resul<-cbind(bid$bio,model1$fitted.values,model1$residuals)
colnames(resul)=c("obs","esp","residuo")
resul
mean(bid$bio)
plot(model1$residuals~model1$fitted.values)
abline(h=0, col=2)	# há tendencia= violou algum dos pressupostos: relação entre variável preditora e resposta não é linear ou variância não homogênea

#Procurar modelos para ver qual se parece mais com a distribuição dos dados

#Transformar para atender presuposto do método y=a*x^b => logy=loga+b*logx

plot(log(bid$bio)~log(bid$dap)) #transformamos dados em log p tornar relação linear
logmodel1<-lm(log(bid$bio)~log(bid$dap))
summary(logmodel1)
exp(0.126) #antilog de a	(logy=loga+b*logx)

#Comparando Modelos:
plot(bid$bio~bid$dap)
abline(model1,col=2,lwd=3)
lines(bid$dap,(1.135*(bid$dap^2.936)),col=3,lwd=3)
est<-1.135*(bid$dap^2.936)	#biomassa estimada
est
cor(est,bid$bio) #correlação entre biomassa estimada e observada
(cor(est,bid$bio))^2#coeficiente de determinação é r^2 ->maior? :D 
#quanto maior, mais ele se ajusta a seus dados


###################### Regressão Não Linear

st<-list(a=1,b=2) #st -> essa lista vai ser chamada(start) na função abaixo
non<-nls(bio~a*(dap^b),data=bid,start=st,trace=T) #nls = função do R para fazer função não linear, tenta fazer a convergência
#trace=T mostra as interações
summary(non)

plot(bid$bio~bid$dap)
lines(bid$dap, fitted.values(non),lwd=2,col="blue")
predict(non)

cor(bid$bio,predict(non))^2 #podes escolher pelo mair R^2 ou pela linha q + encaixa-se

#comparando todos os Modelos:
plot(bid$bio~bid$dap)
abline(model1,col=2,lwd=3) #linear sem transformação
lines(bid$dap,(1.135*(bid$dap^2.936)),col=3,lwd=3) #linear com transformação, ajusta-se melhor aos valores menores
lines(bid$dap, fitted.values(non),lwd=2,col=4) #não linear, melhor com valores maiores


############## Prova 10
afo<-c(2,3,4,2,20)
her<-c(3,2,1,4,40)
plot(her~afo)

model1<-lm(her~afo)
summary(model1)
resul<-cbind(her,model1$fitted.values,model1$residuals)
colnames(resul)=c("obs","esp","residuo")
resul
mean(her)
plot(model1$residuals~model1$fitted.values)
abline(h=0, col=2)	#dados muito diferentes (outlier tem grande influencia nos dados)
hist(model1$residuals)	#não normais

#tentar não linear
st<-list(a=1,b=2) 
non<-nls(her~a*(afo^b),start=st,trace=T) #nls = função do R para fazer função não linear, tenta fazer a convergência
#trace=T mostra as interações
summary(non)
plot(her~afo)
lines(afo, fitted.values(non),lwd=2,col="blue")
predict(non)
cor(her,predict(non))^2 #podes escolher pelo mair R^2 ou pela linha q + encaixa-se

# Outilier: erro na coleta? ou erro de delinemento? (falta de coleta dos valores intermediarios)
par(mfrow=c(1,2))
afo2<-c(2,3,4,2,2) #muitas vezes é erro de digitação, nesse caso é só corrigir
her2<-c(3,2,1,4,4) #se não for tens q explicar muito bem a razão dele e decidir se usa ou não
plot(her2~afo2)


########################### Analise de Regressão Múltipla
area<-c(1,2,3,4,5,6)
mac<-c(1,3,2,5,5,7)
arv<-c(2,1,4,3,6,5)

plot(mac~arv) #tentativa com regressão simples
model1<-lm(mac~arv)
summary(model1) #sem relação 
abline(model1,col=2,lwd=3)
plot(mac~area)
model2<-lm(mac~area)
summary(model2) #relação forte mas não é a pergunta q queriamos
abline(model2,col=2,lwd=3)

#usando 2 fatores (previsoras) pra 1 resposta ### Passo a passo:

##Tirando efeito da area
reg1<-lm(mac~area) #regressao macaco/area
dreg1<-resid(reg1) #salva os resíduos gerados (se usar summary vc vê todas info geradas)
dreg1
r1<-lm(arv~area) #regressao arvore/area
d1<-resid(r1)
d1

##Fazendo regressão com os desvios e olhando os coenficientes b:
macarv<-lm(dreg1~d1) #desvios de macacos e árvores sem efeito de área
summary(macarv)

##Plotando grafico de regressão parcial:
par(mfrow=c(1,2))
plot(dreg1~d1,pch=19,main="Efeito de arvores sem da area") #desvios de macacos e árvores sem efeito de área
abline(macarv)

##Tirando efeito de arvore:
reg2<-lm(mac~arv)	#regressao macaco/arvore
dreg2<-resid(reg2)
dreg2
r2<-lm(area~arv) #regressao area/arvore
d2<-resid(r2)
d2
macarea<-lm(dreg2~d2) #Regressão com os desvios de macacos e árvores sem efeito de área
summary(macarea)
plot(dreg2~d2,pch=19,main="Efeito da area sem da arvore") #Plotando parcial dos desvios de macacos e árvores sem efeito de área
abline(macarea)

####### Versão direta
###e onde adquirimos o a(intercept) do grafico, alem dos b q vimos tbm na forma passo a passo
rem<-lm(mac~arv+area)
summary(rem) #estimate: Intercept=a, arv=b1, area=b2, DF=N-k(preditoras)-1= 5-2-1
#p de baixo é do efeito global, se ele mostrar q há efeito->vc olha o p de cada variável para ver qual tem efeito

anova(rem) #para ver a tabela da analise de variância

plot(rem$resid~rem$fitted.values)	#olhar efeito dos resíduos
abline(h=0,col=2)

install.package(car)
library(car)
avPlots(rem) #graficos da regressão multipla são os gráficos das parciais


##########################################
# Delineamento Amostral
# 20.III.2018	ANOVA Fatorial, ANCOVA, Analise dos Caminhos
# 21.III.2018	Multivariada
#
 
############## Anova com 2 fatores

cpsh<-read.table(file.choose(),header=T) 

analises<-aov(cpsh$cp~cpsh$sexo*cpsh$hab)		# se tiver interação * 
	#se tiveres certeza q nao tem interação usa +
analises<-aov(cpsh$cp~cpsh$sexo+cpsh$hab)
summary(analises)		#F= meanSq de um fator/meanSq do residuo

plot(analises$resid~analises$fitted.values) #analisas residuos
abline(h=0,col=3)

plot(cp~sexo,data=cpsh)
plot(jitter(cp)~h,data=cpsh,col=sexo) #separa sexo por cor e por h=habitat

analise2<-aov(cpsh$cp2~cpsh$sexo*cpsh$hab)
summary(analise2)		#se há interação, nao olhe mais as variáveis independentemente, o que importa é explicar a interação


############## Anova 3 fatores

y<-rnorm(40,50,4)
FA<-rep(gl(2,5),times=4)	#gl=general levels cria:2 niveis,5 repetições,4x
FB<-rep(gl(2,10),times=2)
FC<-rep(gl(2,20),times=1)	#criou dados

anova3<-lm(y~FA*FB*FC)
anova(anova3)

############### ANCOVA
alpe<-read.table(file.choose(),header=T) 
head(alpe)
plot(alpe$pe~alpe$alt,pch=19,col=alpe$sexo,xlim=c(155,185),ylim=c(20,28))


ancov<-aov(alpe$pe~alpe$alt*alpe$sexo)
summary(ancov)
	#b são iguais pois não houve interação sig entre as variáveis, só nao sabemos valor (ainda)
	#se o sexo não teve efeito sobre o resultado (nao sig) então o sexo não gerou diferenças nas retas logo "a" são iguais

f<-subset(alpe,sexo=="F")
m<-subset(alpe,sexo=="H")
rf<-(lm(f$pe~f$alt))
rm<-(lm(m$pe~m$alt))
summary(rf)		#encontra a e b de cada reta
summary(rm)		#apesar dos numeros serem diferentes eles são estatisticamente iguais

plot(alpe$pe~alpe$alt,pch=19,col=alpe$sexo,xlim=c(0,185),ylim=c(0,28))
abline(rf,col=2)
abline(rm,col=1)	#visualiza as retas (diferentes mas estatisticamente equivalentes

plot(ancov$resid~ancov$fitted.values)
abline(h=0,col=3)		#avaliar se resíduo esta normal
shapiro.test(ancov$resid) #teste de normalidade (mas a analise grafica já resolve)


#################### Analise dos Caminhos

lagost<-read.table(file.choose(),header=T)
pol<-lagost$pol/sd(lagost$pol) 	#Padronização dos dados
peixe<-lagost$peixe/sd(lagost$peixe)
fito<-lagost$fito/sd(lagost$fito)
lago<-lagost$lagost/sd(lagost$lagost)

lagost.t<-data.frame(pol,peixe,fito,lago)
lagost.t
regmult.t<-lm(lagost.t$lago~lagost.t$pol+lagost.t$peixe+lagost.t$fito)
summary(regmult.t)	#Observar efeitos diretos (b Estimate)

##Estimando efeitos indiretos
reg1<-lm(lagost.t$peixe~lagost.t$pol)
summary(reg1)
reg2<-lm(lagost.t$fito~lagost.t$pol)
summary(reg2)

## Efeito total da poluição=Blago~pol+(Blago~peixe*Bpol~peixe)+(Blago~fito*Bpol~fito)
Ef.pol=(-0.156)+(0.569*0.544)+((-0.657)*(-0.394))
Ef.pol

###############################
# Delineamento Amostral		#
# 21.III.2018 Multivariada	#
###############################

forma<-read.table(file.choose(),header=T)

#PCA
require(vegan)
medidas<-forma[,3:9]
pairs(medidas) 	#matriz de correlações
pairs(log(medidas))	#matriz de correlações dos logaritimos
cor(medidas) #observar os indices de correlações

pca.med<-rda(medidas,scale=T)
scores<-scores(pca.med)	#sao criados tantoos eixos quanto variaveis, cada um pega uma parte da variação, os primeiros representam a maioir parte da variabilidade
scores  	#valor no eixo, cada site representa o novo endereço da pessoa no eixo, é o valor que resume seus dados
pca.med #autovetor: quanto maior, mais o valor é representado (PCA1 e 2, maiores)
biplot(pca.med) #plota os eixos já com as relações lineares

plot(pca.med,t='n',xlim=c(-2,2),ylim=c(-2,2))
text(pca.med,display=c("sites"),lab=forma$nome) #id(nome) de cada site
points(pca.med,display=c("sites"),col=forma$sexo,pch=19) #pontos com cores diferentes p cada sexo
ordihull(pca.med,group=forma$sexo,show="H") 	#agrupa por alguma variavel categorica
ordihull(pca.med,group=forma$sexo,show="F",col=2)


scu<-scores(pca.med,1)	#salva os scores do eixo 1
scu2<-scores(pca.med,2)
cor(scu$sites,medidas) #correlação do eixo pca com os atributos para descobrir qual atributo explica mais a variabilidade do eixo
cor(scu2$sites,medidas)


########################### Teste de hipotese com eixos da PCA
par(mfrow=c(1,2))
plot(scu$sites~forma$sexo,pch=19)
plot(scu2$sites~forma$sexo,pch=19)

y<-cbind(scu$sites,scu2$sites)
regre<-manova(y~forma$sexo)
summary.aov(regre) #univariate MANOVA, com um P e um F para cada eixo
summary.aov(regre,test="Wilks") #MANOVA pelo teste de lambda de Wilks, o valor wilks mais importante que o F que é só uma aproxiação
summary.aov(regre, test="Pillai")







