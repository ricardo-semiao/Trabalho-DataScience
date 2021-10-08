########### PACOTES E FUNÇÕES   ##########
library(tidyr) # Manipulação de data frame
library(ggplot2) # Gráficos
library(stargazer) # Tabela do latex
library(lmtest) # Testes pro lm
library(car) # Testes pro lm
library(stargazer)


########### DADOS               ##########
#======== Ler e criar dummies ==========
censoT = as.matrix(read.delim("m:/Users/Marcus/Downloads/Censo 2010/Amostra_Pessoas_35_RMSP.txt", header=FALSE))
censo = censoT[-which(substr(censoT, 263, 269)%in%c("       ","0000000"))]

#"SALÁRIO PRINCIPAL"=c(219, 224),"RENDIMENTO TOTAL"=c(263, 269)), "SALÁRIO TOTAL"=C(247, 253)
Y = log(as.numeric(substr(censo, 263, 269)))
n = length(Y)

Dicio = numeric()
for(i in paste0("X",1:4)){
  Dicio = rbind(Dicio,
                as.data.frame(readxl::read_excel("m:/Users/Marcus/Downloads/Censo 2010/Dicionário2.xls",
                                                 sheet=i)[,c(3:5,9)]))}

Vars = as.data.frame(matrix(nrow=length(censo),ncol=nrow(Dicio)))

for(i in 1:nrow(Dicio)){
  col = as.numeric(substr(censo, Dicio[i,2], Dicio[i,3]))
  if(Dicio$DUMMIE[i]=="S"){col=as.factor(col)}
  Vars[,i] = col}
colnames(Vars) = Dicio$`NOME CURTO`

remove(col)

#======== Tratar manualmente  ==========
for(i in 1:ncol(Vars)){
  soma = sum(is.na(Vars[,i]))
  if(soma>=1){
    print(paste(soma, "-", i, colnames(Vars)[i]))}}

#NACIONALIDADE -> 1 (BR)
#$FAM e NFAM -> 0
#IDADE FILHO -> 0 e dummie
#N nascidos e N vivos -> 0
#tempo deslocamento -> 0 e dummie
#N trabalh0s -> 1
#Retornar -> 1 (sim)
#ATIVIDADE e RELIGIÃO com poucos -> 0
#BOLSA, TRANSFERENCIAS e OUTROS 9 -> 0 

#Dummies de NA
Vars$`IDADE FILHO NA` = as.factor(as.numeric(is.na(Vars$`IDADE FILHO`)))
Vars$`TEMPO DESLOCAMENTO NA` = as.factor(as.numeric(is.na(Vars$`TEMPO DESLOCAMENTO`)))

#Mudar valores
na = c("$ FAMILIAR/C", "N FAMÍLIA", "N FILHOS VIVOS", "IDADE FILHO", "TEMPO DESLOCAMENTO")
Vars[,na][is.na(Vars[,na])] = 0

na = c("NACIONALIDADE", "RETORNAR", "N TRABALHOS")
Vars[,na][is.na(Vars[,na])] = 1

na = c("ATIVIDADE", "RELIGIÃO")
Vars[,na] = apply(Vars[,na],2,as.character)
Vars[is.na(Vars[,na]),na] = "0"

#Diminuir quantidade de dummies de atividade e religião
vc = c("ATIVIDADE"=4000,"RELIGIÃO"=4000)
for(j in names(vc)){
  for(i in unique(Vars[,j])){
    index = Vars[,j]==i
    if(sum(index) <= vc[j]){
      Vars[index,j] = "Outros"}}}
Vars$RELIGIÃO = as.factor(Vars$RELIGIÃO)
Vars$ATIVIDADE = as.factor(Vars$ATIVIDADE)

na = c("BOLSA-FAMÍLIA", "TRANSFERÊNCIAS", "OUTROS RENDIMENTOS", "MAE VIVA", "APOSENTADORIA")
Vars[,na][Vars[,na]==9] = 1

na = c("CURSO GRADUAÇÃO", "CURSO MESTRADO", "CURSO DOUTORADO")
Vars[,na] = apply(Vars[,na],2,is.na)
Vars[,na] = data.frame(lapply(Vars[,na], as.factor))

na = c("PAGA INSS", "IMIGRANTE UF")
Vars[,na][is.na(Vars[,na])] = 3

na = c("NASCIDOS VIVOS", "SEXO FILHO", "NASCIDOS MORTOS", "NATUNI")
Vars[,na] = apply(Vars[,na], 2, as.character)
Vars[,na][is.na(Vars[,na])] = 0
Vars[,na] = data.frame(lapply(Vars[,na], as.factor))

#Idade ao quadrado:
Vars$IDADE2 = Vars$IDADE^2
Vars$IDADE3 = Vars$IDADE^3
Vars$IDADE4 = Vars$IDADE^4
Vars$IDADE5 = Vars$IDADE^5

#Dummies de deficiência
for(i in c("VISÃO","AUDIÇÃO","LOCOMOÇÃO")){
  Vars[[paste0(i,2)]] = as.factor(as.numeric(Vars[,i]%in%1:2))
  Vars[,i] = NULL}
Vars$INTELECTO2 = as.factor(ifelse(Vars$INTELECTO==2,0,2))
Vars$INTELECTO = NULL

##Criar leva para as novas variáveis
#names = c("IDADE FILHO NA","TEMPO DESLOCAMENTO NA","IDADE2","VISÃO2","AUDIÇÃO2","LOCOMOÇÃO2","INTELECTO2")
#leva = c("F", "R", "C", "D", "D", "D", "D")
#for(i in 1:length(names)){
#  Dicio = rbind(Dicio, c(names[i], rep(0,3), leva[i]))}

Dicio = Dicio[-which(Dicio[,1]%in%c("VISÃO","AUDIÇÃO","LOCOMOÇÃO","INTELECTO")),]
#Vars = Vars[-which(Vars$RAÇA==9 | Vars$INTELECTO2==9),]

#na = apply(Vars, 1, function(x){any(!is.na(x))})
na = is.na(Vars$`TEMPO MUNICÍPIO`) | is.na(Vars$`TEMPO UF`)
Vars = Vars[!na,]
Y = Y[!na]

na = is.na(Y) | is.infinite(Y)
Vars = Vars[!na,]
Y = Y[!na]

n = nrow(Vars)

for(i in colnames(Vars)){
  if(is.factor(Vars[,i])){
    Vars[,i] = factor(Vars[,i])}}

for(i in colnames(Vars)){
  if(is.factor(Vars[,i])){
    print(i)
    print(table(Vars[,i]))}}

#cols = numeric()
#for(i in colnames(Vars)){
#  if(is.factor(Vars[,i])){
#    if(any(table(Vars[,i])<=1500)){
#      cols = c(cols, i)}}}

Vars$NACIONALIDADE = NULL

remove(na, index, soma, names, leva, vc)

#Vars = Vars[,-c(41:60)]

#save(Vars, file="m:/Users/Marcus/Downloads/Vars.RData")
#load("m:/Users/Marcus/Downloads/Vars.RData")

########### marcelofernandes.com.br #########
KK = c(5000, 35000, 210000)

mods = list(r=list(),b=list())
for(k in KK){
  size = sample(1:n, k)
  
  nt = sample(size, 0.5*k)
  
  Yt = Y[nt]
  Xt = Vars[nt,]
  
  Yr = Y[size[-which(size%in%nt)]]
  Xr = Vars[size[-which(size%in%nt)],]
  
  for(i in colnames(Xt)){
    if(is.factor(Xt[,i])){
      Xt[,i] = factor(Xt[,i])
      if(length(levels(Xt[,i]))<=1){
        Xt[,i] = NULL}}}
  
  for(i in colnames(Xr)){
    if(is.factor(Xr[,i])){
      Xr[,i] = factor(Xr[,i])
      if(length(levels(Xr[,i]))<=1){
        Xr[,i] = NULL}}}
  
  r = 10
  rand = numeric()
  for(i in 1:r){
    rand = cbind(rand, rnorm(k))}
  
  colnames(rand) = paste0("R", 1:r)
  Xt = cbind(Xt, rand[1:(0.5*k),])
  Xr = cbind(Xr, rand[(0.5*k+1):k,])
  
  modr.t = lm(Yt ~ ., Xt)
  #summary(modr.t)
  
  modr.r = lm(Yr ~ ., Xr)
  #summary(modr.r)
  
  modb.t = lm(Yt[Xt$R1>=0] ~ ., Xt[Xt$R1>=0,])
  #summary(modb.t)
  
  modb.r = lm(Yr[Xr$R1<0] ~ ., Xr[Xr$R1<0,])
  #summary(modb.r)
  
  mods$r[[paste(k)]] = list(modr.t, modr.r)
  mods$b[[paste(k)]] = list(modb.t, modb.r)}

rm(modb.r, modb.t, modr.r, modr.t)

stargazer(mods$r, single.row=TRUE,
          se=list(NA,NA,NA,NA,NA,NA),
          digits=2)

mytabb = capture.output(stargazer(mods$b,
                                 single.row=TRUE,
                                 digits=2))

mytabr = capture.output(stargazer(mods$r,
                                 single.row=TRUE,
                                 digits=2))

cat(paste(gsub("\\([0-9]\\.[0-9]+\\)", "", mytabb), collapse = "\n"), "\n")

cat(paste(gsub("\\([0-9]\\.[0-9]+\\)", "", mytabr), collapse = "\n"), "\n")


stargazer(mods$r,
          single.row=TRUE,
          digits=2,
          apply.coef=function(x){x*100},
          apply.se=function(x){x*100},
          keep="R")

stargazer(mods$b,
          single.row=TRUE,
          digits=2,
          keep="R")

summary(mods$r$`5000`[[1]])


for(i in KK){
  print(paste("r", i))
  print(sum((Yr - predict(mods$r[[paste(i)]][[1]], Xr))^2))
  print(paste("b", i))
  print(sum((Yr[Xr$R1<0] - predict(mods$b[[paste(i)]][[1]], Xr[Xr$R1<0,]))^2))}

linearHypothesis(mods$I, singular.ok=TRUE,
                 c("`OUTROS TOTAL`:INTELECTO22",
                   "`OUTROS TOTAL`:VISÃO2",
                   "`OUTROS TOTAL`:AUDIÇÃO2",
                   "`OUTROS TOTAL`:LOCOMOÇÃO2"))