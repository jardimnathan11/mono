###### pacotes utilizados
library(forecast)
library(tseries)
library(urca)
library(lmtest)
library(tsDyn)
library(vars)
library(readxl)
library(ggplot2)
library(rmgarch)
#############################

########################## Puxar variáveis
sup <- read_excel("C:/Users/Nathan/Downloads/monografia/sup.xlsx")
sup<-sup[,2] ##tirar datas do excel
obrig <- read_excel("C:/Users/Nathan/Downloads/monografia/obrig.xls")
obrig<-obrig[,2]##tirar datas do excel
sup<-ts(sup, frequency = 12, start=2002, end=2019)# transformando em series temporais
obrig<-ts(obrig, frequency = 12, start=2002, end = 2019)# transformando em series temporais
plot(obrig, main=' Obrigações em proporção ao PIB ', ylab="Proporção ao pib")
plot(sup, col='red',main=' Superávit em proporção ao PIB', ylab="Proporção ao pib",type='l')
sup_diff<-diff(sup, differences = 1)# pegando em diferenças
obrig_diff<-diff(obrig, differences = 1)# pegando em diferenças
###############

########### testes de raizes unitarias

adf.test(sup, alternative = 'stationary', k=0)# p-value smaller than printed p-value, ou seja, estacionário
adf.test(sup_diff, alternative = 'stationary', k=0)# p-value smaller than printed p-value, ou seja, estacionário
adf.test(obrig, alternative = 'stationary', k=0)# p-value = 0.2098
adf.test(obrig_diff, alternative = 'stationary', k=0)#p-value smaller than printed p-value
pp.test(obrig, alternative = c("stationary"), lshort = TRUE)#p-value = 0.3402
pp.test(obrig_diff, alternative = c("stationary"), lshort = TRUE)#p-value smaller than printed p-value
adf.test(log(obrig), alternative = 'stationary', k=0)

######### apenas uma confirmação de que nao ha cointegracao
reg<-lm(obrig~sup)
erro<-resid(reg)
Box.test(erro,lag=3,type="Ljung-Box")##p-value < 2.2e-16

eg=ur.df(erro)
summary(eg)# -1.4298  > -1.95 (5%) -> não rejeitamos H0 de não cointegração, logo as series não se cointegram.

#########  


########selecao do VAR 

superavit<-sup[1:204] # para ter o numero de periodos do obrig_diff
var1<-ts.intersect(superavit,obrig_diff) # unir as series
VARselect(var1,lag.max = 20, type = 'const', season =12  ) # funcao para criterio de informacao
##AIC(n)  HQ(n)  SC(n) FPE(n) 
#    3      3      2      3 
dummy_obrig=seasonaldummy(obrig_diff) # definicao de dummies sazonais para o VAR
var_results1<-VAR(var1, type='const', p =11, exogen = dummy_obrig )
normality.test(var_results1, multivariate.only = TRUE) # teste de normalidade 
summary(var_results1) 
#########autocor
roots(var_results1) # taizes da eq caracteristica
serial.test(var_results1) # teste de autocorelacao
arch.test(var_results1, lags.multi = 11,  multivariate.only = TRUE)# teste de heterocedascidade
######

############### definicao de matrizes para a decomposicao de Cholesky
a.mat <- diag(2)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
b.mat <- diag(2)
diag(b.mat) <- NA
var_results_sup_primeiro<-SVAR(var_results1,Amat = a.mat, Bmat = b.mat, max.iter = 10000, lrtest = FALSE,
                               hessian = TRUE) # construcao do VAR estrutural 
################

######### impulsos resposta 
ir.1 = irf(var_results_sup_primeiro, impulse = "superavit", response = "obrig_diff", n.ahead =20,
           ortho = FALSE, cumulative = T)
plot(ir.1, main = 'resposta de obrigações à superávit com superávit primeiro')
ir.2 = irf(var_results_sup_primeiro, impulse = "superavit", response = "superavit", n.ahead = 12,
           ortho = FALSE)
plot(ir.2,main = 'resposta de superávit à superávit com superávit primeiro')
##############

######### mesma coisa agora faremos com outra ordem no ts por causa da decomposição de cholesky
var2<-ts.intersect(obrig_diff, superavit)
VARselect(var2,lag.max = 20, type = 'const', season =12  )
##AIC(n)  HQ(n)  SC(n) FPE(n) 
#    3      3      2      3 
dummy_obrig=seasonaldummy(obrig_diff)
var_results2<-VAR(var2, type='const', p =11, exogen = dummy_obrig )
normality.test(var_results2, multivariate.only = TRUE)
summary(var_results2)
#########testes
roots(var_results2) # raizes da eq caracteristica
serial.test(var_results2)# teste de autocorrelacao
arch.test(var_results2, lags.multi = 11,  multivariate.only = TRUE)# teste de heterocedascidade
######decomposicao de cholesky
a.mat <- diag(2)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
b.mat <- diag(2)
diag(b.mat) <- NA
#######
var_results_obrig_primeiro<-SVAR(var_results2,Amat = a.mat, Bmat = b.mat, max.iter = 10000, lrtest = FALSE,
                                 hessian = TRUE)# var estrutural
### impulsos resposta
ir.3 = irf(var_results_obrig_primeiro, impulse = "superavit", response = "obrig_diff", n.ahead = 20,
           ortho = FALSE, cumulative = T)
plot(ir.3,main = 'resposta de obrigações à superávit com obrigações primeiro')
ir.4 = irf(var_results_obrig_primeiro, impulse = "superavit", response = "superavit", n.ahead = 12,
           ortho = FALSE)
plot(ir.4,main = 'resposta de superávit à superávit com obrigações primeiro' )

#########

###########  mesmo procedimento, mas agora com sup_diff
var3<-ts.intersect(sup_diff,obrig_diff)
VARselect(var3,lag.max = 20, type = 'const', season=12)
##AIC(n)  HQ(n)  SC(n) FPE(n) 
#   11      2      2     11 
var_results3<-VAR(var3, type='const', p =11, exogen = dummy_obrig )
summary(var_results3)
#########testes 
roots(var_results3) # raizes da eq carateristica
serial.test(var_results3) # teste de autocorrelacao
arch.test(var_results3, lags.multi = 11,  multivariate.only = TRUE)# teste de heterocedascidade

################## teste de autocorrelacao da variável sup
Box.test(sup) # teste ljung box nivel

Box.test(sup_diff) # teste primeira dif
Box.test(sup, type = 'Ljung-Box', lag=12)### rejeito a nula, logo correlação no teste com 12 diferencas
###### 

########## agora vamos botar juros no modelo
alfa <- read_excel("C:/Users/Nathan/Downloads/monografia/selic.xlsx")
alfa<-alfa[,2] # tirar datas
alfa<-alfa[1:205,] # definir os periodos
alfa<-ts(alfa, frequency = 12, start=2002)# transformar em serie 
plot(alfa, main=' Taxa de juros selic ', ylab="valor percentual")
alfa_diff<-diff(alfa, differences = 1) ## pegar em diferença

######

## testes de raizes
adf.test(alfa, alternative = 'stationary', k=0)# p-value = 0.9189
adf.test(alfa_diff, alternative = 'stationary', k=0)# p-value = <0.01
pp.test(alfa, alternative = c("stationary"), lshort = TRUE)#p-value 0.5808
pp.test(alfa_diff, alternative = c("stationary"), lshort = TRUE)#p-value smaller than printed p-valuest(alfa_diff, alternative = 'stationary', k=0)# p-value smaller than printed p-value, ou seja, estacionário
##########
selic<-alfa## mudei o nome 
selic_diff<-alfa_diff #### mudei o nome 
var_alfa1<-ts.intersect(superavit,obrig_diff, selic_diff) # unimos a serie com essa ordem 

VARselect(var_alfa1,lag.max = 20, type = 'const', season=12)#### teste de informacao
##AIC(n)  HQ(n)  SC(n) FPE(n) 
#    6      2      1      6   
var_results_alfa1<-VAR(var_alfa1, type='const', p =11, exogen = dummy_obrig) # estimamos o var
######

####### decomposicao de cholesky
a.mat <- diag(3)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
b.mat <- diag(3)
diag(b.mat) <- NA
######

######## testes do VAR como no modelo sem selic
summary(var_results_alfa1)
roots(var_results_alfa1)
serial.test(var_results_alfa1)
arch.test(var_results_alfa1,   multivariate.only = TRUE)
#########
var_results_alfa_sup_primeiro<-SVAR(var_results_alfa1,Amat = a.mat, Bmat = b.mat, max.iter = 10000, lrtest = FALSE,
        hessian = TRUE)# estimar o var estrutural
######## impulsos respostas
ir.7 = irf(var_results_alfa_obrig_primeiro, impulse = "superavit", response = "obrig_diff", n.ahead =20
        ,  ortho = FALSE, cumulative = T)
plot(ir.7, main = 'resposta de obrigações à superávit com superávit primeiro')
ir.8 = irf(var_results_alfa_obrig_primeiro, impulse = "superavit", response = "superavit", n.ahead = 12,
           ortho = FALSE)
plot(ir.8, main = 'resposta de superávit à superávit com superávit primeiro')
ir.9 = irf(var_results_alfa_obrig_primeiro, impulse = "superavit", response = "selic_diff", n.ahead = 12,
           ortho = FALSE, cumulative = T)
plot(ir.9, main = 'resposta de selic à superávit com superávit primeiro')
#####

#########mudamos a ordem temporal do modelo para obrig antes e fizemos tudo novamente
var_alfa2<-ts.intersect(obrig_diff,superavit, selic_diff)# ordenamneto
VARselect(var_alfa2,lag.max = 20, type = 'const', season=12)# criterio da informacao
##AIC(n)  HQ(n)  SC(n) FPE(n) 
#    6      2      1      6   
var_results_alfa2<-VAR(var_alfa2, type='const', p =11, exogen = dummy_obrig) # estimar var
###
######## decomposicao de cholesky
a.mat <- diag(3)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
b.mat <- diag(3)
diag(b.mat) <- NA
###### testes 
summary(var_results_alfa2)
roots(var_results_alfa2)
serial.test(var_results_alfa2)
arch.test(var_results_alfa2,   multivariate.only = TRUE)
#########
var_results_alfa_obrig_primeiro<-SVAR(var_results_alfa2,Amat = a.mat, Bmat = b.mat, max.iter = 10000, lrtest = FALSE,
                                    hessian = TRUE) # var estrutural

#### impulsos respostas 
ir.10 = irf(var_results_alfa_obrig_primeiro, impulse = "superavit", response = "obrig_diff", n.ahead = 20,
           ortho = FALSE, cumulative = T)
plot(ir.10, main = 'resposta de obrigações à superávit com obrigações primeiro')
ir.11 = irf(var_results_alfa_obrig_primeiro, impulse = "superavit", response = "superavit", n.ahead = 12,
           ortho = FALSE)
plot(ir.11 ,main = 'resposta de superávit à superávit com obrigações primeiro')
ir.12 = irf(var_results_alfa_obrig_primeiro, impulse = "superavit", response = "selic_diff", n.ahead = 12,
           ortho = FALSE, cumulative = T)
plot(ir.12,main = 'resposta de Selic à superávit com obrigações primeiro' )
