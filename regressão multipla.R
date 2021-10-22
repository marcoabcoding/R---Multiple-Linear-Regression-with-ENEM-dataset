
# Limpa a memória do software R
rm(list = ls())

# Utiliza virgula para demarcar casas decimais (padrao portugues)
options(OutDec = ",")

# Verifica se os pacotes do software R que serao utilizados estao instalados. Em caso negativo, realiza instalacao 
if(!require("ggplot2")){install.packages("ggplot2"); dependencies=TRUE}
if(!require("MASS")){install.packages("MASS"); dependencies=TRUE}
if(!require("car")){install.packages("car"); dependencies=TRUE}
if(!require("gridExtra")){install.packages("gridExtra"); dependencies=TRUE}

# Carrega os pacotes que serao utilizados
require(ggplot2)
require(MASS)
require(car)
require('gridExtra')

# ------------------------------------------------------------------------------
# Primeiro Passo: Realizar a Leitura dos Dados

# Leitura dos dados

dados <- read.table("C:\\Users\\marki\\OneDrive\\�rea de Trabalho\\regressao_multipla.csv", 
                    sep = ";", dec = ",", header = T)
head(dados)

# ------------------------------------------------------------------------------
## Análise descritiva dos Dados

# 1) Se X for contínua, utilize um gráfico de Dispersão

# Gráfico de Dispersão entre X e Y, utilizando o pacote GGPLOT 
g1 = ggplot(dados, aes(y=`Notas_EST`, x=`NOTAS_ENEM`)) + 
  geom_point(size = 1) + 
  # O comando abaixo permite incluir a reta de regressao no grafico de dispersao
  geom_smooth(method=lm, se=F)+
  theme_bw()+
  labs(y="Nota em Estatística", x="Nota no ENEM",  title = "(a)")+
  theme(axis.text.x = element_text(hjust = 1, size=12),
        axis.text.y = element_text(hjust = 1, size=12),
        plot.title = element_text(hjust = 0.5),
        axis.title=element_text(size=12))
modelo4 = lm(Notas_EST ~ NOTAS_ENEM, data = dados)
summary(modelo4)$r.squared


g2 = ggplot(dados, aes(y=`Notas_EST`, x=`Idade`)) + 
  geom_point(size = 1) + 
  # O comando abaixo permite incluir a reta de regressao no grafico de dispersao
  geom_smooth(method=lm, se=F)+
  theme_bw()+
  labs(y="Nota em Estatística", x="Idade",  title = "(b)")+
  theme(axis.text.x = element_text(hjust = 1, size=12),
        axis.text.y = element_text(hjust = 1, size=12),
        plot.title = element_text(hjust = 0.5),
        axis.title=element_text(size=12))


# 2) Se X for uma variável categórica, uma opção é utilizar Boxplot**
# Apenas como exemplo, suponha que houvesse uma variavel categofica x2
# Variavel Sexo
g3 = ggplot(dados, aes(x=factor(`Sexo`), y=`Notas_EST`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota em Estatística", x="Sexo",  title = "(c)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))


g4 = ggplot(dados, aes(x=factor(`Horas_estudo`), y=`Notas_EST`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota em Estatística", x=" Horas de estudo",  title = "(d)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))




#Organiza todos os gráficos em uma única figura
windows()
grid.arrange(arrangeGrob(g1, g2, g3, g4, ncol = 2, nrow =2))


#--------------------------------------------------------------------------------------------
# site para aprender um pouco mais sobre o GGPLOT. Varios Exemplos de Graficos. 
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

##------------------------------------------------------------------------------------------

## Ajuste do Modelo

# O comando "lm" ajusta o modelo de regressão linear no software R
modelo = lm(Notas_EST ~ Turma + Sexo + Idade + Horas_estudo + NOTAS_ENEM, data = dados)
modelo2 = lm(Notas_EST ~ NOTAS_ENEM + Horas_estudo + Sexo + Idade  + Turma, data = dados)
# Utilize o comando summary para visualizar os resultados
summary(modelo)
summary(modelo2)
#-----------------------------------------------------------------------------------
# ANOVA

#Para calcular a anova no R é necessário ajustar o modelo nulo
modelo.null = lm(Notas_EST ~1, data = dados)
summary(modelo.null)

anova(modelo.null, modelo)

#-----------------------------------------------------------------------------------
# Verificando se existe multicolinearidade
vif(modelo)

#-----------------------------------------------------------------------------------
# Seleção de variáveis

#Pode-se fazer manualmente ou utilizando o comando step
summary(modelo)

modelo2 = lm(Notas_EST ~ ENEM_MAT+ Horas_estudo + Sexo, data = dados)
summary(modelo2)

modelo3 = lm(Notas_EST ~ ENEM_MAT+ Horas_estudo, data = dados)
summary(modelo3)

# Utilizando o comando step (critério AIC)
step(modelo,direction = c("both")) direction = ("both")
?step
modelo3 = lm(Notas_EST ~ NOTAS_ENEM+ Horas_estudo, data = dados)
summary(modelo3)

summary(modelo3)$adj.r.squared

#-----------------------------------------------------------------------------------
# Modelo final
modelo.final = lm(Notas_EST ~ NOTAS_ENEM + Horas_estudo, data = dados)
summary(modelo.final)

modelo = lm(Notas_EST ~ Turma + Sexo + Idade + Horas_estudo + NOTAS_ENEM, data = dados)
# Coeficientes do Modelo
round(summary(modelo)$coef,4)

# Coeficiente de Determinação R^2
summary(modelo.final)$adj.r.squared

# Estimativa de sigma
summary(modelo.final)$sigma

#-----------------------------------------------------------------------------------
## Análise de resíduos

#Obtendo os resíduos
residuos = residuals(modelo3) 

# Teste de Normalidade para os resíduos
shapiro.test(residuos)$p.value

#-----------------------------------------------------------------------------------
## Gráficos para análise dos resíduos

windows()
par(mfrow = c(1,3))

qqnorm(residuos, xlab = "Quantis Teóricos", ylab = "Quantis Empíricos")
qqline(residuos)

plot(fitted(modelo.final), residuos, ylab = "Resíduos"
     , xlab = "Valores ajustados")
abline(h = 0, col = "red", lty = 2)

plot(residuos, ylab= "Resíduos", xlab = "Ordem")
abline(h = 0, col = "red", lty = 2)


## OU utilize os resíduos padronizados ------------------------------------------------

X = model.matrix(modelo)
H= X %*% solve(t(X)%*%X)%*%t(X)
resid.padrao = residuals(modelo)
s  = summary(modelo)$sigma
resid.h = resid.padrao /(s *sqrt(1-diag(H)))

windows()
par(mfrow = c(1,3))

qqnorm(resid.h, xlab = "Quantis Teóricos", ylab = "Quantis Empíricos")
qqline(resid.h)

plot(fitted(modelo.final), resid.h, ylab = "Resíduos padronizados"
     , xlab = "Valores ajustados")
abline(h = 0, col = "red", lty = 2)

plot(resid.h, ylab= "Resíduos padronizados", xlab = "Ordem")
abline(h = 0, col = "red", lty = 2)

#------------------------------------------------------------------------------------
# Pontos Influentes
#------------------------------------------------------------------------------------
# Cálculo da Distância de Cook

windows()
cooksd <- cooks.distance(modelo3)
sample_size <- nrow(dados)

maximo = max(cooksd, 1.1)
plot(cooksd, pch="*",  ylab = "Distância de Cook", 
     ylim = c(0,maximo))  # Gráfico da Distância de Cook
abline(h = 1, col="orange")  # Critério 1
abline(h = 4/sample_size, col="red")  # Critério 2

# Cálculo da Distância de Cook

distanca.cook = cooks.distance(modelo3)
max(distanca.cook)

