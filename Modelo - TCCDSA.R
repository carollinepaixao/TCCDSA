# Modelo Regressão Logística Binária - TCC

#Step1 - Instalar pacotes

pacotes <- c("plotly","tidyverse","ggrepel","knitr", "kableExtra", 
             "sjPlot","FactoMineR","amap","ade4","readxl","dplyr",
             "fastDummies","reshape2","rgl","car","jtools","magick",
             "cowplot","equatiomatic","stargazer","lmtest","caret",
             "pROC","ROCR","nnet","globals")


options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Step 2 - Carregar dados

dados <- read_csv("Database 2010-2020 - C421.csv")

summary(dados)

#Step 3 - Remover as variáveis não relevantes da base

dados <- dados %>%
  select(-Codigo_do_Paciente, -Nome_do_RCBP, -Data_de_Nascimento, -Nacionalidade,	-Naturalidade_Estado,	-Naturalidade,
         -Codigo_Profissao, -Nome_Profissao, -Cidade_Endereco, -Descricao_da_Morfologia, -Codigo_da_Morfologia,	
         -Descricao_da_Doenca, -Codigo_da_Doenca,	-Descricao_da_Doenca_Infantil,	-Codigo_da_Doenca_Infantil,
         -Descricao_da_Doenca_Adulto_Jovem,	-Codigo_da_Doenca_Adulto_Jovem,	-Indicador_de_Caso_Raro, 
         -Extensao,-Lateralidade,	-Estadiamento,	-TNM, -Data_de_Ultimo_Contato,	-Metastase_a_distancia, -Data_do_obito,	
         -Data_Obito2,-Data_de_Diagnostico, -Data_Diag2, -Morte_Cancer,	-Check, -Codigo_da_Topografia,
         -Data_Diagnostico,-Status_Vital, -Ano_Obito)


#Step 4 - Criar Variável Indicar Casos de Morte de Câncer de Medula Óssea em 1 ano

dados <- dados %>%
  mutate(Morte_Primeiro_Ano = case_when((Dias_Diagn_Obito <= 365 & Data_Obito != "" & Tipo_do_Obito == 'CÂNCER') ~ 1,
                                        TRUE ~ 0))

table(dados$Morte_Primeiro_Ano)

#Step 5 - Categorizar variáveies "Idade" e "Estado_Endereço" 

dados <- dados %>%
  mutate(Categ_Idade = case_when(Idade <= quantile(Idade, 0.3333, na.rm = T) ~ "Menores_Idades",
                                 Idade > quantile(Idade, 0.3333, na.rm = T) & Idade <= quantile(Idade, 0.6667, na.rm = T) ~ "Idades_Médias",
                                 Idade > quantile(Idade, 0.6667, na.rm = T) ~ "Maiores_Idades"))

dados <- dados %>%
  mutate(Regiao = case_when(Estado_Endereco %in% c("DISTRITO FEDERAL", "GOIAS", "MATO GROSSO", "MATO GROSSO DO SUL") ~ "Centro-Oeste",
                            Estado_Endereco %in% c("ALAGOAS", "CEARÁ", "PARAÍBA", "PERNAMBUCO", "SERGIPE") ~ "Nordeste", 
                            Estado_Endereco %in% c("ACRE", "AMAPA", "AMAZONAS", "PARÁ", "RONDONIA", "RORAIMA", "TOCANTINS") ~ "Norte",
                            Estado_Endereco %in% c("ESPIRITO SANTO", "MINAS GERAIS", "RIO DE JANEIRO", "SÃO PAULO") ~ "Sudeste",
                            Estado_Endereco %in% c("PARANÁ", "RIO GRANDE DO SUL", "SANTA CATARINA") ~ "Sul"))
#Step 6 - Reordenar Base

dados <- dados %>% select("Morte_Primeiro_Ano",everything())

dados <- dados %>%
  select(-Idade, -Dias_Diagn_Obito, -Data_Obito,-Tipo_do_Obito, -Ano_Diagnostico, -Descricao_da_Topografia, -Estado_Endereco)

#Step 7 - Transformar Variáveis em factor

dados <- as.data.frame(unclass(dados), stringsAsFactors=TRUE)

glimpse(dados)
summary(dados)

#Step 8 - Alterar Categorias de Referência

levels(dados$Raca.Cor)
dados$Raca.Cor <- relevel(dados$Raca.Cor, ref = "BRANCO")

levels(dados$Categ_Idade)
dados$Categ_Idade <- relevel(dados$Categ_Idade, ref = "Menores_Idades")

levels(dados$Regiao)
dados$Regiao <- relevel(dados$Regiao, ref = "Sudeste")

#Step 9 - Dummizar variáveis

dados_dummies <- dummy_columns(.data = dados,
                               select_columns = c("Sexo",
                                                  "Categ_Idade",
                                                  "Raca.Cor",
                                                  "Grau_de_Instrução",
                                                  "Estado_Civil",
                                                  "Regiao",
                                                  "Meio_de_Diagnostico"),
                               remove_selected_columns = T,
                               remove_first_dummy = T,
                               ignore_na = T)

#Step 10 - Estimar modelo

modelo_dadosdm <- glm(formula = Morte_Primeiro_Ano ~ ., 
                      data = dados_dummies, 
                      family = binomial(link = "logit"))

#Parâmetros do modelo
summ(modelo_dadosdm, confint = T, digits = 3, ci.width = .95)

# Valor do LL do modelo
logLik(modelo_dadosdm)

#Step 11 - Análise Pressupostos 

#Checar Ausência Outliers

library("MASS")

plot(modelo_dadosdm, which =5)

#Checar Ausência de Multicolinearidade

library("psych")

pairs.panels(dados)

#Fator Inflação - VIF

vif(modelo_dadosdm)

#Step 12 - Analisar Modelo - Teste Qui-Quadrado

modelo_nulo <- glm(formula = Morte_Primeiro_Ano ~ 1, 
                   data = dados, 
                   family = binomial(link = "logit"))
logLik(modelo_nulo)

modelo_dados <- glm(formula = Morte_Primeiro_Ano ~ ., 
                    data = dados, 
                    family = binomial(link = "logit"))
logLik(modelo_dados)

chi2 <- -2*(logLik(modelo_nulo)-logLik(modelo_dados))
chi2
pchisq(chi2, df = 30, lower.tail = F)

# Step 13 - Análise de efeito geral do modelo (teste Z de Wald)

Anova(modelo_dados, type ="II", test = "Wald") 

#Step 14 - Log-Likelihood - Obter razões de chance com IC 95%

exp(cbind(OR = coef(modelo_dadosdm), confint(modelo_dadosdm)))

# Step 15 - AIC/BIC

AIC(modelo_dadosdm)
BIC(modelo_dadosdm)

#Step 16 - Tabela de classificação

library(QuantPsyc)

ClassLog(modelo_dadosdm, dados$Morte_Primeiro_Ano)

#Step 17 - Stepwise

step_modelodm <- step(object = modelo_dadosdm,
                      k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

# Parâmetros do modelo step_modelodm
summ(step_modelodm, confint = T, digits = 3, ci.width = .95)

#Valor do LL do modelo após o stepwise
logLik(step_modelodm)

#Step 18 - Comparando os modelos step_modelodm e modelo_dadosdm

lrtest(modelo_dadosdm, step_modelodm)

export_summs(modelo_dadosdm, step_modelodm,
             model.names = c("Modelo Dummies","Modelo Dummies Stepwise"),
             scale = F, digits = 4)

#Step 19 - Log-Likelihood - Obter razões de chance com IC 95% após stepwise

exp(cbind(OR = coef(step_modelodm), confint(step_modelodm)))

# Step 20 - AIC/BIC após stepwise

AIC(step_modelodm)
BIC(step_modelodm)

#Step 21 - Tabela de classificação após stepwise

ClassLog(step_modelodm, dados$Morte_Primeiro_Ano)
