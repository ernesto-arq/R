#
#
#
#
#INSTALACAO E LEITURA DOS PACTOES
#install.packages(vetor_pacotes)
vetor_pacotes=c("readr","ggplot2","plotly","e1071",
                "dplyr","Hmisc","DescTools","esquisse","gridExtra")
#
#
#
#
#
#INSTALACAO DOS PACTOS REQUERIDOS PARA DESENVOLVIMENTO E ANALISE
#install.packages(vetor_pacotes)
lapply(vetor_pacotes, require, character.only = TRUE)
library(readr)
library(ggplot2)
library(plotly)
library(e1071)
library(utils)
require(dplyr)
require(Hmisc)
require(esquisse)
library(DescTools)
require(gridExtra)
require(e1071)
#
#
#
#
#
vetor_pacotes=c("readr",
                "ggplot2",
                "plotly",
                "e1071",
                "dplyr",
                "Hmisc",
                "DescTools",
                "esquisse",
                "gridExtra",
                "e1071",
                "devtools"
)
#
#
#
#
#
lapply(vetor_pacotes, 
       require, 
       character.only = TRUE)
#
#
#
#
#
## [[1]]
## [1] TRUE
## 
## [[2]]
## [1] TRUE
## 
## [[3]]
## [1] TRUE
## 
## [[4]]
## [1] TRUE
## 
## [[5]]
## [1] TRUE
## 
## [[6]]
## [1] TRUE
## 
## [[7]]
## [1] TRUE
## 
## [[8]]
## [1] TRUE
## 
## [[9]]
## [1] TRUE
## 
## [[10]]
## [1] TRUE
## 
## [[11]]
## [1] TRUE
#
#
#
#
#
#install_github("haozhu233/kableExtra")
library(kableExtra)
#
#
#
#
#
#Importacao do banco do ENADE/INEP E SET DO LOCAL DO DIRETORIO DO PROJETO A SER LIDO
setwd("C:\\Users\\Ernesto\\Downloads\\Projeto Final")
getwd()
#
#
#
#
#
microdados_enade = read.table("MICRODADOS_ENADE_2017.txt",
                              header = TRUE, 
                              sep=";", 
                              dec = ",", 
                              colClasses=c(NT_OBJ_FG="numeric"))
#
#
#
#
#
enade2017 = read_csv2("MICRODADOS_ENADE_2017.txt") 
#
#
#
#
#
#verificando a head, check de leitura do arquivo
head(enade2017)
#
#
#
#
#
##Selecionando as variaveis desejadas
microdados_enade_filtrados= enade2017 %>% dplyr::select(CO_GRUPO,CO_REGIAO_CURSO,NU_IDADE,
                                                        TP_SEXO,CO_TURNO_GRADUACAO,NT_GER,
                                                        QE_I01,QE_I02,QE_I08,
                                                        QE_I21,QE_I23,NT_OBJ_FG, 
                                                        NT_OBJ_CE
)      
#
#
#
#
#
##Selecionando o curso de ADS
microdados_gestao_ti= microdados_enade_filtrados %>% filter(CO_GRUPO==6409)
#
#
#
#
#
##Transformando as variaveis (Colocando os labels)
microdados_gestao_ti = microdados_gestao_ti %>% mutate(estado_civil2 = case_when( QE_I01 == "A" ~ "Solteiro(a)",
                                                                    QE_I01 == "B" ~ "Casado(a)",
                                                                    QE_I01 == "C" ~ "Separado(a)",
                                                                    QE_I01 == "D" ~ "Viúvo(a)",
                                                                    QE_I01 == "E" ~ "Outro"
)) 

microdados_gestao_ti = microdados_gestao_ti %>% mutate(regiao = case_when( CO_REGIAO_CURSO == 1 ~ "Norte",
                                                             CO_REGIAO_CURSO == 2 ~ "Nordeste",
                                                             CO_REGIAO_CURSO == 3 ~ "Sudeste",
                                                             CO_REGIAO_CURSO == 4 ~ "Sul",
                                                             CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"
)) 

microdados_gestao_ti = microdados_gestao_ti %>% mutate(sexo = case_when( TP_SEXO == "M" ~ "Masculino",
                                                           TP_SEXO == "F" ~ "Feminino"
)) 

microdados_gestao_ti = microdados_gestao_ti %>% mutate(horas_estudos = case_when( QE_I23 == "A" ~ "Nenhuma, apenas assisto as aulas",
                                                               QE_I23 == "B" ~ "De uma a três",
                                                               QE_I23 == "C" ~ "De quatro a sete",
                                                               QE_I23 == "D" ~ "De oito a doze",
                                                               QE_I23 == "E" ~ "Mais de doze"
)) 

microdados_gestao_ti = microdados_gestao_ti %>% mutate(Renda = case_when( QE_I08 == "A" ~ "1.405,50",
                                           QE_I08 == "B" ~ "1.405,51 A 2.2811,00",
                                           QE_I08 == "C" ~ "2.2811,01 A 4.216,50",
                                           QE_I08 == "D" ~ "4.216,51 A 5622,00",
                                           QE_I08 == "E" ~ "5622,00 A 9.370,00",
                                           QE_I08 == "F" ~ "9.370,00 A 28.110,00",
                                           QE_I08 == "G" ~ "MAIOR QUE 28.110,00"
))

#microdados_gestao_ti = microdados_gestao_ti %>% mutate(CATEGORIA = case_when( CO_CATEGAD == 1 ~ "PUBLICA FEDERAL",
#                                                                CO_CATEGAD == 2 ~ "PUBLICA ESTADUAL",
#                                                                CO_CATEGAD == 3 ~ "PUBLICA MUNICIPAL",
#                                                                CO_CATEGAD == 4 ~ "PRIVADA COM FINS LUCRATIVOS",
#                                                                CO_CATEGAD == 5 ~ "PRIVADA SEM FINS LUCRATIVOS",
#                                                                CO_CATEGAD == 7 ~ "ESPECIAL",
#))

microdados_gestao_ti = microdados_gestao_ti %>% mutate(turno = case_when( CO_TURNO_GRADUACAO == 1 ~ "MANHA",
                                            CO_TURNO_GRADUACAO == 2 ~ "TARDE",
                                            CO_TURNO_GRADUACAO == 3 ~ "INTERINO",
                                            CO_TURNO_GRADUACAO == 4 ~ "NOITE"
))

microdados_gestao_ti = microdados_gestao_ti %>% mutate(raca = case_when( QE_I02 == "A" ~ "BRANCO",
                                           QE_I02 == "B" ~ "NEGRO",
                                           QE_I02 == "C" ~ "AMARELO",
                                           QE_I02 == "D" ~ "PARDO",
                                           QE_I02 == "E" ~ "INDIGINA",
                                           QE_I02 == "F" ~ "NAO DECLARADO"
))

microdados_gestao_ti = microdados_gestao_ti %>% mutate(superior = case_when( QE_I21 == "A" ~ "SIM",
                                                               QE_I21 == "B" ~ "NÃO"
))
#
#
#
#
#
##Data quality e Bloco da analise descritiva das variaveis
describe(microdados_gestao_ti$estado_civil2)
describe(microdados_gestao_ti$regiao)
describe(microdados_gestao_ti$sexo)
describe(microdados_gestao_ti$horas_estudos)
describe(microdados_gestao_ti$Renda)
describe(microdados_gestao_ti$turno)
describe(microdados_gestao_ti$raca)
describe(microdados_gestao_ti$superior)
#
#
#
#
#
#verificando
names(microdados_gestao_ti)
#
#
#
#
#
#Verificando a classe das variaveis
class(microdados_gestao_ti$estado_civil)
class(microdados_gestao_ti$estado_civil2)
class(microdados_gestao_ti$regiao)
class(microdados_gestao_ti$sexo)
class(microdados_gestao_ti$horas_estudos)
class(microdados_gestao_ti$Renda)
class(microdados_gestao_ti$turno)
class(microdados_gestao_ti$raca)
class(microdados_gestao_ti$superior)
class(microdados_gestao_ti$NT_OBJ_CE)
#
#
#
#
#
#BLOCO DA ANaLISE DESCRITIVA DAS VARI?VEIS
#Resumindo os dados
s=summary(microdados_gestao_ti)  
d=describe(microdados_gestao_ti)
#
#
#
#
#
#Selecionando-se os resumos de interesse
d$TP_SEXO
d$regiao$values
d$regiao$values$frequency/sum(d$CO_REGIAO_CURSO$values$frequency)
s[1:4,1:6]
#
#
#
#
#
#Frequencias simples das variaveis de outra forma (estadocivil2:#resumo estado civil
#estado civil
t=table(microdados_gestao_ti$estado_civil2)
p=prop.table(t)
describe(microdados_gestao_ti$estado_civil2)
unique(microdados_gestao_ti$estado_civil2)

#regiao
t=table(microdados_gestao_ti$regiao)
p=prop.table(t)
describe(microdados_gestao_ti$regiao)
unique(microdados_gestao_ti$regiao)

#sexo
t=table(microdados_gestao_ti$sexo)
p=prop.table(t)
describe(microdados_gestao_ti$sexo)
unique(microdados_gestao_ti$sexo)

#horas_estudos
t=table(microdados_gestao_ti$horas_estudos)
p=prop.table(t)
describe(microdados_gestao_ti$horas_estudos)
unique(microdados_gestao_ti$horas_estudos)

#Renda
t=table(microdados_gestao_ti$Renda)
p=prop.table(t)
describe(microdados_gestao_ti$Renda)
unique(microdados_gestao_ti$Renda)

#turno
t=table(microdados_gestao_ti$turno)
p=prop.table(t)
describe(microdados_gestao_ti$turno)
unique(microdados_gestao_ti$turno)

#raca
t=table(microdados_gestao_ti$raca)
p=prop.table(t)
describe(microdados_gestao_ti$raca)
unique(microdados_gestao_ti$raca)

#superior
t=table(microdados_gestao_ti$superior)
p=prop.table(t)
describe(microdados_gestao_ti$superior)
unique(microdados_gestao_ti$superior)

#NT_OBJ_CE
t=table(microdados_gestao_ti$NT_OBJ_CE)
p=prop.table(t)
describe(microdados_gestao_ti$NT_OBJ_CE)
unique(microdados_gestao_ti$NT_OBJ_CE)
#
#
#
#
#
#Total, GRUPAMENTOS, ESTADO CIVIL
microdados_gestao_ti %>% 
  select(estado_civil) %>% 
  group_by(estado_civil) %>% 
  summarise(total = n())

#regiao
microdados_gestao_ti %>% 
  select(regiao) %>% 
  group_by(regiao) %>% 
  summarise(total = n())

#sexo
microdados_gestao_ti %>% 
  select(sexo) %>% 
  group_by(sexo) %>% 
  summarise(total = n())

#horas_estudos
microdados_gestao_ti %>% 
  select(horas_estudos) %>% 
  group_by(horas_estudos) %>% 
  summarise(total = n())

#Renda
microdados_gestao_ti %>% 
  select(Renda) %>% 
  group_by(Renda) %>% 
  summarise(total = n())

#turno
microdados_gestao_ti %>% 
  select(turno) %>% 
  group_by(turno) %>% 
  summarise(total = n())

#raca
microdados_gestao_ti %>% 
  select(raca) %>% 
  group_by(raca) %>% 
  summarise(total = n())

#superior
microdados_gestao_ti %>% 
  select(superior) %>% 
  group_by(superior) %>% 
  summarise(total = n())

#NT_OBJ_CE
microdados_gestao_ti %>% 
  select(NT_OBJ_CE) %>% 
  group_by(NT_OBJ_CE) %>% 
  summarise(total = n())
#
#
#
#
#
#média, agrupada por Estado civil
microdados_gestao_ti %>% 
  select(estado_civil,NT_OBJ_FG) %>% 
  group_by(estado_civil) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#regiao
microdados_gestao_ti %>% 
  select(regiao,NT_OBJ_FG) %>% 
  group_by(regiao) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#sexo
microdados_gestao_ti %>% 
  select(sexo,NT_OBJ_FG) %>% 
  group_by(sexo) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#horas_estudos
microdados_gestao_ti %>% 
  select(horas_estudos,NT_OBJ_FG) %>% 
  group_by(horas_estudos) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#Renda
microdados_gestao_ti %>% 
  select(Renda,NT_OBJ_FG) %>% 
  group_by(Renda) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#turno
microdados_gestao_ti %>% 
  select(turno,NT_OBJ_FG) %>% 
  group_by(turno) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#raca
microdados_gestao_ti %>% 
  select(raca,NT_OBJ_FG) %>% 
  group_by(raca) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#superior
microdados_gestao_ti %>% 
  select(superior,NT_OBJ_FG) %>% 
  group_by(superior) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))
#
#
#
#
#
#Verificando (NAÂ´S) em cada variavel
resumo_nas= microdados_gestao_ti %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))

View(resumo_nas)


#Removendo  NAS de todas as variaveis
microdados_gestao_ti_sem_NA=microdados_gestao_ti %>% na.omit()

#verificando se foram corretamente removidas
resumo_nas=microdados_gestao_ti_sem_NA %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))

View(resumo_nas)
#
#
#
#
#
#Quatidade De Linhas Do Banco Original
dim(microdados_gestao_ti)[1]
#Quatidade De Linhas Do Banco sem os NAS
dim(microdados_gestao_ti_sem_NA)[1]
#Total de linhas removidas que continhm NAS
total_linhas_excluidas=dim(microdados_gestao_ti)[1]-dim(microdados_gestao_ti_sem_NA)[1]
#quantidade de linhas excluidas
total_linhas_excluidas
#
#
#
#
#
#Estatasticas descritivas da variavel NOTA
#Calulando o Tamanho do vetor de notas
quantidade_de_notas=length(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
#Calculando a Media
media=mean(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
#Calculando a mediana
#De forma direta
mediana=median(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
#print da medina
mediana
#print da media
media
#Calculando teoricamente
(sort(microdados_gestao_ti_sem_NA$NT_OBJ_CE)[4818]+sort(microdados_gestao_ti_sem_NA$NT_OBJ_CE)[4819])/2
#Moda
#Primeira etapa: Calcular as frequencias simples
fs=table(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
#Calcular o maximo das frequencias simples
maximo=max(fs)
#trazer os nomes que correspondem as observacoes das fs
nomes=names(fs)
#trazer os nomes que satisfazem a comparacao logica
moda_texto=nomes[fs==maximo]
#Transformar em numero
moda_numero=as.numeric(moda_texto)
#print moda
moda_numero
#
moda=Mode(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
#
consolidado_notas=data.frame("Quantidade_de_notas"=quantidade_de_notas,
                             "Media"=media,
                             "Mediana"=mediana,
                             "moda"=moda_numero)
#print dos dados
consolidado_notas
#Para calcular a assimetria:
assimetria=skewness(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
#A curtose do que o R calcula, ? padronizada, tirando -3, comparada a da normal
curtose=kurtosis(microdados_gestao_ti_sem_NA$NT_OBJ_CE)
consolidado_notas_completo=cbind(consolidado_notas,assimetria, curtose)
#print consolidado_notas_completo
consolidado_notas_completo
#pelo R, temos que se 
#k>0, leptocurtica
#k=0, Mesocurtica
#k<0, Platicurtica
#
#
#
#
#
#Consideramos entao platicurtica.
microdados_gestao_ti_sem_NA %>% 
  select(NT_OBJ_CE) %>% 
  summarise(  quantidade=n(),
              media = mean(NT_OBJ_CE),
              mediana = median(NT_OBJ_CE),
              moda=Mode(NT_OBJ_CE),
              cv=sd(NT_OBJ_CE)/media*100,
              assimetria=skewness(NT_OBJ_CE),
              curtose=kurtosis(NT_OBJ_CE)
  ) %>% 
  arrange(desc(mediana))
#
#
#
#
#
#Estatasticas descritivas da variavel QE_I02
#Estatasticas descritivas da variavel QE_I02
#Estatasticas descritivas da variavel QE_I02
#Estatasticas descritivas da variavel QE_I02
#
#Calulando o Tamanho do vetor
quantidade_de_notas=length(microdados_gestao_ti_sem_NA$QE_I02)
#Calculando a Media
media=mean(microdados_gestao_ti_sem_NA$QE_I02)
#Calculando a mediana
#De forma direta
mediana=median(microdados_gestao_ti_sem_NA$QE_I02)
#print da medina
mediana
#print da media
media
#Calculando teoricamente
(sort(microdados_gestao_ti_sem_NA$QE_I02)[4818]+sort(microdados_gestao_ti_sem_NA$QE_I02)[4819])/2
#Moda
#Primeira etapa: Calcular as frequencias simples
fs=table(microdados_gestao_ti_sem_NA$QE_I02)
#Calcular o maximo das frequencias simples
maximo=max(fs)
#trazer os nomes que correspondem as observacoes das fs
nomes=names(fs)
#trazer os nomes que satisfazem a comparacao logica
moda_texto=nomes[fs==maximo]
#Transformar em numero
moda_numero=as.numeric(moda_texto)
#print moda
moda_numero
#
moda=Mode(microdados_gestao_ti_sem_NA$QE_I02)
#
consolidado_QE_I02=data.frame("Quantidade_de_notas"=quantidade_de_notas,
                             "Media"=media,
                             "Mediana"=mediana,
                             "moda"=moda_numero)
#print dos dados
consolidado_notas
#Para calcular a assimetria:
assimetria=skewness(microdados_gestao_ti_sem_NA$QE_I02)
#A curtose do que o R calcula, ? padronizada, tirando -3, comparada a da normal
curtose=kurtosis(microdados_gestao_ti_sem_NA$QE_I02)
consolidado_QE_I02_completo=cbind(consolidado_QE_I02,assimetria, curtose)
#print consolidado_notas_completo
consolidado_QE_I02_completo
#pelo R, temos que se 
#k>0, leptocurtica
#k=0, Mesocurtica
#k<0, Platicurtica

#
#
#
#
#
#Estatasticas descritivas da variavel quantidade_de_TURNO_GRADUACAO
#Estatasticas descritivas da variavel quantidade_de_TURNO_GRADUACAO
#Estatasticas descritivas da variavel quantidade_de_TURNO_GRADUACAO
#Estatasticas descritivas da variavel quantidade_de_TURNO_GRADUACAO
#
#Calulando o Tamanho do vetor
quantidade_de_TURNO_GRADUACAO=length(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
#Calculando a Media
media=mean(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
#Calculando a mediana
#De forma direta
mediana=median(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
#print da medina
mediana
#print da media
media
#Calculando teoricamente
(sort(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)[4818]+sort(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)[4819])/2
#Moda
#Primeira etapa: Calcular as frequencias simples
fs=table(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
#Calcular o maximo das frequencias simples
maximo=max(fs)
#trazer os nomes que correspondem as observacoes das fs
nomes=names(fs)
#trazer os nomes que satisfazem a comparacao logica
moda_texto=nomes[fs==maximo]
#Transformar em numero
moda_numero=as.numeric(moda_texto)
#print moda
moda_numero
#
moda=Mode(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
#
consolidado_TURNO_GRADUACAO=data.frame("Quantidade_de_notas"=quantidade_de_notas,
                             "Media"=media,
                             "Mediana"=mediana,
                             "moda"=moda_numero)
#print dos dados
consolidado_notas
#Para calcular a assimetria:
assimetria=skewness(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
#A curtose do que o R calcula, e padronizada, tirando -3, comparada a da normal
curtose=kurtosis(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO)
consolidado_TURNO_GRADUACAO_completo=cbind(consolidado_TURNO_GRADUACAO,assimetria, curtose)
#print consolidado_notas_completo
consolidado_TURNO_GRADUACAO_completo
#pelo R, temos que se 
#k>0, leptocurtica
#k=0, Mesocurtica
#k<0, Platicurtica
##
##
##
##
##
#criacao de graficos
#GRaFICOS PARA IDENTIFICAR O QUE CONSTANTAMOS
#GRaFICO HISTOGRAMA DA NOTA DOS ALUNOS COM A FREQUeNCIA RELATIVA DAS NOTAS
g_hist=ggplot(microdados_gestao_ti_sem_NA,aes(x=NT_OBJ_CE)) + 
  geom_histogram(color = "black",fill="lightblue",bins =50,aes(y=(..count..)/sum(..count..)))+
  ggtitle("Histograma da nota dos alunos gestao de T.I")+
  xlab("nota") +
  ylab("Frequencia relativa")
g_hist
ggplotly(g)

#GRaFICO HISTOGRAMA DA NOTA DOS ALUNOS COM A FREQUeNCIA RELATIVA CO_TURNO_GRADUACAO
#CO_TURNO_GRADUACAO	Codigo do turno de graduacao	
#1 = Matutino
#2 = Vespertino
#3 = Integral
#4 = Noturno
g_hist=ggplot(microdados_gestao_ti_sem_NA,aes(x=CO_TURNO_GRADUACAO)) + 
  geom_histogram(color = "black",fill="lightblue",bins =5,aes(y=(..count..)/sum(..count..)))+
  ggtitle("Histograma da TURNO dos alunos gestao de T.I")+
  xlab("Turno") +
  ylab("Frequencia relativa")
g_hist
ggplotly(g)

#GRaFICO HISTOGRAMA DA NOTA DOS ALUNOS COM A FREQUeNCIA RELATIVA REGIAO
#CO_REGIAO_CURSO	Codigo da regiÃ£o de funcionamento do curso	
#1 = Norte
#2 = Nordeste
#3 = Sudeste
#4 = Sul
#5 = Centro-Oeste
g_hist=ggplot(microdados_gestao_ti_sem_NA,aes(x=CO_REGIAO_CURSO)) + 
  geom_histogram(color = "black",fill="lightblue",bins =5,aes(y=(..count..)/sum(..count..)))+
  ggtitle("Histograma da regiao dos alunos gestao de T.I")+
  xlab("Turno") +
  ylab("Frequencia relativa")
g_hist
ggplotly(g)
#
#
#
#
#
#GRaFICO DENSIDADE DA NOTA DOS ALUNOS COM A FREQUeNCIA RELATIVA nota
g_densidade=ggplot(microdados_gestao_ti_sem_NA,aes(x=NT_OBJ_CE))+
  geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Curva de densidade da nota dos alunos gestao de T.I") +
  xlab("nota") +
  ylab("FrequEncia relativa")
g_densidade
ggplotly(g_densidade)

#GRaFICO DENSIDADE DA NOTA DOS ALUNOS COM A FREQUeNCIA RELATIVA turno
#CO_TURNO_GRADUACAO	Codigo do turno de graduacao	
#1 = Matutino
#2 = Vespertino
#3 = Integral
#4 = Noturno
g_densidade=ggplot(microdados_gestao_ti_sem_NA,aes(x=CO_TURNO_GRADUACAO))+
  geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Curva de densidade da REGIAO dos alunos gestao de T.I") +
  xlab("turno") +
  ylab("FrequEncia relativa")
g_densidade
ggplotly(g_densidade)

#GRaFICO DENSIDADE DA NOTA DOS ALUNOS COM A FREQUeNCIA RELATIVA curso
#CO_REGIAO_CURSO	Codigo da regiao de funcionamento do curso	
#1 = Norte
#2 = Nordeste
#3 = Sudeste
#4 = Sul
#5 = Centro-Oeste
g_densidade=ggplot(microdados_gestao_ti_sem_NA,aes(x=CO_REGIAO_CURSO))+
  geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Curva de densidade da nota dos alunos gestao de T.I") +
  xlab("regiao") +
  ylab("FrequEncia relativa")
g_densidade
ggplotly(g_densidade)
#
#
#
#
#
#grafico do histograma de densidade das notas dos alunos
g_hist_densidade = ggplot(microdados_gestao_ti_sem_NA,aes(x=NT_OBJ_CE)) + 
  geom_histogram(color = "black",fill="lightblue",bins =50,aes(y=(..count..)/sum(..count..)))+
  geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Histograma e curva de densidade dos alunos gestao de T.I")+
  xlab("nota") +
  ylab("Frequencia relativa")
g_hist_densidade
ggplotly(g_hist_densidade)


#grafico do histograma de densidade das CO_REGIAO_CURSO
#CO_REGIAO_CURSO	Codigo da regiao de funcionamento do curso	
#1 = Norte
#2 = Nordeste
#3 = Sudeste
#4 = Sul
#5 = Centro-Oeste
g_hist_densidade = ggplot(microdados_gestao_ti_sem_NA,aes(x=CO_REGIAO_CURSO)) + 
  geom_histogram(color = "black",fill="lightblue",bins =20,aes(y=(..count..)/sum(..count..)))+
  geom_density(col=2,size = 1, aes(y = 30 * (..count..)/sum(..count..))) +
  ggtitle("Histograma e curva de densidade dos alunos gestao de T.I")+
  xlab("regiao") +
  ylab("Frequencia relativa")
g_hist_densidade
ggplotly(g_hist_densidade)

#grafico do histograma de densidade das notas dos alunos
#CO_TURNO_GRADUACAO	Codigo do turno de graduacao	
#1 = Matutino
#2 = Vespertino
#3 = Integral
#4 = Noturno
g_hist_densidade = ggplot(microdados_gestao_ti_sem_NA,aes(x=CO_TURNO_GRADUACAO)) + 
  geom_histogram(color = "black",fill="lightblue",bins =50,aes(y=(..count..)/sum(..count..)))+
  geom_density(col=2,size = 1, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Histograma e curva de densidade dos alunos gestao de T.I")+
  xlab("turno") +
  ylab("Frequencia relativa")
g_hist_densidade
ggplotly(g_hist_densidade)
##
##
##
##
##
grid.arrange( g_hist,
              g_densidade,
              g_hist_densidade,
              nrow=3,ncol=1)
##
##
##
##
##
#Comparar as medias por sexo e estado civil
microdados_ti_mod2= microdados_gestao_ti_sem_NA %>% 
  select(estado_civil2,NT_GER,sexo) %>% 
  group_by(sexo,estado_civil2) %>% 
  summarise(  quantidade=n(),
              media = mean(NT_GER,na.rm = T),
              mediana = median(NT_GER,na.rm = T),
              cv=sd(NT_GER,na.rm=T)/media*100,
              amplitude_interquartil=IQR(NT_GER)) %>% 
  arrange(desc(mediana))
#Tabulacao cruzada
table(microdados_gestao_ti_sem_NA$estado_civil2,microdados_gestao_ti_sem_NA$sexo)
#Tabulacao cruzada proporcao
prop.table(table(microdados_gestao_ti_sem_NA$estado_civil2,microdados_gestao_ti_sem_NA$sexo))

dados_casados = microdados_gestao_ti_sem_NA %>% 
  select(estado_civil2,NT_GER) %>% 
  group_by(estado_civil2) %>% 
  #filter(estado_civil=="Casado(a)") %>% 
  summarise(  quantidade=n(),
              media = mean(NT_GER),
              mediana = median(NT_GER),
              cv=sd(NT_GER)/media*100,
              amplitude_interquartil=IQR(NT_GER),
              assimetria=skewness(NT_GER),
              curtose=kurtosis(NT_GER)
  ) %>% 
  
  arrange(desc(cv))
#Histograma
dados=microdados_gestao_ti_sem_NA
grafico_histograma1 = ggplot(dados, aes(x=NT_GER,fill=estado_civil2)) + 
  geom_histogram() +
  ggtitle("Grafico histograma da Nota por estado civil") +
  xlab("Notas") +
  ylab("Frequencia simples") +
  facet_grid(~estado_civil2)

ggplotly(grafico_histograma1)

dados=microdados_gestao_ti_sem_NA
grafico_boxplot1 = ggplot(dados, aes(x=estado_civil2,y=NT_GER,fill=estado_civil2)) + 
  geom_boxplot() +
  ggtitle("Grafico de Box-plot da Nota por Estado civil e Sexo")+
  xlab("Estado civil") +
  ylab("Notas") +
  facet_grid(~sexo)

ggplotly(grafico_boxplot1)
#
#
#
#
#
#COMPARACAO DE MICRODADOS POR RACA
#QE_I02	Qual a sua cor ou raÃ§a?	
#A = Branca.
#B = Preta.
#C = Amarela.
#D = Parda.
#E = IndÃ­gena.
#F = NÃ£o quero declarar.
#Comparar as medias por sexo e QE_I02(raca)
microdados_ti_mod3= microdados_gestao_ti_sem_NA %>% 
  select(QE_I02,NT_GER,sexo) %>% 
  group_by(sexo,QE_I02) %>% 
  summarise(  quantidade=n(),
              media = mean(NT_GER,na.rm = T),
              mediana = median(NT_GER,na.rm = T),
              cv=sd(NT_GER,na.rm=T)/media*100,
              amplitude_interquartil=IQR(NT_GER)) %>% 
  arrange(desc(mediana))
#Tabulacao cruzada
table(microdados_gestao_ti_sem_NA$QE_I02,microdados_gestao_ti_sem_NA$sexo)
#Tabulacao cruzada proporcao
prop.table(table(microdados_gestao_ti_sem_NA$QE_I02,microdados_gestao_ti_sem_NA$sexo))

dados_raca = microdados_gestao_ti_sem_NA %>% 
  select(QE_I02,NT_GER) %>% 
  group_by(QE_I02) %>% 
  #filter(estado_civil=="Casado(a)") %>% 
  summarise(  quantidade=n(),
              media = mean(NT_GER),
              mediana = median(NT_GER),
              cv=sd(NT_GER)/media*100,
              amplitude_interquartil=IQR(NT_GER),
              assimetria=skewness(NT_GER),
              curtose=kurtosis(NT_GER)
  ) %>% 
  
  arrange(desc(cv))
#Histograma
dados2=microdados_gestao_ti_sem_NA
grafico_histograma1 = ggplot(dados, aes(x=NT_GER,fill=QE_I02)) + 
  geom_histogram() +
  ggtitle("Grafico histograma da Nota por raca") +
  xlab("Notas") +
  ylab("Frequencia simples") +
  facet_grid(~QE_I02)

ggplotly(grafico_histograma1)

dados2=microdados_gestao_ti_sem_NA
grafico_boxplot1 = ggplot(dados, aes(x=QE_I02,y=NT_GER,fill=QE_I02)) + 
  geom_boxplot() +
  ggtitle("Grafico de Box-plot da Nota por raca e Sexo")+
  xlab("Estado civil") +
  ylab("Notas") +
  facet_grid(~sexo)

ggplotly(grafico_boxplot1)
#
#
#
#
#

#Comparar as medias por sexo e regiao
microdados_ti_mod3= microdados_gestao_ti_sem_NA %>% 
  select(estado_civil2,NT_GER,regiao,horas_estudos,sexo) %>% 
  group_by(sexo,regiao) %>% 
  summarise(quantidade=n(),
            media = mean(NT_GER),
            mediana = median(NT_GER),
            cv=sd(NT_GER)/media*100,
            amplitude_interquartil=IQR(NT_GER),
            assimetria=skewness(NT_GER),
            curtose=kurtosis(NT_GER)) %>% 
  arrange(desc(media))
#Tabulacao cruzada
table(microdados_gestao_ti_sem_NA$regiao,microdados_gestao_ti_sem_NA$sexo)
#Tabulacao cruzada propor??o
prop.table(table(microdados_gestao_ti_sem_NA$regiao,microdados_gestao_ti_sem_NA$sexo))

#Histograma
dados=microdados_gestao_ti_sem_NA
grafico_histograma2 = ggplot(dados, aes(x=NT_GER,fill=regiao)) + 
  geom_histogram()+
  ggtitle("Grafico histograma da Nota por regieo e sexo" )+
  xlab("Notas") +
  ylab("Frequencia simples") +
  facet_grid(~sexo)

ggplotly(grafico_histograma2)

#box-plot
dados=microdados_gestao_ti_sem_NA
grafico_boxplot2 = ggplot(dados, aes(x=regiao,y=NT_GER,fill=regiao)) + 
  geom_boxplot() +
  ggtitle("Grafico boxplot da Nota por regiao e sexo")+
  ylab("Notas") +
  facet_grid(~sexo)

ggplotly(grafico_boxplot2)

grid.arrange( grafico_histograma1,
              grafico_boxplot1,
              grafico_histograma2,
              grafico_boxplot2,
              nrow=2,ncol=2)
#
#
#
#
#

#COMPARACAO DE MICRODADOS POR CO_TURNO_GRADUACAO
#Comparar as medias por nota e CO_TURNO_GRADUACAO
microdados_ti_mod4= microdados_gestao_ti_sem_NA %>% 
  select(CO_TURNO_GRADUACAO,NT_GER,sexo) %>% 
  group_by(sexo,CO_TURNO_GRADUACAO) %>% 
  summarise(  quantidade=n(),
              media = mean(NT_GER,na.rm = T),
              mediana = median(NT_GER,na.rm = T),
              cv=sd(NT_GER,na.rm=T)/media*100,
              amplitude_interquartil=IQR(NT_GER)) %>% 
  arrange(desc(mediana))
#Tabulacao cruzada
table(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO,microdados_gestao_ti_sem_NA$sexo)
#Tabulacao cruzada proporcao
prop.table(table(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO,microdados_gestao_ti_sem_NA$sexo))

dados_CO_TURNO_GRADUACAO = microdados_gestao_ti_sem_NA %>% 
  select(CO_TURNO_GRADUACAO,NT_GER) %>% 
  group_by(CO_TURNO_GRADUACAO) %>% 
  #filter(estado_civil=="Casado(a)") %>% 
  summarise(  quantidade=n(),
              media = mean(NT_GER),
              mediana = median(NT_GER),
              cv=sd(NT_GER)/media*100,
              amplitude_interquartil=IQR(NT_GER),
              assimetria=skewness(NT_GER),
              curtose=kurtosis(NT_GER)
  ) %>% 
  
  arrange(desc(cv))
#Histograma
dados3=microdados_gestao_ti_sem_NA
grafico_histograma1 = ggplot(dados, aes(x=NT_GER,fill=CO_TURNO_GRADUACAO)) + 
  geom_histogram() +
  ggtitle("Grafico histograma da Nota por CO_TURNO_GRADUACAO") +
  xlab("Notas") +
  ylab("Frequencia simples") +
  facet_grid(~CO_TURNO_GRADUACAO)

ggplotly(grafico_histograma1)

#
#
#
#
#

#Comparar as medias por sexo e regiao
microdados_ti_mod4= microdados_gestao_ti_sem_NA %>% 
  select(CO_TURNO_GRADUACAO,NT_GER,regiao,horas_estudos,sexo) %>% 
  group_by(sexo,CO_TURNO_GRADUACAO) %>% 
  summarise(quantidade=n(),
            media = mean(NT_GER),
            mediana = median(NT_GER),
            cv=sd(NT_GER)/media*100,
            amplitude_interquartil=IQR(NT_GER),
            assimetria=skewness(NT_GER),
            curtose=kurtosis(NT_GER)) %>% 
  arrange(desc(media))
#Tabulacao cruzada
table(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO,microdados_gestao_ti_sem_NA$sexo)
#Tabulacao cruzada propor??o
prop.table(table(microdados_gestao_ti_sem_NA$CO_TURNO_GRADUACAO,microdados_gestao_ti_sem_NA$sexo))

#Histograma
dados=microdados_gestao_ti_sem_NA
grafico_histograma2 = ggplot(dados, aes(x=NT_GER,fill=CO_TURNO_GRADUACAO)) + 
  geom_histogram()+
  ggtitle("Grafico histograma da Nota por CO_TURNO_GRADUACAO e sexo" )+
  xlab("Notas") +
  ylab("Frequencia simples") +
  facet_grid(~sexo)

ggplotly(grafico_histograma2)

#box-plot
dados=microdados_gestao_ti_sem_NA
grafico_boxplot2 = ggplot(dados, aes(x=regiao,y=NT_GER,fill=CO_TURNO_GRADUACAO)) + 
  geom_boxplot() +
  ggtitle("Grafico boxplot da Nota por CO_TURNO_GRADUACAO e sexo")+
  ylab("Notas") +
  facet_grid(~sexo)

ggplotly(grafico_boxplot2)

grid.arrange( grafico_histograma1,
              grafico_boxplot1,
              grafico_histograma2,
              grafico_boxplot2,
              nrow=2,ncol=2)
#
#
#
#
#
#BLOCO GRaFICOS POINT CLICK POR MEIO DO ESQUISSE

#Analises graficas point click
microdados_ti_mod= microdados_gestao_ti_sem_NA %>% 
  select(estado_civil2,NT_OBJ_FG,regiao,horas_estudos,sexo) %>% 
  group_by(estado_civil2,regiao,horas_estudos,sexo) %>% 
  summarise(media = mean(NT_OBJ_FG,na.rm = T))

#devtools::install_github("dreamRs/esquisse")
esquisser(viewer = "browser")


##Exemplo: Há razão para desconfiar que pessoas centro-oeste têm melhores notas que nordeste?  	
## em verdade seria o contrario pessoas nordeste tem notas melhores que pessoas centro-este analisando grafico, a mediana ## do grupo de pessoas amarelas encontra-se mais elevadas que a de pessoas brancas e também a possição em relação a ## frequencia de notas.
##
##Exemplo: Há razão para desconfiar que pessoas brancas têm melhores notas que amarelos?  (analisando branco, amarelo ## e negro)	
## Homens brancos tem melhores notas que negros em termos geral assim como, mediana acima em relacao a frequencia 
## relativa, porém em relacao aos amarelos existe maiores intervalos de notas maiores que brancos, porém a mediana de
## homens brancos esta acima da mediana de homens amarelos identificando maior frequencia acima de conjunto de notas.
##
##
##
#Exemplo: Há razão para desconfiar que pessoas que estudam no turno da manhã têm notas maiores que os que estudam à noite?  	
##
###1 = Matutino
#2 = Vespertino
#3 = Integral
#4 = Noturno	
## analisando o histograma dos turnos em relacao as notas existe uma maior curvatura e distribuicao em maior ## quantidade em relacao as pessoas que estudando no turno da noite de uma forma mais normalizada, porem
## pessoas que estudam no vespertido, sendo esse tendo pouca distribuicao e aparente maior concentracao em notas ## abaixo de 50 enquanto existe a crescente curvatura em diracao a 50 ou mais em turno noturno. Em relacao ao turno 3 ## Integral existe uma maior curvatura inicial e maiores picos em montantes iniciais abaixo de 50 achatando a direita ## com notas maiores. Turno 1 matutino possui distriuicao bastante disforme e irregular.
##
##
##
##
#Exemplo: Há razão para desconfiar que pessoas qualquer cor e sexo destacam-se de alguma maneira? (amarelas e brancas)
###QE_I02	Qual a sua cor ou raÃ§a?	
#A = Branca.
#B = Preta.
#C = Amarela.
#D = Parda.
#E = IndÃ­gena.
#F = NÃ£o quero declarar.
## ## analiosando grafico de nota por raca e sexo entre os homens brancos possuem uma maior taxa de frequencia na mediana 
## acima das outras notas, porem em relacao aos amarelos existem maiores indices com maiores notas, porem a frequencia esta abaixo
## da mediana de brancos.
## Entre as mulhere brancas existe numa situacao parecia, estando com um pontuacao acima, porem embora as notas das mulheres brancas
## seja mais elevada e estaja distribuida de maneira central as mulheres pardas possuem uma mediana acima do montante geral
## 
#Exemplo: Há razão para desconfiar que pessoas pretas da região sul têm notas maiores que pessoas pretas da região norte?	
## De fato pelas mediana e entre os homens e a frequencia relativa (entre homens), assim como entre as mulheres existe uma tendencia de ambos terem melhores valores de notas
##
##
##
##