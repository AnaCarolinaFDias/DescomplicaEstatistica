##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Definindo limite de memoria para compilacao do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=20000)
options(warn=aviso)
rm(aviso)

# Definindo opcao de codificacao dos caracteres e linguagem
aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo opcao de exibicao de numeros sem exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo opcao de repositorio para instalacao dos pacotes necessarios
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Definindo area de trabalho
caminho <- getwd()
setwd(caminho)

# Carregando informacoes do pacote PNADcIBGE
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNADcIBGE", dependencies=TRUE)
}
library("PNADcIBGE")
packageDescription("PNADcIBGE")
help(package="PNADcIBGE")

# Descrevendo as funcoes do pacote PNADcIBGE
get_pnadc
read_pnadc
pnadc_labeller
pnadc_deflator
pnadc_design
pnadc_example

# Gerando micrdados da PNAD Continua pelo metodo manual
variaveis_selecionadas <- c("V2007", "V2009", "V2010", "VD3004", "VD4019", "VD4031")
input_path <- pnadc_example(path="input_example.txt")
data_path <- pnadc_example(path="exampledata.txt")
dictionary.path <- pnadc_example(path="dictionaryexample.xls")
deflator.path <- pnadc_example(path="deflatorexample.xls")
dadosPNADc <- read_pnadc(microdata=data_path, input_txt=input_path, vars=variaveis_selecionadas)
dadosPNADc <- pnadc_labeller(data_pnadc=dadosPNADc, dictionary.file=dictionary.path)
dadosPNADc <- pnadc_deflator(data_pnadc=dadosPNADc, deflator.file=deflator.path)
dadosPNADc <- pnadc_design(data_pnadc=dadosPNADc)
View(dadosPNADc)

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Gerando microdados da PNAD Continua pelo metodo automatico
variaveis_selecionadas <- c("V2007", "V2009", "V2010", "VD3004", "VD4019", "VD4031")
dadosPNADc <- get_pnadc(year=2018, quarter=3, labels=TRUE, deflator=TRUE, design=FALSE, vars=variaveis_selecionadas)

# Obtendo total e media da renda mensal habitual para a amostra da base de coleta
sum(x=dadosPNADc$VD4019, na.rm=TRUE)
mean(x=dadosPNADc$VD4019, na.rm=TRUE)

# Obtendo total e proporcao de sexo para a amostra da base de coleta
table(dadosPNADc$V2007)
prop.table(table(dadosPNADc$V2007))

# Aplicando a incorporacao do desenho amostral nos microdados
dadosPNADc <- pnadc_design(data_pnadc=dadosPNADc)

# Carregando informacoes do pacote survey
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("survey")
}
library("survey")

# Estimando total da renda mensal habitual
totalrenda <- svytotal(x=~VD4019, design=dadosPNADc, na.rm=TRUE)
totalrenda

# Exibindo coeficiente de variacao e intervalo de confianca
cv(totalrenda)
confint(totalrenda)

# Estimando total de sexo e cor ou raca
totalsexoraca <- svytotal(x=~V2007 + V2010, design=dadosPNADc, na.rm=TRUE)
totalsexoraca

# Estimando media da renda mensal habitual
mediarenda <- svymean(x=~VD4019, design=dadosPNADc, na.rm=TRUE)
mediarenda

# Estimando proporcao de sexo
propsexo <- svymean(x=~V2007, design=dadosPNADc, na.rm=TRUE)
propsexo

# Estimando mediana e quantis da renda mensal habitual
medianarenda <- svyquantile(x=~VD4019, design=dadosPNADc, quantiles=c(0.25, 0.5, 0.75, 0.9, 0.95, 0.98), na.rm=TRUE)
medianarenda

# Estimando a renda media para mulheres acima de 30 anos
rendaM30 <- svymean(x=~VD4019, design=subset(dadosPNADc, V2007 == "Mulher" & V2009 > 30), na.rm=TRUE)
rendaM30

# Estimando a frequencia relativa de homens e mulheres em cada nivel de instrucao
freqSexoInstr <- svyby(formula=~V2007, by=~VD3004, design=dadosPNADc, FUN=svymean, na.rm=TRUE, vartype=NULL)
print(freqSexoInstr, row.names=FALSE)

# Esbocando boxplot do numero de horas trabalhadas por sexo
svyboxplot(formula=VD4031 ~ V2007, design=dadosPNADc, all.outliers=TRUE)

# Esbocando histograma da distribuicao de frequencia do numero de horas trabalhadas
svyhist(formula=~as.numeric(VD4031), design=dadosPNADc, main="Histograma", xlab="Número de Horas Trabalhadas", ylab="Densidade")

# Esbocando grafico de dispersao entre numero de horas trabalhadas e renda mensal habitual
svyplot(formula=VD4019 ~ VD4031, design=dadosPNADc, style="transparent", xlab="Horas habitualmente trabalhadas", ylab="Rendimento habitual")

# Realizando teste t de student para diferenca de rendimentos entre sexos
svyttest(formula=VD4019 ~ V2007, design=dadosPNADc)

# Carregando microdados da PNAD Continua dos topicos suplementares anuais
PNADcAnual_Visita <- get_pnadc(year=2018, interview=1, labels=TRUE, deflator=TRUE, design=TRUE, defyear=2019)
PNADcAnual_Trimestre <- get_pnadc(year=2018, topic=2, labels=TRUE, deflator=TRUE, design=TRUE, defyear=2019, defperiod=2)

##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Carregando informacoes do pacote PNSIBGE
if("PNSIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNSIBGE", dependencies=TRUE)
}
library("PNSIBGE")
packageDescription("PNSIBGE")
help(package="PNSIBGE")

# Descrevendo as funcoes do pacote PNSIBGE
get_pns
read_pns
pns_labeller
pns_deflator
pns_design
pns_example

# Carregando microdados da PNS
dadosPNS_2013 <- get_pns(year=2013, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNS_2013_Selecionado <- get_pns(year=2013, selected=TRUE, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNS_2019 <- get_pns(year=2019, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNS_2019_Selecionado <- get_pns(year=2019, selected=TRUE, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNS_2019_Antropometria <- get_pns(year=2019, anthropometry=TRUE, labels=TRUE, deflator=TRUE, design=TRUE)
load("Microdados IBGE - Bases PNSIBGE.RData")

# Ajustando desenho amostral para estratos que possuem observacoes unicas
aviso <- getOption("warn")
options(warn=-1)
options(survey.lonely.psu="adjust")
options(warn=aviso)
rm(aviso)

# Obtendo proporcao de diagnostico de doenca cronica na PNS 2013 e na PNS 2019
DoenCron_2013 <- svymean(x=~J007, design=dadosPNS_2013, na.rm=TRUE)
DoenCron_2013
cv(DoenCron_2013)
confint(DoenCron_2013)
DoenCron_2019 <- svymean(x=~J007, design=dadosPNS_2019, na.rm=TRUE)
DoenCron_2019
cv(DoenCron_2019)
confint(DoenCron_2019)

# Obtendo proporcao de avaliacao do estado de saude na PNS 2013 e na PNS 2019
AvalSaude_2013 <- svymean(x=~N001, design=dadosPNS_2013_Selecionado, na.rm=TRUE)
AvalSaude_2013
cv(AvalSaude_2013)
confint(AvalSaude_2013)
AvalSaude_2019 <- svymean(x=~N001, design=dadosPNS_2019_Selecionado, na.rm=TRUE)
AvalSaude_2019
cv(AvalSaude_2019)
confint(AvalSaude_2019)

# Obtendo media de peso e altura na PNS 2013 e na PNS 2019
MediaPeso_2013 <- svymean(x=~W00101, design=dadosPNS_2013_Selecionado, na.rm=TRUE)
MediaPeso_2013
cv(MediaPeso_2013)
confint(MediaPeso_2013)
MediaPeso_2019 <- svymean(x=~W00101, design=dadosPNS_2019_Antropometria, na.rm=TRUE)
MediaPeso_2019
cv(MediaPeso_2019)
confint(MediaPeso_2019)
MediaAltura_2013 <- svymean(x=~W00201, design=dadosPNS_2013_Selecionado, na.rm=TRUE)
MediaAltura_2013
cv(MediaAltura_2013)
confint(MediaAltura_2013)
MediaAltura_2019 <- svymean(x=~W00201, design=dadosPNS_2019_Antropometria, na.rm=TRUE)
MediaAltura_2019
cv(MediaAltura_2019)
confint(MediaAltura_2019)

##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Carregando informacoes do pacote COVIDIBGE
if("COVIDIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("COVIDIBGE", dependencies=TRUE)
}
library("COVIDIBGE")
packageDescription("COVIDIBGE")
help(package="COVIDIBGE")

# Descrevendo as funcoes do pacote COVIDIBGE
get_covid
read_covid
covid_labeller
covid_deflator
covid_design
covid_example

# Carregando microdados da PNAD COVID19
dadosPNADCOVID19_052020 <- get_covid(year=2020, month=5, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADCOVID19_062020 <- get_covid(year=2020, month=6, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADCOVID19_072020 <- get_covid(year=2020, month=7, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADCOVID19_082020 <- get_covid(year=2020, month=8, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADCOVID19_092020 <- get_covid(year=2020, month=9, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADCOVID19_102020 <- get_covid(year=2020, month=10, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADCOVID19_112020 <- get_covid(year=2020, month=11, labels=TRUE, deflator=TRUE, design=TRUE)
load("Microdados IBGE - Bases COVIDIBGE.RData")

# Analisando proporcao de pessoas afastadas do trabalho nos meses de coleta da PNAD COVID19
propafast_052020 <- svymean(x=~C002, design=dadosPNADCOVID19_052020, na.rm=TRUE)
propafast_062020 <- svymean(x=~C002, design=dadosPNADCOVID19_072020, na.rm=TRUE)
propafast_072020 <- svymean(x=~C002, design=dadosPNADCOVID19_062020, na.rm=TRUE)
propafast_082020 <- svymean(x=~C002, design=dadosPNADCOVID19_082020, na.rm=TRUE)
propafast_092020 <- svymean(x=~C002, design=dadosPNADCOVID19_092020, na.rm=TRUE)
propafast_102020 <- svymean(x=~C002, design=dadosPNADCOVID19_102020, na.rm=TRUE)
propafast_112020 <- svymean(x=~C002, design=dadosPNADCOVID19_112020, na.rm=TRUE)

# Criando base com informacoes de pessoas afastadas
propafast <- matrix(0, nrow=7, ncol=2)
propafast[1,] <- propafast_052020
propafast[2,] <- propafast_062020
propafast[3,] <- propafast_072020
propafast[4,] <- propafast_082020
propafast[5,] <- propafast_092020
propafast[6,] <- propafast_102020
propafast[7,] <- propafast_112020
propafast <- as.data.frame(propafast)
propafast <- round(propafast*100, digits=2)
colnames(propafast) <- c("Sim","Nao")
rownames(propafast) <- c("Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro")
View(propafast)

# Carregando informacoes do pacote ggplot2
if("ggplot2" %in% rownames(installed.packages())==FALSE)
{
  install.packages("ggplot2")
}
library("ggplot2")

# Carregando informacoes do pacote reshape2
if("reshape2" %in% rownames(installed.packages())==FALSE)
{
  install.packages("reshape2")
}
library("reshape2")

# Gerando grafico do percentual de pessoas afastadas do trabalho
propafast$Mes <- rownames(propafast)
propafast$Mes <- factor(propafast$Mes, levels=c("Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro"))
propafast <- melt(data=propafast, id.vars="Mes", value.name="Percentual", variable.name="Afastadas")
grafico_propafast <- ggplot(subset(propafast, Afastadas=="Sim"), aes(x=Mes, y=Percentual, group=Afastadas)) +
  geom_text(aes(label=Percentual), vjust=1, hjust=1, size=3, show.legend=FALSE) +
  geom_line(aes(color=Afastadas)) +
  geom_point(aes(color=Afastadas)) +
  labs(title="Percentual de Pessoas Afastadas do Trabalho por Mes - PNAD COVID19") +
  theme(plot.title=element_text(hjust=0.5))
x11()
grafico_propafast

##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Carregando informacoes do pacote POFIBGE
if("POFIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("POFIBGE", dependencies=TRUE)
}
library("POFIBGE")
packageDescription("POFIBGE")
help(package="POFIBGE")

# Descrevendo as funcoes do pacote POFIBGE
get_pof
read_pof
pof_labeller
pof_deflator
pof_design
pof_example

# Carregando microdados da POF
dadosPOF_2008 <- get_pof(year=2008, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPOF_2017 <- get_pof(year=2017, labels=TRUE, deflator=TRUE, design=TRUE)

##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Carregando informacoes do pacote SIPDIBGE
if("SIPDIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("SIPDIBGE", dependencies=TRUE)
}
library("SIPDIBGE")
packageDescription("SIPDIBGE")
help(package="SIPDIBGE")

# Descrevendo as funcoes do pacote SIPDIBGE
sipd_conflicts
sipd_deps
sipd_logo
sipd_packages
sipd_sitrep
sipd_update

# Indicando se existe conflito das funcoes dos pacotes do SIPDIBGE com outros pacotes
sipd_conflicts()

# Indicando pacotes de dependencia dos pacotes do SIPDIBGE
sipd_deps()

# Mostrando logos das pesquisas relacionadas aos pacotes do SIPDIBGE
sipd_logo()

# Indicando pacotes que fazem parte do ambito do pacote SIPDIBGE
sipd_packages()

# Indicando informacoes de versoes dos pacotes do SIPDIBGE
sipd_sitrep()

# Indicando atualizacoes sobre as versoes dos pacotes do SIPDIBGE
sipd_update()

##############################################################################################################################