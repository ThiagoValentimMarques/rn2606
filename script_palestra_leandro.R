
######################### Construção da Linha do Tempo ######################

# Mudar o diretório de trabalho

setwd("C:\\Users\\Thiago\\Desktop\\Palestra - Instagram do Prof. Leandro\\conjuntos de dados\\Ministério da Saúde")

#### Leitura dos dados

library(tidyverse)

#Local da pasta
caminho <- "C:\\Users\\Thiago\\Desktop\\Palestra - Instagram do Prof. Leandro\\conjuntos de dados\\Linha do Tempo\\linhadotempo.csv" 
tabela <- read.csv(caminho,header=TRUE,sep=";")

tabela <- tabela %>%
  mutate(acontecimento = as.character(acontecimento),
         cor = c(rep(NA,23),rep(0,31),rep(NA,29),rep(0,31),rep(NA,30),
                 rep(0,31),rep(NA,17)))
ord <- c(6,-3,4,-7,9,-5,6,-2,3,-8,7,-5,5,-3,9,-1.3,2,-7.2,-9,6,
         -6,3,-2,10,-3,-6.5,5,-8,8,-5,9,-1,3)
tabela2<-tabela[complete.cases(tabela$acontecimento),]
tabela2[,6]<-as.numeric(row.names(tabela2))
names(tabela2)[6]<-"linha"

par(mar=c(0,0,0,0),xpd=TRUE)
plot(NA,xlim=c(-10,dim(tabela)[1])+10,ylim=c(-10,10),ann=FALSE,axes=FALSE)
segments(tabela2$dias,0,tabela2$dias,ord,col="blue",lty=10)


points(tabela2$linha,ord,col="darkblue",lty=10,pch=16)
lines(tabela$dias,rep(0,dim(tabela)[1]),lwd=4,col="red4",type="l")
lines(tabela$dias,tabela$cor,lwd=4,col="darkolivegreen1",
      xlim=c(1,dim(tabela)[1]))

meses<-c("dez/2019","jan/2020","fev/2020","mar/2020","abr/2020","mai/2020",
         "jun/2020")
x<-tapply(tabela$dias,tabela$mes,mean)
text(x[7],0.3,labels=meses[1],cex=0.9,font=2)
text(x[1],-0.3,labels=meses[2],cex=0.9,font=2)
text(x[2],0.3,labels=meses[3],cex=0.9,font=2)
text(x[3],-0.3,labels=meses[4],cex=0.9,font=2)
text(x[4],0.3,labels=meses[5],cex=0.9,font=2)
text(x[5],-0.3,labels=meses[6],cex=0.9,font=2)
text(x[6],0.3,labels=meses[7],cex=0.9,font=2)



for(i in 1:dim(tabela2)[1]){
  text(tabela2$linha[i],y=ord[i],
       labels=paste(strwrap(tabela2$acontecimento[i],width=35),
                    collapse="\n"),pos=1,cex=1.0,font=2)
}

#Salvar em pdf 8 x 18 inches

############################################################################
############### JOHNS HOPKINS UNIVERSITY ###################################
############################################################################

#-------------------------------- Códigos em R ---------------------------------#

#########################################################
#---------- Pacotes necessários para a análise ---------#
#########################################################

library(tidyverse) #ggplot2, dplyr, ...
library(ggrepel) #Labels em retângulos
library(zoo) #Médias móveis
library(gridExtra)

#########################################################
#------------------ URL dos dados ----------------------#
#########################################################

# Casos confirmados
url1 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv" 

# Óbitos 
url2 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"

#########################################################
#-------------- Preparação dos dados -------------------#
#########################################################

casos <- read.csv(url1,header=TRUE)
obitos <- read.csv(url2,header=TRUE)

# Países que desejo fazer a análise
paises <- c("Brazil","Italy","Germany","US","Mexico",'Spain',
            "New Zealand","Korea, South","India","Chile","Peru") 

# Nomemclatura que serão exibidas nas análises
sel <- c("Brasil","Itália","Alemanha","EUA","México","Espanha",
         "Nova Zelândia","Coréia do Sul","Índia","Chile","Peru")

# População dos respectivos países
pop <- c(212521080,60463697,83777677,330954637,128896013,46754380,
         5002100,51268205,1379641143,19111913,32963598)

# Testes para 1 milhão de pessoas
teste1M <- c(13452,85394,64605,92859,4101,110426,75620,
             24036,5636,53631,48041) #Fonte: worldometers

##############################################################################
################ Início da rotina para os casos ##############################
##############################################################################

casos <- casos %>%
  filter(Country.Region %in% paises)

n<-dim(casos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  valor <- as.vector(apply(casos[casos$Country.Region==i,-c(1,2,3,4)],2,sum))
  if(names(table(valor))[1]=="0"){
    cont<-table(valor)[1]
    valor<-valor[-c(1:cont)]
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    valor[(length(valor)+1):(length(valor)+cont)]<-rep("NA",cont)
    last_point[(length(last_point)+1):(length(last_point)+
                                         cont)]<-rep(NA_character_,cont)
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }else{
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }
  j<-j+1
  matriz[,j]<-valor
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
} 

point<-as.vector(matriz2)
casos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
logcasos <- log10(casos)
propcasos100k <- 100000*casos/rep(pop,each=n)
propdia1m <- 1000000*diario/rep(pop,each=n)
data <- seq(as.Date("2020/02/26"), by = "day", length.out = n)
data <- rep(data,length(paises))
data <- substr(data,6,10)
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
corona <- data.frame(data,dia,País,casos,logcasos,propcasos100k,point,
                     diario,propdia1m)
corona <- as.tibble(corona)

##############################################################################
################# Final da rotina para os casos ##############################
##############################################################################

#########################################################################
######################### Rotina para os óbitos #########################
#########################################################################


obitos <- obitos %>%
  filter(Country.Region %in% paises)

n<-dim(obitos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  valor <- as.vector(apply(obitos[obitos$Country.Region==i,-c(1,2,3,4)],2,sum))
  if(names(table(valor))[1]=="0"){
    cont<-table(valor)[1]
    valor<-valor[-c(1:cont)]
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    valor[(length(valor)+1):(length(valor)+cont)]<-rep("NA",cont)
    last_point[(length(last_point)+1):(length(last_point)+
                                         cont)]<-rep(NA_character_,cont)
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }else{
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }
  j<-j+1
  matriz[,j]<-valor
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
} 

point<-as.vector(matriz2)
obitos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
logobitos <- log10(obitos)
propobt100k <- 100000*obitos/rep(pop,each=n)
propdiaobt1m <- 1000000*diario/rep(pop,each=n)
data <- seq(as.Date("2020/02/26"), by = "day", length.out = n)
data <- rep(data,length(paises))
data <- substr(data,6,10)
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
cor_obt <- data.frame(data,dia,País,obitos,logobitos,propobt100k,point,
                      diario,propdiaobt1m)
cor_obt <- as.tibble(cor_obt)

##############################################################################
################# Final da rotina para os óbitos #############################
##############################################################################

require(plotly)
##### Mundo 0 (casos)

a<-corona%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México"))%>%
  ggplot(.,aes(x=dia,y=casos))+ geom_line(size=0.8)+
  # geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Casos registrados")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos de Covid-19",
       caption=" ",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

ggplotly(a)  #Visualização dinâmica

####### Mundo 0 (óbitos)

#point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

b<-cor_obt%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México"))%>%
  ggplot(.,aes(x=dia,y=obitos))+ geom_line(size=0.8)+
  #geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Óbitos registrados")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Acumulado de óbitos por Covid-19",
       caption="Fonte: Johns Hopkins University    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

grid.arrange(a,b,nrow=1)
#salvar em pdf 9 x 14 inches

ggplotly(b)  #Visualização dinâmica

##### Mundo 1 (casos)

c<-corona%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México"))%>%
  ggplot(.,aes(x=dia,y=diario))+ geom_line(size=0.8)+
 # geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Casos novos")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Casos novos de Covid-19",
       caption=" ",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

ggplotly(c) #Visualização dinâmica

####### Mundo 1 (óbitos)

#point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

d<-cor_obt%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México"))%>%
  ggplot(.,aes(x=dia,y=diario))+ geom_line(size=0.8)+
  #geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
    ylab("Óbitos novos")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Óbitos novos por Covid-19",
       caption="Fonte: Johns Hopkins University    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

grid.arrange(c,d,nrow=1)
#salvar em pdf 9 x 14 inches

ggplotly(d) #Visualização dinâmica

##### Mundo 2 (porporção de casos)

e<-ggplot(corona,aes(x=dia,y=propcasos100k,group=País,colour=País))+
  geom_line(size=1.2)+
  ylab("Casos confirmados para cada 100 mil habitante")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos confirmados por Covid-19 (incidência)",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="right", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]+10))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")
#salvar em pdf 6 x 8 inches

ggplotly(e)

##### Mundo 3 (porporção de óbitos)

f<-ggplot(cor_obt,aes(x=dia,y=propobt100k,group=País,colour=País))+
  geom_line(size=1.2)+
  ylab("Óbitos confirmados para cada 100 mil habitantes")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Acumulado de óbitos por Covid-19 (mortalidade)",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="right", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]-25))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

ggplotly(f)

##### Mundo 4 (Testes e Letalidade)

letalidade <- NULL
for(i in sel){
  a<-max(corona[corona$País==i,]$casos,na.rm=TRUE)
  b<- max(cor_obt[cor_obt$País==i,]$obitos,na.rm=TRUE)
  letalidade[i]<-round(100*b/a,2)
}

let <- as.vector(letalidade)
letalidade <- data.frame(let,sel,teste1M)

p1<-ggplot(letalidade, aes(x=reorder(substr(sel,1,3),desc(let)), y=let,fill=substr(sel,1,3))) + geom_col()+
  ylab("Letalidade (%)")+xlab("Países")+ 
  labs(title="Letalidade da Covid-19",
       caption= " ",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

ggplotly(p1)

p2<-ggplot(letalidade, aes(x=reorder(substr(sel,1,3),desc(teste1M)), y=teste1M,fill=substr(sel,1,3))) + geom_col()+
  ylab("Testes por 1 mihão de pessoas")+xlab("Países")+ 
  labs(title="Testes de Covid-19",
       caption="Fonte: Johns Hopkins University e Worldometer   Autor: Thiago Valentim",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(p1, p2, nrow = 1)

ggplotly(p2)

#salvar em pdf 5 x 9 inches

############################################################################
######################## Nordeste ##########################################
############################################################################

library(tidyverse)
library(brazilmaps)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(ggspatial)
library(scales) #datas

########## INFORMAÇÕES DA POPULAÇÃO DE CADA ESTADO DO NORDESTE ###################

dados <- read.csv("COVIDBR_25jun2020.csv",header=TRUE,sep=";")

dados <- as_tibble(dados)

ne <- dados %>%
  filter(estado %in% c("RN","PB","PE","SE","MA","CE","BA","PI","AL"), is.na(codmun))%>%
  mutate(data = substr(data,1,5))

dia <- factor(rep(c("TER","QUA","QUI","SEX","SAB",
                rep(c("DOM","SEG","TER","QUA","QUI","SEX","SAB"),16),
                "DOM","SEG","TER","QUA","QUI"),9),
              levels=c("DOM","SEG","TER","QUA","QUI","SEX","SAB"))


nordeste <- rep(c("RN","PB","PE","SE","MA","CE","BA","PI","AL"),each=n)

## População estimada 2019 (IBGE)

pop_ne <- rep(c(3506853,4018127,9557071,2298696,7075181,9132078,14873064,3273227,
                3337357),each=n)

ne[,18] <- dia
names(ne)[18] <- "dia"

ne[,19]<-rep(seq(as.Date("25/02/2020",format="%d/%m/%y"),
             as.Date("25/06/2020",format="%d/%m/%y"),
             by=1),9)
names(ne)[19] <- "datapt"

### Função para inserir a média em facet_wrap

StatMeanLine <- ggproto("StatMeanLine", Stat,
                        compute_group = function(data, scales) {
                          transform(data, yintercept=mean(y))
                        },
                        required_aes = c("x", "y")
)

stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


hone <- "25/06" #Informar a data de hoje dd/mm

ne <- ne %>%   
  mutate(label = if_else(data == hoje,
                         as.character(estado), NA_character_),
         propcasos = (casosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000,
         propobitos = (obitosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000)


############################

####### Nordeste

#### NE 1 (casos)
g<-ne %>%
  filter(datapt > "2020-03-30")%>%
ggplot(.,aes(x=datapt,y=casosAcumulado,group=estado,
              colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Acumulados de casos de Covid-19 por estado",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="right", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

ggplotly(g)

#salvar em pdf 6 x 8 inches

#### NE 2 (óbitos)
h<-ne %>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=obitosAcumulado,group=estado,
               colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados", colour = "Estado",
       title="Acumulados de óbitos por Covid-19",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="right", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

ggplotly(h)

#salvar em pdf 6 x 8 inches

#### NE 3 (casos proporcional)
i<-ne %>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=propcasos,group=estado,
               colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados por 100 mil habitantes", colour = "Estado",
       title="Acumulados de casos de Covid-19 por estado (incidência)",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="right", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

ggplotly(i)

#### NE 4 (óbitos proporcional)
j<-ne %>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=propobitos,group=estado,
               colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados por 100 mil habitantes", colour = "Estado",
       title="Acumulados de óbitos por Covid-19 (mortalidade)",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="right", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")
#salvar em pdf 6 x 8 inches

ggplotly(j)

#### NE 5 (a - casos no RN)
p3<-ne %>%
  filter(estado == "RN")%>%
  filter(datapt > "2020-03-19")%>%
  ggplot(.,aes(x=datapt,y=casosAcumulado,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="blue")+
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Acumulado de casos no RN",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

ggplotly(p3)
#salvar em pdf 6 x 8 inches

#### NE 5 (b- óbitos no Piauí)
p4<-ne %>%
  filter(estado == "RN")%>%
  filter(datapt > "2020-03-19")%>%
  ggplot(.,aes(x=datapt,y=obitosAcumulado,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="red")+
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados", colour = "Estado",
       title="Acumulado de óbitos no RN",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

ggplotly(p4)

#### NE 5 (c- Casos diários)
p5<-ne %>%
  filter(estado == "RN")%>%
  filter(datapt > "2020-03-19")%>%
  ggplot(.,aes(x=datapt,y=casosNovos,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="blue")+
   scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Casos diários no RN",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

ggplotly(p5)

#### NE 5 (d- Óbitos diários)
p6<-ne %>%
  filter(estado == "RN")%>%
  filter(datapt > "2020-03-19")%>%
  ggplot(.,aes(x=datapt,y=obitosNovos,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="red")+
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados", colour = "Estado",
       title="Óbitos diários no RN",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

ggplotly(p6)

grid.arrange(p3, p4,p5,p6, nrow = 2)
#salvar em pdf 6 x 8 inches

#### NE 6 (casos no RN)

  ne %>%
  filter(estado == "RN",semanaEpi>18) %>%
  ggplot(.,aes(x=dia,y=casosNovos,group = regiao, colour = regiao))+
  geom_point(colour="blue")+stat_mean_line(color="black",lty=2)+
  geom_line(size=1.1,colour="blue")+facet_wrap(~semanaEpi)+
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  labs(x = "Dia da semana", y = "Casos diários", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="Casos diários por Covid-19 distribuídos por semana epidemiológica", 
       subtitle="A linha tracejada representa a média diária")

#salvar em pdf 6 x 8 inches
#### NE 7 (óbitos RN)

ne %>%
  filter(estado == "PI",semanaEpi>18) %>%
  ggplot(.,aes(x=dia,y=obitosNovos,group = regiao, colour = regiao))+
  geom_point(colour="red")+stat_mean_line(color="black",lty=2)+
  geom_line(size=1.1,colour="red")+facet_wrap(~semanaEpi)+
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  labs(x = "Dia da semana", y = "Óbitos diários", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="Óbitos diários por Covid-19 distribuídos por semana epidemiológica", 
       subtitle="A linha tracejada representa a média diária")
#salvar em pdf 6 x 8 inches
########################################################################
########################################################################

#### Mapa 1 (casos)

RN_mapa <- dados %>%
  filter(estado == "RN",codmun!=240000)%>%
  mutate(data = substr(data,1,5))%>%
  filter(data=="25/06") #Substituir por 01/04, 01/05, 01/06 e 25/06

RN_mapa <- RN_mapa[-1,]

mapa <- get_brmap("City",geo.filter = list(State = 24))
mapa2 <- get_brmap("Region")

names(RN_mapa)[5]<-"City"

mapa<- mapa %>%
  mutate(City = as.numeric(substr(as.character(mapa$City),1,6)))

geral2 <- merge(x = mapa, y = RN_mapa, by = "City", all.x=TRUE)

geral2$casosAcumulado[is.na(geral2$casosAcumulado)]<-0

#(length(geral2$casosAcumulado[geral2$casosAcumulado>0])/length(geral2$nome))*100
#max(geral2$casosAcumulado[geral2$casosAcumulado>0])

geral2 <- geral2%>%
  mutate(categ = cut(casosAcumulado, c(-1,0,10, 100, 500, 2000,8773)))%>%
  mutate(inc = (casosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000)

  k<-ggplot(geral2) + geom_sf(data=mapa2,fill = "gray70")+
  geom_sf(aes(fill =categ,text=nome),size=0.1)+
  theme(panel.background = 
          element_rect(fill = "lightblue"),
        panel.grid = element_line(colour = "transparent"),
        legend.position = "right",
        legend.text = element_text(size=6))+
 # scale_fill_gradient(low="lightyellow", high="red3",
    #                  na.value = "white")+
    scale_fill_manual(values = c("white","lightyellow","rosybrown2","tomato1","red",
                                 "red4"),
                      labels=c("0","1 a 10","11 a 100","101 a 500","501 a 2000",
                               "2001 a 8773"))+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(fill = "",                                
       title=" ",
       subtitle ="25/06/2020", 
       caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
  coord_sf(xlim = c(-38.5, -35), ylim = c(-7, -4.8))
  
#Salvar em pdf 5 x 7 inches
#Salvar em pdf 5 x 7.2 inches (data mais recente)

ggplotly(k,tooltip = c("x","y","text"))

########## Mapa 2 (óbitos)
  
  RN_mapa <- dados %>%
    filter(estado == "RN",codmun!=240000)%>%
    mutate(data = substr(data,1,5))%>%
    filter(data=="25/06")  #Substituir por 01/04, 01/05, 01/06 e 25/06
  
  RN_mapa <- RN_mapa[-1,]
  
  mapa <- get_brmap("City",geo.filter = list(State = 24))
  mapa2 <- get_brmap("Region")
  
  names(RN_mapa)[5]<-"City"
  
  mapa<- mapa %>%
    mutate(City = as.numeric(substr(as.character(mapa$City),1,6)))
  
  geral3 <- merge(x = mapa, y = RN_mapa, by = "City", all.x=TRUE)
  
  geral3$obitosAcumulado[is.na(geral3$obitosAcumulado)]<-0

#(length(geral3$obitosAcumulado[geral3$obitosAcumulado>0])/length(geral3$nome))*100

  geral3 <- geral3%>%
    mutate(categ = cut(obitosAcumulado, c(-1,0,1, 10, 20, 50,328)))%>%
    mutate(mort = (obitosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000)
    l<-ggplot(geral3) + geom_sf(data=mapa2,fill = "gray70")+
    geom_sf(aes(fill =categ,text=nome),size=0.1)+
    theme(panel.background = 
            element_rect(fill = "lightblue"),
          panel.grid = element_line(colour = "transparent"),
          legend.position = "right",
          legend.text = element_text(size=6))+
    # scale_fill_gradient(low="lightyellow", high="red3",
    #                  na.value = "white")+
    scale_fill_manual(values = c("white","lightyellow","rosybrown2","tomato1","red",
                                 "red4"),
                      labels=c("0","1","2 a 10","11 a 20","21 a 50",
                               "51 a 328"))+
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)+
    labs(fill = "",                                
         title="",
         subtitle ="25/06/2020", 
         caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
      coord_sf(xlim = c(-38.5, -35), ylim = c(-7, -4.8))
  
  #Salvar em pdf 5 x 7 inches
  #Salvar em pdf 5 x 7.2 inches (data mais recente)
    
    ggplotly(l,tooltip = c("x","fill","text"))
    
  
########## Mapa 3 (incidência e mortalidade)
  
### a - Incidência
    
  m<-ggplot(geral2) + geom_sf(data=mapa2,fill = "gray70")+
      geom_sf(aes(fill =inc,text=nome),size=0.1)+
      theme(panel.background = 
              element_rect(fill = "lightblue"),
            panel.grid = element_line(colour = "transparent"),
            legend.position = "bottom",
            legend.text = element_text(size=6))+
       scale_fill_gradient(low="lightyellow", high="red3",
                        na.value = "white")+
      annotation_scale(location = "br", width_hint = 0.3) +
      annotation_north_arrow(location = "br", which_north = "true", 
                             pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering)+
      labs(fill = "Taxa por 100k hab.",                                
           title="Incidência",
           subtitle ="25/06/2020", 
           caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
    coord_sf(xlim = c(-38.5, -35), ylim = c(-7, -4.8))
  
  #Salvar em pdf 7 x 8 inches
  
  ggplotly(m,tooltip = c("x","fill","text"))
  
### b- mortalidade
  
  n<-ggplot(geral3) + geom_sf(data=mapa2,fill = "gray70")+
    geom_sf(aes(fill =mort,text=nome),size=0.1)+
    theme(panel.background = 
            element_rect(fill = "lightblue"),
          panel.grid = element_line(colour = "transparent"),
          legend.position = "bottom",
          legend.text = element_text(size=6))+
    scale_fill_gradient(low="white", high="red3",
                        na.value = "white")+
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)+
    labs(fill = "Taxa por 100k hab.",                                
         title="Mortalidade",
         subtitle ="25/06/2020", 
         caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
    coord_sf(xlim = c(-38.5, -35), ylim = c(-7, -4.8))

  #Salvar em pdf 7 x 8 inches
  ggplotly(n,tooltip = c("x","fill","text"))
  
####################################  



