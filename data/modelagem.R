dados<-dados %>% 
  select(c(1,3:9))

dados<-dados %>% 
  filter(classe=="Apelação")

### Juntar data de entrada

base<-dados %>% 
  mutate(processo_sem_ponto=str_remove_all(processo,"\\D+")) %>% 
  left_join(entrada,by=c("processo_sem_ponto"="processo"))


## Retirar assuntos 


assuntos<-c("ASSUNTOS ANTIGOS DO SAJ - Lei 8.137/90 - Sonegação Fiscal", 
  "DIREITO PENAL - Crimes Previstos na Legislação Extravagante", 
  "DIREITO PENAL - Crimes Previstos na Legislação Extravagante - Crimes contra a Ordem Tributária", 
  "DIREITO PENAL-Parte Geral-Extinção da Punibilidade-Pagamento ou Parcelamento do Crédito Tributário",
  "DIREITO PENAL",
  "DIREITO PROCESSUAL PENAL")

base<-base %>% 
  filter(assunto %in% !!assuntos)



base<-base %>% 
  select(-c(classe,area))

base<-base %>%
  rename(data_entrada="data")

base<-base %>%
  separate(origem,c("comarca","foro","vara")," ?/ ?")

## Juntar decisões

decisao<-decisao %>% 
  filter(situacao_julgamento=="Julgado") %>% 
  mutate(data_julgamento=dmy(data_julgamento)) %>% 
  select(processo,data_julgamento,decisao)

base<-base %>% 
      left_join(decisao,by=c("processo_sem_ponto"="processo"))

recorrente<-recorrente %>% 
  mutate(parte=ifelse(str_detect(parte,"te"),"Apelante","Apelado"))

base<-recorrente %>% 
  left_join(base,by=c("processo"="processo_sem_ponto"))


base$parte_nome<-NULL

base<-base %>% 
     mutate(tempo_processo=as.Date(data_julgamento)-as.Date(data_entrada)/dyears)

### PDFs


library(pdftools)

a<-list.files("pdfs",full.names = TRUE)
processo<-str_extract(a,"\\d{5,}")

system.time(

pdfs<-map2_dfr(a,processo,~{
  
texto<-  pdf_text(.x,) %>% paste0(collapse="\r\n\r\n")
tibble(processo=.y,texto=texto)


})
)


library(JurisMiner)

insignificancia<-pt_kwic(pdfs$texto[1:100],pdfs$processo[1:100],keyword = "(insigni\\w+|bagatela)",type = "regex",before=10,after=10,unite=T)

insignificancia<-insignificancia %>% 
  filter(!is.na(keyword))
insi<-insignificancia %>% 
  select(id_decision,pre,keyword,post)

 insignificancia<-insignificancia %>% 
   filter(!is.na(keyword))
 
 
 #### Administrador
 
 
administrador<-pt_kwic(pdfs$texto,pdfs$processo,keyword = "administra\\w+",type = "regex",unite=T)
 
administrador<-administrador %>% 
   filter(!is.na(keyword)) %>% 
  select(id_decision,pre,keyword,post)

library(magrittr)


administrador %<>%
  filter(!str_detect(keyword,"(?i)administrativ[ao]"))
  
 

### Prescrição



prescricao<-pt_kwic(pdfs$texto,pdfs$processo,keyword = "prescr\\w+",type = "regex",unite=T)

prescricao<-prescricao %>% 
  filter(!is.na(keyword)) %>% 
  select(id_decision,pre,keyword,post)


### Primariedade


primariedade<-pt_kwic(pdfs$texto,pdfs$processo,keyword = "(primari\\w+|anteced\\w+|reincid\\w+)",type = "regex",unite=T)

primariedade<-primariedade %>% 
  filter(!is.na(keyword)) %>% 
  select(id_decision,pre,keyword,post)



## Declaração falsa


declaracao_falsa<-pt_kwic(pdfs$texto,pdfs$processo,keyword = "declaracao falsa",type = "regex",unite=T)

declaracao_falsa<-declaracao_falsa %>% 
  filter(!is.na(keyword)) %>% 
  select(id_decision,pre,keyword,post)

### 





dificuldade<-pt_kwic(pdfs$texto,pdfs$processo,keyword = "dificuldades? financeiras?",type = "regex",unite=T)

dificuldade<-dificuldade %>% 
  filter(!is.na(keyword)) %>% 
  select(id_decision,pre,keyword,post)




base %<>%
mutate(foro=NULL,
       comarca=NULL,
       vara=NULL,
       numeros_de_origem=NULL,
       data_entrada=NULL,
       data_julgamento=NULL) %>% 
  filter(parte=="Apelante")

base$tempo_processo<-NULL

count(base,distribuicao) %>% View()


base<-base %>% 
  select(processo,distribuicao)
base<-na.omit(base) 

base<-unique(base)
base<-base %>% 
   mutate(camara=str_replace_all(distribuicao,"Câmara de Direito Criminal","CDC"),
          camara=str_replace_all(camara,"Câmara Criminal Extraordinária","CCE"))

base1<-base

base2<-na.omit(base)


base<-base %>% 
  select(processo,parte=mp_defesa,camara,decisao)
 

administrador<-administrador %>% 
  select(processo=id_decision) %>% 
  distinct()

prescricao<-prescricao %>% 
  select(processo=id_decision) %>% 
  distinct()


prescricao$prescricao<-"sim"
administrador$administrador<-"sim"

base<-base %>% 
left_join(administrador)

base<-base %>% 
   left_join(prescricao)


base<-base %>% 
  mutate(prescricao=ifelse(is.na(prescricao),"não",prescricao),
         administrador=ifelse(is.na(administrador),"não",administrador))


base<-base %>% 
  filter(str_detect(decisao,"provido"))

base<-base %>% 
mutate_all(as.factor)
  
library(caret)


btrain<-createDataPartition(base$decisao,p=.85,list=F)

train<-base[btrain,]

test<-base[-btrain,]



ctrl <- trainControl(method = "repeatedcv", # Para resampling usa validação cruzada repetica
                     
                     number = 10, ## Número de iterações
                     
                     repeats = 5, ## Número de folds a serem computados
                     
                     summaryFunction = twoClassSummary, ## Função para computar métricas de desempenho na validação cruzada
                     
                     classProbs = TRUE, ## Computa as probabilidades das classes/etiquetas
                     
                     savePredictions = TRUE, ## salva as predições no resampling
                     
                     allowParallel = TRUE, ## autoriza paralelização.
                     
                     sampling="up" ## Equilibra as classes para cima, já que a maioria é "denegado"
                     
)

train$processo<-NULL


mod_GLM <- train(decisao ~ .,data=train, method="glm", family="binomial",
                 
                 trControl = ctrl, tuneLength = 5,
                 
                 metric = "ROC")


test$processo<-NULL
#### Predição



pglm<-predict(mod_GLM,test)

ppglm<-confusionMatrix(pglm,test$decisao)

ppglm





#### Diagnóstico



p<-predict(mod_GLM,test,"prob")[[1]]

t<-ifelse(test$decisao=="concedido",1,0)

a<-classifierplots::roc_plot(t,p)



## GBM

grid_gbm <- expand.grid(interaction.depth=5, n.trees = 250,
                        
                        shrinkage=0.01,
                        
                        n.minobsinnode=10)



mod_GBM <- train(decisao ~ .,  data=train, method="gbm",
                 
                 trControl = ctrl,tuneGrid=grid_gbm,tuneLength = 5,metric = "ROC")


pgbm<-predict(mod_GBM,test)

ppgbm<-confusionMatrix(pgbm,test$decisao)

ppgbm

test$probabilidade <- predict(mod_GBM,test,"prob")[[1]]

test$t <- ifelse(test$decisao=="improvido",1,0)

classifierplots::roc_plot(test$t,test$probabilidade)
vd_test$predito<-ifelse(test$probabilidade>.25,"improvido","provido")
confusionMatrix(table(vd_test$predito,vd_test$decisao))

p<-predict(mod_GBM,test,"prob")[[1]]

t<-ifelse(test$decisao=="concedido",1,0)

classifierplots::roc_plot(t, p)






## Feature engeneering
relator_camara<-