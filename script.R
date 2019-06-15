
con<-dbx::dbxConnect()

library(tidyverse)

## Contar número de linhas
k<-DBI::dbGetQuery(con,
  'SELECT count(*) AS ct            
     , min(id)  AS min_id
     , max(id)  AS max_id
     , max(id) - min(id) AS id_span
FROM   dados_2018')

DBI::dbClearResult(con)

sql_sample <- function(con, tbl=NULL,column=NULL,value=NULL, n=10) {
tbl="part"
if (is.null(column)){
  query<-glue::glue_sql("SElECT * 
                      FROM {`tbl`} 
                      ORDER BY random() LIMIT {n}",
                        .con=con)
} else {
query<-glue::glue_sql("SElECT * 
                      FROM {`tbl`} 
                      WHERE {`tbl`}.{`column`} = {value}  
                      ORDER BY random() LIMIT {n}",
                      .con=con)
}
DBI::dbGetQuery(con,query)
}






dc<-sql_sample(con,"dados_2018","subarea","DIREITO DO CONSUMIDOR",500)

write.csv(dc,file = "introducao/data/dc.csv")

readr::write_csv(dc,path = "introducao/data/dc.csv")
readr::write_delim(dc,"introducao/data/dc.csv", delim=";")


saveRDS(dc,"introducao/data/dc.rds")
writexl::write_xlsx(dc,"introducao/data/dc.xlsx")

save(dc,file="introducao/data/dc.RData")
save(dc,k,file="introducao/data/dck.rda")
save.image("introducao/data/tudo.rda")
jsonlite::write_json(dc,"introducao/data/dc.json")

df<-readRDS("introducao/data/dc.rds")

id<-dc$id
id<-dc[["id"]]
dc$id<-NULL

dc <- dc %>% 
  tibble::add_column(antigoid=id,.after="vara")

dc <- dc %>% 
  rownames_to_column("novoid")

View(mtcars)
mtcars<-mtcars %>% 
  rownames_to_column("id")

### tidyr

query<-'SELECT * FROM partes_2018 ORDER BY random() LIMIT 500'

partes<-DBI::dbGetQuery(con,query)

saveRDS(partes,"/home/cintia/partes.rds")

p<-spread(partes,parte,-processo)
pl<-gather(p,"parte",,-processo, -id,-parte_nome)
pl<- p %>% 
  group_by(processo) %>% 
  gather("parte",-id,)


dc3<-dc3 %>% 
  separate(processo_principal,c("cnj_ordem","cnj_digito","cnj_ano","cnj_segmento","cnj_uf","cnj_distribuidor"),sep="\\D")

dc3<-dc3 %>% 
  unite(processo_principal,c("cnj_ordem","cnj_digito","cnj_ano","cnj_segmento","cnj_uf","cnj_distribuidor"))

## Dplyr

### Filter

#### Comparação
advdo<-partes %>% 
  filter(parte=="Advogado")

#### Regex
adv<-partes %>% 
  filter(str_detect(parte,"dv.{0,3}d"))

n_adv<-partes %>% 
  filter(str_detect(parte,"dv.{0,3}d",negate=TRUE))

#### Ou
n_adv<-partes %>% 
  filter(!str_detect(parte,"dv\\X{0,3}d"))


#### Operadores

dc1<-dc %>% 
  filter()


saveRDS(partes,"/home/saylon/rpartes.rds")

dc0001<-dc %>% 
  filter(str_detect(processo,"0001$")) %>% 
  filter(digital==TRUE)







### Select ####

dc1<-dc %>% 
  select(juiz,everything())
  

# fechando select ####

### arrange ####

dc2<-dc %>% 
   arrange(data_recebimento)

dc2<-dc %>% 
  arrange(desc(valor_da_acao))

### Mutate

dc3<-dc %>% 
  mutate(duplo_valor=valor_da_acao*2)

library(lubridate)
options(scipen=999)
options(digits=20)

dc3<-dc %>% 
  mutate(n_foro=str_extract(processo,"\\d{4}$"),
         ano=year(data_recebimento),
         mes=month(data_recebimento,abbr=FALSE,label=TRUE),
         dia=day(data_recebimento),
         dia_semana=wday(data_recebimento,label=TRUE,abbr=FALSE),
         faixa_valor=cut(valor_da_acao,breaks=c(0,200,500,1000,2000,20000,Inf),
                         dig.lab=10,
                         labels=c("De zero a 199","De 200 a 499","De 500 a 999","De 1000 a 1999","De 2000 a 19999","20 mil ou mais"))) 
  
  
### Summarize

dc4<- dc3 %>% 
  #select(faixa_valor,valor_da_acao) %>% 
drop_na(valor_da_acao) %>%  
  group_by(faixa_valor) %>% 
  summarize(media=mean(valor_da_acao,na.rm=T),
            desvio_padrao=sd(valor_da_acao,na.rm=T),
            mediana=median(valor_da_acao,na.rm=TRUE),
            soma=sum(valor_da_acao))


clusters<-dc3$valor_da_acao %>% 
  na.omit() %>% 
  kmeans(3)

partes2<-partes %>% 
   distinct(processo,.keep_all = TRUE)
  
  



