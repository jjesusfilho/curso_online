library(caret)
library(tidymodels)
vd<-readRDS("data/vd.rds")

vd<-vd %>%
  select(camara,origem,decisao,ano,apelante,relator) %>%
  filter(row_number(camara)>49) %>% 
  droplevels()

data_split <- initial_split(vd, strata = "decisao")
train <- training(data_split)
test <- testing(data_split)

ctrl <- trainControl(method = "repeatedcv", # Para resampling usa validação cruzada repetica
                     number = 10, ## Número de folds a serem computados
                     repeats = 5, ## Número de iterações
                     summaryFunction = twoClassSummary, ## Função para computar métricas de desempenho na validação cruzada
                     classProbs = TRUE, ## Computa as probabilidades das classes/etiquetas
                     savePredictions = TRUE, ## salva as predições no resampling
                     sampling="down", ## Equilibra as classes para baixo
                     allowParallel = TRUE ## autoriza paralelização.
)

grid_gbm <- expand.grid(interaction.depth=5, n.trees = 250,
                        shrinkage=0.01,
                        n.minobsinnode=10)
library(caret)

mod_GBM <- train(decisao ~ camara:relator+apelante+origem,  data=train, method="gbm",
                 trControl = ctrl,tuneGrid=grid_gbm,tuneLength = 5,metric = "ROC")
mod_xgb<-train(decisao~origem+apelante+relator:camara,data=train,method="xgbLinear",trControl=ctrl)
