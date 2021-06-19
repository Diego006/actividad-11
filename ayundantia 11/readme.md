AYUDANTIA 11 {.title .toc-ignore}
============

``` {.r}
library(psych)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` {.r}
library(stringr)
library(datasets.load)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v readr   1.4.0
    ## v tibble  3.1.0     v purrr   0.3.4
    ## v tidyr   1.1.3     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x ggplot2::%+%()   masks psych::%+%()
    ## x ggplot2::alpha() masks psych::alpha()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()

``` {.r}
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` {.r}
library(tidytext)
library(quanteda)
```

    ## Package version: 3.0.0
    ## Unicode version: 10.0
    ## ICU version: 61.1

    ## Parallel computing: 8 of 8 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:tm':
    ## 
    ##     stopwords

    ## The following objects are masked from 'package:NLP':
    ## 
    ##     meta, meta<-

``` {.r}
library(quanteda.textstats)
library(ggplot2)
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` {.r}
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` {.r}
library(readxl)
library(chron)
```

    ## 
    ## Attaching package: 'chron'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     days, hours, minutes, seconds, years

``` {.r}
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` {.r}
library(flexclust)
```

    ## Loading required package: grid

    ## Loading required package: lattice

    ## Loading required package: modeltools

    ## Loading required package: stats4

``` {.r}
library(cluster)
library(ggdendro)
library(factoextra)
library(knitr)
library(mclust)
```

    ## Package 'mclust' version 5.4.7
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## 
    ## Attaching package: 'mclust'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

    ## The following object is masked from 'package:psych':
    ## 
    ##     sim

``` {.r}
library(dbscan)
library(e1071)
```

    ## 
    ## Attaching package: 'e1071'

    ## The following object is masked from 'package:flexclust':
    ## 
    ##     bclust

``` {.r}
library(olsrr)
```

    ## 
    ## Attaching package: 'olsrr'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     rivers

``` {.r}
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` {.r}
library(class)
library(discrim)
```

    ## Loading required package: parsnip

    ## 
    ## Attaching package: 'parsnip'

    ## The following object is masked from 'package:modeltools':
    ## 
    ##     fit

``` {.r}
library(tidymodels)
```

    ## Registered S3 method overwritten by 'tune':
    ##   method                   from   
    ##   required_pkgs.model_spec parsnip

    ## -- Attaching packages -------------------------------------- tidymodels 0.1.3 --

    ## v broom        0.7.6      v rsample      0.1.0 
    ## v dials        0.0.9      v tune         0.1.5 
    ## v infer        0.5.4      v workflows    0.2.2 
    ## v modeldata    0.1.0      v workflowsets 0.0.2 
    ## v recipes      0.1.16     v yardstick    0.0.8

    ## -- Conflicts ----------------------------------------- tidymodels_conflicts() --
    ## x ggplot2::%+%()          masks psych::%+%()
    ## x scales::alpha()         masks ggplot2::alpha(), psych::alpha()
    ## x NLP::annotate()         masks ggplot2::annotate()
    ## x scales::discard()       masks purrr::discard()
    ## x dplyr::filter()         masks stats::filter()
    ## x parsnip::fit()          masks modeltools::fit()
    ## x recipes::fixed()        masks stringr::fixed()
    ## x dplyr::lag()            masks stats::lag()
    ## x mclust::map()           masks purrr::map()
    ## x tune::parameters()      masks dials::parameters(), flexclust::parameters(), modeltools::parameters()
    ## x rsample::permutations() masks e1071::permutations()
    ## x yardstick::spec()       masks readr::spec()
    ## x recipes::step()         masks stats::step()
    ## x tune::tune()            masks e1071::tune()
    ## x recipes::update()       masks stats4::update(), stats::update()
    ## * Use tidymodels_prefer() to resolve common conflicts.

``` {.r}
library(naivebayes)
```

    ## naivebayes 0.9.7 loaded

``` {.r}
library(kknn)
library(patchwork)
```

Leemos el arcchivo y cargamos los datos

``` {.r}
setwd("C:/Users/Dieca/OneDrive/Escritorio/ayundantia 11")
creditcard <- read.csv("UCI_Credit_Card.csv", sep = ",")
glimpse(creditcard)
```

    ## Rows: 30,000
    ## Columns: 25
    ## $ ID                         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, ~
    ## $ LIMIT_BAL                  <dbl> 20000, 120000, 90000, 50000, 50000, 50000, ~
    ## $ SEX                        <int> 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1~
    ## $ EDUCATION                  <int> 2, 2, 2, 2, 2, 1, 1, 2, 3, 3, 3, 1, 2, 2, 1~
    ## $ MARRIAGE                   <int> 1, 2, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2~
    ## $ AGE                        <int> 24, 26, 34, 37, 57, 37, 29, 23, 28, 35, 34,~
    ## $ PAY_0                      <int> 2, -1, 0, 0, -1, 0, 0, 0, 0, -2, 0, -1, -1,~
    ## $ PAY_2                      <int> 2, 2, 0, 0, 0, 0, 0, -1, 0, -2, 0, -1, 0, 2~
    ## $ PAY_3                      <int> -1, 0, 0, 0, -1, 0, 0, -1, 2, -2, 2, -1, -1~
    ## $ PAY_4                      <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, -1, -1, ~
    ## $ PAY_5                      <int> -2, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, -1, ~
    ## $ PAY_6                      <int> -2, 2, 0, 0, 0, 0, 0, -1, 0, -1, -1, 2, -1,~
    ## $ BILL_AMT1                  <dbl> 3913, 2682, 29239, 46990, 8617, 64400, 3679~
    ## $ BILL_AMT2                  <dbl> 3102, 1725, 14027, 48233, 5670, 57069, 4120~
    ## $ BILL_AMT3                  <dbl> 689, 2682, 13559, 49291, 35835, 57608, 4450~
    ## $ BILL_AMT4                  <dbl> 0, 3272, 14331, 28314, 20940, 19394, 542653~
    ## $ BILL_AMT5                  <dbl> 0, 3455, 14948, 28959, 19146, 19619, 483003~
    ## $ BILL_AMT6                  <dbl> 0, 3261, 15549, 29547, 19131, 20024, 473944~
    ## $ PAY_AMT1                   <dbl> 0, 0, 1518, 2000, 2000, 2500, 55000, 380, 3~
    ## $ PAY_AMT2                   <dbl> 689, 1000, 1500, 2019, 36681, 1815, 40000, ~
    ## $ PAY_AMT3                   <dbl> 0, 1000, 1000, 1200, 10000, 657, 38000, 0, ~
    ## $ PAY_AMT4                   <dbl> 0, 1000, 1000, 1100, 9000, 1000, 20239, 581~
    ## $ PAY_AMT5                   <dbl> 0, 0, 1000, 1069, 689, 1000, 13750, 1687, 1~
    ## $ PAY_AMT6                   <dbl> 0, 2000, 5000, 1000, 679, 800, 13770, 1542,~
    ## $ default.payment.next.month <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0~

``` {.r}
str(creditcard)
```

    ## 'data.frame':    30000 obs. of  25 variables:
    ##  $ ID                        : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ LIMIT_BAL                 : num  20000 120000 90000 50000 50000 50000 500000 100000 140000 20000 ...
    ##  $ SEX                       : int  2 2 2 2 1 1 1 2 2 1 ...
    ##  $ EDUCATION                 : int  2 2 2 2 2 1 1 2 3 3 ...
    ##  $ MARRIAGE                  : int  1 2 2 1 1 2 2 2 1 2 ...
    ##  $ AGE                       : int  24 26 34 37 57 37 29 23 28 35 ...
    ##  $ PAY_0                     : int  2 -1 0 0 -1 0 0 0 0 -2 ...
    ##  $ PAY_2                     : int  2 2 0 0 0 0 0 -1 0 -2 ...
    ##  $ PAY_3                     : int  -1 0 0 0 -1 0 0 -1 2 -2 ...
    ##  $ PAY_4                     : int  -1 0 0 0 0 0 0 0 0 -2 ...
    ##  $ PAY_5                     : int  -2 0 0 0 0 0 0 0 0 -1 ...
    ##  $ PAY_6                     : int  -2 2 0 0 0 0 0 -1 0 -1 ...
    ##  $ BILL_AMT1                 : num  3913 2682 29239 46990 8617 ...
    ##  $ BILL_AMT2                 : num  3102 1725 14027 48233 5670 ...
    ##  $ BILL_AMT3                 : num  689 2682 13559 49291 35835 ...
    ##  $ BILL_AMT4                 : num  0 3272 14331 28314 20940 ...
    ##  $ BILL_AMT5                 : num  0 3455 14948 28959 19146 ...
    ##  $ BILL_AMT6                 : num  0 3261 15549 29547 19131 ...
    ##  $ PAY_AMT1                  : num  0 0 1518 2000 2000 ...
    ##  $ PAY_AMT2                  : num  689 1000 1500 2019 36681 ...
    ##  $ PAY_AMT3                  : num  0 1000 1000 1200 10000 657 38000 0 432 0 ...
    ##  $ PAY_AMT4                  : num  0 1000 1000 1100 9000 ...
    ##  $ PAY_AMT5                  : num  0 0 1000 1069 689 ...
    ##  $ PAY_AMT6                  : num  0 2000 5000 1000 679 ...
    ##  $ default.payment.next.month: int  1 1 0 0 0 0 0 0 0 0 ...

``` {.r}
creditcard$ID <- NULL

creditcard$SEX <- factor(creditcard$SEX, levels=1:2, labels=c("Male", "Female"))
creditcard$EDUCATION <- as.factor(creditcard$EDUCATION)
creditcard$MARRIAGE <- as.factor(creditcard$MARRIAGE)
creditcard$default.payment.next.month <- factor(creditcard$default.payment.next.month, levels = 0:1, labels=c("No", "Yes"))
```

\#\#arboles de decision

El primer paso que hacemos es separar la data en conjunto de
entrenamiento y de prueba, donde tidymodels tiene la funcion
initial\_split

``` {.r}
data_split <- initial_split(creditcard, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
```

    ## [1] 7500

``` {.r}
#train_data %>% nrow()
```

Luego, creamos la “receta” del modelo, que consiste en la relacion de
“caja negra” entre las variables de entrada y las variables de salida.
En este caso, la receta será modelar Exited en funcion de todas las
variables presentes en el conjunto de datos.

``` {.r}
receta <- 
  recipe(default.payment.next.month ~ ., data = train_data) 

receta
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor         23

Ahora si creamos el modelo, donde utilizaremos un arbol de decision con
5 capas de decision, y un minimo numero de entidades por hoja (poda) de
10. La libreria que se utiliza para calcular este modelo sera la de
rpart, que viene precargada en los paquetes que estamos utilizando. Con
este paso solo definimos el modelo, aun lo calculamos.

``` {.r}
modelo <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo
```

    ## Decision Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   tree_depth = 5
    ##   min_n = 10
    ## 
    ## Computational engine: rpart

Ahora hacemos el fit del modelo, calculamos sus predicciones y
calculamos el valor de AUC

``` {.r}
fitea <- function(mod){
  
  modelo_fit <- 
  workflow() %>% 
  add_model(modelo) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) 

return(model_pred %>% 
  roc_auc(truth = default.payment.next.month, .pred_0))
}
##fitea(modelo)
```

Ahora veremos la magia de tidymodels, haremos una comparacion con otros
modelos, como el modelo de regresion logistica, naive bayes o Knn. Para
esto, lo unico que debemos cambiar es el modelo, ya que la receta es la
misma, y el flujo de validacion tambien es el mismo. Por lo tanto
podemos utilizar la funcion que creamos mas arriba para evaluar los
diferentes modelos y compararlos.

``` {.r}
modelo_rl <- 
  logistic_reg() %>% 
  set_engine("glm")

#fitea(modelo_rl)
```

``` {.r}
modelo_nb <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("naivebayes")

#fitea(modelo_nb)
```

``` {.r}
modelo_knn <-
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

#fitea(modelo_knn)
```

ahora realizaremos la prediccion .

``` {.r}
modelo_fit <- 
  workflow() %>% 
  add_model(modelo_knn) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data)
view(model_pred)
```
