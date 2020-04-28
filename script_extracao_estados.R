#################### Extracao dos dados COVID-19 para estados e municipios

###### Download dos csv a partir das bases de dados

repeat {
    
    if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
    if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
    
    setwd("C:/Users/jaylt/Desktop/Dados atualizados estados e municipios") #diretorio de trabalho
    
    cases_deaths_cum <- read.csv("https://brasil.io/dataset/covid19/caso?format=csv")
    
    daily_cases_est <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
    
    daily_cases_mun <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time_changesOnly.csv")
    
    inf_estados <- read.csv("https://raw.githubusercontent.com/JotaB921/Dados-para-o-modelo/master/inf_estados.csv")
    
    dias_infec <- data.frame(dias_infec=(seq(as.Date("2020-02-25"),
                                             as.Date(Sys.Date()-1), by="days"))) #criando um dataframe com os dias do 1º caso ate o dia anterior a hoje
    
    ####### Ajustando o dataset dos estados para novos casos
    
    mg_daily_cases_est <- merge(daily_cases_est, inf_estados, 
                                by.x = "state", by.y = "UF")  #colocando o inf_estados no dataset dos novos casos
    
    mg_daily_cases_est$date <- as.numeric(as.character(as.Date(
    mg_daily_cases_est$date, format = "%Y-%m-%d"), format="%Y%m%d")) #transformando as datas em numeros
    
    chave <- mg_daily_cases_est$date * mg_daily_cases_est$ibgeID #gerando a chave data * ibgeID
    
    mg_daily_cases_est <- data.frame(mg_daily_cases_est,chave) #criando um novo dataframe com a chave
    
    tinf_estados <- data.frame(t(inf_estados[-1])) #transpondo para gerar a chave para todos os dias da infeccao
    
    colnames(tinf_estados) <- inf_estados$ibgeID
    
    dias_infec$dias_infec <- as.numeric(as.character(as.Date(
    dias_infec$dias_infec, format = "%Y-%m-%d"), format="%Y%m%d")) #convertendo as datas da infeccao para numero
    
    dias_teste <- cbind(dias_infec,replicate(27,dias_infec$dias_infec)) #criando 27 colunas com os dias da infeccao para todos os estados
    
    dias_teste <- dias_teste[-1] # removendo a coluna dias_infec
    
    tdias_teste <- data.frame(t(dias_teste)) # transpondo para fazer a multiplicacao e gerar a chave para todos os dias da infec e todas UF
    
    teste_chave <- tdias_teste*inf_estados$ibgeID #gerando a chave data * ibgeID
    
    teste_chave <- data.frame(t(teste_chave)) #transpondo para ibgeID nas colunas e datas nas linhas
    
    colnames(teste_chave) <- colnames(tinf_estados) #nomes das colunas como ibgeID
    
    row.names(teste_chave) <- dias_infec$dias_infec #nomes das linhas como datas da infeccao
    
    stack_dias_teste <- stack(dias_teste) #gerando os dias da infeccao do tamanho do vetor de chaves
    
    stack_dias_teste <- stack_dias_teste[-2]
    
    colnames(stack_dias_teste) <- c("data")
    
    stack_teste <- stack(teste_chave) #empilhando as colunas em um unico vetor
    
    names(stack_teste) <- c("chave", "ibgeID") #renomeando as colunas
    
    juntando <- data.frame(stack_teste, stack_dias_teste) #juntando as chaves/ibgeID/ data em um unico dataframe (dataset x para o merge)
    
    novos_casos_est <- data.frame(mg_daily_cases_est$chave, mg_daily_cases_est$newCases) # extraindo novos casos por uf de daily_cases_est (dataset y para o merge)
    
    colnames(novos_casos_est) <- c("chave", "newCases")
    
    mg_teste <- merge(juntando, mg_daily_cases_est[,
                                                   c("chave", "newCases")], by="chave", all.x = TRUE) #merge com apenas as colunas desejadas de y (mg_daily)
    
    mg_teste[is.na(mg_teste)] <- 0 # substituição de na por zeros
    
    mg_teste2 <- mg_teste[-1] # removendo a coluna das chaves para formatacao final
    
    teste_2 <- data.frame(spread(mg_teste2, "data", "newCases")) #organizando os dados empilhados no dataframe final
    
    final_novos_casos_UF <- merge(inf_estados, teste_2, by.x = "ibgeID", by.y = "ibgeID")
    
    final_novos_casos_UF$UF <- NULL
    
    final_novos_casos_UF[is.na(final_novos_casos_UF)] <- ""
    
    teste_final_novos_casos_UF <- final_novos_casos_UF %>%
    select(2, 1, everything())
    
    
    teste_final_novos_casos_UF[is.na(teste_final_novos_casos_UF)] <- ""
    
    write.csv(teste_final_novos_casos_UF, file = 
                "C:/Users/jaylt/Desktop/Dados atualizados estados e municipios/novos_casos_estados_25_04.csv", row.names = FALSE)
    
    #-----------------------------------------------------------------------
    ####### Ajustando o dataset dos estados para mortes acumuladas
    #-----------------------------------------------------------------------
    
    mg_cases_deaths_cum <- merge(cases_deaths_cum, inf_estados, by.x = "state", by.y = "UF")  #colocando o inf_estados no dataset dos novos casos
    
    mg_cases_deaths_cum$date <- as.numeric(as.character(as.Date(
    mg_cases_deaths_cum$date, format = "%Y-%m-%d"), format="%Y%m%d")) #transformando as datas em numeros
    
    chave <- mg_cases_deaths_cum$date * mg_cases_deaths_cum$city_ibge_code #gerando a chave data * ibgeID
    
    mg_cases_deaths_cum <- data.frame(mg_cases_deaths_cum,chave) #criando um novo dataframe com a chave
    
    tinf_estados <- data.frame(t(inf_estados[-1])) #transpondo para gerar a chave para todos os dias da infeccao
    
    colnames(tinf_estados) <- inf_estados$ibgeID
    
    #colnames(tcdg_UFs) <- cdg_UFs[,1] #renomeando as colunas 
    
    dias_infec$dias_infec <- as.numeric(as.character(as.Date(
    dias_infec$dias_infec, format = "%Y-%m-%d"), format="%Y%m%d")) #convertendo as datas da infeccao para numero
    
    dias_teste <- cbind(dias_infec,replicate(27,dias_infec$dias_infec)) #criando 27 colunas com os dias da infeccao para todos os estados
    
    dias_teste <- dias_teste[-1] # removendo a coluna dias_infec
    
    tdias_teste <- data.frame(t(dias_teste)) # transpondo para fazer a multiplicacao e gerar a chave para todos os dias da infec e todas UF
    
    teste_chave <- tdias_teste*inf_estados$ibgeID #gerando a chave data * ibgeID
    
    teste_chave <- data.frame(t(teste_chave)) #transpondo para ibgeID nas colunas e datas nas linhas
    
    colnames(teste_chave) <- colnames(tinf_estados) #nomes das colunas como ibgeID
    
    row.names(teste_chave) <- dias_infec$dias_infec #nomes das linhas como datas da infeccao
    
    stack_dias_teste <- stack(dias_teste) #gerando os dias da infeccao do tamanho do vetor de chaves
    
    stack_dias_teste <- stack_dias_teste[-2]
    
    colnames(stack_dias_teste) <- c("data")
    
    stack_teste <- stack(teste_chave) #empilhando as colunas em um unico vetor
    
    names(stack_teste) <- c("chave", "ibgeID") #renomeando as colunas
    
    juntando <- data.frame(stack_teste, stack_dias_teste) #juntando as chaves/ibgeID/ data em um unico dataframe (dataset x para o merge)
    
    obt_acum_est <- data.frame(mg_cases_deaths_cum$chave,mg_cases_deaths_cum$deaths) # extraindo novos casos por uf de daily_cases_est (dataset y para o merge)
    
    colnames(obt_acum_est) <- c("chave", "obitosAcum")
    
    ###### extraindo os dados dos obitos
    
    mg_teste <- merge(juntando, obt_acum_est[,c("chave", "obitosAcum")], by="chave", all.x = TRUE) #merge com apenas as colunas desejadas de y (mg_daily)
    
    mg_teste[is.na(mg_teste)] <- 0 # substituição de na por zeros
    
    mg_teste2 <- mg_teste[-1] # removendo a coluna das chaves para formatacao final
    
    teste_2 <- data.frame(spread(mg_teste2, "data", "obitosAcum")) #organizando os dados empilhados no dataframe final
    
    final_obitos_UF <- merge(inf_estados, teste_2, by.x = "ibgeID", by.y = "ibgeID")
    
    final_obitos_UF$UF <- NULL
    
    final_obitos_UF[is.na(final_obitos_UF)] <- ""
    
    teste_final_obitos_UF <- final_obitos_UF %>%
    select(2, 1, everything())
    
    teste_final_obitos_UF[is.na(teste_final_obitos_UF)] <- ""
    
    write.csv(teste_final_obitos_UF, file = "C:/Users/jaylt/Desktop/Dados atualizados estados e municipios/obitos_acum_estados_24_04.csv", row.names = FALSE)

Sys.sleep(200)
}


#Sys.sleep(20) # intervalo de tempo para repeticao (em segundos) - 21.600 segundos (intervalo de 12 horas)
#}











