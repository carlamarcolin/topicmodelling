# References:
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
# https://github.com/hadley/rvest/blob/master/demo/tripadvisor.R
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

library(rvest)
#library(xlsx)
library(tm)
library(lsa)
library(dplyr)
library(data.table)
library(readxl)

#######Todos links
#Pega o link após aplicar o filtro:
urllinkmain <- "https://www.vivareal.com.br/aluguel/sp/sao-paulo/zona-sul/vila-nova-conceicao/apartamento_residencial/?__vt=plp:b#onde=BR-Sao_Paulo-NULL-Sao_Paulo-Zona_Sul-Vila_Nova_Conceicao&tipos=apartamento_residencial"

#Verifica o total de resultados da busca:
openCon <- urllinkmain %>% read_html

total_busca <- openCon %>%
  html_node("[class='results-summary__count js-total-records']") %>%
  html_text()

#Transforma em numeric (\\. para escapar só do "." que significa "all characters")
total_busca <- as.numeric(gsub("\\.", "", total_busca))

#Calcula até que página vamos (truncado)
pag <- trunc(total_busca / 36)

DFLink <- vector()

for (i in 2:pag){
     
#Fase 1: insere no main link a pagina
urllinkpre=paste(strsplit(urllinkmain,"plp:b")[[1]][1],"plp:&",sep="")
urllinkpost=strsplit(urllinkmain,"plp:b")[[1]][2]
pagina = paste("pagina=",i)
pagina = gsub("\\s","",pagina)
novolink = paste(urllinkpre,pagina,urllinkpost, sep="")

#Fase 2: tira um pedaço do link que desaparece ao clicar em "proxima pagina"
urllinkpre=paste(strsplit(novolink,"/apartamento_residencial")[[1]][1],"",sep="")
urllinkpost=strsplit(novolink,"/apartamento_residencial")[[1]][2]
urlopen <- paste (urllinkpre,urllinkpost,sep="")

DFLink[i] <- urlopen

}

#Insere na primeira posicao do vetor o link "main", o do resultado da busca
DFLink[1] <- urllinkmain
#Cria o DF que insere todos os títulos completos
DFtextos <- as.data.frame(matrix(ncol = 1,nrow=1))
colnames(DFtextos) <- c("name.f")
DFtextos$name.f <- as.character(DFtextos$name.f)
f <- 1
#######Captura de infos dos imoveis
i = 1
for (i in 1:length(DFLink)){
  
  url <- DFLink[i]
  
  openCon <- url %>% read_html 
  
  #Aqui captura TUDO em um grade caracter
  name <- openCon %>%
    html_nodes("[class='property-card__content']")
    html_text()
  
  #titulo_anunc <- openCon %>%
    #html_nodes("[class='property-card__title js-cardLink js-card-title']") %>%
    #html_text()  
  
  #Não funciona pois tem alguns espaços entre o nome da classe, parecem uns enters
  #enderecos <- openCon %>%
    #html_nodes("[class='property-card__address js-property-card-address']") %>%
    #html_text()  
  
  #ammenities <- openCon %>%
  #  html_nodes("[class='property-card__amenities']")
  #  html_text()
  
  #Guarda os 36 textoes de cada imovel no DF
  for (f in 1:length(name)){
  
  newRow <- data.frame(name[f])
  names(newRow) <- names(DFtextos)
  DFtextos <- rbind(DFtextos,newRow)
  }
}

#####Teste para tratamento dos super textos
#Usei para descobrir que o texto começa no 6º caracter/espaço
strsplit(DFtextos[2,], " ")
#Remove todos espaços "sobrando"
gsub("\\s+", " ", DFtextos[2,])

i = 1
metros2 <- vector()
quartos <- vector()
aluguel <- vector()
for (i in 2:nrow(DFtextos)){
  anunc_terms <- strsplit(DFtextos[i,], " ")
  #Pega dados
  m2 <- anunc_terms[[1]][11]
  metros2[i] <- m2
  qt <- anunc_terms[[1]][7]
  quartos[i] <- ifelse(is.na(qt)|qt=="",1,qt)
  for (j in 1:lengths(anunc_terms)){
    indice_mes <- which(anunc_terms[[1]] %in% "/Mês")
    aluguel[i] <- anunc_terms[[1]][indice_mes - 1]
  }
}  

###Antigo contador de sequencia dentro do for
#counter for additional pages
##morepg=as.numeric(morepglist[[pickhotel]])
##morepg = c(1:100)
##generate a counter 5 to 5 (since there are 5 comments per page)
##morepg = seq(from=0, to=100, by=5)
#morepg = seq(from=0, to=200, by=5)
#urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
#urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]
#urllink=rep(NA,length(morepg)+1)
#urllink[1]=urllinkmain
#j=0
#for(j in 1:length(morepg)){
#urllink[j+1]=paste(urllinkpre,"-or",morepg[j],"-",urllinkpost,sep="")
#}
#head(urllink)

#Get the coordinates only
library(tidyr)
library(dplyr)

zip_codes <- DF$zip
zip_codes <- removePunctuation(zip_codes)
zip_codes <- as.data.frame(sapply(zip_codes,gsub,pattern="Lisboa ",replacement=""))

zip_codes_loc <- DF_Loc$zip
zip_codes_loc <- removePunctuation(zip_codes_loc)
zip_codes_loc <- as.data.frame(sapply(zip_codes_loc,gsub,pattern="Lisboa ",replacement=""))

#Uniques
zip_codes_u <- unique(zip_codes)
#both as.character
zip_codes_u$`sapply(zip_codes, gsub, pattern = "Lisboa ", replacement = "")` <- as.character(zip_codes_u$`sapply(zip_codes, gsub, pattern = "Lisboa ", replacement = "")`)
colnames(zip_codes_u)<- c("V8")
colnames(zip_codes_loc)<- c("V8")

postal_codes <- removePunctuation(pt_postal_codes$`Postal Code`)
pt_postal_codes[,8]<- postal_codes

pt_postal_codes[,8]<- as.numeric(pt_postal_codes$V8)
zip_codes_u$V8 <- as.numeric(zip_codes_u$V8)

zip_codes_loc$V8 <- as.character(zip_codes_loc$V8)
zip_codes_loc$V8 <- as.numeric(zip_codes_loc$V8)

#Iguais
zip_postal <- inner_join(pt_postal_codes,zip_codes_u)
zip_postal_10 <- inner_join(pt_postal_codes,zip_codes_loc)
#Fora da base
zip_postal_out <- anti_join(zip_codes_u,pt_postal_codes)

write.csv(zip_postal_10,"zip_postal_10.csv")


################
#Data treatment 1: remove \n that implies as new "enter" when write.csv:
df2 <- as.data.frame(sapply(DF,gsub,pattern="\n",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern="\r",replacement=" "))
#And then remove "," and ";":
df2 <- as.data.frame(sapply(df2,gsub,pattern=",",replacement=""))
df2 <- as.data.frame(sapply(df2,gsub,pattern=";",replacement=""))
##Acentos que nao vieram
#Agudo no i
df3 <- as.data.frame(sapply(df2,gsub,pattern="<U+00ED>",replacement="i"))
#Til a
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00E3>",replacement="a"))
#Til o
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00F5>",replacement="o"))
#Agudo e
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00E9>",replacement="e"))
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00C9>",replacement="e"))
#Agudo u
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00D3>",replacement="u"))
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00FA>",replacement="u"))
#Agudo a
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00E1>",replacement="a"))
#Cecedilha
df3 <- as.data.frame(sapply(df3,gsub,pattern="<U+00E7>",replacement="c"))
#Agudo o
df2 <- as.data.frame(sapply(df2,gsub,pattern="<U+00F3>",replacement="o"))
#E chapeu
df2 <- as.data.frame(sapply(df2,gsub,pattern="<U+00EA>",replacement="e"))
#O chape
df2 <- as.data.frame(sapply(df2,gsub,pattern="<U+00F4>",replacement="o"))
#Crase
df2 <- as.data.frame(sapply(df2,gsub,pattern="<U+00E0>",replacement="a"))

#Data treatment 2: remove empty lines
df3<-df2[-which(apply(df2,1,function(x)all(is.na(x)))),]

####################
#Remove Pontuacao para exportar CSV tranquilamente
DF$quote <- removePunctuation(DF$quote)
DF$review <- removePunctuation(DF$review)

write.csv(DF, "df.csv")
write.csv(df2, "df2.csv")
write.csv(df3, "df3.csv")
#######
