# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
#-----------------------------------------------------------#

# carregando os pacotes necessários ####
library("tidyr")
library("base") # bem provável que este pacote seja básico e já venha estalado. Na dúvia, carregue-o também

# lendo os dados ####

comm <- read.table("./dados/comm.txt", sep = ",", h = T)
coord <- read.table("./dados/coord.txt", sep = ",", h = T)
envir <- read.table("./dados/envir.txt", sep = ",", h = T)
splist <- read.table("./dados/splist.txt", sep = ",", h = T)
traits <- read.table("./dados/traits.txt", sep = ",", h = T)

# entendendo cada objeto ####
# comm
head(comm)
dim(comm)
summary(comm)

# coord
head(coord)
dim(coord)
summary(coord)

#envir
head(envir)
dim(envir)
summary(envir)

# splist
head(splist)
dim(splist)
summary(splist)

# traits
head(traits)
dim(traits)
summary(traits)

# sumário dos dados ####
# n espécies
nrow(splist)

# n areas amostrais
nrow(comm) #ou:
nrow(envir)

# n var abientais
# todas as variáveis exceto a primeira coluna com o id
names(envir)[-1]
# contando quantas variáveis
length(names(envir)[-1])

# calculo de riqueza por área
# transformando a matrix de abundância em uma de incidência
comm.pa <- comm[, -1] > 0
# nomeando as linhas das planilhas com o id dos sites
row.names(comm.pa) <- envir$Sites # TRUE = 1, FALSE = 0
# agora a soma
sum(comm.pa[1, ])

# calculo de riqueza total
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)

######################################################################
# manipulando banco de dados                                         #
######################################################################

# juntando tabelas diferentes por meio de id comuns ####
# add a coluna de coordenadas ao objeto envir.
# chave primária de envir: Sites
envir$Sites
summary(envir$Sites)
# transformando a envir$Sites (o identificador de áreas) em uma classe de vetor, e não em número.
class(envir$Sites)
as.factor(envir$Sites)
envir$Sites <- as.factor(envir$Sites)
coord$Sites <- as.factor(coord$Sites) # o mesmo para coord$Sites

# unindo os objetos coord e envir
envir.coord <- merge(x = envir,
                     y = coord,
                     by = "Sites") # ou você pode alterar a ordem de montagem:

envir.coord <- merge(x = coord,
                     y = envir,
                     by = "Sites")

# chegando a junção
dim(envir)
dim(coord)
dim(envir.coord)
head(envir.coord)

# criando uma tabela de dados integrada: matriz de espécies por área #####
# linhas: abundância de uma espécie em uma determinada área, n = 56
# colunas: variaveis ambientais, n = 97
# matriz final: n = 5432 linhas (97 x 56)

# vetor de sites
Sites <- envir$Sites
length(Sites)

# vetor n sp
n.sp <- nrow(splist)
n.sp

# criando tabela com cada especie em cada area especies em linhas
comm.df <- tidyr::gather(comm[, -1])
dim(comm.df)
head(comm.df)

# alterando os nomes das colunas comm.df
# nomes atuais
colnames(comm.df)
# modificando os nomes das colunas
colnames(comm.df) <-  c("TaxCode", "Abundance")
# checando os novos nomes
colnames(comm.df)

# adicionando a coluna Sites ao novo obj. isso tem que ser feito repetidas vezes, onde as 97 localidades se repentem 56 x (que é o num de espécies), para ao final, termos 5432 elementos
seq.site <- rep(Sites, each = n.sp) # sequencia
length(seq.site) # checando a dimensão
comm.df$Sites <- seq.site # add ao objeto comm.df
head(comm.df) # checando

# juntando todas as variáveis à comm.df ####
# obs: as tabelas são sempre unidas par a par

# comm.df e splist
comm.sp <- merge(comm.df, splist, by = "TaxCode")
head(comm.sp)

# comm.sp e traits
names(traits)
# renomeando o primeiro elemento
colnames(traits)[1] <- "TaxCode"

comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)

# comm.traits e envir.coord
# obs: envir.coord já é uma tabela unida de var ambientais + as coord. ver linhas 78 a 99
comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)

# exportanto a planilha final modificada
write.csv(x = comm.total,
          file = "dados/planilha_final_combinada.csv",
          row.names = FALSE)

# não entendi por ao final, a planilha começa com o site 1, e vai pro site 10. onde estão os intermediários?
