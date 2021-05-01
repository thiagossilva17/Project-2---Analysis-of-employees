BD<- read.csv(file.choose(), na.strings = "", sep = ";")

colnames(BD)
colnames(BD) <- c("individuo", "salario", "posicao", "anosexperiencia", "sexo")
head(BD)

#CHECANDO PORCENTAGEM DE DADOS FALTANTES DE CADA VARIAVEL
NAs<- round(colSums(is.na(BD))*100/nrow(BD),2)
NAs
#CHECANDO SE EXISTE ALGUM DADO FALTANTE NA BASE
anyNA(BD)

#CHECANDO DIMENSAO DO CONJUNTO DE DADOS
dim(BD)

#Explorando dados
propsexo <- round(table(BD$sexo)*100/nrow(BD),2)
propsexo

#Removendo variavel ID
library(dplyr)
BDclusters <- BD %>% select(-1)

#Visualizando correlacoes
library(GGally)
ggcorr(BD, label=T)

#Verficando o tipo das colunas
str(BDclusters)

#Substituindo ponto por virgula na variavel anosexperiencia e formatando para tipo numerico
anosexperiencia <- sapply(BDclusters, function(x) any(grepl(",", x)))
BDclusters$anosexperiencia <- sapply(BDclusters[,c("anosexperiencia")], function(x) as.numeric(sub(",", ".", x)))
#Verficando o tipo das colunas
str(BDclusters)


#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
dados<- scale(BDclusters[,c(1:4)])

#ENCONTRANDO O NUMERO IDEAL DE CLUSTERS
library(factoextra)
#EXECUTANDO ALGORITMO KMEANS E METODO GAP_STAT PARA ENCONTRAR O NUMERO IDEAL DE CLUSTERS
fviz_nbclust(dados, kmeans, method= "gap_stat")


library(caret)
#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters, method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)
clusters<- kmeans(BDclusters, centers=3)

dendograma <- hclust(dist(BDclusters))
plot(dendograma)
#Dividindo o dendograma em 3 grupos com cores diferentes
y = cutree(dendograma, 3)
ColorDendrogram(dendograma, y = y, labels = names(y), main = "Dendograma", branchlength = 80)

#Visualizando grupos
fviz_cluster(clusters, data = BDclusters,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#CLUSTERS
clusters$cluster
#CENTROS DOS CLUSTERS
clusters$centers
#TAMANHO DOS CLUSTERS
clusters$size

#INSERINDO CLUSTERS NO DATASET E VISUALIZANDO A CLASSIFICACAO EM TABELA  
BD$Cluster <- clusters$cluster
#head(BDclusters)
head(BD)

#Proporcao da quantidade de individuos por grupo
proporcaocluster <- round(table(BD$Cluster)*100/nrow(BD),2)
proporcaocluster

#substituindo ponto por virgula
anosexperiencia <- sapply(BD, function(x) any(grepl(",", x)))
BD$anosexperiencia <- sapply(BD[,c("anosexperiencia")], function(x) as.numeric(sub(",", ".", x)))
#Verficando o tipo das colunas
str(BD)
BD$anosexperiencia <- round(BD$anosexperiencia,0)

#Alterando tipos das variaveis
str(BD)
BD$individuo <- as.factor(BD$individuo)
BD$posicao <- as.factor(BD$posicao)
BD$sexo <- as.factor(BD$sexo)
BD$Cluster <- as.factor(BD$Cluster)

#Plotando grupos
library(ggplot2)

#Sexo barras
ggplot(BD) +
  aes(x = sexo, fill = sexo) +
  geom_bar() +
  scale_fill_hue() +
  theme_gray() +
  facet_wrap(vars(Cluster))

#salario histogram
ggplot(BD) +
  aes(x = salario, fill = Cluster) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_gray() +
  facet_wrap(vars(Cluster))

#Explorando os salarios dos grupos
mediasalario <- BD %>% group_by(Cluster) %>% summarise(media.salario= mean(salario))
minimosalario <- BD %>% group_by(Cluster) %>% summarise(minimo.salario= min(salario))
maxsalario <- BD %>% group_by(Cluster) %>% summarise(max.salario= max(salario))
cbind(minimosalario, mediasalario, maxsalario)

#Anos experiencia boxplot
ggplot(BD) +
  aes(x = "", y = anosexperiencia, fill = Cluster) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_gray() +
  facet_wrap(vars(Cluster))

#Posicao barras
ggplot(BD) +
  aes(x = posicao, fill = Cluster) +
  geom_bar() +
  scale_fill_hue() +
  theme_gray() +
  facet_wrap(vars(Cluster))

