library(tidyr)
library(igraph)
library(data.table)
library(tcltk)
library(ggm)
library(plyr)

## ---- test-a --------
#lista1e1

a <- c(3,3,2,2,2,1)
b <- c(6,6,6,4,4,2,2)
c <- c(6,6,6,4,4,3,3)
g <- list(a,b,c)
g
gs <- lapply(g, is_graphical)
gs
gp <- lapply(g, sample_degseq)
layout(c(1,2))
sapply(gp, plot)
layout(c(1))
plot(sample_degseq(g[[3]], method = 'vl'))

#Lista1e3
k10 <- lapply(1:9, sample_k_regular, no.of.nodes=10)
k10
layout(matrix(1:9, nrow=3, byrow=TRUE))
sapply(k10, plot, vertex.label=NA)

pet <- make_graph("Petersen")
V(pet)
E(pet)
tkplot(pet)
plot(pet, main = "Petersen 10:15 K3")
degree(pet)

## ---- test-b --------
#lista1e4

f10 <- lapply(1:9, make_full_graph, directed = FALSE)
f10
layout(matrix(1:12, nrow=3, byrow=TRUE))
sapply(f10, plot, vertex.label=NA)

k5 <- lapply(1:5, sample_k_regular, no.of.nodes=6)
k5
ecount(k5[[2]])
sapply(k5, plot, vertex.label=NA)

c5 <- lapply(k5, complementer)
c5
ecount(c5[[2]])
sapply(c5, plot, vertex.label=NA)

## ---- test-c --------
#lista2

g <- make_star(5, mode = "undirected")
plot(g)
is_bipartite(g)
b <- matrix(c(1,2,1,4,1,5,2,3,3,4,3,5), nc=2, byrow = TRUE)
b <- graph_from_edgelist(b, directed = FALSE)
plot(b)
is_bipartite(b)
b <- as_adjacency_matrix(b) %>% as.matrix(b)
b
fundCycles(b)
cycleMatrix(b)
c <- c(3,3,4,2,2,2)
d <- c(2,2,4,2,2,4)
e <- c(2,3,2,3,3,3)
l <- list(b,c,d,e)
g <- lapply(l, sample_degseq, method = "vl")
g
layout(matrix(1:4, nrow = 2, byrow = TRUE))
sapply(g, plot, vertex.label = NA)
bi <- lapply(g, is_bipartite)
print(bi)

#trab mariana

g <- graph(edges = c("v1","v2","v1","v3","v2","v4","v3","v4","v4","v5"))
g <- as.undirected(g, mode = "collapse")
ma <- as_adjacency_matrix(g, type = "both") %>% as.matrix(g)
plot(g)
ma
g
g <- set_edge_attr(g, "weight", value = c(-3,1,2,6,-5))
map <- as_adjacency_matrix(g, type = "both", attr = "weight") %>% as.matrix(g)
map
g
E(g)$weight
plot(g, vertex.size = 20, edge.label = E(g)$weight, edge.lty = ifelse(E(g)$weight > 0, "solid", "dashed"), layout = layout_nicely(g))

library(BatchGetSymbols)
library(rvest)
library(dplyr)
library(magrittr)
ticker <- c("MSFT","CSCO","V","MMM","GOOG","HD","JPM","CAT","GS","UNH","RTX","BA","DIS","PFE","AMZN","AAPL","XOM","CVX","IBM","INTC","JNJ","MRK","NKE","KO","WMT","FB","MCD","PG","VZ","TSLA")

d30 <- BatchGetSymbols(tickers = ticker,
                       first.date = "2018-01-01",
                       last.date = "2018-12-31",
                       freq.data = "daily",
                       do.cache = TRUE)
getwd()
#save(d30, file = "d30.dta")
load("d30.dta")
d30
d30$df.control$ticker
tail(filter(d30$df.tickers, ticker == "V"))
d30 <- d30$df.tickers
filter(d30, ticker == "GOOG")
filter(d30, is.na(ret.closing.prices))
db <- d30 %>% drop_na(ret.closing.prices)
db <- db[,c('ref.date','ticker','ret.closing.prices')]
db
wdb <- pivot_wider(db, names_from = ticker, values_from = ret.closing.prices)
wdb
tail(wdb)
op <- colnames(wdb[-1])
glimpse(op)
op
mat.cor <- rquery.cormat(select(wdb, -c('ref.date')),
                         type = 'full')
mat.cor
mat.cor <- mat.cor$r

#salvar para aula 2
class(mat.cor)
library(xlsx)
library(Hmisc)
write.xlsx(as.data.frame(mat.cor), file = "matrizcor.xlsx")


cor <- rquery.cormat(select(wdb, -c('ref.date')),
                     type = 'flatten')
cor
cor <- cor$r
cor <- arrange(cor, cor)
head(cor)
tail(cor)
fr <- mean(cor[,3])
fr
g <- graph_from_adjacency_matrix(mat.cor, mode = "undirected", weighted = TRUE, diag = FALSE)
g
plot(g)
str <- strength(g, vids = V(g), mode = "all", loops = FALSE)
str
class(str)
str <- as.data.frame(str)
str <- arrange(str, str)
str
str <- tibble::rownames_to_column(str, "Código da Ação")
str
str[1:12,]
cor <- cor[which(cor$cor > mean(cor[,3])),]
cor
names(cor)[1] <- "from"
names(cor)[2] <- "to"
names(cor)[3] <- "weight"
head(cor) # Edgelist j? filtrada pelo Fator de Relevancia
unique(cor$to)  # possuem links
class(cor)
cor[,1:3]
op
g <- graph_from_data_frame(cor[,1:3], directed = FALSE, vertices = op)
g
#save(g, file = "g.dta")
#load("g.RData")
g
is_weighted(g) # Objeto grafo
E(g)$weight
g[1:5,1:5] #matriz de correla??o
plot(g, layout = layout.fruchterman.reingold,vertex.size = 5, vertex.label.dist = 1, label.cex = 0.1, edge.width = E(g)$weight) # Grafo

adj.mat <- as_adjacency_matrix(g, type = "both")
adj.mat
g.adj <- graph_from_adjacency_matrix(adj.mat, mode = "undirected", weighted = NULL)
g.adj
dg <- degree(g.adj)
dg
class(dg)
dg <- as.data.frame(dg)
dg
dg <- arrange(dg, desc(dg))
dg
eigg <- eigen_centrality(g.adj, scale = FALSE, weights = NULL)$vector
eigg
eigg <- as.data.frame(eigg)
eigg <- arrange(eigg,eigg)
eigg

fit <- fit_power_law(g.adj)
fit

hist(dg$dg)

# plot and fit the power law distribution
fit_power_law = function(graph) {
  # calculate degree
  d = degree(graph)
  dd = degree.distribution(graph, cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       col = 1, main = "Degree Distribution")
  curve(power.law.fit, col = "red", add = T, n = length(d))
}

fit_power_law(g.adj)

g.adj
dg2 <- degree_distribution(g.adj, mode = "all", cumulative = FALSE)
x <- 1:max(degree(g.adj)) - 1
zeros <- (dg2 == 0)
plot(x[!zeros], dg2[!zeros], log = "xy",
     xlab = "log Degree",
     ylab = "Log Frequency")
