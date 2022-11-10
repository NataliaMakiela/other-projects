### Envgraph class lists and plots all the accessible environments


rm(list=ls())
library(rlang)
library(pryr)
library(igraph)

##class envgraph
listAllEnvs <- function(){
  object <- list()
  
  ##search path
  spath <- function(env){
    if(identical(env[[length(env)]],emptyenv())){
      return (env)
    }
    spath(c(env,list((parent.env(env[[length(env)]])))))
  }
  sp <- spath(list(globalenv()))
  
  ##namespaces
  ns<-lapply(X=(.packages()), FUN=ns_env)
  
  ##imports
  im<-lapply(X=(.packages()), FUN=ns_imports_env)
  
  ##environments created by the user
  srodowiska_u <- function(env){
    l<-mget(ls(envir = env, all.names = T), envir = env)
    l2<-sapply(X=l, FUN=is.environment)
    env2 <- parent.env(environment()) 
    if (any(l2)){
      if (!exists('t', envir = env2)){
        env2$t <- l[l2]
      } else {
        env2$t <- c(env2$t, l[l2])
      }
      lapply(l[l2], srodowiska_u) 
    }
    l3 <- list()
    for (i in env2$t){
      l3 <- c(l3, i)
    }
    return(l3)
  }
  sr_uzytkownika <- srodowiska_u(global_env())
  #rm(t)
  ## all envs
  lista <- c(ns, im, sp, sr_uzytkownika)
  
  attr(object, "lista") <- lista
  class(object) <- "envgraph"
  return(object)
}

print.envgraph <- function(x){
  
  cat('--> envgraph object\n\n')
  print(attr(x, "lista"))
  
}

plot.envgraph <- function(x, ...){
  
  envs <- attr(x, "lista")
  
  ##searching for env parents
  parent <- function(env){
    if (identical(env, empty_env())){
      return(empty_env())
    } else {
      return(parent.env(env))
    }
  }
  parent_envs <- lapply(envs, parent)
  
  ###Graph object
  en <-lapply(envs, capture.output)
  pa <- lapply(parent_envs, capture.output)
  d<-data.frame(cbind(en, pa))
  
  ##changing env names into numbers
  mapowanie <- function(kol1, kol2, d){
    d$ind1 <- 1:length(kol1)
    for (i in 1:length(kol2)){
      for (j in 1:length(kol1)){
        if(identical(kol1[[j]], kol2[[i]])){
          d$ind2[i] <- j
        }
      }
    } 
    return(d[, c("ind1", "ind2")])
  }
  graph <- mapowanie(kol1=d$en, kol2=d$pa, d=d)
  graph<-graph_from_data_frame(graph, directed = F)
  
  ##graph
  plot(graph, ...)
  
}

##example

## envs
b<-new.env()
b$a<-new.env(parent = b)
b$c<-new.env(parent = b)
b$a$d<-new.env(parent = b$a)
e<-new.env(parent=emptyenv())
attr(e, "name") <- "test"
e[['f']]<-new.env(parent=e)
g<-new.env(parent=parent.env(global_env()))
g[['h']]<-new.env(parent=g)
i<-new.env(parent=ns_env((.packages())[[1]]))
i[['j']]<-new.env(parent=i)

w <- listAllEnvs()
w
## plot
plot(w, vertex.color = "yellow", vertex.label.cex = .5)
dev.copy(device = png, "./fig1.png")
dev.off()

