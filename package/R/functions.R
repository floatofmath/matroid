ij2l <- function(ij,m){
  ## convert mean indices to a unique number
  ## assume i<j
  sum(m:(m-ij[1]+1))+(ij[2]-ij[1])-m
}

l2ij <- function(l,m){
  ## convert number to unique mean
  ## indeces such that i<j and i,j<=m
  i <- sum(cumsum((m-1):1) <= (l-1))
  j <- l%%sum((m-1):(m-i))+i
  c(i+1,j+1)
}

I2L <- function(I,m){
  ## convert an index set to its combinadic
  k <- length(I)
  if(k == 1){
    return(I)
  }
  ## we do not assume that I is sorted
  I <- sort(I)
  sum(choose(m,1:k-1)) + sum(choose(I-1,1:k)) 
}

L2I <- function(L,m){
  ## convert combinadic to index set
  cs <- cumsum(choose(m,1:(m-1)))
  ## no of hypotheses
  k <- sum(cs < L)+1
  if(k == 1){
    return(L)
  }
  L <- L - cs[k-1] - 1
  if(L == 0){
    return(1:k)
  }
  I <- integer(k)
  ## greedy algorithm to find combination
  for(i in k:1){
    cs <- choose(1:m,i)
    h <- max(c(i-1,which(cs <= L)))
    L <- L-cs[h]
    I[i] <- h+1
  }
  return(I)
}

means <- function(el){
  ## get the vertices of an edge list
  sort(unique(as.numeric(el)))
}

el2partition <- function(el){
  ## get the partition representation of an edge list
  ## uses igraph
  part <- list()
  elc <- matrix(as.character(el),nrow(el),ncol(el))
  coco <- clusters(graph.edgelist(elc))
  for(i in 1:coco$no){
  ## beach boys yeah
    part[[i]] <- means(el)[coco$membership == i]
  }
  return(part)
}

partition2closedel <- function(part){
  ## get closure of a partition
  el <- do.call('cbind',lapply(part,function(p){
    if(length(p)<2) return(NULL)
    combn(p,2)
  }))
  if(is.matrix(el)){
    return(t(el))
  } else {
    return(NULL)
  }
}

intersectel <- function(al,bl){
  ## restrict al to bl
  ## id <- (al[,1] %in% bl[,1]) & (al[,2] %in% bl[,2])
  ## al[id,]
  m <- max(max(al),max(bl))
  ai <- paste(al[,1],al[,2],sep='')
  ai <- paste(bl[,1],bl[,2],sep='')
  al[ai %in% bi,]
}

partition2el <- function(part,EL){
  ## turn a partion into an edgelist
  ## EL is the Edgelist of the parent graph
  el <- partition2closedel(part)
  intersectel(el,EL)
}

el2intersect <- function(el,m){
  ## turn an edgelist into an index set
  if(!is.matrix(el)){
    ij2l(el,m)
  } else {
    apply(el,1,ij2l,m)
  }
}

intersect2el <- function(set,m){
  ## turn an index set into an edgelist
  t(sapply(set,l2ij,m))
}

to.bin <- function(int,n=floor(log2(int))+1){
  ## binary conversion
  if(n+2<=floor(log2(int))){
    stop('Vector length to small to hold binary number')
  }
  ((int)%/% 2^((n:1)-1))%%2
}

refineOne <- function(set){
 ## split a set into two
 N <- length(set)
 codewords <- sapply(1:(2^(N-1)-1),to.bin,N)
 ## one may remove singleton sets somehow
 apply(codewords,2,function(codeword) list(set[codeword == 0],set[codeword==1]))
}

refine <- function(part){
  ## refine a partition
  ## should be improved
  M <- length(part)
  if(M < 2){
    return(refineOne(part[[1]]))
  }
  rpart <- list()
  for(i in 1:M){
    ref <- refineOne(part[[i]])
    ref <- lapply(ref,function(r) c(head(part,i-1),r,tail(part,-i)))
    rpart <- c(rpart,ref)
  }
  rpart
}

partition <- function(G){
  ## compute partition corresponding to el
  ## uses igraph
  el <- get.edgelist(G)
  el2partition(el)
}

get.hyperplanes <- function(el){
  ## get hyperplanes of graph
  p <- el2partition(el)prune <- function(ex,R){
  has <- sapply(ex,function(e) any(R %in% e))
  ex[which(!has)]
}

  refine(p)
}

prune <- function(ex,R){
  ## remove sets that contain rejected elementary hypotheses
  has <- sapply(ex,function(e) any(R %in% e))
  ex[which(!has)]
}

getWeights <- function(I,h,gW){
  ## get weights for intersection I from gMCP
  ## !!! this is a tremendously stupid way to compute the weights !!!
  ## use combinadics or sth. and maybe markov chain stuff to compute weights as needed
  warning("Brain damaged getWeights function")
  I <- which(h == sum(2^I))
  return(gW[I,])
}

restrict <- function(G,I){
  ## restrict comparigram to intersection hypothesis
  ## uses igraph
  subgraph.edges(G,which(I>0))
}





