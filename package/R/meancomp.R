meancomp.qseq <- function(p,G,gMCP,alpha,gW,h,...){
  ## quasi sequential procedures for weighted comparisons of means
  p <- matrix(p,nrow=1)
  if(ecount(G) != length(p)){
    stop('p-values need to have same length as edges in the comparigram')
  }
  ## number of elementary null hypotheses
  N <- length(p)
  M <- vcount(G)
  ## Initialize index set
  I <- as.integer(1:N)
  ## Initialize rejection set
  R <- integer(0)
  ## precompute gMCP weights
  if(missing('gW')){
    gW <- generateWeights(gMCP@m,gMCP@weights)
    gI <- gW[,1:n]
    h <- apply(gW[,1:n],1,function(i) sum(2^which(i > 0)))
    gW <- gW[,(n+1):(2*n)]
    gW[gI<1] <- Inf
  } else {
    warning("Using precomputed weights")
  }
  ## Initialize weights
  w <- getWeights(I,h,gW)
  ## Bonferroni test, this may be replaced by the stepwise graphical procedure!
  R <- which(p <= w*alpha)
  if(length(R) %in% c(0,N)){
    ## if Bonferroni test rejects all or none stop
    attr(R,'comps') <- 0
    return(R)
  }
  ## remaining hypotheses
  I <- setdiff(I,R)
  el <- t(sapply(I,l2ij,M))
  ## compute hyperplanes
  hpp <- get.hyperplanes(el)
  el.hpp <- lapply(hpp,partition2el,el)
  exhaustive <- lapply(el.hpp,el2intersect,M)
  ## remove rejected hyperplanes
  exhaustive <- prune(exhaustive,R)
  ## count number of operations
  lb <- 0
  ## temporary variable
  vault <- integer(0)
  while(length(R) < N){
    ## get Weights from tree
    w <- sapply(exhaustive,function(I) getWeights(I,h,gW))
    ## take the smallest weight across all hyperplanes
    ## this may be improved doing a full closed testing
    ## procedure
    lb <- length(exhaustive)+lb
    if(!is.matrix(w)){
      w <- w[I]
    } else {
      w <- apply(w[I,,drop=F],1,min,na.rm=T)
    }
    ## it may happen that all hyperplanes containing some hypothesis have been rejected but the hypothesis has not yet been rejected, so we keep it for the next level, basically this means that we may go on even if nothing can be rejected locally
    vault <- c(vault,I[w == Inf])
    ## reject locally
    r <- integer(0)
    r <- setdiff(I[p[I]<=w*alpha],vault)
    ## if nothing can be rejected locally
    if(length(r) + length(vault) == 0){
      ## stop
      attr(R,'comps') <- lb
      return(R)
    } 
    R <- c(R,r)
    I <- setdiff(I,R)
    exhaustive <- prune(exhaustive,R)
    if(length(I) <= 1){
      if(length(I) == 0){
        attr(R,'comps') <- lb
        return(R)
      }
      ## the procedure may not be exhaustive, so we can't simply use alpha
      if(p[I] <= getWeights(I,h,gW)[I]*alpha){
        attr(R,'comps') <- lb
        return(c(R,I))
      } else {
        attr(R,'comps') <- lb
        return(R)
      }
    }
    if(length(exhaustive) == 0){
      I <- c(I,vault)
      ## go to next smaller Intersection
      ## restrict comparigram
      ## get hyperplanes
      el <- intersect2el(I,M)
      hpp <- get.hyperplanes(el)
      el.hpp <- lapply(hpp,partition2el,el)
      exhaustive <- lapply(el.hpp,el2intersect,M)
    }
  }
  attr(R,'comps') <- lb
  R
} 
