#' \code{xabmerge} produces \code{t-x} trajectories for a pair of lead and following vehicles.
#'
#' @return The \code{xabmerge}, a wrapper function for \code{bmfree2}, \code{xabparam} and
#' \code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} trajectory.
#' for the following vehicle. The lead vehicle trajectory is not affected.
#' Incidentally, inputs \code{df1} and \code{df2} were created with  \code{bmfree2} and used
#' for testing.
#' @param df1 leading vehicle, a matrix
#' @param df2 following vehicle, a matrix
#' @param leff vehicle length, a number
#' @param step time-step size, a number
#' @param k counter, a number
#' @param type logical for plotting where TRUE creates a plot
#' @usage xabmerge(df1, df2, leff, step, k, type)
# #' @examples
# #' xabmerge(df1, df2, leff, step, k, type)
#' @export
xabmerge <- function(df1, df2, leff, step, k, type) {
  # Adjust df2 if in violation of headway rule
  if(is.null(k)) {
    h.safe <- as.numeric(hsafe(df2[1,2], leff))
    hdwy   <- as.numeric(df1[3] - df2[3])
    if(hdwy < h.safe) {
      tlen   <- dim(df2)[1]
      for(i in 1:tlen) df2[i,3] <- df2[i,3]- h.safe
    }
    k <- 0
  }
  # set up df3, a matrix for car-following
  df3     <- cbind(df1,df2)[,-4]
  if(type == TRUE) lines(df3[,1],df3[,5], lty = 2, lwd = 2, col = "yellow")
  tseq   <- df3[,1]
  tlen   <- length(tseq)
  # Determine safety violations
  h.safe <- hdwy <- violation <- score <- rep(NA,tlen)
  for(i in 1:tlen) {
    h.safe[i] <- hsafe(df3[i,4], leff)
    hdwy[i]   <- df3[i,3] - df3[i,5]
    score[i]  <- h.safe[i] - hdwy[i]
  }
  df3 <- cbind(df3, hdwy, h.safe, violation, score)
  for(i in 1:tlen) {
    if(round(df3[i,6],2) >= round(df3[i,7],2)) df3[i,8] = 0
    else df3[i,8] = 1
  }
  # Determine safety violations continued.
  maxviol  <- max(df3[,9])
  tmaxviol <- df3[df3[,9] == maxviol,1]
  xmaxviol <- df3[df3[,9] == maxviol,5]
  umaxviol <- df3[df3[,9] == maxviol,4]
  # Plot safety violations
  if(maxviol > 0 & type == TRUE) {
    points(tmaxviol, xmaxviol, pch = 16)
    text(tmaxviol, xmaxviol, labels = k, pos = 4)
  }
  browser()
  # Determine if there is a safety violation k.
  for(i in 1:(tlen-2)) {
    if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1) {
      k <- k + 1
      break
    } else {
      k <- 0
    #  print("No headway violations")
    }
  }
  if(k == 1) {
    tstart <- tend <- ustart <- uend <- xstart <- xend <- {}
    # k = 1 and breakdown between tstart and tend
    # print("Breakdown between tstart and tend")
    for(i in 1:(tlen-2)) {
      if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1)
        tstart <- as.numeric(df3[i,1])
      if(df3[i,8] == 1 & df3[i+1,8] == 0)
        tend  <- as.numeric(df3[i,1])
      if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1)
        ustart <- as.numeric(df3[i,4])
      if(df3[i,8] == 1 & df3[i+1,8] == 0)
        uend  <- as.numeric(df3[i,4])
      if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1)
        xstart <- as.numeric(df3[i,5])
      if(df3[i,8] == 1 & df3[i+1,8] == 0)
        xend  <- as.numeric(df3[i,5])
      if(!is.null(tstart) & !is.null(tend)) {
        break
      }
    }
    # breakdown at tend
    if(is.null(tend)) {
      # print("Breakdown at tend")
      tend <- as.numeric(df3[tlen,1])
      uend <- as.numeric(df3[tlen,4])
      xend <- as.numeric(df3[tlen,3]) - as.numeric(df3[tlen,7])
    }
    dfab <- data.frame(tstart, tend, ustart, uend, xstart, xend, step)
    trj  <- trajectoryab(
                tstart = tstart,
                tend   = tend,
                ustart = ustart,
                uend   = uend,
                xstart = xstart,
                xend   = xend,
                step   = step)
    u.fix <- as.numeric(trj[[3]])
    x.fix <- as.numeric(trj[[4]])
    u     <- c(df3[df3[,1] < tstart,4], u.fix, df3[df3[,1] > tend,4])
    x     <- c(df3[df3[,1] < tstart,5], x.fix, df3[df3[,1] > tend,5])
    df3[,4] <- u
    df3[,5] <- x
    if(type == TRUE) lines(df3[,1], df3[,5], lwd = 3, col = "orange")
    return(list(df3, k, dfab))
  }
  if(k == 0) {
    # print("Stop search")
    tstart <- tend <- ustart <- uend <- xstart <- xend <- NA
    dfab <- data.frame(tstart, tend, ustart, uend, xstart, xend, step)
    return(list(df3, k, dfab))
  }
  if(k == 2) {
    # print("k = 2")
    tstart <- tend <- ustart <- uend <- xstart <- xend <- {}
    for(i in 1:(tlen-2)) {
      if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1)
        tstart <- as.numeric(df3[i,1])
      if(df3[i,8] == 1 & df3[i+1,8] == 0)
        tend  <- as.numeric(df3[i,1])
      if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1)
        ustart <- as.numeric(df3[i,4])
      if(df3[i,8] == 1 & df3[i+1,8] == 0)
        uend  <- as.numeric(df3[i,4])
      if(df3[i,8] == 0 & df3[i+1,8] == 1 & df3[i+2,8] == 1)
        xstart <- as.numeric(df3[i,5])
      if(df3[i,8] == 1 & df3[i+1,8] == 0)
        xend  <- as.numeric(df3[i,5])
      if(!is.null(tstart) & !is.null(tend)) break
    }
#    browser()
    dfab     <- data.frame(tstart, tend, ustart, uend, xstart, xend, step)
    tstart1  <- as.numeric(dfab[1])
    ustart1  <- as.numeric(dfab[3])
    xstart1  <- as.numeric(dfab[5])
    tend1    <- as.numeric(tmaxviol)
    uend1    <- as.numeric(umaxviol)
    umn1     <- 3600/5280*uend1
    xend1    <- as.numeric(xmaxviol) - hsafe(umn1,leff)
    if(type == TRUE) {
      points(tend1,xend1, pch = 16)
      text(tend1,xend1, label = k, pos = 4)
    }
    trj1  <- trajectoryab(
      tstart = tstart1,
      tend   = tend1,
      ustart = ustart1,
      uend   = uend1,
      xstart = xstart1,
      xend   = xend1,
      step   = step)
    t.fix1 <- as.numeric(trj1[[2]])
    u.fix1 <- as.numeric(trj1[[3]])
    x.fix1 <- as.numeric(trj1[[4]])
    if(type == TRUE) lines(t.fix1,x.fix1, lwd = 2, col = "wheat")
    # second segment of the trajectory
    tend2   <- as.numeric(dfab[2])
    uend2   <- as.numeric(dfab[4])
    xend2   <- as.numeric(dfab[6])
    tstart2 <- as.numeric(tmaxviol)
    ustart2 <- as.numeric(umaxviol)
    umn2    <- 3600/5280*ustart1
    xstart2 <- as.numeric(xmaxviol) - hsafe(umn1,leff)
    if(type == TRUE) {
      points(tend2,xend2, pch = 16)
      text(tend2,xend2, label = k, pos = 2)
    }

    tstart <- tstart2
    ustart <- ustart2
    xstart <- xstart2
    tend   <- tend2
    ustart <- ustart2
    xstart <- xstart2
    df3.   <- df3[df3[,1] >= tstart,]
    tlen.  <- dim(df3.)[1]
    tend   <- {}
    for(i in 1:tlen.) {
      if(df3.[i,8] == 1 & df3.[i+1,8] == 0) tend  <- as.numeric(df3.[i,1])
      if(df3.[i,8] == 1 & df3.[i+1,8] == 0) uend  <- as.numeric(df3.[i,4])
      if(df3.[i,8] == 1 & df3.[i+1,8] == 0) xend  <- as.numeric(df3.[i,5])
      if(type == TRUE) points(as.numeric(df3.[i,1]),  as.numeric(df3.[i,5]))
      if(!is.null(tend)) break
    }
    trj2   <- trajectoryab(
      tstart = tstart,
      tend   = tend,
      ustart = ustart,
      uend   = uend,
      xstart = xstart,
      xend   = xend,
      step   = step)
    t.fix2 <- as.numeric(trj2[[2]])
    u.fix2 <- as.numeric(trj2[[3]])
    x.fix2 <- as.numeric(trj2[[4]])
    u      <- c(df3[df3[,1] < tstart1,4], u.fix1, u.fix2[-1], df3[df3[,1] > tend,4])
    x      <- c(df3[df3[,1] < tstart1,5], x.fix1, x.fix2[-1], df3[df3[,1] > tend,5])
    df3[,4] <- u
    df3[,5] <- x
    if(type == TRUE) {
      lines(t.fix2,x.fix2, lwd = 2, col = "tan")
      lines(df3[,1], df3[,5], lwd = 2)
    }
    return(list(df3, k, dfab))
  }
}
