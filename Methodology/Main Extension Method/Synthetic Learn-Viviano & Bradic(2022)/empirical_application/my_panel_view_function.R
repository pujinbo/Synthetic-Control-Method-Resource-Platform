my_panelView <- function(data, # a data frame (long-form)
                      formula = NULL,
                      Y = NULL,
                      D = NULL,
                      X = NULL,
                      index, # c(unit, time) indicators
                      na.rm = TRUE, # remove missing values
                      treatment = TRUE,
                      outcome.type = "continuous", # continuous or discrete
                      type = "treat", ## treat or outcome
                      by.group = FALSE, # (color pre-treatment treated differently)
                      theme.bw = FALSE,
                      xlim = NULL, 
                      ylim = NULL,
                      xlab = NULL, 
                      ylab = NULL,
                      gridOff = FALSE,
                      legendOff = FALSE,
                      legend.labs = NULL,
                      main = NULL,
                      id = NULL,
                      show.id = NULL,
                      color = NULL,
                      axis.adjust = FALSE,
                      axis.lab = "both",
                      axis.lab.gap = c(0, 0),
                      shade.post = TRUE, 
                      my_gap = 30) {  
  varnames <- all.vars(formula)
  ## outcome
  Y <- varnames[1]
  ## treatment indicator and covar
  D <- varnames[2]
  X <- NULL
  data <- dd
  ## exclude other covariates 
  data <- data[,c(index, Y, D, X)] 
  type <- "outcome"

  ## check treatment indicator
  d.level <- NULL
  d.bi <- FALSE
  id = NULL 
    
    d.level <- sort(unique(data[, D]))
    d.bi <- d.level[1] == 0 & d.level[2] == 1 & length(d.level) == 2
    
    
  
  ## ------------------------ ##
  ## parsing data.            ##
  ## ------------------------ ##
  
  ## index names
  index.id <- index[1]
  index.time <- index[2]
  
  ## raw id and time
  raw.id <- sort(unique(data[,index[1]]))
  raw.time <- sort(unique(data[,index[2]]))
  N <- length(raw.id)
  TT <- length(raw.time)
  

  
  ## store variable names
  ## data.old <- data
  Yname <- Y
  Dname <- D
  Xname <- X
  
  
  ## id to be plotted 
  input.id <- NULL
  if (!is.null(id)) {
    if (!is.null(show.id)) {
      warning("Using specified id.\n")
    }
    ## check id 
    remove.id <- setdiff(id, raw.id)
    if (length(remove.id) != 0) {
      cat("list of removed units from dataset:", remove.id)
      cat("\n\n")
      input.id <- intersect(sort(id), raw.id)
    } else {
      input.id <- sort(id)
    }
  } else {
    if (!is.null(show.id)) {
      
      if (length(show.id) > N ) {
        stop("Length of \"show.id\" should not be larger than total number of units. \n")
      }
      if (!class(show.id) %in% c("numeric", "integer")) {
        stop("\"show.id\" option misspecified. Try, for example, show.id = 1:100. \n")
      }
      if (sum(show.id > N) > 0) {
        stop("Some specified units are not in the data.\n")
      }
      if (length(unique(show.id)) != length(show.id)) {
        stop("Repeated values in \"show.id\" option.")
      }
      
      input.id <- raw.id[show.id]
      
    } else {
      input.id <- raw.id
    }
  }
  
  

  ## sort data
  data <- data[order(data[,index.id], data[,index.time]), ]
  
  id.all <- time.all <- count <- coordin <- data.x <- x.na <- NULL
  
  ## check balanced panel and fill unbalanced panel
  Y <- I <- D <- NULL
  if (dim(data)[1] != TT*N) {
    
    data[,index.id] <- as.numeric(as.factor(data[,index.id]))
    data[,index.time] <- as.numeric(as.factor(data[,index.time]))
    
    if (!is.null(Yname)) {
      Y <- matrix(NA, TT, N)
    }
    I <- matrix(0, TT, N)
    if (treatment == 1) {
      D <- matrix(0, TT, N)
    }
    for (i in 1:dim(data)[1]) {
      if (!is.null(Yname)) {
        Y[data[i,index.time],data[i,index.id]] <- data[i,Yname]
      }
      if (treatment == 1) {
        D[data[i,index.time],data[i,index.id]] <- data[i,Dname]
      }
      I[data[i,index.time],data[i,index.id]] <- 1
    }
    
  } else {
    I <- matrix(1, TT, N)
    if (!is.null(Yname)) {
      Y <- matrix(data[,Yname], TT, N)
    }
    if (treatment == 1) {
      D <- matrix(data[,Dname], TT, N)
    }
  }
  
  
  
  ## binary treatment indicator 
    D.old <- D ## store the original indicators
      
    ## once treated, always treated
    D <- apply(D, 2, function(vec){cumsum(vec)})
    D <- ifelse(D > 0, 1, 0)
    
    ## timing
    tr.pos <- which(D[TT,] == 1)
  
    T0 <- apply(D == 0, 2, sum)[tr.pos] ## first time expose to treatment
    DID <- length(unique(T0)) == 1 ## DID type 
    
    ## check DID mode
    if (sum(abs(D.old[which(I==1)] - D[which(I==1)])) == 0) {
      by.group <- by.group
      FEmode <- 0
    }

  
  ## missing matrix 
  
  ########################################
  ## unified labels:
  ##  -2 for missing
  ##  -1 for control condition (or observed)
  ##   0 for treated pre
  ##   1 for treated post  
  ########################################
  
  obs.missing <- NULL
  
  if (treatment == 1 && d.bi == 1) {
    
    con1 <- type == "treat" && no.pre.post == FALSE 
    con2 <- type == "outcome" && by.group == FALSE
    
    if (FEmode == 0 && (con1 || con2)) {  ## DID type data
      
      tr <- D[TT,] == 1     # cross-sectional: treated unit
      
      id.tr <- which(tr==1)
      id.co <- which(tr==0)
      
      D.tr <- as.matrix(D[,which(tr==1)])
      I.tr <- as.matrix(I[,which(tr==1)])
      Y.tr <- as.matrix(Y[,which(tr==1)])
      Y.co <- as.matrix(Y[,which(tr==0)])
      
      Ntr <- sum(tr)
      Nco <- N - Ntr
      
      ## 1. control group: -1
      obs.missing <- matrix(-1, TT, N) 
      ## 2. add treated units
      obs.missing[, id.tr] <- D[, id.tr]
      ## 3. set missing values
      obs.missing[which(I==0)] <- -2 ## missing -2
      
      unit.type <- rep(1, N) ## 1 for control; 2 for treated; 3 for reversal
      unit.type[id.tr] <- 2
      
    } else {
      
      unit.type <- rep(NA, N) ## 1 for control; 2 for treated; 3 for reversal
      
      for (i in 1:N) {
        di <- D.old[, i]
        ii <- I[, i]
        if (length(unique(di[which(ii==1)])) == 1) { ## treated or control
          if (0 %in% unique(di[which(ii==1)])) {
            unit.type[i] <- 1 ## control
          } else {
            unit.type[i] <- 2 ## treated
          }
        } else {
          unit.type[i] <- 3 ## reversal
        }
      }
      
      ## 1. using D.old  
      obs.missing <- D.old 
      ## 2. set controls
      obs.missing[which(D.old == 0)] <- -1 ## under control
      ## 3. set missing 
      obs.missing[which(I==0)] <- -2 ## missing
    }
    
    obs.missing.treat <- obs.missing
    if (length(unique(c(D.old))) > 2) {
      obs.missing[which(obs.missing > 1)] <- 1
    }
    
  } else {
    
    if (length(d.level) > 0 && type == "treat") { ## multiple treatment levels
      
      obs.missing <- D
      obs.missing[which(I == 0)] <- NA
      
    } else {
      
      obs.missing <- matrix(-1, TT, N) ## observed 
      obs.missing[which(I==0)] <- -2 ## missing
      treatment <- 0
      
    }
    
  }
  
  colnames(obs.missing) <- input.id
  rownames(obs.missing) <- raw.time
  
  
  time <- raw.time
  id <- input.id 
  
  ## ------------------------------------- ##
  ##          part 2: plot
  ## ------------------------------------- ##
  
  outcome <- NULL ## global variable
  labels1 <- labels2 <- labels3 <- NULL
  xlab <- 'Year'
  ylab = 'Percentage'
  legendOff = F
  axis.adjust = T
  if (axis.adjust == TRUE) {
    angle <- 45
    x.v <- 1
    x.h <- 1
  } else {
    angle <- 0
    x.v <- 0
    if (type == "treat") {
      x.h <- 0.5
    } else {
      x.h <- 0
    }
  }
  
  ## type of plots
  if (!is.numeric(time[1])) {
    time <- 1:TT
  }
  show <- 1:length(time)
  
  nT <- length(show)
  time.label <- raw.time[show]
  T.b <- 1:length(show)
  axis.lab.gap = c(my_gap, 0)
  
  ## labels
  N.b <- 1:N
  x.gap <- axis.lab.gap[1]

  if (x.gap != 0) {
    T.b <- seq(from = 1, to = length(show), by = (x.gap + 1))
  }
  legend.pos <- "bottom"
  theme.bw = F
  
  ############  START  ###############    
  if (type == "outcome") {
    
  
 
          raw.color <- c("blue", "#FC8D6280", "red")
 
    outcome.type = "continuous"
 if (treatment == 1 && by.group == FALSE) { ## Mixed units
      
      ## time-line
      if (outcome.type == "continuous") { ## continuous outcome
        
     
          
          time.bf <- time[unique(T0)]
          pst <- D.tr
          
          for (i in 1:Ntr) {
            pst[T0[i], i] <- 1 ## paint the period right before treatment
          }
          
          time.pst <- c(pst[show,] * time[show])
          time.pst <- time.pst[which(c(pst[show,])==1)]
          Y.tr.pst <- c(Y.tr[show,])[which(pst[show,]==1)]
          id.tr.pst <- matrix(rep(1:Ntr,each=TT),TT,Ntr,byrow=FALSE)[show,]
          id.tr.pst <- c(id.tr.pst)[which(pst[show,]==1)]
          
          data <- cbind.data.frame("time" = c(rep(time[show], N), time.pst),
                                   "outcome" = c(c(Y.tr[show,]),
                                                 c(Y.co[show,]),
                                                 Y.tr.pst),
                                   "type" = c(rep("tr",(Ntr*nT)),
                                              rep("co",(Nco*nT)),
                                              rep("tr.pst",length(Y.tr.pst))),
                                   "id" = c(rep(1:N,each = nT), id.tr.pst*(-1)))
          
          ## legend
          legend.labs = NULL
          set.limits = c("co", "tr", "tr.pst")
          set.colors = c(raw.color[1], raw.color[2], raw.color[3])
          set.linetypes = c("solid","solid","solid")
          set.linewidth = c(0.5, 0.5, 0.5)
        
            set.labels <- c("Controls","Treated (Pre)","Treated (Post)") 
        
          labels.ncol <- 3
          
    #else { ## FE mode data
         
        
        
        ## theme
        p <- ggplot(data) + xlab(xlab) +  ylab(ylab)
        
        p <- p + theme(legend.position = legend.pos,
                       axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                       plot.title = element_text(size=20, hjust = 0.5, face="bold"))
        
        if (DID == TRUE && Ntr >= 1) {
          if (time.bf >= min(show) && time.bf <= max(show)) {
            p <- p + geom_vline(xintercept=time.bf,colour="white",size = 2)
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,
                                ymin=-Inf, ymax=Inf, alpha = .3) 
            }                            
          }
        }
        
        ## main
        p <- p + geom_line(aes(time, outcome,
                               colour = type,
                               size = type,
                               linetype = type,
                               group = id))
        
        set.linewidth[1] <- 0.05
        set.linewidth[c(2,3)] <-1
        p <- p + scale_colour_manual(limits = set.limits,
                                     labels = set.labels,
                                     values =set.colors) +
          scale_linetype_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linetypes) +
          scale_size_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linewidth) +
          guides(linetype = guide_legend(title=NULL, ncol=labels.ncol),
                 colour = guide_legend(title=NULL, ncol=labels.ncol),
                 size = guide_legend(title=NULL, ncol=labels.ncol))
        
      }  
      
        
          p <- p + ggtitle(main)
      
      #  p <- p + coord_cartesian(ylim = ylim)
      
    
      ## end of raw plot
      
    
    p <- p + 
      scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
    p  
 }
  }
  }
    




my_panelView2 <- function(data, # a data frame (long-form)
                         formula = NULL,
                         Y = NULL,
                         D = NULL,
                         X = NULL,
                         index, # c(unit, time) indicators
                         na.rm = TRUE, # remove missing values
                         treatment = TRUE,
                         outcome.type = "continuous", # continuous or discrete
                         type = "treat", ## treat or outcome
                         by.group = FALSE, # (color pre-treatment treated differently)
                         theme.bw = FALSE,
                         xlim = NULL, 
                         ylim = NULL,
                         xlab = NULL, 
                         ylab = NULL,
                         gridOff = FALSE,
                         legendOff = FALSE,
                         legend.labs = NULL,
                         main = NULL,
                         id = NULL,
                         show.id = NULL,
                         color = NULL,
                         axis.adjust = FALSE,
                         axis.lab = "both",
                         axis.lab.gap = c(0, 0),
                         shade.post = TRUE) {  
  varnames <- all.vars(formula)
  ## outcome
  Y <- varnames[1]
  ## treatment indicator and covar
  D <- varnames[2]
  X <- NULL
  data <- dd
  ## exclude other covariates 
  data <- data[,c(index, Y, D, X)] 
  type <- "outcome"
  
  ## check treatment indicator
  d.level <- NULL
  d.bi <- FALSE
  id = NULL 
  
  d.level <- sort(unique(data[, D]))
  d.bi <- d.level[1] == 0 & d.level[2] == 1 & length(d.level) == 2
  
  
  
  ## ------------------------ ##
  ## parsing data.            ##
  ## ------------------------ ##
  
  ## index names
  index.id <- index[1]
  index.time <- index[2]
  
  ## raw id and time
  raw.id <- sort(unique(data[,index[1]]))
  raw.time <- sort(unique(data[,index[2]]))
  N <- length(raw.id)
  TT <- length(raw.time)
  
  
  
  ## store variable names
  ## data.old <- data
  Yname <- Y
  Dname <- D
  Xname <- X
  
  
  ## id to be plotted 
  input.id <- NULL
  if (!is.null(id)) {
    if (!is.null(show.id)) {
      warning("Using specified id.\n")
    }
    ## check id 
    remove.id <- setdiff(id, raw.id)
    if (length(remove.id) != 0) {
      cat("list of removed units from dataset:", remove.id)
      cat("\n\n")
      input.id <- intersect(sort(id), raw.id)
    } else {
      input.id <- sort(id)
    }
  } else {
    if (!is.null(show.id)) {
      
      if (length(show.id) > N ) {
        stop("Length of \"show.id\" should not be larger than total number of units. \n")
      }
      if (!class(show.id) %in% c("numeric", "integer")) {
        stop("\"show.id\" option misspecified. Try, for example, show.id = 1:100. \n")
      }
      if (sum(show.id > N) > 0) {
        stop("Some specified units are not in the data.\n")
      }
      if (length(unique(show.id)) != length(show.id)) {
        stop("Repeated values in \"show.id\" option.")
      }
      
      input.id <- raw.id[show.id]
      
    } else {
      input.id <- raw.id
    }
  }
  
  
  
  ## sort data
  data <- data[order(data[,index.id], data[,index.time]), ]
  
  id.all <- time.all <- count <- coordin <- data.x <- x.na <- NULL
  
  ## check balanced panel and fill unbalanced panel
  Y <- I <- D <- NULL
  if (dim(data)[1] != TT*N) {
    
    data[,index.id] <- as.numeric(as.factor(data[,index.id]))
    data[,index.time] <- as.numeric(as.factor(data[,index.time]))
    
    if (!is.null(Yname)) {
      Y <- matrix(NA, TT, N)
    }
    I <- matrix(0, TT, N)
    if (treatment == 1) {
      D <- matrix(0, TT, N)
    }
    for (i in 1:dim(data)[1]) {
      if (!is.null(Yname)) {
        Y[data[i,index.time],data[i,index.id]] <- data[i,Yname]
      }
      if (treatment == 1) {
        D[data[i,index.time],data[i,index.id]] <- data[i,Dname]
      }
      I[data[i,index.time],data[i,index.id]] <- 1
    }
    
  } else {
    I <- matrix(1, TT, N)
    if (!is.null(Yname)) {
      Y <- matrix(data[,Yname], TT, N)
    }
    if (treatment == 1) {
      D <- matrix(data[,Dname], TT, N)
    }
  }
  
  
  
  ## binary treatment indicator 
  D.old <- D ## store the original indicators
  
  ## once treated, always treated
  D <- apply(D, 2, function(vec){cumsum(vec)})
  D <- ifelse(D > 0, 1, 0)
  
  ## timing
  tr.pos <- which(D[TT,] == 1)
  
  T0 <- apply(D == 0, 2, sum)[tr.pos] ## first time expose to treatment
  DID <- length(unique(T0)) == 1 ## DID type 
  
  ## check DID mode
  if (sum(abs(D.old[which(I==1)] - D[which(I==1)])) == 0) {
    by.group <- by.group
    FEmode <- 0
  }
  
  
  ## missing matrix 
  
  ########################################
  ## unified labels:
  ##  -2 for missing
  ##  -1 for control condition (or observed)
  ##   0 for treated pre
  ##   1 for treated post  
  ########################################
  
  obs.missing <- NULL
  
  if (treatment == 1 && d.bi == 1) {
    
    con1 <- type == "treat" && no.pre.post == FALSE 
    con2 <- type == "outcome" && by.group == FALSE
    
    if (FEmode == 0 && (con1 || con2)) {  ## DID type data
      
      tr <- D[TT,] == 1     # cross-sectional: treated unit
      
      id.tr <- which(tr==1)
      id.co <- which(tr==0)
      
      D.tr <- as.matrix(D[,which(tr==1)])
      I.tr <- as.matrix(I[,which(tr==1)])
      Y.tr <- as.matrix(Y[,which(tr==1)])
      Y.co <- as.matrix(Y[,which(tr==0)])
      
      Ntr <- sum(tr)
      Nco <- N - Ntr
      
      ## 1. control group: -1
      obs.missing <- matrix(-1, TT, N) 
      ## 2. add treated units
      obs.missing[, id.tr] <- D[, id.tr]
      ## 3. set missing values
      obs.missing[which(I==0)] <- -2 ## missing -2
      
      unit.type <- rep(1, N) ## 1 for control; 2 for treated; 3 for reversal
      unit.type[id.tr] <- 2
      
    } else {
      
      unit.type <- rep(NA, N) ## 1 for control; 2 for treated; 3 for reversal
      
      for (i in 1:N) {
        di <- D.old[, i]
        ii <- I[, i]
        if (length(unique(di[which(ii==1)])) == 1) { ## treated or control
          if (0 %in% unique(di[which(ii==1)])) {
            unit.type[i] <- 1 ## control
          } else {
            unit.type[i] <- 2 ## treated
          }
        } else {
          unit.type[i] <- 3 ## reversal
        }
      }
      
      ## 1. using D.old  
      obs.missing <- D.old 
      ## 2. set controls
      obs.missing[which(D.old == 0)] <- -1 ## under control
      ## 3. set missing 
      obs.missing[which(I==0)] <- -2 ## missing
    }
    
    obs.missing.treat <- obs.missing
    if (length(unique(c(D.old))) > 2) {
      obs.missing[which(obs.missing > 1)] <- 1
    }
    
  } else {
    
    if (length(d.level) > 0 && type == "treat") { ## multiple treatment levels
      
      obs.missing <- D
      obs.missing[which(I == 0)] <- NA
      
    } else {
      
      obs.missing <- matrix(-1, TT, N) ## observed 
      obs.missing[which(I==0)] <- -2 ## missing
      treatment <- 0
      
    }
    
  }
  
  colnames(obs.missing) <- input.id
  rownames(obs.missing) <- raw.time
  
  
  time <- raw.time
  id <- input.id 
  
  ## ------------------------------------- ##
  ##          part 2: plot
  ## ------------------------------------- ##
  
  outcome <- NULL ## global variable
  labels1 <- labels2 <- labels3 <- NULL
  xlab <- 'Year'
  ylab = 'Percentage'
  legendOff = F
  axis.adjust = T
  if (axis.adjust == TRUE) {
    angle <- 45
    x.v <- 1
    x.h <- 1
  } else {
    angle <- 0
    x.v <- 0
    if (type == "treat") {
      x.h <- 0.5
    } else {
      x.h <- 0
    }
  }
  
  ## type of plots
  if (!is.numeric(time[1])) {
    time <- 1:TT
  }
  show <- 1:length(time)
  
  nT <- length(show)
  time.label <- raw.time[show]
  T.b <- 1:length(show)
  axis.lab.gap = c(30, 0)
  
  ## labels
  N.b <- 1:N
  x.gap <- axis.lab.gap[1]
  
  if (x.gap != 0) {
    T.b <- seq(from = 1, to = length(show), by = (x.gap + 1))
  }
  legend.pos <- "bottom"
  theme.bw = F
  
  ############  START  ###############    
  if (type == "outcome") {
    
    
    
    raw.color <- c("blue", "#FC8D6280", "red")
    
    outcome.type = "continuous"
    if (treatment == 1 && by.group == FALSE) { ## Mixed units
      
      ## time-line
      if (outcome.type == "continuous") { ## continuous outcome
        
        
        
        time.bf <- time[unique(T0)]
        pst <- D.tr
        
        for (i in 1:Ntr) {
          pst[T0[i], i] <- 1 ## paint the period right before treatment
        }
        
        time.pst <- c(pst[show,] * time[show])
        time.pst <- time.pst[which(c(pst[show,])==1)]
        Y.tr.pst <- c(Y.tr[show,])[which(pst[show,]==1)]
        id.tr.pst <- matrix(rep(1:Ntr,each=TT),TT,Ntr,byrow=FALSE)[show,]
        id.tr.pst <- c(id.tr.pst)[which(pst[show,]==1)]
        
        data <- cbind.data.frame("time" = c(rep(time[show], N), time.pst),
                                 "outcome" = c(c(Y.tr[show,]),
                                               c(Y.co[show,]),
                                               Y.tr.pst),
                                 "type" = c(rep("tr",(Ntr*nT)),
                                            rep("co",(Nco*nT)),
                                            rep("tr.pst",length(Y.tr.pst))),
                                 "id" = c(rep(1:N,each = nT), id.tr.pst*(-1)))
        
        ## legend
        legend.labs = NULL
        set.limits = c("co", "tr", "tr.pst")
        set.colors = c(raw.color[1], raw.color[2], raw.color[3])
        set.linetypes = c("solid","solid","solid")
        set.linewidth = c(0.5, 0.5, 0.5)
        
        set.labels <- c("Counterfactual","Treated (Pre)","Treated (Post)") 
        
        labels.ncol <- 3
        
        #else { ## FE mode data
        
        
        
        ## theme
        p <- ggplot(data) + xlab(xlab) +  ylab(ylab)
        
        p <- p + theme(legend.position = legend.pos,
                       axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
                       plot.title = element_text(size=20, hjust = 0.5, face="bold"))
        
        if (DID == TRUE && Ntr >= 1) {
          if (time.bf >= min(show) && time.bf <= max(show)) {
            p <- p + geom_vline(xintercept=time.bf,colour="white",size = 2)
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,
                                ymin=-Inf, ymax=Inf, alpha = .3) 
            }                            
          }
        }
        
        ## main
        p <- p + geom_line(aes(time, outcome,
                               colour = type,
                               size = type,
                               linetype = type,
                               group = id))
        
        set.linewidth[1] <- 0.7
        set.linewidth[c(2,3)] <-1
        p <- p + scale_colour_manual(limits = set.limits,
                                     labels = set.labels,
                                     values =set.colors) +
          scale_linetype_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linetypes) +
          scale_size_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linewidth) +
          guides(linetype = guide_legend(title=NULL, ncol=labels.ncol),
                 colour = guide_legend(title=NULL, ncol=labels.ncol),
                 size = guide_legend(title=NULL, ncol=labels.ncol))
        
      }  
      
      
      p <- p + ggtitle(main)
      
      #  p <- p + coord_cartesian(ylim = ylim)
      
      
      ## end of raw plot
      
      
      p <- p + 
        scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
      p  
    }
  }
}
