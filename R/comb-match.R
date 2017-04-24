##' combine 2 matchings
##'
##' if a set of individuals have been matched to two different controls, this
##'     functions can combine these sets of clusterings to a single clustering
##' @param x first matching, must contain an id-, a cluster-, and a treatment variable
##' @param y second matching, must contain same things as x
##' @param id name of id variable, if different in x and y, supply a vector
##' @param cl name of cluster variable, if different in x and y, supply a vector
##' @param tr name of treatment variable, if different in x and y, supply a vector
##' @param tr.value value in tr indicating treatment, if different in x and y,
##'     supply a vector
##' @return a list of data frames; clust 1 and 2 are the old clusters mapped to
##'     the new; id 1 and 2 are the data set id:s mapped onto the new cluster;
##'     new.cluster contains info on number of cases and controls; data contains
##'     all individuals
##' @export
comb_match <- function(x, y, id, cl, tr, tr.value = 1, progress = FALSE){
    P <- progress
    if(P) cat("Starting comb_match\n")
    id1 <- id[1]
    id2 <- if(length(id)>1) id[2] else id[1]
    cl1 <- cl[1]
    cl2 <- if(length(cl)>1) cl[2] else cl[1]
    tr1 <- tr[1]
    tr2 <- if(length(tr)>1) tr[2] else tr[1]
    tr.v1 <- tr.value[1]
    tr.v2 <- if(length(tr.value)>1) tr.value[2] else tr.value[1]
    ## -------------------------------------
    df1 <- x[, c(id1, cl1, tr1)]
    names(df1) <- c("id1", "cl1", "tr1")
    df2 <- y[, c(id2, cl2, tr2)]
    names(df2) <- c("id2", "cl2", "tr2")
    ## -------------------------------------
    trid1 <- df1$id1[df1$tr1 == tr.v1]
    trid2 <- df2$id2[df2$tr2 == tr.v2]
    same_id <- setequal(trid1, trid2)
    if(!same_id) stop("the set of id's of treated is not the same")
    if(length(unique(trid1)) != sum(df1$tr1 == tr.v1) |
       length(unique(trid2)) != sum(df2$tr2 == tr.v2)){
        stop("id-variable is not unique")
    }
    ## -------------------------------------
    ctrl1id <- df1$id1[df1$tr1 != tr.v1]
    ctrl2id <- df2$id2[df2$tr2 != tr.v2]
    ctrl_overlap <- any(ctrl1id %in% ctrl2id)
    ## -------------------------------------
    get_clust <- function(id){
        list("clust1" = sort(df1$cl1[df1$id1 == id]),
             "clust2" = sort(df2$cl2[df2$id2 == id]))
    }
    nabo <- function(gc){
        ids1 <- df1$id1[df1$cl1 %in% gc[[1]] & df1$tr1 == tr.v1]
        kl2 <- df2$cl2[df2$id2 %in% ids1]
        ids2 <- df2$id2[df2$cl2 %in% gc[[2]] & df2$tr2 == tr.v2]
        kl1 <- df1$cl1[df1$id1 %in% ids2]
        list("clust1" = sort(unique(kl1)), "clust2" = sort(unique(kl2)))
    }
    glue <- function(x, y) paste(x, collapse = y)
    n <- length(trid1)
    R <- data.frame(id = trid1, comb.clust = rep(NA, n),
                    cl1 = rep(NA, n), cl2 = rep(NA, n),
                    tr_n = rep(NA, n), ctrl_n = rep(NA, n),
                    ctrl1_n = rep(NA, n), ctrl2_n = rep(NA, n),
                    wate.tr = rep(NA, n),
                    wate.ctrl1 = rep(NA, n), wate.ctrl2 = rep(NA, n),
                    watt.tr = rep(NA, n),
                    watt.ctrl1 = rep(NA, n), watt.ctrl2 = rep(NA, n))
    if(P) cat("... starting main loop\n")
    for(i in seq_along(trid1)){ ## i = 1
        if(P & i %in% floor(seq(1, n, length.out = 10 + 1)[-1])){
            cat(round(100 * i/n), "percent done.\n")
        }
        id <- trid1[i]
        gc <- get_clust(id)
        R$cl1[i] <- gc[1]
        R$cl2[i] <- gc[2]
        gc_next <- nabo(gc)
        while(!setequal(gc[[1]], gc_next[[1]]) |
              !setequal(gc[[2]], gc_next[[2]])){
            gc <- gc_next
            gc_next <- nabo(gc)
        }
        tr <- c(df1$id1[df1$cl1 %in% gc[[1]] & df1$tr1 == tr.v1],
                df2$id2[df2$cl2 %in% gc[[2]] & df2$tr2 == tr.v2])
        R$tr_n[i] <- length(unique(tr))
        ctrl1 <- c(df1$id1[df1$cl1 %in% gc[[1]] & df1$tr1 != tr.v1])
        R$ctrl1_n[i] <- length(unique(ctrl1))
        ctrl2 <- c(df2$id2[df2$cl2 %in% gc[[2]] & df2$tr2 != tr.v2])
        R$ctrl2_n[i] <- length(unique(ctrl2))
        R$ctrl_n[i] <- R$ctrl1_n[i] + R$ctrl2_n[i]
        R$comb.clust[i] <- glue(c(glue(gc_next[[1]], ":"),
                                  glue(gc_next[[2]], ":")), "/")
        size <- R$tr_n[i] + R$ctrl_n[i]
        tr_size <- R$tr_n[i]
        R$wate.tr[i] <- size / (3 * R$tr_n[i])
        R$wate.ctrl1[i] <- size / (3 * R$ctrl1_n[i])
        R$wate.ctrl2[i] <- size / (3 * R$ctrl2_n[i])
        R$watt.tr[i] <- tr_size / (3 * R$tr_n[i])
        R$watt.ctrl1[i] <- tr_size / (3 * R$ctrl1_n[i])
        R$watt.ctrl2[i] <- tr_size / (3 * R$ctrl2_n[i])
    }
    if(P) cat("\n... finishing up\n")
    X <- merge(df1, R[!duplicated(R$cl1),
                      c("comb.clust", "cl1", "wate.tr",
                        "wate.ctrl1", "watt.tr", "watt.ctrl1")], by = "cl1")
    X$ATE.weight <- ifelse(X$tr1 == tr.v1, X$wate.tr, X$wate.ctrl1)
    X$ATT.weight <- ifelse(X$tr1 == tr.v1, X$watt.tr, X$watt.ctrl1)
    X$group <- ifelse(X$tr1 == tr.v1, "case", "control1")
    Y <- merge(df2, R[!duplicated(R$cl2),
                      c("comb.clust", "cl2", "wate.tr",
                        "wate.ctrl2", "watt.tr", "watt.ctrl2")], by = "cl2")
    Y$ATE.weight <- ifelse(Y$tr2 == tr.v2, Y$wate.tr, Y$wate.ctrl2)
    Y$ATT.weight <- ifelse(Y$tr2 == tr.v2, Y$watt.tr, Y$watt.ctrl2)
    Y$group <- ifelse(Y$tr2 == tr.v2, "case", "control2")
    if(ctrl_overlap){
        X$id1 <- ifelse(X$tr1 != tr.v1, paste0(X$id1, ":1"), X$id1)
        Y$id2 <- ifelse(Y$tr2 != tr.v2, paste0(Y$id2, ":2"), Y$id2)
        message(paste0("\n  Control group id's are not unique between data\n",
                       "  sets and have been changed in part of the output.\n"))
    }
    X1 <- X[, c("id1", "group", "comb.clust", "ATE.weight", "ATT.weight")]
    Y1 <- Y[, c("id2", "group", "comb.clust", "ATE.weight", "ATT.weight")]
    names(X1) <- c("id", "group", "comb.clust", "ATE.weight", "ATT.weight")
    names(Y1) <- c("id", "group", "comb.clust", "ATE.weight", "ATT.weight")
    Z <- rbind(X1, subset(Y1, group != "case"))
    U <- R[!duplicated(R$comb.clust), c("comb.clust", "tr_n", "ctrl1_n",
                                        "ctrl2_n")]
    if(P) cat("done!\n")
    list(
        "clust1" = R[!duplicated(R$cl1), c("cl1", "comb.clust")],
        "clust2" = R[!duplicated(R$cl2), c("cl2", "comb.clust")],
        "id1" = X[, c("id1", "comb.clust")],
        "id2" = Y[, c("id2", "comb.clust")],
        "new.cluster" = U,
        "data" = Z[order(Z$comb.clust, Z$group, Z$id), ]
    )
}


if(FALSE){

    df1 <- data.frame(
        id1 = 1:14,
        tr1 = rep(1:0, each = 7),
        cl1 = c("a", "b", "b", "hoo", "d", "kzr", "f",
                "a", "a", "b", "hoo", "d", "kzr", "f"),
        stringsAsFactors = FALSE
    )
    df2 <- data.frame(
        id2 = c(1:7, 15:22),
        tr2 = rep(1:0, c(7,8)),
        cl2 = c("A", "QWERTY", "C", "C", "D", "D", "D",
                rep("A", 4), "QWERTY", "QWERTY", "C", "D"),
        stringsAsFactors = FALSE
    )
    (R <- comb_match(x = df1, y = df2, id = c("id1", "id2"),
                     cl = c("cl1", "cl2"), tr = c("tr1", "tr2")))

    x = df1
    y = df2
    id = c("id1", "id2")
    cl = c("cl1", "cl2")
    tr = c("tr1", "tr2")
    tr.value = 1

    tmp <- R$data
    tapply(tmp$ATE.weight, tmp$comb.clust, sum)
    tapply(tmp$ATE.weight, tmp$group, sum)
    tapply(tmp$ATE.weight, paste(tmp$comb.clust, tmp$group), sum)

    tapply(tmp$ATT.weight, tmp$comb.clust, sum)
    tapply(tmp$ATT.weight, tmp$group, sum)
    tapply(tmp$ATT.weight, paste(tmp$comb.clust, tmp$group), sum)



}
