##' multiple survival curves
##'
##' create multiple survival curves for multiple endpoints,
##'     stratifications and groupings
##' @param data the data
##' @param surv the \code{Surv} objects (as character, refering to
##'     variables in 'data') OR the common names of time + event, if
##'     these have a consistent prefix.
##' @param strata stratifying variable
##' @param glist a grouping list, or the name of a grouping variable
##'     in 'data'
##' @param w weight or name of variable that provides weights
##' @param prefix prefix values for times and events
##' @param progress primitive displayer of progress
##' @param stringsAsFactors logical, if TRUE will keep ordering of ingoing stuff
##' @import survival
##' @importFrom broom tidy
##' @importFrom stats as.formula
##' @return A \code{tbl_df} with an \code{rbind}ed \code{broom::tidy}'d
##'     \code{survival::survfit}ed version of all the combinations of
##'     'strata' and 'glist' for each 'surv'. That is a crap
##'     explanation...
##' @export
##' @examples
##' n = 1200
##' df <- data.frame(
##'         t.foo = abs(rnorm(n, 10, 2)),
##'         ev.foo = rbinom(n, 1, 0.2),
##'         t.bar = abs(rnorm(n, 10, 3)),
##'         ev.bar = rbinom(n, 1, 0.3),
##'         strutt = sample(letters[1:2], n, TRUE),
##'         gsak = sample(LETTERS[4:7], n, TRUE),
##'         wait = .5 + abs(rnorm(n, 0, 1))
##' )
##' surv_curv(data = df, surv = c("foo", "bar"), w = "wait",
##'           strata = "strutt", glist = "gsak")
surv_curv <- function(data, surv, strata = NULL, glist = NULL, w = NULL,
                      prefix = c(event = "ev.", time = "t."),
                      progress = FALSE,
                      stringsAsFactors = TRUE){
    N <- nrow(data)
    if(is.null(w)) w <- rep(1, N)
    if(is.character(w)) w <- data[[w]]
    if(is.null(glist)) glist <- list(All = rep(TRUE, N))
    if(!is.list(glist)) glist <- make_glist(glist, ref = data)
    glist_lev <- names(glist)
    if(is.null(strata)){
        data$strata <- factor(rep("no strata", N))
        strata <- "strata"
    }
    if(!is.factor(data[[strata]])){
        data[[strata]] <- factor(data[[strata]])
    }
    strata_lev <- levels(data[[strata]])
    if(!all(surv %in% names(data))){
        ## if variables are not of class Surv they must have consistent
        ## naming, with the same prefix for the time- and event variable, resp.
        for(i in seq_along(surv)){
            ti <- tryCatch(
                expr = {
                    get(paste0(prefix["time"], surv[i]), data)
                },
                error = function(e){
                    stop(paste0(" oops! cant find time ", surv,
                                " in the data\n "))
                }
            )
            ev <- tryCatch(
                expr = {
                    get(paste0(prefix["event"], surv[i]), data)
                },
                error = function(e){
                    stop(paste0(" oopsie! cant find event ", surv,
                                " in the data\n"))
                }
            )
            data[[surv[i]]] <- survival::Surv(time = ti, event = ev)
        }
    }
    R <- NULL
    for(i in seq_along(glist)){
        for(j in seq_along(strata_lev)){
            filter <- glist[[i]] & data[[strata]] == strata_lev[j]
            X <- data[filter, ]
            W <- w[filter]
            for(k in seq_along(surv)){
                if(progress){
                    cat(paste0(i, "/", length(glist), ", ",
                               paste0(j, "/", length(strata_lev), ", ",
                                      paste0(k, "/", length(surv), "\n"))))
                }
                f <- stats::as.formula(paste0(surv[k], " ~ 1"))
                tmp <- broom::tidy(
                  survival::survfit(formula = f, data = X, weight = W)
                )
                tmp$outcome <- surv[k]
                tmp$strata <- strata_lev[j]
                tmp$group <- names(glist)[i]
                R <- if(is.null(R)) tmp else rbind(R, tmp)
            }
        }
    }
    if(stringsAsFactors){
        R$outcome <- factor(R$outcome, levels = surv)
        R$strata <- factor(R$strata, levels = strata_lev)
        R$group <- factor(R$group, levels = glist_lev)
    }
    dplyr::tbl_df(R)
}

#' helper function for 'surv_curv'
#' @param x thing to create glist from
#' @param ref reference for thing
make_glist <- function(x, ref = NULL){
    if(!is.null(ref)){
        if(is.data.frame(ref)){
            if(is.character(x)){
                x <- ref[[x]]
            } else {
                if(length(x) != nrow(ref)) stop("nah1")
            }
        } else {
            if(length(x) != length(ref)) stop("nah2")
        }
    }
    y <- as.factor(x)
    if(length(levels)>100) stop("nah3")
    g <- as.list(NULL)
    for(k in levels(y)){
        g[[k]] <- y == k
    }
    g
}


if(FALSE){

    n = 3*1000
    df <- data_frame(
        t.foo = abs(rnorm(n, 10, 2)),
        ev.foo = rbinom(n, 1, 0.2),
        t.bar = abs(rnorm(n, 10, 3)),
        ev.bar = rbinom(n, 1, 0.3),
        strutt = sample(letters[1:2], n, TRUE),
        gsak = sample(LETTERS[4:7], n, TRUE),
        wait = .5 + abs(rnorm(n, 0, 1))
    )
    X <- surv_curv(data = df, surv = c("foo", "bar"),
                   w = "wait",
                   strata = "strutt", glist = "gsak")
    ggplot(X, aes(time, estimate, color = strata)) +
        geom_line() +
        facet_grid(outcome ~ group)

}
