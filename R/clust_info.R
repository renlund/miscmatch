##' Extra cluster info
##'
##' Some additional details I find useful in connection with a
##'     fullmatch (or similar matching)
##' @title cluster level information
##' @param mi output from \code{match_info}
##' @param score matching variable (1-dimensional)
##' @param output 'cluster' for statistics on cluster level, 'delta'
##'     for score differences on individual level and 'both' for both
##'     (as list)
##' @return data frame of info, or list of data frames
##' @author Henrik Renlund
##' @export

clust_info <- function(mi, score, output = "cluster"){
    if(! "dplyr" %in% installed.packages()[,1]){
        stop("[match_info] this function is coded in 'dplyr' which needs to be available")
    }
    if(! "tidyr" %in% installed.packages()[,1]){
        stop("[match_info] this function is coded in 'tidyr' which needs to be available")
    }
    if(!"match_info" %in% class(mi)){
        warning("match_loc is designed for objects created by match_info")
    }
    if(!output %in% c("cluster", "delta", "both")){
        stop("wrong output argument")
    }
    D <- mi %>% rename_("score" = score) %>%
        tidyr::separate(col = cid, into = c("cl", "tr"),
                        sep = ":", remove = FALSE) %>%
        mutate(tr = gsub("[0-9]*", "", tr)) %>%
        group_by(cl) %>%
        mutate(tMp_cOmPaReR = ifelse(!is.na(tr_n) & tr_n == 1, score[tr == 'tr'],
                              ifelse(!is.na(ctrl_n) & ctrl_n == 1, score[tr =="ctrl"],
                                     NA_real_)),
               tMp_DeLtA = ifelse((!is.na(cl.weight) & cl.weight != 1) |
                             (!is.na(tr_n) & tr_n == 1 & ctrl_n == 1),
                              score - tMp_cOmPaReR,
                             NA_real_),
               d = ifelse(!is.na(tr_n) & tr_n == 1 & ctrl_n == 1 & tr == 'tr', NA, tMp_DeLtA),
               cl.score = ifelse(is.na(d), NA_real_, score)
               ) %>%
        select(-tMp_cOmPaReR, -tMp_DeLtA)
    delta <- D %>% ungroup() %>% select(cid, score, d)
    if(output == "delta") return(delta)
    minfo <- attr(mi, "match_info")
    cluster <- D %>% mutate(d_min = min(d, na.rm = TRUE),
                 d_max = max(d, na.rm = TRUE),
                 d_mean = mean(d, na.rm = TRUE),
                 q.ctrl = ctrl_n / tr_n,
                 d.ctrl = ctrl_n - tr_n) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        select_(.dots = c(as.character(minfo['cl']), "tr_n", "ctrl_n", "cl.score:d.ctrl"))
    ## select(-score, -cl, -tr, -d, -cl.weight)

    if(output == "cluster") return(cluster)
    if(output == "both") return(list(cluster = cluster, delta = delta))
    NULL
}

if(FALSE){
    df <- data_frame(
        id = 1:13,
        foo = c(0,0,1, 0,1, 1,1,0, 1,1,0,0, 0),
        bar = c(rep(c(letters[1:4]), c(3,2,3,4)), NA),
        x = round(runif(13),2)
    )
    mi <- match_info(df, tr = "foo", cl = "bar")
    clust_info(mi = mi, score = "x")
}


