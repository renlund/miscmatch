
##' Extra fullmatch info
##'
##' Some additional details I find useful in connection with a
##'     fullmatch (or similar matching)
##' @title What?
##' @param data the data
##' @param tr the binary treatment variable, as character
##' @param cl the clustering variable, as character
##' @param id the id variable (optional), as character. If this is
##'     used then only the relevant variables will be
##'     returned (in the same order as data).
##' @param trv the value of the treatment (of variable \code{tr}), '1'
##'     by default
##' @return data frame (\code{tbl_df}) with new parameters \code{tr_n} the number of
##'     treated within the cluster,  \code{ctrl_n} the number of
##'     controls within the cluster, \code{cl.weight} the weight,
##'     \code{cid} for describing the match it is useful to have a
##'     'cluster id', use this with e.g. \code{dplyr::group_by(tr, cid)} and
##'     \code{summarise} with functions using \code{weight =
##'     cl.weight} to get stats for weighted treated and control statistics.
##' @author Henrik Renlund
##' @examples df <- data_frame(
##'    id = 1:13,
##'    foo = c(0,0,1, 0,1, 1,1,0, 1,1,0,0, 0),
##'    bar = c(rep(c(letters[1:4]), c(3,2,3,4)), NA),
##'    x = round(runif(13),2)
##')
##' match_info(data = df, tr = "foo", cl = "bar")
##' match_info(data = df, tr = "foo", cl = "bar", id = "id")
##' df$foo <- ifelse(df$foo == 1, "Treated", "Control")
##' match_info(data = df, tr = "foo", cl = "bar", trv = "Treated")
##' @import  dplyr
##' @export

match_info <- function(data, tr, cl, id = NULL, trv = 1){
    if(! "dplyr" %in% installed.packages()[,1]){
        stop("[match_info] this function is coded in 'dplyr' which needs to be available")
    }
    m_i_out.names <- c("tr_n", "ctrl_n", "cl.weight", "cid")
    if(any(c(tr, cl, id) %in% m_i_out.names)){
        stop(paste0("[match_info] names of data conflicts with created variables (",
                    paste0(m_i_out.names, collapse = ", "), ")"))
    }
    if(!any(trv %in% data[[tr]])){
        stop(paste0("[match_info] there are no values '", trv, "' in the variable '", tr,
             "'."))
    }
    D <- if(!is.null(id)){
        dplyr::select_(.data = data, .dots = c(id, tr, cl))
    } else {
        data
    }
    XNA <- dplyr::filter_(.data = D, paste0("is.na(", cl, ")")) %>%
        dplyr::mutate(tr_n = NA, ctrl_n = NA, cl.weight = NA, cid = NA)
    X <- dplyr::filter_(.data = D, paste0("!is.na(", cl, ")")) %>%
        dplyr::group_by_(cl) %>%
        dplyr::mutate_('tr_n' = paste0("sum(",tr,"=='",trv,"')") ,
                       'ctrl_n'  = paste0("sum(",tr,"!='",trv,"')")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_(
            'cl.weight' = paste0("paste0(ifelse(", tr,
                           " == '", trv, "', 1/tr_n, 1/ctrl_n))"),
            'cid' =  paste0("paste0(", cl, ", ':', ifelse(", tr,
                            " == '", trv, "', 'tr', 'ctrl'), ifelse(",
                            tr, " == '", trv, "', tr_n, ctrl_n))")
            )
    Y <- dplyr::tbl_df(rbind(X, XNA))
    class(Y) <- c(class(Y), "match_info")
    if(!is.null(id)){
        dplyr::left_join(select_(.data = data, id), Y, by = id)
    } else {
        message("Beware! Order of data may have changed.")
        Y
    }
}

if(FALSE){ ## example
    df <- data_frame(
        id = 1:13,
        foo = c(0,0,1, 0,1, 1,1,0, 1,1,0,0, 0),
        bar = c(rep(c(letters[1:4]), c(3,2,3,4)), NA),
        x = round(runif(13),2)
    )
    match_info(data = df, tr = "foo", cl = "bar")
    match_info(data = df, tr = "foo", cl = "bar", id = "id")
    df$foo <- ifelse(df$foo == 1, "Treated", "Control")
    match_info(data = df, tr = "foo", cl = "bar", trv = "Treated")
}
