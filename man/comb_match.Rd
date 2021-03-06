% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comb-match.R
\name{comb_match}
\alias{comb_match}
\title{combine 2 matchings}
\usage{
comb_match(x, y, id, cl, tr, tr.value = 1, progress = FALSE)
}
\arguments{
\item{x}{first matching, must contain an id-, a cluster-, and a treatment variable}

\item{y}{second matching, must contain same things as x}

\item{id}{name of id variable, if different in x and y, supply a vector}

\item{cl}{name of cluster variable, if different in x and y, supply a vector}

\item{tr}{name of treatment variable, if different in x and y, supply a vector}

\item{tr.value}{value in tr indicating treatment, if different in x and y,
supply a vector}
}
\value{
a list of data frames; clust 1 and 2 are the old clusters mapped to
    the new; id 1 and 2 are the data set id:s mapped onto the new cluster;
    new.cluster contains info on number of cases and controls; data contains
    all individuals
}
\description{
if a set of individuals have been matched to two different controls, this
    functions can combine these sets of clusterings to a single clustering
}
