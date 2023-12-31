% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Clover_Above_ground_production.R
\name{clover_DM_yield}
\alias{clover_DM_yield}
\title{clover_DM_yield}
\usage{
clover_DM_yield(yield_clover_dm_max = 6000, f_clover)
}
\arguments{
\item{yield_clover_dm_max}{maximum clover yield in monoculture (Yc ;kg ha-1 yr-1)}

\item{f_clover}{portion of clover occupation  (0-1) (pc)}
}
\value{
vetor
}
\description{
computes clover yield based on maximum possible yield and fraction of clover occupation
}
\examples{
clover_DM_yield(6000, 0.3)
}
