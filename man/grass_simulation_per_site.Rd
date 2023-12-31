% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grass_parametrization.R
\name{grass_simulation_per_site}
\alias{grass_simulation_per_site}
\title{grass_simulation_per_site}
\usage{
grass_simulation_per_site(
  site_name = "Leeds",
  yrs = Years,
  N_fert = seq(0, 600, 100)
)
}
\arguments{
\item{site_name}{site name with experimental data}

\item{yrs}{experimental data years}

\item{N_fert}{fertilisation levels (seq(0,600,100))}
}
\value{
returns a dataframe with the best simulations for the different years and fertilisation levels
}
\description{
for a given sitename, loops for the different N fert levels and finds the optimal parameters (based on the minimum nRMSE)
}
\note{
This is not terribly efficient as it is; just to test whether it works
}
\examples{
looping_per_site('Leeds',c(1978,1980,1981))
}
