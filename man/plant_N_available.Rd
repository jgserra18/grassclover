% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/N_plant_available.R
\name{plant_N_available}
\alias{plant_N_available}
\title{plant_N_available}
\usage{
plant_N_available(N_eff_fert, N_res_avail_t, N_res_avail_t1, N_back)
}
\arguments{
\item{N_eff_fert}{N effective fertiliser after NH3 volatilization (kg N ha-1 yr-1); see input_Nfertiliser}

\item{N_res_avail_t}{N available from crop residues at timestep t}

\item{N_res_avail_t1}{N available from crop residues from previous year}

\item{N_back}{N available from semi-stable SON (kg N ha-1 yr-1); see input_background_soil_N}
}
\description{
Computes plant N available
}
