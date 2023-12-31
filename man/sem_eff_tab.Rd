% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem_eff_tab.R
\name{sem_eff_tab}
\alias{sem_eff_tab}
\title{Process SEM effects output for OE,MMI, EPT relabel variable names to make into a table}
\usage{
sem_eff_tab(sem_output)
}
\arguments{
\item{sem_output}{SEM estimated effects output saved as .csv file}
}
\value{
Processed dataframe that selected direct, indirect, and total effects
}
\description{
Process SEM effects output for OE,MMI, EPT relabel variable names to make into a table
}
\examples{
west_oe_m1_full<- sem_eff_tab("model1") # name dataframe loaded with raw SEM effects output
}
