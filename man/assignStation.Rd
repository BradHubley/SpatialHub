% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignStation.r
\name{assignStation}
\alias{assignStation}
\title{assignStation}
\usage{
assignStation(
  events,
  maxdist = 0.01,
  res = 0.005,
  expwin = 0.05,
  map = NULL,
  lines = F,
  ...
)
}
\arguments{
\item{events}{= PBSmapping::EventData of tows}

\item{maxdist}{= maximum distance between points considered to be the same statiom}

\item{res}{= resolution of the spatstat.geom::distmap}

\item{expwin}{= expansion window beyond supplied points that defines the area examined}

\item{map}{= preset location to pass to bioMap}

\item{lines}{= logical, if TRUE X1, Y1, X2, Y2 must be supplied to describe the tows}

\item{...}{additional arguments passed to bioMap}
}
\description{
Assigns station number to tows based on proximity.
}
\author{
Brad Hubley
}
