\name{data_snapshot_helpers}
\alias{data_snapshot_helpers}
\alias{ea_build_latest_snapshot}
\alias{ea_create_snapshot_bundle}
\alias{ea_data_path}
\alias{ea_latest_data_dir}
\alias{ea_load_dataset}
\alias{ea_load_manifest}
\alias{ea_validate_snapshot_dir}
\title{EnergyAnalytics data snapshot helpers}
\description{
Build, load, validate, and package the canonical local data snapshot used by
the EnergyAnalytics package and dashboard.
}
\usage{
ea_latest_data_dir()

ea_data_path(dataset)

ea_load_dataset(dataset)

ea_load_manifest()

ea_build_latest_snapshot(latest_dir = NULL, release_dir = NULL, snapshot_id = NULL)

ea_create_snapshot_bundle(snapshot_dir = NULL, bundle_dir = NULL)

ea_validate_snapshot_dir(snapshot_dir = NULL)
}
\arguments{
  \item{dataset}{Dataset name with or without the \code{.feather} suffix.}

  \item{latest_dir}{Directory where the working snapshot is written. Defaults to
  \code{inst/extdata/latest} under the project root.}

  \item{release_dir}{Directory where snapshot tarballs are written. Use
  \code{NULL} to skip bundle creation during a build.}

  \item{snapshot_id}{Optional snapshot identifier. When omitted, a UTC timestamp
  is generated.}

  \item{snapshot_dir}{Directory containing an existing snapshot to archive or
  validate.}

  \item{bundle_dir}{Directory where archived snapshot tarballs are written.}
}
\value{
  \code{ea_latest_data_dir()} and \code{ea_data_path()} return absolute file
  paths. \code{ea_load_dataset()} returns a data frame loaded from Feather.
  \code{ea_load_manifest()} returns the parsed manifest list.
  \code{ea_build_latest_snapshot()} returns the in-memory manifest invisibly
  after writing snapshot files, checksums, and metadata. 
  \code{ea_create_snapshot_bundle()} returns the tarball path invisibly.
  \code{ea_validate_snapshot_dir()} returns \code{TRUE} invisibly when the
  snapshot passes schema, uniqueness, and checksum validation.
}
\details{
The snapshot contract is package-based and deterministic at the environment
level. Commodity prices come from \code{RTL::dflong}, EIA data comes from
\code{RTL::eia2tidy_all()}, FRED data comes from
\code{tidyquant::tq_get(..., get = "economic.data")}, and
\code{RTL::expiry_table} is used only as reference metadata.
}
\keyword{datasets}
