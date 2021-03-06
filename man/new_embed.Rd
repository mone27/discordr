% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rest-api-tools.R
\name{new_embed}
\alias{new_embed}
\title{Makes a discord embed}
\usage{
new_embed(
  title = NULL,
  type = "rich",
  description = NULL,
  url = NULL,
  timestamp = NULL,
  color = NULL,
  footer = NULL,
  image = NULL,
  thumbnail = NULL,
  video = NULL,
  provider = NULL,
  author = NULL,
  fields = NULL
)
}
\arguments{
\item{title}{title}

\item{type}{(will be deprecated by discord)}

\item{description}{description (main content)}

\item{url}{url}

\item{timestamp}{timestamp}

\item{color}{side color}

\item{footer}{footer}

\item{image}{list with image props}

\item{thumbnail}{thumbnail}

\item{video}{list with image props}

\item{provider}{provider}

\item{author}{author}

\item{fields}{list of fields}
}
\value{
the embed object with class discordr_embed
}
\description{
for more info check
https://discord.com/developers/docs/resources/channel#embed-object-embed-structure
}
