% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_book.of.workflow.R
\name{book.of.workflow}
\alias{book.of.workflow}
\title{Book of Workflow Overview}
\description{
The following functional families are covered in `book.of.workflow`:\cr
}
\section{Chapter 1 - Environment-processing}{

The focus is on loading libraries and managing environment objects:\cr
\itemize{
	\item{\code{\link{load_unloaded}}}
	\item{\code{\link{save_image}}}
	\item{\code{\link{copy_obj}}}
}
}

\section{Chapter 2 - Environment Integrity}{

 This family of functions is focused on maintaining the integrity of object environments, primarily, to mitigate errors due to  missing object dependencies:\cr
 \itemize{
 	\item{\code{\link{\%missing\%}}}
 	\item{\code{\link{\%must.have\%}}}
 	\item{\code{\link{\%check\%}}}
 	\item{\code{\link{\%+must.have\%}}}
 	\item{\code{\link{\%+=\%}}}
 	\item{\code{\link{\%-=\%}}}
 }
}

\section{Chapter 3 - External Data Management}{

 This family of functions focuses on connecting to external data (primarily databases) as well as parallelized computing contexts with special consideration of Windows environments without SSH implemented:\cr
 \itemize{
 	\item{\code{\link{make_query}}}
 	\item{\code{\link{get_data}}}
 	\item{\code{\link{export_data}}}
 	\item{\code{\link{check.db_conn}}}
 }
}

\section{Chapter 4 - Workflow Management}{

 This family of functions focuses on code execution workflow:\cr
 \itemize{
 	\item{\code{\link{read.snippet}}}
 	\item{\code{\link{make.snippet}}}
 	\item{\code{\link{snippets_toc}}}
 	\item{\code{\link{mgr_upgrade}}}
 }
}

