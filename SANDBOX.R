# dir("pkg/R", full.names = TRUE) |> purrr::walk(source)
library(purrr);
library(stringi);
library(parallelly);
library(future);
library(foreach);
library(data.table);

# ~ do.get.data() ====
library(stringi)
library(magrittr)
library(purrr)
library(DBI)
ENV <- new.env();

do.get_data(dbConnect(odbc::odbc(), "EDW"), "", "object", this.data = mtcars, promise = TRUE)

(function(
	SELECT
	, FROM
	, WHERE = NULL
	, `GROUP BY` = NULL
	, HAVING  = NULL
	, `ORDER BY` = NULL
	, LIMIT = NULL
	, ...
	){
	rlang::list2(
			SELECT			=	rlang::enexpr(SELECT) |> purrr::map(rlang::as_label)
		, FROM				= rlang::enexpr(FROM) |> purrr::map(rlang::as_label)
		, WHERE 			= rlang::enexpr(WHERE) |> purrr::map(rlang::as_label)
		, `GROUP BY`	= rlang::enexpr(`GROUP BY`) |> purrr::map(rlang::as_label)
		, HAVING			=	rlang::enexpr(HAVING) |> purrr::map(rlang::as_label)
		, `ORDER BY`	= rlang::enexpr(`ORDER BY`) |> purrr::map(rlang::as_label)
		, LIMIT 			= rlang::enexpr(LIMIT) |> purrr::map(rlang::as_label)
		) |> purrr::compact()
})(
	SELECT = c("TOP 10", col_1, col_2)
	, FROM = c("TABLE", "AS X", "(NOLOCK)")
	, WHERE = c("1=1", col_2 <= 20)
	, `GROUP BY` = NULL
	, HAVING = NULL
	, `ORDER BY` = NULL
	, LIMIT = 30
	)
