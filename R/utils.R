#' Join metrics
#'
#' Calculate and join selected evaluation metrics given a `data.frame` of simulation study results
#' Provides a fast way to add multiple metrics and their Monte Carlo standard errors.
#'
#' @param data A `data.frame` that contains results from simulation study and the necessary columns to compute metrics.
#' @param id_cols Column name(s) on which to group data and calculate metrics.
#' @param metrics A vector of metrics to be calculated.
#' @param true_value The true parameter to be estimated.
#' @param ll_col Name of the column that contains the lower limit of the confidence intervals. (Required for calculating coverage.)
#' @param ul_col Name of the column that contains the upper limit of the confidence intervals. (Required for calculating coverage.)
#' @param estimates_col Name of the column that contains the parameter estimates. (Required for calculating bias, empSE, and mse.)
#' @param se_col Name of the column that contains the standard errors. (Required for calculating modSE.)
#' @param p_col Name of the column that contains the p-values. (Required for calculating rejection.)
#' @param alpha The nominal significance level specified. (Required for calculating rejection.)
#'
#' @return `data.frame` containing metrics and `id_cols`
#' @export
#'
#' @examples
#' simulations_df <- data.frame(
#'   idx=rep(1:10, 100),
#'   idx2=sample(c("a", "b"), size=1000, replace=TRUE),
#'   p_value=runif(1000),
#'   est=rnorm(n=1000),
#'   conf.ll= rnorm(n=1000, mean=-20),
#'   conf.ul= rnorm(n=1000, mean=20)
#' )
#' res <- join_metrics(
#'   data=simulations_df,
#'   id_cols=c("idx", "idx2"),
#'   metrics=c("rejection", "coverage", "mse"),
#'   true_value=0,
#'   ll_col="conf.ll",
#'   ul_col="conf.ul",
#'   estimates_col="est",
#'   p_col="p_value",
#' )
join_metrics <- function(
  data,
  id_cols,
  metrics=c("coverage", "mse", "modSE"),
  true_value=NULL,
  ll_col=NULL,
  ul_col=NULL,
  estimates_col=NULL,
  se_col=NULL,
  p_col=NULL,
  alpha=0.05){

  assertthat::assert_that(all(id_cols %in% names(data)))
  assertthat::assert_that(length(id_cols) == length(unique(id_cols)))

  metrics_appropriate <- c("bais", "biasEliminatedCoverage", "coverage", "empSE", "modSE", "mse", "rejection", "relativeErrorModSE")

  if(!all(metrics %in% metrics_appropriate)){
    warning(
      paste0("The following metrics provided are not appropriate for this function:", paste0(metrics[!metrics %in% metrics_appropriate], collapse=", "),
             "\nThese will be ignored.\nThis function can only handle the following metrics:", paste0(metrics_appropriate, collapse=", "))
    )
    metrics <- metrics[metrics %in% metrics_appropriate]
  }


  if(length(true_value)==1 & is.numeric(true_value)){
    # if the argument is given as a value, assign the column to be that value
    data$true_value_col <- true_value
    true_value_col <- "true_value_col"
  }else{
    # if the argument given is the name of the column, store that name to be used later
    true_value_col <- true_value
  }

  df_grouped <- dplyr::group_by(.data=data, dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(.group_id=dplyr::cur_group_id())

  get_metrics_group <- function(df, id){
    df_do <- df[df$.group_id == id,]

    parms_list <- list()
    # add upper and lower limits
    if("coverage" %in% metrics){
      parms_list <- c(parms_list, list(ll=df_do[[ll_col]], ul=df_do[[ul_col]]))
    }
    # add estimates
    if(any(c("bias", "empSE", "mse", "relativeErrorModSE") %in% metrics)){
      parms_list <- c(parms_list, list(estimates=df_do[[estimates_col]]))
    }
    # add true value
    if(any(c("coverage", "bias", "mse") %in% metrics)){
      parms_list <- c(parms_list, list(true_value=df_do[[true_value_col]]))
    }
    # add p-value
    if("rejection" %in% metrics){
      parms_list <- c(parms_list, list(p=df_do[[p_col]], alpha=alpha))
    }
    # add standard error
    if(any(c("modSE", "relativeErrorModSE") %in% metrics)){
      parms_list <- c(parms_list, list(se=df_do[[se_col]]))
    }

    metrics <- purrr::map(metrics, function(m) do.call(m, parms_list))
    unlist(c(metrics, list(.group_id=id)))
  }

  df_metrics <- purrr::map_dfr(unique(df_grouped$.group_id), function(x) get_metrics_group(df=df_grouped, id=x))

  df_out <-
    dplyr::left_join(
      dplyr::distinct(df_grouped[, c(id_cols, ".group_id")]),
      df_metrics,
      by=".group_id"
    )
  df_out$.group_id <- NULL
  dplyr::ungroup(df_out)
}
