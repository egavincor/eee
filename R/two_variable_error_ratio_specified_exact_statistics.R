# two variable, non-phylogenetic, eiv parameterized with lambda (error ratio),
#   exact hypothesis testing focused functions
# Edric Gavin


#' Validate Inputs for EIV Hypothesis Testing
#'
#' note that these variable descriptions are for use in other roxygen comments
#' and largely do not reflect the defaults for this validation function
#' which are always either `NA` or `FALSE`
#'
#' this function only checks some basic issues like that the `correlation`
#' argument is between -1 and 1, etc.
#'
#' @param correlation `numeric` correlation of x-axis and y-axis variable.
#' @param validate_correlation `logical` `TRUE` if argument should be validated.
#' @param sdOFy_divided_by_sdOFx `numeric`sd(y-axis variable)/sd(x-axis variable).
#' @param validate_sdOFy_divided_by_sdOFx `logical` `TRUE` if argument should be validated.
#' @param square_root_of_lambda `numeric` sqrt(lambda), where lambda
#' is the ratio of the error in the y-axis variable to that in the x-axis
#' variable.
#' @param validate_square_root_of_lambda `logical` `TRUE` if argument should be validated.
#' @param lambda `numeric` lambda
#' is the ratio of the error in the y-axis variable to that in the x-axis
#' variable.
#' @param validate_lambda `logical` `TRUE` if argument should be validated.
#' @param sample_size `numeric` number of observations per variable
#' @param validate_sample_size `logical` `TRUE` if argument should be validated.
#' @param slope_hypothesis `numeric` slope (if any) to test as null hypothesis.
#' @param validate_slope_hypothesis `logical` `TRUE` if argument should be validated.
#' @param slope_hypotheses `numeric` (vector if multiple slopes to test)
#' slope (if any) to test as null hypothesis.
#' @param validate_slope_hypotheses `logical` `TRUE` if argument should be validated.
#' @param CI_coefficient `numeric` defaults to 0.95, % of time not to reject the
#' null hypothesis assuming it is true.
#' @param validate_CI_coefficient `logical` `TRUE` if argument should be validated.
#' @param sql_round_to_zero_threshold `numeric` if the square root of lambda
#' is less than this threshold, then lambda is rounded
#' to zero as appropriate and so the answer should be identical to as if
#' zero was given as `lambda`/`square_root_of_lambda`
#' @param validate_sql_round_to_zero_threshold `logical` `TRUE`
#' if argument should be validated.
#' @param lostDoF `numeric` defaults to 2, I have never tried changing
#' this value, but you can change it IF you have a compelling reason.
#' Most significance tests require subtracting 2
#' degrees of freedom from the sample size.
#' @param validate_lostDoF `logical` `TRUE` if argument should be validated.
#' @param print_citation_info `logical` defaults to `TRUE`, which means it
#' prints citation information for this function. Suggested citations is/are the
#' works that contribute to at least some output or calculation of the
#' function in some way and deserve citation.
#' @param validate_print_citation_info `logical` `TRUE` if argument should be validated.
#' @param checkArg `logical` defaults to `TRUE`, which checks arguments for
#' some basic formatting mistakes.
#' @param validate_checkArg `logical` `TRUE` if argument should be validated.
#' @param debugArg `logical` defaults to `FALSE`, `TRUE` prints internal
#' functional information (if any) to terminal.
#' @param validate_debugArg `logical` `TRUE` if argument should be validated.
#' @param y `numeric` vector of y-axis observations
#' @param validate_y `logical` `TRUE` if argument should be validated.
#' @param x `numeric` vector of x-axis observations
#' @param validate_x `logical` `TRUE` if argument should be validated.
#' @param validate_x_and_y `logical` `TRUE` if x and y should be jointly validated.
#'
#' @returns `invisible(TRUE)` if all checks pass; throws an
#' informative error via stop() if any validation fails.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' try(validate_two_variable_exact_CI_function_arguments(correlation=2,
#' validate_correlation=TRUE))
#' validate_two_variable_exact_CI_function_arguments(correlation=0.2,
#' validate_correlation=TRUE)
#' validate_two_variable_exact_CI_function_arguments(correlation=2,
#' validate_correlation=FALSE)
#' validate_two_variable_exact_CI_function_arguments(correlation=0.2,
#' validate_correlation=FALSE)
#' }
validate_two_variable_exact_CI_function_arguments<-function(
    correlation=NA,
    validate_correlation=FALSE,
    sdOFy_divided_by_sdOFx=NA,
    validate_sdOFy_divided_by_sdOFx=FALSE,
    square_root_of_lambda=NA,
    validate_square_root_of_lambda=FALSE,
    lambda=NA,
    validate_lambda=FALSE,
    sample_size=NA,
    validate_sample_size=FALSE,
    slope_hypothesis=NA,
    validate_slope_hypothesis=FALSE,
    slope_hypotheses=NA,
    validate_slope_hypotheses=FALSE,
    CI_coefficient=NA,
    validate_CI_coefficient=FALSE,
    sql_round_to_zero_threshold=NA,
    validate_sql_round_to_zero_threshold=FALSE,
    lostDoF=NA,
    validate_lostDoF=FALSE,
    print_citation_info=NA,
    validate_print_citation_info=FALSE,
    checkArg=NA,
    validate_checkArg=FALSE,
    debugArg=NA,
    validate_debugArg=FALSE,
    y=NA,
    validate_y=FALSE,
    x=NA,
    validate_x=FALSE,
    validate_x_and_y=FALSE
){
  validate_boolean_flags(
    "validate_correlation" = validate_correlation,
    "validate_sdOFy_divided_by_sdOFx" = validate_sdOFy_divided_by_sdOFx,
    "validate_square_root_of_lambda" = validate_square_root_of_lambda,
    "validate_sample_size" = validate_sample_size,
    "validate_slope_hypothesis" = validate_slope_hypothesis,
    "validate_slope_hypotheses" = validate_slope_hypotheses,
    "validate_CI_coefficient" = validate_CI_coefficient,
    "validate_sql_round_to_zero_threshold"=validate_sql_round_to_zero_threshold,
    "validate_lostDoF" = validate_lostDoF,
    "validate_print_citation_info" = validate_print_citation_info,
    "validate_checkArg" = validate_checkArg,
    "validate_debugArg" = validate_debugArg,
    "validate_y"=validate_y,
    "validate_x"=validate_x
  )
  if (validate_correlation) {
    validate_scalar(x=correlation,
                    name="correlation", type="numeric")
    if (correlation>1) {
      stop("`correlation` must be <=1.", call. = FALSE)
    }
    if (correlation<(-1)) {
      stop("`correlation` must be >=-1.", call. = FALSE)
    }
    if (correlation==0) {
      stop("for computational simplicity, `correlation` cannot be exactly zero.")
    }
  }
  if (validate_sdOFy_divided_by_sdOFx) {
    validate_scalar(x=sdOFy_divided_by_sdOFx,
                    name="sdOFy_divided_by_sdOFx", type="numeric")
    if (sdOFy_divided_by_sdOFx<=0) {
      stop("`sdOFy_divided_by_sdOFx` must be >0.", call. = FALSE)
    }
  }
  if (validate_square_root_of_lambda) {
    validate_scalar(x=square_root_of_lambda,
                    name="square_root_of_lambda", type="numeric")
    if (square_root_of_lambda<0) {
      stop("`square_root_of_lambda` must be >=0.", call. = FALSE)
    }
  }
  if (validate_lambda) {
    validate_scalar(x=lambda,
                    name="lambda", type="numeric")
    if (lambda<0) {
      stop("`lambda` must be >=0.", call. = FALSE)
    }
  }
  if (validate_sample_size) {
    validate_scalar(x=sample_size,
                    name="sample_size", type="numeric")
    if (sample_size <= 2) {
      stop("`sample_size` must be greater than 2.", call. = FALSE)
    }
  }
  if (validate_slope_hypothesis) {
    if (!is.na(slope_hypothesis)) {
      validate_scalar(x=slope_hypothesis,
                      name="slope_hypothesis", type="numeric")
    }
  }
  if (validate_slope_hypotheses) {
      # pass for now
  }
  if (validate_CI_coefficient) {
    validate_scalar(x=CI_coefficient,
                    name="CI_coefficient", type="numeric")
    if (CI_coefficient >= 1) {
      stop("`CI_coefficient` must be less than 1.", call. = FALSE)
    }
    if (CI_coefficient <= 0) {
      stop("`CI_coefficient` must be greater than 0.", call. = FALSE)
    }
  }
  if (validate_sql_round_to_zero_threshold) {
    validate_scalar(x=sql_round_to_zero_threshold,
                    name="sql_round_to_zero_threshold", type="numeric")
  }
  if (validate_lostDoF) {
    # pass for now
  }
  if (validate_print_citation_info) {
    validate_scalar(x=print_citation_info,
                    name="print_citation_info", type="logical")
  }
  if (validate_checkArg) {
    validate_scalar(x=checkArg,
                    name="checkArg", type="logical")
  }
  if (validate_debugArg) {
    validate_scalar(x=debugArg,
                    name="debugArg", type="logical")
  }
  if (validate_y) {
    # pass for now
  }
  if (validate_x) {
    # pass for now
  }
  if (validate_x_and_y) {
    if (length(y)!=length(x)) {
      stop ("y-axis and x-axis variables must have same length", call. = FALSE)
    }
  }
  invisible(TRUE)
}


#' calculate slope, exact CI, etc. for two variable relationship given lambda
#'
#' @inheritParams validate_two_variable_exact_CI_function_arguments
#'
#' @returns `list` NOTE: all polar angles are with respect to the x-axis line
#' from the origin in the positive direction. Thus, going clockwise from
#' the x-axis towards the line y=x gives an angle of -135 degrees and going
#' counterclockwise from the x-axis gives an angle of 45 degrees. Confidence
#' intervals (CIs) always stretch from the clockwise line
#'  towards the counterclockwise line.
#' \describe{
#'  \item{slope_cartesian}{slope of best fit line}
#'  \item{slope_polar}{All "polar" angles measure angle between line
#'  and x-axis from origin going in positive direction.
#'  (e.g. line y=x could be 45 degrees in quadrant 1 or -135 in quadrant 3).}
#'  \item{clockwise_CI_cartesian}{ clockwise confidence interval (see "NOTE")
#'  expressed in cartesian terms.}
#'  \item{clockwise_CI_polar}{see "NOTE"}
#'  \item{counterclockwise_CI_cartesian}{see "NOTE"}
#'  \item{counterclockwise_CI_polar}{see "NOTE"}
#'  \item{CI_polar_width}{width (in degrees) of range of slopes
#'  in the CI (confidence interval).}
#'  \item{x_reliability_ratio}{MLE estimate (so divided by n instead of (n-1))
#'  of proportion of x-axis variable's observed variance that is part of
#'  the related (true) x-values and not the error. Remember
#'  v(x_o)=v(x_t)+v(x_e), so this gives v(x_t)/v(x_o).}
#'  \item{slope_test_p_value}{ vector of p-value(s) of tested slope hypothesis(es) (if given)}
#'  \item{calculationTerms}{May contain code's intermediate calculation terms
#'  and such (useful for debugging)}
#'  \item{givenArguments}{`list` of function argument values used in call.}
#'  }
#' @export
#'
#' @examples
#' set.seed(0); n<-10; y1<-rnorm(n); x1<-rnorm(n); y2<--y1
#' c1a<-two_variable_exact_CI(correlation=cor(x1,y2),
#'  sdOFy_divided_by_sdOFx=sd(y2)/sd(x1),
#'  square_root_of_lambda=1,
#'  sample_size=n,
#'  CI_coefficient=0.80,
#'  print_citation_info=FALSE,
#'  slope_hypotheses=-1.45)
#' c1a$slope_cartesian
two_variable_exact_CI<-function(correlation,
                                sdOFy_divided_by_sdOFx,
                                square_root_of_lambda,
                                sample_size,
                                slope_hypotheses=NA,
                                CI_coefficient=0.95,
                                sql_round_to_zero_threshold=1e-10,
                                lostDoF=2,
                                print_citation_info=TRUE,
                                checkArg=TRUE,
                                debugArg=FALSE) {
  givenArguments<-list("correlation"=correlation,
                       "sdOFy_divided_by_sdOFx"=sdOFy_divided_by_sdOFx,
                       "square_root_of_lambda"=square_root_of_lambda,
                       "sample_size"=sample_size,
                       "slope_hypotheses"=slope_hypotheses,
                       "CI_coefficient"=CI_coefficient,
                       "sql_round_to_zero_threshold"=sql_round_to_zero_threshold,
                       "lostDoF"=lostDoF,
                       "print_citation_info"=print_citation_info,
                       "checkArg"=checkArg,
                       "debugArg"=debugArg)
  # Check arguments
  if (checkArg) { # all arguments should at least be passed onto checker.
    validate_two_variable_exact_CI_function_arguments(
      correlation           =correlation, validate_correlation=TRUE,
      sdOFy_divided_by_sdOFx=sdOFy_divided_by_sdOFx, validate_sdOFy_divided_by_sdOFx=TRUE,
      square_root_of_lambda =square_root_of_lambda, validate_square_root_of_lambda=TRUE,
      sample_size           =sample_size, validate_sample_size=TRUE,
      CI_coefficient        =CI_coefficient, validate_CI_coefficient=TRUE,
      slope_hypotheses      =slope_hypotheses, validate_slope_hypotheses=TRUE,
      sql_round_to_zero_threshold=sql_round_to_zero_threshold,validate_sql_round_to_zero_threshold=TRUE,
      lostDoF               =lostDoF, validate_lostDoF=TRUE,
      print_citation_info   =print_citation_info, validate_print_citation_info=TRUE,
      checkArg              =checkArg, validate_checkArg=TRUE,
      debugArg              =debugArg, validate_debugArg=TRUE
    )
  }
  # p (rho) is used for correlation in notation
  # e is used for square root of lambda in notation
  # b is used as notation for sdOFy_divided_by_sdOFx
  CtoP<-180/pi
  b2<-sdOFy_divided_by_sdOFx^2
  e2<-square_root_of_lambda^2
  t1<-b2-e2
  twopbe<-2*correlation*sdOFy_divided_by_sdOFx*square_root_of_lambda
  d_theta<-atan2(twopbe,-t1)/2
  if (square_root_of_lambda > sql_round_to_zero_threshold) {
    m <- square_root_of_lambda * tan(d_theta)
  } else {
    m <- ((t1) + sqrt(t1^2 + twopbe^2)) / (2 * correlation * sdOFy_divided_by_sdOFx)
  }
  slopeTheta<-atan(m)*CtoP
  twoTheta<-2*d_theta
  df<-sample_size-lostDoF
  t2<-stats::qt(0.5+CI_coefficient/2, df)
  qa<-sqrt((1-correlation^2)/df)
  q<-t2*qa/correlation
  inside_asin<-q*sin(twoTheta)
  if (inside_asin>1) {
    c_CIcartesian<-c_CIpolar<-ccCIcartesian<-ccCIpolar<-pWidth<-qb<-NA
    CI1p<-CI2p<-NA
  } else {
    qb<-asin(inside_asin)
    if (square_root_of_lambda > sql_round_to_zero_threshold) {
      CI1p<-square_root_of_lambda*tan((twoTheta+qb)/2)
      CI2p<-square_root_of_lambda*tan((twoTheta-qb)/2)
    } else {
      # The exact Cartesian limits when lambda -> 0
      CI1p <- m / (1 - q)
      CI2p <- m / (1 + q)
    }
    c_CIcartesian<-CI2p
    c_CIpolar<-atan(CI2p)*CtoP
    ccCIcartesian<-CI1p
    ccCIpolar<-atan(CI1p)*CtoP
    if (c_CIpolar>ccCIpolar) {
      if (slopeTheta<0) {
        c_CIpolar<-c_CIpolar-180
      } else {
        ccCIpolar<-ccCIpolar+180
      }
    }
    pWidth<-ccCIpolar-c_CIpolar
  }
  if (length(slope_hypotheses) == 1 && is.na(slope_hypotheses[1])) {
    p_value<-NA
  } else {
    p_value<-c()
    t_test_helper<-correlation * sqrt(df / (1 - correlation^2))
    if (square_root_of_lambda > sql_round_to_zero_threshold) {
        theta0 <- atan(slope_hypotheses / square_root_of_lambda)
        qb_test <- 2 * (theta0 - d_theta)
        q_test <- sin(qb_test) / sin(twoTheta)
    } else {
        # The exact Cartesian limit for q_test as lambda -> 0
        q_test <- (slope_hypotheses - m) / slope_hypotheses
    }
    t_test <- q_test * t_test_helper
    p_value <- 2 * (1 - stats::pt(abs(t_test), df))  
  }
  calculationTerms<-list("b2"=b2,"e2"=e2,"twopbe"=twopbe,
                         "df"=df,"t1"=t1,"t2"=t2,"d_theta"=d_theta,
                         "twoTheta"=twoTheta,"qa"=qa,"q"=q,
                         "inside_asin"=inside_asin,"qb"=qb,
                         "CI1p"=CI1p,"CI2p"=CI2p)
  returnList<-list("slope_cartesian"=m,
                   "slope_polar"=slopeTheta,
                   "clockwise_CI_cartesian"=c_CIcartesian,
                   "clockwise_CI_polar"=c_CIpolar,
                   "counterclockwise_CI_cartesian"=ccCIcartesian,
                   "counterclockwise_CI_polar"=ccCIpolar,
                   "CI_polar_width"=pWidth,
                   #MLE divided by n instead of (n-1)
                   "x_reliability_ratio"=correlation*sdOFy_divided_by_sdOFx/m,
                   "slope_test_p_value"=p_value,
                   "calculationTerms"=calculationTerms,
                   "givenArguments"=givenArguments)
  if (print_citation_info) {
    internal_print_citations(list("Rayner85"),list(""))
  }
  return(returnList)
}


#' Calculate Exact Confidence Interval for a Two-Variable Relationship via Formula
#'
#' Accepts a
#' standard R formula and optionally a data frame, extracting the response and
#' predictor variables automatically before computing the specified
#' error-in-variable regression
#'
#' @param formula A two-sided formula of the form \code{y ~ x}, where \code{y}
#'   is the response variable and \code{x} is the predictor variable. Both
#'   variables must be numeric vectors of the same length with no missing
#'   values. Multivariate formulas (e.g. \code{y ~ x1 + x2}) are not
#'   supported.
#' @param data An optional data frame (or environment) in which to evaluate
#'   \code{formula}. If \code{NULL} (the default), variables are looked up in
#'   the calling environment.
#'
#' @inheritParams validate_two_variable_exact_CI_function_arguments
#'
#' @returns A named \code{list} with the following elements:
#' \describe{
#'  \item{slope_cartesian}{see [two_variable_exact_CI()]}
#'  \item{slope_polar}{see [two_variable_exact_CI()]}
#'  \item{clockwise_CI_cartesian}{see [two_variable_exact_CI()]}
#'  \item{clockwise_CI_polar}{see [two_variable_exact_CI()]}
#'  \item{counterclockwise_CI_cartesian}{see [two_variable_exact_CI()]}
#'  \item{counterclockwise_CI_polar}{see [two_variable_exact_CI()]}
#'  \item{CI_polar_width}{see [two_variable_exact_CI()]}
#'  \item{x_reliability_ratio}{see [two_variable_exact_CI()]}
#'  \item{slope_test_p_value}{see [two_variable_exact_CI()]}
#'  \item{calculationTerms}{see [two_variable_exact_CI()].}
#'  \item{givenArguments}{see [two_variable_exact_CI()]}
#'  \item{x_error_variance}{estimate of variance of x-axis variable's error,
#'  calculated as `(1-x_reliability_ratio)*var(x)`, where
#'  `var(x)` is calculated by dividing by `(n-1)` instead of `n`}
#'  \item{y_error_variance}{estimate of variance of y-axis variable's error}
#'  \item{y_intercept}{point where line of best fit crosses y-axis}
#'  \item{x_intercept}{point where line of best fit crosses x-axis}
#' }
#'
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' df <- data.frame(y = rnorm(20), x = rnorm(20))
#'
#' # Using a data frame
#' result <- two_variable_exact_CI_formula(
#'   formula             = y ~ x,
#'   data                = df,
#'   lambda = 1,
#'   print_citation_info = FALSE
#' )
#' result$slope_cartesian
#'
#' # Using variables from the calling environment
#' y <- rnorm(20); x <- rnorm(20)
#' result2 <- two_variable_exact_CI_formula(
#'   formula             = y ~ x,
#'   lambda = 1,
#'   print_citation_info = FALSE
#' )
#' result2$slope_cartesian
two_variable_exact_CI_formula <- function(
    formula,
    data                        = NULL,
    lambda,
    slope_hypothesis            = NA,
    CI_coefficient              = 0.95,
    print_citation_info         = TRUE,
    sql_round_to_zero_threshold = 1e-10,
    lostDoF                     = 2,
    debugArg                    = FALSE,
    checkArg                    = TRUE
) {

  # ── 1. Validate the formula argument ────────────────────────────────────────
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula object (e.g. y ~ x).", call. = FALSE)
  }
  if (length(formula) != 3L) {
    stop("`formula` must be two-sided (e.g. y ~ x).", call. = FALSE)
  }
  formula_vars <- all.vars(formula)
  if (length(formula_vars) != 2L) {
    stop(
      "`formula` must contain exactly one response and one predictor ",
      "(e.g. y ~ x). Multivariate formulas are not supported.",
      call. = FALSE
    )
  }

  # ── 2. Extract y and x from data or the calling environment ─────────────────
  # model.frame() handles both the data-frame and environment cases cleanly,
  # and will raise informative errors if variables are not found.
  env <- if (is.null(data)) parent.frame() else as.environment(data)
  mf <- tryCatch(
    stats::model.frame(formula, data = if (is.null(data)) parent.frame() else data),
    error = function(e) stop(
      "Could not extract variables from `formula`: ", conditionMessage(e),
      call. = FALSE
    )
  )
  y_vec <- mf[[1L]]   # response  (left-hand side of formula)
  x_vec <- mf[[2L]]   # predictor (right-hand side of formula)

  # ── 3. Basic sanity checks on the extracted vectors ─────────────────────────
  if (!is.numeric(y_vec)) {
    stop("The response variable (left side of formula) must be numeric.",
         call. = FALSE)
  }
  if (!is.numeric(x_vec)) {
    stop("The predictor variable (right side of formula) must be numeric.",
         call. = FALSE)
  }
  if (anyNA(y_vec) || anyNA(x_vec)) {
    stop(
      "Variables extracted from `formula` must not contain missing values (NA).",
      call. = FALSE
    )
  }

  givenArguments<-list("formula"=formula,
                       "data"=data,
                       "lambda"=lambda,
                       "slope_hypothesis"=slope_hypothesis,
                       "CI_coefficient"=CI_coefficient,
                       "sql_round_to_zero_threshold"=sql_round_to_zero_threshold,
                       "lostDoF"=lostDoF,
                       "print_citation_info"=print_citation_info,
                       "checkArg"=checkArg,
                       "debugArg"=debugArg)
  # Check arguments
  if (checkArg) { # all arguments should at least be passed onto checker.
    validate_two_variable_exact_CI_function_arguments(
      lambda                =lambda, validate_lambda=TRUE,
      CI_coefficient        =CI_coefficient, validate_CI_coefficient=TRUE,
      slope_hypothesis      =slope_hypothesis, validate_slope_hypothesis=TRUE,
      sql_round_to_zero_threshold=sql_round_to_zero_threshold,validate_sql_round_to_zero_threshold=TRUE,
      lostDoF               =lostDoF, validate_lostDoF=TRUE,
      print_citation_info   =print_citation_info, validate_print_citation_info=TRUE,
      checkArg              =checkArg, validate_checkArg=TRUE,
      debugArg              =debugArg, validate_debugArg=TRUE,
      y                     =y_vec,validate_y=TRUE,
      x                     =x_vec,validate_x=TRUE,
      validate_x_and_y=TRUE
    )
  }

  # ── 4. Compute the summary statistics required by two_variable_exact_CI() ───
  correlation            <- stats::cor(x_vec, y_vec)
  sdOFy_divided_by_sdOFx <- stats::sd(y_vec) / stats::sd(x_vec)
  square_root_of_lambda=sqrt(lambda)
  sample_size <- length(y_vec)
  mean_y<-mean(y_vec)
  mean_x<-mean(x_vec)

  # ── 5. Delegate to two_variable_exact_CI() ──────────────────────────────────
  # Argument validation is already handled above, so checkArg = FALSE avoids
  # redundant checking in the inner function.
  inner <- two_variable_exact_CI(
    correlation             = correlation,
    sdOFy_divided_by_sdOFx  = sdOFy_divided_by_sdOFx,
    square_root_of_lambda   = square_root_of_lambda,
    sample_size             = sample_size,
    slope_hypothesis        = slope_hypothesis,
    CI_coefficient          = CI_coefficient,
    sql_round_to_zero_threshold = sql_round_to_zero_threshold,
    lostDoF                 = lostDoF,
    print_citation_info     = print_citation_info,
    checkArg                = FALSE,   # validated above
    debugArg                = debugArg
  )

  # ── 6. Build the return list explicitly ─────────────────────────────────────
  # Each element is assigned by name so that this list can be extended or
  # modified independently of two_variable_exact_CI()'s output in future.
  returnList <- list(
    slope_cartesian                = inner$slope_cartesian,
    slope_polar                    = inner$slope_polar,
    clockwise_CI_cartesian         = inner$clockwise_CI_cartesian,
    clockwise_CI_polar             = inner$clockwise_CI_polar,
    counterclockwise_CI_cartesian  = inner$counterclockwise_CI_cartesian,
    counterclockwise_CI_polar      = inner$counterclockwise_CI_polar,
    CI_polar_width                 = inner$CI_polar_width,
    x_reliability_ratio            = inner$x_reliability_ratio,
    slope_test_p_value             = inner$slope_test_p_value,
    calculationTerms               = inner$calculationTerms,
    givenArguments                 = givenArguments
  )
  returnList$y_intercept <- mean_y - returnList$slope_cartesian*mean_x
  returnList$x_intercept <- mean_x - mean_y/returnList$slope_cartesian
  if (square_root_of_lambda<sql_round_to_zero_threshold) {
    returnList$x_related<-(y_vec-returnList$y_intercept)/returnList$slope_cartesian
    returnList$y_related<-y_vec
    returnList$x_residual<-x_vec-returnList$x_related
    returnList$y_residual<-rep(0,sample_size)
    returnList$x_error_variance<-NA
    returnList$y_error_variance<-0
  } else {
    b00<-returnList$slope_cartesian/(returnList$slope_cartesian^2+lambda)
    b01<-y_vec-returnList$y_intercept-returnList$slope_cartesian*x_vec
    returnList$x_related<-x_vec+b00*b01
    returnList$y_related<-returnList$slope_cartesian*returnList$x_related+returnList$y_intercept
    returnList$x_residual<-x_vec-returnList$x_related
    returnList$y_residual<-y_vec-returnList$y_related
    returnList$x_error_variance<-NA
    returnList$y_error_variance<-NA
  }
  return(returnList)
}

