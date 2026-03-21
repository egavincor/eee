# Home for GLOBAL package stuff
# Edric Gavin


#' Internal Package Environment
#'
#' Private environment used to store mutable package-level state for the
#' package.
#'
#' This environment contains internal variables that control package
#' behavior during runtime without placing objects in the user's global
#' environment.
#'
#' Currently stored variables include:
#'
#' * `GLOBAL_print_citations` — Logical scalar controlling whether package
#'   functions print citation information.
#'     * When `TRUE`, functions may print citation information according to
#'       their individual settings.
#'     * When `FALSE`, citation printing is globally suppressed regardless
#'       of individual function defaults.
#'
#' Users should not modify objects in this environment directly. Instead,
#' use [change_citation_printing_default()] to change the global citation
#' printing behavior.
#'
#' @keywords internal
#' @noRd
eee_internal_environment_0000 <- new.env(parent = emptyenv())


eee_internal_environment_0000$GLOBAL_print_citations<-TRUE


#' Internal Citation Dictionary
#'
#' A named list mapping short citation identifiers used throughout the
#' package to the full formatted citation strings printed to the user.
#'
#' This centralized dictionary allows citation text to be updated in a
#' single location without modifying every function that references the
#' citation.
#'
#' Keys are short identifiers (e.g., `"McArdle88"`) and values are the
#' full citation text displayed to users when citation printing is enabled.
#'
#' @format A named list of character strings.
#'
#' @details
#' Package functions pass citation identifiers to an internal printing
#' helper (`internal_print_citations()`). That function retrieves the
#' corresponding citation text from this object and formats it for display.
#'
#' @keywords internal
#' @noRd
citationStrings<-list(
  "lmodel2"="R package `lmodel2`",
  "McArdle88"=paste0("McArdle, B. H. (1988). The structural relationship:\n",
      "       regression in biology. Canadian journal of zoology,\n",
      "       66(11), 2329-2339."),
  "JolMos68"=paste0("JOLICOEUR, P. & MOSIMANN, J. E. (1968).\n",
      "       Intervalles de confiance pour la pente de l'axe majeur d'une\n",
      "       distribution normale bidimensionnelle. Biom~trie-Praxim~trie\n",
      "       9, 121-140."),
  "Rayner85"=paste0("Rayner, J. M. (1985). Linear relations in biomechanics:\n",
      "       the statistics of scaling functions.\n",
      "       Journal of Zoology, 206(3), 415-439."),
  "AuthorAllLambdaPaperIdentifier123"="Package Author's Paper (in prep.)"
  )


#' Change Default Citation Printing Behavior
#'
#' Sets the global default controlling whether functions in this package
#' automatically print citation information.
#'
#' If set to `FALSE`, citation messages are completely suppressed for all
#' package functions, regardless of their individual default behavior.
#'
#' If set to `TRUE`, the global override is disabled and each function
#' behaves according to its own default citation-printing settings.
#'
#' This function modifies a package-internal global flag stored in an
#' internal environment. It allows users to disable citation printing
#' across the entire package without modifying individual function calls.
#'
#' @param TRUE_for_print_FALSE_for_not Logical scalar.
#' `FALSE` disables all automatic citation printing.
#' `TRUE` restores the default per-function citation behavior.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' # Disable all citation printing
#' change_citation_printing_default(FALSE)
#'
#' # Restore normal function-level behavior
#' change_citation_printing_default(TRUE)
#'
#' @export
change_citation_printing_default<-function(TRUE_for_print_FALSE_for_not) {
  eee_internal_environment_0000$GLOBAL_print_citations<-TRUE_for_print_FALSE_for_not
}


#' Print Citation Information
#'
#' Internal helper used by package functions to print citation information
#' for methods implemented in the package.
#'
#' The function looks up citation identifiers in the internal
#' `citationStrings` dictionary and prints the associated formatted
#' references.
#'
#' Citation printing only occurs when the global flag
#' `GLOBAL_print_citations` is `TRUE`. If the user disables citation
#' printing via `change_citation_printing_default(FALSE)`, this function
#' produces no output.
#'
#' @param citedItemList Character vector or list of citation identifiers.
#' Each identifier must correspond to a name in `citationStrings`.
#'
#' @return No return value. Called for its side-effect of printing text.
#'
#' @details
#' In addition to the requested citations, the function always appends
#' references to:
#'
#' * R software
#' * This package
#'
#' The output is formatted as a numbered list.
#'
#' @keywords internal
#' @noRd
internal_print_citations<-function(citedItemList=list(), messages=list()){
  if (eee_internal_environment_0000$GLOBAL_print_citations) { #only need to take action if actually printing
    nItems<-length(citedItemList)
    fI<-function(fIndex) { #formats index for printing
      if (nItems>7 && fIndex<10) {
        return(paste0("\n  ",fIndex,") "))
      } else {
        return(paste0("\n ",fIndex,") "))
      }
    }
    pS00<-paste0("you can TURN OFF autoprint",
                 " citations for all functions with\n",
                 "change_citation_printing_default(FALSE)\n")
    pS01<-"For this function please cite ..."
    pSmid<-""
    if (nItems>0) {
      if (nItems!=length(messages)) {
        stop("length of <citedItemList> and <messages> must be identical")
      }
      for (index1 in 1:nItems) {
        pSmid<-paste0(pSmid,fI(index1), messages[[index1]],
                      citationStrings[[citedItemList[[index1]]]])
      }
    }
    pS07<-paste0(fI(nItems+1),"R Software")
    pS08<-paste0(fI(nItems+2),"This package")
    pS09<-"\nThank you. \n"
    cat(pS00,pS01,pSmid,pS07,pS08,pS09, sep = "")
  }
}


#' Validate Boolean Flag Arguments
#'
#' Ensures that one or more arguments are valid logical flags.
#'
#' Each argument must be a single non-`NA` logical value (`TRUE` or `FALSE`).
#' This helper is intended for validating function parameters that control
#' optional behavior such as printing, debugging, or input checking.
#'
#' @param ... Named or unnamed arguments to validate.
#'
#' @return Invisibly returns `TRUE` if all checks pass. Otherwise an error
#' is thrown.
#'
#' @details
#' If arguments are named, their names are used in error messages.
#' Unnamed arguments are labeled automatically (e.g. `"argument_1"`).
#'
#' @keywords internal
#' @noRd
validate_boolean_flags <- function(...) {
  flags <- list(...)
  flag_names <- names(flags)
  if (is.null(flag_names)) {
    flag_names <- paste0("argument_", seq_along(flags))
  }
  for (i in seq_along(flags)) {
    value <- flags[[i]]
    name <- flag_names[i]
    if (name == "") {
      name <- paste0("argument_", i)
    }
    if (length(value) != 1 || !is.logical(value) || is.na(value)) {
      stop(sprintf(
        "`%s` must be TRUE or FALSE (a single non-NA logical value).",
        name
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}


#' Validate Scalar Input
#'
#' Confirms that a value is a scalar (length-1) object of a specified type
#' and is not `NA`.
#'
#' This helper function is used internally to validate user inputs for
#' package functions.
#'
#' @param x Value to validate.
#' @param name Character string giving the argument name, used in
#' error messages.
#' @param type Character string specifying the required type of `x`.
#' Must be either `"numeric"` or `"logical"`.
#'
#' @return Invisibly returns `TRUE` if validation succeeds. Otherwise an
#' error is thrown.
#'
#' @details
#' The function checks that:
#'
#' * `x` has length 1
#' * `x` is not `NA`
#' * `x` is of the specified type
#'
#' Clear error messages referencing `name` are generated if validation fails.
#'
#' @keywords internal
#' @noRd
validate_scalar <- function(x, name, type) {
  if (!type %in% c("numeric", "logical")) {
    stop("`type` must be 'numeric' or 'logical'.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  if (length(x) != 1) {
    stop(sprintf("`%s` must be a single value.", name), call. = FALSE)
  }
  if (is.na(x)) {
    stop(sprintf("`%s` cannot be NA.", name), call. = FALSE)
  }
  if (type == "numeric" && !is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }
  if (type == "logical" && !is.logical(x)) {
    stop(sprintf("`%s` must be logical.", name), call. = FALSE)
  }
  invisible(TRUE)
}
