# --- 1. Internal Environment & Flag Tests ---

test_that("Internal environment and global flag are configured correctly", {
  # Check the environment itself
  expect_type(eee_internal_environment_0000, "environment")
  expect_identical(parent.env(eee_internal_environment_0000), emptyenv())

  # Check the default flag
  expect_true(exists("GLOBAL_print_citations", envir = eee_internal_environment_0000))
  expect_type(eee_internal_environment_0000$GLOBAL_print_citations, "logical")
  expect_length(eee_internal_environment_0000$GLOBAL_print_citations, 1)
})

# --- 2. Citation Dictionary Tests ---

test_that("citationStrings is a properly formatted named list", {
  expect_type(citationStrings, "list")
  expect_named(citationStrings) # Ensures the list has names

  # Verify all elements are character strings
  is_char <- vapply(citationStrings, is.character, logical(1))
  expect_true(all(is_char), info = "All citation entries must be character strings.")

  # Check for the existence of required keys
  expected_keys <- c("lmodel2", "McArdle88", "JolMos68", "Rayner85")
  expect_true(all(expected_keys %in% names(citationStrings)))
})

# --- 3. State Modification Tests ---

test_that("change_citation_printing_default modifies the global flag", {
  # Save the original state to restore it after the test
  orig_state <- eee_internal_environment_0000$GLOBAL_print_citations
  on.exit({
    eee_internal_environment_0000$GLOBAL_print_citations <- orig_state
  })

  # Test setting to FALSE
  change_citation_printing_default(FALSE)
  expect_false(eee_internal_environment_0000$GLOBAL_print_citations)

  # Test setting to TRUE
  change_citation_printing_default(TRUE)
  expect_true(eee_internal_environment_0000$GLOBAL_print_citations)
})

# --- 4. Output & Printing Tests ---

test_that("internal_print_citations produces correct output based on global flag", {
  # Save original state
  orig_state <- eee_internal_environment_0000$GLOBAL_print_citations
  on.exit({
    eee_internal_environment_0000$GLOBAL_print_citations <- orig_state
  })

  # Scenario A: Printing is ENABLED
  change_citation_printing_default(TRUE)

  # expect_output captures cat() and print() output to match against a regex
  expect_output(
    internal_print_citations(list("lmodel2", "Rayner85")),
    regexp = "you can TURN OFF.*lmodel2.*Rayner.*R Software.*This package",
    ignore.case = TRUE
  )

  # Check that numbering formatting works (1, 2, 3, 4)
  expect_output(
    internal_print_citations(list("lmodel2")),
    regexp = "1\\).*2\\) R Software.*3\\) This package"
  )

  # Scenario B: Printing is DISABLED
  change_citation_printing_default(FALSE)

  # expect_silent ensures absolutely no messages, warnings, or cat() output occur
  expect_silent(
    internal_print_citations(list("lmodel2"))
  )
})

# --- 5. Tests for validate_boolean_flags ---

test_that("validate_boolean_flags accepts valid logical scalars", {
  # Testing expect_invisible since the function returns invisibly
  expect_invisible(validate_boolean_flags(TRUE))

  # expect_true also works well here since it returns TRUE on success
  expect_true(validate_boolean_flags(FALSE))

  # Testing multiple unnamed flags
  expect_true(validate_boolean_flags(TRUE, FALSE, TRUE))

  # Testing multiple named flags
  expect_true(validate_boolean_flags(flag_a = TRUE, flag_b = FALSE))
})

test_that("validate_boolean_flags rejects invalid types", {
  # Named arguments should show the name in the error
  expect_error(
    validate_boolean_flags(bad_flag = 1),
    regexp = "`bad_flag` must be TRUE or FALSE"
  )
  expect_error(
    validate_boolean_flags(char_flag = "TRUE"),
    regexp = "`char_flag` must be TRUE or FALSE"
  )

  # Unnamed arguments should auto-generate "argument_i"
  expect_error(
    validate_boolean_flags(TRUE, "FALSE"),
    regexp = "argument_2.*must be TRUE or FALSE"
  )
  expect_error(
    validate_boolean_flags("a"=TRUE, "FALSE"),
    regexp = "argument_2.*must be TRUE or FALSE"
  )
  expect_error(
    validate_boolean_flags(NULL),
    regexp = "argument_1.*must be TRUE or FALSE"
  )
})

test_that("validate_boolean_flags rejects invalid lengths and NAs", {
  expect_error(
    validate_boolean_flags(vec_flag = c(TRUE, FALSE)),
    regexp = "`vec_flag` must be TRUE or FALSE"
  )
  expect_error(
    validate_boolean_flags(c(TRUE, FALSE)),
    regexp = "argument_1.*must be TRUE or FALSE"
  )
  expect_error(
    validate_boolean_flags(empty_flag = logical(0)),
    regexp = "`empty_flag` must be TRUE or FALSE"
  )
  expect_error(
    validate_boolean_flags(na_flag = NA),
    regexp = "`na_flag` must be TRUE or FALSE"
  )
})

# --- 6. Tests for validate_scalar ---

test_that("validate_scalar validates its own internal arguments (type, name)", {
  # Invalid 'type' parameter
  expect_error(
    validate_scalar(1, "my_var", type = "character"),
    regexp = "`type` must be 'numeric' or 'logical'"
  )

  # Invalid 'name' parameters
  expect_error(
    validate_scalar(1, name = "", type = "numeric"),
    regexp = "`name` must be a single non-empty character string"
  )
  expect_error(
    validate_scalar(1, name = c("a", "b"), type = "numeric"),
    regexp = "`name` must be a single non-empty character string"
  )
  expect_error(
    validate_scalar(1, name = NA_character_, type = "numeric"),
    regexp = "`name` must be a single non-empty character string"
  )
  expect_error(
    validate_scalar(1, name = 5, type = "numeric"),
    regexp = "`name` must be a single non-empty character string"
  )
})

test_that("validate_scalar accepts valid scalar inputs", {
  # Numeric validations (both double and integer classes are "numeric" in R)
  expect_invisible(validate_scalar(42.5, "my_double", "numeric"))
  expect_true(validate_scalar(10L, "my_int", "numeric"))

  # Logical validations
  expect_true(validate_scalar(TRUE, "my_bool", "logical"))
  expect_true(validate_scalar(FALSE, "my_bool", "logical"))
})

test_that("validate_scalar rejects invalid lengths and NAs", {
  # Length issues
  expect_error(
    validate_scalar(c(1, 2), "my_num", "numeric"),
    regexp = "`my_num` must be a single value"
  )
  expect_error(
    validate_scalar(numeric(0), "my_num", "numeric"),
    regexp = "`my_num` must be a single value"
  )

  # NA issues
  expect_error(
    validate_scalar(NA_real_, "my_num", "numeric"),
    regexp = "`my_num` cannot be NA"
  )
  expect_error(
    validate_scalar(NA, "my_log", "logical"),
    regexp = "`my_log` cannot be NA"
  )
})

test_that("validate_scalar rejects type mismatches", {
  expect_error(
    validate_scalar(TRUE, "my_num", "numeric"),
    regexp = "`my_num` must be numeric"
  )
  expect_error(
    validate_scalar("10", "my_num", "numeric"),
    regexp = "`my_num` must be numeric"
  )
  expect_error(
    validate_scalar(1, "my_log", "logical"),
    regexp = "`my_log` must be logical"
  )
})
