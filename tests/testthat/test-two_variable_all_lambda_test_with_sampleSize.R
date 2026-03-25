test_that("two_variable_exact_CI", {

  # Check near Inf lambda, givenArguments, argument checks
  expected_names <- c(
    "correlation", "sdOFy_divided_by_sdOFx", "square_root_of_lambda",
    "sample_size", "slope_hypothesis", "CI_coefficient",
    "lostDoF", "sql_round_to_zero_threshold","print_citation_info",
    "checkArg", "debugArg"
  )
  y<-c(1.62168318085215,-0.337278839171313,0.389150100303892,1.15660399927245,-0.400327274413509,-1.29768656104402,-2.35366542944604,0.0712206762586596,0.242645476125059,2.46994157052957,0.782749852800733,-0.541670871833835,-1.79666708694525,-0.408630336106261,0.364920581996794,0.68945826939902,0.395994928914202,-1.00967472545052,-0.476385067592619,-2.67512466275994,-1.02135741035027,1.63147875243167,0.905478546619372,0.584673882991468,-0.481917057761096,0.0846278728117671,2.08275622305479,-0.966731868784858,-0.0285805365660883,0.693400562683697)
  n<-length(y)
  x<-c(1.02724772844129,-0.869121615715904,0.896488945465718,0.622957674633172,1.14139218184186,-0.38803828781651,0.0635933307322598,-0.724233556282441,1.23253692810584,2.12530710700368,2.52149655095117,-0.238263158101312,-1.60044098178951,-1.12150486980606,-1.46578566498202,-1.47710141318336,-1.31155860291487,0.264615869865611,1.26773042792811,-1.46486711335471,0.0418694763937954,0.000692927398072973,2.57470098970943,0.00885039248953601,-0.111984248095387,0.753749295087879,1.70401265571193,-0.863577342342688,-3.50849962788213,-1.21688821278223)
  cxy<-cor(x,y)
  syx<-sd(y)/sd(x)
  sqlam01<-10^100
  a3<-two_variable_exact_CI(correlation=cxy,
                                 sdOFy_divided_by_sdOFx=syx,
                                 square_root_of_lambda=sqlam01,
                                 sample_size=n,
                                 print_citation_info=FALSE)
  expect_equal(round(a3$slope_cartesian,15), round(0.374480607116664,15))
  expect_equal(round(a3$slope_polar,10), round(20.5299506656645,10))
  expect_equal(round(a3$clockwise_CI_cartesian,15), round(0.0709886980454927,15))
  expect_equal(round(a3$clockwise_CI_polar,10), round(4.06054104278031,10))
  expect_equal(round(a3$counterclockwise_CI_cartesian,15), round(0.677972516187836,15))
  expect_equal(round(a3$counterclockwise_CI_polar,10), round(34.1361918679234,10))
  expect_equal(round(a3$CI_polar_width,10), round(30.0756508251431,10))
  expect_true(is.numeric(a3$x_reliability_ratio))
  expect_true(is.na(a3$slope_test_p_value))
  expect_equal(length(a3$givenArguments),11)
  expect_true(setequal(names(a3$givenArguments), expected_names))
  a4<-two_variable_exact_CI(correlation=-cxy,
                                 sdOFy_divided_by_sdOFx=syx,
                                 square_root_of_lambda=sqlam01,
                                 sample_size=n,
                                 print_citation_info=FALSE,
                                 slope_hypothesis=-1)
  expect_equal(round(a4$slope_cartesian,15), round(-0.374480607116664,15))
  expect_equal(round(a4$slope_polar,10), round(-20.5299506656645,10))
  expect_equal(round(a4$clockwise_CI_cartesian,15), round(-0.677972516187836,15))
  expect_equal(round(a4$clockwise_CI_polar,10), round(-34.1361918679234,10))
  expect_equal(round(a4$counterclockwise_CI_cartesian,15), round(-0.0709886980454927,15))
  expect_equal(round(a4$counterclockwise_CI_polar,10), round(-4.06054104278031,10))
  expect_equal(round(a4$CI_polar_width,10), round(30.0756508251431,10))
  expect_true(is.numeric(a4$x_reliability_ratio))
  expect_equal(a4$slope_test_p_value,round(0.0002311470130027674972986,15))
  expect_true(a4$givenArguments$checkArg)
  expect_false(a4$givenArguments$debugArg)
  expect_false(a4$givenArguments$print_citation_info)
  expect_equal(a4$givenArguments$lostDoF,2)
  expect_equal(a4$givenArguments$CI_coefficient,0.95)
  expect_equal(a4$givenArguments$slope_hypothesis,-1)
  expect_equal(a4$givenArguments$sample_size,n)
  expect_equal(a4$givenArguments$correlation,-cxy)
  expect_equal(a4$givenArguments$sdOFy_divided_by_sdOFx,syx)
  expect_equal(a4$givenArguments$square_root_of_lambda,1e+100)
  # despite having negative sdy/sdx ratio, no error thrown with checkArg=FALSE
  a5<-two_variable_exact_CI(correlation=1,
                                 sdOFy_divided_by_sdOFx=-syx,
                                 square_root_of_lambda=sqlam01,
                                 sample_size=n,
                                 print_citation_info=FALSE,
                                 slope_hypothesis=-10,
                                 checkArg=FALSE)
  expect_error(
    two_variable_exact_CI(
      correlation = 1,
      sdOFy_divided_by_sdOFx = -syx, # The cause of the error
      square_root_of_lambda = sqlam01,
      sample_size = n,
      print_citation_info = FALSE,
      slope_hypothesis = -10,
      checkArg = TRUE
    ),
    # Use a regular expression to match the specific error message
    regexp = "`sdOFy_divided_by_sdOFx` must be >0"
  )
  expect_error(
    two_variable_exact_CI(
      correlation = 1,
      sdOFy_divided_by_sdOFx = syx, # The cause of the error
      square_root_of_lambda = -sqlam01,
      sample_size = n,
      print_citation_info = FALSE,
      slope_hypothesis = -10,
      checkArg = TRUE
    ),
    # Use a regular expression to match the specific error message
    regexp = "`square_root_of_lambda` must be >=0"
  )
  expect_error(
    two_variable_exact_CI(
      correlation = 1,
      sdOFy_divided_by_sdOFx = syx, # The cause of the error
      square_root_of_lambda = sqlam01,
      sample_size = -n,
      print_citation_info = FALSE,
      slope_hypothesis = -10,
      checkArg = TRUE
    ),
    # Use a regular expression to match the specific error message
    regexp = "`sample_size` must be greater than 2"
  )
  expect_error(
    two_variable_exact_CI(
      correlation = 1,
      sdOFy_divided_by_sdOFx = syx, # The cause of the error
      square_root_of_lambda = sqlam01,
      sample_size = n,
      print_citation_info = 5,
      slope_hypothesis = 10,
      checkArg = TRUE
    ),
    # Use a regular expression to match the specific error message
    regexp = "`print_citation_info` must be logical"
  )
  expect_error(
    two_variable_exact_CI(
      correlation = 1,
      sdOFy_divided_by_sdOFx = syx, # The cause of the error
      square_root_of_lambda = sqlam01,
      sample_size = n,
      print_citation_info =FALSE,
      slope_hypothesis = FALSE,
      checkArg = TRUE
    ),
    # Use a regular expression to match the specific error message
    regexp = "`slope_hypothesis` must be numeric"
  )

  #test zero and near zero lambda
  b1<-two_variable_exact_CI(correlation=cxy,
                            sdOFy_divided_by_sdOFx=syx,
                            square_root_of_lambda=0,
                            sample_size=n,
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(b1$slope_cartesian,15), round(2.01578968480004,15))
  expect_equal(round(b1$slope_polar,10), round(63.6147490929255,10))
  expect_equal(round(b1$clockwise_CI_cartesian,15), round(1.11342882928057,15))
  expect_equal(round(b1$clockwise_CI_polar,10), round(48.0721468804829,10))
  expect_equal(round(b1$counterclockwise_CI_cartesian,15), round(10.6337229131836,15))
  expect_equal(round(b1$counterclockwise_CI_polar,10), round(84.6276793027248,10))
  expect_equal(round(b1$CI_polar_width,10), round(36.5555324222419,10))
  expect_true(is.numeric(b1$x_reliability_ratio))
  expect_equal(b1$slope_test_p_value,round(0.01587505455605661697227,15))
  set.seed(0); n<-10; y1<-rnorm(n); x1<-rnorm(n); y2<--y1
  b2<-two_variable_exact_CI(correlation=cor(x1,y1),
                            sdOFy_divided_by_sdOFx=sd(y1)/sd(x1),
                            square_root_of_lambda=0,
                            sample_size=n,
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(b2$slope_cartesian,15), round(-7.23020920600019,15))
  expect_equal(round(b2$slope_polar,10), round(-82.1254596334547,10))
  expect_equal(round(b2$clockwise_CI_cartesian,15), round(3.25920814745955,15))
  expect_equal(round(b2$clockwise_CI_polar,10), round(-107.057217541695,10))
  expect_equal(round(b2$counterclockwise_CI_cartesian,15), round(-1.71397182579868,15))
  expect_equal(round(b2$counterclockwise_CI_polar,5), round(-59.7389987380566,5))
  expect_equal(round(b2$CI_polar_width,10), round(47.3182213383672,10))
  expect_true(is.numeric(b2$x_reliability_ratio))
  expect_equal(b2$slope_test_p_value,round(0.0003629677688609799922521,15))
  b3<-two_variable_exact_CI(correlation=cor(x1,y2),
                            sdOFy_divided_by_sdOFx=sd(y2)/sd(x1),
                            square_root_of_lambda=0,
                            sample_size=n,
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(b3$slope_cartesian,15), round(7.23020920600019,15))
  expect_equal(round(b3$slope_polar,10), round(82.1254596334547,10))
  expect_equal(round(b3$clockwise_CI_cartesian,15), round(1.71397182579868,15))
  expect_equal(round(b3$clockwise_CI_polar,5), round(59.7389987380566,5))
  expect_equal(round(b3$counterclockwise_CI_cartesian,15), round(-3.25920814745955,15))
  expect_equal(round(b3$counterclockwise_CI_polar,5), round(107.057217541695,5))
  expect_equal(round(b3$CI_polar_width,10), round(47.3182213383672,10))
  expect_true(is.numeric(b3$x_reliability_ratio))
  expect_equal(b3$slope_test_p_value,round(0.002099903863610729556205,15))
  b4<-two_variable_exact_CI(correlation=cor(x1,y2),
                            sdOFy_divided_by_sdOFx=sd(y2)/sd(x1),
                            square_root_of_lambda=10e-13,
                            sample_size=n,
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(b4$slope_cartesian,15), round(7.23020920600019,15))
  expect_equal(round(b4$slope_polar,10), round(82.1254596334547,10))
  expect_equal(round(b4$clockwise_CI_cartesian,15), round(1.71397182579868,15))
  expect_equal(round(b4$clockwise_CI_polar,5), round(59.7389987380566,5))
  expect_equal(round(b4$counterclockwise_CI_cartesian,15), round(-3.25920814745955,15))
  expect_equal(round(b4$counterclockwise_CI_polar,5), round(107.057217541695,5))
  expect_equal(round(b4$CI_polar_width,10), round(47.3182213383672,10))
  expect_true(is.numeric(b4$x_reliability_ratio))
  expect_equal(b4$slope_test_p_value,round(0.002099903863610729556205,15))
  b5<-two_variable_exact_CI(correlation=cor(x1,y2),
                            sdOFy_divided_by_sdOFx=sd(y2)/sd(x1),
                            square_root_of_lambda=10e-6,
                            sample_size=n,
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(b5$slope_cartesian,15), round(7.23020920600019,15))
  expect_equal(round(b5$slope_polar,10), round(82.1254596334547,10))
  expect_equal(round(b5$clockwise_CI_cartesian,15), round(1.71397182579868,15))
  expect_equal(round(b5$clockwise_CI_polar,5), round(59.7389987380566,5))
  expect_equal(round(b5$counterclockwise_CI_cartesian,15), round(-3.25920814745955,15))
  expect_equal(round(b5$counterclockwise_CI_polar,5), round(107.057217541695,5))
  expect_equal(round(b5$CI_polar_width,10), round(47.3182213383672,10))
  expect_true(is.numeric(b5$x_reliability_ratio))
  expect_equal(b5$slope_test_p_value,round(0.002099903863610729556205,15))

  #test MA (lambda=1)
  c1<-two_variable_exact_CI(correlation=cor(x1,y2),
                            sdOFy_divided_by_sdOFx=sd(y2)/sd(x1),
                            square_root_of_lambda=1,
                            sample_size=n,
                            CI_coefficient=0.95,
                            print_citation_info=FALSE,
                            slope_hypothesis=-5.13152523882259,
                            checkArg=TRUE)
  expect_equal(round(c1$slope_cartesian,10), round(5.13152523882259,10))
  expect_equal(round(c1$slope_polar,10), round(78.9727551096947,10))
  expect_true(is.na(c1$clockwise_CI_cartesian))
  expect_true(is.na(c1$clockwise_CI_polar))
  expect_true(is.na(c1$counterclockwise_CI_cartesian))
  expect_true(is.na(c1$counterclockwise_CI_polar))
  expect_true(is.na(c1$CI_polar_width))
  expect_true(is.numeric(c1$x_reliability_ratio))
  expect_equal(c1$slope_test_p_value,round(0.220766116569667,10))
  c1a<-two_variable_exact_CI(correlation=cor(x1,y2),
                            sdOFy_divided_by_sdOFx=sd(y2)/sd(x1),
                            square_root_of_lambda=1,
                            sample_size=n,
                            CI_coefficient=0.80,
                            print_citation_info=FALSE,
                            slope_hypothesis=-1.45,
                            checkArg=TRUE)
  expect_equal(round(c1a$slope_cartesian,10), round(5.13152523882259,10))
  expect_equal(round(c1a$slope_polar,10), round(78.9727551096947,10))
  expect_equal(round(c1a$clockwise_CI_cartesian,10), round(1.45204020251556,10))
  expect_equal(round(c1a$clockwise_CI_polar,5), round(55.4453530920995,5))
  expect_equal(round(c1a$counterclockwise_CI_cartesian,15), round(-4.51064996401304,10))
  expect_equal(round(c1a$counterclockwise_CI_polar,5), round(102.50015712729,5))
  expect_equal(round(c1a$CI_polar_width,10), round(47.0548040351905,10))
  expect_true(is.numeric(c1a$x_reliability_ratio))
  expect_equal(c1a$slope_test_p_value,round(0.0928516680564616,10))
  c2<-two_variable_exact_CI(correlation=cor(x1,y1),
                            sdOFy_divided_by_sdOFx=sd(y1)/sd(x1),
                            square_root_of_lambda=1,
                            sample_size=n,
                            print_citation_info=FALSE,
                            slope_hypothesis=-5.13152523882259,
                            checkArg=TRUE)
  expect_equal(round(c2$slope_cartesian,10), round(-5.13152523882259,10))
  expect_equal(round(c2$slope_polar,10), round(-78.9727551096947,10))
  expect_true(is.na(c2$clockwise_CI_cartesian))
  expect_true(is.na(c2$clockwise_CI_polar))
  expect_true(is.na(c2$counterclockwise_CI_cartesian))
  expect_true(is.na(c2$counterclockwise_CI_polar))
  expect_true(is.na(c2$CI_polar_width))
  expect_true(is.numeric(c2$x_reliability_ratio))
  expect_equal(c2$slope_test_p_value,round(1,10))
  c2a<-two_variable_exact_CI(correlation=cor(x1,y1),
                             sdOFy_divided_by_sdOFx=sd(y1)/sd(x1),
                             square_root_of_lambda=1,
                             sample_size=n,
                             CI_coefficient=0.80,
                             print_citation_info=FALSE,
                             slope_hypothesis=5.13152523882259,
                             checkArg=TRUE)
  expect_equal(round(c2a$slope_cartesian,10), round(-5.13152523882259,10))
  expect_equal(round(c2a$slope_polar,10), round(-78.9727551096947,10))
  expect_equal(round(c2a$clockwise_CI_cartesian,10), round(4.51064996401304,10))
  expect_equal(round(c2a$clockwise_CI_polar,5), round(-102.50015712729,5))
  expect_equal(round(c2a$counterclockwise_CI_cartesian,15), round(-1.45204020251556,10))
  expect_equal(round(c2a$counterclockwise_CI_polar,5), round(-55.4453530920995,5))
  expect_equal(round(c2a$CI_polar_width,10), round(47.0548040351905,10))
  expect_true(is.numeric(c2a$x_reliability_ratio))
  expect_equal(c2a$slope_test_p_value,round(0.220766116569667,10))
  c3<-two_variable_exact_CI(correlation=cxy,
                            sdOFy_divided_by_sdOFx=syx,
                            square_root_of_lambda=1,
                            sample_size=length(y),
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(c3$slope_cartesian,15), round(0.724908630966859,15))
  expect_equal(round(c3$slope_polar,10), round(35.938680302493,10))
  expect_equal(round(c3$clockwise_CI_cartesian,15), round(0.189880594207318,15))
  expect_equal(round(c3$clockwise_CI_polar,5), round(10.7513638669367,5))
  expect_equal(round(c3$counterclockwise_CI_cartesian,15), round(1.8134411219583,15))
  expect_equal(round(c3$counterclockwise_CI_polar,5), round(61.1259967380492,5))
  expect_equal(round(c3$CI_polar_width,10), round(50.3746328711125,10))
  expect_true(is.numeric(c3$x_reliability_ratio))
  expect_equal(c3$slope_test_p_value,round(0.415099248868063,15))
  c4<-two_variable_exact_CI(correlation=-cxy,
                            sdOFy_divided_by_sdOFx=syx,
                            square_root_of_lambda=1,
                            sample_size=length(y),
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=TRUE)
  expect_equal(round(c4$slope_cartesian,15), round(-0.724908630966859,15))
  expect_equal(round(c4$slope_polar,10), round(-35.938680302493,10))
  expect_equal(round(c4$clockwise_CI_cartesian,15), round(-1.8134411219583,15))
  expect_equal(round(c4$clockwise_CI_polar,5), round(-61.1259967380492,5))
  expect_equal(round(c4$counterclockwise_CI_cartesian,15), round(-0.189880594207318,15))
  expect_equal(round(c4$counterclockwise_CI_polar,5), round(-10.7513638669367,5))
  expect_equal(round(c4$CI_polar_width,10), round(50.3746328711125,10))
  expect_true(is.numeric(c4$x_reliability_ratio))
  expect_equal(c4$slope_test_p_value,round(0.415099248868063,15))

  #test SMA (coincidence)
  d1<-two_variable_exact_CI(correlation=cxy,
                            sdOFy_divided_by_sdOFx=syx,
                            square_root_of_lambda=syx,
                            sample_size=length(y),
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=FALSE)
  expect_equal(round(d1$slope_cartesian,15), round(syx,15))
  expect_equal(round(d1$slope_polar,10), round(40.9852521723021,10))
  expect_equal(round(d1$clockwise_CI_cartesian,15), round(0.28114206899172,15))
  expect_equal(round(d1$clockwise_CI_polar,5), round(15.7029069827168,5))
  expect_equal(round(d1$counterclockwise_CI_cartesian,15), round(2.6850273517965,15))
  expect_equal(round(d1$counterclockwise_CI_polar,5), round(69.5728737250825,5))
  expect_equal(round(d1$CI_polar_width,10), round(53.8699667423657,10))
  expect_true(is.numeric(d1$x_reliability_ratio))
  expect_equal(d1$slope_test_p_value,round(0.726691816927342,15))
  d2<-two_variable_exact_CI(correlation=-cxy,
                            sdOFy_divided_by_sdOFx=syx,
                            square_root_of_lambda=syx,
                            sample_size=length(y),
                            print_citation_info=FALSE,
                            slope_hypothesis=1,
                            checkArg=FALSE)
  expect_equal(round(d2$slope_cartesian,10), round(-syx,10))
  expect_equal(round(d2$slope_polar,8), round(-40.9852521723021,8))
  expect_equal(round(d2$clockwise_CI_cartesian,10), round(-2.6850273517965,10))
  expect_equal(round(d2$clockwise_CI_polar,5), round(-69.5728737250825,5))
  expect_equal(round(d2$counterclockwise_CI_cartesian,15), round(-0.28114206899172,15))
  expect_equal(round(d2$counterclockwise_CI_polar,5), round(-15.7029069827168,5))
  expect_equal(round(d2$CI_polar_width,10), round(53.8699667423657,10))
  expect_true(is.numeric(d2$x_reliability_ratio))
  expect_equal(d2$slope_test_p_value,round(0.726691816927342,15))

  # test lambda that isn't near 0,1, sd(y)/sd(x) near Inf
  e1<-two_variable_exact_CI(correlation=cxy,
                            sdOFy_divided_by_sdOFx=syx,
                            square_root_of_lambda=1/2,
                            sample_size=length(y),
                            print_citation_info=FALSE,
                            slope_hypothesis=1/(2*syx),
                            checkArg=TRUE)
  expect_equal(round(e1$slope_cartesian,15), round(1.51339038282243,15))
  expect_equal(round(e1$slope_polar,10), round(56.5445465586807,10))
  expect_equal(round(e1$clockwise_CI_cartesian,15), round(0.77831501717598,15))
  expect_equal(round(e1$clockwise_CI_polar,5), round(37.8941579317994,5))
  expect_equal(round(e1$counterclockwise_CI_cartesian,15), round(7.43324226404915,15))
  expect_equal(round(e1$counterclockwise_CI_polar,5), round(82.3379560888292,5))
  expect_equal(round(e1$CI_polar_width,10), round(44.4437981570298,10))
  expect_true(is.numeric(e1$x_reliability_ratio))
  expect_equal(e1$slope_test_p_value,round(0.00533044302134256,15))

})

test_that("two_variable_exact_CI prints citation info when print_citation_info=TRUE", {

  # Ensure global citation printing is on, and restore it after test exits
  change_citation_printing_default(TRUE)
  on.exit(change_citation_printing_default(TRUE))

  set.seed(0); n <- 10; x1 <- rnorm(n); y1 <- rnorm(n)

  # Check that key components of the citation output are actually printed
  expect_output(
    two_variable_exact_CI(
      correlation            = cor(x1, y1),
      sdOFy_divided_by_sdOFx = sd(y1) / sd(x1),
      square_root_of_lambda  = 1,
      sample_size            = n,
      print_citation_info    = TRUE
    ),
    regexp = "Rayner"  # from citationStrings[["Rayner85"]]
  )

  expect_output(
    two_variable_exact_CI(
      correlation            = cor(x1, y1),
      sdOFy_divided_by_sdOFx = sd(y1) / sd(x1),
      square_root_of_lambda  = 1,
      sample_size            = n,
      print_citation_info    = TRUE
    ),
    regexp = "change_citation_printing_default"
  )

  expect_output(
    two_variable_exact_CI(
      correlation            = cor(x1, y1),
      sdOFy_divided_by_sdOFx = sd(y1) / sd(x1),
      square_root_of_lambda  = 1,
      sample_size            = n,
      print_citation_info    = TRUE
    ),
    regexp = "R Software"
  )

  # Also verify that change_citation_printing_default(FALSE) suppresses output
  # even when print_citation_info=TRUE, since internal_print_citations
  # gates on the global flag
  change_citation_printing_default(FALSE)
  expect_output(
    two_variable_exact_CI(
      correlation            = cor(x1, y1),
      sdOFy_divided_by_sdOFx = sd(y1) / sd(x1),
      square_root_of_lambda  = 1,
      sample_size            = n,
      print_citation_info    = TRUE
    ),
    regexp = NA  # asserts NO output is produced
  )
})

test_that("two_variable_exact_CI", {
 expect_equal(2,2)
})
