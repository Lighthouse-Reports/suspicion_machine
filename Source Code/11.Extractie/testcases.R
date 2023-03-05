context("Test structuur extractietabel")

test_that("Classes van kolommen zijn niet gelijk!", {
  expect_that(lapply(df, class) , is_identical_to(lapply(dfold, class)))}
)

test_that("Aantal kolommen is niet gelijk!", {
  expect_that(ncol(df) == ncol(dfold), is_true())}
)

test_that("Namen van kolommen zijn niet gelijk!", {
  expect_that(names(df), is_identical_to(names(dfold)))}
)

test_that("aantal records niet groter dan eerdere tabel!", {
  expect_that(nrow(df) > nrow(dfold), is_true())}
)

test_that("Aantal records is  kleiner dan vorige export!", {
  expect_that(nrow(df) >= nrow(dfold), is_true())}
)

test_that("De nieuwe data bevat minder gevulde rijen in minstens een kolom dan in de oude data!", {
  expect_that(all(df %>% summarise_all(funs(sum(!is.na(.)))) >= 
                    dfold %>% summarise_all(funs(sum(!is.na(.))))), is_true())})
