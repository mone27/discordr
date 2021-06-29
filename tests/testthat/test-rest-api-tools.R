test_that("to_unbox single el", {
  expect_equal(
    lapply(list(a=1), to_unboxed),
    list(a=unbox(1)))
})

test_that("to_unbox multiple el el", {
  expect_equal(
    lapply(list(a=list(b=1, c=2)), to_unboxed),
    list(a=list(b=unbox(1), c=unbox(2))))
})

test_that("to_unbox deep el", {
  expect_equal(
    lapply(list(a=list(b=1, c=list(d=2, e=3))), to_unboxed),
    list(a=list(b=unbox(1), c=list(d=unbox(2), e=unbox(3)))))
})


test_that("new embed", {
  embed <- list(title=unbox("Hello"), type=unbox("rich"), description=unbox("World"))
  class(embed) <- c("discordr_embed", "list")
  expect_equal(
    new_embed(title="Hello", description="World"),
    embed
  )
})





