library(testthat)
library(lab3package)
data("wiki_graph")

# Euclidean
test_that("euclidean finds greatest common divisor between two integers", {
  expect_equal(euclidean(4,2), 2)
  expect_equal(euclidean(20,3), 1)
  expect_equal(euclidean(93164,-5826), 2)
})

test_that("euclidean of characters throws an error", {
  expect_error(euclidean(2,"a"), "non-numeric argument to binary operator")
  expect_error(euclidean("A",3), "non-numeric argument to binary operator")
  expect_error(euclidean("a","+"), "non-numeric argument to binary operator")
})

test_that("euclidean of decimal numbers throws an error", {
  expect_error(euclidean(4,2.1), "a%%1 == 0 & b%%1 == 0 is not TRUE")
  expect_error(euclidean(20.5,3), "a%%1 == 0 & b%%1 == 0 is not TRUE")
  expect_error(euclidean(20.55,3.44), "a%%1 == 0 & b%%1 == 0 is not TRUE")
})

test_that("euclidean of vectors throws an error", {
  expect_error(euclidean(1:5, 3), "length(a) == 1 is not TRUE", fixed = TRUE)
  expect_error(euclidean(c("a","b"), 2), "non-numeric argument to binary operator")
  expect_error(euclidean(1:4, c("a","b")), "non-numeric argument to binary operator")
})

# Dijkstra
test_that("Dijkstra finds the shortest path",{
  expect_equal(dijkstra(wiki_graph, 1), c(0,7,9,20,20,11))
  expect_equal(dijkstra(wiki_graph, 2), c(7,0,10,15,21,12))
})
test_that("graph is not dataframe",{
  expect_error(dijkstra(1, 1), "is.data.frame(graph) is not TRUE", fixed=TRUE)
  expect_error(dijkstra("a", 1), "is.data.frame(graph) is not TRUE", fixed=TRUE)
})

test_that("init_node is not a numeric scalar",{
  expect_error(dijkstra(wiki_graph, "a"), "is.numeric(init_node) is not TRUE", fixed=TRUE)
  expect_error(dijkstra(wiki_graph, list("a", "b")), "is.numeric(init_node) is not TRUE", fixed=TRUE)
})

test_that("graph is a data frame with the columns (v1, v2, w)",{
  expect_error(dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                                   er=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                                   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 1), "colnames(graph) == c(\"v1\", \"v2\", \"w\") are not all TRUE", fixed=TRUE)
})


