#' Calculate Entropy
#'
#' This is a description
#'
#' @param counts A matrix of count data
#'
#' @return A vector of entropies
#' @export
#'
#' @examples
#'
#' mat <- matrix(1:12, ncol = 3)
#' calc_entropy(mat)
#'
calc_entropy <- function(counts) {

  if (missing(counts)) {
    stop("\n \u2716 Counts matrix has not been supplied")
  }

  norm_counts <- apply(
    counts,
    2,
    \(x){x/sum(x)}
  )

  gene_entr <- apply(
    norm_counts,
    2,
    \(x){entropy::entropy(x)}
  )

  gene_entr

}

#' Select Genes By Entropy
#'
#' This is a. escription
#'
#' @param counts counts matrix
#' @param threshold Threshold
#'
#' @return Vector of truth values
#' @export
#'
#' @examples
#'
#' mat <- matrix(1:12, ncol = 3)
#' select_entropy(mat, 1.3)
#'
select_entropy <- function(counts, threshold) {

  gene_entr <- calc_entropy(counts)

  gene_entr < threshold

}


