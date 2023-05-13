#' @importFrom dplyr group_by_drop_default
#' @importFrom tidyselect everything
#' @importFrom lifecycle deprecated

#' @importFrom dplyr add_count
#' @export
add_count.edsurvey.data.frame <- function (x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated()){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr anti_join
#' @export
anti_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr arrange
#' @export
arrange.edsurvey.data.frame <- function (.data, ..., .by_group = FALSE, .locale = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr arrange_
#' @export
arrange_.edsurvey.data.frame <- function (.data, ..., .dots = list(), .by_group = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr as.tbl
#' @export
as.tbl.edsurvey.data.frame <- function (x, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr auto_copy
#' @export
auto_copy.edsurvey.data.frame <- function (x, y, copy = FALSE, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr collapse
#' @export
collapse.edsurvey.data.frame <- function (x, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr collect
#' @export
collect.edsurvey.data.frame <- function (x, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr compute
#' @export
compute.edsurvey.data.frame <- function (x, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr count
#' @export
count.edsurvey.data.frame <- function (x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = group_by_drop_default(x)){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr cross_join
#' @export
cross_join.edsurvey.data.frame <- function (x, y, ..., copy = FALSE, suffix = c(".x", ".y")){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr distinct
#' @export
distinct.edsurvey.data.frame <- function (.data, ..., .keep_all = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr distinct_
#' @export
distinct_.edsurvey.data.frame <- function (.data, ..., .dots = list(), .keep_all = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr do
#' @export
do.edsurvey.data.frame <- function (.data, ...){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr do_
#' @export
do_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.edsurvey.data.frame <- function (data, cols){
  .Class <- "data.frame"
  data <- buildDF(data)
  NextMethod()
}


#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.edsurvey.data.frame <- function (data, template){
  .Class <- "data.frame"
  data <- buildDF(data)
  NextMethod()
}


#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.edsurvey.data.frame <- function (data, i, ...){
  .Class <- "data.frame"
  data <- buildDF(data)
  NextMethod()
}


#' @importFrom dplyr filter
#' @export
filter.edsurvey.data.frame <- function (.data, ..., .by = NULL, .preserve = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr filter_
#' @export
filter_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr full_join
#' @export
full_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na",     "never"), multiple = "all", relationship = NULL){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr group_by
#' @export
group_by.edsurvey.data.frame <- function (.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_by_
#' @export
group_by_.edsurvey.data.frame <- function (.data, ..., .dots = list(), add = FALSE, .drop = group_by_drop_default(.data)){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_data
#' @export
group_data.edsurvey.data.frame <- function (.data){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_indices
#' @export
group_indices.edsurvey.data.frame <- function (.data, ...){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_indices_
#' @export
group_indices_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_keys
#' @export
group_keys.edsurvey.data.frame <- function (.tbl, ...){
  .Class <- "data.frame"
  .tbl <- buildDF(.tbl)
  NextMethod()
}


#' @importFrom dplyr group_map
#' @export
group_map.edsurvey.data.frame <- function (.data, .f, ..., .keep = FALSE, keep = deprecated()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_modify
#' @export
group_modify.edsurvey.data.frame <- function (.data, .f, ..., .keep = FALSE, keep = deprecated()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr group_nest
#' @export
group_nest.edsurvey.data.frame <- function (.tbl, ..., .key = "data", keep = FALSE){
  .Class <- "data.frame"
  .tbl <- buildDF(.tbl)
  NextMethod()
}


#' @importFrom dplyr group_size
#' @export
group_size.edsurvey.data.frame <- function (x){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr group_split
#' @export
group_split.edsurvey.data.frame <- function (.tbl, ..., .keep = TRUE, keep = deprecated()){
  .Class <- "data.frame"
  .tbl <- buildDF(.tbl)
  NextMethod()
}


#' @importFrom dplyr group_trim
#' @export
group_trim.edsurvey.data.frame <- function (.tbl, .drop = group_by_drop_default(.tbl)){
  .Class <- "data.frame"
  .tbl <- buildDF(.tbl)
  NextMethod()
}


#' @importFrom dplyr group_vars
#' @export
group_vars.edsurvey.data.frame <- function (x){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr groups
#' @export
groups.edsurvey.data.frame <- function (x){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr inner_join
#' @export
inner_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na",     "never"), multiple = "all", unmatched = "drop", relationship = NULL){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr left_join
#' @export
left_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na",     "never"), multiple = "all", unmatched = "drop", relationship = NULL){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr mutate
#' @method mutate edsurvey.data.frame
#' @export
mutate.edsurvey.data.frame <- function (.data, ..., .by = NULL, .keep = c("all", "used", "unused", "none"), .before = NULL,     .after = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr mutate_
#' @export
mutate_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr n_groups
#' @export
n_groups.edsurvey.data.frame <- function (x){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr nest_by
#' @export
nest_by.edsurvey.data.frame <- function (.data, ..., .key = "data", .keep = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr nest_join
#' @export
nest_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, keep = NULL, name = NULL, ..., na_matches = c("na",     "never"), unmatched = "drop"){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr pull
#' @export
pull.edsurvey.data.frame <- function (.data, var = -1, name = NULL, ...){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr reframe
#' @export
reframe.edsurvey.data.frame <- function (.data, ..., .by = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr relocate
#' @export
relocate.edsurvey.data.frame <- function (.data, ..., .before = NULL, .after = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}

#' @importFrom dplyr rename_with
#' @export
rename_with.edsurvey.data.frame <- function (.data, .fn, .cols = everything(), ...){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr right_join
#' @export
right_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na",     "never"), multiple = "all", unmatched = "drop", relationship = NULL){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rows_append
#' @export
rows_append.edsurvey.data.frame <- function (x, y, ..., copy = FALSE, in_place = FALSE){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rows_delete
#' @export
rows_delete.edsurvey.data.frame <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rows_insert
#' @export
rows_insert.edsurvey.data.frame <- function (x, y, by = NULL, ..., conflict = c("error", "ignore"), copy = FALSE, in_place = FALSE){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rows_patch
#' @export
rows_patch.edsurvey.data.frame <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rows_update
#' @export
rows_update.edsurvey.data.frame <- function (x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rows_upsert
#' @export
rows_upsert.edsurvey.data.frame <- function (x, y, by = NULL, ..., copy = FALSE, in_place = FALSE){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr rowwise
#' @export
rowwise.edsurvey.data.frame <- function (data, ...){
  .Class <- "data.frame"
  data <- buildDF(data)
  NextMethod()
}


#' @importFrom dplyr same_src
#' @export
same_src.edsurvey.data.frame <- function (x, y){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr sample_frac
#' @export
sample_frac.edsurvey.data.frame <- function (tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL, ...){
  .Class <- "data.frame"
  tbl <- buildDF(tbl)
  NextMethod()
}


#' @importFrom dplyr sample_n
#' @export
sample_n.edsurvey.data.frame <- function (tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...){
  .Class <- "data.frame"
  tbl <- buildDF(tbl)
  NextMethod()
}


#' @importFrom dplyr select
#' @export
select.edsurvey.data.frame <- function (.data, ...){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr select_
#' @export
select_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr semi_join
#' @export
semi_join.edsurvey.data.frame <- function (x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr slice
#' @export
slice.edsurvey.data.frame <- function (.data, ..., .by = NULL, .preserve = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr slice_
#' @export
slice_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr slice_head
#' @export
slice_head.edsurvey.data.frame <- function (.data, ..., n, prop, by = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr slice_max
#' @export
slice_max.edsurvey.data.frame <- function (.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr slice_min
#' @export
slice_min.edsurvey.data.frame <- function (.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr slice_sample
#' @export
slice_sample.edsurvey.data.frame <- function (.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr slice_tail
#' @export
slice_tail.edsurvey.data.frame <- function (.data, ..., n, prop, by = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr summarise
#' @export
summarise.edsurvey.data.frame <- function (.data, ..., .by = NULL, .groups = NULL){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr summarise_
#' @export
summarise_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr symdiff
#' @export
symdiff.edsurvey.data.frame <- function (x, y, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr tally
#' @export
tally.edsurvey.data.frame <- function (x, wt = NULL, sort = FALSE, name = NULL){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.edsurvey.data.frame <- function (x){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr transmute
#' @export
transmute.edsurvey.data.frame <- function (.data, ...){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr transmute_
#' @export
transmute_.edsurvey.data.frame <- function (.data, ..., .dots = list()){
  .Class <- "data.frame"
  .data <- buildDF(.data)
  NextMethod()
}


#' @importFrom dplyr ungroup
#' @export
ungroup.edsurvey.data.frame <- function (x, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


#' @importFrom dplyr union_all
#' @export
union_all.edsurvey.data.frame <- function (x, y, ...){
  .Class <- "data.frame"
  x <- buildDF(x)
  NextMethod()
}


