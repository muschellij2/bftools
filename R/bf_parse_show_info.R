process_core_data = function(data) {
  info = item = number = series = series_number = value = x = NULL
  rm(list = c("info", "item", "number", "series",
              "series_number", "value", "x"))

  df = data.frame(x = data$core, stringsAsFactors = FALSE)
  df = df %>%
    mutate(series = grepl("Series [#]", df$x),
           series_number = sub("Series [#](.*) :", "\\1", x),
           series_number = ifelse(series, series_number, NA),
           series_number = zoo::na.locf(series_number, na.rm = FALSE),
           x = sub("<=>", " = ", x, fixed = TRUE),
           x = trimws(x))
  df = df %>%
    filter(!series) %>%
    select(-series) %>%
    filter(x != "",
           x != "-----",
           x != "----")

  df = df %>%
    separate(x, sep = " = ", into = c("item", "value"))
  fname = df %>%
    filter(item == "filename")
  count = df %>%
    filter(item == "Series count")
  df = df %>%
    filter(item != "filename",
           item != "Series count")
  df=  df %>%
    mutate(item = tolower(item),
           item = gsub(" ", "_", item))
  df = df %>%
    spread(item, value)
  df
}


#' Parse output from \code{\link{showinf}}
#'
#' @param file Output from \code{\link{showinf}},
#' a filename
#'
#' @return A list of output
#' @export
#'
#' @examples
#' file = "~/Downloads/2017_08_03__0006.czi"
#' if (file.exists(file)) {
#' res = showinf(file = file)
#' parsed = bf_parse_show_info(res)
#' res = showinf(file = file, debug = TRUE)
#' parsed2 = bf_parse_show_info(res)
#' testthat::expect_equal(parsed2$core, parsed$core)
#' testthat::expect_equal(parsed2$`series #0`, parsed$`series #0`)
#' }
#' @importFrom zoo na.locf
#' @importFrom dplyr mutate filter select %>%
#' @importFrom tidyr separate spread gather
bf_parse_show_info = function(file) {
  info = item = number = series = series_number = value = x = NULL
  rm(list = c("info", "item", "number", "series",
              "series_number", "value", "x"))

  if (length(file) == 1 & file.exists(file)
      & !inherits(file, "showinf_result")) {
    tryout = showinf(file)
    result = attributes(tryout)$result
    if (result == 0) {
      file = tryout
    }
  }
  x = readLines(file)

  readers = grep("Reading", x)
  vals = sub("Reading ", "", x[readers])
  vals = trimws(sub("metadata", "", vals))
  if (readers[1] > 1) {
    vals = c("other", vals)
  }

  readers = cbind(c(1, readers + 1),
                  c(readers - 1, length(x)))
  inds = apply(readers, 1, function(r) {
    seq(r[1], r[2])
  })

  data = lapply(inds, function(r) {
    x[r]
  })
  names(data) = vals

  # y = data$global
  split_global_data = function(y) {
    y = y[ y != "" ]
    ss = strsplit(y, split = "|", fixed = TRUE)
    types = sapply(ss, `[`, 1)
    stopifnot(!any(is.na(types)))
    utypes = sort(unique(types))
    for (itype in utypes) {
      yy = ss[ types == itype]
    }
  }

  data$core = process_core_data(data)
  series = grepl("series", names(data) )
  if (any(series)) {
    y = data[series][[1]]
    out_series = lapply(data[series], function(y) {
      y = y[ y != "" ]
      y = y[ !grepl("^Location.mapFile", y)]
      ss = strsplit(y, split = "|", fixed = TRUE)
      ss = sapply(ss, trimws)
      if (is.matrix(ss)) {
        ss = t(ss)
        if (ncol(ss) == 3) {
          colnames(ss) = c("type", "series", "info")
          ss = as.data.frame(ss, stringsAsFactors = FALSE)
          ss = ss %>% separate(info, into = c("number", "value"),
                               sep = ":") %>%
            mutate(number = sub("#", "", number, fixed = TRUE),
                   value = trimws(value),
                   number = trimws(value),
                   series = sub("Series", "", series),
                   series = trimws(series))
        }
      }
      return(ss)
    })
    data[series] = out_series
  }
  return(data)
}

