bf_parse_show_info = function(file) {
  if (length(file) == 1 & file.exists(file)
      & !inherits(file, "showinf_result")) {
    file = showinf(file)
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

  y = data$global
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

}

