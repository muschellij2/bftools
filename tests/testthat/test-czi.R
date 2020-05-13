testthat::context("Downloading CZI")
# Link from
# https://forum.image.sc/t/opening-large-czi-files-tiled-tiff-files/8648
# or
# https://forum.image.sc/t/fiji-bioformat-importer-does-not-automatically-adjust-physical-size-for-different-resolutions/5655/4
# url = 'https://cloud.mpi-cbg.de/index.php/s/5ssfA6v2F5OMhk8/download'

options(digits = 10)
# from https://figshare.com/articles/Fluorescence_microscopy_of_Chlamydomonas_reinhardtii_for_mCherry_detection_secretion_peptides_strains_/9114647/1
url = "https://ndownloader.figshare.com/files/16618418"
destfile = file.path(tempdir(), "pJP28mCherry.czi")
if (!file.exists(destfile)) {
  download.file(url, destfile = destfile)
}
testthat::test_that("showinf", {
  showinf.help()

  res = showinf(destfile, range = c(0, 5), crop = "0,0,10,10",
                autoscale = TRUE, debug = TRUE)
  testthat::expect_equal(attr(res, "result") , 0)

  res = showinf(destfile, range = c(0, 5), crop = c(0, 0, 10, 10))
  testthat::expect_equal(attr(res, "result") , 0)
  testthat::expect_is(res, "showinf_result")

  res = showinf(destfile)
  testthat::expect_is(res, "showinf_result")
  showinf_version()
  out = bf_parse_show_info(res)
  testthat::expect_named(out, c("other", "core", "global", ""))
  testthat::expect_equal(as.numeric(out$core$series_number), 0)

  out = bf_parse_show_info(destfile)

  res = showinf(destfile, ome_xml = TRUE)
  doc = xml2::read_xml(as.character(res))
  doc = xml2::xml_ns_strip(doc)
  testthat::expect_is(doc, c("xml_document"))
  testthat::expect_length(xml2::xml_find_all(doc, xpath = "//Image"), 1)



  meta = bf_meta_subset(destfile)
  testthat::expect_named(meta, c("image_info", "instrument", "channel"))
  testthat::expect_equal(meta$channel$ID, "Channel:0:0")

})


testthat::test_that("bfconvert_version", {
  bfconvert_version()

  res = bfconvert(destfile, bigtiff = TRUE, range = c(0, 5),
                  compression = "LZW", lookup = FALSE)
  testthat::expect_equal( tools::file_ext(res), "tiff")
  if (requireNamespace("tiff", quietly = TRUE)) {
    tif = tiff::readTIFF(res, as.is = TRUE)
    testthat::expect_equal(dim(tif), c(512L, 512L))
    testthat::expect_equal(sum(tif), 272451L)
    testthat::expect_equal(mean(tif), 1.0393180847168)
  }

  res = bfconvert(destfile, crop = c(0, 0, 10, 10))
  testthat::expect_error({
    bfconvert(destfile, outfile = as.character(res), overwrite = FALSE)
  })
  testthat::expect_equal( tools::file_ext(res), "tiff")
  if (requireNamespace("tiff", quietly = TRUE)) {
    tif = tiff::readTIFF(res, as.is = TRUE)
    testthat::expect_equal(dim(tif), c(10L, 10L, 3L))
    testthat::expect_equal(sum(tif), 768L)
  }

})
