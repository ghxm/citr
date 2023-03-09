test_that("tidy bib file writing works (non-existing tidy file)", {

  # copy tidy_original.bib to tidy.bib
  file.copy("bib_files/tidy_original.bib", "bib_files/tidy.bib", overwrite = TRUE)

  tidy_bib_file("rmd_files/example.rmd", "bib_files/zotero_better_bibtex.bib", "bib_files/tidy2.bib", encoding = "UTF-8", betterbiblatex_format = "bibtex")

  # read in tidy2.bib
  tidy_bib <- RefManageR::ReadBib("bib_files/tidy2.bib", check = FALSE, .Encoding = "UTF-8")

  # testing conditions
  expect_true(all(c("baumer_markdown_2014", "savage_empirical_2009") %in% names(tidy_bib)) & NROW(tidy_bib) == 2)

})


test_that("tidy bib file writing works (existing tidy file, keep all)", {

  # copy tidy_original.bib to tidy.bib
  file.copy("bib_files/tidy_original.bib", "bib_files/tidy.bib", overwrite = TRUE)

  tidy_bib_file("rmd_files/example.rmd", "bib_files/zotero_better_bibtex.bib", "bib_files/tidy.bib", keep_existing = "all", encoding = "UTF-8", betterbiblatex_format = "bibtex")

  # read in tidy.bib
  tidy_bib <- RefManageR::ReadBib("bib_files/tidy.bib", check = FALSE, .Encoding = "UTF-8")

  # testing conditions
  expect_true(all(c("baumer_markdown_2014", "savage_empirical_2009", "savage_empirical_2010", "notinmessybutintidy", "notinrmd" ) %in% names(tidy_bib)) & NROW(tidy_bib) == 5)

})

test_that("tidy bib file writing works (existing tidy file, keep necessary)", {

  # copy tidy_original.bib to tidy.bib
  file.copy("bib_files/tidy_original.bib", "bib_files/tidy.bib", overwrite = TRUE)

  tidy_bib_file("rmd_files/example.rmd", "bib_files/zotero_better_bibtex.bib", "bib_files/tidy.bib", keep_existing = "necessary", encoding = "UTF-8", betterbiblatex_format = "bibtex")


  # read in tidy.bib
  tidy_bib <- RefManageR::ReadBib("bib_files/tidy.bib", check = FALSE, .Encoding = "UTF-8")

  # testing conditions
  expect_true(all(c("baumer_markdown_2014", "savage_empirical_2009", "notinmessybutintidy") %in% names(tidy_bib)) & NROW(tidy_bib) == 3)



})


test_that("tidy bib file writing works (existing tidy file, keep none)", {

  # copy tidy_original.bib to tidy.bib
  file.copy("bib_files/tidy_original.bib", "bib_files/tidy.bib", overwrite = TRUE)

  tidy_bib_file("rmd_files/example.rmd", "bib_files/zotero_better_bibtex.bib", "bib_files/tidy.bib", keep_existing = "none", encoding = "UTF-8", betterbiblatex_format = "bibtex")

    # read in tidy.bib
  tidy_bib <- RefManageR::ReadBib("bib_files/tidy.bib", check = FALSE, .Encoding = "UTF-8")

  # testing conditions
  expect_true(all(c("baumer_markdown_2014", "savage_empirical_2009") %in% names(tidy_bib)) & NROW(tidy_bib) == 2)




})


