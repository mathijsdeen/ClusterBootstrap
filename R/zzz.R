# avoid the "no visible bindings for global variable" NOTE on R CMD check for data.table's non-standard evaluation paradigm issues
utils::globalVariables(c(".", ".N", ":=", ".I"))