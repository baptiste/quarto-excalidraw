
library(purrr)

# system("pandoc --lua-filter ./filter.lua extract.tex -o math.tex")
# system("de-macro 01-maxwell")
#
#
# fe <- 'maxwell'
#
# parse <- readLines(glue("{fe}-math.tex"))
# content <- parse[parse != '']
# lines <- grep("math start", content)

# system(glue("ltximg --latex --imgdir eqs --svg --png -o {fe}-out {fe}.tex "))

svg_sizing <- function(fe){

  # tmp <- tempfile(fileext = '.tex')
  # parse <- readLines(glue("{fe}-math.tex"))
  # cat(r"(\documentclass{article}
  #     \usepackage{amsmath}
  #     \usepackage[active,tightpage]{preview}\begin{document})", file = tmp, append = FALSE)
  # cat(paste(parse, collapse = "\n"), file = tmp, append = TRUE)
  # cat(r"(\end{document})", file = tmp, append = TRUE)
  # tinytex::xelatex(tmp)
  # grab images
  # system(glue("ltximg --latex --imgdir eqs --svg --png -o {fe}-out {fe}.tex "))
  system(glue("pdfcrop {fe}.pdf "))
  fs::dir_create(glue("{fe}"))
  system(glue("pdf2svg {fe}-crop.pdf {fe}/%3d.svg all"))

}


grab_math_strings <- function(fe){

  parse <- readLines(glue("{fe}-math.tex"))
  content <- parse[parse != '']
  lines <- grep("math start", content)

  start <- lines+1
  end <- c(lines[-1], length(content) + 1) - 1

  library(purrr)

  eqs <- map2(start, end, function(x,y) paste(gsub("\\\\\\(|\\\\\\)|\\\\\\[|\\\\\\]", "",
                                                   content[x:y]), collapse = ' '))
  eqs <- gsub('\\\\ss',"ß",eqs)
  eqs <- gsub('è|é',"e",eqs)
  eqs <- gsub('à',"a",eqs)

  eqs
}

size_eqs <- function(fe){
  library(minisvg)
  lf <- list.files(glue("{fe}"), full.names = TRUE)
  get_size <- function(f){
    svg <- minisvg::parse_svg_doc(f)
    as.numeric(gsub("pt","",c(svg$attribs$width,  svg$attribs$height)))
  }
  map(lf, get_size)
}


extract_math <- function(fe){
  library(glue)
  # fe <- fs::path_ext_remove(f)
  # system(glue("de-macro {fe}"))
  svg_sizing(fe)
  system(glue("quarto pandoc --lua-filter ./_extract-maths.lua {fe}.tex -o {fe}-math.tex"))
  grab_math_strings(fe)
}

export_math <- function(eqs, sizes, out = 'tmp.excalidraw', hstep=0, vstep=50, fontSize=24){

  library(minixcali)

  scaling <- 1.1*fontSize / 9
  heights <- vapply(sizes, "[", 2, 2)
  vstep <- pmax(max(heights), vstep)
  vpos <- scaling*cumsum(c(0,heights))
  d <- Excali_doc()

  for (i in seq_along(eqs)) {

    equation <- xkd_math(
      x = (i - 1)*hstep,
      y = vpos[i],
      width = scaling*sizes[[i]][1],
      height = scaling*sizes[[i]][2],
      text = eqs[[i]],
      fontSize=fontSize
    )

    d$add(equation)

  }

  d$export(out)

}

process_subject <- function(fe = 'maxwell'){
  eqs <- extract_math(fe)
  sizes <-  size_eqs(fe)
  export_math(eqs, sizes, out = glue('{fe}.excalidraw'))
}

subjects <- fs::path_ext_remove(fs::dir_ls(glob = "*.qmd"))
purrr::walk(subjects, process_subject)
