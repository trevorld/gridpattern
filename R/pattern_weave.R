#' Weave pattern matrix
#'
#' `pattern_weave()` returns a logical matrix indicating where the warp lines should
#'  be "up" for a specified weave pattern type and subtype.
#' `names_weave` is a character vector listing supported weave pattern types.
#'
#' Here is a list of the various weave `type`s supported:
#'
#' \describe{
#' \item{basket}{A simple criss-cross pattern using two threads at a time.
#'              Same as the "matt_irregular" weave but with a default `subtype` of `2L`.}
#' \item{matt}{A simple criss-cross pattern using 3 (or more) threads at a time.
#'             Same as the "matt_irregular" weave but with a default `subtype` of `3L`.}
#' \item{matt_irregular}{A generalization of the "plain" weave.
#'                       A character `subtype` `"U/D(L+R)"` is a standard matt weave specification:
#'                       `U` indicates number warp up, `D` indicates number warp down,
#'                       `L` indicates number of warp up in repeat, and
#'                       `R` indicates number of warp down in repeat.
#'                       An integer `subtype` `N` will be interpreted as a `"N/N(N+N)"` irregular matt weave.
#'                       A character `subtype` `"U/D"` will be interpreted as a `"U/D(U+D)"` irregular matt weave.
#'                       Has a default `subtype` of `"3/2(4+2)"`.}
#' \item{plain}{A simple criss-cross pattern.
#'              Same as the "matt_irregular" weave but with a default `subtype` of `1L`.}
#' \item{rib_warp}{A plain weave variation that emphasizes vertical lines.
#'                 An integer `subtype` `N` will be interpreted as a "matt_irregular" `"N/N(1+1)"` weave.
#'                 A character `subtype` `"U/D"` will be interpreted as a "matt_irregular" `"U/D(1+1)"` weave.
#'                 Default `subtype` of `2L`.}
#' \item{satin}{A "regular" satin weave is a special type of the elongated twill weave
#'              with a move number carefully chosen so no twill line is distinguishable.
#'              Same as the "twill_elongated" weave but with a default `subtype` of `5L`.}
#' \item{twill}{A simple diagonal pattern.
#'              Same as the "twill_elongated" weave but with a default `subtype` of `"2/1"`.}
#' \item{twill_elongated}{A generalization of the "twill" weave.
#'                        A character `subtype` `"U/D(M)"` is a standard twill weave specification:
#'                        `U` indicates number warp up, `D` indicates number warp down,
#'                        and `M` indicates the "move" number.
#'                        A character `subtype` `"U/D"` will be interpreted as a `"U/D(1)"` elongated twill weave.
#'                        An integer `subtype` `N` will provide a `"{N-1}/1(1)"` elongated twill weave
#'                        if `N` is less than 5, 6, or greater than 14 otherwise it will
#'                        provide a `"{N-1}/1(M)"` weave where `M` is the largest
#'                        possible regular "satin" move number.
#'                        Default `subtype` of `"4/3(2)"`.}
#' \item{twill_herringbone}{Adds a (vertical) "herringbone" effect to
#'                          the specified "twill_elongated" weave.
#'                          Default `subtype` of `"4/3(2)"`.}
#' \item{twill_zigzag}{Adds a (vertical) "zig-zag" effect to the specified "twill_elongated" weave.
#'                     Default `subtype` of `"4/3(2)"`.}
#' }
#' For both "matt" and "twill" weaves the `U/D` part of the subtype can be further extended
#' to `U1/D1*U2/D2`, `U1/D1*U2/D2*U3/D3`, etc.
#' For the "matt" weave the "(L+R)" part of the subtype can be further extended
#' to `(L1+R1+L2+R2)`, `(L1+R1+L2+R2+L3+R3)`, etc.
#'
#' @param type Type of weave.  See Details.
#' @param subtype Subtype of weave.  See Details.
#' @param nrow Number of rows (length of warp).
#' @param ncol Number of columns (length of weft).
#' @return A matrix of logical values indicating where the "warp"
#'         is "up" (if `TRUE`) or "down" (if `FALSE`).
#'         Indices `[1,1]` of the matrix corresponds to the bottom-left of the weave
#'         while indices `[1,ncol]` corresponds to the bottom-right of the weave.
#'         This matrix has a "pattern_weave" subclass which supports a special `print()` method.
#' @examples
#'  # supported weave names
#'  print(names_weave)
#'
#'  plain <- pattern_weave("plain", nrow = 7, ncol = 9)
#'  print(plain)
#'
#'  matt_irregular <- pattern_weave("matt_irregular", nrow = 9, ncol = 11)
#'  print(matt_irregular)
#'
#'  satin <- pattern_weave("satin", nrow = 9, ncol = 11)
#'  print(satin)
#'
#'  twill <- pattern_weave("twill", nrow = 9, ncol = 11)
#'  print(twill)
#'
#'  twill_zigzag <- pattern_weave("twill_zigzag", nrow = 18, ncol = 11)
#'  print(twill_zigzag)
#'
#' @seealso [grid.pattern_weave()] for drawing weaves onto a graphics device.
#'          See \url{https://textilestudycenter.com/derivatives-of-plain-weave/}
#'          for further information on the "matt" family of weaves,
#'          \url{https://textilelearner.net/twill-weave-features-classification-derivatives-and-uses/}
#'          for further information on the "twill" family of weaves, and
#'          \url{https://texwiz101.blogspot.com/2012/03/features-and-classification-of-satin.html}
#'          for further information on "satin" weaves.
#' @export
pattern_weave <- function(type = "plain", subtype = NULL, nrow = 5L, ncol = 5L) {
    spec <- get_weave_spec(type, subtype)

    if (is_twill(type))
        m <- pattern_weave_twill(spec, nrow, ncol)
    else
        m <- pattern_weave_matt(spec, nrow, ncol)
    class(m) <- c("pattern_weave", "matrix", "array")
    m
}

# rep_each(1:2, 1:4) -> c(1, 2,2, 1,1,1, 2,2,2,2)
rep_each <- function(x, each) {
    n <- max(lengths(list(x, each)))
    x <- rep_len(x, n)
    each <- rep_len(each, n)
    unlist(lapply(seq.int(n), function(i) rep_len(x[i], each[i])))
}

pattern_weave_matt <- function(spec, nrow, ncol) {
    m <- matrix(FALSE, nrow = nrow, ncol = ncol)
    v <- rep_each(c(TRUE, FALSE), spec$up_down)
    v <- rep_len(v, nrow)
    not_v <- !v
    repeat_up_down <- rep_each(c(TRUE, FALSE), spec$up_down_reps)
    repeat_up_down <- rep_len(repeat_up_down, ncol)
    for (j in seq.int(ncol)) {
        if (repeat_up_down[j])
            m[, j] <- v
        else
            m[, j] <- not_v
    }
    m
}

pattern_weave_twill <- function(spec, nrow, ncol) {
    m <- matrix(FALSE, nrow = nrow, ncol = ncol)
    skip <- 0L
    v0 <- rep_each(c(TRUE, FALSE), spec$up_down)
    for (j in seq_len(ncol)) {
        v <- cycle_elements(v0, -skip)
        if (spec$zigzag)
            v <- add_zigzag(v)
        if (spec$herringbone)
            v <- add_herringbone(v)
        v <- rep_len(v, nrow)
        m[, j] <- v
        skip <- skip + spec$move
    }
    m
}

#' @export
print.pattern_weave <- function(x, ...) {
    indices_x <- which(x)
    indices_o <- which(!x)
    x[indices_x] <- "X"
    x[indices_o] <- " "
    cat("/", rep_len("-", ncol(x)), "\\", "\n")
    for (i in rev(seq_len(nrow(x)))) {
        cat("|", x[i, ], "|", "\n")
    }
    cat("\\", rep_len("-", ncol(x)), "/", "\n")
    invisible(NULL)
}

#' @rdname pattern_weave
#' @export
names_weave <- c("basket",
                 "matt",
                 "matt_irregular",
                 "plain",
                 "rib_warp",
                 "satin",
                 "twill",
                 "twill_elongated",
                 "twill_herringbone",
                 "twill_zigzag")

get_weave_spec <- function(type = "plain", subtype = NULL) {
    if (!is.null(subtype) && is.na(subtype)) subtype <- NULL
    switch(type,
           # cases of irregular matt weave
           basket = get_weave_spec_matt(subtype %||% 2L),
           plain = get_weave_spec_matt(subtype %||% 1L),
           matt = get_weave_spec_matt(subtype %||% 3L),
           matt_irregular = get_weave_spec_matt(subtype %||% "3/2(4+2)"),
           rib_warp = get_weave_spec_matt(subtype %||% 2L,
                                          warp = TRUE),

           # cases of elongated twill weave
           satin = get_weave_spec_twill(subtype %||% 5L),
           twill = get_weave_spec_twill(subtype %||% "2/1"),
           twill_elongated = get_weave_spec_twill(subtype %||% "4/3(2)"),
           twill_zigzag = get_weave_spec_twill(subtype %||% "4/3(2)",
                                               zigzag = TRUE),
           twill_herringbone = get_weave_spec_twill(subtype %||% "4/3(2)",
                                                    herringbone = TRUE),

           abort(paste("Don't know weave type", type)))
}

is_twill <- function(type) grepl("^twill|^satin", type)

# elongated twill U/D(M)
# U = number warp up, D = number warp down, M = move number
get_weave_spec_twill <- function(subtype = "2/1(2)", zigzag = FALSE, herringbone = FALSE) {
    if (is_integer(subtype))
        subtype <- n_to_twill(subtype)
    if (is_ud(subtype))
        subtype <- ud_to_twill(subtype)

    up_down <- get_ud(subtype)
    move <- get_extra(subtype)
    list(up_down = up_down, move = move, herringbone = herringbone, zigzag = zigzag)
}

# irregular matt U/D(L+R)
# U = number warp up, D = number warp down,
# L = number of warp up in repeat,  R = number of warp down in repeat
get_weave_spec_matt <- function(subtype = "3/3(4+2)", warp = FALSE) {
    if (is_integer(subtype))
        subtype <- n_to_matt(subtype, warp = warp)
    if (is_ud(subtype))
        subtype <- ud_to_matt(subtype, warp = warp)

    up_down <- get_ud(subtype)
    reps <- get_extra(subtype)
    list(up_down = up_down, up_down_reps = reps)
}

n_to_matt <- function(n = 1L, warp = FALSE) {
    n <- as.integer(n)
    if (warp)
        glue("{n}/{n}(1+1)")
    else
        glue("{n}/{n}({n}+{n})")
}
ud_to_matt <- function(ud = "2/1", warp = FALSE) {
    v_ud <- get_ud(ud)
    if (warp)
        glue("{ud}(1+1)")
    else
        glue("{ud}({paste(v_ud, collapse='+')})")
}

# satin is special case of twill elongated
# legal satin move is not one, repeat number, repeat number minus one, (multiple of) a factor of repeat number
# legal: 5:2 | 7:2,3 | 8:3 | 9:2,4 | 10:3 | 11:2,3,4,5 | 12:5 | 13: 2,3,4,5,6 | 14:3,5
n_to_twill <- function(n = 1L) {
    n <- as.integer(n)
    if (n < 5L) {
        move <- 1L
    } else {
        move <- switch(as.character(n),
                       "5" = 2L,
                       "6" = 1L, # no legal satin move
                       "7" = 3L,
                       "8" = 3L,
                       "9" = 4L,
                       "10" = 3L,
                       "11" = 5L,
                       "12" = 5L,
                       "13" = 6L,
                       "14" = 5L,
                       n - 1L)
    }
    glue("{n-1}/1({move})")
}

ud_to_twill <- function(ud = "2/1") {
    glue("{ud}(1)")
}

is_integer <- function(s) is.integer(s) || grepl("^[[:digit:]]+$", s)

# "5/1" -> TRUE ; "5" -> FALSE ; "5/1(3)" -> FALSE ; "5/1(4+2)" -> FALSE
is_ud <- function(ud) grepl("^[[:digit:]]+/[[:digit:]]+(\\*[[:digit:]]+/[[:digit:]]+)*$", ud)

# "5/1(4+2)" -> c(5L, 1L) or "5/1(3)" -> c(5L, 1L)
get_ud <- function(ude) {
    ud <- as.integer(strsplit(gsub("\\(.*", "", ude), "/|\\*")[[1]])
    stopifnot(length(ud) %% 2L == 0L)
    ud
}

# "5/1(4+2)" -> c(4L, 2L) or "5/1(3)" -> c(3L)
get_extra <- function(ude) as.integer(strsplit(gsub("[[:digit:]/*]+\\((.*)\\)", "\\1", ude), "\\+")[[1]])

add_zigzag <- function(x) {
    n <- length(x)
    c(x, rev(x[-n]), x[n])
}

add_herringbone <- function(x) c(x, !rev(x))
