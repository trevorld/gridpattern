#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create grob objects for the pattern elements within a boundary
##
## @param params params/coords for a single element. named list or single row data.frame
## @param boundary_df mask for the pattern rendering
## @param aspect_ratio a aspect ratio of the plotting area.
## @param legend is the pattern being created in the legend? default FALSE.
##  Use this flag if you want different pattern drawing behviour for the legend.
##
## @return grid grob objects.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_none <- function(params, boundary_df, aspect_ratio,
                                legend = FALSE) {
  grid::nullGrob()
}
