
create_labels <- function(label_data, pars) {

  attach(pars)

  x_starts <- 0:(n_cols_labels - 1) * label_width
  y_starts <- rev(1:n_rows_labels * label_height)
  
  n_labels_per_page <- n_rows_labels * n_cols_labels
  n_pages <- ceiling(nrow(label_data) / n_labels_per_page)

  n_labels_final_page <- nrow(label_data) %% n_labels_per_page

  for (k in 1:n_pages) {

    n_labels_this_page <- n_labels_per_page
    if (k == n_pages & n_labels_final_page > 0) n_labels_this_page <- n_labels_final_page
    page_data <- label_data[k * c(1:n_labels_this_page), ]

    filename <- paste0("labels_page_", sprintf("%04d", k), ".pdf")
    pdf(filename, height = 11, width = 8.5)

    par(mar = margins * margin_scale)
    plot(1, 1, type = "n", xlim = c(0, n_cols_labels * label_width),
      ylim = c(0, n_rows_labels * label_height), frame.plot = TRUE,
      axes = FALSE, xaxs = "i", yaxs = "i", xlab = "", ylab = "")

    for (i in 1:n_rows_labels) {
      for (j in 1:n_cols_labels) {
        rect(xleft = x_starts[j], xright = x_starts[j] + label_width,
          ytop = y_starts[i], ybottom = y_starts[i] - label_height)
        my_data_row <- (i - 1) * n_cols_labels + j
        if (my_data_row <= nrow(page_data)) {
          text(x_starts[j] + text_left_margin,
            y_starts[i] - text_top_margin, page_data$name[my_data_row],
            pos = 4, cex = text_size)
          text(x_starts[j] + text_left_margin,
            y_starts[i] - text_top_margin - line_spacer, page_data$code[my_data_row],
            pos = 4, cex = text_size)
          text(x_starts[j] + text_left_margin,
            y_starts[i] - text_top_margin - 2 * line_spacer,
            paste(page_data$pid[my_data_row], page_data$lid[my_data_row], sep = "-"),
            pos = 4, cex = text_size)
        }
      }
    }
    dev.off()
  }

  files <- list.files(".", pattern = "labels_page_")
  pdf_combine(files, output = "labels.pdf")
  file.remove(files)

  detach(pars)

}
