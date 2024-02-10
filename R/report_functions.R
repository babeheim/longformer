create_labels<-
function (label_data,pars , output = "output.pdf", qrs_label = TRUE) 
{
	if(pars == "field_labels"){		
		pars <- list(
		  n_rows_labels = 17,
		  n_cols_labels = 5,
		  label_width = 31.75,
		  label_height = 12.7,
		  text_left_margin = 0,
		  text_top_margin = 5,
		  line_spacer = 2,
		  text_size = .5,
		  margin_scale = 5,
		  margins = c(0.1875, 0.75, 0.1875, 0.75)
		)
	}
    attach(pars)
    x_starts <- 0:(n_cols_labels - 1) * label_width
    y_starts <- rev(1:n_rows_labels * label_height)
    n_labels_per_page <- n_rows_labels * n_cols_labels
    n_pages <- ceiling(nrow(label_data)/n_labels_per_page)
    n_labels_final_page <- nrow(label_data)%%n_labels_per_page
    for (k in 1:n_pages) {
        n_labels_this_page <- n_labels_per_page
        if (k == n_pages & n_labels_final_page > 0) 
            n_labels_this_page <- n_labels_final_page
        page_data <- label_data[k * c(1:n_labels_this_page), ]
        filename <- paste0("labels_page_", sprintf("%04d", k), ".pdf")
        pdf(filename, height = 11, width = 8.5)
        par(mai = margins)
        plot(1, 1, type = "n", xlim = c(0, n_cols_labels * 
            label_width), ylim = c(0, n_rows_labels * label_height), 
            frame.plot = FALSE, axes = FALSE, xaxs = "i", 
            yaxs = "i", xlab = "", ylab = "")
        for (i in 1:n_rows_labels) {
            for (j in 1:n_cols_labels) {
                rect(xleft = x_starts[j], xright = x_starts[j] + 
                  label_width, ytop = y_starts[i], ybottom = y_starts[i] - 
                  label_height, border=NA)
                my_data_row <- (i - 1) * n_cols_labels + j
                if (my_data_row <= nrow(page_data)) {
					if(qrs_label==TRUE){
						my_qr = qr_code(page_data$label_id[my_data_row], ecl = "H")
						my_qr_pars <- data.frame(
								xleft=x_starts[j] + (0.5 + 0.79375), 
								ybottom=y_starts[i]-label_height + (3.175 - 0.79375),
								xright=x_starts[j]+label_height - (3.175 + 0.79375),
								ytop=y_starts[i] - (3.175))
						rasterImage(my_qr, my_qr_pars[1],my_qr_pars[2],my_qr_pars[3],my_qr_pars[4])
						} else{ my_qr_pars<-c(0,0,0,0)
							}
					my_qr_width <- my_qr_pars[3] - my_qr_pars[1]
						
					
                  text(x_starts[j] + text_left_margin + my_qr_width, y_starts[i] - 
                    text_top_margin, paste(page_data$pid[my_data_row], page_data$code[my_data_row], sep='-'), 
                    pos = 4, cex = text_size)
                  text(x_starts[j] + text_left_margin + my_qr_width, y_starts[i] - 
                    text_top_margin - line_spacer, page_data$name[my_data_row], 
                    pos = 4, cex = text_size)
                  text(x_starts[j] + text_left_margin + my_qr_width, y_starts[i] - 
                    text_top_margin - 2 * line_spacer, "_____2023 NDC:_______", 
                    pos = 4, cex = text_size)
                }
            }
        }
        dev.off()
    }
    files <- list.files(".", pattern = "labels_page_")
    pdf_combine(files, output = output)
    file.remove(files)
    detach(pars)
}