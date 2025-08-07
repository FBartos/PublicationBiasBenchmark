is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

save_rbind <- function(df_list) {

  if (length(df_list) == 1)
    return(df_list[[1]])

  # Step 1: Get all unique column names across all data frames
  all_cols <- unique(unlist(lapply(df_list, names)))

  # Step 2: Add missing columns with NA to each data frame
  df_list_aligned <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    # Reorder to match the full column list
    df[all_cols]
  })

  df <- do.call(rbind, df_list_aligned)

  return(df)
}
