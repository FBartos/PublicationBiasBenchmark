is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

safe_rbind <- function(df_list) {

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
safe_merge <- function(df_list) {

  if (length(df_list) == 1)
    return(df_list[[1]])

  df_out <- df_list[[1]]
  df_out$merge_id <- with(df_out, paste0(method, "-", method_setting, "-", condition_id))

  for(i in 2:length(df_list)){

    df_temp <- df_list[[i]]
    df_temp$merge_id <- with(df_temp, paste0(method, "-", method_setting, "-", condition_id))

    df_temp[["method"]]          <- NULL
    df_temp[["method_setting"]]  <- NULL
    df_temp[["condition_id"]]    <- NULL

    df_out <- merge(df_out, df_temp, by = "merge_id")
  }

  df_out[["merge_id"]] <- NULL
  return(df_out)
}
