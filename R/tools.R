is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# Collision-free composite key for a method and its setting. The byte length of
# the method makes the key injective for any strings (unlike "a.b"/"c" versus
# "a"/"b.c" under a plain separator) and the function is vectorised.
.method_key <- function(method, method_setting) {
  method <- as.character(method)
  # Empty input has no keys (paste0() would recycle it into the single key ":/").
  if (!length(method) && !length(method_setting)) return(character())
  paste0(nchar(method, type = "bytes"), ":", method, "/", method_setting)
}

safe_rbind <- function(df_list) {

  if (!length(df_list))
    return(NULL)

  # Remove empty data.frames
  nrows   <- vapply(df_list, nrow, integer(1))

  if (all(nrows == 0L))
    return(df_list[[1]])

  df_list <- df_list[nrows > 0L]

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

    measure_name <- sub("_mcse$", "", grep("_mcse$", names(df_temp), value = TRUE))

    colnames(df_temp)[colnames(df_temp) %in% c("replaced", "n_valid")] <- paste0(
      colnames(df_temp)[colnames(df_temp) %in% c("replaced", "n_valid")], "_", measure_name
    )

    df_out <- merge(df_out, df_temp, by = "merge_id", all.x = TRUE, all.y = TRUE)
  }

  df_out[["merge_id"]] <- NULL
  return(df_out)
}
