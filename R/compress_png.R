#' ensure pngquant is available (non-interactive, cross-platform)
#'
#' order:
#' 1) PNGQUANT_PATH env var
#' 2) Sys.which("pngquant")
#' 3) windows-only: download official zip to temp dir and add to PATH
#' 4) else: fail with guidance (brew/apt)
#'
#' @param verbosity logical. print info if TRUE.
#' @return character path or NULL if unavailable.
#' @noRd
ensure_pngquant <- function(verbosity = FALSE) {
  # helper: does path point to an executable
  is_exec <- function(p) {
    nzchar(p) && file.exists(p) && file.access(p, 1) == 0
  }

  # 1) honor env var
  env_path <- Sys.getenv("PNGQUANT_PATH", unset = "")
  if (is_exec(env_path)) {
    return(normalizePath(env_path))
  }

  # 2) on PATH already - try multiple detection methods
  # Try Sys.which() first, but handle potential failures
  on_path <- tryCatch(
    {
      result <- Sys.which("pngquant")
      if (nzchar(result)) result else ""
    },
    error = function(e) ""
  )

  # If Sys.which() failed or returned empty, try 'which' command directly
  if (!is_exec(on_path)) {
    on_path <- tryCatch(
      {
        result <- system("which pngquant 2>/dev/null", intern = TRUE)
        if (length(result) > 0 && nzchar(result[1])) result[1] else ""
      },
      error = function(e) "",
      warning = function(w) ""
    )
  }

  # If still not found, check common installation paths
  if (!is_exec(on_path)) {
    common_paths <- c(
      "/usr/local/bin/pngquant",
      "/usr/bin/pngquant",
      "/opt/homebrew/bin/pngquant",  # Apple Silicon Macs
      "/opt/local/bin/pngquant"       # MacPorts
    )

    for (path in common_paths) {
      if (is_exec(path)) {
        on_path <- path
        break
      }
    }
  }

  if (is_exec(on_path)) {
    return(normalizePath(on_path))
  }

  # 3) windows: download official zip to a user-writable dir
  if (.Platform$OS.type == "windows") {
    tools_dir <- file.path(tempdir(), "pngquant_tools")
    dir.create(tools_dir, recursive = TRUE, showWarnings = FALSE)

    zip_url <- "https://pngquant.org/pngquant-windows.zip"
    zip_file <- file.path(tools_dir, "pngquant-windows.zip")

    if (verbosity) {
      cli::cli_alert_info("downloading pngquant (windows zip)...")
    }

    utils::download.file(
      url = zip_url,
      destfile = zip_file,
      mode = "wb",
      quiet = !verbosity
    )
    utils::unzip(zip_file, exdir = tools_dir)

    cand <- list.files(
      tools_dir,
      pattern = "^pngquant\\.exe$",
      full.names = TRUE,
      recursive = TRUE
    )
    if (length(cand) == 0) {
      if (verbosity) {
        cli::cli_alert_warning("pngquant.exe not found.")
      }
      return(NULL)
    }

    exe <- normalizePath(cand[1])
    Sys.setenv(PATH = paste(dirname(exe), Sys.getenv("PATH"), sep = ";"))
    if (verbosity) {
      cli::cli_alert_success(paste("pngquant ready at", exe))
    }
    return(exe)
  }

  # 4) macos/linux: rely on system package managers
  if (verbosity) {
    cli::cli_bullets(c(
      "!" = "pngquant not available on PATH.",
      " " = "install with your package manager:",
      " " = "- macOS: brew install pngquant",
      " " = "- Ubuntu/Debian: sudo apt-get install -y pngquant"
    ))
  }
  NULL
}

#' find or install pngquant executable (wrapper for back-compat)
#'
#' @param verbosity logical. print info if TRUE.
#' @return character path or NULL if unavailable.
#' @noRd
find_pngquant <- function(verbosity = FALSE) {
  ensure_pngquant(verbosity = verbosity)
}

#' Calculate Compression Statistics
#'
#' This function calculates statistics for file compression operations,
#' including bytes saved and percentage reduction.
#'
#' @param filename String, The name of the file being compressed
#' @param init_size Numeric. Initial file size in bytes before compression.
#' @param final_size Numeric. Final file size in bytes after compression.
#' @param verbosity Logical. Controls output verbosity. FALSE = silent,
#'    TRUE = verbose.
#'
#' @return A list containing compression statistics:
#'   \item{initial_size}{Original file size in bytes}
#'   \item{final_size}{Compressed file size in bytes}
#'   \item{bytes_saved}{Number of bytes saved}
#'   \item{percent_saved}{Percentage of size reduction}
#'
#' @examples
#' # Basic usage
#' stats <- compression_stats("mylovely.plot.png", 100000, 75000)
#' str(stats)
#'
#' # With verbosity to display results
#' if (interactive()) {
#'   compression_stats("mylovely.plot.png", 5242880, 3145728, verbosity = TRUE)
#' }
#' @noRd
compression_stats <- function(filename, init_size, final_size,
                              verbosity = FALSE) {
  savings <- init_size - final_size
  pct_saved <- round(100 * savings / init_size, 2)

  # Format sizes in appropriate units (MB, KB, bytes)
  format_size <- function(size) {
    if (size >= 1048576) { # 1MB in bytes
      paste0(round(size / 1048576, 2), " MB")
    } else if (size >= 1024) {
      paste0(round(size / 1024, 2), " KB")
    } else {
      paste0(size, " bytes")
    }
  }

  result <- list(
    initial_size = init_size,
    final_size = final_size,
    bytes_saved = savings,
    percent_saved = pct_saved
  )

  if (verbosity) {
    cli::cli_h2("Compression Summary")

    cli::cli_alert_success(
      paste("Successfully compressed:", crayon::blue(filename))
    )

    cli::cli_alert_info(
      paste(
        "Total compression:",
        format_size(savings),
        sprintf("(%.2f%% saved)", pct_saved)
      )
    )

    cli::cli_bullets(c(
      "i" = if (pct_saved > 50) {
        "Excellent compression!"
      } else if (pct_saved > 20) {
        "Good compression"
      } else {
        "Minimal compression"
      }
    ))

    cli::cli_h3("File Size")
    cli::cli_bullets(c(
      "\u2022" = paste("Before compression:", format_size(init_size)),
      "\u2022" = paste("After compression:", format_size(final_size))
    ))
  }

  invisible(result)
}

#' Compress a single PNG file using pngquant
#'
#' @description
#' Compresses a single PNG file using the pngquant utility, which performs
#' lossy compression to reduce file size while maintaining visual quality.
#'
#' @param file Character string specifying the path to the PNG file to compress
#' @param speed Integer. Speed/quality trade-off from 1 (brute-force) to 10
#' (fastest). Default is 3. Speed 10 has 5% lower quality but is 8 times
#'    faster.
#' @param png_overwrite Logical. If TRUE, will overwrite existing files.
#'    Default is TRUE
#' @param verbosity Logical. Controls output verbosity. FALSE = silent,
#'    TRUE = verbose.
#'
#' @return A list containing:
#'   \item{success}{Logical. TRUE if compression was successful, FALSE
#'         otherwise}
#'   \item{stats}{Compression statistics if successful, NULL otherwise}
#'
#' @details
#' The function uses system parameters defined in the parent environment:
#' - pngquant_path: Path to the pngquant executable
#' - speed: Compression speed (1-11, where 1 is slowest but highest quality)
#'
#' The function forces overwriting of the original file with the compressed
#' version. Status code 99 indicates that the file was already compressed.
#' Compress a single PNG file using pngquant
#'
#' @description
#' Compresses a single PNG file using the pngquant utility, which performs
#' lossy compression to reduce file size while maintaining visual quality.
#'
#' @param pngquant_path  Character string specifying path to the pngquant
#'    executable
#' @param file Character string specifying the path to the PNG file to compress
#' @param verbosity Logical. Controls output verbosity. FALSE = silent,
#'    TRUE = verbose.
#' @param speed Integer. Speed/quality trade-off from 1 (brute-force) to 10
#' (fastest). Default is 3. Speed 10 has 5% lower quality but is 8 times
#'    faster.
#'
#' @return A list containing:
#'   \item{success}{Logical. TRUE if compression was successful, FALSE
#'    otherwise}
#'   \item{stats}{Compression statistics if successful, NULL otherwise}
#'
#' @details
#' The function uses system parameters defined in the parent environment:
#' - pngquant_path: Path to the pngquant executable
#' - speed: Compression speed (1-11, where 1 is slowest but highest quality)
#' @noRd
pngquant_compress_single_file <- function(pngquant_path, file,
                                          speed, png_overwrite,
                                          verbosity = FALSE) {
  # Get initial file size before compression
  init_size <- file.info(file)$size

  cmd_parts <- c(
    shQuote(pngquant_path),
    "--speed", as.character(speed),
    if (png_overwrite) "--force" else NULL,
    "--ext", ".png",
    shQuote(file)
  )
  result <- system(
    paste(cmd_parts, collapse = " "),
    intern = TRUE,
    ignore.stderr = FALSE
  )
  stat <- attr(result, "status")

  # Extract filename from path
  filename <- basename(file)

  if (is.null(stat) || stat == 0) {
    # Get file sizes for statistics
    final_size <- file.info(file)$size

    # Show compression info
    if (final_size < init_size) {
      if (verbosity) {
        stats <- compression_stats(
          filename, init_size, final_size, verbosity
        )
      }
    } else if (verbosity) {
      cli::cli_alert_info(
        paste("File already compressed:", crayon::blue(filename))
      )
    }

    stats <- list(
      initial_size = init_size,
      final_size = final_size,
      bytes_saved = init_size - final_size,
      percent_saved = (init_size - final_size) / init_size * 100
    )
    list(success = TRUE, stats = stats)
  } else {
    list(success = FALSE, stats = NULL)
  }
}

#' Compress PNG Files in a Directory or a Single PNG File with pngquant
#'
#' This function compresses either a single PNG file or all PNG files in a
#' specified directory using pngquant optimization to reduce file size while
#' maintaining visual quality. pngquant is a lossy compression tool that can
#' reduce file sizes by up to 70% while preserving full alpha transparency.
#'
#' @param path A string specifying either the path to a single PNG file or
#' a directory containing PNG files.
#' @param png_overwrite Logical. If TRUE, will overwrite existing files.
#'    Default is TRUE
#' @param speed Integer. Speed/quality trade-off from 1 (brute-force) to 10
#' (fastest). Default is 3. Speed 10 has 5% lower quality but is 8 times
#'    faster.
#' @param verbosity Logical. Controls the amount of information displayed.
#'    FALSE = minimal, TRUE = detailed. Default is TRUE.
#' @return For single files, returns a list with compression statistics.
#'    For directories, returns a data frame with statistics for all files.
#'    The function also works by side effect, compressing PNG files.
#' @examples
#' # Compress all PNG files in a directory
#' # compress_png("path/to/your/folder")
#'
#' # Compress a single PNG file
#' # compress_png("path/to/your/image.png")
#'
#' # Compress with automatic installation if pngquant is not found
#' # compress_png("path/to/your/folder", auto_install = TRUE)
#'
#' # Compress and overwrite existing files
#' # compress_png("path/to/your/folder", force = TRUE)
#'
#' @export
compress_png <- function(path, png_overwrite = TRUE,
                         speed = 1, verbosity = TRUE) {

  # Find pngquant executable
  pngquant_path <- find_pngquant(verbosity)
  if (is.null(pngquant_path)) {
    cli::cli_alert_warning("pngquant not found and installation declined.")
    return(invisible(NULL))
  }

  # Check if path is a file or directory
  is_file <- FALSE
  if (file.exists(path)) {
    if (!dir.exists(path) && grepl("\\.png$", path, ignore.case = TRUE)) {
      is_file <- TRUE
      png_files <- path
    } else if (dir.exists(path)) {
      png_files <- list.files(path, pattern = "\\.png$", full.names = TRUE)
      if (length(png_files) == 0) {
        cli::cli_alert_warning("No PNG files found in the directory.")
        return(invisible(NULL))
      }
    } else {
      cli::cli_alert_warning("Path is neither a PNG file nor a directory.")
      return(invisible(NULL))
    }
  } else {
    cli::cli_alert_warning("Path does not exist.")
    return(invisible(NULL))
  }

  # Process files
  if (is_file) {
    # Single file processing
    result <- pngquant_compress_single_file(
      pngquant_path,
      png_files,
      speed,
      png_overwrite,
      verbosity = verbosity
    )
    if (result$success) {
      if (!is.null(result$already_compressed) && result$already_compressed) {
        cli::cli_alert_info(
          "File is already compressed, skipping compression."
        )
      }
      invisible(result$stats)
    } else {
      cli::cli_alert_warning("Error compressing file.")
      invisible(NULL)
    }
  } else {
    # Directory processing
    pb <- progress::progress_bar$new(
      format = "Compressing files [:bar] :percent  ETA: :eta",
      total = length(png_files), width = 60,
      show_after = 0.1
    )

    compressed_count <- 0
    skipped_count <- 0
    errors <- character()
    all_stats <- list()

    for (file in png_files) {
      result <- pngquant_compress_single_file(
        pngquant_path, file, speed,
        png_overwrite, verbosity
      )
      if (result$success) {
        if (!is.null(result$already_compressed) && result$already_compressed) {
          skipped_count <- skipped_count + 1
        } else {
          compressed_count <- compressed_count + 1
        }
        all_stats[[file]] <- result$stats
      } else {
        errors <- c(errors, paste("Error with file:", file))
      }
      pb$tick()
    }
    # Convert all_stats list to a data frame
    if (length(all_stats) > 0) {
      stats_df <- do.call(rbind, all_stats)
      stats_df <- as.data.frame(stats_df)
      stats_df$filename <- names(all_stats)

      # Count how many files were actually compressed (init_size > final_size)
      actually_compressed <- sum(stats_df$init_size > stats_df$final_size)
      total_planned <- nrow(stats_df)
      optimised <- total_planned - actually_compressed
    } else {
      stats_df <- data.frame()
      actually_compressed <- 0
      total_planned <- 0
    }


    cli::cli_h2(
      glue::glue("Out of {total_planned} images:")
    )

    cli::cli_bullets(c(
      "\u2714" = glue::glue("{actually_compressed} were compressed"),
      "\u2139" = glue::glue("{optimised} were skipped as already compressed")
    ))

    if (length(errors) > 0) {
      cli::cli_alert_warning(
        paste(length(errors), "files could not be compressed:")
      )

      for (i in seq_len(min(5, length(errors)))) {
        cli::cli_alert_info(errors[i])
      }

      if (length(errors) > 5) {
        cli::cli_alert_info(
          sprintf("... and %d more", length(errors) - 5)
        )
      }
    }

    invisible(stats_df)
  }
}
