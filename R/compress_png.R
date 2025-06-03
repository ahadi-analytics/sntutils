#' Find or install pngquant executable
#'
#' This function checks if pngquant is installed on the system
#' and returns its path. If not found, it offers to install pngquant
#' automatically. On Windows, it will attempt to find the executable
#' in common installation locations before offering to install.
#'
#' @return Path to pngquant executable as character string, or NULL
#'   if not found/installed or if installation was declined
#' @export
find_pngquant <- function() {
  os <- Sys.info()[["sysname"]]
  pngquant_path <- Sys.which("pngquant")

  ## macOS: check common Homebrew / MacPorts locations if Sys.which() failed
  if (pngquant_path == "" && os == "Darwin") {
    homebrew_paths <- c(
      "/opt/homebrew/bin/pngquant", # Apple Silicon
      "/usr/local/bin/pngquant", # Intel Mac
      "/opt/local/bin/pngquant" # MacPorts
    )
    for (path in homebrew_paths) {
      if (file.exists(path)) {
        pngquant_path <- path
        cli::cli_alert_info(paste("Found pngquant at:", pngquant_path))
        break
      }
    }
  }

  ## Windows: search multiple likely install directories
  if (pngquant_path == "" && .Platform$OS.type == "windows") {
    path_sep <- "\\"
    potential_paths <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "pngquant", "pngquant.exe"),
      file.path(Sys.getenv("APPDATA"), "pngquant", "pngquant.exe"),
      file.path(
        Sys.getenv("USERPROFILE"),
        "Documents",
        "pngquant",
        "pngquant.exe"
      ),
      file.path(
        Sys.getenv("USERPROFILE"),
        "Downloads",
        "pngquant",
        "pngquant.exe"
      ),
      file.path(Sys.getenv("USERPROFILE"), "pngquant", "pngquant.exe"),
      file.path("pngquant_bin", "pngquant.exe"),
      file.path("pngquant", "target", "release", "pngquant.exe"),
      file.path("C:", "Program Files", "pngquant", "pngquant.exe"),
      file.path("C:", "Program Files (x86)", "pngquant", "pngquant.exe")
    )
    potential_paths <- sapply(
      potential_paths,
      function(p) normalizePath(p, winslash = path_sep, mustWork = FALSE)
    )
    for (p in potential_paths) {
      if (file.exists(p)) {
        pngquant_path <- p
        cli::cli_alert_info(paste("Found pngquant at:", pngquant_path))
        break
      }
    }
  }

  ## If still not found, ask user whether to install
  if (pngquant_path == "") {
    ans <- readline("pngquant is not installed. Install automatically? (y/n): ")
    if (tolower(ans) != "y") {
      cli::cli_alert_warning("pngquant installation aborted. Exiting function.")
      return(invisible(NULL))
    }

    ## Determine install_dir based on OS
    install_dir <- if (os == "Windows") {
      normalizePath(
        file.path(Sys.getenv("LOCALAPPDATA"), "pngquant"),
        winslash = path_sep,
        mustWork = FALSE
      )
    } else {
      normalizePath("/usr/local/bin", mustWork = FALSE)
    }
    dir.create(install_dir, showWarnings = FALSE, recursive = TRUE)

    ## Windows installation path
    if (os == "Windows") {
      if (Sys.which("git") != "") {
        clone_cmd <- paste0(
          "git clone -b msvc --recursive ",
          "https://github.com/kornelski/pngquant.git"
        )
        if (system(clone_cmd) != 0) {
          stop("Failed to clone pngquant repository.")
        }
        src_dir <- file.path(getwd(), "pngquant")
        build_cmd <- sprintf(
          "cmd /c cd %s && cargo build --release",
          shQuote(src_dir)
        )
        if (system(build_cmd) != 0) {
          stop("Failed to build pngquant.")
        }
        pngquant_path <- file.path(src_dir, "target", "release", "pngquant.exe")
        if (!file.exists(pngquant_path)) {
          stop("pngquant binary not found after build.")
        }
        file.copy(pngquant_path, file.path(install_dir, "pngquant.exe"))
        cli::cli_alert_success(paste("pngquant installed at:", pngquant_path))
        cli::cli_alert_info(
          paste("Also copied to:", file.path(install_dir, "pngquant.exe"))
        )
      } else {
        cli::cli_alert_info(
          "Git not available. Downloading pre-built pngquant binary..."
        )
        zip_url <- "https://pngquant.org/pngquant-windows.zip"
        zip_file <- file.path(tempdir(), "pngquant-windows.zip")
        utils::download.file(zip_url, destfile = zip_file, mode = "wb")
        utils::unzip(zip_file, exdir = install_dir)
        files_extracted <- list.files(
          install_dir,
          pattern = "pngquant\\.exe$",
          full.names = TRUE,
          recursive = TRUE
        )
        if (length(files_extracted) == 0) {
          stop("Pre-built pngquant binary not found after extraction.")
        }
        pngquant_path <- files_extracted[1]
        if (pngquant_path != file.path(install_dir, "pngquant.exe")) {
          file.copy(
            pngquant_path,
            file.path(install_dir, "pngquant.exe"),
            overwrite = TRUE
          )
          pngquant_path <- file.path(install_dir, "pngquant.exe")
        }
        cli::cli_alert_success(paste("pngquant installed at:", pngquant_path))
        Sys.setenv(PATH = paste(Sys.getenv("PATH"), install_dir, sep = ";"))
      }
    } else {
      ## Unix-like installation (macOS/Linux)
      if (Sys.which("git") != "") {
        clone_cmd <- paste0(
          "git clone --recursive https://github.com/kornelski/pngquant.git"
        )
        if (system(clone_cmd) != 0) {
          stop("Failed to clone pngquant repository.")
        }
        src_dir <- file.path(getwd(), "pngquant")
      } else {
        zip_url <- "https://github.com/kornelski/pngquant/" %+%
          "archive/refs/heads/master.zip"
        zip_file <- "pngquant-master.zip"
        utils::download.file(zip_url, destfile = zip_file, mode = "wb")
        utils::unzip(zip_file)
        src_dir <- file.path(getwd(), "pngquant-master")
      }
      build_cmd <- sprintf("cd %s && cargo build --release", shQuote(src_dir))
      if (system(build_cmd) != 0) {
        stop("Failed to build pngquant.")
      }
      pngquant_path <- file.path(src_dir, "target", "release", "pngquant")
      if (!file.exists(pngquant_path)) {
        stop("pngquant binary not found after build.")
      }
      cli::cli_alert_success(paste("pngquant installed at:", pngquant_path))
    }
  }

  ## Final sanity check: does the path actually exist?
  if (!file.exists(pngquant_path)) {
    cli::cli_alert_danger(
      paste("pngquant path exists but file not found at:", pngquant_path)
    )
    return(invisible(NULL))
  }

  ## Try a few system checks (which/command -v/type -p) to double-check
  system_checks <- c(
    system("which pngquant 2>/dev/null", intern = TRUE),
    system("command -v pngquant 2>/dev/null", intern = TRUE),
    system("type -p pngquant 2>/dev/null", intern = TRUE)
  ) |>
    suppressWarnings()

  for (check in system_checks) {
    if (!is.null(check) && check != "" && file.exists(check)) {
      cli::cli_alert_success(paste("Found pngquant at:", check))
      return(invisible(check))
    }
  }

    if (is.null(pngquant_path)) {
      cli::cli_bullets(c(
        "!" = "Cannot find pngquant. Common installation methods:",
        " " = "\u2014 First, try Homebrew (macOS): brew install pngquant",
        " " = "\u2014 Next, on Debian/Ubuntu: sudo apt-get install pngquant",
        " " = "\u2014 Or download manually: https://pngquant.org"
      ))
    }

  invisible(pngquant_path)
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
#' @export
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
#'
pngquant_compress_single_file <- function(pngquant_path, file, speed, png_overwrite,
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
  ensure_packages("progress")

  # Find pngquant executable
  pngquant_path <- find_pngquant()
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
      pngquant_path, png_files, speed,
      png_overwrite, verbosity
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
