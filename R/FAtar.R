### For tar archives containing multiple FA files
### especially different lead times from a single run.
### If the tar file contains subdirectories,
### these also show up as separate entries in the listing.
### So to cope with this, FAopen must be "forgiving"...

FindInTar <- function(archname, filename, by=10^7, buffer=FALSE, quiet=TRUE){
  blocksize <- 512
  # open archive
  if (!file.exists(archname)) stop("File ", archname, " not found")
  # compressed file?
  atype <- arch_type(archname)

  tf <- file(archname, open='rb')
  tf <- switch(atype,
               "tar" = file(archname, open='rb'),
               "gz"  = gzfile(archname, open="rb"),
               "xz"  = xzfile(archname, open="rb"),
               "bz"  = bzfile(archname, open="rb"),
               exit("unknown compression type")
               )
  on.exit(close(tf))
  # offset of first entry is zero

  offset <- 0
  count <- 0
  tf_loc <- 0
  found <- FALSE
  while (TRUE) {
    # goto beginning of entry
    if (atype == "tar") {
      seek(tf, offset)
    } else if (offset > tf_loc) {
      tf <- zip_skip(tf, offset - tf_loc, by=by)
    }
    tf_loc <- offset
    header <- readBin(tf, what="raw", n=blocksize)
    tf_loc <- tf_loc + blocksize

    if (length(header) < blocksize) {
      if (!quiet) print("Reached end of file?")
      break
    }
    # a tar archive usually ends with two 0-filled records
    if (all(header == 0)) {
      if (!quiet) print("Empty header: file end?")
      break
    }

    # check UStar format
    # it /should/ be a \0 terminated string "ustar"
    # but older GNU-tar files have "ustar  "
    # so don't be too strict...
    check <- rawToChar(header[258:262])
    if (check != "ustar") {
      stop("Probably not a correct tar archive.", "count=", count, "check=",check)
    }

    fName <- rawToChar(header[1:100])
    # size
    # file size is encoded as a length 12 octal string,
    # with the last character being '\0' (so 11 actual characters)
    sz <- rawToChar(header[125:136])
    # convert string to number of bytes
    fsize <- sum(as.numeric(strsplit(sz,'')[[1]])*8^(10:0))

    # is it a real file? (not a link, directory, PAX header...)
    typeflag <- rawToChar(header[157])
    pax <- (typeflag %in% c("x", "g"))
    if (typeflag %in% c("", "0")) {
      # it's a file, not some PAX extension
      count <- count + 1
      # name
      if (nchar(fName)==0) break
      if (!quiet) print(paste("Found file", fName))
      if (!quiet) print(paste("  size:", fsize))
      if (!quiet) print(paste("  loc:", tf_loc))
      # prefix?
      prefix <- rawToChar(header[346:500])
      if (!prefix=="") fName <- paste(prefix, fName, sep="/")
      if ( (is.numeric(filename) && count == filename) || fName==filename) {
        found <- TRUE
        break
      }
    }
    # goto the next file
    offset <- offset + blocksize*(ceiling(fsize/blocksize)+1)
  }
  # we have arrived at the requested file, or it is not available
  # tf_loc is pointing at the actual start of the file
  if (!found) stop("file ", filename, " not available")
  if (!buffer) return(tf_loc)
  attr(fName, "tar.offset") <- tf_loc
  attr(fName, "tarfile") <- archname
  attr(fName, "size") <- fsize
  attr(fName, "membuff") <- readBin(tf, what="raw", n=fsize)
  result <- list(fName)
  names(result) <- fName
  return(result)
}
  
# in a [bgx]zipped file, using seek() to skip to a byte location is unsafe
# so we do it by reading raw bytes
# just to avoid having to read multiple GB of data, we do it in steps
# default step is 10MB
# NOTE: this is different from seek: we can only go FORWARD
zip_skip <- function(zfile, skip, by=10^7) {
  while (skip > by) {
    readBin(zfile, "raw", by)
    skip <- skip - by
  }
  if (skip > 0) readBin(zfile, "raw", skip)
  zfile
}


arch_type <- function(filename) {
  if (is.character(filename)) {
    if (!file.exists(filename)) stop(sprintf("File %s does not exist.", filename))
    zf <- file(filename, open='rb')
    on.exit(close(zf))
  }
  sig <- as.character(readBin(zf, what="raw", n=300))
  if (all(sig[1:7] == c("fd","37","7a","58","5a","00","00"))) {
    return("xz")
  } else if (all(sig[1:2] == c("1f","8b"))) {
    return("gz")
  } else if (all(sig[1:3] == c("42","5a","68"))) {
    return("bz")
  } else if (all(sig[258:262] == c("75", "73", "74", "61", "72"))) {
    return("tar")
  } else {
    return("unknown")
  }
}


#' Parse a (zipped) tar file and make a list of the files
#' @param archname The name of a (gzipped) tar file
#' @param buffer Set to TRUE if the internal files must be read into memory buffers.
#'          This is more efficient when working with compressed archive files.
#' @return A named list of the files contained in the archive.
#'     Every list element has 3 attributes: filename, (byte) location and file size.
#' @export
ParseTar <- function(archname, buffer = FALSE) {
  # TODO: check for "gzip" by looking at first 2 bytes (==? as.raw(c(31,139)))
  # BUT: FAopen will not work easily on tgz files, because you'd have to re-code all binary reads
  blocksize <- 512
  if (!file.exists(archname)) stop("File ", archname, " not found")
  # open archive
  # check file type
  atype <- arch_type(archname)
  tf <- switch(atype,
               "tar" = file(archname, open='rb'),
               "gz"  = gzfile(archname, open="rb"),
               "xz"  = xzfile(archname, open="rb"),
               "bz"  = bzfile(archname, open="rb"),
               exit("unknown compression type")
               )

  on.exit(close(tf))
  # offset of first entry is zero
  fnames <- list()

  offset <- 0
  nfile <- 0
  tf_loc <- 0
  while (TRUE) {
    # goto beginning of entry
    # in zipped archives and on windows: avoid using seek()!!!
    if (atype == "tar") {
      seek(tf, offset)
    } else if (offset > tf_loc) {
    ### but for up to ~10MB : no problem, I guess
    ### "seek" is not even defined for xz files...
      ## large file: split readBin into blocks of e.g. 1E7 bytes (~10MB)?
      # readBin(tf, what = "raw", n = offset - seek(tf))
      tf <- zip_skip(tf, offset - tf_loc)
    }
    tf_loc <- offset
    # read file name
    # readBin(..., what="char") can give errors in gzipped file
    header <- readBin(tf, what="raw", n=blocksize)
    tf_loc <- tf_loc + blocksize
    if (length(header) < blocksize) break
    # a tar archive usually ends with two 0-filled records
    if (all(header == 0)) break
    # check UStar format
    # it /should/ be a \0 terminated string "ustar"
    # but older GNU-tar files have "ustar  "
    # so don't be too strict...
    check <- rawToChar(header[258:262])
    if (check != "ustar") stop("Probably not a correct tar archive.")
    # size
    # file size is encoded as a length 12 octal string,
    # with the last character being '\0' (so 11 actual characters)
    sz <- rawToChar(header[125:136])
    # convert string to number of bytes
    fsize <- sum(as.numeric(strsplit(sz,'')[[1]])*8^(10:0))
    # is it a real file? (not a link, directory, PAX header...)
    typeflag <- rawToChar(header[157])
    if (typeflag %in% c("", "0")) {
      nfile <- nfile + 1
      # name
      fName <- rawToChar(header[1:100])
      if (nchar(fName)==0) break
      # prefix?
      prefix <- rawToChar(header[346:500])
      if (!prefix=="") fName <- paste(prefix, fName, sep="/")

      fnames <- c(fnames, fName)
      attr(fnames[[nfile]], "tar.offset") <- offset + blocksize
      attr(fnames[[nfile]], "tarfile") <- archname
      attr(fnames[[nfile]], "size") <- fsize
      # cat(sprintf('entry %s, %i bytes (type %s)\n', fName, fsize, typeflag))
      if (buffer) {
        # we should be at the start of the file now (header already done)
        attr(fnames[[nfile]], "membuff") <- readBin(tf, what="raw", n=fsize)
        tf_loc <- tf_loc + fsize
      }
    } else {
      ### "x": PAX header, "g": global PAX header, "5"=(sub)directory
      # cat(sprintf('entry %s, %i bytes, TYPE: %s\n', rawToChar(header[1:100]), fsize, typeflag))
    }
    # goto the next (header) message
    # skip actual file
    offset <- offset + blocksize*(ceiling(fsize/blocksize) + 1)
  }
# return a named list of characters strings with attributes?
  names(fnames) <- fnames
  return(fnames)
}

FAopenTar <- function(archname, lparse=FALSE, quiet=TRUE){
### parse a tar file and create a list of FAfile objects
### by default don't parse the complete files
###   (so you won't see which fields are spectral etc)
### only the field names and data locations.
### this is faster and you probably don't really need the extra info anyway.
### it may be interesting to index simply by lead time of the file
### because often tar files are archives of a single run

  if (!file.exists(archname)) stop("File ", archname, " not found")
  # For compressed archives, we must take all files in memory!
  atype <- arch_type(archname)
  if (!quiet) print(paste("Archive type:", atype))
  if (atype == "tar") {
    archlist <- ParseTar(archname, buffer = FALSE)
    if (!quiet) print(archlist)
    print(archlist[[1]])
  } else if (atype %in% c("bz", "gz", "xz")) {
    archlist <- ParseTar(archname, buffer = TRUE)
  } else {
    stop("Can not read archive type ", atype)
  }

  # the error-catching is very ugly. There must be a cleaner way.
  result <- lapply(archlist, function(ff) try(
               FAopen(ff, lparse=lparse, quiet=quiet),
               silent=TRUE))
  #names(result) <- archlist

# any errors? if yes, take only the entries that were OK.
  print(result[[1]])
  NOK <- vapply(result, function(x) inherits(x, "try-error"), FUN.VALUE=TRUE)

  if (any(NOK)) result <- result[!NOK]
  if (all(NOK)) stop("Archive does not contain any valid FA files.")
  result
}


