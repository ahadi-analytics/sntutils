# Vectorized version of digest::digest

This function applies the digest::digest function to each element of a
vector, returning a vector of hash values.

## Usage

``` r
vdigest(
  object,
  algo = c("md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64",
    "murmur32", "spookyhash", "blake3", "crc32c", "xxh3_64", "xxh3_128"),
  serialize = TRUE,
  file = FALSE,
  length = Inf,
  skip = "auto",
  ascii = FALSE,
  raw = FALSE,
  seed = 0,
  errormode = c("stop", "warn", "silent"),
  serializeVersion = .getSerializeVersion()
)
```

## Arguments

- object:

  An R object to calculate a hash value for

- algo:

  The hashing algorithm to be used. See digest::digest for options

- serialize:

  Whether to serialize the object first. Default TRUE

- file:

  A filename to read from. Default FALSE

- length:

  The number of characters to read from file. Default NULL

- skip:

  Number of bytes to skip before reading file. Default 0

- ascii:

  If TRUE, return hash in ASCII format. Default FALSE

- raw:

  If TRUE, return raw vector. Default FALSE

- seed:

  Random seed for hash. Default 0

- errormode:

  Error mode for file reading. Default "stop"

- serializeVersion:

  Version used in serialization. Default NULL

## Value

A character vector of hash values

## Examples

``` r
vdigest(c("a", "b", "c"))
#>                                  a                                  b 
#> "127a2ec00989b9f7faf671ed470be7f8" "ddf100612805359cd81fdc5ce3b9fbba" 
#>                                  c 
#> "6e7a8c1c098e8817e3df3fd1b21149d1" 
vdigest(as.character(iris$Species))
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                             setosa                             setosa 
#> "946a2c38121bed59091a362f5015327e" "946a2c38121bed59091a362f5015327e" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                         versicolor                         versicolor 
#> "fa66f5fefadcc79a57a5afe78fe680db" "fa66f5fefadcc79a57a5afe78fe680db" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
#>                          virginica                          virginica 
#> "b313f00809c319b9b5918795d13ca47a" "b313f00809c319b9b5918795d13ca47a" 
```
