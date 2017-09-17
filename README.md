# storage-copy

Here's a short little script I wrote to upload a local directory to Azure blob storage using Haskell. This was for my own
enrichment, but feel free to take a peek if you so desire. Most of the interesting code is in `./src/Lib.hs`.

## How to use

When prompted, enter the path to the directory that you would like to upload. Then enter the name of the blob storage account
you would like to use. If your blob storage account url looks like `https://myblobstorage.blob.core.windows.net/` then you 
would say `myblobstorage`. Then for the final question, enter your Shared Access Signature without the `?`. You can generate
a SAS in the azure portal.

## How it works.

The script creates a new container with the name of the directory. For example, running the script with the project directory of 
this repo would create a container called `storage-copy` in azure. It then walks all the directories uploading every file that it
finds to that container with the path relative to the root. Empty directories won't be uploaded because they contain no contents.

## Caveats

The script does minimal error handling. It will recover if the container already exists, but all other errors will crash the
program. The `PUT Blob` method has a size limit of 100MB. There are other methods to upload large files, but this script does
not fall back on them so the upload will fail.

In a few tests, it doesn't work on Windows. This appears to be an issue with the `req` networking package (or potentially one
of it's dependencies). I haven't tried it on the Linux Subsystem for Windows. That might be a workaround.
