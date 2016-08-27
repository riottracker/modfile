{-# LANGUAGE RecordWildCards #-}

{- This implementation will follow the documentation of .MOD files found at
 - http://www.fileformat.info/format/mod/corion.htm
 - This means it assumes there can be up to 31 instruments.
 - The structure, according to the document, is as follows:
 - 20 bytes for the songname; padded with spaces (according to another source,
 -    zeroes)
 - Then, 31 times: (Offset 20d)
 -   22 bytes for the instrument/sample name; padded with null bytes
 -   2 words (4 bytes): sampleLength / 2
 -   1 byte finetune (whereas only the lower half of the byte is taken into
 -     regard)
 -   1 byte: sample volume, valid between 0-40 hex
 -   1 word: sample looping start position / 2
 -   1 word: sample loop length / 2
 - 1 byte: 
