Methuselah Generator
====================

Hi! This is the beginning of a haskell api generator for neovim, and there will be an accompanying package for actually talking to neovim.

Things that are done:

+ Base types + Prisms for reading the API out of a message pack dump from neovim.

To be done:

+ Transform the data types into legit haskell via TH and dump to a module.

## Using

You can't really use it yet. Basically main looks for a file called data.bin that it assumes you got by running 'nvim --api-msgpack-metadata', then dumps that to the screen. The function `toApi` tries to make a nicely typed version of it. That's all for now.
