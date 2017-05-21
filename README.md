# Shcrabble

Generate an SVG Scrabble-board from any text document.

## Example

Generate a board from the file `haiku.txt`:

```
> cat haiku.txt
Over the wintry
forest, winds howl in rage
with no leaves to blow.

> ./bin/shcrabble haiku.txt haikuboard
```

This will generate an `haikuboard.svg` file:

![haikuboard.svg](https://cdn.rawgit.com/rkrzr/shcrabble/master/haikuboard.svg#v1)

## Installation

You will need the `stack` package manager to install `shcrabble`:

```
git clone git@github.com:rkrzr/shcrabble.git
cd shcrabble
stack build
stack install
```

This will put the `shcrabble` executable in the cloned directory in `./bin/shcrabble`


## Usage

`shcrabble` is a simple command-line program that takes a text file as input
and writes an SVG file of the generated scrabble-board as output.

```
Usage: shcrabble [-g|--generate-svg-per-turn] INPUT_FILE OUTPUT_FILE
```

## Current Status

This is a toy program that was written for fun.
The current implementation is rather inefficient and can only handle smaller text files.
