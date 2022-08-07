# Obsmd 2 LogSeq

Migrate Markdown files from an [Obsidian](https://obsidian.md/) vault to a [LogSeq](https://logseq.com/) one.

WARNING: this is a work-in-progress, please backup your data before using!

## Installation

This is a [Haskell](https://www.haskell.org/) program that must be compiled using [Stack](https://docs.haskellstack.org)
```sh
stack install
```

## Usage

Create a LogSeq vault first, then:
```sh
obsmd2logseq-exe <PATH_TO_OBSIDIAN_VAULT> <PATH_TO_LOGSEQ_VAULT>
```
