#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# LANGUAGE OverloadedStrings #-}  --
import Turtle
import Data.List
import qualified Data.Text as T

main = do
  let parser = argPath "directory" "Stack project directory (should have a stack.yml file in it)"
  dir <- options "A utility for generating open source attribution reports for Haskell projects" parser
  cd dir
  stdout $ select intro
  stdout (go $ inshell "licensor" empty)

intro = msg ++ hdr
  where
  hdr = [ "| Package name | License  |"
        , "| ------------ | -------- |" ]
  msg = [
    "# Credits",
    "",
    "Unison is open source (see [LICENSE](./LICENSE))",
    "and is the work of [many excellent contributors](./CONTRIBUTORS.markdown).",
    "",
    "Unison also depends on open source software written by others.",
    "Below we list all transitive dependencies of Unison,",
    "with links to each project and its license.",
    "",
    "A big thank you to everyone who has helped build the ecosystem",
    "that Unison relies on! 🙏",
    "",
    "If you notice any broken links or know of a dependency that should",
    "be listed here, please [file a ticket](https://github.com/unisonweb/unison/issues/new/choose).",
    "",
    "This file was generated using [unisonweb/credits-generator](http://github.com/unisonweb/credits-generator).",
    "",
    "### Listing ",
    "These are listed in alphabetical order.",
    ""
    ]

go :: Shell Line -> Shell Line
go lines = do
    line <- limitWhile isOk (lines >>= skipFirst)
    (packageName, licenseName) <- select $ match linePat (lineToText line)

    -- todo: need to do a bit more work to get the actual license URL
    let hackageUrl = "https://hackage.haskell.org/package/"
        packageUrl = hackageUrl <> packageName
        licenseUrl = packageUrl <> if "ListLike" `T.isPrefixOf` packageName
                                   then "/src/COPYRIGHT"
                                   else "/src/LICENSE"
    pure . unsafeTextToLine $
      "| " <> link packageUrl packageName <> " | " <> link licenseUrl (cleanup licenseName) <> " |"
  where
  cleanup "BSD-3" = "BSD3"
  cleanup "BSD-2" = "BSD2"
  cleanup l = l
  link url name = "[" <> name <> "](" <> url <> ")"
  linePat = do
    segs <- segment `sepBy` "."
    "..."
    license <- chars
    let pkg = T.intercalate "." segs
    pure (pkg, license)
    where
    segment :: Pattern Text
    segment = star (alphaNum <|> char '-')
  skipFirst "Found stack.yaml..." = mempty
  skipFirst s = pure s
  isOk "-----" = False
  isOk _ = True
