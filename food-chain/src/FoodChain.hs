module FoodChain (song) where
import Data.List
import Control.Applicative

bases :: [String]
bases = fmap unlines $ reverse $ init $ tails  [
            "She swallowed the cow to catch the goat.",
            "She swallowed the goat to catch the dog.",
            "She swallowed the dog to catch the cat.",
            "She swallowed the cat to catch the bird.",
            "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
            "She swallowed the spider to catch the fly.",
            "I don't know why she swallowed the fly. Perhaps she'll die."
            ]

song :: String
song =
    let
    toZip :: [String]
    toZip = unlines <$> [
        [
            "I know an old lady who swallowed a fly."
        ],
        [
            "I know an old lady who swallowed a spider.",
            "It wriggled and jiggled and tickled inside her."
        ],
        [
            "I know an old lady who swallowed a bird.",
            "How absurd to swallow a bird!"
        ],[
            "I know an old lady who swallowed a cat.",
            "Imagine that, to swallow a cat!"
        ],[
            "I know an old lady who swallowed a dog.",
            "What a hog, to swallow a dog!"
        ],[
            "I know an old lady who swallowed a goat.",
            "Just opened her throat and swallowed a goat!"
        ],[
            "I know an old lady who swallowed a cow.",
            "I don't know how she swallowed a cow!"
        ]]
    finalPhrase = [
            "I know an old lady who swallowed a horse.",
            "She's dead, of course!"
        ]
    zipped:: [String]
    zipped = (getZipList $
            (++) <$> (ZipList toZip) <*> (ZipList bases))
            ++ finalPhrase
    in
        unlines zipped
