---
author: Vincent Hanquez
title: "code tokenization"
tags: parsec,parser,haskell
date: January 10, 2010
---

I wanted to develop a very small tokenizer, for the purpose of coloring code. I
started with a very naive approach or cuttings lines and words, the way i
probably would do it poorly in other language. As expected the approach is so
poor, that more than half of what I wanted to highlight were not properly
atomized, since **words** only works on whitespaces boundaries, which means
that keywords or digits with a punctuation mark just afterwards were wrongly
"worded".

I thought about a simple solution for this problem. 2 options were
there: regexes, or a parser. In a past, i would probably choose the regexes
option, even though regexes are only good up to a certain point, and are very
hard to read after a while. It would certainly still holds in another language
where parser means to use a different tool (bison for C, ..), with a different
syntax (BNF), with lots of mind trick in the tool to make the grammar
sane to the underlying algorithm.

This time is now gone, parsec is so easy to use, integrate and develop that
even regexes are painful.

First I want to have something really simple. something that takes number and symbol
and have them properly labeled. the data type would be something like:

```haskell
    data Atom =
              Symbol String
            | Number String
            | Other Char
```

The `Other` constructor is there to catch all characters that were not identified
either as a symbol or as a number. The first thing is defining the highlevel
entry point such as:

```haskell
    atomize = manyTill (choice [ symbol, number, other ]) eof
```

It means `atomize` is a parser that do: until (`manyTill`) we reach end of file
(`eof`), choose (`choice`) between the symbol parser (`symbol`), or the number parser
(`number`) or the other parser (`other`). choice will iterate over the parser list
until one succeed.

Let's start by defining the easiest parser: `other`. This parser is here to
catch any character. it's not suppose to fail, since it accept anything, and return
an other atom. The definition is extremely simple:

```haskell
    other = anyChar >>= return . Char
```

Then it's time to define the `symbol` and `number` parser. Because both parsers should fail if
there are not parsing a symbol or a number (respectively), you need to wrap the parser in a
`try` function. It tells the parser to save the input and if the parser following
try fails, just rewind the input to where it was saved.

So a symbol is one or more characters that have those specific constraints: the
first character need to be an alpha character **a-z** and **A-Z** and we also allow \_.
then following characters, if they exists, can be alpha character, underscore
and digits.

```haskell
    symbolFirstChar = [ 'a'..'z' ] ++ [ 'A'..'Z' ] ++ [ '_' ]
    symbolChar = symbolFirstChar ++ [ '0'..'9' ]

    symbol = try $ do
    	f <- oneOf symbolFirstChar
    	ending <- many (oneOf symbolChar)
    	return $ Symbol (f : ending)
```

And the `number` is even simpler, it just one or more digits characters.

```haskell
    number = try $ do many1 (oneOf [ '0'..'9' ]) >>= return . Number
```

That's it ! You have a very simple lexer in 12 lines:

```haskell
    data Atom =
              Symbol String
            | Number String
            | Other Char
    atomize = manyTill (choice [ symbol, number, other ]) eof

    other = anyChar >>= return . Char
    number = try $ do many1 (oneOf [ '0'..'9' ]) >>= return . Number
    symbol = try $ do
    	f <- oneOf symbolFirstChar
    	ending <- many (oneOf symbolChar)
    	return $ Symbol (f : ending)

    symbolFirstChar = [ 'a'..'z' ] ++ [ 'A'..'Z' ] ++ [ '_' ]
    symbolChar = symbolFirstChar ++ [ '0'..'9' ]
```

At this point you might say, alright but with regex it would probably fit in this amount
of line too, which is true, but I think it misses the point that not only it's as simple
to parse simple tokens like those, it also possible to extends it easily and naturally,
to complex cases, because the regex state machine is not able to
really understand things that requires a proper parser. For example the following example
mix comment and string. the comment beginning tag appears in the string, so it should not
be taken as a beginning of a comment:

```C
    	printf("C comment looks like: /*\n");
    	printf("followed by the closing tag: */\n");
```

It's hard for a regex state machine to do the right thing in recursive cases.
