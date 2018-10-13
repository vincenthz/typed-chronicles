---
author: Vincent Hanquez
title: Using QuickCheck
tags: haskell,QuickCheck,testing,tutorial
date: December 2, 2010
---

Quickcheck is a framework to generate arbitrary input to test properties.

<!--more-->

There's 2 mains part in the framework:

* the arbitrary framework: encode rules on how to generate your inputs.
* the properties framework: encore what properties to check.

note I'm using QuickCheck v2.4 in examples, but it mostly apply to QuickCheck
v2 in general.

Generating input
----------------

First, the arbitrary class is the backbone of generating data. it's define as such:

    class Arbitrary a where
            arbitrary :: Gen a
            shrink :: a -> [a]

Quickcheck will generate randomized data using the Arbitrary class, and by default it comes with most basic types already instanciated. see [Hackage Arbitrary page](http://hackage.haskell.org/packages/archive/QuickCheck/2.4.0.1/doc/html/Test-QuickCheck-Arbitrary.html) for the full documentation. What left to do is fill the blank between your types and the basic types.

For example defining arbitrary for a simple Foo data type:

    data Foo = Foo Int Int
    
    instance Arbitrary Foo where
             -- we get 2 arbitrary ints and create a Foo type from it
             arbitrary = do
                  i1 <- arbitrary
                  i2 <- arbitrary
                  return (Foo i1 i2)
             -- idiomaticly it's easier to write
             arbitrary = liftM2 Foo arbitrary arbitrary

Said differently, you combine multiple arbitrary instances for each of your basic types composed in the type you want to define.

To help you define your instances or generator, you get the following function already defined:

* arbitrarySizedIntegral : generate an arbitrary integral value.
* arbitraryBoundedIntegral : generate a bounded arbitrary integral value.
* arbitrarySizedBoundedIntegral : same as above, but more likely to do small numbers.
* arbitrarySizedFractional : generate an arbitrary fractional type value.
* arbitraryBoundedRandom : generate an arbitrary value for a bounded and random type.
* vector : generate an arbitrary list of specific size

    -- a uuid has to be 16 word8
    data UUID = UUID [Word8]
    
    instance Arbitrary UUID where
         arbitrary = liftM UUID (vector 16)

Now to complete the generators, there's even more helpers available in the Gen module. [Hackage Gen page](http://hackage.haskell.org/packages/archive/QuickCheck/2.4.0.1/doc/html/Test-QuickCheck-Gen.html)

We can use the sample function to test the output of a generator, for example for the previous UUID example:

    #> let a = arbitrary :: Gen UUID
    #> sample a
    UUID [0,0,1,0,1,0,0,0,1,0,1,1,0,1,0,1]
    UUID [0,0,1,0,0,0,1,1,1,0,0,1,0,1,0,1]
    UUID [2,1,1,2,2,2,0,0,0,1,0,2,0,0,2,0]
    UUID [2,3,0,1,4,3,1,3,2,0,4,2,3,4,4,4]
    UUID [8,7,6,8,8,5,7,4,5,4,6,5,2,3,6,0]
    UUID [1,2,6,9,2,15,1,3,11,12,9,6,7,8,5,13]
    UUID [6,13,11,7,10,5,16,15,13,7,6,15,16,12,6,15]
    UUID [17,32,9,3,23,23,31,12,26,0,22,24,28,27,13,0]
    UUID [32,18,1,40,37,51,18,39,43,0,38,39,51,0,15,43]
    UUID [125,99,2,75,43,1,15,119,95,23,128,30,31,104,39,89]
    UUID [126,40,48,195,99,249,17,191,161,53,59,7,170,202,150,126]

a second run will have a different result. Quickcheck use the random generator for the input.

All generator can be combined and tweaked, using combinators:

* sized : very useful to generate recursive data that are not infinite.
* suchThat : generate data that have specific requirements.
* choose : same as suchThat, but just specify a lower and upper bound on a data set.
* oneOf : combine multiple generator in a list, and alternate randomly between all those generator to generate data.
* frequence : same as oneOf, but instead of choosing randomly the generator, each generator have a frequency associated with them.
* elements : take a list of value and get value from the list randomly. this is useful to simple algebraic type.

For example:

    -- generate uuid that start with 1.
    uuidS1Gen :: Gen UUID
    uuidS1Gen = suchThat a (\(UUID l) -> head l == 1)
    
    -- generate uuid that end with 0.
    uuidE0Gen :: Gen UUID
    uuidE0Gen = suchThat a (\UUID l) ->(head $ drop 15 l) == 1)
    
    -- generate uuid that start either by 0, or end by 1
    uuidGen :: Gen UUID
    uuidGen = oneOf [ uuidS1Gen, uuidE0Gen ]
    
    -- using elements
    data T = Foo | Bar
    instance Arbitrary T where
         arbitrary = elements [ Foo, Bar ]
    
Keep in mind, that the data exhaustiveness quality of your tests are highly dependent on the quality of your Arbitrary instances.

Writing properties
------------------

now we need to encode to QuickCheck which properties it need to check. see [Quickcheck Property](http://hackage.haskell.org/packages/archive/QuickCheck/2.4.0.1/doc/html/Test-QuickCheck-Property.html) for the whole documentation.

Property as their simpliest is taking one of your generated value, and returning a bool. if True, the property holds.

for example:

    stringRevRevIdentity :: String -> Bool
    stringRevRevIdentity :: reverse (reverse s) == s

Behind the scene, every property need to be an instance of Testable. the previous pattern (a -> Bool) is already defined as such by the following combinaison of instance:

    Testable Bool
    (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)

As QuickCheck is a combinator library, you can combine your testable properties into bigger properties. There's lots of helpers to that end:

* ==> : define implication. if the left hand of the operator is true, *then* quickcheck will test that the property on the right is true.
* within : define a timeout for an operation to complete. this is useful is you want to encode specific time bound for the function testing requirements.
* forAll : explicitely combine a generator with a property. this is useful when you need to use a generator with specific values instead of the general generator.
* .&&. and conjoin : make a property from multiple properties that need to hold. it's visually equivalent to the and boolean operator.
* .||. and disjoin : make a property from multiple properties that at least one of them need to hold. it's visually equivalent to the or boolean operator.

some example using the helpers

    -- generate only even number
    numEven :: Gen Int
    numEven = suchThat arbitrary (\i -> even i)
    
    -- check for all even number generated, that dividing by 2 and then multiplying by 2 is an identity
    prop :: Property
    prop = forAll numEven (\i -> (i `div` 2) * 2 == i)

to see more examples, the following package contains a big list of properties checking:

* [text package properties](http://github.com/bos/text/blob/master/tests/Properties.hs)

Bringing it all together
------------------------

The last bit is taking those generators and properties and handing them to QuickCheck for testing.

The main function here is quickCheckResult. it takes a property and give back a result type.

    -- this is an example property. checking that any generated uuid are of length 16.
    my_property :: UUID -> Bool
    my_property (UUID l) = length l == 16
    
    main = quickCheckResult my_property >> return ()

There's other functions to test property, but they are just variant of each others, letting you configure quickcheck differently for example. verboseCheck and its variants are quite useful to check that your generator are generating data as expected.

I didn't cover every corner of QuickCheck, but hopefully it's good enough to get you started with QuickCheck, and more importantly raise the quality of your code.

To infinity and beyond.

