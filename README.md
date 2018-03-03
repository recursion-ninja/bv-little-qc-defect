## Minimal Woring example of a QuickCheck anomoly

    cabal sandbox init
    cabal install --enable-library-profiling --enable-executable-profiling --enable-tests
    cabal ./dist/dist-sandbox-*/build/test-suite/test-suite +RTS -p -RTS
    ^C
    head test-suite.prof

Which results in the following output, note the loop in problemFunction:

    Fri Mar  2 20:05 2018 Time and Allocation Profiling Report  (Final)

       test-suite +RTS -p -RTS

       total time  =        4.48 secs   (4482 ticks @ 1000 us, 1 processor)
       total alloc =  86,863,456 bytes  (excludes profiling overheads)

    COST CENTRE     MODULE            %time %alloc

    problemFunction Data.ExampleType   99.9   96.7