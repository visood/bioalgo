Bio-informatic Algorithms
=========================

The continuing quest to learn algorithms and data structure used in bioinformatics, and their implementation in Haskell.

Haskell Stack : How to build a project with Stack
-------------------------------------------------

`Stack is a cross-platform program for developing Haskell projects. It is` `aimed at Haskellers both new and experienced.` Stack can be used to :

-   Install GHC automatically, in an isolated location.
-   Install packages needed for your project.
-   Build your project.
-   Test your project.
-   Benchmark your project.

### Start your new project :

Create a new directory containing all the needed files to start a project,

``` example
stack new my-project 
cd my-project
```

Download the compiler if needed, in an isolated location (defaults to `` `~/.stack` ``). The isolated location will not interfere with any system-level installation. Use `stack path` for information on installation paths.

To build a minimal project :

``` example
stack build
```

To execute :

``` example
stack exec my-project-exe
```

To install an executable use stack,

``` example
stack install <package-name>
```

To launch a REPL :

``` example
stack ghci
```

### Project Structure

The project has the following directory structure:

-   `data` to hold the (test) data
-   `src` to hold the source code
-   `test` to hold the test code
-   `app` to hold Main

Under `src` we have a `lib` to hold the library :

-   `Util` to hold general utilities, for example `Command.hs` used for CLI.
-   `Genome` to hold genomic functionality
-   `Examples` to hold examples
-   `Chapters` to hold chapter by chapter code examples

~Test~ code has (almost) identical directory lay-out.

I am stuck with the same ideas, have not been able to break new ground.

Plotting in Haskell
-------------------

Plotting is essential in any scientific field. However Haskell is not particularly helpful in producing interactive plots. We can take this as an opportunity in scripting our plots --- this requires learning more about Haskell charts. Interactive plotting readily becomes hectic hacking away to get the plot right. We can do the same if we have to script the plot -- however we will have to think more and the end-result of our efforts will be preserved in the script.

### <span class="todo TODO">TODO</span> &lt;2017-01-06 Fre&gt; center the GC diff plot around its minima.

Faster pattern finding
----------------------

While we have used dictionaries (Haskell Map) to implement pattern finding algorithms, Compeau and Pevzner suggest using Frequency Arrays. Frequency Arrays work with mapping the four (can be five if 'N' is allowed) nucleotides to numbers, and using a positional factor to convert strings of nucleotides to numbers. For a pattern of length *k*, there are 4<sup>*k*</sup> possible strings, and hence the numbers will range from 0 to 4<sup>*k*</sup> − 1. This number will be prohibitively large even for moderate values of *k*. However for the problem of locating the origin of replication, *k* is typically smaller than 10, and the frequency array will be of the order of a million. We can implement the same pattern search algorithms using the frequency array under their own module.
