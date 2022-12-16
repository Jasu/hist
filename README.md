# A fast working-directory history and recommender for zsh

Command-line tool and zsh plugin for ranking and suggesting
recently visited directories by frecency.

Has a frontend that uses [Rofi](https://github.com/davatorium/rofi):

![Rofi frontend searching through history](https://raw.githubusercontent.com/Jasu/hist/main/misc/rofi_hist_screenshot.png)

Licenced under CC0 - **but be warned - TDB, which this depends on,
is licensed under LGPL.**

[![License: CC0](https://licensebuttons.net/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

## Limitations

Since this is a work-in-extremely-slow progress and rather
quirky, I put the (likely non-exhaustive) limitations section
first, to avoid wasting other developers time.

  - Only [Rofi](https://github.com/davatorium/rofi) support.
    This is extendable, via `zstyle` configuration, but only
    a Rofi frontend exists now.
    **Note: Since Rofi is a graphical program, this prevents
    the program from working over SSH.**
  - This has only ever been compiled and run on Arch Linux, and
    only by me (at least at the time of writing). Thus, it has 
    plenty of bugs for others to find.
  - Currently, this only compiles on Clang with the LLD linker.
    This has little to do with actual requirements, it is a side
    effect of trying to squeeze the executable as small as possible.
    (The executable is 23kB, but the size saving is obviously not
     worth forcing a specific compiler or having to maintain separate
     configurations for various toolchains)
  - Only knows about directories - i.e. there is no support for
    tracking most frecently run programs. I might add it some day but
    no promises.
  - No installer / packaging - you're stuck with copying files manually.


## Implementation

Written in C (C2X), using [TDB](https://tdb.samba.org/) as the database.
Requires Clang and `lld`, due to optimizing for the sake of optimization.

The database structure consists of four queues that each contain entries
for frecently visited directories. Every time a directory is visited, it
gets a queue-specific constant added to its frecency. And when time passes,
the frecencies of directories decay exponentially - and with a different
half-life in each queue. Thus, if you only visit a directory once, it
will jump to the top of the fastest queue and will show in your history
search instantly - but the other queues tend to favor frequency over
recency. so the oft-visited directories will stay in the list even though
they haven't been used in a while, but the recently but only once used
directory will disappear in about an hour.

The database consists of a single file (in `~/.local/share/hist/db.tdb`),
managed with the TDB library. This supports simultaneoius access on a
local machine, since TDB uses transactions. TDB's file-based locking
is used instead of the newer locking mechanism based on shared Mutexes.
This is because the command-line program only does a small task to write
or read the database and exits - the runtime check for robust mutex support
in TDB would spawn subprocesses to verify that the mutexes work. This would
invariably take more time than the actual task in this case.

Since losing the directory recommendations would only be a mild
inconvenience, syncing the transactions to disk is disabled.
Thus, power failure could easily crash the database - but it will
not randomly crash due to concurrent access. So far, it's been uncrashed
much longer than the various SQLite based history recommenders I've
tried.

# Compilation / installation

  0. Clone this repository.
  1. Install dependenciese:
      - `clang`,
      - `lld` (LLVM's linker - this might come with Clang on some distributions).
      - `meson` and `ninja`,
      - `tdb`
      - `xxhash`
      - `rofi`
  2. Create a `build` directory under the repository root, and enter it:
    ```
    mkdir build
    cd build
    ```
  3. Run Meson **with clang set as its default compiler**:
     ```
     # In the build directory:
     env CC=clang meson ..
     ```
  4. Build with Ninja:
     ```
     # In the build directory:
     ninja
     ```
  5. Assuming that you want to keep the compilation artifacts
     in the build dir (there is no packaging yet), add configuration
     to your `.zshrc`:
     ```
     source "$HOME/hist/_show_rofi.zsh"
     source "$HOME/hist/hist.zsh"

     # To change the menu from the default Rofi, you the frontend
     # can be set to whatever command here. The output is not a simple
     # DMenu though - at least disable the markup option to avoid having
     # to deal with Pango markup.
     zstyle ':hist:*' menu "rofi_select --markup --prompt Cd:"
     zstyle ':hist:*' markup 1
     # Ignore the home directory - it would likely show on the top every time
     zstyle ':hist' ignored_dirs "$HOME"
     # Shorten /home/YOUR-NAME-HERE/ to ~/
     zstyle ':hist:*' shorten_home 1

     # Bind the history selector to some key (here bound to Ctrl-G):
     bindkey "^[G" hist-cd
     ```

  6. See what breaks, and open issues or pull requests!

