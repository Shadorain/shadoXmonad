# ShadoXmonad

## Window manager of the shadows.

With a beautiful colorscheme, and many configuration options, shadoXmonad
has a nice set of weapons for you to fight the shadows and shine your light
of efficiency and power!

## Install

Super simple install. You do not even need `xmonad`/`xmonad-contrib`/`xmobar`
as dependencies. They will all be built locally for you so that:

1. You aren't in haskell dependency hell on your root system
2. You get the power of bleeding edge xmonad with many features!

One thing you may need as a dependency before hand is this package which may
be only if you are on Void Linux but it symlinks libraries that are needed:
`ncurses-libtinfo-devel`. Make sure to install this before you run setup

Simply clone the repository and run the setup script provided from your xmonad
directory.

```bash
❱  git clone https://github.com/Shadorain/shadoXmonad ~/.xmonad
❱  cd ~/.xmonad/      # default location
❱  chmod +x ./setup
❱  ./setup
❱  xmonad --recompile # should compile with no errors
```

You may receive a warning that you need to add `~/.local/bin` to your path.
This is so that the `build` script can run `xmonad` without a full path. 
To add this to your path you simply can add `export PATH=$PATH:~/.local/bin/`
to your sourced initialization shell script (ie: `~/.bashrc`, `~/.zshrc`).

If the setup script and compiling exits with no errors then you are set
and ready to use shadoXmonad!
If you are running a display manager, make sure to add it as an entry to it.
If you are not like me, then simply add `exec xmonad` to your `~/.xinitrc` file
and this will run on startup to load into `xmonad`.

Hope you all enjoy. I plan to add a list of keybinds and features of my window
manager here sometime in the future. For screenshots, check out my main page here:
[| Shadorain's Github |](https://github.com/Shadorain) and scroll down some :D!
