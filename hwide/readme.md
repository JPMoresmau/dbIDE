#Introduction
The Haskell Web IDE is a proof of concept of using ThreePenny-GUI as the UI library, so most of the code runs on the web server and manipulates the HTML. The source code viewer is CodeMirror.
It saves us from having to require a Haskell UI library as everybody has a decent browser these days. The "web server" of course runs locally, this is not meant to be a cloud development environment.

# Current State
This is only a proof of concept, so don't use for serious work, but feel free to fork and extend! See the HACKING file for some information.
Start hwide in the directory you want to use as your root folder. It can contain several Cabalized projects or be the root of one project, as you wish.
There is a file browser that lets you see the files from your root folder, and also the workspace of hwide, which is where the sandbox and log files end up.
Currently, you can create, open files, and save them. Upon save, cabal configure and build is run on the project, and errors are shown at the top. You can click on an error and it will create a gutter mark on the first line of the error. This is still in its infancy, for example it doesn't work very well if you're not already on the proper source file.
There is no code mirror mode for Cabal files (yet, feel free to volunteer) so they open as text. I've just added the yaml mode because the configuration file for hwide is in yaml.
The configuration file can contain things like the cabal path.

# TODO
* Fix gutter handling
* Scratch file support: when you start HWide you see an untitled file you can edit, because I didn't want to show nothing. It should be possible to do save as on that buffer, or run it to use it as some worksheet/interpreter.
* Outline support: integrate haskell-src-exts to get a list in a drop down at the top of all the defined functions in the module, etc
* Autocompletion support: use ideas from scion-browser and other tools to build a coherent database of symbols to use with autocompletion, with doc
* Integration will other tools like hoogle, hlint, stylish-haskell, etc.
* Everything to have a full fledge IDE! 

Have fun!



