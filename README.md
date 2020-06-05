# Gemini+Write mode

This uses [Elpher](https://thelambdalab.xyz/elpher/) to browse Gemini
sites and [Gemini Mode](https://git.carcosa.net/jmcbray/gemini.el) to
edit them.

Note: I needed the latest *Elpher* from the author's repository for it
to work well with Gemini!

Once you have all three packages installed, use `e` to edit a Gemini
page on a site that has Gemini+Write enabled. Use `C-c C-c` to save,
use `C-c C-k` to cancel. Customize `elpher-gemini-tokens` to set
passwords, tokens, or whatever you need in order to edit sites.

Currently this only works for [Alex Schroeder's Blog](gemini://alexschroeder.ch),
I guess.

If you're installing all three packages from source, here's an example
of how to set it all up:

```
(add-to-list 'load-path "/home/alex/src/elpher")
(autoload 'elpher "elpher" "Gopher and Gemini client" t)

(add-to-list 'load-path "/home/alex/src/gemini.el")
(autoload 'gemini-mode "gemini-mode" "Gemini Mode" t)

(add-to-list 'load-path "/home/alex/src/gemini-write")
(autoload 'gemini-write-init "gemini-write" "Initialize Gemini Write Mode" t)

(add-hook 'elpher-mode-hook 'gemini-write-init)
```
