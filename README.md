# Gemini & Titan mode

This uses [Elpher](https://thelambdalab.xyz/elpher/) to browse Gemini
sites and [Gemini Mode](https://git.carcosa.net/jmcbray/gemini.el) to
*edit* them using the [Titan](https://communitywiki.org/wiki/Titan)
protocol.

Note: This needs *Elpher* 2.8. Maybe you need to get the latest
version from the author's repository for it to work.

Once you have all three packages installed, use `e` to edit a Gemini
page on a site that has Titan enabled. Use `C-c C-c` to save.
Customize `elpher-gemini-tokens` to set passwords, tokens, or whatever
you need in order to edit sites.

The known sites that have Titan enabled:

* [The Transjovian Council](gemini://transjovian.org)

If you're installing all three packages from source, here's an example
of how to set it all up:

```
(add-to-list 'load-path "/home/alex/src/elpher")
(autoload 'elpher "elpher" "Gopher and Gemini client" t)

(add-to-list 'load-path "/home/alex/src/gemini.el")
(autoload 'gemini-mode "gemini-mode" "Gemini Mode" t)

(add-to-list 'load-path "/home/alex/src/gemini-write")
(autoload 'elpher-edit "gemini-write" "Edit a Gemini page." t)
(define-key elpher-mode-map (kbd "e") 'elpher-edit)
```
