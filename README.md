# Gemini & Titan mode

This uses [Elpher](https://thelambdalab.xyz/elpher/) to browse Gemini
sites and [Gemini Mode](https://git.carcosa.net/jmcbray/gemini.el) to
*edit* them using the [Titan](https://transjovian.org:1965/titan)
protocol.

## Quickstart

I would suggest the following:

1. install the `elpher` package from MELPA
2. install the `gemini-mode` package from MELPA
3. install this package (`gemini-write` is not on MELPA)

Add the following to your init file:

```
;;; add gemini-write support to `elpher' and `gemini-mode'
(define-key elpher-mode-map (kbd "e") 'gemini-write-text)
(define-key elpher-mode-map (kbd "w") 'gemini-write-file)
(define-key gemini-mode-map (kbd "C-c C-c") 'gemini-write)
```

Once you have all three packages installed, use `e` to edit a Gemini
page on a site that has Titan enabled. Use `C-c C-c` to save.
Customize `elpher-gemini-tokens` to set passwords, tokens, or whatever
you need in order to edit sites.

## Where can I use it?

The known sites that have Titan enabled:

* [Emacs Wiki](gemini://emacswiki.org/), about Emacs
* [Campaign Wiki](gemini://emacswiki.org/), about RPG campaigns
* [Community Wiki](gemini://communitywiki.org/), about community building and wikis
* [The Transjovian Council](gemini://transjovian.org/), about Gemini and Titan

## Installing from Source

If you're installing all three packages from source, here's an example
of how to set it all up, given that I keep all my sources in
“/home/alex/src”.

```
(add-to-list 'load-path "/home/alex/src/elpher")
(autoload 'elpher "elpher" "Gopher and Gemini client" t)

(add-to-list 'load-path "/home/alex/src/gemini.el")
(autoload 'gemini-mode "gemini-mode" "Gemini Mode" t)

(add-to-list 'load-path "/home/alex/src/gemini-write")
(autoload 'elpher-edit "gemini-write" "Edit a Gemini page." t)

;; make sure "e" can be used to edit raw pages
(eval-after-load "elpher" '(load-library "gemini-write"))
```

## Tokens

`gemini-write` can pull tokens from the `elpher-gemini-tokens` alist,
or from the `auth-source` library. A gemini-write token entry in
“~/.authsource” should contain the host and the port, which by default
is 1965, along with the token in the `password` field, like this:

```
machine example.com port 1965 password example-password
```
