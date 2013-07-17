Emacsy
======

Emacsy is an embeddable Emacs-like library for [GNU Guile
Scheme](http://www.gnu.org/software/guile/).  It was a [kickstarter
project](http://www.kickstarter.com/projects/568774734/emacsy-an-embeddable-emacs/?ref=kicktraq).
It is now a [Google Summer of Code 2013
project](https://google-melange.appspot.com/gsoc/proposal/review/google/gsoc2013/shanecelis/1).
I will be working with Ludovic Courtès from the [GNU
Project](http://www.gnu.org/gnu/thegnuproject.html).  Keep abreast of
its development by watching this repository or following me on twitter
[@shanecelis](https://twitter.com/shanecelis).

WARNING
-------

This project is currently in development.  It is as alpha as can be.
Not meant for general consumption yet.  Contributors, welcome.

Dependencies
------------

* [GNU Guile Scheme 2.0](http://www.gnu.org/software/guile/)

* [Noweb](http://www.cs.tufts.edu/~nr/noweb/) - not necessary if building from [distributed source](https://github.com/shanecelis/emacsy/releases)

Building from github
--------------------

    $ git clone https://github.com/shanecelis/emacsy.git
    $ cd emacsy
    $ git submodule update
    $ ./autogen.sh
    $ ./configure
    $ make

Building from a release
---------------------

    $ wget https://github.com/shanecelis/emacsy/releases/v0.1.0/641/emacsy-0.1.0.tar.gz
    $ tar xfz emacsy-0.1.0.tar.gz
    $ cd emacsy-0.1.0
    $ ./configure
    $ make

Running 
-------

Run the [minimal example
program](http://gnufoo.org/emacsy/minimal-emacsy-example.pdf).

    $ make run

![screenshot](https://raw.github.com/shanecelis/emacsy/master/support/images/screenshot-small.png)

Reading 
-------

This is a literate program, so you can read it.  

    $ make preview

The literate documents are bundled in the distribution as `emacys.pdf`
and `hello-emacsy-paper.pdf`.

Running Tests
-------------

    $ make check

TODO
----

Lots to do.  

- [ ] First TODO, collect up all the TODOs.

License
-------

Emacsy is available under the GNU GPLv3. See the bundled LICENSE file
for details.  
