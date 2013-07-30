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

* [guile-lib](http://www.nongnu.org/guile-lib/)

* Only necessary if building from github
  * [Noweb](http://www.cs.tufts.edu/~nr/noweb/) 

  * pdflatex

Building from a release (easy)
------------------------------

    $ wget https://github.com/shanecelis/emacsy/releases/download/v0.1.1/emacsy-0.1.1.tar.gz
    $ tar xfz emacsy-0.1.1.tar.gz
    $ cd emacsy-0.1.1
    $ ./configure
    $ make

Building from github (harder)
-----------------------------

    $ git clone https://github.com/shanecelis/emacsy.git
    $ cd emacsy/example
    $ git clone https://github.com/shanecelis/hello-emacsy.git
    $ git clone https://github.com/shanecelis/emacsy-webkit-gtk.git
    $ cd ..
    $ ./autogen.sh
    $ ./configure
    $ make


Running 
-------

Run the [minimal example
program](http://gnufoo.org/emacsy/minimal-emacsy-example.pdf) and the
[barebones webkit
browser](https://github.com/shanecelis/emacsy-webkit-gtk) example.

    $ make run

![minimal example screenshot]()

<a href="https://github.com/shanecelis/hello-emacsy#readme"><img src="https://raw.github.com/shanecelis/emacsy/master/support/images/screenshot-small.png"></a>

<a href="https://github.com/shanecelis/emacsy-webkit-gtk#screenshot"><img src=https://raw.github.com/shanecelis/emacsy-webkit-gtk/master/support/image/emacsy-webkit-gtk-screenshot-1-small.png></a>

Reading 
-------

This is a literate program, so you can read it.  

    $ make show-doc

The literate documents are bundled in the distribution as `emacys.pdf`
and `hello-emacsy-paper.pdf`.

Running Tests
-------------

    $ make check

TODO
----

Lots to do.  See the `todo.org` file.

License
-------

Emacsy is available under the GNU GPLv3. See the bundled LICENSE file
for details.  
