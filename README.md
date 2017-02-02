Collage
=======

Make photo collages. I needed this in order to prepare a printed photo book,
without resorting to a web interface, maybe will it be useful to other people.

Usage
-----
Basically, you run the program as
```
collage my-collage
```
where `my-collage` is a text file containing text such as
```
=__h__hv
img1.jpg
img2.jpg
img3.jpg
img4.jpg

=__v_ph
img1.jpg
img2.jpg
img3.jpg
```
The first line describes the format of the collage as follows:
- such a description always starts with `=`,
- `_` means take an image, `_` means take another image and `h` means group the two previous horizontally,
- `v` means group the two previous vertically,

and the four required files are listed below. It will thus make a 2×2 square of photos. The second collage is similar, the new operation `p` means that an image is "prioritary", i.e. it should take as much space as available (while keeping aspect ratio).

Some more examples can be found in the [test](test) directory.

Options
-------
The currently supported options are
- `--display` to immediately show the result
- `--draft` to generate a lower resolution image (which is faster)

Supported features
------------------
Right now the program contains basically only what I needed, for instance the output format is hardcoded A4 at 600 dpi. Enhancing it should not be too hard and you can submit [issues](https://github.com/smimram/collage/issues) or better [pull requests](https://github.com/smimram/collage/pulls).

License
-------
The code is licensed under GPLv2.
