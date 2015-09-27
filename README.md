# gl-type
Simple Freetype font rendering in OpenGL with a single vertex buffer and texture. 

Only horizontal, left-to-right rendering is currently supported.

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install gl-type`.

## Requirements
- freetype
- gl-utils
- miscmacros 
- opengl-glew

## Documentation
    [procedure] (load-face PATH SIZE [char-set: CHAR-SET] [mode: MODE])

Load the font face given by the [Freetype-openable file](http://freetype.org/) pointed to by `PATH` with a size of `SIZE` pixels. `CHAR-SET` is the [SRFI 14](http://srfi.schemers.org/srfi-14/srfi-14.html) character-set that should be loaded, defaulting to `char-set:graphic`. Any non-graphical characters in the set will waste space. `MODE` is the mode in which the font should be rendered and should be one of `#:normal` (the default) – for normally hinted, anti-aliased rendering – or `#:mono` – for monochrome (aliased) rendering. Returns a `face` record.

    [record] face
    [procedure] (face? FACE)
    [procedure] (face-atlas FACE)
    [procedure] (face-char-set FACE)
    [procedure] (face-height FACE)

The record type returned by `load-face`.

    [procedure] (string-mesh STRING FACE [line-spacing: SPACING] [max-width: MAX-WIDTH] [x: X] [y: Y] [mesh: MESH])

Render the given `STRING` into a [gl-utils mesh](http://api.call-cc.org/doc/gl-utils/mesh) with the provided `FACE`. `SPACING` is multiplied by the standard line advancement – a smaller spacing results in less space between successive lines – defaults to `1.3`. If `MAX-WIDTH` is provided, the string will be word-wrapped such that it does not exceed the given width. `X` and `Y` may be given to provide an offset to the rendered string’s origin. `MESH` may be supplied in order to reuse a mesh previously created with `string-mesh`. When `MESH` is supplied, the number of graphical characters (non-whitespace characters in the char-set of `FACE`) in `STRING` must be equal to or less than the number of graphical characters in the string used to create `MESH`.

Strings are rendered with the upper-left corner of the text at the origin, with the dimensions of the mesh set in pixels. The mesh that is created has vertex attributes `position` – a 2 element `#:float` – and `tex-coord` – a 2 element normalized `#:ushort`, and `#:ushort` indices.

    [procedure] (string-width STRING FACE)

Returns the width, in pixels, of the `STRING` if it were rendered with `FACE`. Any non-graphical characters other than `#\space` will be ignored.

    [parameter] (pixel-density-ratio)

Used to control the resolution at which fonts are rendered, defaulting to 1: each pixel unit corresponds to one display pixel. For most “Retina” displays a value of 2 should be used: each pixel unit corresponds to two display pixels.

## Examples
See the [examples directory](https://github.com/AlexCharlton/gl-type/tree/master/examples).

## Version history
### Version 0.3.0
27 September 2014

- Add support for high density displays

### Version 0.2.0
23 December 2014

- `string-mesh` accepts keyword `mesh` argument

### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/gl-type).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD
