# scbmp
A pure R7RS scheme library for bmp image format

![Example](examples/example.bmp)

```scm
(letrec ((width 100)
	 (height 100)
	 (bpp 24)
	 (dpi 300)
	 (image (image-make width height 255 0 255 bpp dpi)))
  (image->file image "example.bmp" '()))
```

## TODO:
- [ ] add simple procedures for drawing shapes, transformations, etc.
- [ ] export library using snow
- [ ] render some fractals?
