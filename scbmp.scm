;;
;; (ricksterhd123 scmbp)
;; A simple bmp image module in R7RS scheme
;;

;; recursive reduce
(define reduce
  (lambda (op init lisp)
    (if (null? lisp)
	init
	(reduce op (op init (car lisp)) (cdr lisp)))))

;; TODO bitshift map method
;; Converts arbitrary number to little-endian bytevector
(define num-to-bytevector
  (lambda (n)
    (letrec
	((max-index (floor (log n 256)))
	 (num-to-bytevector-iter
	  (lambda (n index vec)
	      (if (<= index 0)
		  (bytevector-append vec (bytevector n))
		  (letrec ((qr (cdr (truncate/ n (expt 256 index))))
		     (q (exact (car qr)))   ; quotient
		     (r (exact (cadr qr)))) ; remainder
		  (num-to-bytevector-iter
		   r
		   (- index 1)
		   (bytevector-append vec (bytevector q))))))))
      (num-to-bytevector-iter n max-index (bytevector)))))

;; given bytevector of length n, return padded bytevector of length m
;; where m > n and n > 0
(define padding-bytes
  (lambda (n m)
    (let ((l (bytevector-length n)))
      (if (< l m)
	  (bytevector-append n (make-bytevector (- m l) 0))
	  n))))

;; Get .bmp header bytes
(define get-header-bytes
  (lambda (image-size)
    (let ((signature-bytes (string->utf8 "BM"))
	  (filesize-bytes (padding-bytes (num-to-bytevector (+ 54 image-size)) 4))
	  (reserved-bytes (bytevector 0 0 0 0))
	  (dataoffset-bytes (bytevector 54 0 0 0))) ;; 54 = size of header (bytes)
      (bytevector-append
       signature-bytes
       filesize-bytes
       reserved-bytes
       dataoffset-bytes))))

;; Get .bmp info header bytes
(define get-info-header-bytes
  (lambda (width height dpi bpp image-size)
    (letrec ((bps-bytes (padding-bytes (num-to-bytevector (exact (floor (* 39.3701 dpi)))) 4))
	  (size-bytes (bytevector 40 0 0 0)) ;; 40 = size of info header (bytes)
	  (width-bytes (padding-bytes (num-to-bytevector width) 4))
	  (height-bytes (padding-bytes (num-to-bytevector height) 4))
	  (plane-bytes (bytevector 1 0))
	  (bpp-bytes (bytevector bpp 0))
	  (compression-bytes (bytevector 0 0 0 0))
	  (image-size-bytes (padding-bytes (num-to-bytevector image-size) 4))
	  (xpixelspm-bytes bps-bytes)
	  (ypixelspm-bytes bps-bytes)
	  (colorsused-bytes (bytevector 0 0 0 0))
	  (importantcolors-bytes (bytevector 0 0 0 0)))
      (bytevector-append
       size-bytes
       width-bytes
       height-bytes
       plane-bytes
       bpp-bytes
       compression-bytes
       image-size-bytes
       xpixelspm-bytes
       ypixelspm-bytes
       colorsused-bytes
       importantcolors-bytes))))

;; Get .bmp pixel bytes
(define get-pixel-bytes
 (lambda (width bpp pixel-rows)
   (letrec (
	 ;; TODO simplify (4 - ((5 * (bpp/8)) % 4)) % 4
	 (padding-bytes-per-row
	     (make-list (modulo (- 4 (modulo (* width (/ bpp 8)) 4)) 4) 0))

	 ;; flatten each pixel row ((0 0 0) (0 0 0)) => (0 0 0 0 0 0)
	 ;; add padding bytes => (0 0 0 0 0 0 0 0)
	 (reduce-pixel-row
	  (lambda (pixel-bytes pixel-row)   
	    (append
	     pixel-bytes
	     (reduce append '() (map reverse pixel-row))
	     padding-bytes-per-row))))

         ;; now create bytevector on the list elements
     (apply bytevector (reduce reduce-pixel-row (list) (reverse pixel-rows))))))

;; A simple image module
(define image-make
  (lambda (width height r g b bpp dpi)
    (let ((Bpp (exact (/ bpp 8))))
       (list
	(cons 'width width)
	(cons 'height height)
	(cons 'bpp bpp)
	(cons 'dpi dpi)
	(list 'pixels
	      (map
	       (lambda (x)
		 (map
		  (lambda (x) (list r g b))
		  (make-list width)))
		 (make-list height)))))))

;; TODO use the car of each pair to search each field
;; or is there hashtable in R7?

(define image-get-width
  (lambda (image)
    (cdr (list-ref image 0))))

(define image-get-height
  (lambda (image)
    (cdr (list-ref image 1))))

(define image-get-bpp
  (lambda (image)
    (cdr (list-ref image 2))))

(define image-get-dpi
  (lambda (image)
    (cdr (list-ref image 3))))

(define image-get-pixel-rows
  (lambda (image)
    (cadr (list-ref image 4))))

(define image-get-pixel-row
  (lambda (image y)
    (list-ref (image-get-pixel-rows image) y)))

(define image-get-pixel
  (lambda (image x y)
    (list-ref (image-get-pixel-row image y) x)))

;; TODO simplify
(define image-set-pixel!
  (lambda (image x y r g b)
    (let ((pixel (image-get-pixel image x y)))
      (list-set! pixel 0 r)
      (list-set! pixel 1 g)
      (list-set! pixel 2 b))))

;; convert image to bytevector
(define image->bytevector
  (lambda (image)
    (letrec (
	     (width (image-get-width image))
	     (height (image-get-height image))
	     (dpi (image-get-dpi image))
	     (bpp (image-get-bpp image))
	     (pixel-rows (image-get-pixel-rows image))
	     (pixel-bytes (get-pixel-bytes width bpp pixel-rows))
	     (image-size (bytevector-length pixel-bytes))
	     (header-bytes (get-header-bytes image-size))
	     (info-header-bytes (get-info-header-bytes width height dpi bpp image-size)))     
      (bytevector-append header-bytes info-header-bytes pixel-bytes))))

;; write image to file under path, call procedure callback with number of bytes
(define image->file
  (lambda (image path callback)
    (with-output-to-file path
      (lambda ()
	(let ((image-bytes (image->bytevector image)))
	  (write-bytevector (image->bytevector image))

	  (if (not (null? callback))
	      (callback (bytevector-length image-bytes))
	      '()))))))

(letrec ((width 100)
	 (height 100)
	 (bpp 24)
	 (dpi 300)
	 (image (image-make width height 255 0 255 bpp dpi)))
  (image->file image "example.bmp" '())
 )
