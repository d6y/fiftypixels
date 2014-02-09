(ns fiftypixels.core
  (:use mikera.image.core)
  (:use mikera.image.colours)
  (:use mikera.image.filters)
  (:require [clojure.data.json :as json]))

;;;
;;; We take an image, scale it to 7x7 (49 pixels) and output a JSON
;;; datastructure.
;;;
;;; One day a 5x10 (50 pixel) version might be nice.


;; Step 1: Scale the image.
;; We first scale the image down, then crop the image to the exact size.

(defn scale-factor
  "Compute the scaling factor to get the image close to a square of the given size (side).
  More specifically, the scale factor will fit the shortest side into the given size, but the longer side may spill out."
  [side img]
  (let [width  (.getWidth img)
        height (.getHeight img)]
    (/ side (min width height))))

(defn scale [image factor]
  (scale-image image (* (.getWidth image) factor) (* (.getHeight image) factor)))

;; asert width <= target-width; ditto height
(defn crop
  [img target-width target-height]
  (let [x-spill (- (.getWidth img) target-width)
        y-spill (- (.getHeight img) target-height)]
    (sub-image img (/ x-spill 2) (/ y-spill 2) target-width target-height)))


;; Step 2: convert to hex colour codes

(defn to-hex [[r g b]]
  (format "#%02X%02X%02X" r g b))

(defn hex-colours [image]
  (->> (get-pixels image)
       (map components-rgb)
       (vec)
       (map to-hex)))


;; Step 3: Boustrophedon


; The colours in the image are a linear sequence of values.
; We want the first pixel of the image (top left) to be the first
; global (closest to the power), but at the end of the first line
; we reverse direction. Like an ox ploughing a field.
;
(comment
  ; For example...
  (bous (range 1 50) 7)
  ; ( ( 1  2  3  4  5 6 7)
  ;   (14 13 12 11 10 9 8)
  ;   (15 16 17 18 19 20 21)
  ; and so on...
)


(defn- alt-reverse
  "Reverse alternative rows in xs, starting with the first row if alt? is true."
  [alt? xs]
  (if (empty? xs)
    xs
    (let [transform (if alt? reverse identity)]
      (cons (transform (first xs)) (alt-reverse (not alt?) (rest xs))))))

(defn bous [xs n]
  (->> xs
      (partition n n)
      (alt-reverse false)))


;; To JSON

(defn pad
  [xs size value]
  (if (> size (count xs))
    (conj xs value)
    xs))

(defn to-json [image]
  (json/write-str {:lights (pad (hex-colours image) 50 "#000000")}))

;;
;; EXAMPLE
;;

; only seems to do pngs
(def dog (load-image "skitters.png"))

(def tiny
  (scale dog (scale-factor 7 dog)))

(def titchy
  (crop tiny 7 7))


(-> (hex-colours tiny)
    (bous 7))


(count (hex-colours tiny))
(count (hex-colours titchy))
(count (pad(hex-colours titchy) 50 "#0000"))

(println (to-json titchy))


(show titchy :zoom 10)
