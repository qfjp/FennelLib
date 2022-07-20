;; Modified from lua code taken from
;; [here](https://github.com/pakozm/lua-tuple/)

;   Copyright 2014, Francisco Zamora-Martinez
;
;   Permission is hereby granted, free of charge, to any person obtaining a copy
;   of this software and associated documentation files (the "Software"), to deal
;   in the Software without restriction, including without limitation the rights
;   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;   copies of the Software, and to permit persons to whom the Software is
;   furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in all
;   copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;   IN THE SOFTWARE.

(local tuple {:_VERSION :0.1 :_NAME :tuple})
(local bit32 (require :bit32))

(let [ver-num (tonumber (string.match _VERSION "[0-9]+%.[0-9]+"))]
  ;; The following hack is needed to allow unpack over tuples
  (when (< ver-num 5.3)
    (local table (require :table))

    (fn table-unpack [t i n]
      (let [i (or i 1)
            n (or n (length t))]
        (when (<= i n)
          (values (. t i) (table-unpack t (+ i 1) n)))))

    (set table.unpack table-unpack)
    (global unpack table-unpack)))

;; Libraries
(local assert assert)
(local getmetatable getmetatable)
(local ipairs ipairs)
(local pairs pairs)
(local select select)
(local tostring tostring)
(local type type)

(local bit32-band bit32.band)
(local bit32-lshift bit32.lshift)
(local bit32-rshift bit32.rshift)
(local bit32-bxor bit32.bxor)
(local math-max math.max)
(local string-byte string.byte)
(local string-format string.format)
(local string-sub string.sub)
(local table-concat table.concat)
(local table-pack table.pack)

;; Constants
(local MAX_BYTES 4)
(local BYTE_SIZE 8)
(local WORD_SIZE (* MAX_BYTES BYTE_SIZE))
(local BYTE_MASK (- (^ 2 BYTE_SIZE) 1))
(local WORD_MASK (- (^ 2 WORD_SIZE) 1))
(local MAX_NUMBER (^ 2 WORD_SIZE))
(local MAX_BUCKET_HOLES_RATIO 100)
(local NUM_BUCKETS (^ 2 18))
(local WEAK_MT {:__mode :v})

;; The list of tuples is a hash table with a max of NUM_BUCKETS
(local list-of-tuples {})

;; A table with metadata of tuples, indexed by tuple reference
(local tuples-metadata (setmetatable {} {:__mode :k}))

(fn char-iterator [data j]
  ;; A function to convert a string to a byte-string, when used
  ;; as a lua iterator
  (let [j (+ j 1)]
    (when (< j (length data))
      (values j (string.byte (data:sub j j))))))

(fn number-iterator [data j]
  ;; A function to convert a number to a base 2^8 string, when used
  ;; as a lua iterator
  (when (< j MAX_BYTES)
    (let [v (bit32-band (bit32-rshift data (* j 8)) BYTE_MASK)]
      (values (+ j 1) v))))

;; forward decl
(local compute-hash nil)

(fn bytes [data]
  "Splits `data` into iterable (8-bit) bytes"
  (let [tt (type data)]
    (if (= tt :string) (values char-iterator data 0) (= tt :number)
        (do
          (assert (< data MAX_NUMBER) "Only valid for 32 bit numbers")
          (values number-iterator data 0)) (= tt :table)
        (bytes (compute-hash data)) (= tt :nil) (fn [])
        (let [str (assert (tostring data)
                          "Needs an array with numbers, tables or strings")]
          (bytes str)))))

(set-forcibly! compute-hash
               (fn [t]
                 "Computes hash for a tuple candidate"
                 (local htab [0])
                 (for [i 1 (length t)]
                   (each [j c (bytes (. t i))] ; compute hash for every byte in v
                     (let [index (+ j (* MAX_BYTES (- i 1)))
                           h1 (+ (. htab index) c)
                           h2 (+ h1 (bit32-lshift h1 10))
                           h3 (bit32-bxor h2 (bit32-rshift h2 6))
                           ;; modulo 2^32
                           h4 (bit32-band h3 WORD_MASK)]
                       (tset htab (+ index 1) h4))))
                 (let [h (. htab (length htab))
                       h1 (+ h (bit32-rshift h 3))
                       h2 (bit32-bxor h1 (bit32-lshift h1 11))
                       h3 (+ h2 (bit32-lshift h2 15))]
                   ;; modulo 2^32
                   (bit32-band h3 WORD_MASK))))

(local tuple-instance-mt
       {:__metatable ; disallow changing metatable
        false
        ;; concats two tuples or a tuple with a (num/string/table)
        :__concat (fn [aArg bArg]
                    (local (a b)
                           (if (not= (type aArg) :table)
                               (values bArg aArg)
                               (values aArg bArg)))
                    (local aux {})
                    (for [i 1 (length a)]
                      (tset aux (+ (length aux) 1) (. a i)))
                    (if (= (type b) :table)
                        (for [i 1 (length b)]
                          (tset aux (+ (length aux) 1) (. b i)))
                        (tset aux (+ (length aux) 1) b))
                    (tuple aux))
        ;; disallow inserting new elements
        :__newindex (fn [self]
                      (error "Unable to modify a tuple"))})

;; Tuple metadata functions

(fn unwrap [self]
  (. (. tuples-metadata self) 1))

(fn len [self]
  (. (. tuples-metadata self) 2))

(fn hash [self]
  (. (. tuples-metadata self) 3))

(local proxy-metatable {:__lt (fn [self other]
                                (local t (unwrap self))
                                (if (not= (type other) :table) false
                                    (< (length t) (length other)) true
                                    (> (length t) (length other)) false
                                    (= t other) false
                                    (let [break [false]]
                                      (for [i 1 (length t) :until (. break 1)]
                                        (when (> (. t i) (. other i))
                                          (tset break 1 true)))
                                      (not (. break 1)))))
                        :__le (fn [self other]
                                ;; equality is compare-by-reference
                                ;; (tuples are interned & immutable)
                                (if (= self other) true (< self other)))
                        :__concat (fn [self other]
                                    (.. (unwrap self) other))
                        :__metatable :is_tuple
                        :__gc (fn [self]
                                (local h (hash self))
                                (when h
                                  (local p (% h NUM_BUCKETS))
                                  (when (and (. list-of-tuples p)
                                             (not (next (. list-of-tuples p))))
                                    (tset list-of-tuples p nil))))
                        :__ipairs (fn [self]
                                    (ipairs (unwrap self)))
                        :__index (fn [self k]
                                   (. (unwrap self) k))
                        :__newindex (fn [self]
                                      (error "Tuples are immutable"))
                        :__tostring (fn [self]
                                      (local t (unwrap self))
                                      (local result {})
                                      (for [i 1 (length self)]
                                        (let [v (if (not= (. t 1) :string)
                                                    (. t i)
                                                    (string-format "%q" (. t i)))]
                                          (tset result (+ (length result) 1)
                                                (tostring v))))
                                      (table-concat ["tuple{"
                                                     (table-concat result ", ")
                                                     "}"]
                                                    " "))
                        :__mode :v
                        :__pairs (fn [self]
                                   (pairs (unwrap self)))
                        :__len (fn [self]
                                 (len self))})

(fn proxy [tpl n]
  "Returns a wrapper proxy table which shades the data table, providing immutability.
  Takes the tuple and the number of elements"
  (setmetatable tpl tuple-instance-mt)
  (local ref (setmetatable {} proxy-metatable))
  ;; The proxy table has an immutable metatable, and stores the real
  ;; tuple data and the number of elements in tuples-metadata.
  (tset tuples-metadata ref [tpl n])
  ref)

(fn tuple-constructor [t]
  "Builds a candidate tuple given a table, recursively converting tables to tuples."
  (let [n (or t.n (length t))
        new-tuple {}]
    (for [i 1 n]
      (local v (. t i))
      (assert (and (= (type i) :number) (> i 0)) "Needs integer keys > 0")
      (if (= (type v) :table) (tset new-tuple i (tuple v)) (tset new-tuple i v)))
    ;; Returns a proxy to the new tuple with length of #t
    (proxy new-tuple n)))

; Metatable defining tuple class
(local tuple-mt {:__call (fn [self ...]
                           (local n (select "#" ...))
                           (var t (table-pack ...))
                           (assert (= (length t) n))
                           (when (= (length t) 1)
                             (set t (. t 1)))
                           (if (not= (type t) :table) t
                               (do
                                 (local mt (getmetatable t))
                                 (if (= mt :is_tuple)
                                     t)
                                 (do
                                   (local new-tuple (tuple-constructor t))
                                   (local h (compute-hash new-tuple))
                                   (local p (% h NUM_BUCKETS))
                                   (var bucket
                                        (or (. list-of-tuples p)
                                            (setmetatable {} WEAK_MT)))
                                   (tset list-of-tuples p bucket)
                                   (var (max n) (values 0 0))
                                   (each [i vi (pairs bucket)]
                                     (var equals true)
                                     (if (= (length vi) (length new-tuple))
                                         (for [j 1 (length vi)]
                                           (local vj (. vi j))
                                           (when (not= vj (. new-tuple j))
                                             (set equals false)
                                             (lua :break)))
                                         (set equals false))
                                     (when (= equals true)
                                       (lua "return vi"))
                                     (set max (math-max max i))
                                     (set n (+ n 1)))
                                   (when (> (/ max n) MAX_BUCKET_HOLES_RATIO)
                                     (local new-bucket {})
                                     (each [i vi (pairs bucket)]
                                       (tset new-bucket
                                             (+ (length new-bucket) 1) vi))
                                     (tset list-of-tuples p new-bucket)
                                     (set bucket new-bucket)
                                     (set max (length bucket))
                                     (collectgarbage :collect))
                                   (tset bucket (+ max 1) new-tuple)
                                   (tset (. tuples-metadata new-tuple) 3 h)
                                   new-tuple))))})

(setmetatable tuple tuple-mt)

(fn tuple.utest []
  (var a (tuple 2 [4 5] :a))
  (var b (tuple 4 5))
  (var c (tuple 2 (. a 2) :a))
  (var d (tuple 3 4))
  (assert (= a c))
  (assert (= b (. a 2)))
  (assert (= b (. c 2)))
  (assert (< b a))
  (assert (> b d))
  (assert (< d b))
  (assert (> a b))
  (assert (= d (tuple 3 4)))
  (assert (= (compute-hash a) 1275614854))
  (assert (= (compute-hash b) 2765647374))
  (assert (= (compute-hash (tuple 1 2)) 897644836))
  (set (a b c d) (values nil nil nil nil))
  (collectgarbage :collect)
  (var aux {})
  (for [i 1 10000]
    (tset aux (tuple i i) i))
  (assert (= (tuple.stats) 10000))
  (collectgarbage :collect)
  (assert (= (tuple.stats) 10000))
  (set aux nil)
  (collectgarbage :collect)
  (assert (= (tuple.stats) 0))
  (assert (not (getmetatable (tuple 1)))))

(set tuple.stats
     (fn []
       (var num-buckets 0)
       (var size 0)
       (each [k1 v1 (pairs list-of-tuples)]
         (set num-buckets (+ num-buckets 1))
         (each [k2 v2 (pairs v1)]
           (set size (+ size 1))))
       (when (= num-buckets 0)
         (set num-buckets 1))
       (var msz 0)
       (each [_ v (pairs tuples-metadata)]
         (set msz (+ msz 1)))
       (values size num-buckets (/ size NUM_BUCKETS) msz)))

(tuple.utest)
tuple

