; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yakup Genc                       *
; *********************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Burak Özdemir
;;;141044027
;;;CSE-341 HOMEWORK-1
;;;---------------------------------------------
;;;https://github.com/clojure/math.combinatorics ;;permutasyon için yardımcı fonksiyonlar burdan alındı.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.decode
	(:gen-class)
	)

;; utility functions 
(load-file "include.clj") ;; "c2i and "i2c"


;;verilen liste elemanını harflerine ayırarak tekrar liste seklinde return eder
;;read-as-list icin yardmcı fonksyıon
(defn wordToLetter [listEleman]
	(def myword (join " " listEleman))
	(def mylettervector (str/split myword #" "))
	(def mylettersymbol (into '() mylettervector))
	(def mylettersymbol (reverse mylettersymbol))
	mylettersymbol
	)
;;dosyadan okur ve paragrafi karakterler listesine boler
(defn read-as-list
	"Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	[filename]
	(def doclist '())
	(with-open [rdr (clojure.java.io/reader filename)]
		(doseq [line (line-seq rdr)]
			(def doclist (concat doclist (cons (wordToLetter line) '())))
			)
		)
	doclist
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

;;spell-checker0 helper function
;;dictionaryden kelimeleri alır ve list return eder
;;atom list kullanarak yapıldı
;;NOTE:sozlugu tersten listeye yerlestriyo
(defn fromDictionarytoAtomList [file]
	(def a (atom ()))
	(with-open [rdr (clojure.java.io/reader file)]
		(doseq [line (line-seq rdr)]
			(swap! a conj line)
			)
		)
	@a
	)
;;suan düz kelime olarak word almalı
;;eger sozlugu file den okursa splitFunction kullan
;;spell-checker0
(defn spell-checker-0 
	[word]
	(def sonuc 0)
	(with-open [rdr (clojure.java.io/reader "dictionary2.txt")]
		(doseq [line (line-seq rdr) ]
			(let [x 0]
				(if (= (clojure.string/join "" word) line) (def sonuc 1))
				)
			)
		)
	(if (= sonuc 1)
		true
		false
		)
)

;;dictionary1.txt den karşlastrma yapıyor
;;suan rakamlarla calısıyor
;;binary search
;;spell-checker1
(defn spell-checker-1
	[word]
	(def dictlist (fromDictionarytoAtomList "dictionary2.txt"))
	(def sonuc 0)
	(def myword (clojure.string/join "" word))
	(def dictlist (reverse dictlist))
	(let [upcount (dec (count dictlist))]
		(loop [alt 0 ust upcount]
			(if (> alt ust ) nil
											 (let [orta (quot (+ ust alt) 2) ortadeger (nth dictlist orta)]
												 (cond
													 (> (compare ortadeger myword) 0) (recur alt (dec orta))
													 (< (compare ortadeger myword) 0) (recur (inc orta) ust)
													 (= (compare ortadeger myword) 0) (def sonuc 1)
													 )
												 )
											 )
			)
		)
	(if (= sonuc 1)
		true
		false
		)
)

;;;;;;;;;;;;;;;;;;;;(Permutations Functions);;;;;;;;;;;;
(defn- iter-perm [v]
	(let [len (count v),
				j (loop [i (- len 2)]
						(cond (= i -1) nil
									(< (v i) (v (inc i))) i
									:else (recur (dec i))))]
		(when j
			(let [vj (v j),
						l (loop [i (dec len)]
								(if (< vj (v i)) i (recur (dec i))))]
				(loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
					(if (< k l)
						(recur (assoc v k (v l) l (v k)) (inc k) (dec l))
						v))))))
(defn- vec-lex-permutations [v]
	(when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))
(defn- lex-permutations
	"DEPRECATED as a public function.
In prior versions of the combinatorics library, there were two similar functions: permutations and lex-permutations.  It was a source of confusion to know which to call.  Now, you can always call permutations.  When appropriate (i.e., when you pass in a sorted sequence of numbers), permutations will automatically call lex-permutations as a speed optimization."
	{:deprecated "1.3"}
	[c]
	(lazy-seq
		(let [vec-sorted (vec (sort c))]
			(if (zero? (count vec-sorted))
				(list [])
				(vec-lex-permutations vec-sorted)))))
(defn- sorted-numbers?
	"Returns true iff s is a sequence of numbers in non-decreasing order"
	[s]
	(and (every? number? s)
			 (or (empty? s) (apply <= s))))
(defn- multi-perm
	"Handles the case when you want the permutations of a list with duplicate items."
	[l]
	(let [f (frequencies l),
				v (vec (distinct l)),
				indices (apply concat
											 (for [i (range (count v))]
												 (repeat (f (v i)) i)))]
		(map (partial map v) (lex-permutations indices))))
(defn- all-different?
	"Annoyingly, the built-in distinct? doesn't handle 0 args, so we need
to write our own version that considers the empty-list to be distinct"
	[s]
	(if (seq s)
		(apply distinct? s)
		true))
(defn permutations
	"All the distinct permutations of items, lexicographic by index
(special handling for duplicate items)."
	[items]
	(cond
		(sorted-numbers? items) (lex-permutations items),

		(all-different? items)
		(let [v (vec items)]
			(map #(map v %) (lex-permutations (range (count v)))))

		:else
		(multi-perm items)))
;;;;;;;;;;;;;;;;;;;(Encode Functions);;;;;;;;;;;;;;;;;;;
(defn getChipChar [ch]
	(def char '())
	(cond
		(= ch "a") (def char (concat char "d"))
		(= ch "b") (def char (concat char "e"))
		(= ch "c") (def char (concat char "f"))
		(= ch "d") (def char (concat char "p"))
		(= ch "e") (def char (concat char "q"))
		(= ch "f") (def char (concat char "a"))
		(= ch "g") (def char (concat char "b"))
		(= ch "h") (def char (concat char "k"))
		(= ch "i") (def char (concat char "l"))
		(= ch "j") (def char (concat char "c"))
		(= ch "k") (def char (concat char "r"))
		(= ch "l") (def char (concat char "s"))
		(= ch "m") (def char (concat char "t"))
		(= ch "n") (def char (concat char "g"))
		(= ch "o") (def char (concat char "y"))
		(= ch "p") (def char (concat char "z"))
		(= ch "q") (def char (concat char "h"))
		(= ch "r") (def char (concat char "i"))
		(= ch "s") (def char (concat char "j"))
		(= ch "t") (def char (concat char "m"))
		(= ch "u") (def char (concat char "n"))
		(= ch "v") (def char (concat char "o"))
		(= ch "w") (def char (concat char "u"))
		(= ch "x") (def char (concat char "v"))
		(= ch "y") (def char (concat char "w"))
		(= ch "z") (def char (concat char "x"))
		)
	char
	)
(defn encodeWord [word]
	(def newword '())
	(if (= nil word) ()
									 (doseq [n word]
										 (def newword (concat newword (getChipChar n)))
										 )
									 )
	newword
	)
(defn encodedDoc [doc]
	(def newdoc '())
	(if (= nil doc) '()
									(doseq [n doc]
										(def newdoc (concat newdoc (cons (encodeWord n) '())))
										)
									)
	newdoc
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;(Helper Functions For Decode Functions);;;;;;;;;;;;
;;;;alfabeleri karsilastirmasi
(defn myWordChecker [encodedAlp word]
	(if (empty? encodedAlp)nil)
	(def myencodedAlp (into '() encodedAlp))
	(def myencodedAlp (reverse myencodedAlp))
	;;(println (nth myencodedAlp 0) (type (nth myencodedAlp 0)) (nth word 0) (type (nth word 0)))
	(def kontrolword '())
	(def demokeys (hash-map (nth myencodedAlp 0) 'a (nth myencodedAlp 1) 'b (nth myencodedAlp 2) 'c (nth myencodedAlp 3) 'd
													(nth myencodedAlp 4) 'e (nth myencodedAlp 5) 'f (nth myencodedAlp 6) 'g (nth myencodedAlp 7) 'h
													(nth myencodedAlp 8) 'i (nth myencodedAlp 9) 'j (nth myencodedAlp 10) 'k (nth myencodedAlp 11) 'l
													(nth myencodedAlp 12) 'm (nth myencodedAlp 13) 'n (nth myencodedAlp 14) 'o (nth myencodedAlp 15) 'p
													(nth myencodedAlp 16) 'q (nth myencodedAlp 17) 'r (nth myencodedAlp 18) 's (nth myencodedAlp 19) 't
													(nth myencodedAlp 20) 'u (nth myencodedAlp 21) 'v (nth myencodedAlp 22) 'w (nth myencodedAlp 23) 'x
													(nth myencodedAlp 24) 'y (nth myencodedAlp 25) 'z)
		)
	(doseq [n word]
		(cond
			(= n "a") (def kontrolword (conj kontrolword (get demokeys 'a)))
			(= n "b") (def kontrolword (conj kontrolword (get demokeys 'b)))
			(= n "c") (def kontrolword (conj kontrolword (get demokeys 'c)))
			(= n "d") (def kontrolword (conj kontrolword (get demokeys 'd)))
			(= n "e") (def kontrolword (conj kontrolword (get demokeys 'e)))
			(= n "f") (def kontrolword (conj kontrolword (get demokeys 'f)))
			(= n "g") (def kontrolword (conj kontrolword (get demokeys 'g)))
			(= n "h") (def kontrolword (conj kontrolword (get demokeys 'h)))
			(= n "i") (def kontrolword (conj kontrolword (get demokeys 'i)))
			(= n "j") (def kontrolword (conj kontrolword (get demokeys 'j)))
			(= n "k") (def kontrolword (conj kontrolword (get demokeys 'k)))
			(= n "l") (def kontrolword (conj kontrolword (get demokeys 'l)))
			(= n "m") (def kontrolword (conj kontrolword (get demokeys 'm)))
			(= n "n") (def kontrolword (conj kontrolword (get demokeys 'n)))
			(= n "o") (def kontrolword (conj kontrolword (get demokeys 'o)))
			(= n "p") (def kontrolword (conj kontrolword (get demokeys 'p)))
			(= n "q") (def kontrolword (conj kontrolword (get demokeys 'q)))
			(= n "r") (def kontrolword (conj kontrolword (get demokeys 'r)))
			(= n "s") (def kontrolword (conj kontrolword (get demokeys 's)))
			(= n "t") (def kontrolword (conj kontrolword (get demokeys 't)))
			(= n "u") (def kontrolword (conj kontrolword (get demokeys 'u)))
			(= n "v") (def kontrolword (conj kontrolword (get demokeys 'v)))
			(= n "w") (def kontrolword (conj kontrolword (get demokeys 'w)))
			(= n "x") (def kontrolword (conj kontrolword (get demokeys 'x)))
			(= n "y") (def kontrolword (conj kontrolword (get demokeys 'y)))
			(= n "z") (def kontrolword (conj kontrolword (get demokeys 'z)))
			)
		)

	(def kontrolword (reverse kontrolword))
	(if (= (spell-checker-1 kontrolword) true)
		kontrolword
		)
	)

;;letterlar karakter tipinde tipinde
;;;;liste icinde gelen kelimelerin harfleri string tipinde
(defn findMost6Letter [paragrafList]
	(def myletterlist '())
	(def mybig6 '())
	(doseq [word paragrafList]
		(doseq [letter word]
			(def myletterlist (concat myletterlist letter))
			)
		)
	(println (type (nth myletterlist 0)))
	(def myfreqmap (frequencies myletterlist))
	(def myfreqmapsort (sort-by last myfreqmap))
	(def myfreqmaplength (count myfreqmapsort))
	(println myfreqmaplength)
	(def mymaplist (keys myfreqmapsort))
	(def mymaplist (reverse mymaplist))

	(loop [x 0]
		(when (< x 6)
			(def mybig6 (concat mybig6 (str (nth mymaplist x))))
			(recur (+ x 1))
			)
		)

	mybig6

	)

(defn findMostLetter [doclist]
	(def myletterlist '())
	(def mybig '())
	(doseq [word doclist]
		(doseq [letter word]
			(def myletterlist (concat myletterlist letter))
			)
		)

	(def myfreqmap (frequencies myletterlist))
	(def myfreqmapsort (sort-by last myfreqmap))
	(def myfreqmaplength (count myfreqmapsort))
	(def mymaplist (keys myfreqmapsort))
	(def mymaplist (reverse mymaplist))

	(def mybig (str (nth mymaplist 0)))
	mybig
	)

;;gen-decoder-b0 icin harflerni karsılastrmasını yaparak plain kelime üretir
(defn myWordCheckerB0 [encodedAlp word]
	(if (empty? encodedAlp)nil)
	(def kontrolword '())
	(def demokeys (hash-map (nth encodedAlp 0) 'a (nth encodedAlp 1) 'b (nth encodedAlp 2) 'c (nth encodedAlp 3) 'd
													(nth encodedAlp 4) 'e (nth encodedAlp 5) 'f (nth encodedAlp 6) 'g (nth encodedAlp 7) 'h
													(nth encodedAlp 8) 'i (nth encodedAlp 9) 'j (nth encodedAlp 10) 'k (nth encodedAlp 11) 'l
													(nth encodedAlp 12) 'm (nth encodedAlp 13) 'n (nth encodedAlp 14) 'o (nth encodedAlp 15) 'p
													(nth encodedAlp 16) 'q (nth encodedAlp 17) 'r (nth encodedAlp 18) 's (nth encodedAlp 19) 't
													(nth encodedAlp 20) 'u (nth encodedAlp 21) 'v (nth encodedAlp 22) 'w (nth encodedAlp 23) 'x
													(nth encodedAlp 24) 'y (nth encodedAlp 25) 'z)
		)
	(doseq [n word]
		(cond
			(= n "a") (def kontrolword (conj kontrolword (get demokeys \a)))
			(= n "b") (def kontrolword (conj kontrolword (get demokeys \b)))
			(= n "c") (def kontrolword (conj kontrolword (get demokeys \c)))
			(= n "d") (def kontrolword (conj kontrolword (get demokeys \d)))
			(= n "e") (def kontrolword (conj kontrolword (get demokeys \e)))
			(= n "f") (def kontrolword (conj kontrolword (get demokeys \f)))
			(= n "g") (def kontrolword (conj kontrolword (get demokeys \g)))
			(= n "h") (def kontrolword (conj kontrolword (get demokeys \h)))
			(= n "i") (def kontrolword (conj kontrolword (get demokeys \i)))
			(= n "j") (def kontrolword (conj kontrolword (get demokeys \j)))
			(= n "k") (def kontrolword (conj kontrolword (get demokeys \k)))
			(= n "l") (def kontrolword (conj kontrolword (get demokeys \l)))
			(= n "m") (def kontrolword (conj kontrolword (get demokeys \m)))
			(= n "n") (def kontrolword (conj kontrolword (get demokeys \n)))
			(= n "o") (def kontrolword (conj kontrolword (get demokeys \o)))
			(= n "p") (def kontrolword (conj kontrolword (get demokeys \p)))
			(= n "q") (def kontrolword (conj kontrolword (get demokeys \q)))
			(= n "r") (def kontrolword (conj kontrolword (get demokeys \r)))
			(= n "s") (def kontrolword (conj kontrolword (get demokeys \s)))
			(= n "t") (def kontrolword (conj kontrolword (get demokeys \t)))
			(= n "u") (def kontrolword (conj kontrolword (get demokeys \u)))
			(= n "v") (def kontrolword (conj kontrolword (get demokeys \v)))
			(= n "w") (def kontrolword (conj kontrolword (get demokeys \w)))
			(= n "x") (def kontrolword (conj kontrolword (get demokeys \x)))
			(= n "y") (def kontrolword (conj kontrolword (get demokeys \y)))
			(= n "z") (def kontrolword (conj kontrolword (get demokeys \z)))
			)
		)
	(def kontrolword (reverse kontrolword))
	(if (= (spell-checker-0 kontrolword) true)
		kontrolword
		)
	)

;;paragraf listesi olusturur.
(defn paraglister [a]
	(def myparagraflist '())

	(def mywordvector (str/split a #"\s+"))

	(def mywordsymbol (into '() mywordvector))
	(def mywordsymbol (reverse mywordsymbol))

	(doseq [myword mywordsymbol]
		(def myparagraflist (concat myparagraflist (cons (wordToLetter myword) '())))
		)
	(reverse myparagraflist)
	myparagraflist
	)

;;butun dokumentı listeye atar kelime kelime
(defn listofdocument [doc]
	(def document (slurp doc))
	(def docvector (str/split document #"\s"))
	(def listwords (into '() docvector))
	(def result '())
	(doseq [n listwords]
		(def result (concat result (cons (wordToLetter n) '())))
		)
	(reverse result)
	)

;;gen-decoder-b0 icin occurenceları alıp alfabe üretir
(defn generatedAlphabe [occurencesList alfabe]
	(def temp (into [] '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)))
	(def resultalfabe '())
	;;(println "occur:0" occurencesList)
	;;(println "alfabe:" alfabe)
	(loop [y 0 z 0]
		(when (and (< y 20) (< z 26))
			(if (or (= z 0) (= z 4) (= z 8) (= z 13) (= z 14) (= z 19))
				(do
					(cond
						(= z 4) (def resultalfabe (concat resultalfabe (str (nth occurencesList 0))))
						(= z 19) (def resultalfabe (concat resultalfabe (str (nth occurencesList 1))))
						(= z 0) (def resultalfabe (concat resultalfabe (str (nth occurencesList 2))))
						(= z 14) (def resultalfabe (concat resultalfabe (str (nth occurencesList 3))))
						(= z 8) (def resultalfabe (concat resultalfabe (str (nth occurencesList 4))))
						(= z 13) (def resultalfabe (concat resultalfabe (str (nth occurencesList 5))))
						)
					(recur y (+ z 1))
					)
				(do (def resultalfabe (concat resultalfabe (str (nth alfabe y))))
						(recur (+ y 1) (+ z 1))
						)
				)
			)
		)
	resultalfabe
	)

;;;kelimeler listesi alır
(defn mydecoder [decoder paragraf documentLength]
	(def paraglength (count paragraf))
	(def plainparag '())

	(def plainparag (concat plainparag (cons (decoder paragraf) '())))
	plainparag
	)

;;occurenceslist:symbol , alfabe:symbol
;; e-4 , t-19 , a-0 , o-14 , i-8 , n-13
;;occurlistte encok tekrar eden 6 harf var ve bunları direk yeni alfabede yerine yerlestir
;; daha sonra alfabe parametresiyle gelen harf kombinasyonlarınıda yerine yerlestir (20 harf) . ya dongu kullan yada
;; direk yerine yerlestir . ve list seklinde return et
(defn generatedAlphabe [occurencesList alfabe]
	(def temp (into [] '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)))
	(def resultalfabe '())
	;;(println "occur:0" occurencesList)
	;;(println "alfabe:" alfabe)
	(loop [y 0 z 0]
		(when (and (< y 20) (< z 26))
			(if (or (= z 0) (= z 4) (= z 8) (= z 13) (= z 14) (= z 19))
				(do
					(cond
						(= z 4) (def resultalfabe (concat resultalfabe (str (nth occurencesList 0))))
						(= z 19) (def resultalfabe (concat resultalfabe (str (nth occurencesList 1))))
						(= z 0) (def resultalfabe (concat resultalfabe (str (nth occurencesList 2))))
						(= z 14) (def resultalfabe (concat resultalfabe (str (nth occurencesList 3))))
						(= z 8) (def resultalfabe (concat resultalfabe (str (nth occurencesList 4))))
						(= z 13) (def resultalfabe (concat resultalfabe (str (nth occurencesList 5))))
						)
					(recur y (+ z 1))
					)
				(do (def resultalfabe (concat resultalfabe (str (nth alfabe y))))
						(recur (+ y 1) (+ z 1))
						)
				)
			)
		)
	resultalfabe
	)
(defn generateAlp [occur alfabe]
	(def resultalfabe '())
	(println occur)
	(loop [y 0 z 0]
		(when (and (< y 5) (< z 5))
			(if (= z 0)
				(do (def resultalfabe (concat resultalfabe occur))
					(recur y (+ z 1))
					)
				(do (def resultalfabe (concat resultalfabe (str (nth alfabe y))))
						(recur (+ y 1) (+ z 1))
						)
				)
			)
		)
	resultalfabe
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A 
	[paragraph]
	(def listgeneratedalfabe (permutations #{'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z
																					 }))
	(def myparaglength (count paragraph))
	(def myplainwords '())
	(def myplaintext '())
	(def mylistgenerated (into '() listgeneratedalfabe))

	(doseq [n mylistgenerated]
		(def myplainwords '())
		(loop [x 0]
			(when (< x myparaglength)
				(if (not= nil (myWordChecker (reverse (into '() n)) (nth paragraph x)))
					(def myplainwords (concat myplainwords (cons (myWordChecker (reverse (into '() n)) (nth paragraph x)) '())))
					)
				(recur (+ x 1))
				)
			)
		(if (= myparaglength (count myplainwords))
			(def myplaintext (into myplaintext myplainwords))
			nil
			)
		)
	(reverse myplaintext)
)

;;;içerde occurenceları bulurken document dosyasını okumak zorunda
;;;sabit dosya adı var içerde
(defn Gen-Decoder-B-0 
	[paragraph]
	(def docOccur (listofdocument paragraph))
	(def occurletter(findMostLetter docOccur))
	(def occurletter (into '() occurletter))
	(def alfabe '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z))
	(def newalfabe)
	(def letter4 '())
	(def letter4 (into letter4 (set/difference (set alfabe) (set occurletter)))) ;;karakter cinsinden
	(def symbolletter '())
	(doseq [n letter4]
		(def symbolletter (concat symbolletter (str n)))
		)
	(def mylistgenerated (permutations (set symbolletter)))

	(def mylistlength (count paragraph))
	(def decodedwords '())
	(def myplaintext '())

	(doseq [n mylistgenerated]
		(def decodedwords '())
		(loop [x 0]
			(when (< x mylistlength)
				(def newalfabe (generatedAlphabe occurletter n))
				(if (not= nil (myWordCheckerB0 newalfabe (nth paragraph x)))
					(def decodedwords (concat decodedwords (cons (myWordCheckerB0 n (nth paragraph x)) '())))
					)
				(recur (+ x 1))
				)
			)
		(if (= (count decodedwords) mylistlength)
			(def myplaintext (into '() decodedwords))
			nil
			)
		)
)

(defn Gen-Decoder-B-1 
	[paragraph]
  	;you should implement this function
)

(defn Code-Breaker 
	[document decoder]
	(def plaintext '())
	(def doclength (count (read-as-list document)))
	(with-open [rdr (clojure.java.io/reader document)]
		(doseq [line (line-seq rdr)]
			(def paragraf (paraglister line))
			(def plaintext (concat plaintext (cons (mydecoder decoder paragraf doclength) '())))
			)
		)
	plaintext
)

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[document]
	(println "------------------Test(Gen-Decoder-A)---------------------")
	(println "Alfabe:" '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(println "ChipAlp:" '(d e f p q a b k l c r s t g y z h i j m n o u v w x))
	(def doc (listofdocument document))
	;;(println "EncodedDoc:" (encodedDoc doc))
	(println "plainDoc:" doc)
	(println "-------------------------------------------------------")
	(println "Found PlainDoc:" (Code-Breaker document Gen-Decoder-A))
	(println "------------------Test(Gen-Decoder-B)---------------------")
	(println "Alfabe:" '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(println "ChipAlp:" '(d e f p q a b k l c r s t g y z h i j m n o u v w x))
	(def doc (listofdocument document))
	;;(println "EncodedDoc:" (encodedDoc doc))
	(println "plainDoc:" doc)
	(println "-------------------------------------------------------")
	(println "Found PlainDoc:" (Code-Breaker document Gen-Decoder-B-0))

)

;;
;;bu fonksiyon paremetre olarak encoded document alır.
(test_on_test_data document)