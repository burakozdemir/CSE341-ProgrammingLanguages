;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Burak Özdemir
;;;141044027
;;;CSE-341 PROJECT-1
;;;---------------------------------------------
;;;file.txt dosyası ile çalışıldı
;;;token listesi return eder
;;;program calısırken token analizinde ekrana cinsini basar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns clojure.lexer
  (:gen-class)
  )


(use '[clojure.string :only (join split)])
(use 'clojure.core)
(require '[clojure.string :as str])

(def resultList '())
;fonksiyon stringler arası bosluk koyar
(defn boslukKoy [mystr]
  (def len (count mystr))
  (def res "")
  (def x 0)
  (doseq [n mystr]
    (if (or (= n \backspace) (= n \tab) (= n \return) )
      (do )
      (do
        (if (or (= n \() (= n \)) (= n \newline))
          (do
            (if (not= x 0)
              (def res (str res \space))
              )
            (def res (str res n))
            (def res (str res \space))
            )
          (do
            (def res (str res n))
            )
          )
        )
      )
    (def x (+ x 1))
    )
  res
  )
;dosyayı tek stringe alıp parcalar
(defn parcala [file]
  (def mystring (slurp file))
  (def mystring (boslukKoy mystring))
  (def fileVector (str/split mystring #"\s+"))
  (def fileList (into '() fileVector))
  (def listOffile (reverse fileList))

  listOffile
  )
;token binary kontrol
(defn isBin [input]
  (def flag false)
  (if (or (= "true" input) (= "false" input))
    (def flag true)
    )
  flag
  )
;token keyword kontrol
(defn isKeyw [input]
  (def flag false)
  (let [x input]
    (cond
      (= x "and") (do (def flag true))
      (= x "or") (do  (def flag true))
      (= x "not") (do (def flag true))
      (= x "equal") (do (def flag true))
      (= x "append") (do (def flag true))
      (= x "concat") (do  (def flag true))
      (= x "set") (do (def flag true))
      (= x "deffun") (do  (def flag true))
      (= x "for") (do (def flag true))
      (= x "while") (do  (def flag true))
      (= x "if") (do  (def flag true))
      (= x "then") (do  (def flag true))
      (= x "else") (do  (def flag true))
      (= x "true") (do  (def flag true))
      (= x "false") (do (def flag true))
      :else false)
    )
  flag
  )
;token operator kontrol
(defn isOpe [input]
  (def flag false)
  (let [x input]
    (cond
      (= x "(") (do (def flag true))
      (= x ")") (do (def flag true))
      (= x "+") (do  (def flag true))
      (= x "-") (do (def flag true))
      (= x "/") (do  (def flag true))
      (= x "*") (do (def flag true))
      :else false)
    )
  flag
  )
;token ınt kontrol
(defn isInt [input]
  (def flag true)
  (def counters 0)
  (def temp "")
  (doseq [n input]
    (if (or (> 0 (compare n \0)) (< 0 (compare n \9)))
      (do
        (if (and (= counters 0) (= 0 (compare n \-)))
          (do
            (def temp (str temp n))
            )
          (def flag false)
          )
        )
      (do
        (def temp (str temp n))
        )
      )
    (def counters (+ counters 1))
    )
  (if (and (= counters 1) (= temp \-)) (do (def flag false)))
  flag
  )
;token ıdentifier kontrol
(defn isIden [input]
  (def myflag true)
  (if (= 0 (count input)) (def myflag false))
  (if (= (isKeyw input) true) (def myflag false))
  (doseq [n input]
    (if (and (or (> 0 (compare n \a)) (< 0 (compare n \z)))
             (or (> 0 (compare n \A)) (< 0 (compare n \Z))))
      (def myflag false)
      )
    )
  myflag
  )
;parametreyı global lısteye ekler
(defn duzenle [input]
  (def temp (cons input '()))
  (def resultList (concat resultList temp))
  )

(defn lexer [file]
  (def mylist (parcala file))
  (println "list of string:" mylist)
  (doseq [n mylist]
    (cond
      (= (isBin n) true) (do (println "binary:" n) (duzenle n))
      (= (isKeyw n) true) (do (println "keyword:" n) (duzenle n))
      (= (isOpe n) true) (do (println "operand:" n) (duzenle n))
      (= (isInt n) true) (do (println "ınteger:" n) (duzenle n))
      (= (isIden n) true) (do (println "ıdentifier:" n) (duzenle n))
      :else (do (println "Illegal Token:" n) (System/exit 0) )
      )
    )
  resultList
  )

(defn mytest [file]
  (println "lexical list:" (lexer file))
  )

(mytest "file.txt")
