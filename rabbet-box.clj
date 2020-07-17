(require '[clojure.string :as str])

(refer-clojure :exclude [compile])

(def ^:dynamic *units* :mm)

(defn units [unit]
  (case unit
    :mm [:g28]
    :in [:g20]))

(defmacro with-units [unit & body]
  `(let [unit# ~unit
         orig# *units*]
     (binding [*units* unit#]
       [(units unit#)
        ~@body
        (units orig#)])))

(defn mm [x]
  {:unit :mm
   :amount x})

(defn in [x]
  {:unit :in
   :amount x})

(defn convert
  ([from] (convert from *units*))
  ([{:keys [unit amount] :as from} to]
   (cond
     (number? from)
     from

     (= unit to)
     amount

     (= to :mm)
     (* amount 25.4)

     (= to :in)
     (/ amount 25.4)

     :else
     (throw (ex-info "Unrecognized conversion."
                     {:reason ::unrecognized-conversion
                      :unit unit
                      :amount amount
                      :to to})))))

(defmacro with-conversions [bindings & body]
  (let [bs (reduce into
                   []
                   (for [binding bindings]
                     [binding (list 'convert binding)]))]
    `(let ~bs
       ~@body)))

(defn positioning [pos]
  (case pos
    :absolute [:g90]
    :relative [:g91]))

(def ^:dynamic *positioning* :absolute)

(defmacro with-positioning [pos & body]
  `(let [pos#  ~pos
         orig# *positioning*]
     (binding [*positioning* pos#]
       [(positioning pos#)
        ~@body
        (positioning orig#)])))

(defn prompt [msg]
  [:m0 msg])

(defn probe [axis]
  [:g28 axis])

(defn reset-coords [coords]
  [:g92 coords])

(defn rapid [& args]
  (into [:g0] args))

(defn move [& args]
  (into [:g1] args))

(defn await-moves-complete []
  [:m400])

(defn path
  [{probe-plate-thickness :probe-plate-thickness
    retract-height        :retract-height
    work-thickness        :work-thickness
    bit-diameter          :bit-diameter
    work-offset-x         :work-offset-x
    dado-pass-length      :dado-pass-length
    :as                   params}]
  (with-conversions [probe-plate-thickness
                     retract-height
                     work-thickness
                     bit-diameter
                     work-offset-x
                     dado-pass-length]
    (let [bit-radius (/ (convert bit-diameter) 2)
          dado-width (/ (convert work-thickness) 2)]
      [(with-positioning :absolute
         (with-units :mm
           ;; [:m84 :s0] ; disable steppers?
           (reset-coords {:x 0 :y 0})
           (prompt "Attach ZProbe - Top")
           (probe :z)
           (reset-coords {:z probe-plate-thickness})
           (rapid {:z retract-height :f 300})
           (await-moves-complete)
           (prompt "Detach ZProbe")

           (prompt "Position Part for Dado")

           (prompt "Start Spindle")

           (let [pass-offset (- dado-width bit-diameter)
                 x1          (+ work-offset-x (- bit-radius) work-thickness)
                 x2          (- x1 pass-offset)]
             (for [depth (map - (range bit-radius (+ dado-width 0.001) bit-radius))]
               [(rapid {:x x1
                        :y 0})
                (rapid {:z depth})
                (move {:x x1
                       :y dado-pass-length})
                (move {:x x2
                       :y dado-pass-length})
                (move {:x x2
                       :y 0})])))

         (prompt "Stop Spindle"))])))

(defn compile-keyword
  [k]
  (-> k name str/upper-case))

(defn number-format
  [n]
  (if (integer? n)
    (str n)
    (with-out-str (printf "%.02f" n))))

(defn compile-value
  [v]
  (cond
    (and (map? v) (:unit v))
    (number-format (convert v))

    (number? v)
    (number-format v)

    :else v))

(defn compile-arg
  [arg]
  (cond
    (map? arg)
    (str/join " "
              (reduce-kv (fn [o k v]
                           (conj o (str (compile-keyword k) (compile-value v))))
                         []
                         arg))

    (keyword? arg)
    (compile-keyword arg)

    (string? arg)
    arg

    :else
    (throw (ex-info (str "Unrecognized operand " arg)
                    {:reason ::unknown-operand
                     ::arg arg}))))

(defn compile-command
  [[cmd & args]]
  (str (compile-keyword cmd)
       " "
       (->> args (map compile-arg) (str/join " "))))

;; Programs are seqs of commands. A command is either a seq starting
;; with a scalar representing an instruction, or a collection, which
;; is itself a program, recursively.

(defn compile
  [program]
  (doseq [command program]
    (if (coll? (first command))
      (compile command)
      (println (compile-command command)))))

(def params
  {:probe-plate-thickness (mm 4.2)
   :retract-height        (mm 5)
   :work-thickness        (in 0.5)
   :bit-diameter          (in 0.125)
   :work-offset-x         (mm 20)
   :dado-pass-length      (in 6)})

(compile (path params))

;; (require 'clojure.pprint)
;; (clojure.pprint/pprint
;;  (path params))
