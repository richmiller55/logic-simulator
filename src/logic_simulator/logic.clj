(ns logic)

(def logic-values #{0 1 :X}) ; 0 (Low), 1 (High), :X (Unknown/High Impedance)

;; Example gate types and their logic
(def gate-types
  {:AND {:inputs 2 :fn (fn [a b] (if (or (= :X a) (= :X b)) :X (min a b)))}
   :OR  {:inputs 2 :fn (fn [a b] (if (or (= :X a) (= :X b)) :X (max a b)))}
   :XOR {:inputs 2 :fn (fn [a b] (if (or (= :X a) (= :X b)) :X (if (= a b) 0 1)))}
   :NAND {:inputs 2 :fn (fn [a b]
                              (if (or (= :X a) (= :X b))
                                :X
                                (- 1 (min a b))))}

   :NOT {:inputs 1 :fn (fn [a] (if (= :X a) :X (- 1 a)))}
   :BUF {:inputs 1 :fn (fn [a] a)} ; Buffer (delay component)
   :INPUT {:inputs 0 :fn (fn [state] (:value state))} ; External input source
   :OUTPUT {:inputs 1 :fn (fn [a] a)} ; Monitor point
   })


(def initial-circuit-state
  {:components {} ; Map of component-id -> {:type :AND, :inputs [wire-id-1 wire-id-2], :output wire-id-3}
   :wires      {} ; Map of wire-id -> current-value (0, 1, or :X)
   :events     '() ; Priority queue or list of events for scheduled simulation
   :time       0})

(defn add-wire [circuit wire-id initial-value]
  (assoc-in circuit [:wires wire-id] (or initial-value :X)))

(defn add-component [circuit comp-id type input-wire-ids output-wire-id]
  (assoc-in circuit [:components comp-id]
            {:type type
             :inputs input-wire-ids
             :output output-wire-id}))

(defn evaluate-gate [circuit comp-data]
  (let [gate-type (:type comp-data)
        gate-def (gate-types gate-type)
        input-wires (:inputs comp-data)
        input-values (mapv #(get-in circuit [:wires %] :X) input-wires) ; Get current wire values
        new-value (apply (:fn gate-def) input-values)]
    new-value))

(defn propagate-change [circuit changed-wire-id]
  (loop [current-circuit circuit
         components-to-check (vals (:components circuit))]
    (if (empty? components-to-check)
      current-circuit
      (let [comp (first components-to-check)
            comp-id (first (keep (fn [[k v]] (when (= v comp) k)) (:components circuit))) ; A better way to get key would be needed
            input-wires (:inputs comp)]
        (if (some #{changed-wire-id} input-wires)
          ;; This component's input changed, re-evaluate
          (let [old-output-wire-id (:output comp)
                old-output-value (get-in current-circuit [:wires old-output-wire-id])
                new-output-value (evaluate-gate current-circuit comp)]
            (if (= old-output-value new-output-value)

                            ;; No change in output, continue
              (recur current-circuit (rest components-to-check))
              ;; Output changed, update wire and recurse to check all components again
              (let [next-circuit (assoc-in current-circuit [:wires old-output-wire-id] new-output-value)]
                (recur (propagate-change next-circuit old-output-wire-id) ; Recursive propagation
                       (rest components-to-check))))
          ;; Input not related, continue
          (recur current-circuit (rest components-to-check)))))))

(defn set-input [circuit wire-id new-value]
  (let [old-value (get-in circuit [:wires wire-id])]
    (if (= old-value new-value)
      circuit
      (-> circuit
          (assoc-in [:wires wire-id] new-value)
          (propagate-change wire-id)))))

(def delayed-circuit-state
  {:components {}
   :wires      {}
   :event-queue '() ; e.g., '({:time 5 :wire-id "w1" :new-value 1} {:time 10 ...})
   :time       0})

(def gate-delay 1) ; Uniform delay for simplicity

(defn schedule-event [circuit time wire-id new-value]
  (update circuit :event-queue conj {:time time :wire-id wire-id :new-value new-value}))

(defn evaluate-gate-with-delay [circuit comp-data current-time]
  (let [new-value (evaluate-gate circuit comp-data)
        output-wire-id (:output comp-data)
        old-value (get-in circuit [:wires output-wire-id])]
    (if (not= old-value new-value)
      ;; Schedule the change for the future
      (schedule-event circuit (+ current-time gate-delay) output-wire-id new-value)
      circuit)))

  (defn process-event [circuit event]
  (let [{:keys [time wire-id new-value]} event
        current-value (get-in circuit [:wires wire-id])]
    (if (= current-value new-value)
      ;; Value is already the expected value (e.g., another event superseded this one)
      circuit
      ;; Value changed, update wire, update time, and re-evaluate all dependent gates
      (let [next-circuit (-> circuit
                             (assoc :time time)
                             (assoc-in [:wires wire-id] new-value))]
        (reduce (fn [acc-circuit [comp-id comp-data]]
                  (if (some #{wire-id} (:inputs comp-data))
                    (evaluate-gate-with-delay acc-circuit comp-data time)
                    acc-circuit))
                next-circuit
                (:components next-circuit))))))

  (defn run-simulation-step [circuit]
    (let [sorted-events (sort-by :time (:event-queue circuit))
        next-event (first sorted-events)]
    (if next-event
      (-> circuit
          (assoc :event-queue (rest sorted-events))
          (process-event next-event))
      ;; No more events
      circuit)))

(defn simulate [circuit max-steps]
  (loop [state circuit
         step 0]
    (if (or (> step max-steps) (empty? (:event-queue state)))
      state
      (recur (run-simulation-step state) (inc step)))))
