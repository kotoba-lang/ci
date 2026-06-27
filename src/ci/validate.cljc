(ns ci.validate
  "Structural validation of a CI workflow EDN model. Pure: returns a vector of
  problem maps `{:ci/severity :error|:warn :ci/code … :ci/id … :ci/msg …}` so
  a caller decides how to surface them. `valid?` is true iff there are no
  :error-level problems (warnings are advisory)."
  (:require [ci.model :as m]))

(defn- problem [severity code id msg]
  {:ci/severity severity :ci/code code :ci/id id :ci/msg msg})

(defn problems
  "Return a vector of structural problems with `model`."
  [model]
  (let [job-ids (set (keys (m/jobs model)))
        ps      (transient [])]
    (doseq [[id job] (m/jobs model)]
      ;; each :ci/needs entry must reference an existing job
      (doseq [need (:ci/needs job)]
        (when-not (contains? job-ids need)
          (conj! ps (problem :error :job/dangling-need id
                             (str "job \"" id "\" needs unknown job \"" need "\"")))))
      ;; missing :ci/runs-on is a warning
      (when-not (:ci/runs-on job)
        (conj! ps (problem :warn :job/missing-runs-on id
                           (str "job \"" id "\" has no :ci/runs-on"))))
      ;; each step must have EXACTLY one of :ci/uses / :ci/run
      (doseq [step (:ci/steps job)]
        (let [has-uses (contains? step :ci/uses)
              has-run  (contains? step :ci/run)]
          (cond
            (and has-uses has-run)
            (conj! ps (problem :error :step/uses-and-run id
                               (str "job \"" id "\" has a step with both :ci/uses and :ci/run")))
            (and (not has-uses) (not has-run))
            (conj! ps (problem :error :step/no-action id
                               (str "job \"" id "\" has a step with neither :ci/uses nor :ci/run")))))))
    ;; job DAG must be acyclic
    (let [order (m/topo-order model)]
      (when (map? order)
        (conj! ps (problem :error :dag/cycle nil
                           (str "job DAG contains a cycle among: "
                                (pr-str (:ci/cycle order)))))))
    (persistent! ps)))

(defn errors
  "Return only the :error-severity problems."
  [model]
  (filterv #(= :error (:ci/severity %)) (problems model)))

(defn valid?
  "True iff `model` has no :error-level structural problems."
  [model]
  (empty? (errors model)))
