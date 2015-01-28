(ns italianverbs.english_rt)

(defn inflection [inflection]
  "turn a keyword describing an inflection (e.g. past, present, future) into a specification."
  ;; TODO: gen.js should use this rather than its own implementation.
  (cond 
   (= inflection :conditional)
   {:synsem {:infl :conditional}}
   
   (= inflection :future)
   {:synsem {:infl :futuro}}

   (= inflection :imperfect)
   {:synsem {:infl :imperfetto}}

   (= inflection :passato)
   {:synsem {:sem {:aspect :perfect
                   :tense :past}}}
        
   (= inflection :present)
   {:synsem {:infl :present
             :sem {:tense :present}}}
        
   ;; no constraints: generate anything.
   true
   :top))



