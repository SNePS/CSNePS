(in-ns 'csneps.core.snuser)

(defn clearkb
  [& clear?]
  (let [clearall (first clear?)]

    ;(snip/resetExecutor)
    
      ;; Initialize Contexts
      (dosync 
        (ref-set ct/CONTEXTS (hash-map))
      
      ;;Setup default contexts
      (ct/defineContext 'BaseCT
                         :docstring "The root of all the contexts."
                         :parents nil)
      (ct/defineContext 'DefaultCT
                         :docstring "The default current context."
                         :parents '(BaseCT))
      (ct/setCurrentContext 'DefaultCT)

      ;;Initialize the Semantic Type hierarchy
      (csneps/initialize-default-hierarchy)

      ;;Remove term-type maps from semantic types.
      (dosync
        (ref-set csneps/type-map (hash-map))
        (ref-set csneps/support-set (hash-map))
        (ref-set csneps/supported-nodes-set (hash-map))
        (ref-set csneps/primaction (hash-map)))


      ;; Initialize the set of terms.
      (dosync (ref-set csneps/WFTCOUNT 0))
      (dosync (ref-set csneps/TERMS (hash-map)))

      ;; Initialize the set of arbitraries.
      (dosync (ref-set csneps/ARBITRARIES #{}))
      (dosync (ref-set csneps/ARBCOUNT 0))

      ;; Initialize the set of indefinites.
      (dosync (ref-set csneps/INDEFINITES #{}))
      (dosync (ref-set csneps/INDCOUNT 0))

      ;; Reinitialize unification tree.
      (dosync (ref-set build/DistNodes {}))


      (doseq [cf (seq @cf/CASEFRAMES)]
        (dosync (ref-set (:terms cf) (hash-set))))


        ;; Needs to erase pointers to terms in the caseframes themselves
    ;    (set:loopset for cf in cf:*CASEFRAMES*
    ;                 do (setf (cf::caseframe-terms cf)
    ;                      (util:make-resource :value (set:new-set))))


      (if clearall
        (do

          ;; Reinitialize slots
          (ref-set slot/SLOTS (hash-map))

            ;; Slots for built-in Propositions
            ;; ===================================
            (defineSlot class :type Category
              :docstring "Points to a Category that some Entity is a member of."
              :negadjust reduce)
            (defineSlot member :type Entity
              :docstring "Points to the Entity that is a member of some Category."
              :negadjust reduce)
            (defineSlot equiv :type Entity
              :docstring "All fillers are coreferential."
              :min 2 :negadjust reduce
              :path (compose ! equiv (kstar (compose equiv- ! equiv))))
            
              ;; Slots for Rules
            ;; ===================
              (defineSlot and :type Proposition
                :docstring "Fillers are arguments of a conjunction."
                :min 2 :posadjust reduce :negadjust expand)
              (defineSlot nor :type Proposition
                :docstring "Fillers are arguments of a nor."
                :min 1 :posadjust reduce :negadjust expand)
              (defineSlot andorargs :type Proposition
                :docstring "Fillers are arguments of an andor."
                :min 2 :posadjust none :negadjust none)
              (defineSlot threshargs :type Proposition
                :docstring "Fillers are arguments of a thresh."
                :min 1 :posadjust none :negadjust none)
              (defineSlot thnor :type Proposition
                :docstring "Fillers are arguments of a thnor."
                :min 1 :posadjust reduce :negadjust reduce)
              (defineSlot ant :type Proposition
                :docstring "antecedent for a set."
                :min 1 :posadjust expand :negadjust reduce)
              (defineSlot cq :type Proposition
                :docstring "consequent for a set."
                :min 1 :posadjust reduce :negadjust expand)
              
              ;; Slots for SNeRE
              ;; ===================
              (defineSlot actions :type Action
                :docstring "The actions of an act."
                :min 1 :max 1
                :posadjust none :negadjust none)

              ;; Reinitialize caseframes
              (ref-set cf/CASEFRAMES (hash-set))
              (ref-set cf/FN2CF (hash-map))
              (ref-set cf/NoviceCaseframes (hash-map))
              (defineCaseframe 'Proposition  '('Isa member class)
                :docstring "[member] is a [class]")
              (defineCaseframe 'Proposition '('Equiv equiv)
                :docstring "[equiv] are all co-referential")
              (defineCaseframe 'Proposition '('and and)
                :docstring "it is the case that [and]")
              (defineCaseframe 'Proposition '('nor nor)
                :docstring "it is not the case that [nor]")
              (defineCaseframe 'Proposition '('thnor thnor)
                :docstring "I don't know that it is the case that [thnor]")
              (defineCaseframe 'Proposition '('andor andorargs))
              (defineCaseframe 'Proposition '('thresh threshargs))
              (defineCaseframe 'Proposition '('if ant cq)
                :docstring "if [ant] then [cq]"))
      ))

      (if clearall
        (println "Knowledge Base cleared. Contexts, slots, caseframes, and semantic types reinitialized.")
        (println "Knowledge Base cleared. Contexts reinitialized."))
    )
)




;(println "Change Package to csneps.core.snuser")