(in-ns 'csneps.core.snuser)

(defn clearkb
  [& clear?]
  (dosync 
    (let [clearall (first clear?)]
      ;; Stop any ongoing inference. 
      (igc/resetExecutor)
      
      ;; Initialize Contexts
      (ref-set ct/CONTEXTS (hash-map))
      
      ;;Setup default contexts
      (ct/defineContext 'BaseCT
        :docstring "The root of all the contexts."
        :parents nil)
      (ct/defineContext 'OntologyCT
        :docstring "Context containing the semantic type ontology and other ontological
                    assertions which are not subject to belief revision.")
      (ct/defineContext 'DefaultCT
        :docstring "The default current context."
        :parents '(OntologyCT))
      ;; Set current context after the dosync, since 
      ;; it's side-effect producing. 

      ;;Remove term-type maps from semantic types.
      (ref-set csneps/type-map (hash-map))
      (ref-set csneps/support-set (hash-map))
      (ref-set csneps/supported-nodes-set (hash-map))
      (ref-set csneps/primaction (hash-map))


      ;; Initialize the set of terms.
      (ref-set csneps/WFTCOUNT 0)
      (ref-set csneps/TERMS (hash-map))

      ;; Initialize the set of arbitraries.
      (ref-set csneps/ARBITRARIES #{})
      (ref-set csneps/ARBCOUNT 0)

      ;; Initialize the set of indefinites.
      (ref-set csneps/INDEFINITES #{})
      (ref-set csneps/INDCOUNT 0)
      
      ;; Initialize the set of question mark variables.
      (ref-set csneps/QVARS #{})
      (ref-set csneps/QVARCOUNT 0)
      
      ;; Reset term parts:
      (ref-set csneps/i-channels {})
			(ref-set csneps/u-channels {})
			(ref-set csneps/g-channels {})
			(ref-set csneps/ant-in-channels {})
			(ref-set csneps/future-fw-infer {})
			(ref-set csneps/instances {})
			(ref-set csneps/expected-instances {})
			(ref-set csneps/up-cablesetw {})
			(ref-set csneps/support {})
			(ref-set csneps/msgs {})
			(ref-set csneps/restriction-set {})
			(ref-set csneps/dependencies {})
			(ref-set csneps/lattice-node {})
			(ref-set csneps/down-cableset {})
			(ref-set csneps/term-caseframe-map {})
   
      ;; Clear cache used by IG.
      (memo-clear! snip/hyp-subst-of-ct?)

      ;; Reinitialize unification tree.
      (build/reset-tree)
      
      ;; Clear the subsumption lattice
      (build/reset-lattice)      

      ;; Remove terms from frames.
      (doseq [cf (seq @cf/CASEFRAMES)]
        (ref-set (:terms cf) (hash-set)))

      ;; Reset slots/frames
      (when clearall         
        ;;Initialize the Semantic Type hierarchy
        (build/initialize-default-hierarchy)
        
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
        ;(defineSlot closedvar :type Entity
        ;  :docstring "Points to a variable in a closure.")
        (defineSlot proposition :type Propositional
          :docstring "Points to a proposition.")
        
        ;; Slots for Rules
        ;; ===================
        (defineSlot and :type Propositional
          :docstring "Fillers are arguments of a conjunction."
          :min 2 :posadjust reduce :negadjust expand)
        (defineSlot nor :type Propositional
          :docstring "Fillers are arguments of a nor."
          :min 1 :posadjust reduce :negadjust expand)
        (defineSlot andorargs :type Propositional
          :docstring "Fillers are arguments of an andor."
          :min 2 :posadjust none :negadjust none)
        (defineSlot threshargs :type Propositional
          :docstring "Fillers are arguments of a thresh."
          :min 1 :posadjust none :negadjust none)
        (defineSlot thnor :type Propositional
          :docstring "Fillers are arguments of a thnor."
          :min 1 :posadjust reduce :negadjust reduce)
        (defineSlot ant :type Propositional
          :docstring "antecedent for a set."
          :min 1 :posadjust expand :negadjust reduce)
        (defineSlot cq :type Propositional
          :docstring "consequent for a set."
          :min 1 :posadjust reduce :negadjust expand)
        
        ;; Slots for SNeRE
        ;; ===================
        (defineSlot action :type Action
          :docstring "The actions of an act."
          :min 1 :max 1
          :posadjust none :negadjust none)
        
        ;; Slots for condition-action rules
        ;; ================================
        (defineSlot condition :type Propositional 
          :docstring "conditions for a rule."
          :min 1 :posadjust expand :negadjust reduce)
        (defineSlot rulename :type Thing
          :docstring "The name of a rule."
          :min 1 :max 1 :posadjust none :negadjust none)
        (defineSlot subrule :type Policy
          :docstring "subrules for a rule."
          :min 0 :posadjust expand :negadjust reduce)
        
        ;; Reinitialize caseframes
        (ref-set cf/CASEFRAMES (hash-set))
        (ref-set cf/FN2CF (hash-map))
        (ref-set cf/NoviceCaseframes (hash-map))
        (defineCaseframe 'Propositional  '('Isa member class)
          :docstring "[member] is a [class]")
        (defineCaseframe 'Propositional '('Equiv equiv)
          :docstring "[equiv] are all co-referential")
        (defineCaseframe 'Propositional '('and and)
          :docstring "it is the case that [and]")
        (defineCaseframe 'Propositional '('nor nor)
          :docstring "it is not the case that [nor]")
        (defineCaseframe 'Propositional '('thnor thnor)
          :docstring "I don't know that it is the case that [thnor]")
        (defineCaseframe 'Propositional '('andor andorargs))
        (defineCaseframe 'Propositional '('thresh threshargs))
        (defineCaseframe 'Propositional '('if ant cq)
          :docstring "if [ant] then [cq]")
        (defineCaseframe 'Propositional '('close proposition)
          :docstring "[proposition] is closed over [closedvar]")
        (defineCaseframe 'Policy '('rule rulename condition action subrule)
          :docstring "for the rule [name] to fire, [condition] must be matched,
                      then [action] may occur, and [subrule] may be matched."))
      ))
  
  (build/initial-semtypes-to-obj-lang)
  
  ;; Do this outside the dosync (see above).
  (ct/setCurrentContext 'DefaultCT)

  ;; Output message.
  (if (first clear?)
    (println "Knowledge Base cleared. Contexts, slots, caseframes, and semantic types reinitialized.")
    (println "Knowledge Base cleared. Contexts reinitialized.")))