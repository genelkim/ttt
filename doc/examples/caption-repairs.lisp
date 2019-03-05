;; Extensively commented by Len 
(defparameter *repair-rules-for-refined-charniak-parse*
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Rules from Adam Purtee's "caption-repairs.lisp" file

'(
; "Raise" locative PP postmodifying a final conjunct so that it modifies
; the entire conjunction 
; - changed from <> to + in rhs template
;(/ (_+ (NP (+ (<> (NP _+1) (? (|,| |,|)))) 
;           (CC and) (NP (NP _+2) (! (PP? locative-p? _+3)))) _*) 
;   (_+ (NP <> (CC and) (NP _+2)) ! _*))
(/ (_+ (NP (+ (<> (NP _+1) (? (|,| |,|)))) 
           (CC and) (NP (NP _+2) (! (PP? locative-p? _+3)))) _*) 
   (_+ (NP + (CC and) (NP _+2)) ! _*))

; For a PP whose NP is postmodified by another PP, separate off that other PP
; (which probably postmodifies the same phrase head as the first PP)
(/ (_+ ((!1 PP?) (IN _!) (NP (NP _+1) ((!2 PP?) _+2))) _*) 
   (_+ (!1  (IN _!) (NP _+1)) (!2 _+2) _*))

; For a fragment consisting of an NP and a PP (possibly with a separating
; comma), attach the PP to the NP
(/ (FRAG (NP _+) (? (|,| |,|)) (PP? _+1) _*)
   (FRAG (NP (NP _+) ? (PP? _+1)) _*))

; For a PP whose NP is a conjunction of a simple nonhuman NP with a complex 
; human NP, with a comma between them, separate off the coordinator and human
; NP (thus yielding a PP with a nonhuman NP, followed by a comma, followed by
; by a (complex) human NP, presumably belonging to a larger NP-conjunction) 
(/ (_+ (PP? (IN _!) (NP (NP _* nn-nonhuman?) (|,| |,|) (CC and) 
                        (NP (NP _*1 nn-human?) _+1))) _*2) 
   (_+ (PP? (IN _!) (NP _* nn-nonhuman?)) (|,| |,|) (CC and) 
       (NP (NP _*1 nn-human?) _+1) _*2))

; Same as the previous rule, with "human" and "nonhuman" interchanged 
(/ (_+ (PP? (IN _!) (NP (NP _* nn-human?) (|,| |,|) (CC and) 
                        (NP (NP _*1 nn-nonhuman?) _+1))) _*2) 
   (_+ (PP? (IN _!) (NP _* nn-human?)) (|,| |,|) (CC and) 
       (NP (NP _*1 nn-nonhuman?) _+1) _*2))

; Same as the second-last of the above rules, except that the human NP
; conjunct is simple
(/ (_+ (PP? (IN _!) (NP (NP _* nn-nonhuman?) (|,| |,|) (CC and) 
                        (NP _*1 nn-human?) _+1)) _*2) 
   (_+ (PP? (IN _!) (NP _* nn-nonhuman?)) (|,| |,|) (CC and) 
       (NP _*1 nn-human?) _+1 _*2))

; Same as the second-last of the above rules, except that the nonhuman NP
; conjunct is simple
(/ (_+ (PP? (IN _!) (NP (NP _* nn-human?) (|,| |,|) (CC and) 
                        (NP _*1 nn-nonhuman?) _+1)) _*2) 
   (_+ (PP? (IN _!) (NP _* nn-human?)) (|,| |,|) (CC and) 
       (NP _*1 nn-nonhuman?) _+1 _*2))

; For an NP consisting of a proper NP, possibly followed by a comma, 
; followed by a complex NP of form 
;    (NP proper-NP , proper-NP and (NP proper-NP PP) additional-material),
; form a new complex NP that coordinates the 4 proper NPs, including the
; commas and coordinator (in the original order), followed by the PP and
; additional material treated as postmodifiers of the conjunction of the
; 4 proper NPs
(/ (_+ (NP (<> (NP _* (NNP _!)) (? (|,| |,|))) 
           (NP (NP (<>1 (NP _* (NNP _!)) (|,| |,|) (NP _* (NNP _!)) (CC AND)) 
                   (NP (! (NP _* (NNP _!))) PPT?)) _+1) _+2)) 
   (_+ (NP (NP <> <>1 !) PPT? _+1 _+2)))

; For an adjective phrase with a PP postmodifier (possibly with an added
; comma), possibly followed by further material, break away the PP (and
; possible comma) from the adjective phrase, so that these parts are now
; at the same structural level as the adjective phrase and any further
; material
(/ (_+ (ADJP _+1 (<> (PP? _+2) (? (|,| |,|)))) _*) 
   (_+ (ADJP _+1) <> _*))

; For a PP consisting of "with" and a sentence starting with an NP and
; a VP, combine the "with" and NP into a (nonclausal) PP, followed by
; the VP and any additional material. [Note: this can certainly introduce
; errors in phrases like "Mary lecturing, with John listening intently";
; it might be a good idea to ensure that "with" isn't preceded by a comma.]
(/ (_* (PP-WITH (IN with) (S (NP _+) (VP _+1) _*1)) _*2) 
   (_* (PP-WITH (IN with) (NP _+)) (VP _+1) _*1 _*2))

; For a PP whose NP is a proper NP postmodified by another PP, detach
; the second PP from the proper NP, so that we are left with two successive
; PPs (and extra material before and possibly after)
(/ (_+ ((! PP?) (IN _!) (NP (NP _* ((!1 NNP  NNPS) _+1)) ((!2 PP?) _+2))) _*1) 
   (_+ (! (IN _!) (NP _* (!1 _+1))) (!2 _+2) _*1))


 )); end of *repair-rules-for-refined-charniak-parse*
