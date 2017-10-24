;;; Index of CSNePS Demo Files

;;; The contents of this file are subject to the University at Buffalo
;;; Public License Version 1.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the
;;; License at http://www.cse.buffalo.edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;;; the License for the specific language governing rights and limitations
;;; under the License.
;;; 
;;; The Original Code is CSNePS.
;;; 
;;; The Initial Developer of the Original Code is Research Foundation of
;;; State University of New York, on behalf of University at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C) 2007
;;; Research Foundation of State University of New York, on behalf of
;;; University at Buffalo. All Rights Reserved.
;;; 
;;; Contributor(s): ______________________________________.

(in-ns 'csneps.demo)

(def demoindex [["Basic - Demonstration of basic CSNePS functionality." "basic-demo.sneps"],
                ["Example of function-valued functions." "refcl.sneps"],
                ["Chng - Demonstration of contextually determining, and changing semantic types." "changedemo.sneps"],
                ["Demonstration of Sort-based inference" "sort-based-derivable.sneps"],
                ["Demonstration of Path-based inference" "pb-inference.sneps"],
                ["Demonstration of Natural Deduction" "natural-deduction-derivable.sneps"],
                ["Demonstration of Natural Deduction with xor" "xor.sneps"],
                ["Demonstration of Andor and Thresh Introduction" "andor.sneps"],
                ["Demonstration of Thresh Elimination" "threshelim.sneps"],
                ["Demonstration of Equivalence Elimination and Introduction" "equivalence.sneps"],
                ["Demonstration of combined Slot-Based Inference and And Elimination" "inferdemo.sneps"],
                ["Using Slot-Based Inference to derive rules" "sbConnectives.sneps"],
                ["Cutting Infinite Recursion" "recursion.sneps"],
                ["Negation by Failure" "negbyfail.sneps"],
                ["SNeRE" "snere.sneps"],
                ["Demonstration of using find (for SNePS Developers)." "finddemo.sneps"],
                ["Demonstration of building arbitrary individuals." "vardemo.sneps"]])
