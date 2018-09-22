(deftemplate dogInfo 
    (declare (from-class dogInfo)
        (slot-specific TRUE)
        (include-variables TRUE)))

(definstance  dogInfo  (new dogInfo))

(defglobal 
    ?*breed* = ""
    ?*age* = nil
    ?*weight* = nil
    ?*dogname* = ""
    ?*name* = ""
    ?*recommend* = nil
    ?*next* = ""
    ?*deworm* =""
    ?*vaccine* =""
    ?*crlf* ="
"
    )



(deftemplate dog
    (slot age(type INTEGER))
    (slot breed (allowed-values small medium large))
    (slot weight(type INTEGER))
    (slot deworming(allowed-values yes no))
    (slot vaccine(allowed-values yes no))
    )



(deftemplate question
    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer
    (slot ident)
    (slot text))


(deffunction is-of-type (?answer ?type)
    "validation"
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
        else (if (eq ?type number) then
            (return (numberp ?answer))
            else (if (eq ?type breed-size) then
                (return (or(eq ?answer small)(eq ?answer medium)(eq ?answer large)))
            else (return (> (str-length ?answer) 0))
            )
        )
    )
    )

(deffunction ask-user (?question ?type)
    "Ask a question, and return the answer"
    (bind ?answer "")
    (while (not (is-of-type ?answer ?type)) do
        (printout t ?question " ")
        (if (eq ?type yes-no) then
            (printout t crlf"(yes or no) "crlf))
        (if (eq ?type breed-size) then
            (printout t crlf"small medium large"crlf))
        (bind ?answer (read)))
    (return ?answer))


(defmodule ask)
(defrule ask::ask-question-by-id
    "Ask a question and assert the answer"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?id) (text ?text) (type ?type))
    (not (MAIN::answer (ident ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (ask-user ?text ?type))
    (assert (MAIN::answer (ident ?id) (text ?answer)))
    (retract ?ask)
    (return))

    
(defmodule start)
(defrule welcome
    =>
    (printout t crlf "********************* Welcome to First time Dog-owner's Expert Guide *********************" crlf)
    (printout t "If you are a first time owner of dog and wondering how to take best care of your new friend."?*crlf*"Well don't worry I'm here to help you with all things you need to take care of your pet!!" crlf crlf)
    (printout t "Even if you are experienced dog owner I can still provide some imporatant information you might have missed" crlf)
    (printout t "Let's first start with your name!!"crlf" (You can use an alias instead of your name)  " crlf)
    (bind ?*name* (read))
    (printout t "What is the name of your pet dog, "?*name* " ?" crlf"(You can use an alias instead of your dog's name)"crlf)
    (bind ?*dogname* (read))
    (printout t crlf "Wow "?*dogname* " sounds such lively name!! Alright let's get started "?*name* "." crlf)
    (printout t "I would first require to get following details of "?*dogname*". But now make sure you provide correct information. " crlf crlf))

;The questions to be asked
(deffacts questions
    "The questions that are asked to user."
    (question (ident age) (type number)
        (text "What is age of your dog? Make sure you enter the age in NUMBER format and in MONTHS: (MAX: 240)"))
    (question (ident breed) (type breed-size)
        (text "What is the breed size of your dog? If your are not aware Google breed size of your DOG. Valid Options:"))
    (question (ident weight) (type number)
        (text "What is the weight of your dog (in kgs)?  (Max: 80)"))
    (question (ident deworm) (type yes-no)
        (text "Have you done deworming of your dog?"))
    (question (ident vaccine) (type yes-no)
        (text "Have you done vaccination of your dog?"))
    )

/* Ask question and get the answer*/
(defmodule request-dog-details)
(defrule request-age
    (declare (salience 30))
    =>
    (assert (ask age)))

(defrule request-breed
    (declare (salience 29))
    =>
    (assert (ask breed)))

(defrule request-weight
    (declare (salience 28))
    =>
    (assert (ask weight)))

(defrule request-deworm
    (declare (salience 27))
    =>
    (assert (ask deworm)))

(defrule request-vaccine
    (declare (salience 26))
    =>
    (assert (ask vaccine)))



(defrule assert-dog-fact
    (answer (ident age) (text ?a))
    (answer (ident breed) (text ?b))
    (answer (ident weight) (text ?w))
    (answer (ident deworm) (text ?d))
    (answer (ident vaccine) (text ?v))
    (dogInfo (OBJECT ?dObj))
    =>
    (bind ?*age* ?a)
    (bind ?*breed* ?b)
    (bind ?*weight* ?w)
    (bind ?*deworm* ?d)
    (bind ?*vaccine* ?v)
    (?dObj setAge ?*age*)
    (?dObj setBreedSize ?*breed*)
    (?dObj setWeight ?*weight*)
    (?dObj setDeworm ?*deworm*)
    (?dObj setVaccine ?*vaccine*)  
    )

    
(defmodule dog-recommendation)

;Dog's stage 
(defrule rule-1
    "If age is greater than 2 months and less than 6 months"
    (declare (salience 100))
    (dogInfo (age ?e&:(>= ?e 2)&:(< ?e 6))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf)
	(printout t "Firstly it's important to know that your dog is currently in Weaning stage which begins after 2 months till 6 months"crlf"You can now start to feed him dry food and take him off mother's milk" crlf)
    (printout t crlf "Once you finish reading press a CHARACTER and then ENTER to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-2
    "If age is less than 2 months"
    (declare (salience 99))
    (dogInfo (age ?e&:(< ?e 2))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf)
	(printout t "Firstly it's important to know that the dog is still a puppy and less than 2 months and not reached weaning stage."?*crlf*"You need to keep your dog on mother's milk" crlf)
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-3
    "If age is more than 6 and less than 12 months"
    (declare (salience 98))
    (dogInfo (age ?e&:(>= ?e 6)&:(< ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf) 
	(printout t "The dog is currently in Growing stage that is from puppy to adult and will become a complete adult at age 12 months" crlf)
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-4
    "If age is 12 months to more"
    (declare (salience 97))
    (dogInfo (age ?e&:(>= ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf) 
	(printout t "Your dog is has already reached 12 months and is an Adult stage" crlf)
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

;Recommeneded weight
(defrule rule-5
    "If age is less than 3 months"
    (declare (salience 96))
    (dogInfo (breedSize ?b)(age ?e&:(< ?e 3))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (eq ?b "small") then
        (printout t "Your dog is a small breed and is less than 3 months old."crlf"The recommended weight is 3 kgs.")
        (bind ?*recommend* 3)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "medium") then
        (printout t "Your dog is a medium breed and is less than 3 months old."crlf"The recommended weight is 5 kgs.")
        (bind ?*recommend* 5)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "large") then
        (printout t "Your dog is a large breed and is less than 3 months old."crlf"The recommended weight is 9 kgs.")
        (bind ?*recommend* 9)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    	(printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    	(bind ?*next* (read t))
    )

(defrule rule-6
    "If age is between 3 to 6 months"
    (declare (salience 95))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 3)&:(< ?e 6))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (eq ?b "small") then
        (printout t "Your dog is a small breed and is between 3 to 6 months old."crlf"The recommended weight is 5 kgs.")
        (bind ?*recommend* 5)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "medium") then
        (printout t "Your dog is a medium breed and is between 3 to 6 months old."crlf"The recommended weight is 11 kgs.")
        (bind ?*recommend* 11)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "large") then
        (printout t "Your dog is a large breed and is between 3 to 6 months old."crlf"The recommended weight is 16 kgs.")
		(bind ?*recommend* 16)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    	(printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    	(bind ?*next* (read t))
    )


(defrule rule-7
    "If age is 6 to 12 months"
    (declare (salience 94))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 6)&:(< ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (eq ?b "small") then
        (printout t "Your dog is a small breed and is between 6 to 12 months old."crlf"The recommended weight is 11 kgs.")
        (bind ?*recommend* 11)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "medium") then
        (printout t "Your dog is a medium breed and is between 6 to 12 months old."crlf"The recommended weight is 22 kgs.")
		(bind ?*recommend* 22)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "large") then
        (printout t "Your dog is a large breed and is between 6 to 12 months old."crlf"The recommended weight is 35 kgs.")
        (bind ?*recommend* 35)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    	(printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    	(bind ?*next* (read t))
    )

(defrule rule-8
    "If age is more than 12 months"
    (declare (salience 93))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (eq ?b "small") then
        (printout t  "Your dog is a small breed and above 12 months ."crlf"The recommended weight is 15 kgs.")
		(bind ?*recommend* 15)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "medium") then
        (printout t "Your dog is a medium breed and above 12 months old."crlf"The recommended weight is 29 kgs.")
		(bind ?*recommend* 29)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (eq ?b "large") then
        (printout t "Your dog is a large breed and above 12 months old."crlf"The recommended weight is 48 kgs.")
		(bind ?*recommend* 48)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


;Food quantity chart
(defrule rule-9
    "If age is 2 to 6 months"
    (declare (salience 92))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 2)&:(< ?e 6))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food quantity:" crlf) 
	(printout t "Since Your Dog is in weaning stage it needs a lot of food" crlf)
    (printout t crlf "FUN FACT: Puppies need more food than an adult dog") 
    (if (eq ?b "small") then
        (printout t crlf crlf"Your dog is a small breed and is between 2 to 6 months old."crlf"The recommended food quantity is 0.44 kgs per day."crlf crlf))
    (if (eq ?b "medium") then
        (printout t crlf crlf "Your dog is a medium breed and is between 2 to 6 months old."crlf"The recommended food quantity is 0.97 kgs per day."crlf crlf))
    (if (eq ?b "large") then
        (printout t crlf crlf "Your dog is a large breed and is between 2 to 6 months old."crlf"The recommended food quantity is 1.42 kgs per day."crlf crlf))
    (printout t "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-10
    "If age is 6 to 12 months"
    (declare (salience 91))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 6)&:(< ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food quantity:" crlf crlf) 
	(printout t "Since Your Dog is in Growing stage and needs to eat in the recommend quantity to avoid getting overweight at adulthood" crlf)
    (printout t crlf "FUN FACT: Puppies need more food than an adult dog") 
    (if (eq ?b "small") then
        (printout t crlf crlf "Your dog is a small breed and is between 6 to 12 months old."crlf"The recommended food quantity is 0.32 kgs per day."crlf crlf))
    (if (eq ?b "medium") then
        (printout t crlf crlf "Your dog is a medium breed and is between 6 to 12 months old."crlf"The recommended food quantity is 0.65 kgs per day."crlf crlf))
    (if (eq ?b "large") then
        (printout t crlf crlf "Your dog is a large breed and is between 6 to 12 months old."crlf"The recommended food quantity is 1.04 kgs per day."crlf crlf))
    (printout t "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-11
    "If age above 12 months"
    (declare (salience 90))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food quantity:" crlf crlf) 
	(printout t "Since Your Dog is in Adult and needs to have a balanced diet" crlf)
    (printout t crlf "FUN FACT: Puppies need more food than an adult dog") 
    (if (eq ?b "small") then
        (printout t crlf crlf "Your dog is a small breed and is above 12 months old."crlf"The recommended food quantity is 0.29 kgs per day."crlf crlf))
    (if (eq ?b "medium") then
        (printout t crlf crlf"Your dog is a medium breed and is above 12 months old."crlf"The recommended food quantity is 0.57 kgs per day."crlf crlf))
    (if (eq ?b "large") then
        (printout t crlf crlf"Your dog is a large breed and is above 12 months old."crlf"The recommended food quantity is 0.95 kgs per day."crlf crlf))
    (printout t "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

;Food Number of time per day

(defrule rule-12
    "If breed is small"
    (declare (salience 89))
    (dogInfo (breedSize ?b&:(eq ?b "small"))(age ?e)(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "It is also important to know how many time a day to feed your dog" crlf)
    (printout t crlf "A puppy eats more times a day than an adult dog" crlf "Reason being that puppy's digestive system is still not fully developed" crlf)
	(printout t crlf "NOTE: You need to equally divide the recommend food quantity per day into the number of times feeding is required." crlf)
    (printout t crlf "Your total intake should not cross recommended food quantity (kgs per day)"crlf)
    (if (< ?e 3) then
        (printout t  "Your dog is a small breed and is still puppy less than 3 months."crlf"I recommed feeding your dog THREE TO FOUR TIMES a day"crlf))
    (if (and(>= ?e 3)(< ?e 6)) then
        (printout t "Your dog is a small breed and is between 3 to 6 months old."crlf"I recommed feeding your dog THREE TIMES a day"crlf))
    (if (and(>= ?e 6)(< ?e 132)) then
        (printout t "Your dog is a small breed and is between 6 months to 11 years old."crlf"I recommed feeding your dog TWO TIMES a day"crlf))
    (if (>= ?e 132) then
        (printout t  "Your dog is a small breed and is above 11 years and is now a senior dog."crlf"I recommed feeding your dog ONLY ONCE a day"crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


(defrule rule-13
    "If breed is medium"
    (declare (salience 88))
    (dogInfo (breedSize ?b&:(eq ?b "medium"))(age ?e)(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "It is also important to know how many time a day to feed your dog" crlf)
    (printout t crlf "A puppy eats more times a day than an adult dog" crlf "Reason being that puppy's digestive system is still not fully developed" crlf)
	(printout t crlf "NOTE: You need to equally divide the recommend food quantity per day into the number of times feeding is required." crlf)
    (printout t crlf "Your total intake should not cross recommended food quantity (kgs per day)"crlf)
    (if (< ?e 3) then
        (printout t  "Your dog is a medium breed and is still puppy less than 3 months."crlf"I recommed feeding your dog THREE TO FOUR TIMES a day"crlf))
    (if (and(>= ?e 3)(< ?e 6)) then
        (printout t "Your dog is a medium breed and is between 3 to 6 months old."crlf"I recommed feeding your dog THREE TIMES a day"crlf))
    (if (and(>= ?e 6)(< ?e 120)) then
        (printout t "Your dog is a medium breed and is between 6 months to 10 years old."crlf"I recommed feeding your dog TWO TIMES a day"crlf))
    (if (>= ?e 120) then
        (printout t  "Your dog is a small breed and is above 10 years and is now a senior dog."crlf"I recommed feeding your dog ONLY ONCE a day"crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-14
    "If breed is large"
    (declare (salience 87))
    (dogInfo (breedSize ?b&:(eq ?b "large"))(age ?e)(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "It is also important to know how many time a day to feed your dog" crlf)
    (printout t crlf "A puppy eats more times a day than an adult dog" crlf "Reason being that puppy's digestive system is still not fully developed" crlf)
	(printout t crlf "NOTE: You need to equally divide the recommend food quantity per day into the number of times feeding is required." crlf)
    (printout t crlf "Your total intake should not cross recommended food quantity (kgs per day)"crlf)
    (if (< ?e 3) then
        (printout t  "Your dog is a large breed and is still puppy less than 3 months."crlf"I recommed feeding your dog THREE TO FOUR TIMES a day"crlf))
    (if (and(>= ?e 3)(< ?e 6)) then
        (printout t "Your dog is a large breed and is between 3 to 6 months old."crlf"I recommed feeding your dog THREE TIMES a day"crlf))
    (if (and(>= ?e 6)(< ?e 108)) then
        (printout t "Your dog is a large breed and is between 6 months to 9 years old."crlf"I recommed feeding your dog TWO TIMES a day"crlf))
    (if (>= ?e 108) then
        (printout t  "Your dog is a large breed and is above 9 years and is now a senior dog."crlf"I recommed feeding your dog ONLY ONCE a day"crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

; Recommended food brands


(defrule rule-15
    "If age is 2 to 12 months"
    (declare (salience 86))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 2)&:(< ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food brands:" crlf crlf) 
	(printout t "Since Your Dog is not an adult and needs to get nutrients essential from weaning till adult stage" crlf)
    (printout t crlf "Most dogs food companies manufacture food for both puppy and adult dog") 
    (if (eq ?b "small") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Wellness Complete Health Small Breed Natural Dry Puppy Food"crlf"2)Blue Buffalo Freedom Grain-Free Chicken Recipe for Small Breed Puppies"crlf"3)Holistic Select Natural Dry Dog Food for Small Breed Puppy"crlf))
    (if (eq ?b "medium") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Taste of the Wild Grain-Free High Prairie Medium Puppy Formula Dry Food"crlf"2)Wellness Complete Health Medium Puppy Recipe"crlf"3)Blue Buffalo Basics Limited-Ingredient Dry Medium Puppy Food"crlf))
    (if (eq ?b "large") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Wellness Complete Health Natural Dry Large Breed Puppy Food, Chicken, Salmon & Rice"crlf"2)Nutro Max Natural Large Breed Puppy Dry Dog Food"crlf"3)Holistic Select Natural Dry Food Large Breed Puppy Health"crlf))
    
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-16
    "If age is 12 to more"
    (declare (salience 85))
    (dogInfo (breedSize ?b)(age ?e&:(>= ?e 12))(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food brands:" crlf crlf) 
	(printout t "Since Your Dog is an adult and needs diet which helps to stay fit and active" crlf)
    (printout t "Most dogs food companies manufacture food for both puppy and adult dog") 
    (if (eq ?b "small") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Wellness Complete Health Small Breed"crlf"2)Wellness Simple Grain-Free Small Breed"crlf"3)Blue BuffaloLife Protection Small Breed"crlf))
    (if (eq ?b "medium") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)ACANA Wild Prairie Regional Formula Grain-Free Dry Medium Dog Food"crlf"2)Victor Yukon River Salmon & Sweet Potato Grain-Free Dry Medium Dog Food"crlf"3)Merrick’s Grain-Free Real Buffalo and Sweet Potato Recipe Medium Breed"crlf))
    (if (eq ?b "large") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)ACANA Wild Prairie Regional Formula Grain-Free Dry Large Dog Food"crlf"2)Victor Yukon River Salmon & Sweet Potato Grain-Free Dry Large Dog Food"crlf"3)Fromm Large Breed Adult Gold"crlf))
    
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

;Life expectancy

(defrule rule-17
    "Life expectancy according to breed"
    (declare (salience 84))
    (dogInfo (breedSize ?b)(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "A sad fact but let me tell you about dog's life expectancy" crlf)
	(if (eq ?b "small") then
        (printout t crlf" The life expectancy of your small dog breed is 15 to 16 years "crlf))    
    (if (eq ?b "medium") then
        (printout t crlf"The life expectancy of your dog is 11 to 13 years "crlf))
    (if (eq ?b "large") then
        (printout t crlf"The life expectancy of your dog is 10 to 12 years "crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

;Deworming

(defrule rule-18
    "Deworming according to age"
    (declare (salience 83))
    (dogInfo (deworm ?d)(age ?a)(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "Worming is a very serious issue in dogs." crlf)
    (printout t crlf "Deworming of dog is essential to make sure it recieves all the nutrients properly as worms in dog can consume the food leading to underweight." crlf)
	(if (eq ?d "yes") then
        (printout t "Good to know you have dewormed your dog."crlf"But you still need to do deworming again at the following intervals")
    	(if(< ?a 2) then
        	(printout t crlf "1)You need to deworm your dog again at 6th week and 8th week"crlf"2)Then again at 3rd and 4th month"crlf"3)Again at 6th and 12th month"crlf"4)After that once every Year"))
        (if(and(>= ?a 2)(< ?a 4)) then
        	(printout t crlf "1)You need to deworm your dog again at 3rd and 4th month"crlf"2)Again at 6th and 12th month"crlf"3)After that once every Year"))
        (if(and(>= ?a 4)(< ?a 12)) then
        	(printout t crlf "1)You need to deworm your dog again at 6th and 12th month"crlf"2)After that once every Year"))
        (if(>= ?a 12) then
        	(printout t crlf "1)As you dog is adult you need to deworm your dog after once every year"))
        )
    (if(eq ?d "no") then
        (printout t "I recommend to visit the VET ASAP and start with deworming."crlf"Following is how a deworming chart looks"crlf)
        (printout t crlf "1)Deworming begins at 2nd week and followed again on 4th week " crlf "2)You need to deworm your dog again at 6th week and 8th week"crlf"3)Then again at 3rd and 4th month"crlf"Again at 6th and 12th month"crlf"4)After that once every Year"))        
        
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    
    )




;Vaccination

(defrule rule-19
    "Vaccination according to age"
    (declare (salience 82))
    (dogInfo (vaccine ?v)(age ?a)(OBJECT ?dObj))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "Vaccination makes sure to keep your dog's immunity system at it's best." crlf)
    (printout t crlf "Vaccination chart is given below:" crlf)
	(if (eq ?v "yes") then
        (printout t "Good to know you have given vaccination to your dog."crlf"But you still need to do vaccination again at the following intervals")
    	(if(and(>= ?a 2)(< ?a 3)) then
        	(printout t crlf "You need to give next vaccination to your dog by 3rd Month:"crlf"NAME OF VACCINATION: DHPP (vaccines for distemper, adenovirus [hepatitis], parainfluenza, and parvovirus)"crlf"Then again at 6th month of :"crlf"NAME OF VACCINATION: Rabies, DHPP"crlf"Again after 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))
        (if(and(>= ?a 3)(< ?a 6)) then
        	(printout t crlf "You need to give next vaccination again at 6th month of :"crlf"NAME OF VACCINATION: Rabies, DHPP"crlf"Again after 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))
        (if(>= ?a 6) then
        	(printout t crlf "After 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))
        )
    (if(eq ?v "no") then
        (printout t "I recommend to visit the VET ASAP and start with vaccination."crlf"Following is how a vaccination chart looks:"crlf)
        (printout t crlf "Firstly your dog needs first vaccination after 1st month of:"crlf"NAME OF VACCINATION: Distemper, measles, parainfluenza"crlf"You need to give next vaccination to your dog by 3rd Month of:"crlf"NAME OF VACCINATION: DHPP (vaccines for distemper, adenovirus [hepatitis], parainfluenza, and parvovirus)"crlf"Then again at 6th month of :"crlf"NAME OF VACCINATION: Rabies, DHPP"crlf"Again after 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))        
        
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    (printout t crlf "Thank You for your time.")
    (printout t crlf "If your are using or following a different way than anything mentioned above kindly confirm with your VET")
    (printout t crlf "HOPE we meet Again!! END")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    
    )


(deffunction run-application ()
    (reset)
    (focus start request-dog-details dog-recommendation)
    (run))

(while TRUE 
    (run-application))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      