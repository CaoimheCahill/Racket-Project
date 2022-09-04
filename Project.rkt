#lang racket

;; Test Data with suitable data for my function(50 as the price of item, standard which stands for standard shipping, US as the country the item is coming from and 3 which stands for 3kg as the weight of the item) containing one piece of malformed data (-10)
(define testData '( 50 -10 standard US 3 ))

;This function checks for malformed data within my test data. As my test data needed both numbers and words for my problem, I did that if there was a number shown, it would check if it was either positive or negative. If it was negative, it would return "error" in its place and if it was positive or it wasn't a number (word), then it would be #t (true).
(define check (lambda(x)
                  (if (number? x)
                      (if (negative? x)
                          "error"
                          #t)
                      #t)))
(map check testData)

;This function filters the malformed data from the list. It filters out any negative numbers, in this case -10.
(define sort (filter
              (lambda (x)
                (if (number? x)
                    (if (negative? x)
                        #f
                        #t)
                    #t))
              testData))
(display sort)


;; defining hypothetical delivery data
(define cuttOff 40) 
(define nextDay 3.99)
(define standard 2.99)
(define y (or nextDay standard))


;; calculating delivery cost. Most websites nowadays usually offer free shipping if you pay over a certain amount. In this case, if x(€) is bigger than the cuttOff(€40 in this case), then the output is 0. If x(€) is less than the cuttOff(€40 in this case), then it determines whether you have chosen next day delivery or standard delivery and the output is the given value for that delivery option.  
(define deliveryCost (lambda (x y)
                       (if(> x cuttOff)
                          0 y)))
  

;; defining hypothetical custom data for 4 random countries. I decided that the customs were certain percentages of the item bought, depending on where they came from.
(define US 0.3) ;; two letter abbreviation for America
(define UK 0.1) ;; two letter abbreviation for the United Kingdom
(define CN 0.4) ;; two letter abbreviation for China
(define DE 0.15) ;; two letter abbreviation for Germany

;; calculating customs cost. Multiplying the cost of the item by the custom percentage of the specific country recursively. If x(€) is zero, customs is also zero as you would of guessed
(define Customs (lambda (x z)
                  (if (zero? x) 0
                      (+ z (Customs (- x 1) z)))))

;; calculating extra price for the weight of the item recursively. If x(€) is less than or equal to 1kg, then the extra cost is €0. If the weight is over 1kg, the price goes up €2 every kg. For example, if the item is 2kg, the price is €2. if the item is 4kg, the price is €6. If its in between kg's then it will round up.
(define weight(lambda (w)
                (if (<= w 1) 0
                    (+ 2(weight (- w 1))))))


;; calculating the total shipping cost by adding the delivery cost, customs price and weight cost
;; x = item price   y = nextDay or standard    z = country   w = weight
(define TotalShippingCost (lambda (x y z w) (+ (deliveryCost x y)(Customs x z)(weight w))))

;; calculating the total cost by adding the total shipping cost and the price of the item bought
;; x = item price   y = nextDay or standard    z = country   w = weight
(define TotalCost (lambda (x y z w) (+ (TotalShippingCost x y z w) x)))






                                  







                           

              




