#|  $Id: auctions.lsp,v 1.1 2002/05/31 21:40:09 bob Exp $

  15 Nov 01 ... Big revisions as reorganize functionality.
   6 Nov 01 ... Cut from pred-auction code to isolate auctions as type of expert.

;;;;

  (load "auctions")

;;-- operators
  (def lOpStrm (send linear-operator-stream-proto :new 1 10))
  (def qOpStrm (send quadratic-operator-stream-proto :new 10))

;;-- bidder tracks cumulative results
  (def lBidder (send bidder-proto :new 10 
                  (make-mixture-handicapper lOpStrm)))
  (def qBidder (send bidder-proto :new 10 
                  (make-mixture-handicapper qOpStrm :geoShare 0.3)))

;;-- join in an auction
  (def auction (send auction-proto :new 30 
                  (list lBidder qBidder)))
  (send auction :print)

  (send auction :next-operator)
  (send auction :operator-rejected)
  (send auction :print)

|#

(require "operators")
(require "pdf")

(provide "auctions")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Auctions
;;
;;       Collections of bidders.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto AUCTION-PROTO
  '( 
    currentBidder  ; list of bidder and second price of most recent bid
    bidders        ; list of bidders
    )
  ()
  operator-stream-proto)

(defmeth auction-proto :ISNEW (initialBalance bidders)
  (setf (slot-value 'bidders) bidders)
  (send self :divide-initial-balance initialBalance)
  )

(defmeth auction-proto :PRINT ()
  (let ((bidders (send self :bidders))  )
    (format t "Auction with ~d bidders:~%" (length bidders))
    (dolist (b bidders)
	    (format t " ~{~a~}~%" (send b :status-string)))))

(defmeth auction-proto :BIDDERS ()
  (slot-value 'bidders))

(defmeth auction-proto :DIVIDE-INITIAL-BALANCE (total)
  ;; Distribute total wealth in equal shares at rate 1/i^2???
  (let ((share (/ total (length (send self :bidders))))  )
    (mapcar #'(lambda (e) (send e :set-balance share))
	    (slot-value 'bidders))
    ))

(defmeth auction-proto :SOLICIT-BIDS ()
  (mapcar #'(lambda (bidder) (send bidder :bid)) 
	  (slot-value 'bidders)))

;;;;

(defmeth auction-proto :NEXT-OPERATOR ()
  (let ((bidder (send self :bidder-and-second-price 
		      (send self :solicit-bids)))  )
    (setf (slot-value 'currentBidder) bidder)
    (send (first bidder) :next-operator)))

(defmeth auction-proto :BIDDER-AND-SECOND-PRICE (bids)
  ;; second largest bid of associated experts
  (let ((bidders (slot-value 'bidders))  )
    (if (= 1 (length bids))
	(list (first bidders) (first bids))
      (let ((down  (order (- bids))))
	(list (elt bidders (first down))
	      (elt bids (second down)))
	))))

;;;;

(defmeth auction-proto :BIDDER ()
  (first (slot-value 'currentBidder)))

(defmeth auction-proto :SECOND-PRICE ()
  (second (slot-value 'currentBidder)))

(defmeth auction-proto :OPERATOR-REJECTED ()
  (let ((cost (send self :second-price)))  
    (send (send self :bidder) :operator-rejected cost)
    cost))

(defmeth auction-proto :OPERATOR-ACCEPTED ()
  (send (send self :bidder) :operator-accepted))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Bidders
;;
;;       Bidders track the value associated with a handicapper
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto BIDDER-PROTO 
  '(balance      ; accumulated value
    currentOp    ; current choice, as (prob op)
    history      ; list of prior considered bids, as (result$ prob op)
    handicapper  ; generates recommedations
    )
  ()
  operator-stream-proto)

(defmeth bidder-proto :ISNEW (initialBalance handicapper)
  (setf (slot-value 'handicapper) handicapper)
  (setf (slot-value 'balance)    0)
  (send self :update-state initialBalance 'start))

(defmeth bidder-proto :STATUS-STRING ()
  (cons (format nil "$~,4f/$~,2f (p=~,3f ~a)" 
		(send self :bid) (slot-value 'balance)
		(first (slot-value 'currentOp))
		(second (slot-value 'currentOp)))
	(send (slot-value 'handicapper) :status-string)))

;;;;   accessors

(defmeth bidder-proto :BALANCE ()
  (slot-value :balance))

(defmeth bidder-proto :SET-BALANCE (balance)
  (format t "Resetting bidder balance from ~a to ~a~%"
	  (slot-value 'balance) balance)
  (setf (slot-value 'balance) balance))

(defmeth bidder-proto :HISTORY ()
  (slot-value 'history))

;;;;   bidding

(defmeth bidder-proto :BID ()
  ;; Next bid for operator given state of model and expert
  (if (and (send self :next-operator) (< 0 (slot-value 'balance)))
      (let ((availBal (* .5 (slot-value 'balance)))  )
	(let ((p (send self :probability)) ); prob of winning $1
	  (if (= p 1)
	      availBal
	    (min availBal (/ p (- 1 p)))
	    )))
    0))

(defmeth bidder-proto :OPERATOR-REJECTED (price)
  ;; Informs the bidder that its choice was not used in model
  (send self :update-state (- price) (slot-value 'currentOp)))

(defmeth bidder-proto :OPERATOR-ACCEPTED ()
  ;; Informs the bidder that its choice was used in model
  (send (slot-value 'handicapper) :picked-winner)
  (send self :update-state 1 (slot-value 'currentOp)))

(defmeth bidder-proto :PROBABILITY ()
  ;; Expert's probability that current choice will work
  (if (send self :next-operator) ; we have an operator
      (first (slot-value 'currentOp))
    0))
  
(defmeth bidder-proto :NEXT-OPERATOR ()
  ;; Operator for the current bid
  (second (slot-value 'currentOp)))
  
(defmeth bidder-proto :UPDATE-STATE (price choice)
  ;; move current operator onto list of past, adjust balance
  (push (cons price choice) (slot-value 'history))
  (setf (slot-value 'balance)
	(+ (slot-value 'balance) price))
  (setf (slot-value 'currentOp)
	(list (send (slot-value 'handicapper) :probability)
	      (send (slot-value 'handicapper) :next-operator)) ; read its stream
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Handicapper
;;        attaches probabilities to a stream
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
  (def hand  (make-mixture-handicapper opStrm))
  (send hand :probability)
  (send hand :next-operator)
|#

(defun  MAKE-MIXTURE-HANDICAPPER (opStream &key (geoShare .6) (geoRate .5))
  (send handicapper-proto :new
	(mixture-pdf (list geoShare (- 1 geoshare))
		     (list #'(lambda (i) (geometric-pdf i :rate geoRate))
			   #'cauchy-pdf))
	opStream))


(defproto HANDICAPPER-PROTO
  '(
    record        ; (nOffered, nRight)
    nSinceLastWin ; track number since last hit 
    pdf           ; used to rate the sequence of offers p(1),p(2),...
    opStream      ; stream of operators
    )
  ()
  operator-stream-proto)

(defmeth handicapper-proto :ISNEW (pdf opStream)
  (setf (slot-value 'record)        '(0 0))
  (setf (slot-value 'nSinceLastWin) 0)
  (setf (slot-value 'pdf)           pdf)
  (setf (slot-value 'opStream)      opStream)
  )

(defmeth handicapper-proto :RECORD ()
  (slot-value 'record))

(defmeth handicapper-proto :PROBABILITY ()
  (funcall (slot-value 'pdf) (1+ (slot-value 'nSinceLastWin))))

(defmeth handicapper-proto :NEXT-OPERATOR ()
  (incf (slot-value 'nSinceLastWin))
  (setf (first (slot-value 'record)) 
	(1+ (first (slot-value 'record))))
  (send (slot-value 'opStream) :next-operator))

(defmeth handicapper-proto :PICKED-WINNER ()
  (setf (second (slot-value 'record)) 
	(1+ (second (slot-value 'record))))
  (setf (slot-value 'nSinceLastWin) 0))


