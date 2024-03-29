(defpackage :payroll
  (:use :cl :lisp-unit :local-time))

(in-package :payroll)

(defvar *db*)

(defclass paycheck ()
  ((pay-date :initarg :pay-date
             :reader pay-date)
   (start-date :initarg :start-date
               :accessor start-date)
   (net-pay :accessor net-pay)
   (gross-pay :accessor gross-pay)
   (deductions :accessor deductions)
   (disposition :accessor disposition)))

(defclass sale ()
  ((date :initarg :date
         :reader date)
   (amount :initarg :amount
           :reader amount)))

(defclass timecard ()
  ((date :initarg :date
         :reader date)
   (hours :initarg :hours
          :reader hours)))

(defclass service-charge ()
  ((date :initarg :date
         :accessor date)
   (charge :initarg :charge
           :accessor charge)))

(defclass no-affiliation () ())
(defclass affiliation ()
  ((dues :initarg :dues
         :accessor dues)
   (charges :initform (list)
            :accessor charges)))

(defclass memory-db ()
  ((classification :accessor classification)
   (method :accessor pay-method
           :initform (make-instance 'hold-method))
   (schedule :accessor schedule)
   (affiliation :initform (make-instance 'no-affiliation)
                :accessor affiliation)))

(defclass commissioned-classification ()
  ((salary :initarg :salary
           :accessor salary)
   (rate :initarg :rate
         :accessor rate)
   (sales :initform (list)
          :accessor sales-receipts)))

(defclass hourly-classification ()
  ((rate :initarg :hourly-rate
         :accessor hourly-rate)
   (timecards :initform (list)
              :accessor timecards)))

(defmethod timecard ((pc hourly-classification) (d timestamp))
  (cdr (assoc d (timecards pc) :test #'timestamp=)))

(defclass salaried-classification ()
  ((salary :initarg :salary
           :accessor salary)))

(defclass hold-method ()
  ((disposition :initform "Hold"
                :accessor disposition)))

(defclass bi-weekly-schedule () ())
(defclass monthly-schedule () ())
(defclass weekly-schedule () ())

(defmethod payment-classification (&optional (db *db*))
  (classification db))
(defmethod payment-schedule (&optional (db *db*))
  (schedule db))
(defmethod payment-method (&optional (db *db*))
  (pay-method db))

(defun change-commissioned (salary rate &optional (db *db*))
  (db-change-classification (make-instance 'commissioned-classification
                                           :salary salary
                                           :rate rate)
                            (make-instance 'bi-weekly-schedule)
                            db))

(defun change-salaried (salary &optional (db *db*))
  (db-change-classification (make-instance 'salaried-classification
                                           :salary salary)
                            (make-instance 'monthly-schedule)
                            db))

(defun change-hourly (rate &optional (db *db*))
  (db-change-classification (make-instance 'hourly-classification
                                           :hourly-rate rate)
                            (make-instance 'weekly-schedule)
                            db))

(defun add-sales-receipt (date amount &optional (db *db*))
  (db-add-sale (make-instance 'sale :date date :amount amount)
               db))

(defun add-timecard (date hours &optional (db *db*))
  (db-add-timecard (make-instance 'timecard :date date :hours hours)
                   db))

(defun delete-employee (&optional (db *db*))
  (db-delete-employee db))

(defun change-union-member (dues &optional (db *db*))
  (db-change-affiliation (make-instance 'affiliation :dues dues) db))

(defun change-unaffiliated (&optional (db *db*))
  (db-change-affiliation (make-instance 'no-affiliation) db))

(defun add-service-charge (date charge &optional (db *db*))
  (db-add-service-charge (make-instance 'service-charge
                                        :date date
                                        :charge charge)
                         db))

(defmethod payday? (date (schedule monthly-schedule))
  (last-day-of-month? date))

(defmethod payday? (date (schedule weekly-schedule))
  (last-day-of-week? date))

(defmethod payday? (date (schedule bi-weekly-schedule))
  (and (last-day-of-week? date) (oddp (truncate (/ (local-time:timestamp-to-universal date) (* 60 60 24 7))))))

(defun last-day-of-week? (date)
  (= 5
     (timestamp-day-of-week date)))

(defun last-day-of-month? (date)
  (= (timestamp-month (timestamp+ date 1 :month))
     (timestamp-month (timestamp+ date 1 :day))))

(defun first-day-of-month (date)
  (timestamp- date (1- (timestamp-day date)) :day))

(defun first-day-of-week (date)
  (timestamp- date (1- (timestamp-day-of-week date)) :day))


(defmethod get-pay-period-start-date (date (schedule monthly-schedule))
  (first-day-of-month date))

(defmethod get-pay-period-start-date (date (schedule weekly-schedule))
  (first-day-of-week date))

(defmethod get-pay-period-start-date (date (s bi-weekly-schedule))
  (timestamp- (first-day-of-week date) 7 :day))

(defmethod calculate-pay ((pc paycheck) (cls salaried-classification))
  (setf (gross-pay pc) (salary cls)))
(defmethod calculate-pay ((pc paycheck) (cls commissioned-classification))
  (setf (gross-pay pc) (+ (salary cls)
                          (* (/ (rate cls) 100)
                             (reduce #'+
                                     (mapcar #'amount
                                             (remove-if (lambda (tc)
                                                          (or (timestamp> (date tc) (pay-date pc))
                                                              (timestamp< (date tc) (start-date pc))))
                                                        (sales-receipts cls))))))))
(defmethod calculate-pay ((pc paycheck) (cls hourly-classification))
  (setf (gross-pay pc) (* (hourly-rate cls)
                          (reduce #'+ (let ((hours
                                             (mapcar
                                              #'hours
                                              (remove-if (lambda (tc)
                                                           (or (timestamp> (date tc) (pay-date pc))
                                                               (timestamp< (date tc) (start-date pc))))
                                                         (mapcar #'cdr (timecards cls))))))
                                        (mapcar (lambda (hours) (if (> hours 8)
                                                               (+ 8 (* 1.5 (- hours 8)))
                                                               hours))
                                                hours))))))

(defmethod calculate-deductions ((pc paycheck) (af no-affiliation))
  (setf (deductions pc) 0))

(defmethod calculate-deductions ((pc paycheck) (af affiliation))
  (setf (deductions pc) (+ (dues af)
                           (reduce #'+ (mapcar #'charge
                                               (remove-if (lambda (tc)
                                                            (or (timestamp> (date tc) (pay-date pc))
                                                                (timestamp< (date tc) (start-date pc))))
                                                          (mapcar #'cdr (charges af))))))))

(defun payday (date &optional (db *db*))
  (let ((schedule (payment-schedule db)))
    (when (payday? date schedule)
      (let* ((start-date (get-pay-period-start-date date schedule))
             (pc (make-instance 'paycheck
                                :start-date start-date
                                :pay-date date)))
        (calculate-pay pc (payment-classification db))
        (calculate-deductions pc (affiliation db))
        (setf (disposition pc) (disposition (payment-method db)))
        (setf (net-pay pc) (- (gross-pay pc) (deductions pc)))
        pc))))

(defmethod db-add-service-charge (sc (db memory-db))
  (push (cons (date sc) sc) (charges (affiliation db))))

(defmethod db-change-affiliation (af (db memory-db))
  (setf (affiliation db) af))

(defmethod db-delete-employee ((db memory-db))
  (setf (classification db) nil)
  (setf (pay-method db) nil)
  (setf (schedule db) nil))

(defmethod db-add-timecard (tc (db memory-db))
  (push (cons (date tc) tc) (timecards (payment-classification db))))

(defmethod db-add-sale ((sale sale) (db memory-db))
  (push sale (sales-receipts (payment-classification db))))

(defmethod db-change-classification (classification schedule (db memory-db))
  (setf (classification db) classification)
  (setf (schedule db) schedule))

(defmethod db-change-salaried (salary (db memory-db))
  (setf (classification db) (make-instance 'salaried-classification
                                           :salary salary))
  (setf (schedule db) (make-instance 'monthly-schedule)))

(defmethod db-change-commissioned (salary rate (db memory-db))
  (setf (classification db) (make-instance 'commissioned-classification
                                           :salary salary
                                           :rate rate))
  (setf (schedule db) (make-instance 'bi-weekly-schedule)))

(defmethod db-change-hourly (rate (db memory-db))
  (setf (classification db) (make-instance 'hourly-classification
                                           :hourly-rate rate))
  (setf (schedule db) (make-instance 'weekly-schedule)))


(remove-tests :all)

(define-test creating-commissioned-employee
  (let ((*db* (make-instance 'memory-db)))
    (change-commissioned 250.0 300.0)
    (let ((pc (payment-classification)))
      (assert-equal 250.0 (salary pc))
      (assert-equal 300.0 (rate pc)))
    (assert-eql 'payroll::hold-method
                (class-name (class-of (payment-method))))
    (assert-eql 'payroll::bi-weekly-schedule
                (class-name (class-of (payment-schedule))))))

(define-test creating-salaried-employee
  (let ((*db* (make-instance 'memory-db)))
    (change-salaried 1111.0)
    (assert-equal 1111.0 (salary (payment-classification)))
    (assert-eql 'PAYROLL::MONTHLY-SCHEDULE
                (class-name (class-of (payment-schedule))))))

(define-test creating-hourly-employee
  (let ((*db* (make-instance 'memory-db)))
    (change-hourly 17.5)
    (assert-equal 17.5 (hourly-rate (payment-classification)))
    (assert-eql 'payroll::weekly-schedule
                (class-name (class-of (payment-schedule))))))

(define-test adding-sales-receipts
  (let ((*db* (make-instance 'memory-db))
        (date (parse-timestring "2012-12-1")))
    (change-commissioned 350 5.0)
    (add-sales-receipt date 500.0)
    (let ((sales (sales-receipts (payment-classification))))
      (assert-true sales)
      (assert-equal 500.0 (amount (first sales)))
      (assert-equality #'timestamp= date (date (first sales))))))

(define-test adding-time-cards
  (let ((*db* (make-instance 'memory-db))
        (date (parse-timestring "2012-12-31")))
    (change-hourly 12.75)
    (add-timecard date 8.0)
    (let ((tc (timecard (payment-classification) date)))
      (assert-equal 8.0 (hours tc))
      (assert-equality #'timestamp= date (date tc)))))

(define-test deleting-an-employee
  (let ((*db* (make-instance 'memory-db)))
    (change-salaried 2222.0)
    (delete-employee)
    (assert-eq nil (payment-classification))
    (assert-eq nil (payment-schedule))
    (assert-eq nil (payment-method))))

(define-test change-union-member
  (let ((*db* (make-instance 'memory-db)))
    (change-union-member 99.42)
    (assert-equal 99.42 (dues (affiliation *db*)))
    (change-unaffiliated)
    (assert-eql 'payroll::no-affiliation
                (class-name (class-of (affiliation *db*))))))

(define-test adding-a-service-charge
  (let ((*db* (make-instance 'memory-db))
        (date (parse-timestring "2012-12-31")))
    (change-union-member 10.0)
    (add-service-charge date 12.95)
    (let ((sc (cdr (assoc date (charges (affiliation *db*)) :test #'timestamp=))))
      (assert-true sc)
      (assert-equal 12.95 (charge sc))
      (assert-equality #'timestamp= date (date sc)))))

(define-test paying-a-single-salaried-employee
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2012-11-30")))
    (change-salaried 2250.0)
    (let ((pc (payday pay-date)))
      (assert-equality #'timestamp= pay-date (pay-date pc))
      (assert-equality #'timestamp=
                       (start-date pc)
                       (parse-timestring "2012-11-1"))
      (assert-equal 2250.0 (gross-pay pc))
      (assert-equal "Hold" (disposition pc))
      (assert-equal 0 (deductions pc))
      (assert-equal 2250.0 (net-pay pc)))))

(define-test not-paying-a-salaried-employee-on-wrong-date
  (let ((*db* (make-instance 'memory-db)))
    (change-salaried 2250.0)
    (assert-eq nil (payday (parse-timestring "2012-11-29")))))

(define-test no-pay-with-zero-timecards
  (let ((*db* (make-instance 'memory-db)))
    (change-hourly 12.5)
    (let ((pc (payday (parse-timestring "2001-11-9"))))
      (assert-equality #'timestamp=
                       (start-date pc)
                       (parse-timestring "2001-11-05"))
      (assert-equalp 0 (gross-pay pc))
      (assert-equalp 0 (deductions pc))
      (assert-equalp 0 (net-pay pc)))))

(define-test pay-with-one-time-card
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-09")))
    (change-hourly 12.5)
    (add-timecard pay-date 2.0)
    (let ((pc (payday pay-date)))
      (assert-equal 25.0 (gross-pay pc))
      (assert-equal 25.0 (net-pay pc)))))

(define-test pay-for-over-time
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-09")))
    (change-hourly 15.25)
    (add-timecard pay-date 9.0)
    (assert-equal (* (+ 8 1.5)
                     15.25)
                  (gross-pay (payday pay-date)))))

(define-test no-pay-on-wrong-date-for-hourly-employee
  (let ((*db* (make-instance 'memory-db))
        (wrong-date (parse-timestring "2001-11-08")))
    (change-hourly 15.25)
    (add-timecard wrong-date 9.0)
    (assert-eq nil (payday wrong-date))))

(define-test pay-with-two-time-cards
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-09")))
    (change-hourly 12.5)
    (add-timecard pay-date 2.0)
    (add-timecard (timestamp- pay-date 1 :day) 8.0)
    (let ((pc (payday pay-date)))
      (assert-equalp 125.0 (gross-pay pc))
      (assert-equalp 125.0 (net-pay pc)))))

(define-test pay-only-one-period
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-09")))
    (change-hourly 12.5)
    (add-timecard pay-date 2.0)
    (add-timecard (timestamp- pay-date 7 :day) 8.0)
    (add-timecard (timestamp+ pay-date 1 :day) 8.0)
    (let ((pc (payday pay-date)))
      (assert-equalp 25.0 (gross-pay pc))
      (assert-equalp 25.0 (net-pay pc)))))

(define-test pay-with-no-commission
  (let ((*db* (make-instance 'memory-db)))
    (change-commissioned 1000.0 10)
    (let ((pc (payday (parse-timestring "2001-11-30"))))
      (assert-equalp 1000 (gross-pay pc))
      (assert-equalp 1000 (net-pay pc)))))

(define-test pay-with-one-sale-in-period
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-30")))
    (change-commissioned 1000.0 10)
    (add-sales-receipt pay-date 500)
    (add-sales-receipt (timestamp- pay-date 14 :day) 1000)
    (let ((pc (payday pay-date)))
      (assert-equalp 1050.0 (gross-pay pc))
      (assert-equalp 1050.0 (net-pay pc)))))

(define-test no-pay-on-wrong-date-for-commissioned-employee
  (let ((*db* (make-instance 'memory-db)))
    (change-commissioned 1000. 10)
    (assert-eq nil (payday (parse-timestring "2001-11-23")))))

(define-test deduct-service-charges
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-09")))
    (change-hourly 12.5)
    (add-timecard pay-date 8.0)
    (change-union-member 80)
    (add-service-charge pay-date 15)
    (let ((pc (payday pay-date)))
      (assert-equalp 100 (gross-pay pc))
      (assert-equalp 95 (deductions pc))
      (assert-equalp 5 (net-pay pc)))))

(define-test deduct-service-charges-when-spanning-multiple-periods
  (let ((*db* (make-instance 'memory-db))
        (pay-date (parse-timestring "2001-11-09")))
    (change-hourly 12.5)
    (add-timecard pay-date 8.0)
    (change-union-member 80)
    (add-service-charge pay-date 15)
    (add-service-charge (timestamp- pay-date 7 :day) 15)
    (add-service-charge (timestamp+ pay-date 1 :day) 15)
    (let ((pc (payday pay-date)))
      (assert-equalp 100 (gross-pay pc))
      (assert-equalp 95 (deductions pc))
      (assert-equalp 5 (net-pay pc)))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
