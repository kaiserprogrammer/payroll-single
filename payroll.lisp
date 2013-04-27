(defpackage :payroll
  (:use :cl :lisp-unit :local-time))

(in-package :payroll)

(defvar *db*)

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

(defclass hold-method () ())

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

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
