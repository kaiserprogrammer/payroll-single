(defpackage :payroll
  (:use :cl :lisp-unit))

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

(defclass memory-db ()
  ((classification :accessor classification)
   (method :accessor pay-method
           :initform (make-instance 'hold-method))
   (schedule :accessor schedule)))

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
   (timecards :initform (make-hash-table :test #'equalp)
              :reader timecards)))

(defmethod timecard ((pc hourly-classification) (d local-time:timestamp))
  (gethash d (timecards pc)))

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
  (db-change-commissioned salary rate db))

(defun change-salaried (salary &optional (db *db*))
  (db-change-salaried salary db))

(defun change-hourly (rate &optional (db *db*))
  (db-change-hourly rate db))

(defun add-sales-receipt (date amount &optional (db *db*))
  (db-add-sale (make-instance 'sale :date date :amount amount)
               db))

(defun add-timecard (date hours &optional (db *db*))
  (db-add-timecard (make-instance 'timecard :date date :hours hours)
                   db))

(defmethod db-add-timecard (tc (db memory-db))
  (setf (gethash (date tc) (timecards (payment-classification db))) tc))

(defmethod db-add-sale ((sale sale) (db memory-db))
  (push sale (sales-receipts (payment-classification db))))

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
        (date (local-time:parse-timestring "2012-12-1")))
    (change-commissioned 350 5.0)
    (add-sales-receipt date 500.0)
    (let ((sales (sales-receipts (payment-classification))))
      (assert-true sales)
      (assert-equal 500.0 (amount (first sales)))
      (assert-equality #'local-time:timestamp= date (date (first sales))))))

(define-test adding-time-cards
  (let ((*db* (make-instance 'memory-db))
        (date (local-time:parse-timestring "2012-12-31")))
    (change-hourly 12.75)
    (add-timecard date 8.0)
    (let ((tc (timecard (payment-classification) date)))
      (assert-equal 8.0 (hours tc))
      (assert-equality #'local-time:timestamp= date (date tc)))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
