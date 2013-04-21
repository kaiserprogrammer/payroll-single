(defpackage :payroll
  (:use :cl :lisp-unit))

(in-package :payroll)

(defvar *db*)

(defclass memory-db ()
  ((classification :accessor classification)
   (method :accessor pay-method
           :initform (make-instance 'hold-method))
   (schedule :accessor schedule)))

(defclass commissioned-classification ()
  ((salary :initarg :salary
           :accessor salary)
   (rate :initarg :rate
         :accessor rate)))

(defclass salaried-classification ()
  ((salary :initarg :salary
           :accessor salary)))

(defclass hold-method () ())

(defclass bi-weekly-schedule () ())
(defclass monthly-schedule () ())

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

(defmethod db-change-salaried (salary (db memory-db))
  (setf (classification db) (make-instance 'salaried-classification
                                           :salary salary))
  (setf (schedule db) (make-instance 'monthly-schedule)))

(defmethod db-change-commissioned (salary rate (db memory-db))
  (setf (classification db) (make-instance 'commissioned-classification
                                           :salary salary
                                           :rate rate))
  (setf (schedule db) (make-instance 'bi-weekly-schedule)))


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
    (assert-equal 'PAYROLL::MONTHLY-SCHEDULE
                  (class-name (class-of (payment-schedule))))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
