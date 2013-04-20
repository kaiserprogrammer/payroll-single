(defpackage :payroll
  (:use :cl :lisp-unit))

(in-package :payroll)



(remove-tests :all)

(define-test creating-commissioned-employee
  (let ((*db* (make-instance 'memory-db)))
    (change-commissioned "Johnny" "Work" 250.0 300.)
    (let ((pc (payment-classification)))
      (assert-equal 250.0 (salary pc))
      (assert-equal 300.0 (rate pc)))
    (assert-eql 'payroll::hold-method
                (class-name (class-of (payment-method))))
    (assert-eql 'payroll::bi-weekly-schedule
                (class-name (class-of (schedule))))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
