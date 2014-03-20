

;; interface summable
;;   method int sum()

;; class sum_list extends object
;;   implements summable
;;   field summable first
;;   field summable left
;;   method void initialize(f : summable, l : summable)
;;    begin
;;     set first = f; set left = l
;;    end
;;   method int sum() +(send first sum(), send left sum())

;; class leaf_node extends object
;;   implements summable
;;   field int value
;;   method void initialize(v : int)set value = v
;;   method int sum() value

;; class interior_node extends object
;;   implements summable
;;   field summable left
;;   field summable right
;;   method void initialize(l : summable, r : summable)
;;    begin
;;     set left = l; set right = r
;;    end
;;   method int sum() +(send left sum(), send right sum())

;; class general_tree extends object
;;   implements summable
;;   field sum_list left
;;   field sum_list right
;;   method void initialize(l : sum_list, r : sum_list)
;;    begin
;;     set left = l; set right = r
;;    end
;;   method int sum + (send left sum(), send right sum())
