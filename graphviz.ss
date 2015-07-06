(define max-label-length 30)
(define test-nodes
  '((living-room ("you are in the living-room"))
    (garden ("you are in the garden"))
    (attic ("you are in the attic"))))
(define test-edges
  '((living-room (garden "west" "door")
		 (attic "upstairs" "ladder"))
    (garden (living-room "east" "door"))
    (attic (living-room "downstairs" "ladder"))))

(define dot-name
  (lambda (exp)
    (list->string (map (lambda (x)
			 (if (not (or (char-alphabetic? x)
				      (char-numeric? x)))
			     #\_
			     x))
		       (string->list (symbol->string exp))))))

(define dot-label
  (lambda (exp)
    (let ([s (format "~a" exp)])
      (if (> (string-length s) max-label-length)
	  (string-append (substring s 0 (- max-label-length 3)) "...")
	  s))))

(define nodes->dot
  (case-lambda
   [(nodes) (nodes->dot nodes (current-output-port))]
   [(nodes port)
    (map (lambda (node)
	   (newline port)
	   (display (dot-name (car node)) port)
	   (display "[label=\"" port)
	   (display (dot-label node) port)
	   (display "\"];") port)
	 nodes)]))

(define edges->dot
  (case-lambda
   [(edges) (edges->dot edges (current-output-port))]
   [(edges port)  
    (map
     (lambda (node)
       (map
	(lambda (edge)
	  (newline port)
	  (display (dot-name (car node)) port)
	  (display "->" port)
	  (display (dot-name (car edge)) port)
	  (display "[label=\"" port)
	  (display (dot-label (cdr edge)) port)
	  (display "\"];"))
	(cdr node)))
     edges)]))

(define graph->dot
  (case-lambda
   [(nodes edges) (graph->dot nodes edges (current-output-port))]
   [(nodes edges port)
    (display "digraph{")
    (nodes->dot nodes)
    (edges->dot edges)
    (display "}")]))

(define dot->png
  (lambda (fname thunk)
    (with-output-to-file fname thunk)
    (let ([cmd (string-append "dot -Tpng -O " fname)])
      (system cmd))))
    
(define graph->png
  (lambda (fname nodes edges)
    (dot->png fname
	      (lambda ()
		(graph->dot nodes edges)))))


