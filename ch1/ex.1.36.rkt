#lang racket/base

(define tolerance 0.00001)

(define (fixed-point-with-logging f first-guess)
  (define count 1)
  (define (increase) (set! count (+ count 1)))

  (define (close-enough? v1 v2)
    (define var (abs (- v1 v2)))
    (display (format "step ~a. trying ~a, variation: ~a" count v2 var))
    (increase)

    (define ok? (< (abs (- v1 v2)) tolerance))
    (if ok?
        (displayln " *** ok")
        (newline))
    ok?)

  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(module+ test)

(module+ main
  (displayln "Without Average Damping")
  (displayln (format "result: ~a"
                     (fixed-point-with-logging (lambda (x)
                                                 (/ (log 1000)
                                                    (log x)))
                                               1.1)))
  (newline)
  (displayln "With Average Damping")
  (displayln (format "result: ~a"
                     (fixed-point-with-logging (lambda (x)
                                                 (/ (+ (/ (log 1000)
                                                          (log x))
                                                       x)
                                                    2))
                                               1.1))))

#|
Without Average Damping
step 1. trying 72.47657378429035, variation: 71.37657378429036
step 2. trying 1.6127318474109593, variation: 70.8638419368794
step 3. trying 14.45350138636525, variation: 12.84076953895429
step 4. trying 2.5862669415385087, variation: 11.867234444826742
step 5. trying 7.269672273367045, variation: 4.683405331828537
step 6. trying 3.4822383620848467, variation: 3.7874339112821986
step 7. trying 5.536500810236703, variation: 2.054262448151856
step 8. trying 4.036406406288111, variation: 1.5000944039485917
step 9. trying 4.95053682041456, variation: 0.9141304141264488
step 10. trying 4.318707390180805, variation: 0.631829430233755
step 11. trying 4.721778787145103, variation: 0.4030713969642985
step 12. trying 4.450341068884912, variation: 0.2714377182601915
step 13. trying 4.626821434106115, variation: 0.17648036522120325
step 14. trying 4.509360945293209, variation: 0.11746048881290605
step 15. trying 4.586349500915509, variation: 0.07698855562230023
step 16. trying 4.535372639594589, variation: 0.05097686132092072
step 17. trying 4.568901484845316, variation: 0.033528845250727635
step 18. trying 4.546751100777536, variation: 0.022150384067780138
step 19. trying 4.561341971741742, variation: 0.014590870964205749
step 20. trying 4.551712230641226, variation: 0.00962974110051551
step 21. trying 4.558059671677587, variation: 0.00634744103636109
step 22. trying 4.55387226495538, variation: 0.004187406722207854
step 23. trying 4.556633177654167, variation: 0.002760912698787088
step 24. trying 4.554812144696459, variation: 0.0018210329577073026
step 25. trying 4.556012967736543, variation: 0.0012008230400839537
step 26. trying 4.555220997683307, variation: 0.0007919700532363905
step 27. trying 4.555743265552239, variation: 0.0005222678689325022
step 28. trying 4.555398830243649, variation: 0.00034443530859018523
step 29. trying 4.555625974816275, variation: 0.00022714457262562604
step 30. trying 4.555476175432173, variation: 0.00014979938410153437
step 31. trying 4.555574964557791, variation: 9.878912561767095e-5
step 32. trying 4.555509814636753, variation: 6.514992103756612e-5
step 33. trying 4.555552779647764, variation: 4.2965011010309695e-5
step 34. trying 4.555524444961165, variation: 2.8334686598796566e-5
step 35. trying 4.555543131130589, variation: 1.8686169424242394e-5
step 36. trying 4.555530807938518, variation: 1.2323192071228561e-5
step 37. trying 4.555538934848503, variation: 8.126909984973452e-6 *** ok
result: 4.555538934848503

With Average Damping
step 1. trying 36.78828689214517, variation: 35.68828689214517
step 2. trying 19.352175531882512, variation: 17.43611136026266
step 3. trying 10.84183367957568, variation: 8.510341852306832
step 4. trying 6.870048352141772, variation: 3.971785327433908
step 5. trying 5.227224961967156, variation: 1.642823390174616
step 6. trying 4.701960195159289, variation: 0.5252647668078669
step 7. trying 4.582196773201124, variation: 0.11976342195816514
step 8. trying 4.560134229703681, variation: 0.02206254349744352
step 9. trying 4.5563204194309606, variation: 0.0038138102727200973
step 10. trying 4.555669361784037, variation: 0.0006510576469231921
step 11. trying 4.555558462975639, variation: 0.00011089880839865174
step 12. trying 4.55553957996306, variation: 1.8883012578463365e-5
step 13. trying 4.555536364911781, variation: 3.2150512794260067e-6 *** ok
result: 4.555536364911781
|#