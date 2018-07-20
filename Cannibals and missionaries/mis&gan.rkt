#lang racket

(define (make-state m c s)  ;миссионеры канибалы сторона
  (list m c s))

(define (missioners-count state) ;номер миссионера в списке 0 возвращаем колво миссионеров
  (list-ref state 0))     ;list-ref lst pos возвращает элемент на позиции pos в списке lst

(define (cannibals-count state)
  (list-ref state 1))

(define (boat-side state)
  (list-ref state 2))

(define (opposite side)      ;меняем стороны
  (cond ((equal? side 'e) 'w)  ;если на вход е то вернем w 
        ((equal? side 'w) 'e)))  ;если на вход w то вернем е

(define (safe state)
(cond
   [(equal? (missioners-count state)(cannibals-count state))  state]  ;проверка безопасности перехода если равное количество миссионеров и ганибаллов
   [(equal? (missioners-count state) 3)  state]      ;если количество миссионеров 3 возвращаем список
   [(equal? (missioners-count state) 0)  state]      ;если количество миссионеров 0 
   (null))         ;если ни одно из условий не выполнено возвращаем null
  )

(define (missi-missi state)   ;переброска 2 миссионеров
 (or (safe (make-state (- '5 (missioners-count state))
                             (- '3 (cannibals-count state))
                             (opposite (boat-side state))))
     (safe (make-state (-  (missioners-count state) '2)
                           (cannibals-count state)
                           (boat-side state)))))
       
(define (missi state)
   (or (safe (make-state (- '4 (missioners-count state))
                         (- '3 (cannibals-count state))
                         (opposite (boat-side state))))
        (safe (make-state (-  (missioners-count state) '1)
                          (cannibals-count state)
                          ( boat-side state))))) 

(define (missi-can state)
   (or (safe (make-state (- '4 (missioners-count state))
                         (- '4 (cannibals-count state))
                         (opposite (boat-side state))))))
      
(define (can-can state)
   (or (safe (make-state (- '3 (missioners-count state))
                         (- '5 (cannibals-count state))
                         (opposite (boat-side state))))
        (safe (make-state (missioners-count state)
                          (- (cannibals-count state) '2)
                          (boat-side state)))))
  
(define (can state)
   (or (safe (make-state (- '3 (missioners-count state))
                         (- '4 (cannibals-count state))
                         (opposite (boat-side state))))
        (safe (make-state (missioners-count state)
                          (-  (cannibals-count state) '1)
                          (boat-side state)))))
 
(define (path state goal been-list)
  (cond ((empty? state) #f)      ;проверяем на пустоту (если пусто то фолс)
        ((equal? state goal) (reverse (cons state been-list)))   
        ((not  (member state been-list)) ;если состояние не входит в список посещенных вершин то выдаст ф,  инвертируем
         (or  (path (missi-can state) goal (cons state been-list))
              (path (can state) goal (cons state been-list))
              (path (missi-missi state) goal (cons state been-list))
              (path (can-can state) goal (cons state been-list))
              (path (missi state) goal (cons state been-list)))            
            )            
        (#t #f)))

(define (solve-mcs state goal)   ;определяем переменные для выычисления пути (состояние цель пройденные вершины)
  (path state goal null))

(solve-mcs '(3 3 e) '(3 3 w))  ;определяем исходные данные что имеем и что должны получить






