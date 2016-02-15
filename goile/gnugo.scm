;;  Copyright (C) 2016
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (goile gnugo)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (goile irregex)
  #:export (init-goile))

(define *gnugo* #f)

(define (init-goile)
  (let ((p (open-pipe "gnugo --mode gtp" OPEN_BOTH)))
    (set! *gnugo* p)
    "ok"))

(define-syntax define-gtp-cmd
  (lambda (x)
    (syntax-case x ()
      ((_ cmd) (identifier? #'cmd)
       #`(define (#,(datum->syntax #'cmd (symbol-append 'gtp: (syntax->datum #'cmd))) . args)
           (format #t "~a ~{~a~^ ~}~%~!" 'cmd args)
           (format *gnugo* "~a ~{~a~^ ~}~%~!" 'cmd args)
           (let lp((line (read-line *gnugo*)) (ret '()))
             (cond
              ((or (not *gnugo*) (port-closed? *gnugo*))
               (format #t "Seems not properly inited, run init-goile!~%"))
              ((eq? 'cmd 'quit) (close *gnugo*))
              ((string-null? line) ret)
              (else
               (display line)
               (newline)
               (lp (read-line *gnugo*) (cons line ret)))))))
      ((_ cmd typeit)
       (typeit (define-gtp-cmd cmd))))))

(define (->vertices x)
  (irregex-extract "[A-Z]\\d+" (car x)))

(define (->color x)
  (let ((xx (car x)))
    (cond
     ((string=? "= empty" xx) #f)
     (else (substring xx 2))))

(define (->move x)
  (string-split (substring (car x) 2) #\sp))

(define (->moves x)
  (map (lambda (xx) (string-split (substring xx 2) #\sp)) x))

(define (->boolean x)
  (or (string=? x "= 1") (string=? x "= true")))

(define-gtp-cmd version)
(define-gtp-cmd quit)
(define-gtp-cmd protocol_version)
(define-gtp-cmd boardsize)
(define-gtp-cmd clear_board)
(define-gtp-cmd orientation)
(define-gtp-cmd query_orientation)
(define-gtp-cmd komi)
(define-gtp-cmd get_komi)
(define-gtp-cmd play) ; -> color -> vertex
;; TODO: (gtp:replay vertices #:key (color "black"))
(define-gtp-cmd fixed_handicap ->vertices) ; -> number_of_stones
(define-gtp-cmd place_free_handicap ->vertices) ; -> number_of_stones
(define-gtp-cmd set_free_handicap) ; -> vertices
(define-gtp-cmd get_handicap)
(define-gtp-cmd loadsgf) ; -> path -> move_number_or_vertex
(define-gtp-cmd color ->color) ; -> vertex
(define-gtp-cmd list_stones ->vertices) ; -> color
(define-gtp-cmd countlib ->integer) ; -> vertex
(define-gtp-cmd findlib ->vertices) ; -> vertex
(define-gtp-cmd accuratelib ->vertices) ; -> color -> vertex
(define-gtp-cmd is_legal ->boolean) ; -> color -> vertex
(define-gtp-cmd all_legal ->vertices) ; -> color
(define-gtp-cmd captures ->integer) ; -> color
(define-gtp-cmd last_move ->move) ; -> last_move
(define-gtp-cmd move_history ->moves)
;; TODO: (gtp:over?)
(define-gtp-cmd invariant_hash)
(define-gtp-cmd invariant_hash_for_moves ->moves) ; -> color
(define-gtp-cmd trymove) ; -> color -> vertex
(define-gtp-cmd tryko) ; -> color -> vertex
(define-gtp-cmd popgo)
(define-gtp-cmd clear_cache)
(define-gtp-cmd increase_depths)
(define-gtp-cmd decrease_depths)
(define-gtp-cmd unconditional_status) ; -> vertex
(define-gtp-cmd genmove) ; -> color
(define-gtp-cmd reg_genmove) ; -> color
(define-gtp-cmd gg_genmove) ; -> color -> random_seed
(define-gtp-cmd restricted_genmove) ; -> color -> vertices
(define-gtp-cmd kgs_genmove_cleanup) ; -> color
(define-gtp-cmd level) ; -> level
(define-gtp-cmd undo)
(define-gtp-cmd gg_undo) ; -> moves
(define-gtp-cmd time_settings) ; -> main_time -> byo_yomi_time -> byo_yomi_stones
(define-gtp-cmd time_left) ; -> color -> time -> stones
(define-gtp-cmd final_score) ; -> random_seed
(define-gtp-cmd final_status) ; -> vertex -> random_seed
(define-gtp-cmd final_status_list ->vertices) ; -> status -> random_seed
(define-gtp-cmd estimate_score)
(define-gtp-cmd experimental_score) ; -> color
(define-gtp-cmd reset_owl_node_counter)
(define-gtp-cmd get_owl_node_counter)
(define-gtp-cmd reset_reading_node_counter)
(define-gtp-cmd get_reading_node_counter)
(define-gtp-cmd reset_trymove_node_counter)
(define-gtp-cmd get_trymove_node_counter)
(define-gtp-cmd reset_connection_node_counter)
(define-gtp-cmd get_connection_node_counter)
(define-gtp-cmd cputime)
(define-gtp-cmd showboard ->board)
(define-gtp-cmd printsgf) ; -> path
(define-gtp-cmd tune_move_ordering) ; -> move_ordering_parameters
(define-gtp-cmd echo) ; -> string
(define-gtp-cmd echo_err) ; -> string
(define-gtp-cmd help ->lines) ; -> help
(define-gtp-cmd known_command ->boolean) ; -> command
(define-gtp-cmd report_uncertainty) ; on_or_off
(define-gtp-cmd get_random_seed)
(define-gtp-cmd set_random_seed) ; -> random_seed
(define-gtp-cmd advance_random_seed) ; -> games
(define-gtp-cmd set_search_diamond) ; -> position
(define-gtp-cmd reset_search_mask)
(define-gtp-cmd limit_search) ; -> value
(define-gtp-cmd set_search_limit) ; -> position
(define-gtp-cmd draw_search_area)
