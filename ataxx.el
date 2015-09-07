;;; ataxx.el --- the ataxx game for emacs

;; Copyright (C) 2004, 2005 Mario Domenech Goulart

;; Author: Mario Domenech Goulart
;; Maintainer: Mario Domenech Goulart
;; Created: nov 2004
;; Keywords: games strategy
;; Version: 0.1
;; 
;; $Id: ataxx.el,v 1.4 2004/12/01 01:42:31 mario Exp $

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The aim of ataxx is to fill in all the squares.  To get a more
;; detailed explanation, check the following sites:

;; http://gameting.vetorial.net/jogo.php?id=3&eng=0 (portuguese)
;; http://gameting.vetorial.net/jogo.php?id=3&eng=1 (english)

;;; Code:

(defvar ataxx-buffer-name "*ataxx*"
  "Ataxx buffer name.")

(defconst ataxx-board-size 7
  "Ataxx's board size.")

(defconst ataxx-board-width 30
  "Ataxx board width (in characters).")

(defconst ataxx-x ?x
  "Ataxx player x.")

(defconst ataxx-o ?o
  "Ataxx player o.")

(defconst ataxx-start-point (+ ataxx-board-width 3)
  "Position of the cursor right after the game begins.")

(defvar ataxx-initial-player ataxx-x
  "The player who starts the game.")

(defvar ataxx-current-player ataxx-initial-player
  "Global variable which indicates the current player.")

(defvar ataxx-current-board nil
  "Global variable which keeps the current board.")

(defvar ataxx-mark nil
  "Global variable which indicates whether the current player
has set mark to the movement. If so, this variable holds the 
marked position  (a cons (x . y)), otherwise, nil.")

;;;
;;; Score variables
;;;
(defconst ataxx-message-plate
"+----------------------------
|                           |
+---------------------------+
| SCORE:   x =              |
|          o =              |
+---------------------------+
")

(defconst ataxx-message-plate-length ataxx-board-width
  "Length (in chars) of the message plate.")

(defconst ataxx-messages-pos 483
  "Position of messages on the screen.")

(defconst ataxx-x-score-pos 557
  "Position of the x score on the screen.")

(defconst ataxx-o-score-pos 587
  "Position of the o score on the screen.")

;;; end of Score variables

(defvar ataxx-mode-map nil
  "Local keymap for the ataxx game.")

(unless ataxx-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?"                       #'describe-mode)
    (define-key map "q"                       #'ataxx-quit-game)
    (define-key map [up]                      #'ataxx-move-up)
    (define-key map [down]                    #'ataxx-move-down)
    (define-key map [left]                    #'ataxx-move-left)
    (define-key map [right]                   #'ataxx-move-right)
    (define-key map [(meta <)]                #'ataxx-goto-start-pos)
    (define-key map [(meta >)]                #'ataxx-goto-end-pos)
    (define-key map [(control a)]             #'ataxx-goto-bol)
    (define-key map [(control e)]             #'ataxx-goto-eol)
    (define-key map [prior]                   #'ataxx-goto-boc)
    (define-key map [next]                    #'ataxx-goto-eoc)
    (define-key map [home]                    #'ataxx-goto-start-pos)
    (define-key map [return]                  #'ataxx-mark-or-move)
    (define-key map " "                       #'ataxx-mark-or-move)
    (define-key map [end]                     #'ataxx-goto-end-pos)
    (setq ataxx-mode-map map)))

;;;
;;; Motion functions
;;;
;;;###autoload
(defun ataxx-move-right ()
  "Move right."
  (interactive)
  (let* ((coord (ataxx-coord-from-pos))
	 (x (car coord))
	 (y (cdr coord)))
    (if (= x 6)
	(goto-char (ataxx-pos-from-coord 0 y))
      (goto-char (ataxx-pos-from-coord (+ x 1) y)))))

;;;###autoload
(defun ataxx-move-left ()
  "Move left."
  (interactive)
  (let* ((coord (ataxx-coord-from-pos))
	 (x (car coord))
	 (y (cdr coord)))
    (if (= x 0)
	(goto-char (ataxx-pos-from-coord 6 y))
      (goto-char (ataxx-pos-from-coord (- x 1) y)))))

;;;###autoload  
(defun ataxx-move-up ()
  "Move up."
  (interactive)
  (let* ((coord (ataxx-coord-from-pos))
	 (x (car coord))
	 (y (cdr coord)))
    (if (= y 0)
	(goto-char (ataxx-pos-from-coord x 6))
      (goto-char (ataxx-pos-from-coord x (- y 1))))))
  
;;;###autoload
(defun ataxx-move-down ()
  "Move down."
  (interactive)
  (let* ((coord (ataxx-coord-from-pos))
	 (x (car coord))
	 (y (cdr coord)))
    (if (= y 6)
	(goto-char (ataxx-pos-from-coord x 0))
      (goto-char (ataxx-pos-from-coord x (+ y 1))))))

;;;###autoload
(defun ataxx-goto-start-pos ()
  (interactive)
  (goto-char ataxx-start-point))

;;;###autoload
(defun ataxx-goto-end-pos ()
  (interactive)
  (goto-char (ataxx-pos-from-coord 6 6)))

;;;###autoload
(defun ataxx-goto-bol ()
  (interactive)
  (beginning-of-line)
  (forward-char 2))

;;;###autoload
(defun ataxx-goto-eol ()
  (interactive)
  (end-of-line)
  (backward-char 3))

;;;###autoload
(defun ataxx-goto-boc ()
  "Move to the beginning of the current column."
  (interactive)
  (goto-char (ataxx-pos-from-coord 
	      (car (ataxx-coord-from-pos)) 0)))

;;;###autoload
(defun ataxx-goto-eoc ()
  "Move to the end of the current column."
  (interactive)
  (goto-char (ataxx-pos-from-coord 
	      (car (ataxx-coord-from-pos)) 6)))

;;; end of Motion functions


;;;
;;; Screen functions
;;;
(defun ataxx-message (text)
  "Prints message TEXT at the plate's message area."
  (let ((filltext-length
	 (if (integerp text)
	     5
	   (+ (length text) 4))))
    (save-excursion
      (goto-char ataxx-messages-pos)
      (ataxx-insert 
       (concat text 
	       (make-string 
		(- ataxx-message-plate-length 
		   filltext-length) ?\ ))))))

(defun ataxx-insert (text)
  "Inserts TEXT to the screen (at point)."
  (setq buffer-read-only nil)
  (let ((filltext-length 
	 (if (integerp text)
	     1
	   (length text))))
    (save-excursion
      (delete-region (point) (+ (point) filltext-length))
      (insert text))
    (setq buffer-read-only t)))

(defun ataxx-place-player-on-screen (player x y)
  "Inserts PLAYER on the screen, at coordinate X Y."
  (let ((p (or player " ")))
    (goto-char (ataxx-pos-from-coord x y))
    (ataxx-insert p)))

(defun ataxx-error (text)
  "Prints a message error (TEXT) and returns nil."
  (ataxx-message text)
  nil)

(defun ataxx-update-score (new-score)
  "Updates the score on screen at the score's plate."
  (save-excursion
    (goto-char ataxx-x-score-pos)
    (ataxx-insert (format "%s    " (car new-score)))
    (goto-char ataxx-o-score-pos)
    (ataxx-insert (format "%s    " (cdr new-score)))))

(defun ataxx-init-screen ()
  "Assembly the ataxx board and the message plate and print them
 the ataxx buffer."
  (pop-to-buffer (get-buffer-create ataxx-buffer-name))
  (delete-other-windows (get-buffer-window ataxx-buffer-name))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (setq ataxx-current-player ataxx-initial-player)
  (let ((vborder "+---")
	(hborder "|   "))
    (dotimes (y ataxx-board-size)
      (dotimes (x ataxx-board-size)
	(insert vborder))
      (insert "+\n")
      (dotimes (x ataxx-board-size)
	(insert hborder))
      (insert "|\n"))
      (dotimes (x ataxx-board-size)
	(insert vborder))
      (insert "+\n"))
  (insert ataxx-message-plate)
  (ataxx-goto-start-pos)
  (setq buffer-read-only nil))

(defun ataxx-refresh-screen (board score)
  (save-excursion
    (ataxx-update-score score)
    (dotimes (x ataxx-board-size)
      (dotimes (y ataxx-board-size)
	(ataxx-place-player-on-screen 
	 (ataxx-board-get board x y) x y)))))
  
;;; end of screen functions


;;;
;;; Board functions
;;;
(defun ataxx-create-board ()
  "Creates the data structure (a bidimensional matrix) to represent
 the ataxx board."
  (let ((board (make-vector ataxx-board-size nil)))
    (dotimes (i ataxx-board-size)
      (aset board i (make-vector ataxx-board-size nil)))
    board))

(defun ataxx-board-set (board x y value)
  "Sets the element at board's coordinate X Y and returns the board."
  (aset (aref board x) y value)
  board)

(defun ataxx-board-get (board x y)
  "Returns the element at board's coordinate X Y."
  (aref (aref board x) y))

(defun ataxx-init-board (board)
  "Sets the initial configuration of the board (one player at each 
corner) and returns the board."
  (ataxx-board-set board 0 0 ataxx-x)
  (ataxx-board-set board (1- ataxx-board-size) (1- ataxx-board-size) ataxx-x)
  (ataxx-board-set board 0 (1- ataxx-board-size) ataxx-o)
  (ataxx-board-set board (1- ataxx-board-size) 0 ataxx-o)
  board)

(defun ataxx-board-count (board elt)
  "Returns the number of occurrences of ELT in BOARD."
  (let ((num-elts 0))
    (dotimes (i ataxx-board-size)
      (dotimes (j ataxx-board-size)
	(if (ataxx-players-eq 
	     elt (ataxx-board-get board i j))
	    (setq num-elts (1+ num-elts)))))
    num-elts))

;;; end of Board functions

;;;
;;; Predicate functions
;;;
(defun valid-coord-p (x y)
  "Returns t if X Y is a valid coordinate, nil otherwise."
  (cond
   ((or (> x 6) (> y 6)) nil)
   ((or (< x 0) (< y 0)) nil)
   (t t)))

(defun ataxx-x-p (player)
  "Returns t if PLAYER is the x player, nil otherwise."
  (= player ataxx-x))

(defun ataxx-o-p (player)
  "Returns t if PLAYER is the o player, nil otherwise."
  (= player ataxx-o))

(defun ataxx-gameover-p (board)
  "Returns t if the game is over, nil otherwise."
   (or (zerop (ataxx-board-count board ataxx-x))
       (zerop (ataxx-board-count board ataxx-o))
       (zerop (ataxx-board-count board nil))))

;;; end of Predicate functions

(defun ataxx-line-number (&optional pos)
  "Returns the line number."
  (save-excursion
    (when pos (goto-char pos))
    (1+ (count-lines 1 (point-at-bol)))))

(defun ataxx-pos-from-coord (x y)
  "Returns the point position relative to the coordinate x y."
  (+ (* y ataxx-board-width 2) ataxx-board-width (* 4 x) 3))

(defun ataxx-coord-from-pos ()
  "Returns a cons (x . y)"
  (let ((pos (point)))
    (cons 
     (truncate (/ (- pos (* ataxx-board-width (1- (ataxx-line-number pos)))) 4))
     (truncate (/ (1- (/ pos ataxx-board-width)) 2)))))

(defun ataxx-players-eq (a b)
  "Returns t if A and B are the same player."
  (cond ((and (null a) (null b)) t)
	((and (null a) (not (null b))) nil)
	((and (null b) (not (null a))) nil)
	((= a b) t)))

(defun ataxx-score (board)
  "Returns a cons (score-x . score-o) indicating the game's 
current score."
  (cons (ataxx-board-count board ataxx-x)
	(ataxx-board-count board ataxx-o)))


(defun ataxx-player-at-coord-p (board player x y)
  "Returns true if PLAYER is at BOARD's coordinate X Y, nil otherwise"
  (ataxx-players-eq player (ataxx-board-get board x y)))

(defun ataxx-eval-movement (board xd yd)
  ;(message "orig (%d . %d), dest (%d . %d)" (car ataxx-mark) (cdr ataxx-mark) (car dest) (cdr dest))
  (let ((xo (car ataxx-mark))
	(yo (cdr ataxx-mark)))
    (cond 
     ((not (ataxx-player-at-coord-p board nil xd yd))
      (ataxx-error "Invalid movement."))
     
     ;; Origin and destination can't be the same
     ((and (= xo xd) (= yo yd))
	(ataxx-error "Invalid movement."))

     ;; Copy movements
     ((and
	(or (= (+ xo 1) xd) (= (- xo 1) xd) (= xo xd))
	(or (= (+ yo 1) yd) (= (- yo 1) yd) (= yo yd)))
      (ataxx-copy board xd yd))

     ;; Move movements (I know there must be a simpler way...)
      ((or
	(and (= xo (+ xd 2)) (= yo (- yd 2)))
	(and (= xo (+ xd 2)) (= yo (- yd 1)))
	(and (= xo (+ xd 2)) (= yo yd))
	(and (= xo (+ xd 2)) (= yo (+ yd 2)))
	(and (= xo (+ xd 2)) (= yo (+ yd 1)))
	
	(and (= xo (- xd 2)) (= yo (- yd 2)))
	(and (= xo (- xd 2)) (= yo (- yd 1)))
	(and (= xo (- xd 2)) (= yo yd))
	(and (= xo (- xd 2)) (= yo (+ yd 2)))
	(and (= xo (- xd 2)) (= yo (+ yd 1)))
	
	(and (= xo (- xd 1)) (= yo (- yd 2)))
	(and (= xo (- xd 1)) (= yo (+ yd 2)))
	
	(and (= xo xd) (= yo (- yd 2)))
	(and (= xo xd) (= yo (+ yd 2)))
	
	(and (= xo (+ xd 1)) (= yo (- yd 2)))
	(and (= xo (+ xd 1)) (= yo (+ yd 2))))

       (ataxx-move board xd yd))
      (t (ataxx-error "Invalid movement.")))))

(defun ataxx-copy (board xd yd)
  (ataxx-board-set board xd yd ataxx-current-player)
  (ataxx-delete-around board xd yd))


(defun ataxx-move (board xd yd)
  (ataxx-board-set board xd yd ataxx-current-player)
  (ataxx-board-set board (car ataxx-mark) (cdr ataxx-mark) nil)
  (ataxx-delete-around board xd yd))

(defun ataxx-delete-around (board xo yo)
  "Delete adversarie's pieces around the coordinate X Y and returns
the resulting board."
  (let ((adversary (ataxx-adversary)))
    (mapc (lambda (coord)
	    (let ((x (car coord))
		  (y (cdr coord)))
	      (if (and (valid-coord-p x y) 
		       (ataxx-player-at-coord-p board adversary x y))
		  (ataxx-board-set board x y ataxx-current-player))))
	  (list (cons (1+ xo) yo)
		(cons xo (1+ yo))
		(cons (1- xo) yo)
		(cons xo (1- yo))
		(cons (1+ xo) (1+ yo))
		(cons (1- xo) (1- yo))
		(cons (1- xo) (1+ yo))
		(cons (1+ xo) (1- yo))))
    board))

(defun ataxx-adversary ()
  "Returns the current adversary (i.e., the opposite of the
current player)."
  (if (ataxx-x-p ataxx-current-player)
      ataxx-o
    ataxx-x))

;;;###autoload
(defun ataxx-mark-or-move ()
  "Set mark or move player.  If mark is not set, set it, otherwise 
play."
  (interactive)
  (if ataxx-mark
      (ataxx-do-play ataxx-current-board)
    (ataxx-set-marked)))

(defun ataxx-set-marked ()
  "Set mark at point."
  (if (looking-at (char-to-string ataxx-current-player))
      (progn
	(setq ataxx-mark (ataxx-coord-from-pos))
	(ataxx-message (format "Player %c has set mark." 
			       ataxx-current-player)))
    (ataxx-message "Invalid movement.")))

(defun ataxx-do-play (board)
  (let* ((coord (ataxx-coord-from-pos))
	 (x (car coord))
	 (y (cdr coord))
	 (tbl (ataxx-eval-movement board x y)))
    (if tbl
      (progn
	(setq ataxx-current-player (ataxx-adversary)
	      ataxx-mark nil)
	  (save-excursion
	    (ataxx-refresh-screen tbl (ataxx-score tbl)))
	  (setq ataxx-current-board tbl)
	(if (ataxx-gameover-p board)
	    (progn
	      (ataxx-message "GAME OVER!")
	      (ataxx-new-game))
	  (ataxx-message (format "It's your turn, player %c." 
				 ataxx-current-player)))))))

(defun ataxx-mode ()
  "A mode for playing ataxx

The key bindings for ataxx-mode are:

\\{ataxx-mode-map}"
  (kill-all-local-variables)
  (use-local-map ataxx-mode-map)
  (setq major-mode 'ataxx-mode
        mode-name  "ataxx")
  (run-hooks 'ataxx-mode-hook)  
  (setq buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo (current-buffer)))

;;;###autoload
(defun ataxx ()
  (interactive)
  (ataxx-init-screen)
  (ataxx-mode)
  (setq ataxx-current-board (ataxx-init-board 
			     (ataxx-create-board)))
  (ataxx-refresh-screen ataxx-current-board 
			(ataxx-score ataxx-current-board))
  (ataxx-message (format "It's your turn, player %c." 
			 ataxx-current-player)))


;;;###autoload
(defun ataxx-new-game ()
  (interactive)
  (if (y-or-n-p "Start a new game? ") 
	(ataxx)))

  