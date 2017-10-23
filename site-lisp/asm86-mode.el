;;; asm86-mode.el -- major mode for Asm86 assembly
;;
;; Author: Gabe Wenz <wenz@ugcs.caltech.edu>
;; Maintainer: Gabe Wenz <wenz@ugcs.caltech.edu>
;; Updated by: Glen George <gleng@its.caltech.edu>
;; Created: 1 Feb 2007
;; Version: 0.907
;; Keywords: asm86, intel
;;
;;
;; Copyright (C) 2002 - 2007 Gabe Wenz, Glen George
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;
;;; Commentary:
;;
;; Provides support for Asm86 assembly editing. The features and
;; details are discussed in reasonable detail in the major mode
;; documentation string.
;;
;; Note that some routines will run faster if the code is byte-compiled
;; first. The most noticable function is (asm86-tab-region). 
;;
;; Additional features to add (wish-list):
;;  * Better EQU formatting (spacing)
;;  * Macro indenting/formatting
;;  * Label finding - find a label within the buffer (electric?)
;;  * Register report - determine which registers are used (altered)
;;    by an assembly function. 
;;

;;; Code: 


;;; TESTING AIDS ;;;
;(global-set-key [?\C-j] 'asm86-test-func)

(defun asm86-test-func ()
  (interactive)
  (if (asm86-indent-line)
      (message "TRUE")
    (message "FASLE")))


;;;;;;;;;;;;;;;;;;;;; ASM86 MODE VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This variable should be customized to your liking. Also edit the
;; function (asm86-insert-func-header) for additional customization.
(defvar asm86-extended-style-headers nil
  "Specifies the function header style. Non-nil gives thorough extended style headers; nil gives simpler headers.") 

;; Defining new values for these strings will customize function
;; and file headers. After inserting a header, the function places
;; the cursor at the end of the first inserted line, so this should
;; be a convenient place to begin typing.

(defvar asm86-function-header-string "\;\;\;\;\;\;\;\;\; Function \n\;\;\; \n\;\;\; Description: \n\;\;\; \n\;\;\; Args: \n\;\;\; Return Val: \n\;\;\; \n\;\;\; Input: \n\;\;\; Output: \n\;\;\; \n\;\;\; Error Handling: \n\;\;\; \n\;\;\; Registers Used: \n\;\;\; Stack Depth: \n\;\;\; \n\;\;\;\;\;\;\;\;\; \n\n"
  "Custom string for function headers.")

(defvar asm86-file-header-string "\;\;\;\;\;\;\;\;\; File: \n\;\;\; \n\;\;\; Description: \n\;\;\;   \n\;\;\; \n\;\;\; Function Summary: \n\;\;\;   \n\;\;\; \n\;\;\; Author: \n\;\;\; \n\;\;\; Revision History: \n\;\;\;   // - Initial version. \n\;\;\; \n\;\;\; Current Status: \n\;\;\; \n\;\;\;\;\;\;\;\;\; \n\n"
  "Custom string for file headers.")


;; Defines the author of the code which is automatically inserted
;; into extended function headers and file headers.

(defvar asm86-author ""
  "*String containing author's name.")


(defvar asm86-ignore-commented-jumps nil
  "Whether to treat commented jump statements as instructions or
ordinary comments. When t, they are not treated specially. When
nil (default), a semicolon followed by a J is NOT considered
a comment.")


;; Control whether or not "electric" charcters are used:
(defvar asm86-electric-colon-on t
  "When non-nil, colon automatically re-tabs the line. When nil, 
it simply inserts itself.")

(defvar asm86-electric-semicolon-on t
  "When non-nil, semicolon automatically re-tabs the line. When nil, 
it simply inserts itself.")

(defvar asm86-electric-gap-size 6
  "How many spaces between the *start* of an instruction and the
start of its argument. When zero or negative, electric gap (via the
TAB key) is turned off.")


;;;; INDENTATION SYMBOLS ;;;;

;; Each type of line has a certain number of spaces it should be
;; indented. These indentation values are customizable, however.
;; So we define all of the symbols using the (defvar ...) function.
;; The number in the declaration the default value, if the user has not
;; specified a particular value already (in the .emacs file).
;;
(defvar asm86-mod-base-offset 5
  "*Indentation for the module name (typically the first line)")
(defvar asm86-segment-base-offset 0
  "*Indentation for a segment definition statement")
(defvar asm86-end-base-offset 0
  "*Indentation for a segment or module END statement")
(defvar asm86-assume-base-offset 5
  "*Indentation for an ASSUME statement")
(defvar asm86-group-base-offset 0
  "*Indentation for a GROUP statement")
(defvar asm86-include-base-offset 0
  "*Indentation for an $INCLUDE statement")
(defvar asm86-extrn-base-offset 5
  "*Indentation for an EXTRN statement")
(defvar asm86-equ-base-offset 0
  "*Indentation for an EQU")
(defvar asm86-proc-start-base-offset 0
  "*Indentation for a procedure definition")
(defvar asm86-proc-end-base-offset 0
  "*Indentation for a procedure close")
(defvar asm86-variable-base-offset 3
  "*Indentation for a variable definition")
(defvar asm86-scope-ident-base-offset 8
  "*Indentation for a PUBLIC identifier")
(defvar asm86-table-base-offset 0
  "*Indentation for a table")
(defvar asm86-tab-entry-base-offset 3
  "*Indentation for a table entry")

;; for editing convenience, blank lines behave like
;; instruction lines (default)
(defvar asm86-blank-base-offset 3
  "*Indentation for a blank line")
(defvar asm86-label-base-offset 0
  "*Indentation for an ordinary code label")
(defvar asm86-header-comment-base-offset 0
  "*Indentation for a header comment (three semi-colons)")
(defvar asm86-code-comment-base-offset 3    ; same as inst
  "*Indentation for a code comment (two semi-colons)")
(defvar asm86-inline-comment-base-offset 3
  "*Indentation for an inline comment (single semi-colon)")
(defvar asm86-inst-base-offset 3
  "*Indentation for any line with code")


;; In addition to the base indentation, some lines are
;; indented more when the occur within a function (PROC)
;; definition. The set of variables below control this
;; additional indentation.
;;
;; Note that many of these should not occur within a 
;; function definition anyway, so they are set to zero. 
;;
(defvar asm86-mod-func-offset 0
  "*Indentation for the module name (typically the first line)")
(defvar asm86-segment-func-offset 0
  "*Indentation for a segment definition statement")
(defvar asm86-end-func-offset 0
  "*Indentation for a segment or module END statement")
(defvar asm86-assume-func-offset 0
  "*Indentation for an ASSUME statement")
(defvar asm86-group-func-offset 0
  "*Indentation for a GROUP statement")
(defvar asm86-include-func-offset 0
  "*Indentation for an $INCLUDE statement")
(defvar asm86-extrn-func-offset 0
  "*Indentation for an EXTRN statement")
(defvar asm86-equ-func-offset 0
  "*Indentation for an EQU")
(defvar asm86-proc-start-func-offset 0
  "*Indentation for a procedure definition")
(defvar asm86-proc-end-func-offset 0
  "*Indentation for a procedure close")
(defvar asm86-variable-func-offset 0
  "*Indentation for a variable definition")
(defvar asm86-scope-ident-func-offset 0
  "*Indentation for a PUBLIC identifier")
(defvar asm86-table-func-offset 0
  "*Indentation for a table")
(defvar asm86-tab-entry-func-offset 0
  "*Indentation for a table entry")

;; Basically, all the "code" stuff moves in three additional 
;; spaces when inside a function:
(defvar asm86-blank-func-offset 3
  "*Indentation for a blank line")
(defvar asm86-label-func-offset 3
  "*Indentation for an ordinary code label")
(defvar asm86-header-comment-func-offset 3
  "*Indentation for a header comment (three semi-colons)")
(defvar asm86-code-comment-func-offset 3
  "*Indentation for a code comment (two semi-colons)")
(defvar asm86-inline-comment-func-offset 3
  "*Indentation for an inline comment (single semi-colon)")
(defvar asm86-inst-func-offset 3
  "*Indentation for any line with code")


;;; Keywords, etc. for syntax highlighting, using font-lock minor mode
(defvar asm86-font-lock-keywords
  '(
    ("\\<\\(include\\|assume\\|group\\|segment\\|ends\\|name\\|end\\|equ\\|label\\|extrn\\|byte\\|word\\)\\>" . font-lock-keyword-face)
    ("\\<\\(proc\\|endp\\|public\\|near\\|far\\)\\>" . font-lock-function-name-face)
    ("\\<\\(db\\|dw\\|dd\\|dq\\|dt\\)\\>" . font-lock-type-face)
    ("^\\s-*[A-Za-z0-9_\\%\\?]+:" . font-lock-constant-face))
  "Default expressions to highlight in Asm86 mode.")

;;;;;;;;;;;;;;;;;;;;; END ASM86 MODE VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;; ASM86 MODE INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Function asm86-mode [Interactive]
;;
;; Function that is called whenever asm86 major mode is entered. Does
;; all of the critical major mode initialization stuff:
;;  1) Keymap
;;  2) Syntax table
;;  3) Local variables
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  06/28/01 - Initial version.
;;  01/07/02 - Turned off Emacs Local Variables processing. 
;;  04/02/02 - Added code for using font-lock minor mode. 
;;
(defun asm86-mode ()
 "v. 0.907

Major mode to aid in indenting, commenting, and other
formatting of Asm86 assembly code. Provides syntax 
highlighting, if desired. 

Semicolons start comments. Single semicolon is considered
inline comment; double semicolon is conisdered code
comment; triple semicolon is considered header comment. 
All indentation and formatting uses spaces. 

For complete documenatation, see the information on the Web. 

Entry to this mode calls the value of \'asm86-mode-hook\' if
that value is bound and has a non-nil value. 

\\{asm86-mode-map}"               ; end of documentation

  (interactive)
  
  (kill-all-local-variables)            ; usually done at the start of a mode
  (setq major-mode 'asm86-mode          ; point to this function
	mode-name "Asm86")              ; a pretty name for the mode

  (defvar asm86-mode-abbrev-table () "")
  (if asm86-mode-abbrev-table
      ()
    (setq asm86-mode-abbrev-table (make-abbrev-table)))  ; empty abbrev table
  (setq local-abbrev-table asm86-mode-abbrev-table)
    
  ;; Helper functions do most of the critical work:
  (asm86-mode-syntax-table)
  (asm86-mode-keymap)
  (asm86-mode-vars)

  ;; Variables for font-lock mode (syntax highlighting):
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(asm86-font-lock-keywords nil t 
     ;; Define important syntax table entries (for font-lock):
     ((?' . "\"")                       ; string quote
      (?; . "<")                        ; comment start (semicolon)
       (?\n . ">"))                     ; comment end (end of line)
     beginning-of-line))

  ;; Miscellaneous things to do: 
  (setq enable-local-variables nil)  ; else this causes complaints with function
                                     ; headers containing the string "Local Variables:"
  (setq indent-tabs-mode nil)        ; don't want tabs screwing up the formatting 
  ;;(auto-fill-mode)              ; handy for comments; you may not like it, though
  (message "Asm86 Mode Installed")
  
  (run-hooks 'asm86-mode-hook))  ; run any additional user customization


;;; Function asm86-mode-vars
;;
;; Helper function for (asm86-mode) that sets up all of the miscellaneous
;; variables for asm86-mode.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/08/01 - Initial version.
;;
(defun asm86-mode-vars ()

  ;; Indentation variables: 
  (make-local-variable 'indent-line-function) ; function that is run when TAB is pressed
  ;(make-local-variable 'indent-region-function)  ; didn't implement this right.. 

  (setq indent-line-function 'asm86-tab)
  ;(setq indent-region-function 'asm86-tab-region) ; faster implementation (?)

  ;; Commenting variables:
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-start-skip)
  
  (setq comment-start "\; "     ; the space is just for style
        comment-end ""          ; no character(s) to finish a comment 
        comment-multi-line nil
	;; When skipping past the start of a comment, skip over the first semicolon,
	;; plus any immediate semicolons or spaces. Note that this is a relatively
	;; inflexible definition, as it does not correctly handle quoting, etc.
	;; For an example of a *real* comment-skip-start regular expression, see the
	;; one used for lisp mode (lisp-mode.el). 
        comment-start-skip "\;[\ \;]*")	; quote both the semicolon and space
  
  ;; An association list controls the base indentation for each type
  ;; of assembly line. The CAR indicates the symbol, while the CDR is
  ;; the number of spaces to indent the line. 
  (setq asm86-base-indent-vals (list
      (cons 'asm86-blank asm86-blank-base-offset)
      (cons 'asm86-mod asm86-mod-base-offset)
      (cons 'asm86-segment asm86-segment-base-offset)
      (cons 'asm86-end asm86-end-base-offset)
      (cons 'asm86-assume asm86-assume-base-offset) 
      (cons 'asm86-group asm86-group-base-offset)
      (cons 'asm86-include asm86-include-base-offset)
      (cons 'asm86-extrn asm86-extrn-base-offset)
      (cons 'asm86-equ asm86-equ-base-offset)
      (cons 'asm86-proc-start asm86-proc-start-base-offset)
      (cons 'asm86-proc-end asm86-proc-end-base-offset)
      (cons 'asm86-variable asm86-variable-base-offset)
      (cons 'asm86-scope-ident asm86-scope-ident-base-offset)
      (cons 'asm86-table asm86-table-base-offset)
      (cons 'asm86-tab-entry asm86-tab-entry-base-offset)

      (cons 'asm86-label asm86-label-base-offset)
      (cons 'asm86-header-comment asm86-header-comment-base-offset)
      (cons 'asm86-code-comment asm86-code-comment-base-offset)
      (cons 'asm86-inline-comment asm86-inline-comment-base-offset)
      (cons 'asm86-inst asm86-inst-base-offset)
      ))

  ;; An association list specifies additional indentation for each
  ;; line type when that line occurs within a function definition. The 
  ;; list setup is the same as above. 
  (setq asm86-func-indent-vals (list
      (cons 'asm86-blank asm86-blank-func-offset)
      (cons 'asm86-mod asm86-mod-func-offset)
      (cons 'asm86-segment asm86-segment-func-offset)
      (cons 'asm86-end asm86-end-func-offset)
      (cons 'asm86-assume asm86-assume-func-offset) 
      (cons 'asm86-group asm86-group-func-offset)
      (cons 'asm86-include asm86-include-func-offset)
      (cons 'asm86-extrn asm86-extrn-func-offset)
      (cons 'asm86-equ asm86-equ-func-offset)
      (cons 'asm86-proc-start asm86-proc-start-func-offset)
      (cons 'asm86-proc-end asm86-proc-end-func-offset)
      (cons 'asm86-variable asm86-variable-func-offset)
      (cons 'asm86-scope-ident asm86-scope-ident-func-offset)
      (cons 'asm86-table asm86-table-func-offset)
      (cons 'asm86-tab-entry asm86-tab-entry-func-offset)

      (cons 'asm86-label asm86-label-func-offset)
      (cons 'asm86-header-comment asm86-header-comment-func-offset)
      (cons 'asm86-code-comment asm86-code-comment-func-offset)
      (cons 'asm86-inline-comment asm86-inline-comment-func-offset)
      (cons 'asm86-inst asm86-inst-func-offset)
      )))


;;; Function asm86-mode-keymap
;;
;; Helper function for (asm86-mode) that sets up the keymap.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/08/01 - Initial version.
;;
(defun asm86-mode-keymap ()

  ;;; Make local keymap, and assign bindings: ;;; 
  (defvar asm86-mode-map () "")         ; user can customize, if desired
  (defvar asm86-ctl-c-map () "")        ; more customized function/key bindings
  (if asm86-mode-map
      ()                                ; nothing to do if already in place
    (setq asm86-mode-map (make-sparse-keymap)))
  (if asm86-ctl-c-map
      ()
    (setq asm86-ctl-c-map (make-sparse-keymap)))

  ;; In order to to multiple-key bindings (like "C-c C-t"), we need to make
  ;; an intermediate keymap, which is where emacs looks whenever C-c
  ;; is pressed. Once this has been set, we can define multiple-key
  ;; bindings like normal. 
  (define-key asm86-mode-map "\C-c" asm86-ctl-c-map)    ; intermediate keymap

  ;; these depend on asm86-ctl-c-map:
  ;(define-key asm86-mode-map "\C-c\C-g" 'asm86-get-line-type)  ; TESTING
  (define-key asm86-mode-map "\C-c\C-t" 'asm86-tab-region)
  (define-key asm86-mode-map "\C-c\C-f" 'asm86-close-function)
  (define-key asm86-mode-map "\C-c\C-u" 'asm86-uncomment-region)
  (define-key asm86-mode-map "\C-c\C-c" 'asm86-comment-region)

  (define-key asm86-mode-map "\C-c\C-h" 'asm86-insert-func-header)
  (define-key asm86-mode-map "\C-c\M-h" 'asm86-insert-file-header)

  (define-key asm86-mode-map "\C-c\C-s" 'asm86-stack-report)
  (define-key asm86-mode-map "\C-c\C-b" 'asm86-beautify)
  ;(define-key asm86-mode-map "\C-c\C-m" 'asm86-mode)    ; for TESTING convenience

  (define-key asm86-mode-map "\C-\M-\\" 'asm86-tab-region)
  (define-key asm86-mode-map "\C-\M-a" 'asm86-beginning-of-func)
  (define-key asm86-mode-map "\C-\M-e" 'asm86-end-of-func)
  ;(define-key asm86-mode-map "\C-\M-h" 'asm86-mark-func) ; not implemented

  (define-key asm86-mode-map "\:" 'asm86-colon)         ; electric colon
  (define-key asm86-mode-map "\;" 'asm86-semicolon)     ; electric semicolon

  (use-local-map asm86-mode-map))


;;; Function asm86-mode-syntax-table
;;
;; Helper function for (asm86-mode) that sets up the syntax table.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/08/01 - Initial version.
;;
(defun asm86-mode-syntax-table ()

  ;;; Make syntax table
  (defvar asm86-mode-syntax-table ()
    "Syntax table for Asm86 mode.")
  (if asm86-mode-syntax-table 
      ()                                ; already exists
    (setq asm86-mode-syntax-table (make-syntax-table))) ; need to make it 
  ; Define syntax of special assembly characters. Not sure
  ; if this is a very good job..
  (modify-syntax-entry ?\' "\"   " asm86-mode-syntax-table)   ; string quote character
  (modify-syntax-entry ?\( "()" asm86-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" asm86-mode-syntax-table)
  (modify-syntax-entry ?\% "\'" asm86-mode-syntax-table)      ; expression prefix

  (modify-syntax-entry ?* "." asm86-mode-syntax-table)        ; puncuation characters 
  (modify-syntax-entry ?+ "." asm86-mode-syntax-table)
  (modify-syntax-entry ?- "." asm86-mode-syntax-table)

  (set-syntax-table asm86-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;; END ASM86 MODE INITIALIZATION;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;; MAIN FUNCTIONS ;;;;;;;;;;;;;;;;;;;;

;;; Function asm86-beautify [Interactive]
;;
;; "Beautifies" a buffer with Asm86 code. Beautification
;; consists of the following steps:
;;   1) Convert all tabs to spaces
;;   2) Tab each line, which indents, aligns the
;;      inline comment, and adjusts the gap between
;;      the assembly instruction and its arguments.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;   12/22/01 - Initial version.
;;
(defun asm86-beautify ()
  "Format entire buffer. Converts tabs to spaces, then re-tabs each line"
  (interactive)
  (save-excursion
    (message "Formatting buffer...")

    ;; 1) Convert tabs to spaces
    (asm86-replace-tabs)

    ;; 2) Tab each line
    (set-mark (point-min))   ; mark entire buffer
    (goto-char (point-max))
    (asm86-tab-region)

    (message "Formatting buffer...done.")))


;;; Function asm86-stack-report [Interactive]
;;
;; Reports (summarizes) the stack usage of the function in which the
;; point currently lies. The summary simply opens up a temporary
;; buffer called *stack* and prints, in order of occurance, each line
;; that contains a stack operation (PUSH, POP, PUSHF, or POPF). 
;;
;; If the point is not currently inside a function, an error
;; message is displayed in the minibuffer, and no other action
;; is taken.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;   12/22/01 - Initial version.
;;
(defun asm86-stack-report ()
  "Report stack usage for function in which the point lies"
  (interactive)
  (let ((proc-name (asm86-get-proc-name))
        (proc-start (asm86-proc-begin-pos))
        (proc-end (asm86-proc-end-pos))
        (asm-buf (current-buffer)) (report-buf "*stack*")
        (stack-op nil) (old-point nil)
        (start-of-line 0) (end-of-line 0)
        (need-blank nil))
    
    (cond (proc-name         ; if inside a procedure
           ;; Prepare report buffer:
           (delete-other-windows)           ; single window
           (split-window)                   ; split it into two
           (get-buffer-create report-buf)    ; create target buffer
           (select-window (next-window))    ; goto bottom window
           (set-window-buffer (selected-window) report-buf) 
           (erase-buffer) 
           
           (insert-before-markers "Stack Report for: ")
           (insert-before-markers proc-name)
           (insert-before-markers "\n\n")
           
           (set-buffer asm-buf)       ; back to asm buffer
           (goto-char proc-start)
           (while (< (point) proc-end)
             (cond ((or (asm86-line-contains "POP")  ; is line a stack op?
                        (asm86-line-contains "PUSH")
                        (asm86-line-contains "POPF")
                        (asm86-line-contains "PUSHF"))
                    ;; stack op on this line
                    (setq need-blank t)
                    
                    ;; pretty sure there exists a better way to grab
                    ;; a line of text, but not sure what it is...
                    (setq old-point (point))
                    (beginning-of-line)
                    (setq start-of-line (point))
                    (end-of-line)
                    (setq end-of-line (point))
                    (goto-char old-point)
                    (setq stack-op (buffer-substring start-of-line end-of-line))
                    
                    (set-buffer report-buf)
                    (insert-before-markers stack-op)
                    (insert-before-markers "\n")
                    (set-buffer asm-buf))
                   
                   (t 
                    ;; no stack op on this line. If we just
                    ;; had a stack op, insert a blank line
                    ;; for better readability:
                    (cond (need-blank
                           (set-buffer report-buf)
                           (insert-before-markers "\n")
                           (set-buffer asm-buf)
                           (setq need-blank nil)))))
             (forward-line))

           ;; Finally, move the point to the beginning
           ;; of the report buffer; but keep assembly
           ;; buffer as the active one:
           (set-buffer report-buf)
           (goto-char (point-min))
           (select-window (next-window))  ; assembly buffer is current
           )
          
          (t     ; not inside a procedure
           (beep)
           (message "Not inside a procedure.")))))

  
;;; Function asm86-replace-tabs [Interactive]
;;
;; Globally replaces all tab characters in the buffer with a single
;; space. In general, tab characters mess up the visual alignment of
;; comments, indentation, etc., so running this on a file that has
;; just been opened in emacs prepares it for formatting.
;;
;; Note that replacing a tab with a single space is, itself, not very
;; visually pleasing, so plan on re-formatting the buffer after
;; running this command. 
;;
;; Args: none
;; 
;; Return: none
;;
;; Revision History:
;;   12/22/01 - Initial version.
;;
(defun asm86-replace-tabs () 
  "Replace all tabs with spaces"
  (interactive)
  (subst-char-in-region (point-min) (point-max) ?\t ?\ ))


;;; Function asm86-tab [Interactive]
;;
;; Function that gets run whenever the TAB key is pressed.
;; This runs two functions. 
;;   1) asm86-indent-line - indent the current line
;;   2) asm86-position-comment - align the inline comment
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  06/27/01 - Initial version. May remove relative comment alignment-
;;             there are times it doesn't look good. 
;;
(defun asm86-tab ()
  (interactive)
  (asm86-indent-line)
  (if (> asm86-electric-gap-size 0)
      (asm86-insert-instr-arg-gap asm86-electric-gap-size)
    ())
  (asm86-position-comment comment-column)
)



;;; Function asm86-tab-region [Interactive]
;;
;; Runs the function asm86-tab on each of the lines in the current region. 
;; In other words, indentation and comment alignment is performed. The
;; cursor is positioned at the beginning or the end of the region, depending
;; on which end it was at when the command was invoked. 
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  06/27/01 - Initial version.
;;  06/28/01 - Changed so region was correctly updated using markers. 
;;  12/27/01 - Added status messages in minibuffer, for when the
;;    region is large. 
;;

(defun asm86-tab-region ()
  "Tab all lines in the region"
  (interactive)
  (message "Tabbing region...")
  (let ((beg-mark (make-marker)) (end-mark (make-marker)))
    ;; need to track the beginning and end of region (which changes as
    ;; lines are indented) so the correct region is operated on. 
    (set-marker beg-mark (region-beginning))
    (set-marker end-mark (region-end))

    (goto-char (marker-position beg-mark))      ; start on first line of region
    (while (< (point) (marker-position end-mark))
      (asm86-tab)                       ; tab current line
      (forward-line))                   ; and try next line

    (message "Tabbing region...done.")
    ;; Put the point back to where it was:
    (if (eq (marker-position end-mark) (mark)) 
        (goto-char (marker-position beg-mark))  ; was at the beginning of the region
      (goto-char (marker-position end-mark)))   ; was at the end of the region
    ))


;;; Function asm86-indent-line [Interactive]
;;
;; Function that is called by the TAB key. Basically, the function
;; evaluates what type of assembly line the point is currently on, 
;; and based on that, decides how many spaces to indent the current 
;; line.
;;
;; Args: none
;;
;; Return: total-indent (int) - how many spaces were indented
;;
;; Revision History:
;;  06/24/01 - Initial version
;;
(defun asm86-indent-line ()
  "Adjust whitespace at beginning of line, based on line content"
  (interactive)
  (save-excursion         ; leave the point where it is in the line
    (let ((asm86-total-indent
           ;; get the indent value for the particular line:
           (cdr (assoc (asm86-get-line-type) asm86-base-indent-vals))))

      ;; if we're in a function, (possibly) indent even more:
      (if (asm86-in-proc)
          (setq asm86-total-indent 
                (+ asm86-total-indent (cdr (assoc (asm86-get-line-type) asm86-func-indent-vals))))
        nil)

      ;(message (int-to-string asm86-total-indent))  ;; interactive TESTING
      (asm86-indent-to asm86-total-indent)
      asm86-total-indent               ; return value
)))


;;; Function asm86-position-comment(pos) [Interactive]
;;
;; Attempts to move the comment on the current line to the position
;; pos on the line, where pos is an integer offset from the beinning
;; of the line. If the normal code has not finished by the said
;; position, the comment is moved to the soonest as possible starting
;; position. Only regular comments are moved (start with single, not 
;; double, semicolon). 
;;
;; This function duplicates much of the built-in functionality of 
;; the emacs function (indent-for-comment), but I like the cursor
;; positioning (it stays put within the line) better. It's especially
;; noticable when tabing a line with only an inline comment (no
;; actual code yet). 
;;
;; Args: pos (int) - positive offset from beginning of line from
;;                   which comments start
;;
;; Return: none
;;
;; Error Handling: negative values of pos are ignored. Does nothing
;;    if the line does not actually have a comment on it. 
;;
;; Revision History:
;;  06/27/01 - Initial version. 
;;  07/05/01 - Changed interpretation of what a comment was to
;;            exclude headers, but allow lines with only an inline 
;;            comment
;;  12/22/01 - Updated to use asm86-xxx-comment-xxx functions.
;;    In particular, this corrects the behavior for commented 
;;    jump statements. 
;; 
(defun asm86-position-comment (pos)
  "Position inline comment at specified column"
  (interactive)
  (cond ((< pos 0) nil)      ; ignore negative positions
        
        ; if the line doesn't have a comment, or it is a header
	; instead of a normal inline comment, then ignore:
        ((or (not (asm86-line-has-comment)) 
	     (asm86-is-header-comment)
             (asm86-is-code-comment))
         nil)

        (t                   ; input and line are valid
         (let ((cur-offset 0))
           (save-excursion    ; want point to be unchanged
             (asm86-goto-comment-start)
             (setq cur-offset (current-column))

             ; now add or delete whitespace until we are at the 
             ; correct position. 
             (cond ((<= cur-offset pos)   ; ADD spaces
                    (insert-char ?\  (- pos cur-offset)))

                   (t     ; else need to REMOVE whitespace
                    ; be more careful here - don't want to delete
                    ; a code line that is extra long
                    (while (and (char-equal (char-after (- (point) 2)) ?\ )
                                (> cur-offset pos))
                      (setq cur-offset (1- cur-offset))  ; update count
                      (delete-backward-char 1))          ; knock out a space
                    ))

             ; Finally, make sure to have at least one space between
             ; assembly code and the comment start, for readability
             (if (> (preceding-char) ?\ )
                 (insert-char ?\  1)
               nil)
           )))))



;;; Function asm86-colon [Interactive]
;;
;; Special function to be executed when a colon is inserted into
;; the buffer. The effect is to add the colon AND re-indent the line,
;; since the default indentation is for instructions, not a label.
;; (This binding can be turned off, if desired). 
;;
;; Args: none
;;
;; Return: none
;; 
;; Revision History:
;;  06/25/01 - Initial version.
;;
(defun asm86-colon ()
  "Insert colon and re-tab the line"
  (interactive)                    ; must be interactive to bind to a key
  (insert ?\:)                     ; insert the colon first
  (if asm86-electric-colon-on
      (asm86-tab)                  ; re-indent the line (in case it's a label)
    ())
)

;;; Function asm86-semicolon [Interactive]
;;
;; Special function to be executed when a semicolon is inserted into
;; the buffer. The effect is to add the semicolon AND re-indent the line
;; and position the comment. (This binding can be turned off, if desired). 
;;
;; Args: none
;;
;; Return: none
;; 
;; Revision History:
;;  06/28/01 - Initial version.
;;
(defun asm86-semicolon ()
  "Insert semicolon and re-tab the line"
  (interactive)			   ; must be interactive to bind to a key
  (insert ?\;)			   ; insert our semicolon
  (if asm86-electric-semicolon-on
    (asm86-tab)			   ; align the comment (& indent)
    ()))



;;; Function asm86-beginning-of-func [Interactive]
;;
;; Moves the point to the beginning of the current procedure, or
;; the previous procedure if it is not inside one. If there is
;; no procedure before the point, the point is left unmoved.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/06/01 - Initial version.
;;
(defun asm86-beginning-of-func ()
  "Move point to the beginning of the current procedure"
  (interactive)
  (let ((beg-pos (asm86-proc-begin-pos)))
    (cond (beg-pos
	   (goto-char beg-pos)
	   (forward-line -1))		; one more line
	  (t 
	   (beep)
	   (message "No function found!"))
)))


;;; Function asm86-end-of-func [Interactive]
;;
;; Moves the point to the end of the current procedure, or
;; the next procedure if it is not inside one. If there is
;; no procedure end after the point, the point is left unmoved.
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/06/01 - Initial version.
;;
(defun asm86-end-of-func ()
  "Move point to the end of the current procedure"
  (interactive)
  (let ((end-pos (asm86-proc-end-pos)))
    (cond (end-pos
	   (goto-char end-pos)
	   (forward-line 1))		; one more line
	  (t 
	   (beep)
	   (message "No end function found!"))
)))



;;; Function asm86-get-line-type [Interactive]
;;
;; Returns a symbol representing the type of line that
;; the point is currently on. This indicates whether
;; it is an instruction, comment, extrn statement, etc.,
;; which can then be used to control indentation, etc.
;;
;; Args: none
;;
;; Return: (symbol) indicates the line type 
;;
;; Revision History:
;;  06/23/01 - Initial version. Not all symbols supported.
;;  06/28/01 - Removed calls to (asm86-line-starts-with) and
;;             (asm86-line-contains) for speedup. Very 
;;             helpful. 
;;  07/01/01 - Removed return of asm86-comment to better 
;;             incorporate built-in comment alignment
;;  04/03/02 - Enclosed variable line-type in a (let ) context,
;;             so it is no longer a "free" variable. 
;;
(defun asm86-get-line-type () 
  "Analyze current line and return type"
  (interactive)
  (let (line-type)
    (save-excursion                 ; don't change the point
      ;; Basically, this is a big switch statement. Most types
      ;; are easy to spot based on the presence of a keyword, esp.
      ;; as the first word on the line. The order of evaluation 
      ;; matters. 

      ;; For efficiency regions (especially when doing an "indent-region"
      ;; type command), identify the meaningful portion of the line
      ;; only once, and do all of the examining "by hand". 
      (first-nonblank)            ; start of region of interest
      (let ((line-start (point)) (word-end 0) (line-end (asm86-last-char)))
        ;; find end of the first word:
        (while (and (not (eobp))     ; scan forward
                    ;; whitespace <=> ASCII value less than or equal to a SPACE:
                    (not (<= (char-after (point)) ?\ ))
                    (not (char-equal (char-after (point)) ?\;)))
          (forward-char))
        (setq word-end (point))
      
        (goto-char line-start)
        ;; meaningful line is between line-start and line-end
  
        (setq line-type
              (cond 
               ;; Lines with only an inline comment (start with single semicolon)
               ;; are considered blank lines. 
               ((line-blank t) 'asm86-blank)
               
               ;; First type of lines are distinguished by starting with a 
               ;; particular keyword. The keywords are case insensitive. 
               ((word-search-forward "NAME" word-end t) 'asm86-mod)
               ((word-search-forward "DB" word-end t) 'asm86-tab-entry)
               ((word-search-forward "DW" word-end t) 'asm86-tab-entry)
               ((word-search-forward "EXTRN" word-end t) 'asm86-extrn)
               ((word-search-forward "PUBLIC" word-end t) 'asm86-scope-ident)
               ((word-search-forward "$INCLUDE" word-end t) 'asm86-include)
               ((word-search-forward "ASSUME" word-end t) 'asm86-assume)
             
               ;; Second set of line types are identified by presence anywhere
               ;; in the line of a specific keyword. (The comparisons are
               ;; case-insensitive).
               ((word-search-forward "EQU" line-end t) 'asm86-equ)
               ((word-search-forward "GROUP" line-end t) 'asm86-group) 
               ((word-search-forward "SEGMENT" line-end t) 'asm86-segment)
               ((word-search-forward "PROC" line-end t) 'asm86-proc-start)
               ((word-search-forward "ENDP" line-end t) 'asm86-proc-end)
               ((word-search-forward "ENDS" line-end t) 'asm86-end)     ; end of a segment
               ((word-search-forward "END" line-end t)  'asm86-end)     ; end of the module
               ((word-search-forward "LABEL" line-end t) 'asm86-table)  ; could also be a FAR label
               ;; we checked DB and DW above, but at the beginning of a line. 
               ;; Inside a line somewhere implies a variable declaration. 
               ((word-search-forward "DB" line-end t) 'asm86-variable)
               ((word-search-forward "DW" line-end t) 'asm86-variable)
               
               ;; Remaining types require more specific analysis. They use
               ;; specialized helper functions. 
               ((asm86-is-label) 'asm86-label)
               ((asm86-is-header-comment) 'asm86-header-comment)
               ((asm86-is-code-comment) 'asm86-code-comment)
               ;; check for regular (inline)comment AFTER checking other two:
               ((and (asm86-line-has-comment) (not (asm86-line-has-code)))
                'asm86-inline-comment)
               (t 'asm86-inst)     ; when all else fails, must be a plain instruction
               ))
      
        ;; Final results depends on whether invoked interactively:
        (if (interactive-p)
            (message (prin1-to-string line-type))       ; print it in minibuffer
          line-type)                                    ; return it
        ))))


;;; Function asm86-insert-instr-arg-gap (num-spaces)
;;
;; Adjusts spaces between an instruction and its arguments to be
;; exactly num-spaces. If the line does not contain an instruction, no
;; action is taken, and nil is returned. Else t is returned. 
;;
;; Args: num-spaces (integer) - number of spaces to insert. This is the
;;       desired number of spaces between the instruction *start* and the
;;       argument start. So this should be at least 5.
;;
;; Return: t if spaces were inserted, nil if no action taken
;;
;; Revision History: 
;;  07/08/01 - Initial version.
;;  12/14/02 - Fixed bug when checking for comment (with (char-after ))
;;    at the very end of the buffer. 
;;
(defun asm86-insert-instr-arg-gap (num-spaces)
  "Adjust spaces between assembly instruction and its arguments"
  (interactive)
  (save-excursion			    ; keep point where it was
    (let ((inst-start 0))
      (cond ((eq (asm86-get-line-type) 'asm86-inst)  ; make sure instruction line
	     (first-nonblank)		    ; start of instruction 
	     (setq inst-start (point))

	     ;; go to *second* non-blank (start of argument or comment, or newline):
             ;; skip instruction
	     (while (and (< (point) (point-max))    ; watch for end of buffer
			 (> (char-after (point)) ?\ )) ; until whitespace
	       (forward-char))
	     
             ;; now skip until not whitespace (or end of line/buffer)
	     (while (and (< (point) (point-max))    ; watch for end of buffer
			 (not (char-equal (char-after (point)) ?\n))
			 (<= (char-after (point)) ?\ )) ; until not whitespace
	       (forward-char))
	     
	     ;; If the nonblank character is a semicolon, there is a comment but
	     ;; no argument for the instruction. In this case, do nothing. Otherwise,
	     ;; there is an argument, newline, or end of buffer, so we do want to
             ;; insert the spaces. 
	     (cond ((and (not (eobp)) (char-equal (char-after (point)) ?\;)) nil)
		   (t
		    (while (<= (preceding-char) ?\ )   ; erase all previous whitespace
		      (delete-char -1))
		    
		    ;; Have to watch for when the "instruction" is longer than the
		    ;; total alloted space. If < 1, insert a single space. 
		    (insert-before-markers (make-string
					    (max (- (+ inst-start num-spaces) (point)) 1) ?\ ))
		    t)))		    ; return success
	    
	    ;; if wasn't an instruction line
	    (t nil)))))



;;; Function asm86-in-proc [Interactive]
;;
;; Returns true if the current line is within the definition
;; of an assembly procedure. The test for this is simple: scan
;; upwards in the code, and if a PROC statement if found before
;; an ENDP statement, then we must be inside a procedure.
;;
;; Note: The function is less flexible because it checks directly
;; for a PROC or ENDP instead of calling (asm86-get-line-type).
;; This is for efficiency. 
;;
;; Note: This function is still really slow on bigger functions, 
;; since there is so much checking for procedures. Should try to
;; optimize.
;; 
;; Args: none
;;
;; Return: t if inside a proc
;;         nil else
;;
;; Revision History:
;;  06/24/01 - Initial version
;;  04/03/02 - Enclosed variable ans in a (let ) context,
;;    so it is no longer a "free" variable. 
;;
(defun asm86-in-proc ()
  "Return t if point is inside a function"
  (interactive)
  (let (ans)
    (save-excursion           ; don't mess up the point
      (beginning-of-line)
      (while (and ;nil         ; speed test - REMOVE!
              (not (bobp))
              (not (asm86-line-contains "PROC"))
              (not (asm86-line-contains "ENDP")))
        (forward-line -1))
      
      (setq ans (asm86-line-contains "PROC"))
      (if (interactive-p)
          (if ans
              (message "True")
            (message "False"))
        ans)
      )))


;;; Function asm86-proc-begin-pos
;;
;; Returns the position of the beginning of the currrent function.
;; If not currently in a function, then looks for the beginning of
;; the previous function (before the point), and failing that, it
;; returns nil (i.e. no function found).
;;
;; Args: none
;;
;; Return: (int) - position of last function, or nil if no function
;;         found. 
;;
;; Revision History:
;;  07/06/01 - Initial version.
;;
(defun asm86-proc-begin-pos ()
  "Return buffer position of current function's beginning"
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (not (asm86-line-contains "PROC")))
      (forward-line -1))		; scan lines before the point
    
    (if (asm86-line-contains "PROC") 
	(point)				; found it
      nil)				; no proc
))


;;; Function asm86-proc-end-pos
;;
;; Returns the position of the end of the currrent function.
;; If not currently in a function, then looks for the end of
;; the next function (after the point), and failing that, it
;; returns nil (i.e. no function found).
;;
;; Args: none
;;
;; Return: (int) - position of end of next function, or nil if no function
;;         found. 
;;
;; Revision History:
;;  07/06/01 - Initial version.
;;
(defun asm86-proc-end-pos ()
  "Return buffer position of current function's end"
  (save-excursion
    (beginning-of-line)
    (while (and (not (eobp))
                (not (asm86-line-contains "ENDP")))
      (forward-line 1))			; scan lines after the point
    
    (if (asm86-line-contains "ENDP") 
	(point)				; found it
      nil)				; no proc
))



;;; Function asm86-insert-func-header [Interactive]
;;
;; Inserts a function header starting at the current line,
;; and places the cursor at the end of the first line
;; of this header, which should be the position to type
;; in the function's name. The exact form of the header 
;; can be customized.
;;
;; Args: none
;; 
;; Return: none
;;
;; Revision History:
;;  02/10/04 - Changed nomenclature to "extended" style from "glen" style and
;;             updated the extended style headers.
;;  06/25/01 - Initial version. Does not yet have header 
;;             customization.
;;  06/26/01 - Added header customization: "glen" style, and "normal"
;;             style. 
;;  01/26/02 - Changed "normal" headers to use customizable string,
;;             instead of a series of text insertions. 
;;
(defun asm86-insert-func-header ()
  "Insert a function header template at the point"
  (interactive)
  (let ((asm86-old-point (point)))

    (cond
     ((not asm86-extended-style-headers)
      ;; function header string can be customized
      (insert-before-markers asm86-function-header-string))
     
     (asm86-extended-style-headers
      ;; ahh, the glen-style header.. 
      (insert-before-markers "\;\;\;\;\;\;\;\;\; Function ")
      (insert-before-markers "\n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Description:       \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Operation:         \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Arguments:         \n")  
      (insert-before-markers "\;\;\; Return Value:      \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Local Variables:   \n")  
      (insert-before-markers "\;\;\; Shared Variables:  \n")  
      (insert-before-markers "\;\;\; Global Variables:  \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Input:             \n")
      (insert-before-markers "\;\;\; Output:            \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Error Handling:    \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Algorithms:        \n")
      (insert-before-markers "\;\;\; Data Structures:   \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Registers Changed: \n")
      (insert-before-markers "\;\;\; Stack Depth:       \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Author:            ")
      (insert-before-markers asm86-author)
      (insert-before-markers "\n")
      (insert-before-markers "\;\;\; Created:           ")
      (insert-before-markers (concat (substring (current-time-string) 4 10) ", " (substring (current-time-string) 20 24) "\n" ) )
      (insert-before-markers "\;\;\; Last Modified:     ")
      (insert-before-markers (concat (substring (current-time-string) 4 10) ", " (substring (current-time-string) 20 24) "\n" ) )
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\;\;\;\;\;\;\; \n\n")
      )    
     )
    (goto-char asm86-old-point)        ; put point in a convenient place
    (end-of-line)))                    ; assumes end of first line is best..


;;; Function asm86-insert-file-header [Interactive]
;;
;; Inserts a file header starting at the third line
;; of the file, and places the cursor at the position
;; to begin typing in the relevant information. 
;;
;; It would also be cool to automatically insert the
;; current date. However, since this may be used on
;; a windows system (WinEmacs), the date utility may
;; not be available. 
;;
;; Args: none
;; 
;; Return: none
;;
;; Revision History:
;;  12/22/01 - Initial version.
;;  01/26/02 - Updated to use customizable string variable, instead
;;             of a series of fixed text insertions. 
;;  02/01/07 - Added extended style which automatically inserts name and date
;;             in the revision history
;;
(defun asm86-insert-file-header ()
  "Insert a file header template at beginning of file"
  (interactive)
  ; goto the beginning of the buffer:
  (goto-char (point-min))
  (forward-line 1)

  (let ((asm86-old-point (point)))   ; remember point to position cursor
    ;check which kind of header to use
    (cond
     ((not asm86-extended-style-headers)
      ;; function header string can be customized
      (insert-before-markers asm86-file-header-string))
     
     (asm86-extended-style-headers
      (insert-before-markers "\;\;\;\;\;\;\;\;\; File: \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Description: \n")
      (insert-before-markers "\;\;\;    \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Global Function Summary: \n")
      (insert-before-markers "\;\;\;    \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Local Function Summary: \n")
      (insert-before-markers "\;\;\;    \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Author: ")
      (insert-before-markers asm86-author)
      (insert-before-markers "\n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Revision History: \n")
      (insert-before-markers "\;\;\;    ")
      (insert-before-markers (format-time-string "%D") )
      (insert-before-markers " - Initial version. \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\; Current Status: \n")
      (insert-before-markers "\;\;\; \n")
      (insert-before-markers "\;\;\;\;\;\;\;\;\; \n\n")
      )    
     )
    (goto-char asm86-old-point)        ; put point in a convenient place
    (end-of-line)))                    ; assumes end of first line is best..


;;; Function asm86-comment-region [Interactive]
;;
;; Comments each of the lines in the region by inserting
;; a triple semicolon at the beginning of the line. This
;; makes it easier to un-comment the region. Plus, it 
;; prevents interference with lines that are already commented,
;; since the uncomment function will simply strip off the
;; three leading semicolons, and it preserves the existing 
;; indentation. 
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/01/01 - Initial version.
;;  01/26/02 - Changed comment type from two to three semicolons. 
;;
(defun asm86-comment-region ()
  "Adds left-margin comments to the region"
  (interactive)
  (let ((beg-mark (make-marker)) (end-mark (make-marker)))
    ;; need to track the beginning and end of region (which changes as
    ;; lines are indented) so the correct region is operated on. 
    (set-marker beg-mark (region-beginning))
    (set-marker end-mark (region-end))

    (goto-char (marker-position beg-mark))      ; start on first line of region
    (while (< (point) (marker-position end-mark))
      ; do the actual work here:
      (beginning-of-line)
      (insert-char ?\; 3)
      (forward-line))                   ; and try next line

    ;; Put the point back to where it was:
    (if (eq (marker-position end-mark) (mark)) 
        (goto-char (marker-position beg-mark))  ; was at the beginning of the region
      (goto-char (marker-position end-mark)))   ; was at the end of the region
    ))



;;; Function asm86-uncomment-region [Interactive]
;;
;; Removes header comment at the beginning of each line
;; in the region. The functionality is simple and primitive,
;; but makes for easy commenting and uncommenting of regions.
;; Lines which do NOT begin with a triple semi-colon are left
;; alone.  
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/01/01 - Initial version.
;;  01/26/02 - Changed comment type from two to three semicolons. 
;;
(defun asm86-uncomment-region ()
  "Removes left-margin comments from the region"
  (interactive)
  (let ((beg-mark (make-marker)) (end-mark (make-marker)))
    ;; need to track the beginning and end of region (which changes as
    ;; lines are indented) so the correct region is operated on. 
    (set-marker beg-mark (region-beginning))
    (set-marker end-mark (region-end))

    (goto-char (marker-position beg-mark))      ; start on first line of region
    (while (< (point) (marker-position end-mark))
      ; do the actual work here:
      (beginning-of-line)
      (if (and (char-equal (char-after (point)) ?\;)     ; make sure commented out
               (char-equal (char-after (1+ (point))) ?\;)
               (char-equal (char-after (+ (point) 2)) ?\;))
          (delete-char 3))
      (forward-line))                   ; and try next line

    ;; Put the point back to where it was:
    (if (eq (marker-position end-mark) (mark)) 
        (goto-char (marker-position beg-mark))  ; was at the beginning of the region
      (goto-char (marker-position end-mark)))   ; was at the end of the region
    ))



;;; Function asm86-close-function [Interactive]
;;
;; Inserts a closing function (ENDP) line into the code.
;; If not currently in a funciton, an error message is
;; printed in the minibuffer. The point is put at the
;; end of the newly-inserted line. 
;;
;; Args: none
;;
;; Return: none
;;
;; Revision History:
;;  07/01/01 - Initial version.
;;
(defun asm86-close-function ()
  "Inserts a closing procedure statement (ENDP) for the current function"
  (interactive)
  (let ((proc-name (asm86-get-proc-name)))
    (cond (proc-name           ; we are inside a procedure
           ;; make sure we're at the start of a line:
           (end-of-line)
           (newline)
           (insert proc-name " ENDP\n")
           )
          (t                        ; not in a procedure => error
           (beep)
           (message "Error: Not in a function.")))))



;;; Function asm86-get-proc-name
;;
;; Returns the name of the current assmebly procedure, as 
;; a string, or nil if not currently in a procedure. 
;;
;; Args: none
;;
;; Return: nil if point is not inside a procedure, else
;;         the name of the procedure it lies in.
;;
;; Revision History:
;;  07/01/01 - Initial version.
;;
(defun asm86-get-proc-name ()
  "Returns the name of procedure in which the point lies"
  (save-excursion
    (cond ((asm86-in-proc)     ; inside a procedure
           (beginning-of-line)
           ;; we know there is a "PROC" line above, so find it:
           (while (not (asm86-line-contains "PROC"))
             (forward-line -1))
           
           ;; the function name is the first word of the line
           (let ((beg-char (point)))
             (while (> (char-after (point)) ?\ )   ; find first whitespace char
               (forward-char))
             (buffer-substring beg-char (point)))  ; extract the function name
           )
          
          (t nil))))             ; not inside a procedure



;;;;;;;;;;;;;;;;;;;; LINE-TYPE HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;


;;; Function asm86-is-label
;;
;; Returns true if the said line is an assembly label. A label
;; is defined as line that starts with a single keyword, terminated
;; by a colon. This definition is admittedly a bit weak. 
;;
;; Args: none
;;
;; Return: t if it is a label
;;         nil else
;;
;; Revision History:
;;  06/23/01 - Initial version
;;  12/27/01 - Added check for beginning of buffer when 
;;    returning answer, since (char-before x) gives an
;;    error in that case. 
;;
(defun asm86-is-label ()
  "Return t if the current line is an assembly label"
  (save-excursion          ; don't mess up the point
    (first-nonblank)           ; goto first non-blank char
    
    ; Move along until we get to a comment or whitespace again
    (while (and (not (eobp))     ; scan forward
	        ; whitespace <=> ASCII value less than or equal to a SPACE:
		(not (<= (char-after (point)) ?\ ))
		(not (char-equal (char-after (point)) ?\;)))
      (forward-char))
    
    ; if a label, the char we just stepped past will be a colon
    (cond ((bobp) nil)         ; special case to watch for
          (t (char-equal (char-before (point)) ?\: )))  ; this is the wanted return value
))



;;; Function asm86-is-code-comment
;;
;; Returns true if the current line is a code comment, which is 
;; defined as a line which is only a comment, and the comment starts
;; with a double (exactly two) semi-colon. 
;;
;; Args: none
;;
;; Return: t if a header line
;;         nil else
;;
;; Revision History:
;;  01/26/02 - Initial version. 
;;
(defun asm86-is-code-comment () 
  "Return t if the current line is a header comment"
  (save-excursion              ; don't mess up the point
    (first-nonblank)           ; goto first non-blank char
    (and (<= (+ 2 (point)) (point-max))   ; watch for end of buffer!
	 (char-equal (char-after (point)) ?\; )
         (char-equal (char-after (+ (point) 1)) ?\; ))))


;;; Function asm86-is-header-comment
;;
;; Returns true if the current line is a header comment, which is 
;; defined as a line which is only a comment, and the comment starts
;; with a triple semi-colon. 
;;
;; Args: none
;;
;; Return: t if a header line
;;         nil else
;;
;; Revision History:
;;  06/23/01 - Initial version. 
;;  01/26/02 - Changed definition of header comment to starting
;;    with three (not two) semicolons. Also changed function
;;    name to be more descriptive. 
;;
(defun asm86-is-header-comment () 
  "Return t if the current line is a header comment"
  (save-excursion              ; don't mess up the point
    (first-nonblank)           ; goto first non-blank char
    (and (<= (+ 3 (point)) (point-max))   ; watch for end of buffer!
	 (char-equal (char-after (point)) ?\; )
	 (char-equal (char-after (+ (point) 1)) ?\; )
         (char-equal (char-after (+ (point) 2)) ?\; ))))


;;; Function asm86-line-has-code
;;
;; Returns true if the current line has "code" on it, which
;; is any nonblank character not within a comment.
;;
;; Args: none
;;
;; Return: t if there is code
;;         nil else
;;
;; Revision History: 
;;  06/24/01 - Initial version.
;;  12/22/01 - Updated stopping condition to use 
;;    (asm86-point-at-comment-start). 
;;  04/03/02 - Enclosed variable ans in a (let ) context,
;;    so it is no longer a "free" variable. 
;;
(defun asm86-line-has-code ()
  "Return t if the current line has code on it"
  (interactive)
  (save-excursion              ; don't mess up the point
    (let (ans)
      (beginning-of-line)
      (while (and (< (point) (point-max))    ; watch for end of buffer
                  (not (char-equal (char-after (point)) ?\n)) ; watch newline
                  (not (char-equal (char-after (point)) ?\;)) ; watch comments
                  (not (> (char-after (point)) ?\ )))
        (forward-char))
      
      ;; return t if we stopped on a nonblank character that does
      ;; not start a comment
      (setq ans (and (< (point) (point-max))
                     (> (char-after (point)) ?\ )
                     (not (asm86-point-at-comment-start)))))))


;;; Function asm86-line-has-comment
;;
;; Returns true if the current line has a comment on it. In 
;; most cases, this returns true if there is a semicolon on
;; the line. However, a semicolon immediatley followed by
;; a 'j' (upper- or lower-case) is NOT considered a comment
;; if the variable asm86-ignore-commented-jumps is nil. 
;;
;; Args: none
;;
;; Return: t if there is a comment
;;         nil else
;;
;; Revision History: 
;;  06/24/01 - Initial version.
;;  12/22/01 - Changed meaning of a comment slightly, to
;;    handle commented-out jump statements more gracefully. All
;;    of the work shifted to the new function,
;;    (asm86-goto-comment-start).
;;
(defun asm86-line-has-comment ()
  "Return t if current line has a comment on it"
  (save-excursion              ; don't mess up the point
    ; this function will return t if it finds a comment
    ; on the line, so let it do the work: 
    (asm86-goto-comment-start)))
  



;;;;;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Function asm86-point-at-comment-start [Interactive]
;;
;; Returns t if the point is currently at the start
;; of a comment. The definition of what a comment is
;; depends on the value of the variable 
;; asm86-ignore-commented-jumps. 
;;
;; Args: none
;;
;; Return: t if point at a comment, else nil
;;
;; Limitations: Function does not gaurantee that the
;;  point is at the *first* comment marker of the
;;  line. So a line that has two comment starts
;;  will return t when the point is at either of them.
;;
;; Revision History:
;;  12/22/01 - Initial version.
;;  12/27/01 - Added explicit check for end of buffer, which
;;    causes an error for (char-after x). 
;;  01/26/02 - Fixed another bug of the same type. 
;;
(defun asm86-point-at-comment-start ()
  "Returns t if point is at a comment start"
  (interactive)

  (cond ((eobp) nil)                    ; special case
        (asm86-ignore-commented-jumps
         ;; comment start is simply a semicolon
         (char-equal (char-after (point)) ?\;))
    
        (t
         ;; else comment start is a semicolon that is
         ;; NOT immediatley followed by a 'j'
         (and (char-equal (char-after (point)) ?\;)
              (or (> (+ (point) 2) (point-max)) ; semicolon is last char in buf
                  (not (or 
                        (char-equal (char-after (+ (point) 1)) ?J)
                        (char-equal (char-after (+ (point) 1)) ?j))))))))


;;; Function asm86-goto-comment-start [Interactive]
;;
;; (Destructively) moves the point to the start of 
;; the comment on the current line, and returns
;; t. If the current line does not have a comment, 
;; nil is returned, and the point is left unchanged.
;;
;; When successful, the point is positioned just before
;; the comment-initiating semicolon.
;;
;; Args: none
;;
;; Return: t if the line has a comment (and the point
;;  was updated; nil if the line has no comment
;;
;; Limitations: none
;;
;; Revision History:
;;  12/22/01 - Initial version.
;;  01/26/02 - Fixed minor bug. If line comment was the very
;;    last character in the buffer, there was a problem. 
;;
(defun asm86-goto-comment-start ()
  "Moves the point to the start of the comment on the current line"
  (interactive)
  (let ((old-point (point)))   ; remember original position, 
                               ; in case of failure
    (beginning-of-line)
    (while (and (< (point) (point-max))    ; watch for end of buffer
                (not (char-equal (char-after (point)) ?\n))
                (not (asm86-point-at-comment-start)))
      (forward-char))

    ; see if stopping point was a comment or end of
    ; line (or buffer):
    (cond ((asm86-point-at-comment-start)
           t)  ; success
          (t  ; no comment found
           (goto-char old-point)  ; restore point
           nil)))) 


;;; Function asm86-last-char
;;
;; Returns the position of the last meaningful character on the
;; current line. Text beyond a comment (semi-colon) is ignored,
;; but if there is extra whitespace at the end, that IS
;; considered meaningful (due to laziness).
;; 
;; Args: none
;;
;; Return: (absolute position)
;;
;; Revision History:
;;  06/23/01 - Initial version completed/tested. 
;;
(defun asm86-last-char ()
  "Finds the last meainingful (non-comment) character on an assembly line"
  (save-excursion                    ; don't mess up the point
    (beginning-of-line)              ; scanning starts here
    (let ((scan-point (point)))
      
      ; Go until we find a newline or a semi-colon, or the buffer
      ; ends. The order of comparison is important because if
      ; scan-point is past (point-max), (char-after) will return
      ; nil. So a short-circuit OR is used. 
      (while (not (or (<= (point-max) scan-point)
		      (char-equal (char-after scan-point) ?\; )
		      (char-equal (char-after scan-point) ?\n )))
		      
	(setq scan-point (+ scan-point 1)))  ; advance one character
      scan-point)))       ; return the value
  


;;; Function asm86-line-contains(keyword) [Interactive]
;;
;; Returns true if the current line in the buffer contains
;; the said keyword. All preceeding whitespace is ignored. Keyword
;; searching is case insensitive. Any text within a comment 
;; (i.e. following a semicolon) is ignored. 
;;
;; Args: keyword (string) - keyword to match against
;;
;; Return: t if the word is there
;;         nil else
;;
;; Revision History:
;;  06/23/01 - Initial version completed/tested. 
;;
(defun asm86-line-contains (keyword) 
  "Searches for keyword in the current line"
  (interactive "sKeyword to Search for: \n")  ; allow to be executed using M-x
  
  (save-excursion                  ; to keep variable scope under control
    (let ((case-fold-search t)         ; ignore case in text searching
	  (line-end (asm86-last-char)))

      ; search from beginning of line to last meaningful character on the
      ; line:
      (beginning-of-line)
      (if (interactive-p)
	  ; Interactive: print result in minibuffer.
	  (if (word-search-forward keyword line-end t)
	      (message "True")
	    (message "False"))
	; Non-interactive: return a value.
	(word-search-forward keyword line-end t)))))



;;; Function asm86-line-starts-with(keyword) [Interactive]
;;
;; Returns true if the line starts with the said keyword.
;; The comparison is only made against meaningful assembly
;; characters (i.e. comments are ignored). The comparison
;; is case insensitive. Beginning whitespace is skipped.
;;
;; There is some redundancy with the function
;; (asm86-last-char). 
;;
;; Args: keyword (string)
;;
;; Return: t if first word matches
;;         nil else
;;
;; Revision History:
;;  06/23/01 - Initial version.
;; 
(defun asm86-line-starts-with (keyword)
  "Searches for keyword at start of the current line"
  (interactive "sKeyword to Search for: \n")  ; allow to be executed using M-x
  
  (save-excursion                  ; to keep variable scope under control
    (let ((case-fold-search t)     ; ignore case in text searching
	  (line-end 0))            ; initial value is invalid
      
      ; First find the end of the first word (delimited by 
      ; whitespace):
      (first-nonblank)             ; move to first non-blank char

      (while (and (not (eobp))     ; scan forward
                  ; whitespace <=> ASCII value less than or equal to a SPACE:
		  (not (<= (char-after (point)) ?\ ))
		  (not (char-equal (char-after (point)) ?\;)))
	(forward-char))

      (setq line-end (point))      ; have the limit value now
      (beginning-of-line)

      (if (interactive-p)
	  ; Interactive: print result in minibuffer.
	  (if (word-search-forward keyword line-end t)
	      (message "True")
	    (message "False"))
	; Non-interactive: return a value.
	(word-search-forward keyword line-end t)))))



;;; Function asm86-indent-to (position)
;;
;; Removes all initial whitespace on the current line, then indents
;; to the said position, which should be a positive value. Indentation
;; is accomplished using only spaces. The point remains at its current
;; position.
;;
;; Args: position (positive int) - how many spaces to insert at the
;;   beginning of the line
;; 
;; Return: void
;;
;; Error Handling: negative numbers are ignored (no action)
;;
;; Revision History:
;;  06/24/01 - Initial Version
;;
(defun asm86-indent-to (position)
  (cond ((< position 0) nil)   ; ignore negative position values
        (t 
         (save-excursion             ; don't mess up the point
           (beginning-of-line)
      
           ; delete all whitespace at the line's start:
           (while (and (not (eobp))
                       (<= (char-after (point)) ?\ )
                       (not (char-equal (char-after (point)) ?\n)))
             (delete-char 1))
      
           ; Now insert the desired number of spaces.
           ; Use (insert-before-markers ..) b/c it moves the point
           ; forward as it inserts, which is generally a more
           ; desirable side effect when editing.
           (insert-before-markers (make-string position ?\ ))
))))



;;; Function line-blank (&optional totally-blank)
;;
;; Returns true if the current line has only whitespace characters. If the
;; optional argument, totally-blank, is non-nil, then the line can only 
;; contain whitespace characters. Otherwise, lines with comments only
;; are considered blank as well. 
;;
;; Args: totally-blank (optional) - if non-nil, line must contain only
;;   whitespace characters to be blank. If nil (the default), line
;;   can contain a comment and still be considered blank. 
;;
;; Return: t line is blank
;;         nil else
;;
;; Revision History:
;;  06/24/01 - Initial version.
;;  07/05/01 - Added optional argument to allow comment lines to be 
;;             considered blank. 
;;
(defun line-blank (&optional totally-blank)
  (save-excursion                ; don't mess up point
    (first-nonblank)
    
    ; line is blank if we reach the end of the buffer, or the next
    ; char is a newline, or next character is a comment start
    (or (= (point) (point-max))
        (char-equal (char-after (point)) ?\n)
	(and (not totally-blank) (char-equal (char-after (point)) ?\;)))))



;;; Function first-nonblank
;;
;; Moves the point (destructive!) to the first non-blank character in
;; the line. If the line contains only whitespace, the point is 
;; positioned just before the newline. If the line is the last line in
;; the buffer, the point is positioned at (point-max).
;;
;; Args: none
;;
;; Return val: none
;;
;; Side Effects: moves the point
;;
;; Revision History:
;;  06/23/01 - Initial version.
;;
(defun first-nonblank ()
  (interactive)
  (beginning-of-line)
  (while (and (< (point) (point-max))    ; watch for end of buffer
              (not (char-equal (char-after (point)) ?\n))
              (<= (char-after (point)) ?\ ))
    (forward-char)))



;;; Change Log:
;;
;; 07/09/01 - Initial version. 
;; 12/22/01 - Lots of updates. Revised the functional breakdown
;;   to make comment handling more flexible (and easier to change).
;;   Implemented several of the functions on the wish-list. 
;; 12/23/01 - Updated documentation strings. 
;; 12/30/01 - Updated major mode documentation string, to be more
;;   consistent with the style of other major modes. 
;; 01/14/02 - Fixed bug in (asm86-insert-instr-arg-gap), which 
;;   occured when current line was the last line of the buffer. 
;; 01/26/02 - Added additional comment category, bringing the total
;;   to three: header comments, code comments, and inline comments. 
;;   Many small changes to accomodate this new functionality.
;; 01/26/02 - Changed function and file headers to be more easily 
;;   customized, requiring only a single string. 
;; 02/20/02 - Added version number to documentation string. Changed mode
;;   string from "ASM86" to "Asm86". 
;; 04/02/02 - Added font-locking (syntax highlighting). Stopped using
;;   auto-fill-mode automatically. 
;; 02/10/04 - Changed nomenclature to "extended" style from "glen" style and
;;   updated the extended style headers.
;; 02/01/07 - Made asm86-author settable from within emacs (added asterisk
;;   to start of description string).
;; 02/01/07 - Added name and date to the file header.
;;

;;; asm86-mode.el ends here 
