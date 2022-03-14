; Project One - Machine Org
; Due 02/15/2022
; flips the screen

; This program loops through every line and reverses it.
; There are 80 characters on each line, SI will start at one end of the line and DI will start at the other end of the line. It'll pick those characters up
; and then reverse them by moving them into AX and DX. A counter will keep track of which line to flip. Push stuff, then when it gets to the middle of the 
; line, pop it, then add 160 to set it to the next line when it returns to main.

;----------------------------------------------------------------------- STACK SEGMENT
MyStack SEGMENT STACK              ; Keyword "STACK" is required!

        DW 256 DUP (?)             ; At least a small stack is needed for interrupt handling in a multiprocessing OS (not really needed when using DOSBox, though).

MyStack ENDS                       ; ENDS = End Segment

;------------------------------------------------------------------------ DATA SEGMENT
MyData SEGMENT

    ScreenMemSeg EQU 0B800h        ; Segment for video RAM.

MyData ENDS                        ; End Segment

;------------------------------------------------------------------------ CODE SEGMENT
MyCode SEGMENT
        ASSUME CS:MyCode, DS:MyData    ; Tell the assembler what segments the segment registers refer to in the following code.

mainProg  PROC                     ; This label will serve as the "user transfer address"

        MOV AX, 0B800h
        MOV ES, AX

        MOV SI, 0                  ; make SI point to the beginning of the line to flip 
        MOV DI, 158                ; make DI point to the end of the line to flip
        MOV CX, 25                 ; counter for the rows


        ; call the flip a line proc 25 times
mainLoop:                   

        CALL flipALine              ; flip the line
        ADD SI, 160                 ; go to the next line
        ADD DI, 160                 ; go to the next line
        INC BYTE PTR ES:[160*30+80]

       LOOP mainLoop  

       CALL colorLine 

       ; end the program
       MOV AH, 4Ch
       INT 21h            
       
mainProg ENDP                      ; ENDP = End of the main Proc

; ============================================

colorLine PROC

        PUSH AX CX SI

        MOV CX, 2000                ; for the 2,000 characters on the screen
        MOV SI, 0                   ; start at 0

myOtherLoop:

        MOV AL, ES:[SI]             ; get the character from the screen
        CMP AL, 'A'                 ; is it less than A?
        JL skip                     ; if so, skip
        CMP AL, 'Z'                 ; is it less than Z?
        JLE colorIt                 ; if so, color it

        CMP AL, 'a'                 ; is it less than a?
        JL skip                     ; then skip
        CMP AL, 'z'                 ; is it less than z
        JLE colorIt                 ; if so, then color it

colorIt:
        MOV ES:[SI + 1], BYTE PTR 01111001b  ; color it blue on white

skip:
        INC SI
        INC SI                      ; increment SI twice to move on to the next character
        LOOP myOtherLoop

        POP SI CX AX
        RET

colorLine ENDP

; ============================================

flipALine PROC

        PUSH AX DX SI DI CX
        MOV CX, 40

myLoop:

        ; flip the characters
        MOV AX, ES:[SI]         ; move the character from [SI] into AX
        MOV DX, ES:[DI]         ; move the character from [DI] into DX
        MOV ES:[SI], DX         ; then replace [SI] with DX
        MOV ES:[DI], AX         ; then replace [DI] with AX

        ; go to the next characters
        ADD SI, 2
        SUB DI, 2

        INC BYTE PTR ES:[160*20+80]

        LOOP myLoop

        POP CX DI SI DX AX

        RET

flipALine ENDP

; ===========================================

MyCode ENDS                        ; End Segment

;-------------------------------------------------------------------------------------
END mainProg                       ; This END marks the physical end of the source code file. The label specified will be the "user transfer address" where execution will begin.