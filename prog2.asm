; Project Two - Machine Org
; Due 03/15/2022
; Displays one of ten hard-coded sentencces chosen at random. The program then allows the user to attempt to type that sentence on the line below
; the displayed sentence

;======================================================================== STACK SEGMENT
MyStack SEGMENT STACK              ; Keyword "STACK" is required!

        DW 256 DUP (?)             ; At least a small stack is needed for interrupt handling in a multiprocessing OS (not really needed when using DOSBox, though).

MyStack ENDS                       ; ENDS = End Segment

;======================================================================== DATA SEGMENT
MyData SEGMENT

        cursorCol               DB ?    ; The column the cursor is on       
        cursorOffset            DW ?    ; The screen offset to the cursor    
        typedSentenceLength     DW ?    ; The length of the sentence the user types
        sentenceLength          DW ?    ; The length of the random sentence
        cursorRow               DB ?    ; The row the cursor is on
        randomSentence          DW ?    ; Holds the randomly picked sentence
        incorrectFlag           DB ?    ; Keeps track if the sentence is incorrect
        startTime               DW 0    ; The time the program started
        maxMinutes              DW ?

        ; these were all generated with a random sentence generator
        first   DB "The blue parrot drove by the hitchhiking mongoose.", 0
        second  DB "African elephants have bigger ears than Asian elephants.", 0
        third   DB "The slow red fox ducked under the giant dog.", 0
        fourth  DB "She ran across the soccer field like a duck.", 0
        fifth   DB "I shouldn't have used the word password as my password.", 0
        sixth   DB "Sam felt sick after eating five huge candy bars.", 0
        seventh DB "Is 'Armageddon' really the best name for a goldfish?", 0
        eighth  DB "The monkey bit the apple and spat it at the giraffe.", 0
        nineth  DB "Would you like a nice warm slice of apple pie?", 0
        tenth   DB "The painting looked like something out of the 1700s.", 0

        ScreenMemSeg EQU 0B800h         ; Segment for video RAM.

MyData ENDS                             ; End Segment

;======================================================================== CODE SEGMENT
MyCode SEGMENT
        ASSUME CS:MyCode, DS:MyData     ; Tell the assembler what segments the segment registers refer to in the following code.

; --------------------------------------------------------------------------------------------------------------------------------------------------
; The main PROC of the program. It loops through keys being input and calls the PROC to process them
; on entry: stuff in data segment is defined and all that
; on exit: the program will end

mainProg  PROC                          ; This label will serve as the "user transfer address"

        MOV     AX, 0B800h              ; Make ES segment register address video memory.
        MOV     ES, AX

        MOV     AX, MyData              ; Make DS address our data segment.
        MOV     DS, AX

        MOV     maxMinutes, 1           ; They have a minute to complete the sentence
        MOV     incorrectFlag, 0        ; Initialize to correct
        MOV     sentenceLength, 0       ; Initialize the sentenceLength
        CALL    displaySentence         ; Display the sentence for the user
        CALL    startTimer              ; Start the timer

mainLoop:

        CALL    updateTimer             ; Call the timer function to update the timer
        CALL    shiftCheck              ; Check if the shifts have been pressed
        CALL    checkComplete
        MOV     AH, 11h                 ; Check buffer
        INT     16h                     ; Wait for a key and read from the buffer
        JZ      mainLoop                ; If no key, skip
        MOV     AH, 10h                 ; Read the key
        INT     16h                     
        CMP     AL, 27                  ; Is it esc?
        JE      exitMainLoop            ; If so, exit
        CALL    processKey              ; If not, process the key input
        CALL    colorSentence           ; Color the sentence
        JMP     mainLoop                ; Loop again

exitMainLoop:

        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        int     21h                     ; This ones part of that as well

       
mainProg ENDP                           ; End of the main Proc

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Check if the sentence the user entered is complete and matches the random one

checkComplete PROC

        PUSH    AX BX
        MOV     AX, sentenceLength
        MOV     BX, typedSentenceLength
        CMP     AX, BX     ; Check if the sentences are the same length
        JE      seeIfCorrect           
        JMP     endCheckComplete                 
seeIfCorrect:

        CALL    sentencesMatch
        CMP     incorrectFlag, 0                        ; If it's the same length, check if the sentence is correct
        POP     BX AX
        JE      isCorrect
        JMP     endCheckComplete

isCorrect:

        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        int     21h                     ; This ones part of that as well

endCheckComplete:

        POP     BX AX
        RET


checkComplete ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; A C style call to color the sentence when the user types a letter
colorSentence PROC

        PUSH    SP
        PUSH    typedSentenceLength     ; The number of characters the user typed 
        PUSH    WORD PTR 23 * 160       ; Where the user typed sentence 
        PUSH    randomSentence          ; Original sentence 
        CALL    sentencesMatch          ; Check if the sentences match
        MOV     DI, [BP + 6]            ; Move the position of the typed sentence into DI
        ADD     SP, 6                   ; 'Clean' the stack. Logically remove the 6 bytes
        POP     SP
        RET


colorSentence ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Checks if the sentence that the user is typing matches the random sentence chosen. It colors the character red or green and sets the flag variable accordingly
; on entry: typedSentenceLength, 23 * 160, and randomSentence are on the stack
; on exit: incorrectFlag is set

sentencesMatch PROC
    
        PUSH    BP                     ; Save BP
        MOV     BP, SP                 ; Make BP point to the call frame on the stack
        PUSH    BX CX SI DI AX

        ; call frame on stack looks like
        ; typedSentenceLength   [BP + 8]
        ; 23 * 160              [BP + 6]
        ; random sentence       [BP + 4]
        ; return address        [BP + 2]
        ; saved BP              [BP]

        MOV     SI, [BP + 4]            ; Move the random sentence into SI
        MOV     DI, [BP + 6]            ; Move the position of the typed sentence into DI
        MOV     BX, [BP + 8]            ; Move the length of the sentence the user typed into BX
        MOV     CX, 0                   ; Initialize the counter to 0

checkLoop:
        CMP     CX, DS:[BX]             ; Is CX equal to the sentence length?
        JE      endCheckLoop            ; If so, exit
        MOV     AX, ES:[DI]             ; Put the typed character into AX
        CMP     DS:[SI], AL             ; Do the characters match?
        JNE     colorRed                ; If not, color it red
        MOV     AH, 00000010b           ; If so, color it green
        MOV     ES:[DI], AX             ; Put the character on screen with the color
        MOV     incorrectFlag, 0        ; If so, set the flag to correct
        ADD     DI, 2                   ; Advance to the next screen position
        INC     SI                      ; Advance to the next character in the random sentence
        INC     CX                      ; Increment the counter
        JMP     checkLoop               ; Loop back around

colorRed:
        MOV     AH, 00000100b
        MOV     ES:[DI], AX
        MOV     incorrectFlag, 5

endCheckLoop:
        POP     AX DI SI CX BX BP
        RET

sentencesMatch ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Processes the keys input by the user
; on entry: A key has been read into AL and needs to be processed
; on exit: It will have called the appropriate PROC to handle that key and the program will wait for the next one to be entered. The cursor will be updated to the 
;          appropriate position.

processKey PROC

        PUSH    AX

        CMP     AL, 8                   ; Is it a backspace?
        JE      backspace               ; Then handle it and jump to the end of the PROC  

        CMP     AH, 4dh                 ; Is it a right arrow key?
        JE      rightArrow              ; Handle it  

        CMP     AH, 4bh                 ; Is it a left arrow key?
        JE      leftArrow               ; Handle it 

        CMP     AL, 00h                 ; Is it the F1 key?
        JE      f1                      ; Handle it

        CMP     AH, 53h                 ; Is it the delete key?
        JE      delete                  ; Handle it

        ; if it's not any of these keys, then it's a character or a key that doesn't do anything
        CALL    handleChar
        JMP     endProcessKey   

; call the corresponding methods to actually handle them
backspace:
        CALL    handleBackspace         
        JMP     endProcessKey          

rightArrow:
        CALL    handleRightArrow
        JMP     endProcessKey

leftArrow:
        CALL    handleLeftArrow
        JMP     endProcessKey


f1:
        CALL    handleF1
        JMP     endProcessKey

delete:
        CALL    handleDelete
        JMP     endProcessKey

endProcessKey:
        INC     cursorCol               ; increment the cursor column each time a button is pressed
        CALL    updateCursorPosition    ; update the cursor position each time a button is pressed
        POP     AX
        RET

processKey ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Divides the number of timer ticks by ten and uses the remainder to select the sentence by looping and counting the 0's. 

getRandomSentence PROC

        PUSH    SI DX BX AX
        
        MOV     AH, 00h                 ; Get number of timer ticks. CX:DX = number of clock ticks since midnight.
        INT     1Ah

        MOV     AX, DX                  ; Put the ticks into AX
        MOV     AH, 0                   ; Clear the high byte 
        MOV     BL, 10                  ; Move ten into BL so we can divide and use remainder to select the sentence
        DIV     BL                      ; Divide
        
        LEA     SI, first               ; Load the address of the first sentence into SI

        CMP     AH, 0                   ; If the random number was 0, then just go to the end since we don't need to count the 0's
        JE      endGetSentence

getSentence:
        ; Find the 0's at the end of the sentence
        INC     SI                      ; Increment the position of the character
        CMP     DS:[SI], BYTE PTR 0     ; Is it a 0?
        JNE     getSentence             ; If not, then go to the next character

        ; If a 0 is found, then increment the counter and check if it's equal to the random number
        INC     BH                      ; If so, then increment the sentence
        CMP     BH, AH                  ; If BH is equal to AH
        JE      endGetSentence          ; The sentence has been found
        JMP     getSentence             ; If not, loop again

endGetSentence:
        INC     SI
        MOV     randomSentence, SI
        POP     AX BX DX SI
        RET

getRandomSentence ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays one of the ten sentences chosen randomly
; on entry: 
; on exit: A random sentence will have been chosen by calling a PROC to get one and it'll be displayed on the screen

displaySentence PROC

        PUSH    AX SI DI
        CALL    clearScreen             ; Clear the screen
        CALL    getRandomSentence; 
        MOV     SI, randomSentence      ; we are looking at the first sentence
        MOV     DI, 21 * 160            ; Move the desired screen position for the random sentence into DI

        MOV     cursorCol, 0            ; initialize to 0, which is the beginning of the screen
        MOV     cursorOffset, 0         ; initialize to 0, which is the beginning of the screen
        MOV     cursorRow, 23           ; initialize to 23 for the row the user will write on

displayLoop:

        MOV     AL, [SI]                ; Get the current letter in the sentence
        CMP     AL, 0                   ; Is it the terminator?
        JE      endOfDisplayLoop        ; If so, quit
        MOV     ES:[DI], AL             ; Else, display it
        INC     sentenceLength
        INC     SI                      ; Advance to the next character
        ADD     DI, 2                   ; Advance to the next screen position
        JMP     displayLoop             ; Loop back around

endOfDisplayLoop:

        POP     DI SI AX
        RET

displaySentence ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; clears the screen
clearScreen PROC

        PUSH    CX DI
        MOV     CX, 0                   ; set the accumulator to 0
        MOV     DI, 0                   ; set the screen position to 0

clearLoop:

        CMP     CX, 2000                ; See if we've cleared 2,000 characters
        JE      endClearLoop            ; If we have, end it
        MOV     BYTE PTR ES:[DI], " "   ; If not, put a space 
        ADD     DI, 2                   ; Next screen position
        INC     CX                      ; Increment the accumulator
        JMP     clearLoop               ; Loop back

endClearLoop:

        POP     DI CX
        RET              

clearScreen ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Deletes the character above the cursor, pulling the tail end of the line (if any) to the left

handleDelete PROC

        SUB     typedSentenceLength, 1
        CALL    updateCursorPosition            ; Update the position of the cursor
        CALL    shiftSentenceTail               ; Shift the sentence

handleDelete ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; If the cursor is in the leftmost position, do nothing. Else, delete the character to the left of the cursor, pulling tail of the line to the left
; on entry: the cursorOffset and column will need adjusted
; on exit: the cursorOffset and column will be adjusted and the character deleted

handleBackspace PROC

        CMP     cursorCol, 0                    ; At beginning?
        JE      endHandleBackspace              ; If so, do nothing
        SUB     typedSentenceLength, 1
        SUB     cursorCol, 2                    ; If not, change cursor position to position before (two cause it increments when a key is pressed)
        SUB     cursorOffset, 2                 ; And also change the offset to the position before
        CALL    updateCursorPosition            ; Update the position of the cursor
        CALL    shiftSentenceTail               ; Shift the sentence

endHandleBackspace:

        RET

handleBackspace ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Handles the shifting of the sentence to the left when a character is deleted.
; on entry: Assume cursorPos points to character to remove. 

shiftSentenceTail PROC

        PUSH    AX SI DI
        MOV     SI, 23 * 160                    ; Put the line where the user is typing into SI
        ADD     SI, cursorOffset                ; Then add the cursorOffset so the cursor is in the correct column
        MOV     DI, 23 * 160 + 158              ; Set DI to the end of the sentence

shiftLoop:

        CMP     SI, DI                          ; Do SI and DI match? 
        JE      endShiftLoop                    ; Then end the loop

        MOV     AX, ES:[SI + 2]                 ; Move the character to the right of the cursor into AX
        MOV     ES:[SI], AX                     ; Then put that character in place of the one where the cursor is
        ADD     SI, 2                           ; Move to the next character
        JMP     shiftLoop                       ; Loop again

endShiftLoop:

        CALL    updateCursorPosition

        POP     DI SI AX
        RET


shiftSentenceTail ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Handles when the user presses the left arrow key on the keyboard.
; on entry: the cursorOffset is set to the correct value of where on the screen the user's cursor is
; on exit: the cursor will be to the left of where it was on entry and the values will be updated

handleLeftArrow PROC

        CMP     cursorCol, 0                    ; At beginning?
        JE      endHandleLeftArrow              ; If so, do nothing
        SUB     cursorCol, 2                    ; If not, change cursor position to position before (One extra cause it increments cursorCol when the button is pressed)
        SUB     cursorOffset, 2                 ; And also change the offset to the position before

endHandleLeftArrow:
        RET

handleLeftArrow ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; handles when the user presses the right arrow key on the keyboard.
; on entry: the cursorOffset is set to the correct value of where on the screen the user's cursor is
; on exit: the cursor will be to the right of where it was on entry and the values will be updated

handleRightArrow PROC

        CMP     cursorOffset, 160                   ; At end?
        JE      endHandleRightArrow                 ; If so, do nothing
        ADD     cursorOffset, 2                     ; If not, change the cursor position to the position after it
        JMP     endHandleRightArrow
        ; updating the cursorCol is already handled in PROC that calls this

endHandleRightArrow:
        RET

handleRightArrow ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Called when the cursor needs moved, changes the position of the cursor based on cursorCol
; on entry: The cursorCol will point to where the cursor needs to be set.
; on exit: The cursor will be set to the cursorCol value and the row the user is set to type on. DH will have cursorRow, DL will have cursorCol

updateCursorPosition PROC

        PUSH    AX DX

        MOV     DH, cursorRow                   ; Put the row that the cursor is on into DX
        MOV     DL, cursorCol                   ; Put the column that the cursor is in into DX
        MOV     AH, 02h                         ; Interupt used to set the cursor position. DH = row. DL = column.
        INT     10h
        JMP     endUpdateCursorPosition

endUpdateCursorPosition:

        POP     DX AX
        RET

updateCursorPosition ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays the character keys that are typed on the screen
; on entry: A character input from the keyboard is in AL
; on exit: The sentence length and cursorOffset's values will be updated and the character will be on the screen. DI will hold the cursorOffset

handleChar PROC

        PUSH    SI AX DI

        CMP     incorrectFlag, 5
        JE      colorRedd
        JMP     colorGreen

colorRedd:
        MOV     AH, 00000100b                   ; Turn it red if it's incorrect 
        MOV     DI, 23 * 160                    ; The line where the characters will appear
        ADD     DI, cursorOffset                ; Add cursor offset
        CMP     cursorCol, 80                   ; Is it the end of the line?
        JE      endHandleChar                   ; If so, don't add the character to the screen
        MOV     ES:[DI], AX                     ; If not, then put character on screen
        INC     typedSentenceLength             ; Update the sentence length since a character is added
        ADD     cursorOffset, 2                 ; Update the cursorOffset
        JMP     endHandleChar

colorGreen:
        MOV     AH, 00000010b                   ; Turn it green if it's correct 
        MOV     DI, 23 * 160                    ; The line where the characters will appear
        ADD     DI, cursorOffset                ; Add cursor offset
        CMP     cursorCol, 80                   ; Is it the end of the line?
        JE      endHandleChar                   ; If so, don't add the character to the screen
        MOV     ES:[DI], AX                     ; If not, then put character on screen
        INC     typedSentenceLength             ; Update the sentence length since a character is added
        ADD     cursorOffset, 2                 ; Update the cursorOffset
        JMP     endHandleChar

endHandleChar:
        POP     DI AX SI
        RET

handleChar ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; clear typed text and reset the timer, but keep the same target sentence

shiftCheck PROC

        PUSH    CX DI AX

        MOV     AH, 12h                 ; Call the shift interrupt
        INT     16h
        AND     AL, 00000011b           ; Mask it with AND
        CMP     AL, 00000011b           ; Compare them

        MOV     CX, 0                   ; Set the accumulator to 0
        MOV     DI, 23 * 160            ; Set the screen position to the location where the user is typing

        JE      handleShiftLoop         ; Clear everything and restart timer if the user pressed both shifts
        JMP     endHandleShift

handleShiftLoop:

        CMP     CX, 160                 ; See if we've cleared 160 characters
        JE      endHandleShiftLoop      ; If we have, end it
        MOV     BYTE PTR ES:[DI], " "   ; If not, put a space 
        ADD     DI, 2                   ; Next screen position
        INC     CX                      ; Increment the accumulator
        MOV     incorrectFlag, 0        ; Reset the incorrect flag
        JMP     handleShiftLoop         ; Loop back

endHandleShiftLoop:

        CALL    startTimer
        MOV     cursorCol, 0            ; initialize to 0, which is the beginning of the screen 
        MOV     cursorOffset, 0         ; initialize to 0, which is the beginning of the screen
        MOV     cursorRow, 23           ; initialize to 23 for the row the user will write on
        CALL    updateCursorPosition

endHandleShift:

        POP     AX DI CX
        RET  

shiftCheck ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; clear typed text, reset the timer, and display a new randomly selected sentence

handleF1 PROC

        CALL    displaySentence
        CALL    startTimer
        DEC     cursorCol
        RET

handleF1 ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Starts the timer
; on exit: the number of timer ticks is in startTime

startTimer PROC

        PUSH    CX AX DX
        MOV     AH, 00h                 ; Get number of timer ticks. CX:DX = number of timer ticks since midnight
        INT     1Ah
        MOV     startTime, DX           ; Move the number of timer ticks into startTime
        POP     DX AX CX
        RET

startTimer ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Updates the timer
; on entry: startTime will have the number of ticks
; on exit: AX will have time passed

updateTimer PROC

        PUSH    CX BX AX
        PUSH    startTime               ; Put startTime on the BP stack
        CALL    computeElapsedTenths    ; Get the time passed in tenths of a second    
        PUSH    AX                      ; Push time passed in tenths of a second onto the stack
        CALL    displayElapsedTenths    ; Display the time on the screen

        MOV     BX, 600                 ; Put 600 millis into BX
        MOV     CX, maxMinutes          ; Put the number of minutes the program is allowed to run into CX. I set it to one, should be more than enough time.
        MUL     CX                      
        CMP     BX, AX                  ; Check if the current time is equal to the max time
        JE      exitProgram             ; if it is, then exit the program

        POP     AX BX CX
        RET

exitProgram:

        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        int     21h                     ; This ones part of that as well

updateTimer ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Gets the amount of time passed since program start in tenths of a second
; on entry: startTime is on the stack
; on exit: AX has the tenths of a second

computeElapsedTenths PROC

        PUSH    BP                      ; Save BP                             
        MOV     BP, SP                  ; Make BP point to the call frame on the stack
        PUSH    DX BX                   ; Push the rest

        MOV     AH, 00h                 ; Get number of timer ticks. CX:DX = number of timer ticks since midnight
        INT     1Ah

        ; call frame on stack looks like
        ; startTime             [BP + 4]
        ; return address        [BP + 2]
        ; saved BP              [BP]

        SUB     DX, [BP + 4]            ; Subtract the start ticks from the current ticks
        MOV     AX, DX                  ; Move that value into AX
        MOV     BX, 55                  ; Move 55 into BX for multiplication
        MUL     BX                      ; Multiply the number of ticks by 55 to get the number of millis

        MOV     BX, 100                 ; Move 100 into BX for division to turn the millis into tenths of a second
        DIV     BX                      ; Get the tenths
        
        POP     BX DX BP
        RET     2

computeElapsedTenths ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Takes the number for the time and displays it as a string
; on entry: AX is on the stack with the tenths of a second

displayElapsedTenths PROC

        PUSH    BP                      ; Save BP 
        MOV     BP, SP                  ; Make BP point to the call frame on the stack
        PUSH    AX BX DX                ; Push the rest

        ; call frame on stack looks like
        ; AX                    [BP + 4]                (time in tenths of a second)
        ; return address        [BP + 2]
        ; saved BP              [BP]

        MOV     AX, [BP + 4]            ; Move the currentTime into AX
        MOV     BX, 10                  ; BX is the divisor

        ; Hundredths place
        MOV     DX, 0                   ; Clear DX
        DIV     BX                      ; Divide by 10
        ADD     DX, '0'                 ; Add '0' to make it ASCII
        PUSH    DX                      ; Push it onto the stack so it can be displayed later

        ; Ones place
        MOV     DX, 0                   ; Clear DX
        DIV     BX                      ; Divide by 10
        ADD     DX, '0'                 ; Add '0' to make it ASCII
        PUSH    DX                      ; Push it onto the stack so it can be displayed later
        
        ; Tens place
        MOV     DX, 0                   ; Clear DX
        DIV     BX                      ; Divide by 10
        ADD     DX, '0'                 ; Add '0' to make it ASCII
        PUSH    DX                      ; Push it onto the stack so it can be displayed later

        ; Don't need the hundreds and thousands place if we're only going to 1 minute

        ; Hundreds place
        ;MOV     DX, 0                   ; Clear DX
        ;DIV     BX                      ; Divide by 10
        ;ADD     DX, '0'                 ; Add '0' to make it ASCII
        ;PUSH    DX                      ; Push it onto the stack so it can be displayed later

        ; Thousands place
        ;MOV     DX, 0
        ;DIV     BX
        ;ADD     DX, '0'
        ;MOV     ES:[16 * 160 + 40], DL

        ;POP     DX                              ; Pop the hundreds place and display it
        ;MOV     ES:[16 * 160 + 42], DL

        POP     DX                              ; Pop the tens place and display it
        MOV     ES:[16 * 160 + 44], DL

        POP     DX                              ; Pop the ones place and display it
        MOV     ES:[16 * 160 + 46], DL

        MOV     ES:[16 * 160 + 48], BYTE PTR '.' ; display the '.'

        POP     DX                              ; Pop the tenths place and display it
        MOV     ES:[16 * 160 + 50], DL

        POP     DX BX AX BP
        RET 2                                   ; Return the BP stack size to clear it

displayElapsedTenths ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------

MyCode ENDS                        ; End Segment

;======================================================================================
END mainProg                       ; This END marks the physical end of the source code file. The label specified will be the "user transfer address" where execution will begin.