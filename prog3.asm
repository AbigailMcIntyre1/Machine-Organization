; Project Three - Machine Org
; Due 03/15/2022
; Displays three letters on the screen after a random wait time. The user must enter the three letters correctly without error to win.

;======================================================================== STACK SEGMENT
MyStack SEGMENT STACK              ; Keyword "STACK" is required!

        DW 256 DUP (?)             ; At least a small stack is needed for interrupt handling in a multiprocessing OS (not really needed when using DOSBox, though).

MyStack ENDS                       ; ENDS = End Segment

;======================================================================== DATA SEGMENT
MyData SEGMENT

        ScreenMemSeg EQU 0B800h         ; Segment for video RAM.

        numLettersCorrect       DB 0                    ; Keeps track of the number of letters the user has gotten correct
        startTime               DW 0                    ; The time the program started
        randomNumSeed           DW 1                    ; The seed for the random number generator
        randomLetters           DB 3 DUP (?)            ; The three randomly selected letters
        copiedLetters           DB 3 DUP (?)            ; Duplicate of the list to remove letters from 
        numCharsCorrect         DB 0                    ; The number of correct letters the user has typed
        timeBeforeStart         DW 0

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

        CALL    clearScreen             ; Clear the screen
        CALL    waitTime                ; Wait a random amount of time
        CALL    getRandomCharacter      ; Then pick the random characters
        CALL    copyCharsIntoList       ; Then copy the characters into the copiedLetters
        CALL    startTimer              ; Start the timer

mainLoop:
        CALL    updateTimer             ; Call the timer function to update the timer
        MOV     AH, 11h                 ; Check buffer
        INT     16h                     ; Wait for a key and read from the buffer
        JZ      mainLoop                ; If no key, skip
        MOV     AH, 10h                 ; Read the key
        INT     16h                     
        CMP     AL, 27                  ; Is it esc?
        JE      exitMainLoop            ; If so, exit
        CALL    processKey              ; If not, process the key input

        JMP     mainLoop                ; Else, loop again

exitMainLoop:

        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        INT     21h                     ; This ones part of that as well

mainProg ENDP                           ; End of the main Proc

; --------------------------------------------------------------------------------------------------------------------------------------------------
; it checks the carry flag, and if its true, then the typed character is correct
checkIfDone PROC

        JC      foundCorrectCharacter   ; If the carry flag is true, the typed character is correct
        MOV     numCharsCorrect, 0      ; If it isn't true, reset the number of correct characters typed to start over
        CALL    copyCharsIntoList       ; And also reset the characters in the list
        RET

foundCorrectCharacter:

        INC     numCharsCorrect         ; Increment the numCharsCorrect
        CMP     numCharsCorrect, 3      ; Check if three of them are correct
        JE      gameHasBeenWon          ; If so, the player wins
        RET                             ; If not, return back

gameHasBeenWon:    

        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        INT     21h                     ; This ones part of that as well
        RET

checkIfDone ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Copies the letters from the randomLetters list into the copiedLetters list
copyCharsIntoList PROC

        PUSH    SI DI CX AX
        LEA     SI, randomLetters       ; Move the address of randomLetters into SI
        LEA     DI, copiedLetters       ; And then move the address of the copiedLetters into DI
        MOV     CX, 0                   ; Initialize the counter to 0         

copyLoop:

        CMP     CX, 3                   ; Have we copied all three characters?
        JE      exitCopyChars           ; If so, exit
        MOV     AL, DS:[SI]             ; Else, move the character into AL
        MOV     DS:[DI], AL             ; And then move the character into the copiedLetters list
        INC     DI                      ; Go to the next character in the copiedLetters list
        INC     SI                      ; Go to the next character in the randomLetters list
        INC     CX                      ; Increment the counter
        JMP     copyLoop                ; Loop back around

exitCopyChars:

        POP     AX CX DI SI
        RET

copyCharsIntoList ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Checks the list and sets the carry bit to 1 if it's been found 
; On entry: AL contains char to check
; On exit: carryflag is 1 iff it is in the list

checkList PROC

        CMP     AL, randomLetters       ; compare to first randomLetter
        JE      foundIt
        CMP     AL, randomLetters + 1   ; to 2nd
        JE      foundIt 
        CMP     AL, randomLetters + 2   ; to 3rd
        JNE     notFound
foundIt:

        STC                             ; set carry bit to I
        RET

notFound:

        CLC                             ; set carry bit to 0 
        RET

checkList ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; On entry: BX contains an upper limit
; On exit: AX contains random number in the range of 0...BX-1

getRandomNum PROC

        PUSH    SI BX DX CX

        ; multiply seed by prime #a
        ; add prime number b
        ; save result in seed
        ; divide seed by BX

        MOV     AH, 00h                 ; Get number of timer ticks. CX:DX = number of timer ticks since midnight
        INT     1ah                     ; DX contains the number of timer ticks

        MOV     AX, DX                  ; Put the number of timer ticks into AX
        MOV     SI, randomNumSeed       ; Put the seed in SI
        MUL     SI                      ; Multiply the seed by the number of timer ticks         
        ADD     AX, 1201                ; Add prime number b

        MOV     DX, 0                   ; Clear DX
        DIV     BX                      ; Divide AX by BX which has the upper bound
        MOV     AX, DX                  ; Put the remainder into AX
        MOV     randomNumSeed, AX       ; Move the last result into the seed

        MOV     AX, DX                  ; Put the result into AX

        POP     CX DX BX SI
        RET

getRandomNum ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Gets a random character and displays it on the screen
getRandomCharacter PROC

        PUSH    AX BX SI DI DX
        LEA     SI, randomLetters

randOne:
        MOV     BX, 26                  ; Get a random number between 1-26
        CALL    getRandomNum            ; ^
        MOV     AH, 0                   ; Clear the high bit
        ADD     AL, 'A'                 ; Add 'A' to get the ascii character
        CALL    checkList               ; Is it already in the list?
        JC      randOne                 ; If so, try again
        MOV     DS:[SI], AL             ; Add it to the list
        MOV     DX, 0                   ; Clear DX
        MOV     DL, AL                  ; Add the letter into AL
        MOV     BX, 1500                ; Put 2000 into BX as the high bound for the random number
        CALL    getRandomNum            ; Get the random number
        SHL     AX, 1                  
        MOV     DI, AX                  ; Move the random number into DI
        MOV     ES:[DI], DL             ; Put the random letter on the screen in a random location

randTwo:
        MOV     BX, 26                  ; Get a random number between 1-26
        CALL    getRandomNum            ; ^
        MOV     AH, 0                   ; Clear the high bit
        ADD     AL, 'A'                 ; Add 'A' to get the ascii character
        CALL    checkList               ; Is it already in the list?
        JC      randTwo                 ; If so, try again
        INC     SI                      ; Go to next location in the list
        MOV     DS:[SI], AL             ; Add it to the list
        MOV     DX, 0                   ; Clear DX
        MOV     DL, AL                  ; Add the letter into AL
        MOV     BX, 1500                ; Put 2000 into BX as the high bound for the random number
        CALL    getRandomNum            ; Get the random number
        SHL     AX, 1                  
        MOV     DI, AX                  ; Move the random number into DI
        MOV     ES:[DI], DL             ; Put the random letter on the screen in a random location

randThree:
        MOV     BX, 26                  ; Get a random number between 1-26
        CALL    getRandomNum            ; ^
        MOV     AH, 0                   ; Clear the high bit
        ADD     AL, 'A'                 ; Add 'A' to get the ascii character
        CALL    checkList               ; Is it already in the list?
        JC      randThree               ; If so, try again
        INC     SI                      ; Go to next location in the list
        MOV     DS:[SI], AL             ; Add it to the list
        MOV     DX, 0                   ; Clear DX
        MOV     DL, AL                  ; Add the letter into AL
        MOV     BX, 1500                ; Put 2000 into BX as the high bound for the random number
        CALL    getRandomNum            ; Get the random number
        SHL     AX, 1                  
        MOV     DI, AX                  ; Move the random number into DI
        MOV     ES:[DI], DL             ; Put the random letter on the screen in a random location

exitPickRandomChar:

        POP    DX DI SI BX AX
        RET

getRandomCharacter ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Processes the keys input by the user
; on entry: A key has been read into AL and needs to be processed

processKey PROC

        PUSH    DI CX BX SI

        LEA     DI, copiedLetters       ; Move the address of copiedLetters into DI
        MOV     CX, 0                   ; Move 0 into CX for the counter

processKeyLoop:
        CMP     CX, 3                   ; Have we checked all three characters?
        JE      charNotFound            ; If we have and haven't found any correct ones, return with carry flag = 0
        CMP     AL, DS:[DI]             ; Is the letter in the list the same as the one the user typed?
        JE      charFound               ; If so, then the character has been found
        INC     DI                      ; If not, go to the next character
        INC     CX                      ; And increment the counter
        JMP     processKeyLoop

charFound:
        MOV     DS:[DI], BYTE PTR 0     ; Set the character at this position to 0
        POP     SI BX CX DI
        STC                             ; Set carry flag to correct
        CALL    checkIfDone
        RET
charNotFound:
        POP     SI BX CX DI
        CLC                             ; Set the carry flag to incorrect
        CALL    checkIfDone
        RET

processKey ENDP

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
        POP     AX BX CX
        RET

exitProgramm:

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

        ; Hundreds place
        MOV     DX, 0                   ; Clear DX
        DIV     BX                      ; Divide by 10
        ADD     DX, '0'                 ; Add '0' to make it ASCII
        PUSH    DX                      ; Push it onto the stack so it can be displayed later

        ; Thousands place
        MOV     DX, 0
        DIV     BX
        ADD     DX, '0'
        MOV     ES:[1 * 160 + 2], DL

        POP     DX                              ; Pop the hundreds place and display it
        MOV     ES:[1 * 160 + 4], DL

        POP     DX                              ; Pop the tens place and display it
        MOV     ES:[1 * 160 + 6], DL

        POP     DX                              ; Pop the ones place and display it
        MOV     ES:[1 * 160 + 8], DL

        MOV     ES:[1 * 160 + 10], BYTE PTR '.' ; display the '.'

        POP     DX                              ; Pop the tenths place and display it
        MOV     ES:[1 * 160 + 12], DL

        POP     DX BX AX BP
        RET 2                                   ; Return the BP stack size to clear it

displayElapsedTenths ENDP

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
; Gets the low order of the number of timer ticks and stores it in a variable, generates a random number between 0-30, then adds 30 to make it 30-60 tenths of a second
; adds that to the startTime variable to get the time the program needs to officially start, loops until the current number of ticks is equal to that number.

waitTime PROC

        PUSH    BX AX CX DX

        MOV     AH, 00h  
        INT     1ah                     ; Get number of ticks and store it in DX

        MOV     timeBeforeStart, DX     ; Store that in out timeBeforeStart variable to keep track of it
        MOV     BX, 30                  ; Move 30 into BX so we can get the random number
        CALL    getRandomNum            ; A random number between 0 - 30 will be stored in AX
        ADD     AX, 30                  ; Add 30 to make it between 30 - 60 tenths
        ;CALL    showNum
        ADD     timeBeforeStart, AX     ; Add AX to the timeBeforeStart variable

waitTimeLoop:
        MOV     AH, 00h
        INT     1ah                     ; Get number of ticks and store it in DX

        CMP     DX, timeBeforeStart     ; Compare it to our wait time
        JGE     exitWaitTime            ; If the current time is greater than or equal to the wait time, exit
        JMP     waitTimeLoop            ; If it's not, then loop again

exitWaitTime:
        POP     DX CX AX BX
        RET

waitTime ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------

MyCode ENDS                        ; End Segment

;======================================================================================
END mainProg                       ; This END marks the physical end of the source code file. The label specified will be the "user transfer address" where execution will begin.