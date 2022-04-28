; Project Four - Machine Org
; Due 04/25/2022
; Processes a text file and creates a new file "output.dat" and displays how long it took to process the file. 
; The input file contains a name (a string of one or more non-whitespace characters) followed by one or more whitespace characters, 
; followed by an unsigned integer followed by one or more white space characters (LF, CR, tab, space). The last numeral in the file 
; may or may not be followed by white space. No name is more than 20 characters long, no numeral is greater than 65000, and there are at most 30000 names
; in the file. The output file contains all of the names from the input file which are followed by a numeral which is greater than the number specified 
; as a command line parameter. The names in the output file should be on separate lines, one name per line.


;======================================================================== STACK SEGMENT
MyStack SEGMENT STACK              ; Keyword "STACK" is required!

        DW 256 DUP (?)             ; At least a small stack is needed for interrupt handling in a multiprocessing OS (not really needed when using DOSBox, though).

MyStack ENDS                       ; ENDS = End Segment

;======================================================================== DATA SEGMENT
MyData SEGMENT

        ScreenMemSeg EQU 0B800h         ; Segment for video RAM.

        endTail         DB   128 DUP (" ")              ; Filled with spaces so that when parsing the numbers, it'll hit a space to end the parsing
        EOF             DW   0                          ; Marks whether we've hit the end of the file
        fileName        DB   128 DUP (0)                ; The name of the file parsed from the command line
        parsedNum       DB   128 DUP (0)                ; The integer from the command line that acts as a parameter in ASCII form
        cmdLineParam    DW   0                          ; The integer parsed from the command line that acts as a parameter in integer form
        outputFile      DB   "output.dat",0             ; The name of the output file
        errorMsg        DB   "Error reading file$",0    ; Error message to be displayed if there's an issue reading the file
        theName         DB   30 DUP (0)                 ; The name read in from the file
        buff            DB   64 DUP (?)
        currBuffPos     DW   $                          ; Points to the location counter which is one past the end of the buffer
        numBytesRead    DW   0                          ; Holds the number of bytes that have been read. It's 0 so there's no byte available the first time
                                                        ; getNextByte is called and it reloads the buffer
        inFileHandle    DW   0                          ; The file handle for the opened file
        outFileHandle   DW   0                          ; The file handle for the output file
        currNameLength  DW   0                          ; The length of the name being read
        currNumber      DW   0                          ; The number that corresponds to the name being read
        startTime               DW 0                    ; The time the program started     

MyData ENDS                             ; End Segment

;======================================================================== CODE SEGMENT
MyCode SEGMENT
        ASSUME CS:MyCode, DS:MyData     ; Tell the assembler what segments the segment registers refer to in the following code.

; --------------------------------------------------------------------------------------------------------------------------------------------------
; The main PROC of the program. It loops through keys being input and calls the PROC to process them
; on entry: stuff in data segment is defined and all that
; on exit: the program will end

mainProg  PROC                          ; This label will serve as the "user transfer address"

        MOV     AX, MyData              ; Make DS address our data segment.
        MOV     DS, AX

        CALL    copyEndTail             ; Copy the command tail into your data segment

        MOV     AX, 0B800h              ; Make ES segment register address video memory.
        MOV     ES, AX

        CALL    clearScreen             ; Clear the screen
        CALL    parseCommandLine        ; Parse the command line to get the file name and limit number
        CALL    displayCmdTail          ; Go ahead and display the command tail that was taken
        CALL    convertToInt            ; Convert the limit number from the command from ASCII to int
        CALL    openFiles               ; Open the two files (display an error message and jump to end of main if an error occurs)
        CALL    startTimer              ; Start the timer
 
mainProgLoop:

        CALL    updateTimer             ; Call the timer function to update the timer
        CALL    getNextName             ; Get the name from the file
        CALL    getNextNumber           ; Get the number from the file
        MOV     AX, currNumber          ; Put the current number into AX to compare

        MOV     BX, cmdLineParam        ; Move the minimum number from the user into cmdLineParam
        CMP     AX, BX                  ; Is the current number greater than the minimum number read from the file?
        JGE     callWriteToFile         ; If so, write it to file

        CMP     EOF, 0                  ; Has the end of the file been reached?
        JNE     exitMainProg            ; If so, then exit
        JE      mainProgLoop            ; If not, loop back around

callWriteToFile:

        CALL    writeToFile             ; Write the name to the output file
        JMP     mainProgLoop            ; Jump back up to the main loop

exitMainProg:

        MOV     AH, 3Eh                 ; Close the file
        LEA     BX, outputFile          ; Load the address of outputFile into BX
        INT     21h                     ; Perform the interrupt

        MOV     AH, 3Eh                 ; Close the file
        LEA     BX, inFileHandle        ; Load the address of inFileHandle into BX
        INT     21h                     ; Perform the interrupt

        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        INT     21h                     ; This ones part of that as well

mainProg ENDP                           ; End of the main PROC

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Writes the name to the file
; On entry: The number next to the name has been compared and found to be within range
; On exit: The name has been written to the file

writeToFile PROC

        PUSH    DX BX CX AX

        MOV     AH, 40h                 ; Write to file number
        MOV     BX, outFileHandle       ; Put the output file handle into BX
        MOV     CX, currNameLength      ; Put the number of bytes to write (the name length) into CX
        LEA     DX, theName             ; The address of the name to be written to the file
        INT     21h                     ; Perform the interrupt
        
        POP     AX CX BX DX
        RET

writeToFile ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Gets the next number in the file.
; On entry: The name has been read and stored in a variable
; On exit: currNumber has the current number read from the file

getNextNumber PROC

        PUSH        AX BX CX DX

        CALL        skipWhiteSpace          ; Skip the whitespace 
        MOV         currNumber, 0           ; Move 0 into the current number so that it's empty when we read it in

getNextNumberLoop:

        CALL        getNextByte             ; Get the next byte in the file
        CMP         AX, " "                 ; Is it white space?
        JLE         endGetNextNumber        ; If so, then the number is over

        MOV         BX, AX                  ; Move the number into BX
        SUB         BX, '0'                 ; Subtract '0' to get the int value
        MOV         CX, 10                  ; Move 10 into CX to multiply by
        MOV         AX, currNumber          ; Put the current number into AX
        MUL         CX                      ; Then multiply the current number by 10
        MOV         DX, 0
        MOV         currNumber, AX          ; Put the result into the current number
        ADD         currNumber, BX          ; Then add the number to the result
        JMP         getNextNumberLoop       ; Loop back around

endGetNextNumber:

        POP         DX CX BX AX
        RET

getNextNumber ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Gets the next byte from the file and stores the character in AL
; On entry: currBuffPos point to
; On exit: AL contains the read byte

getNextByte PROC

        PUSH    SI DX CX
        MOV     SI, currBuffPos         ; Move the current position of the buffer into SI
        LEA     DX, buff                ; Move the address of the buffer into DX
        ADD     DX, numBytesRead        ; Move the number of bytes that have already been read into DX
        CMP     SI, DX                  ; If it's less, then DX is pointing to data
        JL      byteIsAvailable         ; Returns the byte and advances the currBuffPos

        ; Load the buffer
        MOV     BX, inFileHandle        
        MOV     CX, 64                  ; Move the number of bytes to read into CX
        LEA     DX, buff                ; Load the address of the buffer into DX
        MOV     AH, 3Fh                 ; Set to function 3Fh to read from the file
        INT     21h                     ; Do the interrupt

        JC      inFileException         ; If the carry flag is set, jump to handle the file exception
        MOV     numBytesRead, AX        ; 
        CMP     AX, 0                   ; Check the number of bytes read, is it 0?
        JE      setEOFflag              ; If so, then set the end of file flag
        LEA     DX, buff                ; Else, reload DX with the buffer
        MOV     currBuffPos, DX         ; Then reload the current buffer position with the buffer
        JMP     byteIsAvailable

byteIsAvailable:

        MOV     SI, currBuffPos
        MOV     AL, DS:[SI]
        MOV     AH, 0
        INC     currBuffPos
        JMP     endGetNextByte

inFileException:

        CALL    displayErrorMsg         ; If there was an error, display the message
        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        INT     21h                     ; This ones part of that as well

setEOFflag:

        MOV     EOF, 1                  ; Move 1 into EOF to signal the end of the file
        JMP     endGetNextByte

endGetNextByte:

        POP     CX DX SI
        RET

getNextByte ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Gets the next name in the inputFile and stores it in theName variable. Also stores the length of the name in currNameLength
; On entry: inFile and outFile have been opened
; On exit: The next name in the file has been stored in theName and the length of the name has been stored in currNameLength

getNextName PROC

        PUSH    SI AX

        MOV     currNameLength, 0
        CALL    skipWhiteSpace          ; Skip the whitespace 
        CMP     EOF, 0                  ; Is it the end of the file?
        JNE     endGetNextName          ; If so, jump to end the PROC (check in main if EOF has been reached)
        LEA     SI, theName             ; If not, load the address of the name variable into SI

copyLoop:

        CALL    getNextByte             ; Get the next byte (stored in AL)
        CMP     AL, " "                 ; Is it a space?
        JLE     endGetNextNameAlmost    ; If so, jump to put a newline and get the size of the name because the name is done being read
        INC     currNameLength
        MOV     DS:[SI], AL             ; Move the character into the name variable
        INC     SI                      ; Move to the next position in theName
        JMP     copyLoop                ; continue looping

endGetNextNameAlmost:

        MOV     DS:[SI], WORD PTR 0Dh   ; Put the carriage return into theName
        INC     SI                      ; Move to the next position in theName
        MOV     DS:[SI], WORD PTR 0Ah   ; Put the line feed into theName
        ADD     currNameLength, 2       ; Add two to the currNameLength

endGetNextName:

        POP     AX SI
        RET

getNextName ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Reads the next byte in a file. If it's the end of the file, it exits. If it's whitespace, it gets the next byte until it's not whitespace. 
; If it's not whitespace, it returns
; On entry: File is being read
; On exit: Whitespace has been skipped and the next byte is a character/number/whatever

skipWhiteSpace PROC

        PUSH    AX

swsLoop:

        CALL    getNextByte             ; Read the next byte in the file
        CMP     EOF, 1                  ; Check if it's the end of the file
        JE      endSkipWhitespace       ; If so, then end the function
        CMP     AL, " "                 ; Else, compare the character to a space
        JLE     swsLoop                 ; If it's less than or equal to a space, then it's whitespace, so loop again

endSkipWhitespace:

        DEC     currBuffPos
        POP     AX
        RET

skipWhiteSpace ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Opens the input and output files
; On entry: The fileName has been parsed and saved
; On exit: an input file will have been opened and an output file will have been created. If there was an error, a message would be on the screen and the program ended

openFiles PROC

        PUSH    CX AX DX

        ; Open the existing input file
        MOV     AH, 3Dh                 ; Set to function 3Dh to open an existing file
        LEA     DX, fileName            ; Load fileName into DS:[DX]
        MOV     AL, 0                   ; Set the access code to read
        INT     21h                     ; Do the interrupt
        MOV     inFileHandle, AX
        JC      errorCaught             ; If there was an error opening the file, jump
        JMP     createFile              ; Else, go on to create the file

createFile:

        ; Create the output file
        MOV     AH, 3Ch                 ; Set to function 3Ch to create a new file
        LEA     DX, outputFile          ; Load the outputFile into DS:[DX]
        MOV     CL, 00000000b           ; Set the file attribute to nothing
        INT     21h                     ; Do the interrupt
        MOV     outFileHandle, AX
        JC      errorCaught             ; If there was an error creating the file, jump 
        JMP     endOpenFiles            ; Jump to end the proc

errorCaught:

        ; Display and error and end the program if there's an error with the file
        CALL    displayErrorMsg         ; Display the error message on the screen if there was an issue opening the file
        MOV     AH, 4Ch                 ; A DOS interrupt to release the memory and return control to DOS
        INT     21h                     ; This ones part of that as well

endOpenFiles:

        POP     DX AX CX
        RET

openFiles ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Parses endTail and splits it into fileName and parsedNum
; On entry: The command tail has been saved in endTail
; On exit: fileName will contain the name of the file and parsedNum will contain the ASCII form of the number

parseCommandLine PROC

        PUSH    DI CX AX DX BX SI

        LEA     DI, endTail             ; Put the address of endTail into DI
        LEA     SI, fileName            ; Put the address of fileName into DX
        LEA     BX, parsedNum           ; Put the address of parsedNum into BX

parseFileNameLoop:

        CMP     [DI], BYTE PTR " "      ; Else, is the character a space?
        JE      endParseFileNameLoop    ; If so, jump to parse the integer
        MOV     AL, [DI]                ; Else, move the character from endTail into AL
        MOV     [SI], AL                ; Then move the character from AL to fileName
        INC     DI                      ; Go to the next position in endTail
        INC     SI                      ; Go to the next position in fileName
        DEC     CL                      ; Decrement the counter
        JMP     parseFileNameLoop       ; Loop back around

endParseFileNameLoop:

        MOV     [SI], BYTE PTR 0        ; Move a 0 to the end of fileName
        ; Advance from the space between the fileName and the integer
        INC     DI                      ; Go to the next position in endTail
        INC     SI                      ; Go to the next position in fileName
        JMP     parseIntegerLoop        ; Jump to parse the integer

parseIntegerLoop:

        CMP     [DI], BYTE PTR 0
        JE      endParseCommandLine     ; If so, end the loop
        MOV     AL, [DI]                ; Else, move the character from endTail into AL
        MOV     [BX], AL                ; Then move the character from AL to parsedNum
        INC     DI                      ; Go to the next position in endTail
        INC     BX                      ; Go to the next position in parsedNum
        JMP     parseIntegerLoop        ; Loop back around

endParseCommandLine:

        INC     DI                      ; Go to the next position in endTail
        INC     BX                      ; Go to the next position in parsedNum
        MOV     [BX], BYTE PTR 0        ; Move a 0 to the end of parsedNum
        POP     SI BX DX AX CX DI
        RET

parseCommandLine ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Converts parsedNum into an integer and stores it in cmdLineParam
; On entry: parsedNum will contain the ASCII form of the number from the command line
; On exit: cmdLineParam will contain the converted integer

convertToInt PROC

        PUSH    BX DX AX SI CX

        LEA     SI, parsedNum           ; Load the address of parsedNum into SI
        MOV     AX, 0                   ; Just in case cause we're multiplying
        MOV     BX, 10                  ; To multiply total by
        MOV     CH, 0

convertToIntLoop:

        MOV     CL, [SI]                ; Get the digit that SI points to
        CMP     CL, 0                   ; Is it 0?
        JE      endConvertToIntLoop     ; If so, end the loop
        MUL     BX                      ; Multiply the total by 10
        SUB     CL, '0'                 ; Convert from char by subtracting '0'
        ADD     AX, CX                  ; Add the int to the total
        INC     SI
        JMP     convertToIntLoop        ; Loop back around

endConvertToIntLoop:

        MOV     cmdLineParam, AX        ; Move the completed integer into the cmdLineParam variable

        POP     CX SI AX DX BX
        RET

convertToInt ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Copies the command tail into the data segment
; On entry: The user has entered the command line containing the file name and the parameter for the number after the names
; On exit: The command tail has been saved in the endTail variable

copyEndTail PROC

        PUSH    SI CX AX DS DI
        MOV     SI, 82h                 ; The character after prog4 and space in t:>prog4 names.dat integer
        MOV     CL, ES:[80h]            ; 80h contains the length of the command tail
        DEC     CL
        MOV     AX, MyData              ; Make AX point to the data segment
        MOV     DS, AX                  ; Move that into DS
        LEA     DI, endTail             ; Load the end of the command tail into DI

copyTailLoop:
        CMP     CL, 0                   ; Have we read all the characters in the command tail?
        JE      endLoop                 ; If so, end the loop
        MOV     AL, ES:[SI]             ; Else, move the character into AL
        MOV     [DI], AL                ; Then put AL into endTail
        INC     SI                      ; Go to the next character position
        INC     DI                      ; Go to the next position in endTail
        DEC     CL                      ; Decrement the loop counter
        JMP     copyTailLoop            ; Loop back around

endLoop:
        MOV     [DI], BYTE PTR 0
        MOV     AX, 0B800h              
        MOV     ES, AX                  ; Make ES segment register address video memory
        POP     DI DS AX CX SI
        RET

copyEndTail ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays an integer in AX on the screen
; On entry: AX contains unsigned int
; On exit: ES:[DI] points to ones digit

displayInt PROC

        PUSH    AX BX DX DI
        MOV     BX, 10                  ; Divisor

cvtLoop:

        XOR     DX, DX                  ; Move 0 into DX
        DIV     BX                      ; DL has low digit now
        ADD     DL, '0'                 ; Make it ASCII
        MOV     ES:[DI], DL             ; Put it on the screen
        SUB     DI, 2                   ; Back up one screen position
        CMP     AX, 0                   ; Is the quotient 0?
        JA      cvtLoop                 ; If not, repeat

        POP     DI DX BX AX
        RET

displayInt ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays what's in endTail
; On entry: The command tail has been saved in endTail
; On exit: endTail will be displayed on the screen

displayCmdTail PROC

        PUSH    SI AX DI

        LEA     SI, endTail                     ; Load the address of endTail into SI
        MOV     DI, 160 * 2 + 4                 ; Random screen position
        MOV     AH, 0Ch

displayCmdTailLoop:

        CMP     [SI], BYTE PTR 0                ; See if we're at the end of the tail
        JE      endDisplayCmdTailLoop           ; If so, end the loop
        MOV     AL, [SI]                        ; Else, move the character into AX
        MOV     ES:[DI], AL                     ; And then move the character to the screen
        ADD     DI, 2                           ; Add 2 to DI to advance to the next screen position
        INC     SI                              ; Go to the next position in endTail
        JMP     displayCmdTailLoop

endDisplayCmdTailLoop:

        POP     DI AX SI
        RET

displayCmdTail ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays what's in fileName
; On entry: The command tail has been saved in endTail
; On exit: endTail will be displayed on the screen

displayFileName PROC

        PUSH    DI SI AX

        LEA     SI, fileName                    ; Load the address of endTail into SI
        MOV     DI, 160 * 7 + 4                 ; Random screen position
        MOV     AH, 0Ch

displayFileNameLoop:

        CMP     [SI], BYTE PTR 0                ; See if we're at the end of the tail
        JE      endDisplayFilenameLoop          ; If so, end the loop
        MOV     AL, [SI]                        ; Else, move the character into AX
        MOV     ES:[DI], AL                     ; And then move the character to the screen
        ADD     DI, 2                           ; Add 2 to DI to advance to the next screen position
        INC     SI                              ; Go to the next position in endTail
        JMP     displayFileNameLoop

endDisplayFilenameLoop:

        POP     AX SI DI
        RET

displayFileName ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays what's in theName
; On entry: The name has been read
; On exit: theName will be displayed on the screen

displayTheName PROC

        PUSH    DI SI AX

        LEA     SI, theName                    ; Load the address of endTail into SI
        MOV     DI, 160 * 7 + 4                ; Random screen position
        MOV     AH, 0Ch

displayTheNameLoop:

        CMP     [SI], BYTE PTR 0                ; See if we're at the end of the tail
        JE      endDisplayTheNameLoop          ; If so, end the loop
        MOV     AL, [SI]                        ; Else, move the character into AX
        MOV     ES:[DI], AL                     ; And then move the character to the screen
        ADD     DI, 2                           ; Add 2 to DI to advance to the next screen position
        INC     SI                              ; Go to the next position in endTail
        JMP     displayTheNameLoop

endDisplayTheNameLoop:

        POP     AX SI DI
        RET

displayTheName ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays what's in parsedNum
; On entry: The number has been parsed and saved in parsedNum
; On exit: parsedNum will be displayed on the screen

displayParsedNum PROC

        PUSH    DI SI AX

        LEA     SI, parsedNum                   ; Load the address of endTail into SI
        MOV     DI, 160 * 8 + 4                 ; Random screen position
        MOV     AH, 0Ch

displayParsedNumLoop:

        CMP     [SI], BYTE PTR 0                ; See if we're at the end of the tail
        JE      endDisplayParsedNumLoop         ; If so, end the loop
        MOV     AL, [SI]                        ; Else, move the character into AX
        MOV     ES:[DI], AL                     ; And then move the character to the screen
        ADD     DI, 2                           ; Add 2 to DI to advance to the next screen position
        INC     SI                              ; Go to the next position in endTail
        JMP     displayParsedNumLoop

endDisplayParsedNumLoop:

        POP     AX SI DI
        RET

displayParsedNum ENDP

; --------------------------------------------------------------------------------------------------------------------------------------------------
; Displays the error message

displayErrorMsg PROC

        PUSH    DI SI AX

        LEA     SI, errorMsg                    ; Load the address of endTail into SI
        MOV     DI, 160 * 9 + 4                 ; Random screen position
        MOV     AH, 0Ch

displayErrorMsgLoop:

        CMP     [SI], BYTE PTR 0                ; See if we're at the end of the tail
        JE      endDisplayErrorMsgLoop          ; If so, end the loop
        MOV     AL, [SI]                        ; Else, move the character into AX
        MOV     ES:[DI], AL                     ; And then move the character to the screen
        ADD     DI, 2                           ; Add 2 to DI to advance to the next screen position
        INC     SI                              ; Go to the next position in endTail
        JMP     displayErrorMsgLoop

endDisplayErrorMsgLoop:

        POP     AX SI DI
        RET

displayErrorMsg ENDP


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
; Clears the screen

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

MyCode ENDS                        ; End Segment

;======================================================================================
END mainProg                       ; This END marks the physical end of the source code file. The label specified will be the "user transfer address" where execution will begin.