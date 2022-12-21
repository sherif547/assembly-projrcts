model LARGE
.STACK
.DATA
    
   
    ;INTRO
    w db "welcome to learn assembly","$"
    q db 10,"choose what you want to learn","$"
    INTRO DB 10,"1-INTRODUCTION:","$"
    Assembly_lan DB 10,"1-Assembly language:","$"
    Assemb_DEF DB 10," Assembly language:- is a low-level programming language and it is a machine ",10," friendly language that is depend mainly on the structure of the computer." ,"$"
        Computer_structure DB 10,"2-Computer structure:","$"
            struc_DEF DB 10,"Computer structure:-refers to the way in which the components of a computer are interrelated.","$"
            CPU DB 10,"1-Central Processing Unit:- controls the operation of the computer and performs its data processing.","$"
                CU DB 10,"1.Control unit:- controls the operation of the CPU and hence the computer.","$"
                ALU DB 10,"2.Arithmetic and logic unit:- Performs the computer?s data processing functions (mathematical/logic operations).","$"
                Reg DB 10,"3.Registers:- Provides storage internal to the CPU (very fast local memory cells, that store operands of operations and intermediater).","$"
            CPU_intercon DB 10,"4.CPU interconnection:- Some mechanism that provides for communication among the control unit, ALU, and registers.","$"
            Main_Memory DB 10,"2-Main Memory:- stores data , instruction and subroutine.","$"
            I_devices DB 10,"3-input / output devices:- move data between the computer and its external environment.","$"
            Sys_Bus DB 10,"4-System Bus:- provides for communication among CPU , Main Memory and input /output devices .","$"
                Data_bus DB 10,"1.Data bus:- carries the actual data between the CPU, the Main memory and all other internal computer components, It can transfers data in or out of the CPU or from one device to another.","$"
                Add_bus DB 10,"2.Address bus:- tells the system where data comes from or goes. this bus takes data from the CPU and stores it in a specific location in the main memory, or vice versa.","$"
                Cont_bus DB 10,"3.Control bus:- determines how the system bus operates by managing the control, timing and coordination of the busses to ensure the data transfers without corruption.","$"

   ;MEMORY SEGMENT
       MEMORY_SEG  DB 10,"2-MEMORY SEGMENT:" ,"$" 
            SEG_DEF DB 10,"MEMORY SEGMENT :-all memory locations within a segment are relative to the",10, "    segment's data, and what is known as the stack. ","$"
            CODE_SEG    DB   10, "> CODE SEGMENT :- Contains the machine instructions that are to execute,",10,"    typically, the first executable instruction is at the start of segment.","$"
            Data_SEG    DB   10, "> DATA SEGMENT :- Contains a program's defined data, constants, and work areas.","$"
            Stack_SEG   DB   10, "> STACK SEGMENT:- Contains any data and addresses that the program needs to ",10,"   save temporarily or for use by your own 'called' subroutines.","$"
   
                
   ;REGESTERS                             
   REGISTERS DB 10,"3-REGISTERS:","$"
        SEG_REGISTERS DB 10,"1-SEGMENT REGISTERS","$"
            _CS DB 10,"> CS :- contains the starting address of a program's code segment.","$"
            _DS DB 10,"> DS :- contains the starring address of a program's data segment.","$"
            _SS DB 10,"> SS :- contains the starting address of a program's stack segment.","$"
            _ES DB 10,"> ES :- Extra Segment.","$"    
         POI_REG DB 10,"2-POINTER REGISTERS","$"
            _IP DB 10,"> Instruction Pointer (IP) :- contains the offset address of the next",10,"      instruction that is to execute (offset address of code segment).","$"
            _SP DB 10,"> Stack Pointer (SP) :- contains the offset address of the current word being",10,"      processed in the stack (offset address of stack segment).","$"
            _BP DB 10,"> Base Pointer (BP) :- points to the offset address of data moving between",10,"     memory and stack (facilitates referencing parameters in stack), (offset",10,"   address of stack segment) .","$"
        General_REGISTERS DB 10,"3-General-Purpose Registers ","$"        
            _EAX DB 10,"> EAX (32-bit):- the rightmost portion is AX register(16-bit) :- the primary",10,"      accumulator register, is used for operations involving input/output and",10,"       0most arithmetic, divided into AH(high ,8-bit) , AL(low ,8-bit).","$"
            _EBX DB 10,"> EBX (32-bit):- the rightmost portion is BX register(16-bit) :- the base",10,"     address register , that can be used as an index to extend address",10,"     (indirect address) , divided into BH(high ,8-bit) , BL(low ,8-bit).","$"
            _EDX DB 10,"> EDX (32-bit):- the rightmost portion is DX register(16-bit) :- the data",10,"     register, Some input/output operations require its use, and multiply and",10,"     divide operations that involve large numbers,values assume the use of",10,"     DX and AX together as a pair,divided into DH(high ,8-bit),DL(low ,8-bit).","$"
            _ECX DB 10,"> ECX (32-bit):- the rightmost portion is CX register(16-bit) :- the counter",10,"       register, It may contain a value to control the number of times a loop",10,"       is repeated or a value to shift bits left or right, divided into",10,"       CH(high ,8-bit) , CL(low ,8-bit).","$"
         Index_Registers DB 10 ,"4-Index registers ","$"
         INDEX_DEFINE DB 10, "Index registers :- are used to move from one location (source) to",10,"     another location (destination) in memory.","$"
         _SI DB 10,"> Source Index (SI) :- It is used in the pointer addressing of data and as a",10,"     source in some string related operations (offset address of data segment).","$"
             _DI DB 10,"> Destination Index (DI) : It is used in the pointer addressing of data and as",10,"      a destination in some string related operations (offset address of",10,"     data segment).","$"
                FLAG DB 10,"5-FLAG REGISTER","$" 
                FLAG_DEFINE DB 10,"FLAG REGISTER :-is modified automatically by CPU after",10," mathematical operations, this allows to determine the type of the result or transfer the control to another part of the program."
            _CF DB 10,"> contains carries from a leftmost bit following an arithmetic operation, carry (Find end carry set CF=1 , no end carry CF=0)."
            _PF DB 10,"> PF (Parity Flag) :- Indicates the number of bits with a value of 1 that result from an operation , an even number of bits causes so called even parity and an odd number causes odd parity."  
            _AF DB 10,"> AF (Auxiliary Carry) :- contains a carry out of bit 3 into bit 4 in an arithmetic operation.","$"
            _ZF DB 10,"> ZF (Zero Flag) :- indicates the result of an arithmetic or comparison operation (0=nonzero, 1=zero).","$"
            _SF DB 10,"> SF (Sign Flag) :- contains the result sign of an arithmetic operation (0= positive , 1=negative ).","$"
            _TF DB 10,"> TF (Trace Flag) :- permits operation of the processor in single-step mode(you can step through execution a single instruction at a time to examine the effect on registers and memory).","$"
            IF_ DB 10,"> IF (Interrupt Flag) :- indicates to all external interrupts (input operations) , are to be processed or ignored.","$"
            _DF DB 10,"> DF (Direction Flag) :- controls the left-to-right or right-to-left direction of string processing.", "$"
            _OF DB 10,"> OF (Overflow Flag) :- indicate when an arithmetic overflow has occurred in an operation." , "$"    
   ;INTEL 8086 DEFINETION
   INTEL8086  DB  10, "4-INTTRO TO INTEL 8086:","$"
   INTEL_DEFINE DB 10,"INTEL 8086:- Processor stores the data in memory in reverse byte sequence",10,"      the least significant byte occurs at the first memory address.The most",10,"       significant byte occurs at the second memory address.","$"         
            
   ;INTERUPTS         
   INTERRUPTS DB 10,"5-INTERRUPTS:","$"
   INTER_DEFINE DB 10, "INTERRUPT HANDLING :- The system provides means for programs to access",10,"      external devices by means of interrupts.","$"
   Disp_STRING DB 10,"> Displaying string :- Function Code = 09H , INT 21h,Get the ASCII code stored",10,"      in DX register.","$"
        DISP_CHAR DB 10,"> Displaying char :- Function Code = 02h , INT 21h,Get the ASCII code stored        in DL register.","$"
        Read_chr DB 10,"> Reading char :- Function Code = 01 , INT 21h,The ascii code of the entered",10,"      char goes to AL register","$"
        BCK DB 10,"6-BACK","$"
        EXI DB 10,"0-EXIT","$"
        ERRO DB 10,"Invalid input","$"
   
   

.CODE
    MAIN PROC FAR
        .STARTUP
        ;MAIN INTERFACE
        BK:
        CALL NEW_INTERFACE 
        
        LEA DX,W
        CALL PRINT_STRING
        
        LEA DX,Q
        CALL PRINT_STRING
        
        LEA DX,INTRO
        CALL PRINT_STRING        
        
        LEA DX,MEMORY_SEG
        CALL PRINT_STRING        
        
        LEA DX,REGISTERS
        CALL PRINT_STRING        
        
        LEA DX,INTEL8086
        CALL PRINT_STRING
        
        LEA DX,INTERRUPTS
        CALL PRINT_STRING       
        
        lea dx,EXI
        CALL PRINT_STRING
        ;READ & CHECK INPUT
        CALL READ_CHAR       
        CMP AL,49
        JE OP_one
        CMP AL,50
        JE OP_TWO
        CMP AL,51
        JZ OP_THREE
        CMP AL,52
        JE OP_FOUR
        CMP AL,53
        JE OP_FIVE
        CMP AL,48
        JE ex
        jmp error        
        op_one:
            call op1            
        
        OP_TWO:
            CALL OP2        
        
        OP_THREE:
            CALL OP3            
        
        OP_FOUR:
            CALL OP4
        
        OP_FIVE:
            CALL OP5
        
        error:
            CALL _ERROR
                
            EX:    
        .EXIT

    MAIN ENDP    
    ;fixed in any interface--------------------------------------
    theEnd proc near 
       lea dx,BCK
       CALL PRINT_STRING 
       
       lea dx,EXI
       CALL PRINT_STRING
       ;check input
       CALL readChar_hidden
       cmp al,54
       je BK
       cmp al,48
       je EX 
       jmp error       
   theEnd endp
   ;print string to user-------------------------------------------  
   PRINT_STRING proc near ;READ FROM DX
        MOV AH,09H
        INT 21H
        RET
   PRINT_STRING ENDP  
 ;Delete old content from interface ---------------------------------------------  
   NEW_INTERFACE proc near
        MOV AH,00H
        MOV AL,03H
        INT 10H
        RET
    NEW_INTERFACE ENDP 
  ;read char from user---------------------------------------------  
    READ_CHAR proc near;STORE IN AL 
        MOV AH,01H
        INT 21H
        RET
    READ_CHAR ENDP 
 ;read char hidden from user -----------------------------------------------
 readChar_hidden proc near
        MOV AH,08H
        INT 21H
        RET
    readChar_hidden ENDP
 ; opption 1----------------------------------------------------------  
    OP1 proc near
        CALL NEW_INTERFACE
        
        LEA DX,Assembly_lan
        CALL PRINT_STRING
        
        LEA DX,Computer_structure
        CALL PRINT_STRING
        ;check input
        CALL READ_CHAR
        CMP AL,49
        JE OPP1
        CMP AL,50
        JE OPP2
        JMP ERROR      
              OPP1:
                 CALL NEW_INTERFACE
                 
                 LEA DX,Assemb_DEF
                 CALL PRINT_STRING
                 
                 CALL THEEND              
              OPP2:
                  CALL NEW_INTERFACE
                  
                  LEA DX,struc_DEF
                  CALL PRINT_STRING                 
                  
                  LEA DX,CPU
                  CALL PRINT_STRING                  
                  
                  LEA DX,CU
                  CALL PRINT_STRING                  
                  
                  LEA DX,ALU
                  CALL PRINT_STRING                 
                  
                  LEA DX,Reg
                  CALL PRINT_STRING                  
                  
                  LEA DX,CPU_intercon
                  CALL PRINT_STRING                  
                  
                  LEA DX,Main_Memory
                  CALL PRINT_STRING                 
                  
                  LEA DX,I_devices
                  CALL PRINT_STRING                  
                  
                  LEA DX,Sys_Bus
                  CALL PRINT_STRING                  
                  
                  LEA DX,Data_bus
                  CALL PRINT_STRING                  
                  
                  LEA DX,Add_bus
                  CALL PRINT_STRING                  
                  
                  LEA DX,Cont_bus
                  CALL PRINT_STRING
                  
                  CALL theEnd                  
    op1 endp  
  ;OPPTION 2--------------------------------------------------------          
    OP2 PROC NEAR
            CALL NEW_INTERFACE            
            
            LEA DX,SEG_DEF
            CALL PRINT_STRING           
            
            LEA DX,CODE_SEG
            CALL PRINT_STRING            
            
            LEA DX,Data_SEG
            CALL PRINT_STRING            
            
            LEA DX,Stack_SEG
            CALL PRINT_STRING
            
            CALL theEnd 
    OP2 ENDP
   ;OPPTION 3-----------------------------------------------------
   OP3 PROC NEAR
                CALL NEW_INTERFACE            
                
                LEA DX,SEG_REGISTERS
                CALL PRINT_STRING                
                
                LEA DX,POI_REG
                CALL PRINT_STRING                
                
                LEA DX,General_REGISTERS
                CALL PRINT_STRING               
                
                LEA DX,Index_Registers
                CALL PRINT_STRING                
                
                LEA DX,FLAG
                CALL PRINT_STRING          
                ;check input
                CALL READ_CHAR
                CMP AL,49
                JE REG1
                CMP AL,50
                JE REG2
                CMP AL,51
                JE REG3
                CMP AL,52
                JE REG4
                CMP AL,53
                JE REG5 
                JMP ERROR              
                REG1:
                     CALL NEW_INTERFACE
                     
                     LEA DX,_CS
                     CALL PRINT_STRING
                     
                     LEA DX,_DS
                     CALL PRINT_STRING
                     
                     LEA DX,_SS
                     CALL PRINT_STRING
                     
                     LEA DX,_ES
                     CALL PRINT_STRING
                     
                     CALL theEnd 
                REG2:
                    CALL NEW_INTERFACE           
                    
                    LEA DX,_IP
                    CALL PRINT_STRING
                    
                    LEA DX,_SP
                    CALL PRINT_STRING
                    
                    LEA DX,_BP
                    CALL PRINT_STRING 
                    
                    CALL theEnd          
                 REG3:
                    CALL NEW_INTERFACE           
                    
                     LEA DX,_EAX
                     CALL PRINT_STRING
                     
                     LEA DX,_EBX
                     CALL PRINT_STRING
                     
                     LEA DX,_EDX
                     CALL PRINT_STRING             
                     
                     LEA DX,_ECX
                     CALL PRINT_STRING
                     
                     CALL theEnd 
                REG4:
                    CALL NEW_INTERFACE            
                    
                    LEA DX,INDEX_DEFINE
                    CALL PRINT_STRING
                     
                     LEA DX,_SI
                     CALL PRINT_STRING            
                     
                     LEA DX,_DI
                     CALL PRINT_STRING
                     
                     CALL theEnd 
                REG5:
                     CALL NEW_INTERFACE
                     
                     LEA DX,FLAG_DEFINE
                     CALL PRINT_STRING
                     
                     LEA DX,_CF
                     CALL PRINT_STRING
                     
                     LEA DX,_PF
                     CALL PRINT_STRING
                     LEA DX,_AF
                     CALL PRINT_STRING
                     
                     LEA DX,_ZF
                     CALL PRINT_STRING
                     
                     LEA DX,_SF
                     CALL PRINT_STRING
                     
                     LEA DX,_TF
                     CALL PRINT_STRING
                     
                     LEA DX,IF_
                     CALL PRINT_STRING
                     
                     LEA DX,_DF
                     CALL PRINT_STRING
                     
                     LEA DX,_OF
                     CALL PRINT_STRING
                     
                     CALL theEnd 
   OP3 ENDP
 ;OPPTION 4------------------------------------------------------------
   OP4 PROC NEAR
            CALL NEW_INTERFACE            
            
            LEA DX,INTEL_DEFINE
            CALL PRINT_STRING
            
            CALL THEEND
   OP4 ENDP
 ;OPPTION 5---------------------------------------------------------
    OP5 PROC NEAR
            CALL NEW_INTERFACE            
            
            LEA DX,INTER_DEFINE
            CALL PRINT_STRING 
            
            LEA DX,Disp_STRING
            CALL PRINT_STRING
            
            LEA DX,DISP_CHAR
            CALL PRINT_STRING
            
            LEA DX,Read_chr
            CALL PRINT_STRING
            
            CALL THEEND
    OP5 ENDP
  ;error-----------------------------------------------------------------
    _error proc near
                CALL NEW_INTERFACE 
                
                LEA DX,ERRO
                CALL PRINT_STRING
                jmp EX
    _error endp 
END MAIN