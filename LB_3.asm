CSEG segment
org 100h
start: 
PUSHA

print WELCOME 
print COMPOS
  
mov cx, MASS_SIZE 
                
mov bx, offset MASS
dec bx 
mov si, offset sign_mass
dec si               
;////////////////////////      INPUT MASS    ////////////////////////////////////            
initialization:
    push cx
    push bx 
    push si
    call input_int  ;input int in number   
    pop si   
    pop bx
    pop cx
  ;  xor bx,bx
    ;xor ax,ax
    
   xor ax,ax 
   ; mov bx, offset MASS
   ; dec bx
   ; mov si, MASS_SIZE
   ; sub si,cx    
   mov ax, number
    mov [bx], ax  
    add bx,2;16
   
   xor ax,ax
   mov ax, sign_number
   mov [si], ax
   
    add si,2;16
  ; mov ax,[si-16] 

loop initialization



;////////////////////               OUTOUT MASS                  ///////////////////////
mov cx, MASS_SIZE
mov bx, offset MASS
dec bx   
print output_str 
   
     
enter: 
xor ax,ax       
    mov  ax,  [bx]
    mov number, ax
    add bx, 2;16
     
    push cx
    push bx
    call output_int 
    pop bx 
    pop cx


    inc count   

    CMP count,COLUMNS
    JE line_new
    JMP not_line_new  




line_new:
    mov count,0
    mov ah,9
	mov dx,offset new_line 
	int 21h
not_line_new: 
loop enter

print COMPOS 
mov cx, COLUMNS
mov count,0
mov comp, 1  

composition:  
                   

        ;xor bx,bx
        ;mov bx,offset comp          
                   
        mov comp, 1
        xor si,si
        
        push cx
        
        mov si, offset sign_mass
        dec si 

        
         
         xor bx,bx
         mov bx, COLUMNS
         sub bx,cx 
         
         mov offset_number, offset MASS
         dec offset_number

         push cx
         mov cx, bx
         CMP cx,0
         JE cx_o
         offset_loop:
         add offset_number, 2;16 
         
         loop offset_loop  
         cx_o:
         pop cx 
         mov bx, offset_number 
        mov ax,[bx] 
        
        mov number, ax   

         
        
        mov cx, ROWS  
        
        mov sign_mass_dd, 0
        MULL: 

push bx 
     
            
            xor ax,ax
             ;mov ax, [si]  
             CMP number,0
             JL neg_number
             JMP not_neg_number
             
             neg_number:
             add sign_mass_dd ,1    
             
            not_neg_number: 
            xor ax,ax 
            xor dx,dx
            xor bx, bx
     
            mov ax, number

            
            
            
            

             mov bx, offset comp
            IMUL comp            ;in ax bx(number)*ax(10)   
          
             
               mov [bx+2], dx 
               mov  [bx] , ax
                 ;mov comp , ax      ;number = number*10  
           ; mov bx, offset comp
            ;mov [bx], dx
            ;mov [bx+2], ax  
            ;   add [bx+1], ax
               
             
               mov  dx,   [bx+2]     
               mov ax,  [bx]
             ;  mov si, comp
           ; mov number, ax
;pop si            
pop bx     
     

        push cx
        mov cx, COLUMNS
        sdvig:
         add bx,2;16
         add si, 2;16
        loop sdvig
        pop cx
        xor ax,ax  
      mov ax, [bx]
       mov number, ax
        
        loop MULL 
        
        
        pop cx
       ; print COMPOS  
       print new_line
        push cx  
        
    call output_int_dd  
    pop cx
    
loop composition

POPA  

macro print str
	mov ah,9
	mov dx,offset new_line 
	int 21h
    mov ah,9
    mov dx,offset str
    int 21h
    
    mov ah,9
	mov dx,offset new_line 
	int 21h
endm    
      
      
; /////////////////////////// PROCEDURE OUTPUT INT START  ////////////////////////////       
output_int proc near 
    pusha
   
     mov sign_number,0
     CMP number, 0
     JE number_zero
     JL out_negative                  ;number <0 
     JMP out_NOT_negative             ;number >0
      
      out_negative:   
            mov sign_number,1  ;number negative
            mov ax, number
            NEG ax
            mov number, ax 
      out_NOT_negative:
            xor ax,ax
            mov cx, 5 
            xor si,si   
            xor dx,dx
            xor bx,bx 
            CMP  sign_number,1
            JE out_neg
            JMP output
      out_neg: 
            mov symbol_out , '-'
            xor ax,ax
            mov ah,9
            mov dx,offset symbol_out 
            int 21h 

    output:   
           xor dx,dx
           xor ax,ax
           CMP number, 0
           JE end_output 
           mov AX,number 
           mov del, 10  
    
           IDIV del  
           mov symbol_out , dl
           mov number, ax 

           add symbol_out, 30h 
           xor bx,bx
           mov bx, offset output_mass
           dec bx ;sub bx
           xor ax,ax
           mov al,symbol_out 
           mov si, cx
           mov [bx][si] ,al
    loop output   
    JMP end_output
    
    
    number_zero:
           mov symbol_out, '0'
           xor ax,ax                                                                                                                                   
           mov ah,9
           mov dx,offset symbol_out  
           int 21h
           mov ah,9
           mov dx,offset new_number 
           int 21h 
           JMP END
    end_output:   
           xor bx,bx
           mov bx, offset output_mass
           mov [bx+5] ,'$'
    
           xor ax,ax                                                                                                                                   
           mov ah,9
           mov dx,offset output_mass
           add dx, cx     ;offset due to the number of elements(0 in begin str)
           int 21h     
         
           mov ah,9
           mov dx,offset new_number 
           int 21h 
    END:        
	popa               
    ret
output_int endp
; /////////////////////////// PROCEDURE OUTPUT INT END  //////////////////////////// 


; /////////////////////////// PROCEDURE INPUT INT START  ////////////////////////////  
  
input_int proc near            ; for model TINY SMALL AND COMPACT
    pusha                      ;All registers in stack 
    mov number, 0
    JMP  start_input
    wrong_input: 
    mov number, 0 
    print error
    start_input: 
        xor ax,ax              
        mov sign_number,0      ;flag for sign number
        mov cx, 6              ; 1 - sign, 5 for symbols
        input:
            xor ax,ax
            
            mov ah,1           ;input symbol
            int 21h
           
            CMP al,32          ;if space
            JE end_input      
           
            CMP al,13          ;if enter
            JE end_input    
           
            CMP al,8           ;if <-
            JNE input_tt
                    inc cx     ;-symbol  
                    CMP number,0 ;if number==0 =>beggin input
                    JE start_input
                    xor ax,ax
                    xor dx,dx
                    mov ax,number 
                    mov del, 10  
    
                    IDIV del     ; number/10
                    mov number, ax  ; number/10 without residue
                    JMP input
            input_tt:      
           
            CMP al,45          ; '-' 
            JNE not_sign       
            CMP cx, 6          ;if at the beginning of the line
            JNE wrong_input
            inc sign_number    ;the number is negative
            jmp sign 
          
            not_sign:
            CMP al, 48         ;symbol <'0'
            JL wrong_input
           
            CMP al, 57         ;symbol >'9'
            JG wrong_input    
            
            CMP sign_number,0  ;check overflow
            JE int_not_neg
            JMP int_neg
            
            int_not_neg:
                   CMP cx, 1
                   JE wrong_input
                   CMP cx,2          ;there is no sign '-' => cx=2
                   JE overflow       ;can be overflow, need check
                   JMP not_overflow  ; can't be overflow
            int_neg:
                   CMP cx, 1         ;there is sign '-' => cx=1
                   JE overflow       ;can be overflow, need check
                   JMP not_overflow  ; can't be overflow
            
            overflow:
                   CMP number, 3276
                   JLE not_overflow   ;< => you will notice an overflow when comparing (does not go out of register)
                   print overflow_str   ;can go out of register
                   JMP wrong_input
            not_overflow:
            xor bx,bx                
            mov symbol, al     ;symbol in number    
            sub symbol, 30h 
            xor ax,ax 
            mov al,10       
            mov bx, number
            IMUL bx            ;in ax bx(number)*ax(10)
            mov number,ax      ;number = number*10

            xor ax,ax  
            mov al,  symbol
            add number, ax     ;number += symbol
            sign:     
           
        loop input 
        end_input:   
            
        CMP sign_number, 0
        JE not_neg_int             ;not negative number
        JMP neg_int                ;negative number
        
        not_neg_int: 
           mov ax, number
             
           CMP ax , 7FFFh     ;if number <=32767
           JNA end_check_overflow   ;if not higher
           print overflow_str 
           JMP wrong_input 
            
        neg_int:
           CMP number , 8000h      ;if number >= - 32768
           JNA end_check_overflow  ;if if not higher 
           print overflow_str 
           JMP wrong_input
               
        end_check_overflow:
        
            CMP sign_number, 1
            JE int_negative
            JMP int_not_negative
       int_negative: 
            mov ax, number
            NEG ax
            mov number, ax 
       
       int_not_negative:   
            popa  
    ret
input_int endp
; /////////////////////////// PROCEDURE INPUT INT END  ////////////////////////////        



; /////////////////////////// PROCEDURE _dd OUTPUT INT START  ////////////////////////////       
output_int_dd proc near 
    pusha  
    mov bx, offset comp 
    xor ax,ax
    xor dx,dx
    
        mov ax, [bx]  
       mov dx, [bx+2]
       CMP ax,0
     JE number_zero_dd_mb
     ;JL out_negative_dd
     JMP check_sign   
     number_zero_dd_mb:
     CMP dx,0
     JE number_zero_dd
     
     check_sign:
     xor ax,ax
     mov sign_number,0 
     mov  ax, sign_mass_dd
     CMP ax, 0
     JE out_NOT_negative_dd
     CMP ax,2
     JE out_NOT_negative_dd
     CMP ax, 4
     JE out_NOT_negative_dd
     
     
    ; JL out_negative_dd
                    ;number <0 
     JMP out_negative_dd             ;number >0
     
     
      
      out_negative_dd:     
      
            CMP dx,0
            JG  out_NOT_negative_dd
            mov sign_number,1  ;number negative   
            
            
             mov bx, offset comp
            mov ax, [bx]   
            NEG ax 
            mov [bx], ax   
            
           CMP dx, 0FFFFh
            JE neg_n
            mov dx,[bx+2]   
            
          
            NEG dx  
            dec dx  
               mov  [bx+2], dx
               JMP out_NOT_negative_dd
               neg_n:
               mov dx,0
               mov [bx+2],dx
               
      out_NOT_negative_dd:
            xor ax,ax
            mov cx, 10 
            xor si,si   
            xor dx,dx
            xor bx,bx 
            CMP  sign_number,1
            JE out_neg_dd
            JMP output_dd
      out_neg_dd: 
            mov symbol_out , '-'
            xor ax,ax
            mov ah,9
            mov dx,offset symbol_out 
            int 21h 

    output_dd:   
           xor dx,dx
           xor ax,ax
           xor bx,bx 
             mov si, offset comp
            mov ax, [si]
            mov dx,[si+2]  
             
            CMP ax,0
            JE end_output_dd_mb    
           JMP not_end_output_dd_mb
           end_output_dd_mb:
            CMP dx,0
           JE end_output_dd   
            CMP dx, 0FF00h
            JE neg_n2          
             JMP not_end_output_dd_mb
            
            neg_n2 :  
            
            
            
            mov dx,0
           not_end_output_dd_mb: 
           
            CMP dx, 0Fh
            JG error_overflow 
            CMP dx, 0FF00h
            JG error_overflow 
           mov del_dd, 10  
                   
           IDIV del_dd  
           mov symbol_out , dl
           mov [si], ax
           mov [si+2],0 
           ;CMP dx, 9
           ;JG 
           
           
           add symbol_out, 30h 
           xor bx,bx
           mov bx, offset output_mass_dd
           dec bx ;sub bx
           xor ax,ax
           mov al,symbol_out
           xor si, si 
           mov si, cx
           mov [bx][si] ,al
    loop output_dd   
    JMP end_output_dd
    
    
    number_zero_dd:
           mov symbol_out, '0'
           xor ax,ax                                                                                                                                   
           mov ah,9
           mov dx,offset symbol_out  
           int 21h
           mov ah,9
           mov dx,offset new_number 
           int 21h 
           JMP END_dd
    end_output_dd:   
           xor bx,bx
           mov bx, offset output_mass_dd
           mov [bx+10] ,'$'
    
           xor ax,ax                                                                                                                                   
           mov ah,9
           mov dx,offset output_mass_dd
           add dx, cx     ;offset due to the number of elements(0 in begin str)
           int 21h     
         
           mov ah,9
           mov dx,offset new_number 
           int 21h 
    END_dd:        
    JMP not_error_overflow
    error_overflow :
     print error_overflow1
     not_error_overflow:      
	popa               
    ret
output_int_dd endp
  






ROWS EQU 5
COLUMNS EQU 6
MASS_SIZE EQU 30
MASS_SIZEE dw 18 
                                  h1                              db '$'
h2    db '$'
h3           db '$' db '$'
h4                         db '$'
h5                                db '$'
h6    db '$'
h7           db '$'
h8                  db '$'
h9                         db '$'
h10                               db '$'
h11   db '$'
h12          db '$'
h13                 db '$'
h14                        db '$'
h15                               db '$'
h16   db '$'
h17          db '$'
h18                 db '$'
h19                        db '$'
h20                               db '$'
h21    db '$'
h22           db '$'
h23                  db '$'
h24                         db '$'
h25                                db '$'
h26   db '$'
h27          db '$'
h28                 db '$'
h29                        db '$'
h30                               db '$'
MASS dw DUP MASS_SIZE (0) 



n1  db '$'
n2   db '$'
n3          db '$'
n4                 db '$'
n5                        db '$'
n6                               db '$'
n7  db '$'
n8         db '$'
n9                db '$'
n10                      db '$'
n11                             db '$'
n12   db '$'
n13          db '$'
n14                 db '$'
n15                        db '$'
n16                               db '$'
n17    db '$'
n18           db '$'
n19                  db '$'
n20                         db '$'
n21                                db '$'
n22   db '$'
n23          db '$'
n24                 db '$'
n25                        db '$'
n26                               db '$'
n27  db '$'
n28         db '$'
n29                db '$'
n30                       db '$'
p1                              db '$'
p2    db '$'
p3           db '$' db '$'
p4                         db '$'
p5                                db '$'
p6    db '$'
p7           db '$'
p8                  db '$'
p9                         db '$'
p10                               db '$'
p11   db '$'
p12          db '$'
p13                 db '$'
p14                        db '$'
p15                               db '$'
p16   db '$'
p17          db '$'
p18                 db '$'
p19                        db '$'
p20                               db '$'
p21    db '$'
p22           db '$'
p23                  db '$'
p24                         db '$'
p25                                db '$'
p26   db '$'
p27          db '$'
p28                 db '$'
p29                        db '$'
p30                               db '$'
     


output_mass db '0', '0', '0', '0', '0', '$', '$'
output_mass_dd db '0', '0', '0', '0', '0', '0', '0', '0', '0', '$', '$'
          

sign_mass dw DUP MASS_SIZE (0)
sign_mass_dd dw 0     




del dw 1
del_dd dw 1
number dw ?   
sign_number dw 0
   
;mass dw DUP 30 (?) 

check dd 0

m14                        db '$'
m15                               db '$'
m16   db '$'
m17          db '$'
m18                 db '$'
m19                        db '$'
m20                               db '$'
m21    db '$'
m22           db '$'
m23                  db '$'
m24                         db '$'
m25                                db '$'
m26   db '$'
m27          db '$'
m28                 db '$'
m29                        db '$'
m30                               db '$'
     


output_str  db 'Your number: $'

offset_number dw 0  





 
COMPOS db 'Compositions:', 13, 10,'$'
overflow_str db 'Overflow! You can enter only integer from -32768 to 32767. Please, try again...$'
new_line db 13,10,'$'  
;MASS dw DUP MASS_SIZE (0) 
;COMPOSITION dd DUP 6 (?)
comp dd 1   
symbol db ?
   

error db 'Wrong input!!!You can enter only integer from -32768 to 32767',13,10,'Please, try enter integer again...$'

symbol_out db  ?,'$' 
WELCOME db 'Welcome! Input 30 elements:.../nYou can enter only integer from -32768 to 32767.$' 
count db 0 
error_overflow1 db 'Composition is too much for ax register. Sorry :)$'
new_number db 9,32, '$'   
CSEG ends
end start     
