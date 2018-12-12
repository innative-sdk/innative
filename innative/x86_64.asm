.CODE

     GetRSPValue PROC
     mov rax, rsp
     add rax, 8
     ret
     GetRSPValue ENDP
     END