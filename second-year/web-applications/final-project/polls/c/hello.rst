                                      1 ;--------------------------------------------------------
                                      2 ; File Created by SDCC : free open source ANSI-C Compiler
                                      3 ; Version 4.2.0 #13081 (Mac OS X x86_64)
                                      4 ;--------------------------------------------------------
                                      5 	.module hello
                                      6 	.optsdcc -mmcs51 --model-small
                                      7 	
                                      8 ;--------------------------------------------------------
                                      9 ; Public variables in this module
                                     10 ;--------------------------------------------------------
                                     11 	.globl _main
                                     12 	.globl _printf
                                     13 ;--------------------------------------------------------
                                     14 ; special function registers
                                     15 ;--------------------------------------------------------
                                     16 	.area RSEG    (ABS,DATA)
      000000                         17 	.org 0x0000
                                     18 ;--------------------------------------------------------
                                     19 ; special function bits
                                     20 ;--------------------------------------------------------
                                     21 	.area RSEG    (ABS,DATA)
      000000                         22 	.org 0x0000
                                     23 ;--------------------------------------------------------
                                     24 ; overlayable register banks
                                     25 ;--------------------------------------------------------
                                     26 	.area REG_BANK_0	(REL,OVR,DATA)
      000000                         27 	.ds 8
                                     28 ;--------------------------------------------------------
                                     29 ; internal ram data
                                     30 ;--------------------------------------------------------
                                     31 	.area DSEG    (DATA)
                                     32 ;--------------------------------------------------------
                                     33 ; overlayable items in internal ram
                                     34 ;--------------------------------------------------------
                                     35 ;--------------------------------------------------------
                                     36 ; Stack segment in internal ram
                                     37 ;--------------------------------------------------------
                                     38 	.area	SSEG
      000041                         39 __start__stack:
      000041                         40 	.ds	1
                                     41 
                                     42 ;--------------------------------------------------------
                                     43 ; indirectly addressable internal ram data
                                     44 ;--------------------------------------------------------
                                     45 	.area ISEG    (DATA)
                                     46 ;--------------------------------------------------------
                                     47 ; absolute internal ram data
                                     48 ;--------------------------------------------------------
                                     49 	.area IABS    (ABS,DATA)
                                     50 	.area IABS    (ABS,DATA)
                                     51 ;--------------------------------------------------------
                                     52 ; bit data
                                     53 ;--------------------------------------------------------
                                     54 	.area BSEG    (BIT)
                                     55 ;--------------------------------------------------------
                                     56 ; paged external ram data
                                     57 ;--------------------------------------------------------
                                     58 	.area PSEG    (PAG,XDATA)
                                     59 ;--------------------------------------------------------
                                     60 ; external ram data
                                     61 ;--------------------------------------------------------
                                     62 	.area XSEG    (XDATA)
                                     63 ;--------------------------------------------------------
                                     64 ; absolute external ram data
                                     65 ;--------------------------------------------------------
                                     66 	.area XABS    (ABS,XDATA)
                                     67 ;--------------------------------------------------------
                                     68 ; external initialized ram data
                                     69 ;--------------------------------------------------------
                                     70 	.area XISEG   (XDATA)
                                     71 	.area HOME    (CODE)
                                     72 	.area GSINIT0 (CODE)
                                     73 	.area GSINIT1 (CODE)
                                     74 	.area GSINIT2 (CODE)
                                     75 	.area GSINIT3 (CODE)
                                     76 	.area GSINIT4 (CODE)
                                     77 	.area GSINIT5 (CODE)
                                     78 	.area GSINIT  (CODE)
                                     79 	.area GSFINAL (CODE)
                                     80 	.area CSEG    (CODE)
                                     81 ;--------------------------------------------------------
                                     82 ; interrupt vector
                                     83 ;--------------------------------------------------------
                                     84 	.area HOME    (CODE)
      000000                         85 __interrupt_vect:
      000000 02 00 06         [24]   86 	ljmp	__sdcc_gsinit_startup
                                     87 ;--------------------------------------------------------
                                     88 ; global & static initialisations
                                     89 ;--------------------------------------------------------
                                     90 	.area HOME    (CODE)
                                     91 	.area GSINIT  (CODE)
                                     92 	.area GSFINAL (CODE)
                                     93 	.area GSINIT  (CODE)
                                     94 	.globl __sdcc_gsinit_startup
                                     95 	.globl __sdcc_program_startup
                                     96 	.globl __start__stack
                                     97 	.globl __mcs51_genXINIT
                                     98 	.globl __mcs51_genXRAMCLEAR
                                     99 	.globl __mcs51_genRAMCLEAR
                                    100 	.area GSFINAL (CODE)
      00005F 02 00 03         [24]  101 	ljmp	__sdcc_program_startup
                                    102 ;--------------------------------------------------------
                                    103 ; Home
                                    104 ;--------------------------------------------------------
                                    105 	.area HOME    (CODE)
                                    106 	.area HOME    (CODE)
      000003                        107 __sdcc_program_startup:
      000003 02 00 62         [24]  108 	ljmp	_main
                                    109 ;	return from main will return to caller
                                    110 ;--------------------------------------------------------
                                    111 ; code
                                    112 ;--------------------------------------------------------
                                    113 	.area CSEG    (CODE)
                                    114 ;------------------------------------------------------------
                                    115 ;Allocation info for local variables in function 'main'
                                    116 ;------------------------------------------------------------
                                    117 ;	hello.c:5: int main()
                                    118 ;	-----------------------------------------
                                    119 ;	 function main
                                    120 ;	-----------------------------------------
      000062                        121 _main:
                           000007   122 	ar7 = 0x07
                           000006   123 	ar6 = 0x06
                           000005   124 	ar5 = 0x05
                           000004   125 	ar4 = 0x04
                           000003   126 	ar3 = 0x03
                           000002   127 	ar2 = 0x02
                           000001   128 	ar1 = 0x01
                           000000   129 	ar0 = 0x00
                                    130 ;	hello.c:9: printf("Hello World");
      000062 74 07            [12]  131 	mov	a,#___str_0
      000064 C0 E0            [24]  132 	push	acc
      000066 74 08            [12]  133 	mov	a,#(___str_0 >> 8)
      000068 C0 E0            [24]  134 	push	acc
      00006A 74 80            [12]  135 	mov	a,#0x80
      00006C C0 E0            [24]  136 	push	acc
      00006E 12 00 A3         [24]  137 	lcall	_printf
      000071 15 81            [12]  138 	dec	sp
      000073 15 81            [12]  139 	dec	sp
      000075 15 81            [12]  140 	dec	sp
                                    141 ;	hello.c:11: return 0;
      000077 90 00 00         [24]  142 	mov	dptr,#0x0000
                                    143 ;	hello.c:12: }
      00007A 22               [24]  144 	ret
                                    145 	.area CSEG    (CODE)
                                    146 	.area CONST   (CODE)
                                    147 	.area CONST   (CODE)
      000807                        148 ___str_0:
      000807 48 65 6C 6C 6F 20 57   149 	.ascii "Hello World"
             6F 72 6C 64
      000812 00                     150 	.db 0x00
                                    151 	.area CSEG    (CODE)
                                    152 	.area XINIT   (CODE)
                                    153 	.area CABS    (ABS,CODE)
