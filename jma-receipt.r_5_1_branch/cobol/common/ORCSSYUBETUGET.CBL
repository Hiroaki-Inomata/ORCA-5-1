      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSSYUBETUGET.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¼ïÊÌÊÖµÑ½èÍý
      *  ´ÉÍý¼Ô            : 
      *  06/10/06    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.02.01    NACL-Æ£¸¶    08/03/25  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *
      *  04.03.01    NACL-Æ£¸¶    08/05/07  ¸øÈñ£´¼ï¤Þ¤Ç¤ÎÊ»ÍÑ¤Îµ­Ï¿ÂÐ±þ
      *  04.03.02    NACL-Æ£¸¶    08/12/01  ¼«Èñ¤Î¼ïÊÌÀßÄê¾ò·ï¤Î½¤Àµ
      *
      *  04.04.01    NACL-Æ£¸¶    09/02/26  Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤Ë£·£µºÐÅþ
      *                                     Ã£»þ¤Ë³¤³°¤Ë½»½ê¤òÍ­¤¹¤ë¾ì¹ç
      *                                     ¤ÎÂÐ±þ
      *
      *  04.05.01    NACL-Æ£¸¶    10/01/05  ¼£¸³ÂÐ±þ
      *                                     Ê¿À®£²£°Ç¯£´·îÊ¬¤è¤ê¼«Èñ¥ì¥»
      *                                     ¤È¤¹¤ë
      *
      *  04.07.01    NACL-Æ£¸¶    14/03/30  Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
      *  04.07.02    NACL-Æ£¸¶    14/05/20  Âè»°¼Ô¹Ô°ÙÂÐ±þ
      *  04.07.03    NACL-Æ£¸¶    14/08/18  £·£µºÐ°Ê¾å¤Î³¤³°µï½»¼Ô£²³äÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(03). 
      *    ¥ì¥»¥×¥È¼ïÊÌÈ½ÄêÍÑÎÎ°è
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO               PIC X(01).
           03  WK-IHO                  PIC X(01).
           03  WK-RJN                  PIC X(01).
           03  WK-KOHI-CNT             PIC 9(02).
           03  WK-HONKZKKBN            PIC X(01).
      *
       01  WRK-CONS-AREA.
      *    Ï«ºÒÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    ¼«ÇåÀÕÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    ¸ø³²ÊÝ¸±ÈÖ¹æ 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975".
      *    Âè»°¼Ô¹Ô°Ù
           03  WRK-CONS-DAISANSYA-HKNNUM
                                       PIC X(03)   VALUE   "970".
      *
           03  WRK-CONS-SRYYM-200209   PIC 9(06)   VALUE   200209.      
           03  WRK-CONS-SRYYM-200609   PIC 9(06)   VALUE   200609.      
           03  WRK-CONS-SRYYM-200803   PIC 9(06)   VALUE   200803.      
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
           03  WRK-CONS-SRYYM-201404   PIC 9(06)   VALUE   201404.      
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSYUBETUGET.INC".
      *****************************************************************
     *
       PROCEDURE                    DIVISION    USING
                                    ORCSSYUBETUGETAREA.
     *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      **   DISPLAY "ORCSSYUBETUGET START"
      **   DISPLAY "SYOFLG : " LNK-SYUBETUGET-SYOFLG
      *
           INITIALIZE                      LNK-SYUBETUGET-OT
           .
       100-INIT-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
      *    ¾ò·ïÀßÄê½èÍý
           IF      LNK-SYUBETUGET-SYOFLG   =   1
               PERFORM 2001-JYOKEN-HENSYU-SEC
               IF      LNK-SYUBETUGET-RECESYUBETU
                                               >   0
                   MOVE    LNK-SYUBETUGET-RECESYUBETU
                                        TO  LNK-SYUBETUGET-RECESYUBETU-Y 
                   GO  TO  200-MAIN-EXT
               END-IF
           END-IF
      *
      **   DISPLAY "SRYYM     : " LNK-SYUBETUGET-SRYYM
      **   DISPLAY "HKNNUM    : " LNK-SYUBETUGET-HKNNUM
      **   DISPLAY "HONKZKKBN : " LNK-SYUBETUGET-HONKZKKBN 
      **   DISPLAY "NYUGAIKBN : " LNK-SYUBETUGET-NYUGAIKBN 
      **   DISPLAY "IHO       : " LNK-SYUBETUGET-IHO 
      **   DISPLAY "KOKUHO    : " LNK-SYUBETUGET-KOKUHO 
      **   DISPLAY "RJN       : " LNK-SYUBETUGET-RJN 
      **   DISPLAY "KOHI-CNT  : " LNK-SYUBETUGET-KOHI-CNT
      **   DISPLAY "BIRTHDAY  : " LNK-SYUBETUGET-BIRTHDAY
      *
      *    Ê¿À®£±£´Ç¯£¹·î¤Þ¤Ç
           IF      LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-200209
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1311     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1312     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1321     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1322     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1331     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1332     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
      *    Ê¿À®£±£´Ç¯£±£°·î²þÀµÂÐ±þ
           IF    ( LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-200209 )
           AND   ( LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-200609 )
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
      *        ÊÝ¸±Ã±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ¸øÈñÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
      *       £²¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
      *       £³¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Ï·¿ÍÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Ï·¿Í¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Ï·¿Í¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Âà¿¦¼ÔÃ±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1419     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Âà¿¦¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420     TO  LNK-SYUBETUGET-RECESYUBETU
      *       Âà¿¦¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
      *    Ê¿À®£±£¸Ç¯£±£°·î²þÀµÂÐ±þ
           IF    ( LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-200609 )
           AND   ( LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-200803 )
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
      *        ÊÝ¸±Ã±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
      *ÊÝ¸±¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸øÈñÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
      *£²¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
      *£³¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
      *Ï·¿ÍÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
      *Ï·¿Í¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
      *Ï·¿Í¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "7"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "7"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼ÔÃ±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1419     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
      *    Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
           IF    ( LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-200803 )
           AND   ( LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-201404 )
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
      *        ÊÝ¸±Ã±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
      *ÊÝ¸±¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£³¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1141     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1141     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1142     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1142     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1143     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1143     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1144     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1144     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1145     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1145     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1146     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1146     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1147     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1147     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1148     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1148     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1149     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1149     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1140     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1140     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£´¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1151     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1151     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1152     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1152     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1153     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1153     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1154     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1154     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1155     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1155     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1156     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1156     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1157     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1157     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1158     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1158     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1159     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1159     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1150     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1150     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸øÈñÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
      *£²¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
      *£³¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
      *£´¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   4
                   MOVE    1241     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   4
                   MOVE    1242     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼ÔÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£³¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   3
                   MOVE    1347     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   3
                   MOVE    1348     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   3
                   MOVE    1349     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   3
                   MOVE    1340     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£´¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   4
                   MOVE    1357     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   4
                   MOVE    1358     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   4
                   MOVE    1359     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   4
                   MOVE    1350     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼ÔÃ±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£³¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1441     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1442     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1443     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1444     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1445     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1446     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£´¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1451     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1452     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1453     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1454     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1455     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1456     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
      *    Ê¿À®£²£¶Ç¯£´·î²þÀµÂÐ±þ
           IF    ( LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-201404 )
             EVALUATE  LNK-SYUBETUGET-HONKZKKBN ALSO
                                    LNK-SYUBETUGET-NYUGAIKBN  ALSO
                       LNK-SYUBETUGET-IHO  ALSO
                                    LNK-SYUBETUGET-KOKUHO     ALSO
                       LNK-SYUBETUGET-RJN  ALSO
                                    LNK-SYUBETUGET-KOHI-CNT
      *        ÊÝ¸±Ã±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1117     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1118     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1127     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1128     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120     TO  LNK-SYUBETUGET-RECESYUBETU
      *ÊÝ¸±¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1137     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1138     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£³¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1141     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1141     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1142     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1142     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1143     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1143     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1144     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1144     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1145     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1145     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1146     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1146     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1147     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1147     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1149     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1147     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1148     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1148     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1140     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1148     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1149     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1149     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  3
                   MOVE    1140     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  3
                   MOVE    1140     TO  LNK-SYUBETUGET-RECESYUBETU
      *        ÊÝ¸±¤È¸øÈñ£´¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1151     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1151     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1152     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1152     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1153     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1153     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1154     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1154     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1155     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1155     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1156     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1156     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1157     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1157     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1159     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1157     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1158     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1158     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   IF    ( LNK-SYUBETUGET-BIRTHDAY <   19440402 )
                   AND   ( LNK-SYUBETUGET-RJNPAYKBN
                                                   =   "99"     )
                       MOVE    1150     TO  LNK-SYUBETUGET-RECESYUBETU
                   ELSE
                       MOVE    1158     TO  LNK-SYUBETUGET-RECESYUBETU
                   END-IF
               WHEN    "7"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1159     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1159     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  4
                   MOVE    1150     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "7"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  4
                   MOVE    1150     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸øÈñÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212     TO  LNK-SYUBETUGET-RECESYUBETU
      *£²¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222     TO  LNK-SYUBETUGET-RECESYUBETU
      *£³¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232     TO  LNK-SYUBETUGET-RECESYUBETU
      *£´¼ï¤Î¸øÈñÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   4
                   MOVE    1241     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   4
                   MOVE    1242     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼ÔÃ±ÆÈ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   ZERO
                   MOVE    1319     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   ZERO
                   MOVE    1310     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   1
                   MOVE    1327     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   1
                   MOVE    1328     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   1
                   MOVE    1329     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   1
                   MOVE    1320     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   2
                   MOVE    1337     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   2
                   MOVE    1338     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   2
                   MOVE    1339     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   2
                   MOVE    1330     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£³¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   3
                   MOVE    1347     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   3
                   MOVE    1348     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   3
                   MOVE    1349     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   3
                   MOVE    1340     TO  LNK-SYUBETUGET-RECESYUBETU
      *¸å´ü¹âÎð¼Ô¤È¸øÈñ£´¼ï¤ÎÊ»ÍÑ
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   4
                   MOVE    1357     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "9"    ALSO   4
                   MOVE    1358     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   4
                   MOVE    1359     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "3"    ALSO  "7"    ALSO   4
                   MOVE    1350     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼ÔÃ±ÆÈ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£±¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£²¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£³¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1441     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1442     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1443     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1444     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1445     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    3
                   MOVE    1446     TO  LNK-SYUBETUGET-RECESYUBETU
      *Âà¿¦¼Ô¤È¸øÈñ£´¼ï¤ÎÊ»ÍÑ
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1451     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1452     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1453     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1454     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1455     TO  LNK-SYUBETUGET-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO    4
                   MOVE    1456     TO  LNK-SYUBETUGET-RECESYUBETU
             END-EVALUATE
           END-IF
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                        TO  LNK-SYUBETUGET-RECESYUBETU-Y 
      *    Ê¿À®£²£°Ç¯£´·î°Ê¹ß¸øÈñ£³Ê»¡¦£´Ê»¤Î¤È¤­
           IF      LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-200803
               EVALUATE    LNK-SYUBETUGET-RECESYUBETU-Y (3:1)
                   WHEN    "4"
                   WHEN    "5"
                       MOVE    "3"              TO
                                      LNK-SYUBETUGET-RECESYUBETU-Y (3:1)
               END-EVALUATE
           END-IF
      *     
      *    ÆÃÊÌÎÅÍÜÈñ¤Î¤È¤­¤Î¥ì¥»¥×¥È¼ïÊÌ¤òÊÔ½¸
           IF      LNK-SYUBETUGET-HKNNUM        =   "068"   OR  "069"
                                                OR  "040"
               MOVE    LNK-SYUBETUGET-RECESYUBETU-Y
                                        TO  LNK-SYUBETUGET-RECESYUBETU-X 
               MOVE    6666             TO  LNK-SYUBETUGET-RECESYUBETU    
           END-IF
           .
        200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾ò·ïÀßÄê½èÍý
      *****************************************************************
       2001-JYOKEN-HENSYU-SEC          SECTION.
      *
      **  DISPLAY "SRYYM          " LNK-SYUBETUGET-SRYYM
      **  DISPLAY "HKNNUM         " LNK-SYUBETUGET-HKNNUM
      **  DISPLAY "NYUGAIKBN      " LNK-SYUBETUGET-NYUGAIKBN 
      **  DISPLAY "HONKZKKBN      " LNK-SYUBETUGET-HKN-HONKZKKBN
      **  DISPLAY "HOJOKBN        " LNK-SYUBETUGET-HKN-HOJOKBN
      **  DISPLAY "HKNKOHSBTKBN   " LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
      **  DISPLAY "AGE            " LNK-SYUBETUGET-AGE 
      **  DISPLAY "SYUGAKUKBN     " LNK-SYUBETUGET-SYUGAKUKBN
      **  DISPLAY "PREFNUM        " LNK-SYUBETUGET-PREFNUM
      **  DISPLAY "KOHNUM     (1) " LNK-SYUBETUGET-KOHNUM     (1)
      **  DISPLAY "RECESKYKBN (1) " LNK-SYUBETUGET-RECESKYKBN (1)
      **  DISPLAY "KOHNUM     (2) " LNK-SYUBETUGET-KOHNUM     (2)
      **  DISPLAY "RECESKYKBN (2) " LNK-SYUBETUGET-RECESKYKBN (2)
      **  DISPLAY "KOHNUM     (3) " LNK-SYUBETUGET-KOHNUM     (3)
      **  DISPLAY "RECESKYKBN (3) " LNK-SYUBETUGET-RECESKYKBN (3)
      **  DISPLAY "KOHNUM     (4) " LNK-SYUBETUGET-KOHNUM     (4)
      **  DISPLAY "RECESKYKBN (4) " LNK-SYUBETUGET-RECESKYKBN (4)
      **  DISPLAY "HKNKBN         " LNK-SYUBETUGET-HKNKBN
      **  DISPLAY "SAIGAIKBN      " LNK-SYUBETUGET-SAIGAIKBN
      **  DISPLAY "RJNHKNNUM      " LNK-SYUBETUGET-RJNHKNNUM
      **  DISPLAY "RJNPAYKBN      " LNK-SYUBETUGET-RJNPAYKBN
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *     
      *    Ï«ºÒ¡¦¼«ÇåÀÕ¤Î¤È¤­
           IF      LNK-SYUBETUGET-HKNNUM
                                       =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM 
               EVALUATE    LNK-SYUBETUGET-HKNKBN
                               ALSO    LNK-SYUBETUGET-SAIGAIKBN
                                       ALSO    LNK-SYUBETUGET-NYUGAIKBN
                   WHEN    "1"         ALSO    "1"     ALSO    "1"   
                       MOVE    7081    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "1"
                       MOVE    7082    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "1"
                       MOVE    7101    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "1"
                       MOVE    7102    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "1"         ALSO    "1"     ALSO    "2"   
                       MOVE    7071    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "1"         ALSO    "2"     ALSO    "2"
                       MOVE    7072    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "2"         ALSO    "1"     ALSO    "2"
                       MOVE    7091    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "2"         ALSO    "2"     ALSO    "2"
                       MOVE    7092    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "3"         ALSO    ANY     ALSO    ANY
                       MOVE    7021    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "4"         ALSO    ANY     ALSO    ANY                     
                   WHEN    "6"         ALSO    ANY     ALSO    ANY                     
                       MOVE    7001    TO  LNK-SYUBETUGET-RECESYUBETU
      *            ¸øÌ³ºÒ³²
                   WHEN    "5"         ALSO    ANY     ALSO    "1"                     
                       MOVE    7061    TO  LNK-SYUBETUGET-RECESYUBETU     
                   WHEN    "5"         ALSO    ANY     ALSO    "2"                     
                       MOVE    7051    TO  LNK-SYUBETUGET-RECESYUBETU     
               END-EVALUATE
               GO  TO  2001-JYOKEN-HENSYU-EXT
           END-IF
      *
      *    ¸ø³²ÊÝ¸±¤Î¤È¤­
           IF      LNK-SYUBETUGET-HKNNUM     =   WRK-CONS-KOUGAI-HKNNUM
               EVALUATE    LNK-SYUBETUGET-NYUGAIKBN
                   WHEN    "1"
                       MOVE    7210    TO  LNK-SYUBETUGET-RECESYUBETU
                   WHEN    "2"
                       MOVE    7200    TO  LNK-SYUBETUGET-RECESYUBETU
               END-EVALUATE
               GO  TO  2001-JYOKEN-HENSYU-EXT
           END-IF
      *     
      *    ¼«Èñ¤Î¤È¤­
           IF    ( LNK-SYUBETUGET-HKNNUM
                                   NOT =   SPACE )
           AND   ( LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
                                       =   "8"   )
               MOVE    8888            TO  LNK-SYUBETUGET-RECESYUBETU
                                           LNK-SYUBETUGET-RECESYUBETU-Y
               GO  TO  2001-JYOKEN-HENSYU-EXT
           END-IF
      *     
      *    ¼£¸³¤Î¤È¤­
           IF    ( LNK-SYUBETUGET-HKNNUM
                                   NOT =   SPACE )
           AND   ( LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
                                       =   "9"   )
           AND   ( LNK-SYUBETUGET-SRYYM
                                       >=  WRK-CONS-SRYYM-200804 )
               MOVE    8888            TO  LNK-SYUBETUGET-RECESYUBETU
                                           LNK-SYUBETUGET-RECESYUBETU-Y
               GO  TO  2001-JYOKEN-HENSYU-EXT
           END-IF
      *
           IF      LNK-SYUBETUGET-HKNNUM     NOT =   SPACE
               EVALUATE    LNK-SYUBETUGET-HKNNUM
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       IF      LNK-SYUBETUGET-HKNNUM =   "060" OR
                                                         "068"
                           MOVE   "1"      TO  WK-KOKUHO
                       ELSE
                           MOVE   "2"      TO  WK-KOKUHO
                       END-IF
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           IF    ( LNK-SYUBETUGET-SRYYM    <   200210 )
           AND   ( LNK-SYUBETUGET-RJNHKNNUM
                                           =   "027"  )
               MOVE        "1"                 TO  WK-RJN
           END-IF
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      LNK-SYUBETUGET-KOHNUM (IDX)   =   SPACE
               IF    ( LNK-SYUBETUGET-KOHNUM (IDX)
                                   NOT =   "027" )
               AND   ( LNK-SYUBETUGET-RECESKYKBN (IDX)
                                       =   "1"   )
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
      *    »³·ÁÃÏÊý¸øÈñÂÐ±þ¡Ê£²£°£°£µÇ¯£±£²·î¡Ë
           IF    ( LNK-SYUBETUGET-PREFNUM  =   "06"     )
           AND   ( LNK-SYUBETUGET-SRYYM    <=  "200512" )
               IF      WK-KOKUHO               =   "1" OR  "2"
                   IF    ( LNK-SYUBETUGET-KOHNUM(1)(1:1)   =   "1" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(2)(1:1)   =   "1" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(3)(1:1)   =   "1" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(4)(1:1)   =   "1" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(1)(1:1)   =   "2" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(2)(1:1)   =   "2" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(3)(1:1)   =   "2" )  OR
                         ( LNK-SYUBETUGET-KOHNUM(4)(1:1)   =   "2" ) 
                       COMPUTE WK-KOHI-CNT =   WK-KOHI-CNT -   1
                   END-IF
               END-IF
           END-IF
      *
      *    Ê¡²¬ÃÏÊý¸øÈñ¡Ê£²£°£°£¶Ç¯£¹·î°ÊÁ°¡Ë
           IF    ( LNK-SYUBETUGET-PREFNUM  =   "40"                  )
           AND   ( LNK-SYUBETUGET-SRYYM    <=  WRK-CONS-SRYYM-200609 )
               IF      WK-KOKUHO               =   "1" OR  "2"
                   IF    ( LNK-SYUBETUGET-KOHNUM   (1)(1:1)
                                                   =   "1" OR  "2" )
                   AND   ( LNK-SYUBETUGET-KOHHBTNUM(1)
                                               NOT =   "41"        )     
                       COMPUTE WK-KOHI-CNT =   WK-KOHI-CNT -   1
                   END-IF
                   IF    ( LNK-SYUBETUGET-KOHNUM   (2)(1:1)
                                                   =   "1" OR  "2" )
                   AND   ( LNK-SYUBETUGET-KOHHBTNUM(2)
                                               NOT =   "41"        )     
                       COMPUTE WK-KOHI-CNT =   WK-KOHI-CNT -   1
                   END-IF
                   IF    ( LNK-SYUBETUGET-KOHNUM   (3)(1:1)
                                                   =   "1" OR  "2" )
                   AND   ( LNK-SYUBETUGET-KOHHBTNUM(3)
                                               NOT =   "41"        )     
                       COMPUTE WK-KOHI-CNT =   WK-KOHI-CNT -   1
                   END-IF
                   IF    ( LNK-SYUBETUGET-KOHNUM   (4)(1:1)
                                                   =   "1" OR  "2" )
                   AND   ( LNK-SYUBETUGET-KOHHBTNUM(4)
                                               NOT =   "41"       )     
                       COMPUTE WK-KOHI-CNT =   WK-KOHI-CNT -   1
                   END-IF
               END-IF
           END-IF
      *
           IF      LNK-SYUBETUGET-HKNNUM   NOT =   SPACE
               MOVE    LNK-SYUBETUGET-HKN-HONKZKKBN
                                             TO  WK-HONKZKKBN
               IF    ( LNK-SYUBETUGET-SRYYM    >=  200210            )
               AND   ( LNK-SYUBETUGET-SRYYM    <
                                               WRK-CONS-SRYYM-200804 )
                   IF      LNK-SYUBETUGET-AGE  <   3
                       MOVE   "3"                  TO  WK-HONKZKKBN
                   END-IF
               END-IF
               IF      LNK-SYUBETUGET-SRYYM    >=  WRK-CONS-SRYYM-200804
                   IF      LNK-SYUBETUGET-SYUGAKUKBN
                                               =   1
                       MOVE   "3"                  TO  WK-HONKZKKBN
                   END-IF
               END-IF
      *
      *        Ê¿À®£²£°Ç¯£µ·î°Ê¹ß 
               IF      LNK-SYUBETUGET-SRYYM    >   WRK-CONS-SRYYM-201404
                   EVALUATE    LNK-SYUBETUGET-HKNNUM
                       WHEN    "002" 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF    ( LNK-SYUBETUGET-AGE  >=  70 )
      *********************AND   ( LNK-SYUBETUGET-AGE  <=  74 )
                             EVALUATE    LNK-SYUBETUGET-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      LNK-SYUBETUGET-AGE  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
               END-IF
      *
      *        Ê¿À®£²£°Ç¯£´·î¡ÁÊ¿À®£²£¶Ç¯£´·î
               IF    ( LNK-SYUBETUGET-SRYYM    >=
                                               WRK-CONS-SRYYM-200804 )
               AND   ( LNK-SYUBETUGET-SRYYM    <=
                                               WRK-CONS-SRYYM-201404 )
                   EVALUATE    LNK-SYUBETUGET-HKNNUM
                       WHEN    "002" 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF    ( LNK-SYUBETUGET-AGE  >=  70 )
      *********************AND   ( LNK-SYUBETUGET-AGE  <=  74 )
                             EVALUATE    LNK-SYUBETUGET-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      LNK-SYUBETUGET-AGE  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
               END-IF
      *
      *        Ê¿À®£±£¸Ç¯£±£°·î¡ÁÊ¿À®£²£°Ç¯£³·î 
               IF    ( LNK-SYUBETUGET-SRYYM    >=  "200610"          )
               AND   ( LNK-SYUBETUGET-SRYYM    <
                                               WRK-CONS-SRYYM-200804 )
                   EVALUATE     LNK-SYUBETUGET-HKNNUM
                       WHEN    "002" 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF    ( LNK-SYUBETUGET-AGE  >=  70)  AND
                                 ( LNK-SYUBETUGET-AGE  <=  74)
                             EVALUATE    LNK-SYUBETUGET-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE   "9"  TO  WK-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE   "8"  TO  WK-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE   "7"  TO  WK-HONKZKKBN
                                   END-EVALUATE
                             END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      LNK-SYUBETUGET-RJNHKNNUM  =   "027"
                       EVALUATE   LNK-SYUBETUGET-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "?"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
      *
               IF    ( LNK-SYUBETUGET-SRYYM      >=  "200210" )
               AND   ( LNK-SYUBETUGET-SRYYM      <=  "200609" )
                   EVALUATE     LNK-SYUBETUGET-HKNNUM
                       WHEN    "002" 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF    ( LNK-SYUBETUGET-AGE  >=  70)  AND
                                 ( LNK-SYUBETUGET-AGE  <=  74)
                               EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   LNK-SYUBETUGET-HKN-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      LNK-SYUBETUGET-RJNHKNNUM    =   "027"
                       EVALUATE   LNK-SYUBETUGET-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )     
           AND   ( LNK-SYUBETUGET-HKN-HONKZKKBN
                                       =   "1"   )     
               IF      LNK-SYUBETUGET-HKNNUM
                                       =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF
      *
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           .
        2001-JYOKEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *??
           DISPLAY "SYUBETU   : " LNK-SYUBETUGET-RECESYUBETU
           DISPLAY "SYUBETU-X : " LNK-SYUBETUGET-RECESYUBETU-X
           DISPLAY "SYUBETU-Y : " LNK-SYUBETUGET-RECESYUBETU-Y
      *??          
           .
       300-END-EXT.
           EXIT. 
