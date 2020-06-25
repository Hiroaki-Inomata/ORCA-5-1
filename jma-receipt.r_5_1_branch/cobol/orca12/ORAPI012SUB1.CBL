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
       PROGRAM-ID.             ORAPI012SUB1.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ´µ¼ÔÅÐÏ¿
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÊÝ¸±Êä½õ¶èÊ¬Ì¾¾Î¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  17/07/XX    NACL-Â¿¡¹Ç¼   ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
      *01  CNT-AREA.
      *
      *    Åº»úÎÎ°è
      *01  IDX-AREA.
      *    03  IDX                 PIC 9(04).
      *
      *    °ì»þÎÎ°è
      *01  WRK-AREA.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *
      *    ¿ô»úÊÑ´¹ÎÎ°è
      *    COPY    "CPORCSNUM.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *    £Ä£Â¸¡º÷
      *    COPY    "MCPDATA.INC".
      *
      *
      *    COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥Ñ¥é¥á¥¿
           COPY    "CPORAPI012SUB1.INC".
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORAPI012SUB1AREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  API012SUB1-OUT-AREA
      **   INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *    Êä½õ¶èÊ¬Ì¾¾Î¼èÆÀ
           PERFORM 100-HOJOKBN-HEN-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    Êä½õ¶èÊ¬Ì¾¾Î¼èÆÀ½èÍý
      *****************************************************************
       100-HOJOKBN-HEN-SEC               SECTION.
      *
      *
           EVALUATE    API012SUB1-HKNNUM
      *    Á¥°÷
               WHEN    "002"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "¿¦Ì³" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "2"
                       MOVE    "²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "3"
                       MOVE    "ÄÌ¶Ð" 
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "7"
                       MOVE    "£³³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "8"
                       MOVE    "£²³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "9"
                       MOVE    "£±³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "A"
                       MOVE    "£±³ä¿¦Ì³" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "B"
                       MOVE    "£±³ä²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "C"
                       MOVE    "£±³äÄÌ¶Ð" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "D"
                       MOVE    "£²³ä¿¦Ì³" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "E"
                       MOVE    "£²³ä²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "F"
                       MOVE    "£²³äÄÌ¶Ð" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "G"
                       MOVE    "£³³ä¿¦Ì³" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "H"
                       MOVE    "£³³ä²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "I"
                       MOVE    "£³³äÄÌ¶Ð" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *    ¸å´ü¹âÎð¼Ô
               WHEN    "039"
               WHEN    "040"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "£±³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "3"
                       MOVE    "£³³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ¶¦ºÑ
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "2"
                       MOVE    "²¼Á¥£³·î"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "7"
                       MOVE    "£³³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "8"
                       MOVE    "£²³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "9"
                       MOVE    "£±³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "B"
                       MOVE    "£±³ä²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "E"
                       MOVE    "£²³ä²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "H"
                       MOVE    "£³³ä²¼Á¥" 
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ¹ñÊÝ
               WHEN    "060"
               WHEN    "068"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "0"
                       MOVE    "Ìµ¤·"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    00          TO  API012SUB1-RATE
                   WHEN    "1"
                       MOVE    "£±³ä"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "2"
                       MOVE    "£²³ä"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "3"
                       MOVE    "£³³ä"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    "4"
                       MOVE    "³°£±³ä Æþ£°³ä"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    10          TO  API012SUB1-RATE
                   WHEN    "5"
                       MOVE    "³°£²³ä Æþ£±³ä"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    20          TO  API012SUB1-RATE
                   WHEN    "6"
                       MOVE    "³°£³³ä Æþ£²³ä"
                                           TO  API012SUB1-HOJOKBN-NAME
                       MOVE    30          TO  API012SUB1-RATE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ¼£¸³
               WHEN    "900"
               WHEN    "901"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "²ÝÀÇ"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "2"
                       MOVE    "Èó²ÝÀÇ"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *        ¼«Èñ
               WHEN    "980"   THRU   "989"
               EVALUATE    API012SUB1-HOJOKBN
                   WHEN    "1"
                       MOVE    "²ÝÀÇ"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    "2"
                       MOVE    "Èó²ÝÀÇ"
                                           TO  API012SUB1-HOJOKBN-NAME
                   WHEN    OTHER
                       MOVE    1           TO  API012SUB1-ERRCD
               END-EVALUATE
      *
               WHEN    OTHER
                   IF      API012SUB1-HKNNUM(1:1)  =   "9"
                       CONTINUE
                   ELSE
      *            ¶¨²ñÅù
                   EVALUATE    API012SUB1-HOJOKBN
                       WHEN    "9"
                           MOVE    "£±³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                           MOVE    10      TO  API012SUB1-RATE
                       WHEN    "8"
                           MOVE    "£²³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                           MOVE    20      TO  API012SUB1-RATE
                       WHEN    "7"
                           MOVE    "£³³ä" 
                                           TO  API012SUB1-HOJOKBN-NAME
                           MOVE    30      TO  API012SUB1-RATE
                       WHEN    SPACE
                           CONTINUE
                       WHEN    OTHER
                           MOVE    1           TO  API012SUB1-ERRCD
                   END-EVALUATE
                   END-IF
           END-EVALUATE
      *
           .
       100-HAKKOU-EXT.
           EXIT.
      *
      *
      *
