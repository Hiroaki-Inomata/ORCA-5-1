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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSIRYOKNS.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : °åÎÅ´Ñ»¡Æþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  10/08/24    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5001.INC".
      *
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    °åÎÅ´Ñ»¡Æþ±¡ÎÁ¥³¡¼¥É¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSIRYOKNS.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSIRYOKNS-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           PERFORM    200-EDIT-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    °åÎÅ´Ñ»¡Æþ±¡ÎÁ¥³¡¼¥É¥»¥Ã¥È
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-IRYOKNS-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-IRYOKNS-RC
           MOVE    ZERO                TO  IDX
      *
           IF  LNK-IRYOKNS-KBN      =      "4"
      *
      *(8801-0)Æþ±¡°å³Ø´ÉÍýÎÁ¤Î¥»¥Ã¥È
             EVALUATE    LNK-IRYOKNS-NYUIN
      *        µÞÀ­´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ
               WHEN    "01"
               WHEN    "02"
               WHEN    "03"
                       ADD     1            TO  IDX
                       MOVE   "188000110"   TO  LNK-IRYOKNS-SRYCD(IDX)
      *        ²óÉü´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ
               WHEN    "04"
               WHEN    "11"
               WHEN    "18"
                       ADD     1            TO  IDX
                       MOVE   "188000510"   TO  LNK-IRYOKNS-SRYCD(IDX)
      *        ¼Ò²ñÉüµ¢´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ
               WHEN    "05"
               WHEN    "06"
               WHEN    "07"
               WHEN    "08"
               WHEN    "09"
               WHEN    "10"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
               WHEN    "15"
               WHEN    "16"
               WHEN    "17"
                       ADD     1            TO  IDX
                       MOVE   "188000610"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-1)Ì¤´ð½à¸º»»¡ÊÉÂÅï¤Ë·¸¤ë´ð½à¤òËþ¤¿¤µ¤Ê¤¤¾ì¹ç¡Ë¤Î¥»¥Ã¥È
             INITIALIZE                   SYSKANRI-REC
             MOVE    LNK-IRYOKNS-HOSPNUM
                                      TO  SYS-HOSPNUM
             MOVE    "5001"           TO  SYS-KANRICD
             MOVE    LNK-IRYOKNS-BTUNUM
                                      TO  SYS-KBNCD
             MOVE    LNK-IRYOKNS-SANTEIYM
                                      TO  SYS-STYUKYMD(1:6)
             MOVE    "01"             TO  SYS-STYUKYMD(7:2)
             MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             PERFORM  910-SYSKANRI-SELECT-SEC
             IF      FLG-SYSKANRI     =   ZERO
               MOVE    "tbl_syskanri" TO  MCP-TABLE
               MOVE    "key10"        TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF     FLG-SYSKANRI     =   ZERO
                   PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL (IDX1     >   14)   OR
                            (SYS-5001-KSNCD(IDX1)  =   SPACE)
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188000270") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188000270"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
                   END-PERFORM
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
      *
      *(8801-2)µÞÀ­´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ¤ÎÌ¤Å¬¹ç¸º»»¤Î¥»¥Ã¥È
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "02"
      *                Ì¤Å¬¹ç¸º»»¡ÊÆþ±¡·èÄêÆü¤«¤é£¹£±Æü°Ê¾å£±Ç¯°ÊÆâ¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188000370"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "03"
      *                Ì¤Å¬¹ç¸º»»¡ÊÆþ±¡·èÄêÆü¤«¤é£±Ç¯Ä¶¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188000470"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)´Ñ»¡²óÉü´ü¸º»»¤Î¥»¥Ã¥È¡Ê»»Äê³«»ÏÆü¤«¤é£²£·£±Æü°Ê¾å¡Ë
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "11"
                       ADD     1            TO  IDX
                       MOVE   "188006570"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)´Ñ»¡²óÉü´ü¸º»»¤Î¥»¥Ã¥È¡Ê»»Äê³«»ÏÆü¤«¤é£±Ç¯£¹£°ÆüÄ¶¡Ë
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "18"
                       ADD     1            TO  IDX
                       MOVE   "188010170"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)´Ñ»¡¼Ò²ñÉüµ¢´ü¸º»»¤Î¥»¥Ã¥È
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "06"
               WHEN    "15"
      *                ´Ñ»¡¼Ò²ñÉüµ¢´ü¸º»»¡Ê»»Äê³«»ÏÆü¤«¤é£±£¸£±Æü°Ê¾å
      *                                    £±Ç¯°ÊÆâ¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188000770"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "07"
      *                ´Ñ»¡¼Ò²ñÉüµ¢´ü¸º»»¡Ê»»Äê³«»ÏÆü¤«¤é£±Ç¯Ä¶
      *                                    £±Ç¯£±£¸£°Æü°ÊÆâ¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188000870"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "08"
      *                ´Ñ»¡¼Ò²ñÉüµ¢´ü¸º»»¡Ê»»Äê³«»ÏÆü¤«¤é£±Ç¯£±£¸£°ÆüÄ¶¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188005870"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "16"
      *                ´Ñ»¡¼Ò²ñÉüµ¢´ü¸º»»¡Ê»»Äê³«»ÏÆü¤«¤é£±Ç¯Ä¶
      *                                    £±Ç¯£±£¸£°Æü°ÊÆâ¡¦±ó³ÖÃÏ²Ã»»¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188009170"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "17"
      *                ´Ñ»¡¼Ò²ñÉüµ¢´ü¸º»»¡Ê»»Äê³«»ÏÆü¤«¤é£±Ç¯£±£¸£°ÆüÄ¶
      *                                    ¡¦±ó³ÖÃÏ²Ã»»¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188009270"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-2)Ì¤·Ð²á¸º»»¤Î¥»¥Ã¥È
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "09"
      *                Ì¤·Ð²á¸º»»¡Ê»»Äê³«»ÏÆü£±Ç¯Ä¶¡¢Âà±¡µö²Ä¤Î¿½Î©¤Æ
      *                            £±£¸£°Æü°ÊÆâ¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188000970"   TO  LNK-IRYOKNS-SRYCD(IDX)
               WHEN    "10"
      *                Ì¤·Ð²á¸º»»¡Ê»»Äê³«»ÏÆü£±Ç¯£±£¸£°ÆüÄ¶¡¢Âà±¡µö²Ä¤Î¿½Î©¤Æ
      *                        £±£¸£°Æü°ÊÆâ¡Ë
                       ADD     1            TO  IDX
                       MOVE   "188005970"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-3)¼Ò²ñÉüµ¢´ü°Ü¹Ô²Ã»»¤Î¥»¥Ã¥È
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "12"
               WHEN    "14"
                       ADD     1            TO  IDX
                       MOVE   "188006670"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-4)±ó³ÖÃÏ²Ã»»¤Î¥»¥Ã¥È
             EVALUATE    LNK-IRYOKNS-NYUIN
               WHEN    "13"
               WHEN    "14"
               WHEN    "15"
                       ADD     1            TO  IDX
                       MOVE   "188009070"   TO  LNK-IRYOKNS-SRYCD(IDX)
             END-EVALUATE
      *
      *
      *(8801-5)¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¤Î¥»¥Ã¥È
             INITIALIZE                   SYSKANRI-REC
             MOVE    LNK-IRYOKNS-HOSPNUM
                                      TO  SYS-HOSPNUM
             MOVE    "5001"           TO  SYS-KANRICD
             MOVE    LNK-IRYOKNS-BTUNUM
                                      TO  SYS-KBNCD
             MOVE    LNK-IRYOKNS-SANTEIYM
                                      TO  SYS-STYUKYMD(1:6)
             MOVE    "01"             TO  SYS-STYUKYMD(7:2)
             MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             PERFORM  910-SYSKANRI-SELECT-SEC
             IF      FLG-SYSKANRI     =   ZERO
               MOVE    "tbl_syskanri" TO  MCP-TABLE
               MOVE    "key10"        TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF     FLG-SYSKANRI     =   ZERO
                   PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL (IDX1     >   14)   OR
                            (SYS-5001-KSNCD(IDX1)  =   SPACE)
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£±£µ¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001070") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001070"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£±£¶¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001170") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001170"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£±£·¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001270") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001270"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£±£¸¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001370") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001370"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£±£¹¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001470") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001470"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£°¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001570") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001570"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£±¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001670") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001670"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£²¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001770") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001770"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£³¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001870") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001870"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£´¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188001970") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188001970"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£µ¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002070") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002070"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£¶¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002170") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002170"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£·¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002270") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002270"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£¸¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002370") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002370"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
      *                 ¾®µ¬³ÊÉÂÅïÉÂ¾²¿ô²Ã»»¡Ê£²£¹¾²¡Ë
                        IF  (SYS-5001-KSNCD(IDX1)  =  "188002470") AND
                            (SYS-5001-KSNKBN(IDX1) =  "2")
                           ADD     1            TO  IDX
                           MOVE   "188002470"   TO  LNK-IRYOKNS-SRYCD
                                                                 (IDX)
                        END-IF
                   END-PERFORM
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
      *
           IF    LNK-IRYOKNS-SRYCD(1)    =   SPACE
               MOVE    1                 TO  LNK-IRYOKNS-RC
           END-IF
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           MOVE      SYSKANRI-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC                 =   ZERO
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-5001-REC
               MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                     SYS-5001-REC
               MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
