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
       PROGRAM-ID.             ORCSKANACHK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : È¾³Ñ¥Á¥§¥Ã¥¯½èÍý
      *  ÊÖµÑ¥¨¥é¡¼¥³¡¼¥É  : 0:Àµ¾ï¡¡1:È¾³Ñº®ºß¡¢£²¡§ÊÑ´¹¥¨¥é¡¼
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-Â¿¡¹Ç¼  02/08/09  ÆþÎÏ¡¦½ÐÎÏÎÎ°è¤òX(1000)¤Ø
      *  01.00.02    NACL-Â¿¡¹Ç¼  04/09/30  Á´³Ñ¥¨¥é¡¼ÄÉ²Ã
      *  01.00.03    NACL-Â¿¡¹Ç¼  05/08/10  kanaconv »ÈÍÑ¤ØÊÑ¹¹
      *  04.01.00    NACL-Â¿¡¹Ç¼  08/01/17  Á´³Ñ¶õÇòÈ½ÄêÄÉ²Ã
      *  04.05.00    NACL-Â¿¡¹Ç¼  09/12/11  Á´³Ñ¶õÇòºï½üÄÉ²Ã
      *  04.06.00    NACL-Â¿¡¹Ç¼  10/10/01  ÆþÎÏ¡¦½ÐÎÏÎÎ°è¤òX(5000)¤Ø
      *  05.01.00    NACL-Â¿¡¹Ç¼  17/09/26  ²þ¹Ô¥³¡¼¥ÉÈ½ÄêÄÉ²Ã
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-KANA2           PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANJI-ERR       PIC 9(01).
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-ZENKAKU1        PIC 9(01).
           03  FLG-ZENKAKU2        PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-MAX2            PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
      *
      *
        01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-INDAY.
               05  WRK-YY              PIC X(10).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
      *
           03  WRK-HMM                 PIC 9(02).
           03  WRK-HDD                 PIC 9(02).
      *
           03  WRK-MOJI                PIC X(01).
           03  WRK-KIGO                PIC X(01).
           03  WRK-ZENMOJI             PIC X(02).
           03  WRK-ZENMOJI2            PIC X(02).
      *
           03  WRK-MEISYO              PIC X(5000).
      *
           03  WRK-ERRCD               PIC X(02).
      *
           03  WRK-MAX-CNT             PIC 9(04).
      *
           03  WRK-IDX1                PIC 9(04).
           03  WRK-IDX2                PIC 9(04).
      *R01.8
           03  WRK-PCNT                PIC 9(04).
      *
      *    ÊÑ´¹¥Æ¡¼¥Ö¥ë
       01  TBL-HENKAN-AREA.
           03  FILLER              PIC X(01)   VALUE   "0".
           03  FILLER              PIC X(02)   VALUE   "£°".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(02)   VALUE   "£±".
           03  FILLER              PIC X(01)   VALUE   "2".
           03  FILLER              PIC X(02)   VALUE   "£²".
           03  FILLER              PIC X(01)   VALUE   "3".
           03  FILLER              PIC X(02)   VALUE   "£³".
           03  FILLER              PIC X(01)   VALUE   "4".
           03  FILLER              PIC X(02)   VALUE   "£´".
           03  FILLER              PIC X(01)   VALUE   "5".
           03  FILLER              PIC X(02)   VALUE   "£µ".
           03  FILLER              PIC X(01)   VALUE   "6".
           03  FILLER              PIC X(02)   VALUE   "£¶".
           03  FILLER              PIC X(01)   VALUE   "7".
           03  FILLER              PIC X(02)   VALUE   "£·".
           03  FILLER              PIC X(01)   VALUE   "8".
           03  FILLER              PIC X(02)   VALUE   "£¸".
           03  FILLER              PIC X(01)   VALUE   "9".
           03  FILLER              PIC X(02)   VALUE   "£¹".
      *±Ñ»ú
           03  FILLER              PIC X(01)   VALUE   "A".
           03  FILLER              PIC X(02)   VALUE   "£Á".
           03  FILLER              PIC X(01)   VALUE   "B".
           03  FILLER              PIC X(02)   VALUE   "£Â".
           03  FILLER              PIC X(01)   VALUE   "C".
           03  FILLER              PIC X(02)   VALUE   "£Ã".
           03  FILLER              PIC X(01)   VALUE   "D".
           03  FILLER              PIC X(02)   VALUE   "£Ä".
           03  FILLER              PIC X(01)   VALUE   "E".
           03  FILLER              PIC X(02)   VALUE   "£Å".
           03  FILLER              PIC X(01)   VALUE   "F".
           03  FILLER              PIC X(02)   VALUE   "£Æ".
           03  FILLER              PIC X(01)   VALUE   "G".
           03  FILLER              PIC X(02)   VALUE   "£Ç".
           03  FILLER              PIC X(01)   VALUE   "H".
           03  FILLER              PIC X(02)   VALUE   "£È".
           03  FILLER              PIC X(01)   VALUE   "I".
           03  FILLER              PIC X(02)   VALUE   "£É".
           03  FILLER              PIC X(01)   VALUE   "J".
           03  FILLER              PIC X(02)   VALUE   "£Ê".
           03  FILLER              PIC X(01)   VALUE   "K".
           03  FILLER              PIC X(02)   VALUE   "£Ë".
           03  FILLER              PIC X(01)   VALUE   "L".
           03  FILLER              PIC X(02)   VALUE   "£Ì".
           03  FILLER              PIC X(01)   VALUE   "N".
           03  FILLER              PIC X(02)   VALUE   "£Î".
           03  FILLER              PIC X(01)   VALUE   "M".
           03  FILLER              PIC X(02)   VALUE   "£Í".
           03  FILLER              PIC X(01)   VALUE   "O".
           03  FILLER              PIC X(02)   VALUE   "£Ï".
           03  FILLER              PIC X(01)   VALUE   "P".
           03  FILLER              PIC X(02)   VALUE   "£Ð".
           03  FILLER              PIC X(01)   VALUE   "Q".
           03  FILLER              PIC X(02)   VALUE   "£Ñ".
           03  FILLER              PIC X(01)   VALUE   "R".
           03  FILLER              PIC X(02)   VALUE   "£Ò".
           03  FILLER              PIC X(01)   VALUE   "S".
           03  FILLER              PIC X(02)   VALUE   "£Ó".
           03  FILLER              PIC X(01)   VALUE   "T".
           03  FILLER              PIC X(02)   VALUE   "£Ô".
           03  FILLER              PIC X(01)   VALUE   "U".
           03  FILLER              PIC X(02)   VALUE   "£Õ".
           03  FILLER              PIC X(01)   VALUE   "V".
           03  FILLER              PIC X(02)   VALUE   "£Ö".
           03  FILLER              PIC X(01)   VALUE   "W".
           03  FILLER              PIC X(02)   VALUE   "£×".
           03  FILLER              PIC X(01)   VALUE   "X".
           03  FILLER              PIC X(02)   VALUE   "£Ø".
           03  FILLER              PIC X(01)   VALUE   "Y".
           03  FILLER              PIC X(02)   VALUE   "£Ù".
           03  FILLER              PIC X(01)   VALUE   "Z".
           03  FILLER              PIC X(02)   VALUE   "£Ú".
      *±Ñ»ú
           03  FILLER              PIC X(01)   VALUE   "a".
           03  FILLER              PIC X(02)   VALUE   "£á".
           03  FILLER              PIC X(01)   VALUE   "b".
           03  FILLER              PIC X(02)   VALUE   "£â".
           03  FILLER              PIC X(01)   VALUE   "c".
           03  FILLER              PIC X(02)   VALUE   "£ã".
           03  FILLER              PIC X(01)   VALUE   "d".
           03  FILLER              PIC X(02)   VALUE   "£ä".
           03  FILLER              PIC X(01)   VALUE   "e".
           03  FILLER              PIC X(02)   VALUE   "£å".
           03  FILLER              PIC X(01)   VALUE   "f".
           03  FILLER              PIC X(02)   VALUE   "£æ".
           03  FILLER              PIC X(01)   VALUE   "g".
           03  FILLER              PIC X(02)   VALUE   "£ç".
           03  FILLER              PIC X(01)   VALUE   "h".
           03  FILLER              PIC X(02)   VALUE   "£è".
           03  FILLER              PIC X(01)   VALUE   "i".
           03  FILLER              PIC X(02)   VALUE   "£é".
           03  FILLER              PIC X(01)   VALUE   "j".
           03  FILLER              PIC X(02)   VALUE   "£ê".
           03  FILLER              PIC X(01)   VALUE   "k".
           03  FILLER              PIC X(02)   VALUE   "£ë".
           03  FILLER              PIC X(01)   VALUE   "l".
           03  FILLER              PIC X(02)   VALUE   "£ì".
           03  FILLER              PIC X(01)   VALUE   "n".
           03  FILLER              PIC X(02)   VALUE   "£î".
           03  FILLER              PIC X(01)   VALUE   "m".
           03  FILLER              PIC X(02)   VALUE   "£í".
           03  FILLER              PIC X(01)   VALUE   "o".
           03  FILLER              PIC X(02)   VALUE   "£ï".
           03  FILLER              PIC X(01)   VALUE   "p".
           03  FILLER              PIC X(02)   VALUE   "£ð".
           03  FILLER              PIC X(01)   VALUE   "q".
           03  FILLER              PIC X(02)   VALUE   "£ñ".
           03  FILLER              PIC X(01)   VALUE   "r".
           03  FILLER              PIC X(02)   VALUE   "£ò".
           03  FILLER              PIC X(01)   VALUE   "s".
           03  FILLER              PIC X(02)   VALUE   "£ó".
           03  FILLER              PIC X(01)   VALUE   "t".
           03  FILLER              PIC X(02)   VALUE   "£ô".
           03  FILLER              PIC X(01)   VALUE   "u".
           03  FILLER              PIC X(02)   VALUE   "£õ".
           03  FILLER              PIC X(01)   VALUE   "v".
           03  FILLER              PIC X(02)   VALUE   "£ö".
           03  FILLER              PIC X(01)   VALUE   "w".
           03  FILLER              PIC X(02)   VALUE   "£÷".
           03  FILLER              PIC X(01)   VALUE   "x".
           03  FILLER              PIC X(02)   VALUE   "£ø".
           03  FILLER              PIC X(01)   VALUE   "y".
           03  FILLER              PIC X(02)   VALUE   "£ù".
           03  FILLER              PIC X(01)   VALUE   "z".
           03  FILLER              PIC X(02)   VALUE   "£ú".
      *
           03  FILLER              PIC X(01)   VALUE   "-".
           03  FILLER              PIC X(02)   VALUE   "¡Ý".
           03  FILLER              PIC X(01)   VALUE   "(".
           03  FILLER              PIC X(02)   VALUE   "¡Ê".
           03  FILLER              PIC X(01)   VALUE   ")".
           03  FILLER              PIC X(02)   VALUE   "¡Ë".
           03  FILLER              PIC X(01)   VALUE   "\".
           03  FILLER              PIC X(02)   VALUE   "¡ï".
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILLER              PIC X(02)   VALUE   "¡¥".
           03  FILLER              PIC X(01)   VALUE   "/".
           03  FILLER              PIC X(02)   VALUE   "¡¿".
           03  FILLER              PIC X(01)   VALUE   "<".
           03  FILLER              PIC X(02)   VALUE   "¡ã".
           03  FILLER              PIC X(01)   VALUE   ">".
           03  FILLER              PIC X(02)   VALUE   "¡ä".
           03  FILLER              PIC X(01)   VALUE   "*".
           03  FILLER              PIC X(02)   VALUE   "¡ö".
           03  FILLER              PIC X(01)   VALUE   "?".
           03  FILLER              PIC X(02)   VALUE   "¡©".
           03  FILLER              PIC X(01)   VALUE   "%".
           03  FILLER              PIC X(02)   VALUE   "¡ó".
           03  FILLER              PIC X(01)   VALUE   "+".
           03  FILLER              PIC X(02)   VALUE   "¡Ü".
           03  FILLER              PIC X(01)   VALUE   "!".
           03  FILLER              PIC X(02)   VALUE   "¡ª".
           03  FILLER              PIC X(01)   VALUE   "#".
           03  FILLER              PIC X(02)   VALUE   "¡ô".
           03  FILLER              PIC X(01)   VALUE   "&".
           03  FILLER              PIC X(02)   VALUE   "¡õ".
           03  FILLER              PIC X(01)   VALUE   "=".
           03  FILLER              PIC X(02)   VALUE   "¡á".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(02)   VALUE   "¡¤".
           03  FILLER              PIC X(01)   VALUE   "_".
           03  FILLER              PIC X(02)   VALUE   "¡²".
           03  FILLER              PIC X(01)   VALUE   "@".
           03  FILLER              PIC X(02)   VALUE   "¡÷".
           03  FILLER              PIC X(01)   VALUE   "[".
           03  FILLER              PIC X(02)   VALUE   "¡Î".
           03  FILLER              PIC X(01)   VALUE   "]".
           03  FILLER              PIC X(02)   VALUE   "¡Ï".
      *
       01  TBL-HENKAN-AREA-R           REDEFINES   TBL-HENKAN-AREA.
           03  TBL-HENKAN-OCC          OCCURS   83
                                       INDEXED  TBL-IDX.
               05  TBL-HALF            PIC X(01).
               05  TBL-ZEN             PIC X(02).
       01  TBL-MAX                     PIC 9(04)   VALUE   83.
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    ³ÈÄ¥Ê¸»úÂÐ±þ
           COPY    "CPORCSSTRING.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKANACHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSKANACHKAREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  KANACHK-RC
           MOVE    ZERO                TO  KANACHK-RC2
           MOVE    SPACE               TO  KANACHK-OUT-INPUT
      *    ºÇÂç·å¿ôÈ½Äê
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX     FROM    5000 BY  -1
                   UNTIL       IDX     <   1
               IF     KANACHK-MAE-INPUT (IDX:1)    NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           IF      IDX-MAX             =   ZERO
               MOVE    ZERO                TO  KANACHK-MAX
               MOVE    SPACE               TO  KANACHK-OUT-INPUT
           ELSE
               MOVE    IDX-MAX             TO  KANACHK-MAX
           END-IF
      *TEST
      *    DISPLAY " KANACHK-MAX:"  KANACHK-MAX
      *    DISPLAY " KANACHK-MAE-INPUT:"  KANACHK-MAE-INPUT
      *                    (1:KANACHK-MAX)
      *
           IF      IDX-MAX             >   ZERO
      *        Á´³Ñ¤ÎÆâÍÆÈ½Äê
               PERFORM 1000-KOUMOKU-CHK-SEC
      *
               EVALUATE    KANACHK-SYORI
                   WHEN    "1"
      *            Á´³ÑÈ½Äê½èÍý
                   PERFORM 100-ZENKAKU-CHECK-SEC
                   WHEN    "2"
      *            Á´³ÑÃÖ´¹½èÍý
                   PERFORM 200-ZENKAKU-HENKAN-SEC
               END-EVALUATE
      *H29.9
      *        ²þ¹Ô¥³¡¼¥É¥Á¥§¥Ã¥¯
               PERFORM 1009-KAIGYO-CHK-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    Á´³Ñ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       1000-KOUMOKU-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK-MAX-CNT     TO  KANACONV-LEN
           IF      KANACHK-MAX-CNT     =   ZERO
               MOVE    IDX-MAX             TO  KANACONV-LEN
           END-IF
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  KANACHK-MAE-INPUT
           MOVE    KANACONV-OUTDATA    TO  KANACHK-OUT-INPUT
           IF      KANACONV-MAXLEN     =   IDX-MAX
               MOVE    ZERO                TO  KANACHK-RC2
           ELSE
               MOVE    7                   TO  KANACHK-RC2
           END-IF
           MOVE    KANACONV-MAXLEN     TO  IDX-MAX
           MOVE    KANACONV-MAXLEN     TO  KANACHK-MAX
           IF      KANACHK-MAX-CNT     =   ZERO
               MOVE    5000                TO  KANACHK-MAX-CNT
           END-IF
           .
       1000-KOUMOKU-CHK-EXT.
           EXIT.
      *
      *H29.9
      *****************************************************************
      *    ²þ¹ÔÈ½Äê½èÍý
      *****************************************************************
       1009-KAIGYO-CHK-SEC           SECTION.
      *
      *R01.8 ÊÑ¹¹
           MOVE    ZERO                TO  WRK-PCNT
           INSPECT     KANACHK-OUT-INPUT  TALLYING  WRK-PCNT
                                      FOR ALL  X"0A"
      *    ²þ¹Ô¥³¡¼¥É¤¢¤ê
           IF      WRK-PCNT            >   ZERO
               MOVE    1                   TO  KANACHK-RC4
           END-IF
      *
           .
       1009-KAIGYO-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Á´³ÑÈ½Äê½èÍý
      *****************************************************************
       100-ZENKAKU-CHECK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  FLG-KUHAKU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
      *        Á´³Ñ¤Î¤ß
               MOVE    1                   TO  FLG-KANJI
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK-MAX-CNT     TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    1                   TO  KANACONV-CONV-FLAG
               MOVE    15                  TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK-MAE-INPUT   TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-MAXLEN     =   IDX-MAX
                   CONTINUE
               ELSE
      *            È¾³Ñ¶õÇò¤ÎÃÖ´¹¤¨¤¢¤ê
                   MOVE    8                   TO  KANACHK-RC2
               END-IF
               MOVE    KANACONV-OUTDATA    TO  KANACHK-OUT-INPUT
               MOVE    2                   TO  KANACHK-SYUBETU
               MOVE    KANACONV-MAXLEN     TO  KANACHK-MAX
           ELSE
      *        È¾³Ñ¤Î¤ß¤ÎÈ½Äê
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK-MAX-CNT     TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    113                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK-MAE-INPUT   TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
                   MOVE    1                   TO  KANACHK-SYUBETU
                   MOVE    KANACONV-MAXLEN     TO  KANACHK-MAX
                   MOVE    KANACONV-MAXLEN     TO  IDX-MAX
      *            ´Ö¤Ë¶õÇò¤¬¤¢¤ë¤«¤ÎÈ½Äê
                   PERFORM 1001-KUHAKU-CHK-SEC
               ELSE
      *            º®ºß¥¨¥é¡¼
                   MOVE    1                   TO  KANACHK-RC
               END-IF
           END-IF
      *
      *    Á´³Ñ¶õÇò¤ÎÈ½Äê
           IF     (KANACHK-SYUBETU     =   2    )  AND
                  (KANACHK-RC          =   ZERO )
               PERFORM 1003-ZENSPACE-CHK-SEC
           END-IF
           .
       100-ZENKAKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ö¤ËÈ¾³Ñ¶õÇò¤¬¤¢¤ë¤«¤ÎÈ½Äê½èÍý
      *****************************************************************
       1001-KUHAKU-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN NOT =   ZERO
               MOVE    8                   TO  KANACHK-RC2
           END-IF
      *
           .
       1001-KUHAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Á´³Ñ¶õÇòÈ½Äê½èÍý
      *****************************************************************
       1003-ZENSPACE-CHK-SEC           SECTION.
      *
           IF      KANACHK-SYORIKBN    =   "1"
      *        ºÇ¸å¤ÎÁ´³Ñ¶õÇòºï½ü
               PERFORM 10031-LASTDEL-HEN-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-ZENKAKU1
           MOVE    ZERO                TO  FLG-ZENKAKU2
           MOVE    ZERO                TO  WRK-IDX1
           MOVE    ZERO                TO  WRK-IDX2
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >   KANACHK-MAX
               IF      KANACHK-OUT-INPUT(IDX:2)    =   "¡¡"
                   MOVE    1               TO  FLG-ZENKAKU2
                   MOVE    IDX             TO  WRK-IDX2
               ELSE
                   MOVE    1               TO  FLG-ZENKAKU1
                   MOVE    IDX             TO  WRK-IDX1
               END-IF
           END-PERFORM
           IF      FLG-ZENKAKU2        =   1
               IF      FLG-ZENKAKU1        =   1
      *            Á´³Ñ¶õÇò¤¢¤ê
                   MOVE    1               TO  KANACHK-RC3
      *            ºÇ¸å¤¬Á´³Ñ¶õÇò
                   IF      WRK-IDX1        <   WRK-IDX2
                       MOVE    3               TO  KANACHK-RC3
                   END-IF
               ELSE
      *            Á´³Ñ¶õÇò¤Î¤ß
                   MOVE    2               TO  KANACHK-RC3
               END-IF
           END-IF
           .
       1003-ZENSPACE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ºÇ¸å¤ÎÁ´³Ñ¶õÇòºï½ü½èÍý
      *****************************************************************
       10031-LASTDEL-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX1
           MOVE    SPACE               TO  WRK-MEISYO
           COMPUTE WRK-IDX2            =   KANACHK-MAX   -  1
           PERFORM VARYING     IDX     FROM    WRK-IDX2  BY  -2
                   UNTIL       IDX     <   1
               IF      KANACHK-OUT-INPUT(IDX:2)    =   "¡¡"
                   CONTINUE
               ELSE
                   COMPUTE WRK-IDX1        =   IDX     +   1
                   MOVE    KANACHK-OUT-INPUT(1:WRK-IDX1)
                                               TO  WRK-MEISYO
                   MOVE    2                   TO  IDX
               END-IF
               IF      IDX             =   1
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
           MOVE    WRK-MEISYO          TO  KANACHK-OUT-INPUT
           MOVE    WRK-IDX1            TO  KANACHK-MAX
           .
       10031-LASTDEL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´³Ñ½èÍý
      *****************************************************************
       200-ZENKAKU-HENKAN-SEC           SECTION.
      *
           MOVE    2                   TO  KANACHK-SYUBETU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
               MOVE    2                   TO  KANACHK-SYUBETU
               MOVE    KANACONV-OUTDATA    TO  KANACHK-OUT-INPUT
               MOVE    KANACONV-MAXLEN     TO  KANACHK-MAX
           ELSE
               MOVE    2                   TO  KANACHK-RC
           END-IF
      *
      *    Á´³Ñ¶õÇò¤ÎÈ½Äê
           IF     (KANACHK-SYUBETU     =   2    )  AND
                  (KANACHK-RC          =   ZERO )
               PERFORM 1003-ZENSPACE-CHK-SEC
           END-IF
           .
       200-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
