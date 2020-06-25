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
       PROGRAM-ID.                 ORCSC90TEN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ºÞ¤´¤ÈÅÀ¿ô·×»»¡Ê2005.4¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/12/01    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.03.00    NACL-Â¿¡¹Ç¼  07/03/06  »ÀÁÇÅÀ¿ô·×»»½¤Àµ
      *  04.05.00    NACL-Â¿¡¹Ç¼  09/06/XX  ¿ôÎÌ¾®¿ô£µ·åÂÐ±þ
      *  04.05.00    NACL-Â¿¡¹Ç¼  10/01/XX  ÌôÉ¾ÂÐ±þ
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
           03  FLG-END             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-GENTEN          PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-END             PIC 9(04).
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
           03  IDX-K               PIC 9(04).
      *
           03  IDX7                PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEIS3        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
           03  WRK-TENSUKEIS       PIC 9(08)V9(6).
           03  WRK-TENSUKEI-9      PIC 9(08).
      *    ÌôºÞ
           03  WRK-ZAIKEI          PIC 9(08)V9(8).
      *    ´ïºà
           03  WRK-KIZKEI          PIC 9(08)V9(5).
           03  WRK-KIZKEI-3        PIC 9(08)V9(5).
      *    ¥Õ¥£¥ë¥à
           03  WRK-FIRKEI          PIC 9(08)V9(5).
           03  WRK-NYU-FIRKEI      PIC 9(08)V9(5).
           03  WRK-NYU-FIRTEN      PIC 9(08)V9(5).
           03  WRK-FIRTEN-G        PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
      *    ÃâÁÇ
           03  WRK-CHISOKEI        PIC 9(08)V9(5).
           03  WRK-CHITEN          PIC 9(08).
      *    »ÀÁÇ
           03  WRK-SANSOKIN        PIC 9(08)V9(5).
           03  WRK-SANSOKIN-SUM    PIC 9(08)V9(5).
           03  WRK-SANSOGOK        PIC 9(08).
           03  WRK-SANTEN          PIC 9(08).
      *
      *    ÌôºÞÅÀ¿ô
           03  WRK-SC90-YKZTEN         PIC 9(08).
      *    ´ïºàÅÀ¿ô
           03  WRK-SC90-KIZTEN         PIC 9(08).
      *    ¥Õ¥£¥ë¥àÅÀ¿ô
           03  WRK-SC90-FIRTEN         PIC 9(08).
      *
      *    Ãí¼Í±Õ·×»»ÍÑ
           03  WRK-SURYO-CHU9      PIC 9(05).
           03  WRK-SURYO-CHU       PIC 9(05)V9(05).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-MAISU           PIC 9(05).
      *
      *¿ÇÎÅ¹Ô°Ù½èÍýºÇÂç¹Ô
       01  MAX-LINE                PIC 9(03)   VALUE   400.
      *
      *    ¥¹¥ÑÎÎ°è
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "CPORCSC90TEN.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  LNK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //LNK-//.
      *
       PROCEDURE                   DIVISION    USING
           ORCSC90AREA
           LNK-SCR-REC
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           IF      LNK-SC90-IDX-STR    =   ZERO
               MOVE    1                   TO  IDX-STR
           ELSE
               MOVE    LNK-SC90-IDX-STR    TO  IDX-STR
           END-IF
           IF      LNK-SC90-IDX-END    =   ZERO
               MOVE    MAX-LINE            TO  IDX-END
           ELSE
               MOVE    LNK-SC90-IDX-END    TO  IDX-END
           END-IF
      *
      *    ¼êµ»Ëè¤Î½¸·×
           PERFORM 100-MEISAI-SET-SEC
      *
           EVALUATE    LNK-SC90-KBN
      *    ºÞ¹ç·×
               WHEN    "2"
      *            »ÀÁÇ¡¦ÃâÁÇ½¸·×
                   PERFORM 200-TENSUKEI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ÌÀºÙÊÔ½¸½èÍý
      *****************************************************************
       100-MEISAI-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *    ¼êµ»ÎÁ°Ê³°¤ÎÅÀ¿ô½¸·×
           PERFORM VARYING     IDX     FROM    IDX-STR  BY  1
                   UNTIL      (IDX     >   IDX-END     )  OR
                             ((LNK-COM-INPUTCD(IDX)    =   SPACE ) AND
                              (LNK-GMN-INPUTCD(IDX)    =   SPACE ))
               IF      LNK-COM-INPUTCD(IDX) (1:3)  =   "059"
                   MOVE    "7"                 TO  WRK-MOJI
               ELSE
                   MOVE    LNK-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
               END-IF
      *        ÌôÉ¾¤ÏÂÐ¾Ý³°
               IF      LNK-COM-YAKUHYO (IDX)   =   "1"
                   CONTINUE
               ELSE
      *
               MOVE    IDX                 TO  IDX-MAX
               EVALUATE    WRK-MOJI
      *        ÌôºÞ
               WHEN    "6"
                   EVALUATE    LNK-COM-YKZKBN(IDX)
                       WHEN    "4"
      *                    Ãí¼ÍÌô
                           PERFORM 1002-CHUUSYA-SET-SEC
                       WHEN    OTHER
      *                    ÆâÉþÌô
                           PERFORM 1001-YAKUZAI-SET-SEC
                   END-EVALUATE
      *        ´ïºà
               WHEN    "7"
                   IF      LNK-COM-TENSIKIBETU (IDX)   =   1
                                                       OR  2
                       IF      LNK-COM-DATAKBN (IDX)   =   "3"
      *                    ¥Õ¥£¥ë¥à¤ÎÊÔ½¸
                           PERFORM 1003-FIRUMU-SET-SEC
                       ELSE
      *                    ¥Õ¥£¥ë¥à°Ê³°¤ÎÊÔ½¸
                           PERFORM 1004-KIZAI-SET-SEC
                       END-IF
                   END-IF
               END-EVALUATE
      *
               END-IF
           END-PERFORM
      *
      *    ÌôºÞÅÀ¿ô·×»»¡ÊºÞÆâ¤Ç¤Þ¤È¤á¤Æ·×»»¡Ë
           PERFORM 1005-KEIZAITENU-SET-SEC
      *    ´ïºàÅÀ¿ô·×»»½èÍý
           PERFORM 1006-KIZTEN-KEI-SEC
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»½èÍý
           PERFORM 1007-FIRUMU-KEI-SEC
      *    ¼êµ»¤Î¹Ô
           IF      LNK-SC90-IDX        =   ZERO
               MOVE    IDX-MAX             TO  IDX-SYU
           ELSE
               MOVE    LNK-SC90-IDX        TO  IDX-SYU
      *        ¼êµ»Ëè
               MOVE    1                   TO  LNK-COM-SYUFLG(IDX-SYU)
           END-IF
      *    ¼êµ»ÅÀ¿ô
           MOVE    LNK-SC90-TENSU      TO  LNK-COM-SYUTEN-M (IDX-SYU)
           ADD     LNK-SC90-TENSU      TO  LNK-SC90-TENSUKEI
      *    ÌôºÞ
           MOVE    WRK-SC90-YKZTEN     TO  LNK-COM-YKZTEN-M(IDX-SYU)
           ADD     WRK-SC90-YKZTEN     TO  LNK-SC90-YKZTEN
      *    ´ïºà
           MOVE    WRK-SC90-KIZTEN     TO  LNK-COM-KIZTEN-M (IDX-SYU)
           ADD     WRK-SC90-KIZTEN     TO  LNK-SC90-KIZTEN
      *    ¥Õ¥£¥ë¥àÅÀ¿ô
           MOVE    WRK-SC90-FIRTEN     TO  LNK-COM-FIRTEN-M (IDX-SYU)
           ADD     WRK-SC90-FIRTEN     TO  LNK-SC90-FIRTEN
           .
       100-MEISAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ÆâÉþ¡¦ÆÜÉþÊÔ½¸½èÍý
      *****************************************************************
       1001-YAKUZAI-SET-SEC           SECTION.
      *
      *    ÅÀ¿ô
           COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)  )
           ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
           .
       1001-YAKUZAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    Ãí¼ÍÌôÊÔ½¸½èÍý
      *****************************************************************
       1002-CHUUSYA-SET-SEC           SECTION.
      *
      *    »ÄÎÌÇÑ´þÈ½Äê
      *!   IF     (LNK-COM-TANICD (IDX)    =   "014"
      *                                    OR  "022"
      *!                                   OR  "046"  )  AND
      *    »ÄÎÌÇÑ´þ¤Î¤¢¤ë¤¹¤Ù¤Æ¤ÎÃí¼ÍÌôºÞ¤òÂÐ¾Ý¤È¤¹¤ë
           IF     (IDX                     <   MAX-LINE    )  AND
                  (LNK-COM-INPUTCD(IDX + 1)  =   "099309901")
               MOVE    LNK-COM-SURYO(IDX)    TO  WRK-SURYO
               IF      WRK-SURYO-S2          >   ZERO
                   COMPUTE WRK-SURYO-CHU9      =   LNK-COM-SURYO (IDX)
                                               +   0.99999
                   MOVE    WRK-SURYO-CHU9          TO  WRK-SURYO-CHU
               ELSE
                   MOVE    LNK-COM-SURYO (IDX)     TO  WRK-SURYO-CHU
               END-IF
      *        ÅÀ¿ô
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   WRK-SURYO-CHU
           ELSE
      *        ÅÀ¿ô
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN(IDX)
                                           *   LNK-COM-SURYO  (IDX)
           END-IF
      *
           ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
           .
       1002-CHUUSYA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ïºà¡Ê¥Õ¥£¥ë¥à¡Ë²èÁü¿ÇÃÇ°Ê³°ÊÔ½¸½èÍý
      *****************************************************************
       1003-FIRUMU-SET-SEC           SECTION.
      *
      *    ÅÀ¿ô
      *    (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)
           COMPUTE WRK-SYOKEIS         =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           MOVE    WRK-SYOKEIS         TO  WRK-SYOKEI
      *
      *    ²èÁü¿ÇÃÇ
           IF      LNK-COM-SRYKBN (IDX)    =   "70"
      *        ÆÃÄê´ïºàÇ¯Îð²Ã»»¶èÊ¬È½Äê
               IF      LNK-COM-TOKUKIZAIAGEKSNKBN (IDX)
                                           =   1
      *            ÆýÍÄ»ù²Ã»»Ê¬
                   PERFORM 10031-FIRUMU-KASAN-SEC
      ************ ADD     WRK-SYOKEI          TO  WRK-NYU-FIRKEI
               ELSE
      *            ¥Õ¥£¥ë¥àÅÀ¿ô²Ã»»
                   ADD     WRK-SYOKEI          TO  WRK-FIRKEI
               END-IF
           ELSE
      *        ¾å¸ÂÅÀ¿ôÈ½Äê¡Ê²èÁü¿ÇÃÇ°Ê³°¡Ë
               IF      LNK-COM-JGNTEN (IDX) =   ZERO
      *            ¥Õ¥£¥ë¥àÅÀ¿ô²Ã»»
                   ADD     WRK-SYOKEI          TO  WRK-FIRKEI
               ELSE
                   COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                               /   10
                                               +   0.5
                   MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
                   IF      WRK-FIRTEN          >   LNK-COM-JGNTEN (IDX)
                       MOVE    LNK-COM-JGNTEN(IDX) TO  WRK-FIRTEN
      *                ´ïºàÅÀ¿ô¤Ë¾å¸ÂÅÀ¿ô¤ò½¸·×
                       ADD     WRK-FIRTEN          TO  WRK-FIRTEN-G
                   ELSE
                       ADD     WRK-SYOKEI          TO  WRK-FIRKEI
                   END-IF
               END-IF
           END-IF
           .
       1003-FIRUMU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ´ïºàÊÔ½¸½èÍý
      *****************************************************************
       1004-KIZAI-SET-SEC            SECTION.
      *
           EVALUATE    LNK-COM-TOKUKIZAISBT1 (IDX)
               WHEN    ZERO
      *            ÄÌ¾ï
      *            (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI
               WHEN    2
                   IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            ÃâÁÇ
      *                (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)¡¡»Í¼Î¸ÞÆþ
                       COMPUTE WRK-SYOKEIS2    =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                       MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                       ADD     WRK-SYOKEIS         TO  WRK-CHISOKEI
                   END-IF
      *            »ÀÁÇ
                   IF      LNK-COM-SANSOKBN (IDX)  =   1
      *                (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)
                       COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX)
                       ADD     WRK-SYOKEIS2        TO  WRK-SANSOKIN-SUM
                   END-IF
               WHEN    3
      *            ¹âÀþÎÌÎ¨¥¤¥ê¥¸¥¦¥à
      *            (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI-3
           END-EVALUATE
      *
           .
       1004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌôºÞÅÀ¿ô·×»»½èÍý
      *****************************************************************
       1005-KEIZAITENU-SET-SEC           SECTION.
      *
           MOVE    WRK-ZAIKEI          TO  WRK-SYOKEIS2
      *
      * =============================================================
      *---// £±ÆüÊ¬¤ÎÅÀ¿ô¡¡15ÅÀ°Ê²¼ ¢Í £±ÅÀ ¡ÊÌôºÞ°Ê³°¤Î»þ¡¢¥¼¥í¡Ë  //
      *-- // £±ÆüÊ¬¤ÎÅÀ¿ô¡¡15ÅÀÄ¶²á ¢Í ¡Ê£±ÆüÊ¬¤ÎÅÀ¿ô¡Ý15¡Ë/ 10 
      *==============================================================
      *
           IF      WRK-SYOKEIS2        <=  15
               IF     (LNK-COM-SRYKBN (01)     =   "21"  OR  "22"  OR
                                                   "23"  OR
                                                   "31"  OR  "32"  OR
                                                   "33"   )  AND
                      (WRK-SYOKEIS2        >   ZERO       )
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
               COMPUTE WRK-SYOKEIS3        =  ((WRK-SYOKEIS2   -  15)
                                           /    10 )
                                           +   1
           END-IF
      *
           COMPUTE WRK-SC90-YKZTEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       1005-KEIZAITENU-EXT.
           EXIT.
      *****************************************************************
      *    ´ïºàÅÀ¿ô·×»»½èÍý¡Ê¥Õ¥£¥ë¥à¡¢»ÀÁÇ¡¢ÃâÁÇ°Ê³°¡Ë
      *****************************************************************
       1006-KIZTEN-KEI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SC90-KIZTEN
      *    ¤½¤ÎÂ¾´ïºà
           IF      WRK-KIZKEI          >   ZERO
      *    ¶â³Û·×¡à£±£°¡¡»Í¼Î¸ÞÆþ
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI
                                           /   10  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
           IF      WRK-KIZKEI-3        >   ZERO
      *    ¶â³Û·×¡à£·£°¡¡»Í¼Î¸ÞÆþ
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI-3
                                           /   70  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *
           .
       1006-KIZTEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²èÁü¿ÇÃÇÆýÍÄ»ù²Ã»»¥Õ¥£¥ë¥àÅÀ¿ô·×»»½èÍý
      *****************************************************************
       10031-FIRUMU-KASAN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEIS
           COMPUTE IDX7                =   IDX   +   1
           IF      LNK-COM-INPUTCD(IDX7)
                                       =   "799990070"
      *        Ä¾²¼¤ËÆýÍÄ»ù²Ã»»¤¬¤¢¤ë»þ
               COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                           *   LNK-COM-KISOTEN (IDX7)
                                           /   100
               COMPUTE WRK-NYU-FIRTEN      =   WRK-SYOKEI
                                           +   WRK-TENSUKEIS
           ELSE
               MOVE    WRK-SYOKEI          TO  WRK-NYU-FIRTEN
           END-IF
      *
           ADD     WRK-NYU-FIRTEN          TO  WRK-NYU-FIRKEI
      *
           .
       10031-FIRUMU-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»½èÍý
      *****************************************************************
       1007-FIRUMU-KEI-SEC           SECTION.
      *
           IF      WRK-NYU-FIRKEI      >   ZERO
               COMPUTE WRK-FIRKEI          =   WRK-FIRKEI
                                           +   WRK-NYU-FIRKEI
           END-IF
      *
           IF      WRK-FIRKEI        >   ZERO
      *        ¶â³Û·×¡à£±£°¡¡»Í¼Î¸ÞÆþ
               COMPUTE WRK-SYOKEIS2        =  (WRK-FIRKEI
                                           /   10  )
                                           +   0.5
               MOVE    WRK-SYOKEIS2        TO  WRK-SC90-FIRTEN
           ELSE
      *        ¾å¸ÂÅÀ¿ôÊ¬
               MOVE    WRK-FIRTEN-G        TO  WRK-SC90-FIRTEN
           END-IF
      *
           .
       1007-FIRUMU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       200-TENSUKEI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-CHISOKEI
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  FLG-GENTEN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   MAX-LINE  )  OR
                             ((LNK-COM-INPUTCD(IDX)    =   SPACE ) AND
                              (LNK-GMN-INPUTCD(IDX)    =   SPACE ))
               ADD     1                   TO  IDX-MAX
               IF     (LNK-COM-INPUTCD (IDX)(1:1)  =   "7"  ) AND
                      (LNK-COM-TENSIKIBETU (IDX)   =   1
                                                   OR  2    )
                   IF      LNK-COM-TOKUKIZAISBT1(IDX)   =  2
      *                »ÀÁÇ¡¦ÃâÁÇ¤Î½¸·×
                       PERFORM 2004-KIZAI-SET-SEC
                   END-IF
               END-IF
      *        ¸ºÅÀ
               IF     (LNK-COM-PLUSKBN (IDX)   =   1   )
                   MOVE    1               TO  FLG-GENTEN
               END-IF
               IF      FLG-GENTEN          =   1
                   MOVE    1               TO  LNK-COM-PLUSKBN (IDX)
               END-IF
           END-PERFORM
      *    ÃâÁÇÅÀ¿ô·×»»½èÍý
           IF      WRK-CHISOKEI        =   ZERO
               MOVE      ZERO              TO  LNK-SC90-CHITEN
           ELSE
               PERFORM 2007-CHITEN-KEI-SEC
           END-IF
      *    »ÀÁÇÅÀ¿ô·×»»½èÍý
           IF      WRK-SANSOKIN-SUM    =   ZERO
               MOVE      ZERO              TO  LNK-SC90-SANTEN
           ELSE
               PERFORM 2008-SANSO-KEI-SEC
           END-IF
      *
           MOVE    LNK-SC90-CHITEN     TO  LNK-COM-CHISOTEN(IDX-MAX)
           MOVE    LNK-SC90-SANTEN     TO  LNK-COM-SANSOTEN(IDX-MAX)
      *
      *    ¹ç·×ÅÀ¿ôÊÔ½¸
           MOVE    LNK-SC90-TENSUKEI   TO  LNK-COM-SYUTEN1 (IDX-MAX)
      *    ÌôºÞ
           MOVE    LNK-SC90-YKZTEN     TO  LNK-COM-YKZTEN  (IDX-MAX)
      *    ´ïºà
           COMPUTE LNK-COM-KIZAITEN(IDX-MAX)
                                       =   LNK-SC90-KIZTEN
                                       +   LNK-SC90-SANTEN
                                       +   LNK-SC90-CHITEN
      *    ¥Õ¥£¥ë¥à
           IF      LNK-COM-SRYKBN (01)    =   "70"
      *        ¥Õ¥£¥ë¥àÎÁ¤ò¼êµ»ÎÁ£²¤Ø
               MOVE    LNK-SC90-FIRTEN     TO  LNK-COM-SYUTEN2(IDX-MAX)
      *        ¥Õ¥£¥ë¥àÎÁ¤ò¼êµ»ÎÁ¤Ø²Ã»»
               COMPUTE LNK-COM-SYUTEN1 (IDX-MAX)
                                           =   LNK-COM-SYUTEN1 (IDX-MAX)
                                           +   LNK-SC90-FIRTEN
           ELSE
               ADD     LNK-SC90-FIRTEN     TO  LNK-COM-KIZAITEN(IDX-MAX)
           END-IF
      *    ÅÀ¿ô·×¡¡·×»»
           COMPUTE LNK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-YKZTEN
                                           +   LNK-SC90-KIZTEN
                                           +   LNK-SC90-FIRTEN
                                           +   LNK-SC90-SANTEN
                                           +   LNK-SC90-CHITEN
      *    ¸ºÅÀÊ¬
           IF      FLG-GENTEN          =   1
               MOVE    1               TO  LNK-COM-PLUSKBN (IDX-MAX)
           END-IF
      *
           .
       200-TENSUKEI-EXT.
           EXIT.
      *****************************************************************
      *    ´ïºàÊÔ½¸½èÍý
      *****************************************************************
       2004-KIZAI-SET-SEC            SECTION.
      *
           EVALUATE    LNK-COM-TOKUKIZAISBT1 (IDX)
               WHEN    2
                   IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            ÃâÁÇ
      *                (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)¡¡»Í¼Î¸ÞÆþ
                       COMPUTE WRK-SYOKEIS2    =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                       MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                       ADD     WRK-SYOKEIS         TO  WRK-CHISOKEI
                   END-IF
      *            »ÀÁÇ
                   IF      LNK-COM-SANSOKBN (IDX)  =   1
      *                (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)
                       COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX)
                       ADD     WRK-SYOKEIS2        TO  WRK-SANSOKIN-SUM
                   END-IF
           END-EVALUATE
      *
           .
       2004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ïºàÅÀ¿ô·×»»½èÍý¡ÊÃâÁÇ¡Ë
      *****************************************************************
       2007-CHITEN-KEI-SEC             SECTION.
      *
      *    ¶â³Û·×¡à£±£°¡¡»Í¼Î¸ÞÆþ
           COMPUTE WRK-SYOKEIS2        =  (WRK-CHISOKEI
                                       /   10  )
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-CHITEN
      *
           .
       2007-CHITEN-KEI-SEC-EXT.
           EXIT.
      *****************************************************************
      *     »ÀÁÇ¤ÎÅÀ¿ô¿ô·×»»½èÍý
      *****************************************************************
       2008-SANSO-KEI-SEC                SECTION.
      *
           MOVE      ZERO              TO  LNK-SC90-SANTEN
      *
      *    »ÀÁÇËè¤ÎÊäÀµÎ¨²Ã»»¤È¤¹¤ë
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
           MOVE    ZERO                TO  IDX-H
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   IDX-MAX )
               EVALUATE    LNK-COM-INPUTCD (IDX)
      *            »ÀÁÇÊäÀµÎ¨¼«Æ°²Ã»»
                   WHEN    "770020070"
                       MOVE    IDX             TO  IDX-H
      *            ¹âµ¤°µ»ÀÁÇ·×»»
                   WHEN    "770030070"
                       MOVE    IDX             TO  IDX-K
               END-EVALUATE
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-SYOKEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-MAX )
               MOVE    ZERO                TO  WRK-SANSOKIN
               MOVE    ZERO                TO  WRK-SANSOGOK
      *        »ÀÁÇ
               IF     (LNK-COM-INPUTCD (IDX)(1:1)  =   "7"  ) AND
                      (LNK-COM-TENSIKIBETU (IDX)   =   1
                                                   OR  2    ) AND
                      (LNK-COM-TOKUKIZAISBT1(IDX)  =   2    ) AND
                      (LNK-COM-SANSOKBN (IDX)      =   1    )
      *            (¹ØÆþ²Á³Ê¡ö¿ôÎÌ)
                   COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                           *   LNK-COM-SURYO  (IDX)
      *            »ÀÁÇ¶â³Û¡Ü¡Ê»ÀÁÇ¶â³Û¡ßÊäÀµÎ¨¡¿£±£°£°¡Ë¡¡»Í¼Î¸ÞÆþ
                   COMPUTE WRK-SYOKEIS3    =   WRK-SYOKEIS2
                                           *   LNK-COM-KISOTEN (IDX-H)
                                           /   100
                   COMPUTE WRK-SANSOKIN    =   WRK-SYOKEIS2
                                           +   WRK-SYOKEIS3
      *            ¹âµ¤°µ»ÀÁÇ·×»»
                   IF      IDX-K           >   ZERO
      *                »ÀÁÇ¶â³Û¡Ü¡Ê»ÀÁÇ¶â³Û¡ßÊäÀµÎ¨¡¿£±£°£°¡Ë
      *                                           ¡ßµ¤°µ¿ô¡¡»Í¼Î¸ÞÆþ
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                               *   LNK-COM-SURYO (IDX-K)
                       MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
                   END-IF
      *            ¶â³Û¡¡»Í¼Î¸ÞÆþ
                   COMPUTE WRK-SANSOGOK        =   WRK-SANSOKIN
                                               +   0.5
                   COMPUTE WRK-SYOKEI      =   WRK-SYOKEI
                                           +   WRK-SANSOGOK
               END-IF
           END-PERFORM
      *
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   IDX-MAX
      *        EVALUATE    LNK-COM-INPUTCD (IDX)
      *            ¹âµ¤°µ»ÀÁÇ·×»»
      *            WHEN    "770030070"
      *                »ÀÁÇ¶â³Û¡Ü¡Ê»ÀÁÇ¶â³Û¡ßÊäÀµÎ¨¡¿£±£°£°¡Ë
      *                                           ¡ßµ¤°µ¿ô¡¡»Í¼Î¸ÞÆþ
      *                COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
      *                                        *   LNK-COM-SURYO (IDX)
      *                MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
      *        END-EVALUATE
      *    END-PERFORM
      *    »ÀÁÇ¤ÎÅÀ¿ô¿ô·×»»
      *    ¶â³Û·×¡¡»Í¼Î¸ÞÆþ
      *    COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
      *                                +   0.5
      *    ¶â³Û·×¡à£±£°¡¡»Í¼Î¸ÞÆþ
           COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *    »ÀÁÇ¤ÎÅÀ¿ô¤ò´ïºàÅÀ¿ô¤Ë²Ã»»
           MOVE    WRK-SANTEN          TO  LNK-SC90-SANTEN
      *
           .
       2008-SANSO-KEI-EXT.
           EXIT.
      *
      *
