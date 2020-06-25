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
       PROGRAM-ID.                 ORCSC70200203.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯¡¢»»Äê¡Ê²èÁü¿ÇÃÇ¡Ë
      *                      £È£±£´¡¥£´²þÀµ¤Þ¤Ç
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  00/12/01    MCC¡ÝÂ¿¡¹Ç¼   ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     MCC-Â¿¡¹Ç¼   01/05/23  ¥Õ¥£¥ë¥àÎÁ¡¢ÌôºÞÎÁ·×»»½¤Àµ
      * 01.00.02     MCC-Â¿¡¹Ç¼   02/03/18  »ÜÀßÉÔÅ¬¹çÄÉ²Ã
      * 01.00.19     NACL-Â¿¡¹Ç¼  05/12/14  MONFUNC ÂÐ±þ
      * 03.05.00     NACL-Â¿¡¹Ç¼  07/04/XX  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
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
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ADD             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-BUIKBN          PIC 9(01).
           03  FLG-JIKASAN         PIC 9(01).
      *    ÆýÍÄ»ù²Ã»»Í­¤ê
           03  FLG-NYUYOJI         PIC 9(01).
      *    Â¤±ÆºÞ²Ã»»Í­¤ê
           03  FLG-ZOEIZAI         PIC 9(01).
      *    ¼«Æ°ÄÉ²Ã¼ïÊÌ
           03  FLG-SATU             PIC 9(01).
      *    £Ã£ÔÍ­¤ê
           03  FLG-XSEN             PIC 9(01).
           03  FLG-KAKUI            PIC 9(01).
           03  FLG-CTMRI            PIC 9(01).
      *
           03  FLG-SINDAN           PIC 9(01).
      *
           03  FLG-CHK              PIC 9(01).
      *
           03  FLG-SYORI            PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SAT             PIC 9(04).
           03  IDX-SAT2            PIC 9(04).
           03  IDX-SAT3            PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-KAS             PIC 9(04).
      *
           03  IDX-GEN             PIC 9(04).
           03  IDX-GEN2            PIC 9(04).
           03  IDX-NYU             PIC 9(04).
           03  IDX-GAI             PIC 9(04).
      *
           03  IDX-9               PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-CT              PIC 9(04).
           03  IDX-BUI             PIC 9(04).
      *    £Í£Á£Ø¹Ô
           03  IDX-MAX             PIC 9(04).
      *    £ØÀþ¤Î³«»Ï¹Ô
           03  IDX-HOZ             PIC 9(04).
      *
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-MOJI            PIC X(01). 
           03  WRK-SRYCD           PIC X(09). 
           03  WRK-SRYCD-2         PIC X(09). 
           03  WRK-HEN-SRYCD       PIC X(09). 
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSU           PIC 9(08)V99.
           03  WRK-TENSU1          PIC 9(08).
           03  WRK-IDX             PIC 9(04).
      *
      *    »£±ÆÎÁ¤ÎÅÀ¿ô·×
           03  WRK-TENSUKEI1       PIC S9(08)V999.
      *    ¿ÇÃÇÎÁ¤ÎÅÀ¿ô·×
           03  WRK-TENSUKEI2       PIC S9(08)V999.
      *    ¤½¤ÎÂ¾¤ÎÅÀ¿ô·×
           03  WRK-TENSUKEI3       PIC S9(08)V999.
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEIS3        PIC 9(08)V999.
           03  WRK-SYOKEI          PIC 9(08).
      *    ÅÀ¿ô·×»»ÍÑ
           03  WRK-KISOTEN-9           PIC 9(07)V9(03).
           03  WRK-KISOTEN             PIC 9(07).
           03  WRK-KISOTEN-G           PIC 9(07).
      *    Äþ¸ºÅÀ¿ô
           03  WRK-GEN-TEN             PIC 9(07)V9(03).
           03  WRK-GENTEN              PIC 9(07)V9(03).
      *    »£±ÆÎÁ²ó¿ô
           03  WRK-CNT-SATUEI          PIC 9(03).
      *
      *    ÆýÍÄ»ù²Ã»»ÍÑ¥ï¡¼¥¯
           03  WRK-NYUYOJI-AREA.
               05  WRK-NYU-BUI     PIC X(01).
               05  WRK-NYU-CD      PIC X(01).
               05  WRK-NYU-FIR     PIC X(01).
               05  WRK-NYU-IDX     PIC 9(04).
               05  WRK-NYU-TEN     PIC 9(08).
      *
               05  WRK-FIRTEN      PIC 9(08).
      *
           03  WRK-TENSUKEIS       PIC 9(08)V999.
           03  WRK-TENSUKEI-9      PIC S9(08).
           03  WRK-TENSUKEI-9G     PIC S9(08).
           03  WRK-TENSUKEI-G      PIC S9(08).
      *
           03  WRK-FIRTEN-KEI      PIC 9(08).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
      *
      *   ¤­¤¶¤ßÃÍ¹Ô°ÙºÆ·×»»ÍÑ
           03  WRK-SATU-TEN        PIC 9(08)V9(03).
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
           03  WRK-KZMTEN          PIC 9(08)V9(03).
      *    »£±ÆÎÁ¤Î¿ôÎÌ¡Ê¿ÇÃÇÎÁ¤Ø¡Ë
           03  WRK-SATUEI-SURYO    PIC 9(05)V9(03).
      *
      *    ¸º¥³¡¼¥É
           03 WRK-GEN-SRYCD        PIC X(09).
      *
      *    ·Ù¹ð¥á¥Ã¥»¡¼¥¸
           03  WRK-KIDCD           PIC X(04).
      *
           03  WRK-ERRCD           PIC X(04).
      *    ÊÝÂ¸ÍÑ
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *   £ØÀþÆ±»þÊ»»Ü¡Ê¿ÇÃÇ¡¦»£±Æ¡Ë
       01  TBL-DOUJI-HEISI-AREA.
      *    ÆÃ¼ì»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170005530".
           03  FILLER              PIC X(09)  VALUE   "170005630".
      *    Æ°ÂÖ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170005730".
           03  FILLER              PIC X(09)  VALUE   "170005830".
      *    ½ÅÊ£»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170006130".
           03  FILLER              PIC X(09)  VALUE   "170006230".
      *    Î©ÂÎ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170006330".
           03  FILLER              PIC X(09)  VALUE   "170006430".
      *    ¥Ñ¥ó¥È¥â¥°¥é¥Õ¥£¡¼
           03  FILLER              PIC X(09)  VALUE   "170006530".
           03  FILLER              PIC X(09)  VALUE   "170006630".
      *    ¥Ñ¥Î¥é¥é¥Þ£ØÀþ
           03  FILLER              PIC X(09)  VALUE   "170006730".
           03  FILLER              PIC X(09)  VALUE   "170006830".
      *    ÃÇÁØ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170006930".
           03  FILLER              PIC X(09)  VALUE   "170007030".
      *    Æ±»þÂ¿ÁØ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007130".
           03  FILLER              PIC X(09)  VALUE   "170007200".
      *    ²óÅ¾²£ÃÇ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007330".
           03  FILLER              PIC X(09)  VALUE   "170007430".
      *    ¥¹¥Ý¥Ã¥È»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007530".
           03  FILLER              PIC X(09)  VALUE   "170007630".
      *    ÁÀ·â»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007730".
           03  FILLER              PIC X(09)  VALUE   "170007830".
      *    ¶ÊÌÌÃÇÁØ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170007930".
           03  FILLER              PIC X(09)  VALUE   "170008030".
      *    »ùÆ¬¹üÈ×ÉÔ¶Ñ¹Õ»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170008430".
           03  FILLER              PIC X(09)  VALUE   "170008530".
      *    Ã±½ã»£±Æ¡¦ÆÃ¼ì»£±Æ
           03  FILLER              PIC X(09)  VALUE   "170001910".
           03  FILLER              PIC X(09)  VALUE   "170002010".
      *
       01  TBL-DOUJI-HEISI-R       REDEFINES  TBL-DOUJI-HEISI-AREA.
           03  TBL-DOUJI-OCC       OCCURS   14  INDEXED  TBL-IDX.
               05  TBL-DOUJI-1     PIC X(09).
               05  TBL-DOUJI-2     PIC X(09).
      *
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê»ÜÀß´ð½à¡Ë
           COPY    "CPSK1006.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÊÔ½¸¥Ñ¥é¥á¥¿
           COPY    "CPORCSC90.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÎò¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSC91.INC".
      *
      *    ¿ÇÎÅ¹Ô°ÙÅÀ¿ôÊÔ½¸¡¦´ðËÜ¥Á¥§¥Ã¥¯
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    ¿ÇÎÅ¹Ô°Ù»»ÄêÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           EVALUATE    LNK-ORCSC-KBN
      *        ºÞÊÌ¡¡¥Á¥§¥Ã¥¯¡¢ÊÔ½¸¡¢¼«Æ°»»Äê
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        Á´ÂÎ¤Î¥Á¥§¥Ã¥¯
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        ¼«Æ°»»Äê
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ºÞÊÌ¤Î¥Á¥§¥Ã¥¯¡¢ÊÔ½¸¡¢¼«Æ°»»Äê½èÍý
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    ¥Á¥§¥Ã¥¯¡¢¼«Æ°»»Äê
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    ÅÀ¿ô·×»»
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌÀºÙÊÌÆâÍÆ¥Á¥§¥Ã¥¯¡¢¼«Æ°»»ÄêÊÔ½¸½èÍý
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-BUI
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  IDX-SAT
           MOVE    ZERO                TO  IDX-GEN
           MOVE    ZERO                TO  IDX-GEN2
           MOVE    ZERO                TO  IDX-NYU
           MOVE    1                   TO  IDX-HOZ
      *
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  FLG-SATU
           MOVE    ZERO                TO  FLG-BUIKBN
           MOVE    ZERO                TO  FLG-ZOEIZAI
           MOVE    ZERO                TO  FLG-SINDAN
           INITIALIZE                  WRK-NYUYOJI-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )  OR
                              (WRK-ERRCD       NOT =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                       CONTINUE
      *
                   WHEN    OTHER
      *                ÊÔ½¸¥Á¥§¥Ã¥¯¤Ø
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
           IF      IDX-SAT             >   ZERO
      *        ¼«Æ°²Ã»»¤òÄÉ²Ã¤¹¤ë
               PERFORM 1002-JIDO-KASAN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KIDCD           TO  WRK-ERRCD
           END-IF
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌÀºÙÊÌÆâÍÆ¥Á¥§¥Ã¥¯¡¢¼«Æ°»»ÄêÊÔ½¸½èÍý
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    ³Æ¥Á¥§¥Ã¥¯½èÍý
           EVALUATE    WRK-MOJI
               WHEN    "0"
      *            Éô°Ì¥³¡¼¥É¥Á¥§¥Ã¥¯
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3)   =   "002"
                       IF      FLG-BUIKBN      NOT =   ZERO
                           MOVE    "0011"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z)
                       ELSE
                           EVALUATE    WRK-COM-BUIKBN (IDX-Z)
      *                        Æ¬Éô
                               WHEN    "1"
      *                        ¶í´´
                               WHEN    "2"
                                   MOVE    2       TO  FLG-BUIKBN
      *                        ¶»Éô¡¦Ê¢Éô
                               WHEN    "5"
                               WHEN    "6"
                                   MOVE    2       TO  FLG-BUIKBN
                                   MOVE    "1"     TO  WRK-NYU-BUI
      *                        ÀÔ¿ñ
                               WHEN    "7"
                                   MOVE    2       TO  FLG-BUIKBN
      *                        ¾Ã²½´ï
                               WHEN    "8"
                                   MOVE    3       TO  FLG-BUIKBN
      *                        ¤½¤ÎÂ¾
                               WHEN    OTHER
                                   MOVE    1       TO  FLG-BUIKBN
                           END-EVALUATE
                       END-IF
                       MOVE    IDX-Z           TO  IDX-BUI
                   END-IF
      *
               WHEN    "1"
      *            »£±ÆÎÁ
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)
                           WHEN    "721"
      *                        £ØÀþ»£±ÆÎÁ
      *                        Éô°Ì¶èÊ¬¤¬¤¢¤ë¤³¤È
                               IF      FLG-BUIKBN      =   ZERO
                                    MOVE    "0036"     TO  WRK-ERRCD
                                    MOVE    "1"        TO  WRK-COM-CUR
                                                                (IDX-Z)
                                    GO      TO      10011-CHK-EXT
                               END-IF
      *
      *                        ²ó¿ô¥Á¥§¥Ã¥¯
                               IF      WRK-CNT-SATUEI      =   2
                                    MOVE    "0072"     TO  WRK-ERRCD
                                    MOVE    "1"        TO  WRK-COM-CUR
                                                                (IDX-Z)
                                    GO      TO      10011-CHK-EXT
                               END-IF
                               IF      FLG-SINDAN          =   1
                                   PERFORM 1002-XSEN-END-SEC
                               ELSE
                                   IF      FLG-SINDAN  NOT =   ZERO
                                       MOVE    "0071"  TO  WRK-ERRCD
                                       MOVE    "1"     TO  WRK-COM-CUR
                                                                (IDX-Z)
                                       GO      TO  10011-CHK-EXT
                                   END-IF
      *                            Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯Åù
                                   PERFORM 10013-DOUJI-CHK-SEC
                               END-IF
                               MOVE    1               TO  FLG-SINDAN
                               MOVE    IDX-Z           TO  IDX-SAT
                               ADD     1           TO  WRK-CNT-SATUEI
                           WHEN    "722"
      *                        ³Ë°å³Ø¿ÇÃÇÎÁ
                               IF      FLG-SINDAN      NOT =   ZERO
                                   MOVE    "1"         TO  WRK-COM-CUR
                                                                (IDX-Z)
                                   MOVE    "0071"      TO  WRK-ERRCD
                               ELSE
                                   MOVE    2           TO  FLG-SINDAN
                                   MOVE    IDX-Z       TO  IDX-SAT
                               END-IF
      *                       £Ã£Ô¿ÇÃÇÎÁ
                           WHEN    "723"
                               IF      FLG-SINDAN      NOT =   ZERO
                                   MOVE    "1"         TO  WRK-COM-CUR
                                                                (IDX-Z)
                                   MOVE    "0071"      TO  WRK-ERRCD
                               ELSE
                                   MOVE    3           TO  FLG-SINDAN
                                   MOVE    IDX-Z       TO  IDX-SAT
                                   MOVE    1           TO  FLG-CTMRI
                               END-IF
      *                       £Í£Ò£É¿ÇÃÇÎÁ
                           WHEN    "724"
                               IF      FLG-SINDAN      NOT =   ZERO
                                   MOVE    "1"         TO  WRK-COM-CUR
                                                                (IDX-Z)
                                   MOVE    "0071"      TO  WRK-ERRCD
                               ELSE
                                   MOVE    3           TO  FLG-SINDAN
                                   MOVE    IDX-Z       TO  IDX-SAT
                                   MOVE    2           TO  FLG-CTMRI
                               END-IF
      *--(01.00.01)----- START
      *                        »£±ÆÎÁ
                           WHEN    "711"
                           WHEN    "712"
                           WHEN    "713"
                           WHEN    "714"
                               CONTINUE
      *                        ¿ÇÃÇÎÁ
      ******************** WHEN    OTHER
                           WHEN    "700"
      *                        ¤½¤ÎÂ¾¡Ê¤É¤Î¿ÇÃÇÎÁ¤Ë¤âÉÕ¿ï¤¹¤ë¡Ë
                               IF      FLG-SINDAN          =   4
                                   PERFORM 1004-ETC-END-SEC
                               ELSE
                                   IF      FLG-SINDAN  NOT =   ZERO
                                       MOVE    "0071"  TO  WRK-ERRCD
                                       MOVE    "1"     TO  WRK-COM-CUR
                                                                (IDX-Z)
      *********************************GO      TO      10011-CHK-EXT
                                   END-IF
                                   GO      TO      10011-CHK-EXT
                               END-IF
                               MOVE    4               TO  FLG-SINDAN
                               MOVE    IDX-Z           TO  IDX-SAT
      *--(01.00.01)----- END  
                       END-EVALUATE
                   END-IF
      *            ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸ºÍ­¤ê¡Ê´ð½àÉÔÅ¬¹çÄþ¸º¶èÊ¬°Ê³°
                   IF     (WRK-COM-TENSIKIBETU (IDX-Z) =   6 )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                                       =   ZERO ) AND
                          (WRK-COM-AUTOKBN     (IDX-Z) =   SPACE )
                       MOVE    IDX-Z               TO  IDX-GEN
                   END-IF
      ************ IF      WRK-COM-INPUTCD (IDX-Z) =   "170020570"
      *                MOVE    IDX-Z               TO  IDX-GEN
      ************ END-IF
      *            ¼êµ»ÎÁ ²Ã»»¥Á¥§¥Ã¥¯
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 10011-KASAN-CHK-SEC
                   END-IF
      *            Ã±½ã»£±ÆÈ½Äê¡ÊÆýÍÄ»ù²Ã»»¡Ë¤ÎÍ­Ìµ
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "170001910"  OR
                                                       "170011650"
                       MOVE    "1"             TO  WRK-NYU-CD
                   END-IF
      *            ÆýÍÄ»ù²Ã»»¡ÊÅÀ¿ô)
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       MOVE    IDX-Z               TO  IDX-SYU
                       PERFORM 1007-NYUYOJI-SET-SEC
                   END-IF
      *        ÌôºÞ
               WHEN    "6"
      *            Â¤±ÆºÞ
                   IF      WRK-COM-ZOEIZAIKBN(IDX-Z)   =   1
                       MOVE   1                TO  FLG-ZOEIZAI
                   END-IF
      *            ËãÆÇ¶èÊ¬
                   IF      WRK-COM-MADOKUKBN (IDX-Z)   =   1
                       MOVE   1                TO  FLG-MADOKU
                   END-IF
      *            À¸Êª³ØÅªÀ½ºÞ¶èÊ¬
                   IF      WRK-COM-SEIBUTUKBN(IDX-Z)   =   1
                       MOVE   1                TO  FLG-SEIBETU
                   END-IF
      *        ´ïºà
               WHEN    "7"
      *            ¥Õ¥£¥ë¥àÈ½Äê
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "3"
      *****************ÆÃÄê´ïºàÇ¯Îð²Ã»»¶èÊ¬È½Äê
      *                IF      WRK-COM-TOKUKIZAIAGEKSNKBN (IDX-Z)
      *                                                =   1
      *                    MOVE    "1"                 TO  WRK-NYU-FIR
      *                    ADD     LNK-SC90-FIRTEN     TO  WRK-NYU-TEN
      *                ELSE
      *                    ADD     LNK-SC90-FIRTEN     TO  WRK-FIRTEN
      *                END-IF
      *****************MOVE    IDX-Z           TO  WRK-NYU-IDX
      *                £ØÀþ¤Î¿ôÎÌÊÑ¹¹¤Î°Ù¤ÎËç¿ô½¸·×
                       IF      WRK-COM-BUNGSU (IDX-Z)  =  ZERO
                           MOVE    1               TO  WRK-COM-BUNGSU
                                                               (IDX-Z)
                       END-IF
                       COMPUTE WRK-KEIMAI      =   WRK-KEIMAI
                                               +  (WRK-COM-SURYO(IDX-Z)
                                               *   WRK-COM-BUNGSU
                                                               (IDX-Z))
                   END-IF
      *
      *            »ÀÁÇ¡¦ÃâÁÇ¤Ï¥¨¥é¡¼
                   IF      WRK-COM-TOKUKIZAISBT1 (IDX-Z) =   2
                       MOVE    "0039"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°²Ã»»ÄÉ²Ã½èÍý
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
      *    £ØÀþ¤Î»þ¡¢¥¨¥é¡¼¤Ê¤é¿ÇÃÇÎÁ¤Î¼«Æ°È¯À¸¤ò¤ª¤³¤Ê¤¦
           IF     (FLG-SINDAN          =   1     )  AND
                  (WRK-ERRCD       NOT =   SPACE )
               PERFORM 10001-XSEN-SINDAN-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-HEN-SRYCD
           IF      FLG-SINDAN          =   3
      *        £Ã£Ô¡¥£Í£Ò£É¤Î»»ÄêÍÑ¥³¡¼¥ÉÊÔ½¸
               MOVE    IDX-SAT             TO  IDX-Z
               PERFORM 10023-CTMRIHEN-SEC
           END-IF
      *
           IF     (FLG-SINDAN          =   3     )  AND
                  (LNK-ORCSC-PG        =   "K02" )  AND
                  (LNK-ORCSC-SYORIFLG  =   ZERO  )  AND
                  (WRK-HEN-SRYCD   NOT =   SPACE )
      *        £Ã£Ô¡¥£Í£Ò£É¤Î¼êµ»ÎÁÊÑ¹¹¥Á¥§¥Ã¥¯
               PERFORM 10012-SYUGI-CHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      1002-JIDO-KASAN-EXT
               END-IF
           END-IF
      *
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î»»ÄêÍÑ¥³¡¼¥ÉÊÑ´¹½èÍý
      *****************************************************************
       10023-CTMRIHEN-SEC                  SECTION.
      *
      *
           MOVE    SPACE               TO  WRK-HEN-SRYCD
      *    Êñ³çÄþ¸º¶èÊ¬È½Äê¡Ê£Ë£°£³¤Ç¤âÆ±ÍÍ¤ÎÈ½Äê¤ò¤·¤Æ¤¤¤ë¡Ë
           EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *        Æ¬Éô
               WHEN    "201"
                   MOVE    "099700101"         TO  WRK-HEN-SRYCD
      *        ¶í´´
               WHEN    "202"
                   MOVE    "099700102"         TO  WRK-HEN-SRYCD
      *        »Í»è
               WHEN    "203"
                   MOVE    "099700103"         TO  WRK-HEN-SRYCD
           END-EVALUATE
           IF      WRK-HEN-SRYCD       =   SPACE
               MOVE    SPACE           TO  WRK-COM-CHK-SRYCD (IDX-Z)
           ELSE
               MOVE    WRK-HEN-SRYCD   TO  WRK-COM-CHK-SRYCD (IDX-Z)
           END-IF
      *
           .
       10023-CTMRIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î¼êµ»ÎÁÊÑ¹¹¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10012-SYUGI-CHK-SEC                  SECTION.
      *
           MOVE    IDX-SAT             TO  IDX-Z
      *
      *****MOVE    SPACE               TO  WRK-HEN-SRYCD
      *    Êñ³çÄþ¸º¶èÊ¬È½Äê¡Ê£Ë£°£³¤Ç¤âÆ±ÍÍ¤ÎÈ½Äê¤ò¤·¤Æ¤¤¤ë¡Ë
      *    EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *        Æ¬Éô
      *        WHEN    "201"
      *            MOVE    "099700101"         TO  WRK-HEN-SRYCD
      *        ¶í´´
      *        WHEN    "202"
      *            MOVE    "099700102"         TO  WRK-HEN-SRYCD
      *        »Í»è
      *        WHEN    "203"
      *            MOVE    "099700103"         TO  WRK-HEN-SRYCD
      *    END-EVALUATE
      *    IF      WRK-HEN-SRYCD       =   SPACE
      *        GO      TO      10012-SYUGI-CHK-EXT
      *    ELSE
      *        MOVE    WRK-HEN-SRYCD   TO  WRK-COM-CHK-SRYCD (IDX-Z)
      *****END-IF
      *
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯
           INITIALIZE                      ORCSC91AREA
           MOVE    "3"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-HEN-SRYCD       TO  LNK-SC91-SRYCD
           MOVE    IDX-Z               TO  LNK-SC91-GMN-IDX
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"       USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "0001"
               MOVE    LNK-SC91-ERRCD      TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               GO      TO      10012-SYUGI-CHK-EXT
           END-IF
      *
      *    Åö·î»»Äê¤Ê¤·
           IF      LNK-SC91-CNT        =   ZERO
      *
      *        £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯
               IF      WRK-COM-INPUTCD (IDX-Z)     =   "170022290"  OR
                                                       "170022390"  OR
                                                       "170022490"  OR
                                                       "170701470"  OR
                                                       "170701570"  OR
                                                       "170701670"
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )  AND 
                          (LNK-ORCSC-TOROKU    NOT =   "2"   )
                       MOVE    "K003"              TO  WRK-KIDCD
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                             (IDX-Z)
                       MOVE    "2"                 TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
               END-IF
           ELSE
      *        Êñ³çÄþ¸º¶èÊ¬È½Äê
               EVALUATE    WRK-COM-HOUKATUTEIGENKBN (IDX-Z)
      *            Æ¬Éô
                   WHEN    "201"
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "170022290"
                                                       OR  "170701470"
                           CONTINUE
                       ELSE
      *                    ¡Ê°ìÈÌ¡Ë
                           MOVE    "170022290"     TO  WRK-COM-INPUTCD
                                                               (IDX-Z)
                       END-IF
      *            ¶í´´
                   WHEN    "202"
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "170022390" 
                                                       OR  "170701570"
                           CONTINUE
                       ELSE
      *                    ¡Ê°ìÈÌ¡Ë
                           MOVE    "170022390"     TO  WRK-COM-INPUTCD
                                                                (IDX-Z)
                       END-IF
      *            »Í»è
                   WHEN    "203"
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "170022490" 
                                                       OR  "170701670"
                           CONTINUE
                       ELSE
      *                    ¡Ê°ìÈÌ¡Ë
                           MOVE    "170022490"     TO  WRK-COM-INPUTCD
                                                               (IDX-Z)
                       END-IF
               END-EVALUATE
      *        ¿ÇÎÅ¥³¡¼¥ÉÊÑ¹¹
               MOVE    1                   TO  FLG-ARI
               MOVE    IDX-Z               TO  IDX-T
               MOVE    WRK-COM-INPUTCD(IDX-Z)
                                           TO  WRK-SRYCD
               PERFORM 1002-JIDOUSET-SEC
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-Z)
               IF      FLG-ADD             =   1
      *            ÊÑ¹¹¤Î¿ÇÎÅ¥³¡¼¥É¤ò¥»¥Ã¥È
                   MOVE    WRK-HEN-SRYCD   TO  WRK-COM-CHK-SRYCD(IDX-S)
               END-IF
           END-IF
           .
       10012-SYUGI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»ÎÁ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10011-KASAN-CHK-SEC                  SECTION.
      *
      *    ²Ã»»¥Ç¡¼¥¿¤Î»þ¡¢Á°¤Ë¼êµ»ÎÁ¤¬¤¢¤ë¤³¤È
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       10011-KASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       10013-DOUJI-CHK-SEC           SECTION.
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH      TBL-DOUJI-OCC   VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-Z) =   TBL-DOUJI-2
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-1 (TBL-IDX)
                                                TO  WRK-SRYCD
           END-SEARCH
      *
      *    ÆÃ¼ì»£±Æ°Ê³°¥¨¥é¡¼
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "170002010" )  OR
                  (WRK-SRYCD               =   SPACE )
               CONTINUE
           ELSE
               MOVE    "0070"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
           .
      *
       10013-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ØÀþ»£±ÆÎÁ½ªÎ»½èÍý
      *****************************************************************
       1002-XSEN-END-SEC           SECTION.
      *
      *    Æ±»þÊ»»Ü¤Î¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH      TBL-DOUJI-OCC   VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-Z) =   TBL-DOUJI-1
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-2 (TBL-IDX)
                                                TO  WRK-SRYCD
           END-SEARCH
      *
      *    Ã±½ã»£±ÆÎÁ¤Î£²²óÌÜ¤òÆÃ¼ì»£±ÆÎÁ¤ØÊÑ´¹
           IF      WRK-SRYCD       NOT =   SPACE
      *        ¿ÇÎÅ¥³¡¼¥ÉÊÑ¹¹
               MOVE    1                   TO  FLG-ARI
               MOVE    IDX-Z               TO  IDX-T
               PERFORM 1002-JIDOUSET-SEC
           END-IF
      *    »£±Æ¤ÎÀÚ¤êÂØ¤¨¥Õ¥é¥°
           MOVE    "K"                 TO  WRK-COM-AUTOKBN (IDX-Z)
      *
      *    ¿ÇÃÇÎÁ²Ã»»ÄÉ²Ã
           MOVE    1                   TO  IDX-HOZ
           PERFORM 10001-XSEN-SINDAN-SEC
      *
           MOVE    ZERO                TO  IDX-SAT
           MOVE    ZERO                TO  IDX-GEN
           MOVE    ZERO                TO  LNK-SC90-BUNGSU
           MOVE    ZERO                TO  LNK-SC90-BUNGSU
           MOVE    ZERO                TO  WRK-KEIMAI
      *    ¥Õ¥£¥ë¥àÆýÍÄ»ù²Ã»»ÎÎ°è¥¯¥ê¥¢
           INITIALIZE                  WRK-NYUYOJI-AREA
      *
      *    ¸½ºß¤Î¥Á¥§¥Ã¥¯°ÌÃÖ¤ËÌá¤¹
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-COM-AUTOKBN (IDX)   =   "K"
                   MOVE    SPACE           TO  WRK-COM-AUTOKBN (IDX)
                   MOVE    IDX             TO  IDX-Z
                   MOVE    SPA-MAX-LINE    TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    IDX-Z               TO  IDX-HOZ
           .
       1002-XSEN-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ØÀþ»£±ÆÎÁºîÀ®½èÍý
      *****************************************************************
       10001-XSEN-SINDAN-SEC           SECTION.
      *
      *    Äþ¸º¤¢¤ê
           IF      IDX-GEN2            >   ZERO
               IF      IDX-SAT         =  (IDX-GEN2 -  1 )
                   COMPUTE IDX-SAT2    =   IDX-SAT  +  1
               ELSE
                   MOVE    IDX-SAT         TO  IDX-SAT2
               END-IF
           ELSE
               MOVE    IDX-SAT         TO  IDX-SAT2
           END-IF
      *
           MOVE    IDX-SAT2            TO  IDX-S
           PERFORM 100012-SINDAN-SET-SEC
      *
      *    »£±ÆÎÁ¤ÎºÆ·×»»
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KISOTEN-G
      *    Ëç¿ô·×»»
      *****COMPUTE WRK-KEIMAI          =   LNK-SC90-BUNGSU
      *                                *   LNK-SC90-MAISU
      *****MOVE    LNK-SC90-MAISU      TO  WRK-KEIMAI
           INITIALIZE                  ORCSC90AREA
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX-Z   FROM    IDX-HOZ BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE ) OR
                              (WRK-COM-AUTOKBN(IDX-Z)  =   "K"   )
      *
               MOVE    IDX-Z               TO  IDX-MAX
      *        ÌÀºÙ·×»»
               PERFORM 100013-XSEN-KEI-SEC
      *
               IF      WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1"
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "1"
                       EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)(1:2)
                           WHEN    "72"
      *                    »£±ÆÎÁÊÔ½¸
                               MOVE    ZERO                TO  FLG-SATU
                               PERFORM 100011-STATUEI-RYO-SEC
                           WHEN    "71"
      *                    ¿ÇÃÇÎÁ
                               MOVE    1                   TO  FLG-SATU
                               PERFORM 100011-STATUEI-RYO-SEC
      *                    ¤½¤ÎÂ¾¡Ê¼«Æ°ÄÉ²Ã¤Ï¿ÇÃÇÎÁ¤È¤¹¤ë¡Ë
                           WHEN    OTHER
                               IF      WRK-COM-AUTOKBN (IDX-Z)  =   "2"
                                   MOVE    1               TO  FLG-SATU
                               ELSE
                                   MOVE    ZERO            TO  FLG-SATU
                               END-IF
                               PERFORM 100011-STATUEI-RYO-SEC
                       END-EVALUATE
                   ELSE
                       IF      WRK-COM-SANFLG (IDX-Z) =   ZERO
      *                ²Ã»»¹àÌÜ
                           MOVE    IDX-Z          TO  IDX-T
                           PERFORM 10071-KASAN-KEI-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»
           PERFORM 1006-FIRTEN-SET-SEC
      *
           .
       10001-XSEN-SINDAN-EXT.
           EXIT.
      *
      *****************************************************************
      *   ÌÀºÙ·×»»½èÍý
      *****************************************************************
       100013-XSEN-KEI-SEC           SECTION.
      *
           IF     (IDX-SAT                 =   IDX-Z  )  AND
                  (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1" ) AND
                  (WRK-COM-DATAKBN (IDX-Z)         =   "1" )
      *        »£±ÆÎÁ¤òÄþ¸º¡¢ÆýÍÄ»ù²Ã»»¸å¤Ë¤­¤¶¤ßÃÍ·×»»¤ò¤¹¤ë
               PERFORM 1000131-SATUEI-KEI-SEC
           ELSE
      *        ¤­¤¶¤ßÃÍÈ½Äê¡Ê¿ÇÃÇÎÁ¡Ë
               IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1" ) AND
                      (WRK-COM-DATAKBN (IDX-Z)         =   "1" )
                   PERFORM 1000133-SINDAN-KEISAN-SEC
               END-IF
           END-IF
      *
      *    ÌÀºÙ·×»»
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
           MOVE    WRK-COM-INPUTCD (IDX-Z)(1:1)    TO  WRK-MOJI
           IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
               MOVE    "7"                     TO  WRK-MOJI
           END-IF
           EVALUATE    WRK-MOJI
      *        ´ïºà
               WHEN    "7"
      *            ¥Õ¥£¥ë¥à½¸·×
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "3"
      *                ÆÃÄê´ïºàÇ¯Îð²Ã»»¶èÊ¬È½Äê
                       IF      WRK-COM-TOKUKIZAIAGEKSNKBN (IDX-Z)
                                                       =   1
                           MOVE    "1"                 TO  WRK-NYU-FIR
                           ADD     LNK-SC90-FIRTEN     TO  WRK-NYU-TEN
                           MOVE    IDX-Z               TO  WRK-NYU-IDX
                       ELSE
                           ADD     LNK-SC90-FIRTEN     TO  WRK-FIRTEN
                       END-IF
                   END-IF
           END-EVALUATE
           .
       100013-XSEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     »£±ÆÎÁÊÔ½¸½èÍý
      *****************************************************************
       1000131-SATUEI-KEI-SEC          SECTION.
      *
           MOVE    WRK-COM-TEN(IDX-Z)  TO  WRK-SATU-TEN
           MOVE    ZERO                TO  WRK-SATUEI-SURYO
      *
           IF     (WRK-CNT-SATUEI      =   1  )  AND
                  (IDX-GEN             >   ZERO )
               PERFORM VARYING     IDX-Z2  FROM    IDX-SAT BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z2)  =   SPACE ) OR
                              (WRK-COM-AUTOKBN(IDX-Z2)  =   "K"   )
                   IF     (WRK-COM-TENSIKIBETU (IDX-Z2) =   6 )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX-Z2)
                                                       =   ZERO ) AND
                          (WRK-COM-AUTOKBN    (IDX-Z2) =   SPACE )
      *                ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»¡Ê¿ÇÃÇÎÁ¡Ë
                       COMPUTE WRK-GENTEN      =   WRK-SATU-TEN
                                           *  (WRK-COM-TEN  (IDX-Z2)
                                           /   100        )
                       COMPUTE WRK-SATU-TEN    =   WRK-SATU-TEN
                                               -   WRK-GENTEN
                       MOVE    1               TO  WRK-COM-SANFLG
                                                          (IDX-Z2)
                       MOVE    SPA-MAX-LINE    TO  IDX-Z2
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ÆýÍÄ»ù²Ã»»¡Ê¡ó²Ã»»¡Ë
           PERFORM VARYING     IDX-Z2  FROM    IDX-SAT BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z2)  =   SPACE ) OR
                              (WRK-COM-AUTOKBN(IDX-Z2)  =   "K"   )
              IF     (WRK-COM-SANFLG(IDX-Z2)  =   2  )  AND
                     (WRK-COM-TENSIKIBETU(IDX-Z2)  =   5  )
                   MOVE    ZERO                TO  WRK-TENSUKEIS
                   COMPUTE WRK-TENSUKEIS       =   WRK-SATU-TEN
                                               *   WRK-COM-TEN (IDX-Z2)
                                               /   100
                   MOVE    1               TO  WRK-COM-SANFLG(IDX-Z2)
                   COMPUTE WRK-SATU-TEN        =   WRK-SATU-TEN
                                               +   WRK-TENSUKEIS
                   MOVE    SPA-MAX-LINE        TO  IDX-Z2
               END-IF
           END-PERFORM
      *
      *    ¤­¤¶¤ßÃÍÈ½Äê
           IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1" ) AND
                  (WRK-COM-DATAKBN (IDX-Z)         =   "1" ) AND
                  (WRK-COM-KZMCOMPSIKIBETU (IDX-Z) NOT =   ZERO)
      *        ¤­¤¶¤ßÃÍ·×»»
               PERFORM 1000132-TENSU-KEISAN-SEC
      *        »£±ÆÎÁ¤Î¿ôÎÌ¤òÊÝÂ¸
               MOVE    WRK-COM-SURYO (IDX-Z)   TO  WRK-SATUEI-SURYO
           ELSE
      *        »£±ÆÎÁ
               COMPUTE WRK-TENSUKEI-9      =   WRK-SATU-TEN  + 0.5
               MOVE    WRK-TENSUKEI-9      TO  WRK-COM-KISOTEN(IDX-Z)
           END-IF
      *
           .
       1000131-SATUEI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     ¿ÇÃÇÎÁ¤½¤ÎÂ¾½èÍý
      *****************************************************************
       1000133-SINDAN-KEISAN-SEC          SECTION.
      *
           MOVE    WRK-COM-TEN  (IDX-Z)    TO  WRK-SATU-TEN
      *
      *    ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»
           IF     (WRK-CNT-SATUEI      =   1   )  AND
                  (WRK-COM-TNSSRYSYUKBN(IDX-Z)(1:2)  =  "71" ) AND
                  (IDX-GEN             >   ZERO )
               PERFORM VARYING     IDX-Z2  FROM    IDX-SAT BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z2)  =   SPACE ) OR
                              (WRK-COM-AUTOKBN(IDX-Z2)  =   "K"   )
                   IF     (WRK-COM-TENSIKIBETU (IDX-Z2) =   6 )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX-Z2)
                                                       =   ZERO ) AND
                          (WRK-COM-AUTOKBN    (IDX-Z2) =   SPACE )
      *                ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»¡Ê¿ÇÃÇÎÁ¡Ë
                       COMPUTE WRK-GENTEN      =   WRK-SATU-TEN
                                           *  (WRK-COM-TEN  (IDX-Z2)
                                           /   100        )
                       COMPUTE WRK-SATU-TEN    =   WRK-SATU-TEN
                                               -   WRK-GENTEN
                       MOVE    1               TO  WRK-COM-SANFLG
                                                          (IDX-Z2)
                       MOVE    SPA-MAX-LINE        TO  IDX-Z2
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ¤­¤¶¤ßÃÍÈ½Äê
           IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1" ) AND
                  (WRK-COM-DATAKBN (IDX-Z)         =   "1" ) AND
                  (WRK-COM-KZMCOMPSIKIBETU (IDX-Z) NOT =   ZERO)
      *        »£±ÆÎÁ¤Î¿ôÎÌ¤ò¿ôÎÌ¤Ø
               IF      WRK-SATUEI-SURYO    >   ZERO
                   MOVE    "1"             TO  WRK-COM-SURFLG (IDX-Z)
                   MOVE    WRK-SATUEI-SURYO    TO  WRK-COM-SURYO (IDX-Z)
               END-IF
      *        ¤­¤¶¤ßÃÍ·×»»
               PERFORM 1000132-TENSU-KEISAN-SEC
           ELSE
               COMPUTE WRK-TENSUKEI-9      =   WRK-SATU-TEN  + 0.5
               MOVE    WRK-TENSUKEI-9      TO  WRK-COM-KISOTEN(IDX-Z)
           END-IF
      *
      *
           .
       1000133-SINDAN-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *     ¤­¤¶¤ßÃÍ·×»»½èÍý
      *****************************************************************
       1000132-TENSU-KEISAN-SEC          SECTION.
      *
           MOVE    WRK-COM-INPUTCD (IDX-Z)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
      *    ¿ôÎÌÆþÎÏ¤Î¤Ê¤¤»þ,¿ôÎÌ¤òËç¿ô¤È¤¹¤ë
           IF      WRK-COM-SURFLG (IDX-Z) =   SPACE
               IF      WRK-KEIMAI          >   WRK-COM-KZMJGN
                                                           (IDX-Z)
                   MOVE    WRK-COM-KZMJGN (IDX-Z)
                                               TO  WRK-COM-SURYO
                                                           (IDX-Z)
               ELSE
                   IF      WRK-KEIMAI          =   ZERO
                       MOVE    1               TO  WRK-COM-SURYO
                                                           (IDX-Z)
                   ELSE
                       MOVE    WRK-KEIMAI      TO  WRK-COM-SURYO
                                                           (IDX-Z)
                   END-IF
                END-IF
           END-IF
      *
      *    ¤­¤¶¤ßÃÍ·×»»
           MOVE    WRK-COM-SURYO (IDX-Z)   TO  WRK-SURYO
           MOVE    ZERO                    TO  WRK-KEI-TEN2
      *    ¤­¤¶¤ßÅÀ¿ô¤ò»»ÄêÅÀ¿ô¤Î£±¡¿£²¤È¤¹¤ë
           COMPUTE WRK-KZMTEN          =   WRK-SATU-TEN   /  2
      *    ¤­¤¶¤ßÃÍÈ½Äê
           EVALUATE    TRUE
      *            ¥Ç¡¼¥¿¡å¡Ê²¼¸ÂÃÍ¡Ý¤­¤¶¤ßÃÍ¡Ë
               WHEN    WRK-SURYO       <=  (TNS-KZMKGN  -  TNS-KZM)
                   IF      TNS-KZMERR          =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   END-IF
      *
      *            ²¼¸ÂÃÍ¡ã¥Ç¡¼¥¿¡å¾å¸ÂÃÍ
               WHEN     (TNS-KZMKGN        <   WRK-SURYO )   AND
                        (TNS-KZMJGN        >=  WRK-SURYO )
      *            ÅÀ¿ô·×»»¼°£±
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO -  TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   WRK-SATU-TEN
                                           + ( WRK-KEI-TEN2
                                           *   WRK-KZMTEN )
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            ¾å¸ÂÃÍ¡ã¥Ç¡¼¥¿
               WHEN    TNS-KZMJGN        <   WRK-SURYO
                   IF      TNS-KZMERR        =   0  OR  2
      *            ÅÀ¿ô·×»»¼°£±¤ò¤»¤º¡¢¥¨¥é¡¼¤È¤¹¤ë
      ****             MOVE    "0022"        TO  SPA-ERRCD
      *            ·Ù¹ð¥¨¥é¡¼¤È¤¹¤ë
                       MOVE    "K030"            TO  SPA-ERRCD
      *            ÅÀ¿ô·×»»¼°£±
                       COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                               -  TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   TNS-TEN
                                               + ( WRK-KEI-TEN2
                                               *   TNS-KZMTEN )
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            ÅÀ¿ô·×»»¼°£²
                       COMPUTE WRK-KEI-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-SATU-TEN
                                               + ( WRK-KEI-TEN2
                                               *   WRK-KZMTEN )
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF     (SPA-ERRCD           =   SPACE) OR
                  (SPA-ERRCD(1:1)      =   "K"  )
               IF      WRK-KEI-TEN2        >   ZERO
                   MOVE    WRK-KEI-TEN2    TO  WRK-COM-KISOTEN  (IDX-Z)
               ELSE
                   COMPUTE WRK-TENSUKEI-9  =   WRK-SATU-TEN  + 0.5
                   MOVE    WRK-TENSUKEI-9  TO  WRK-COM-KISOTEN(IDX-Z)
               END-IF
           END-IF
      *
      *
           .
       1000132-TENSU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *     ±ÇÁü¿ÇÃÇÍÑ¡¡¿ÇÃÇÎÁÊÔ½¸½èÍý
      *****************************************************************
       100011-STATUEI-RYO-SEC          SECTION.
      *
      ***  IF      WRK-CNT-SATUEI      =   1
      *        ´ÖÀÜ»£±Æ¤Ë¤è¤ëÄþ¸º·×»»
      *        IF      IDX-GEN         NOT =   ZERO
      *            COMPUTE WRK-GENTEN      =   WRK-COM-KISOTEN(IDX-Z)
      *                                    *  (WRK-COM-KISOTEN(IDX-GEN)
      *                                    /   100        )
      *                                    +   0.5
      *            COMPUTE WRK-COM-KISOTEN (IDX-Z)
      *                                    =   WRK-COM-KISOTEN(IDX-Z)
      *                                    -   WRK-GENTEN
      *        END-IF
      *****END-IF
      *
           IF      FLG-SATU            =   ZERO
      *        »£±ÆÎÁ²Ã»»
               ADD     WRK-COM-KISOTEN (IDX-Z)    TO  WRK-TENSUKEI1
           ELSE
      *        ¿ÇÃÇÎÁ²Ã»»
      *        £²²óÌÜ¤Î»þ¡¢100Ê¬50¡¡¤È¤¹¤ë
               IF     (WRK-CNT-SATUEI      >   1       )  AND
                      (WRK-COM-TNSSRYSYUKBN (IDX-Z) =   "711" )
                   COMPUTE WRK-TENSU1          =   WRK-COM-KISOTEN
                                                              (IDX-Z)
                                               *   0.5
                                               +   0.5
                   MOVE    WRK-TENSU1          TO  WRK-TENSU
                   ADD     WRK-TENSU           TO  WRK-TENSUKEI2
               ELSE
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSUKEI2
               END-IF
           END-IF
      *
           .
       100011-STATUEI-RYO-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¿ÇÃÇÎÁ¼«Æ°²Ã»»ºîÀ®½èÍý
      *****************************************************************
       100012-SINDAN-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    ¿ÇÃÇ¡¦»£±ÆÆ±»þ¤Î¥Á¥§¥Ã¥¯
           SET     TBL-IDX             TO  1
           SEARCH      TBL-DOUJI-OCC   VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-SAT) =   TBL-DOUJI-1
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-2 (TBL-IDX)
                                                TO  WRK-SRYCD
                   WHEN    WRK-COM-INPUTCD (IDX-SAT) =   TBL-DOUJI-2
                                                           (TBL-IDX)
                       MOVE    TBL-DOUJI-1 (TBL-IDX)
                                                TO  WRK-SRYCD
           END-SEARCH
           IF     (WRK-SRYCD           =   SPACE      )  OR
                  (WRK-SRYCD           =   "170002010"  OR 
                                           "170001910" )
               MOVE    ZERO                TO  FLG-SYORI
           ELSE
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    1                   TO  FLG-SYORI
           END-IF
      *
           IF      WRK-COM-INPUTCD (IDX-SAT)   =   "170002010"
      *        ÆÃ¼ì»£±Æ¤Î¼Ì¿¿¿ÇÃÇ¡Ê£²²óÌÜ°Ê¹ß¡Ë
               MOVE    "170000610"         TO  WRK-SRYCD
           ELSE
      *    Ã±½ã»£±Æ¤Î¿ÇÃÇÎÁ
               IF      WRK-COM-INPUTCD (IDX-SAT)   =   "170002110"
      *            Â¤±ÆºÞ»ÈÍÑ¤Î¿ÇÃÇÎÁ
                   MOVE    "170000810"         TO  WRK-SRYCD
               ELSE
                   EVALUATE    FLG-BUIKBN
                       WHEN    1
                       WHEN    3
      *                    ¤½¤ÎÂ¾
                           MOVE    "170000510"         TO  WRK-SRYCD
                       WHEN    2
      *                    Æ¬¡¦¶»¡¦Ê¢¡¦ÀÔ¿ñ
                           MOVE    "170000410"         TO  WRK-SRYCD
                   END-EVALUATE
               END-IF
           END-IF
           IF      FLG-SYORI           =   1
               MOVE    SPACE               TO  WRK-SRYCD
           END-IF
      *    ¿ÇÃÇÎÁ¼«Æ°ÄÉ²Ã
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    1                   TO  FLG-SATU
               MOVE    IDX-SAT2            TO  IDX-S
               PERFORM 1003-JIDOU-CHK-SEC
               PERFORM 1002-JIDOUSET-SEC
      *
      ******   £±²óÌÜ¤Ç´ÖÀÜ»£±Æ¥³¡¼¥É¤ÎÆþÎÏ¤¬¤¢¤ë»þ¡¢¿ÇÃÇÎÁ¤Î¸å¤Ë¤âÄÉ²Ã
      *        IF      (FLG-ADD            =   1    )  AND
      *                (IDX-GEN            >   ZERO )  AND
      *                (WRK-CNT-SATUEI     =   1    )
      *            MOVE    ZERO            TO  FLG-ARI
      **           MOVE    WRK-COM-INPUTCD (IDX-GEN)
      *            MOVE    WRK-GEN-SRYCD   TO  WRK-SRYCD
      *            PERFORM 1002-JIDOUSET-SEC
      ******** END-IF
           END-IF
      *
      *    ¾Ã²½´É¤ÇÂ¤±ÆºÞ»£±Æ¤Î»þ¡¢Æ©»ë¿ÇÃÇÎÁ¤ÎÄÉ²Ã¡Ê»£±ÆÎÁ¤Î¾å¤ËÄÉ²Ã¡Ë
           IF     (FLG-BUIKBN          =   3  )   AND
                  (WRK-COM-INPUTCD (IDX-SAT)   =   "170002110" )
               MOVE    "170000310"         TO  WRK-SRYCD
               COMPUTE IDX-S               =   IDX-SAT    -    1
               MOVE    ZERO                TO  FLG-SATU
               PERFORM 1003-JIDOU-CHK-SEC
               PERFORM 1002-JIDOUSET-SEC
           END-IF
      *
           .
       100012-SINDAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»ÎÁÄÉ²Ã½èÍý
      *****************************************************************
       1002-JIDOUSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÊÔ½¸¡¦´ðËÜ¥Á¥§¥Ã¥¯
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    ¥¨¥é¡¼¤Î»þ¡¢ÄÉ²Ã¤Ê¤·
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1002-JIDOUSET-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    ¼«Æ°ÄÉ²ÃÊ¬¡ÊÆþÎÏ»þ¤ËÄÉ²Ã¡Ë
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
      *****ADD     1                   TO  WRK-GMN-MAX
      *
           MOVE    1                   TO  FLG-ADD
           .
       1002-JIDOUSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°²Ã»»¡¡Â¸ºß¥Á¥§¥Ã¥¯²Ã½èÍý
      *****************************************************************
       1003-JIDOU-CHK-SEC        SECTION.
      *
      *    Â¸ºß¥Á¥§¥Ã¥¯
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
               IF     (WRK-SRYCD               =    "170000510"   OR
                                                    "170000410" )  AND
                      (WRK-COM-INPUTCD (IDX1)  =    "170000510"   OR
                                                    "170000410" )
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       1003-JIDOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¯Îð²Ã»»ÄÉ²Ã½èÍý¡Ê¶¦ÄÌ¡Ë
      *****************************************************************
       1007-NYUYOJI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-SYU IDX2)
                                          =   SPACE  OR   ZERO )
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-SYU IDX2)
                                               TO  WRK-SRYCD
               MOVE    IDX-SYU            TO   IDX-T
               MOVE    IDX-SYU            TO   IDX-S
               PERFORM 1003-JIDOU-CHK-SEC
      *        ²Ã»»ÄÉ²Ã
               MOVE    3                   TO  FLG-SATU
               PERFORM 1002-JIDOUSET-SEC
               IF      FLG-ADD             =   1
                   MOVE    2                   TO  WRK-COM-SANFLG 
                                                           (IDX-S)
      *            »£±ÆÎÁ¤ÎÆýÍÄ»ù²Ã»»¤Î»þ¡¢°ÌÃÖÊÝÂ¸
                   IF      IDX-SYU             =   IDX-SAT
                       MOVE    IDX-S           TO  IDX-NYU
                   END-IF
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»·×»»½èÍý
      *****************************************************************
       10071-KASAN-KEI-SEC             SECTION.
      *
      *    Äþ¸º¤Ç´ð½àÉÔÅ¬¹çÄþ¸º¶èÊ¬°Ê³°¤Ï¤¹¤Ç¤Ë·×»»
           IF     (WRK-COM-TENSIKIBETU(IDX-T)    =   6 )  AND
                  (WRK-COM-KIJUNTEIGENKBN(IDX-T) =   ZERO)
               GO      TO      10071-KASAN-KEI-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-9
           MOVE    ZERO                TO  WRK-TENSUKEI-9G
           EVALUATE    WRK-COM-TENSIKIBETU(IDX-T)
      *        »£±ÆÎÁ¤Ë²Ã»»¤¹¤ë¤â¤Î
               WHEN    5
                 IF      WRK-COM-SANFLG (IDX-T)  =   ZERO  OR  2
                   MOVE    ZERO                TO  WRK-TENSUKEIS
                   COMPUTE WRK-TENSUKEIS   =   WRK-COM-KISOTEN(IDX-SAT)
                                           *   WRK-COM-KISOTEN(IDX-T)
                                           /   100
                                           +   0.5
                   MOVE    WRK-TENSUKEIS       TO  WRK-TENSUKEI-9
                 END-IF
      *        ´ð½àÉÔÅ¬¹çÄþ¸º¶èÊ¬
               WHEN    6
                   MOVE    ZERO                TO  WRK-TENSUKEIS
                   COMPUTE WRK-TENSUKEIS   =   WRK-COM-KISOTEN(IDX-SAT)
                                           *   WRK-COM-KISOTEN(IDX-T)
                                           /   100
      *************************************+   0.5
                   MOVE    WRK-TENSUKEIS       TO  WRK-TENSUKEI-9G
      *        ¤½¤ÎÂ¾¤Î²Ã»»¤¹¤ë¤â¤Î
               WHEN    OTHER
                   MOVE    WRK-COM-KISOTEN (IDX-T) TO  WRK-TENSUKEI-9
           END-EVALUATE
      *
      *    ·×»»ºÑ¥Õ¥é¥°
           MOVE    1                   TO  WRK-COM-SANFLG (IDX-T)
      *
           COMPUTE WRK-TENSUKEI1       =   WRK-TENSUKEI1
                                       +   WRK-TENSUKEI-9
                                       -   WRK-TENSUKEI-9G
      *
           .
       10071-KASAN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»ÊÔ½¸½èÍý
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-END
      *
           EVALUATE    FLG-SINDAN
               WHEN    1
      *            £ØÀþ»£±ÆÎÁ
                   PERFORM 10001-XSEN-SINDAN-SEC
               WHEN    2
      *            ³Ë°å³Ø¿ÇÃÇÎÁ
                   PERFORM 10002-KZKU-SINDAN-SEC
               WHEN    3
      *            £Ã£Ô¡¦£Í£Ò£É¿ÇÃÇÎÁ
                   PERFORM 10003-CTMRI-SINDAN-SEC
               WHEN    OTHER
      *            ¤½¤ÎÂ¾
                   PERFORM 10004-ETC-SINDAN-SEC
           END-EVALUATE
      *
      *    ÌôºÞÅÀ¿ô·×»»
           PERFORM 1004-SATU-ZAITENU-SEC
      *    ÆÃÄê´ïºàÅÀ¿ô·×»»
           PERFORM 1005-KIZAI-TENSU-SEC
      *
           COMPUTE LNK-SC90-TENSUKEI   =   WRK-TENSUKEI1
                                       +   WRK-TENSUKEI2
                                       +   WRK-TENSUKEI3
      *    ³ÆÅÀ¿ôÊÔ½¸
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1
                                                         (IDX-MAX)
      *    ÌôºÞÅÀ¿ô
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    ´ïºàÅÀ¿ô
           COMPUTE WRK-COM-KIZAITEN(IDX-MAX)
                                       =   LNK-SC90-KIZTEN
                                       +   WRK-FIRTEN-KEI
      *
      *    ÅÀ¿ô·×¡¡·×»»
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
                                           +   WRK-FIRTEN-KEI
      *    ·×
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³Ë°å³Ø¿ÇÃÇÎÁºîÀ®½èÍý
      *****************************************************************
       10002-KZKU-SINDAN-SEC           SECTION.
      *
      *    »£±ÆÎÁ¤ÎºÆ·×»»
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KISOTEN-G
      *    Ëç¿ô·×»»
           MOVE    ZERO                TO  WRK-KEIMAI
      *
           INITIALIZE                  ORCSC90AREA
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *        ÌÀºÙ·×»»
               PERFORM 100013-XSEN-KEI-SEC
      *
               IF      WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1"
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "1"
                       EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)(1:2)
                           WHEN    "72"
      *                    »£±ÆÎÁÊÔ½¸
                               MOVE    ZERO                TO  FLG-SATU
                               PERFORM 100011-STATUEI-RYO-SEC
                           WHEN    "71"
      *                    ¿ÇÃÇÎÁ
                               MOVE    1                   TO  FLG-SATU
                               PERFORM 100011-STATUEI-RYO-SEC
      *                    ¤½¤ÎÂ¾
                           WHEN    OTHER
                               ADD     WRK-COM-KISOTEN(IDX-Z) 
                                                     TO  WRK-TENSUKEI3
                       END-EVALUATE
                   ELSE
                       IF      WRK-COM-SANFLG (IDX-Z) =   ZERO
      *                    ²Ã»»¹àÌÜ
                           MOVE    IDX-Z           TO  IDX-T
                           PERFORM 10071-KASAN-KEI-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»
           PERFORM 1006-FIRTEN-SET-SEC
      *
           .
       10002-KZKU-SINDAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¦£Í£Ò£É¿ÇÃÇÎÁºîÀ®½èÍý
      *****************************************************************
       10003-CTMRI-SINDAN-SEC           SECTION.
      *
      *    »£±ÆÎÁ¤ÎºÆ·×»»
           MOVE    ZERO                TO  WRK-TENSUKEI
      *    Ëç¿ô·×»»
           MOVE    ZERO                TO  WRK-KEIMAI
      *
      *    Â¤±ÆºÞ»ÈÍÑ²Ã»»ÎÁ
           IF      FLG-ZOEIZAI         =   1
               IF      FLG-CTMRI           =   1
      *            £Ã£Ô²Ã»»
                   MOVE    "170012070"         TO  WRK-SRYCD
               ELSE
      *            £Í£É£Ò²Ã»»
                   MOVE    "170020470"         TO  WRK-SRYCD
               END-IF
               MOVE    IDX-SAT             TO  IDX-S
               MOVE    ZERO                TO  FLG-SATU
               PERFORM 1003-JIDOU-CHK-SEC
               PERFORM 1002-JIDOUSET-SEC
           END-IF
      *
      *
           INITIALIZE                  ORCSC90AREA
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-KAS
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *        ÌÀºÙ·×»»
               PERFORM 100013-XSEN-KEI-SEC
      *
               IF      WRK-COM-INPUTCD (IDX-Z)(1:1)   =   "1"
                   IF      WRK-COM-DATAKBN (IDX-Z)    =   "1"
                       EVALUATE    WRK-COM-TNSSRYSYUKBN(IDX-Z)(1:2)
                           WHEN    "72"
      *                    »£±ÆÎÁÊÔ½¸
                               MOVE    ZERO                TO  FLG-SATU
                               PERFORM 100011-STATUEI-RYO-SEC
                           WHEN    "71"
      *                    ¿ÇÃÇÎÁ
                               MOVE    1                   TO  FLG-SATU
                               PERFORM 100011-STATUEI-RYO-SEC
      *                    ¤½¤ÎÂ¾
                           WHEN    OTHER
                               ADD     WRK-COM-KISOTEN(IDX-Z) 
                                                     TO  WRK-TENSUKEI3
                       END-EVALUATE
                   END-IF
      *            ²Ã»»¤ÇÄê³Û²Ã»»¤Î»þ¤Î½¸·×
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "2"
                       IF      WRK-COM-TENSIKIBETU(IDX-Z)  =   5
                           MOVE    IDX-Z               TO  IDX-KAS
                       ELSE
                           IF     (WRK-COM-SANFLG (IDX-Z)  =   ZERO  )
                             AND  (WRK-COM-TENSIKIBETU(IDX-Z)
                                                       NOT =   6)
                               ADD     WRK-COM-KISOTEN (IDX-Z)
                                                     TO  WRK-TENSUKEI1
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    »£±ÆÎÁ¤Ë¡ó²Ã»»¤¹¤ë
           IF     (IDX-KAS                 >   ZERO  )  AND
                  (WRK-COM-TENSIKIBETU(IDX-KAS)    =   5 )
               COMPUTE WRK-TENSUKEI        =   WRK-TENSUKEI1
                                           *   WRK-COM-KISOTEN(IDX-KAS)
                                           /   100
                                           +   0.5
               MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
      *        ·×»»ºÑ¥Õ¥é¥°
               MOVE    1                   TO  WRK-COM-SANFLG (IDX-KAS)
               COMPUTE WRK-TENSUKEI1       =   WRK-TENSUKEI1
                                           +   WRK-TENSUKEI-9
           END-IF
      *
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»
           PERFORM 1006-FIRTEN-SET-SEC
      *
           .
       10003-CTMRI-SINDAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¤½¤ÎÂ¾±ÆÎÁ½ªÎ»½èÍý
      *****************************************************************
       1004-ETC-END-SEC           SECTION.
      *
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»
           PERFORM 1006-FIRTEN-SET-SEC
      *
           MOVE    ZERO                TO  LNK-SC90-BUNGSU
           MOVE    ZERO                TO  LNK-SC90-MAISU
      *    ¥Õ¥£¥ë¥àÆýÍÄ»ù²Ã»»ÎÎ°è¥¯¥ê¥¢
           INITIALIZE                  WRK-NYUYOJI-AREA
           .
       1004-ETC-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¤½¤ÎÂ¾¿ÇÃÇÎÁºîÀ®½èÍý
      *****************************************************************
       10004-ETC-SINDAN-SEC           SECTION.
      *    Ëç¿ô·×»»
           MOVE    ZERO                TO  WRK-KEIMAI
      *
           INITIALIZE                  ORCSC90AREA
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX-Z   FROM    1       BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *        ÌÀºÙ·×»»
               PERFORM 100013-XSEN-KEI-SEC
      *
               IF      WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1"
                   IF      WRK-COM-DATAKBN (IDX-Z)     =   "1"
                       ADD     WRK-COM-KISOTEN(IDX-Z)
                                               TO  WRK-TENSUKEI3
                   ELSE
                       IF      WRK-COM-SANFLG (IDX-Z)  =   ZERO
      *                    ²Ã»»¹àÌÜ
                           MOVE    IDX-Z           TO  IDX-T
                           PERFORM 10071-KASAN-KEI-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»
           PERFORM 1006-FIRTEN-SET-SEC
      *
           .
       10004-ETC-SINDAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌôºÞÅÀ¿ô·×»»½èÍý¡Ê±ÇÁü¿ÇÃÇ¡Ë
      *****************************************************************
       1004-SATU-ZAITENU-SEC         SECTION.
      *
           COMPUTE WRK-SYOKEIS2        =   LNK-SC90-ZAIKEI
                                       +   LNK-SC90-GAIKEI
      *
           MOVE    ZERO                TO  LNK-SC90-ZAIKEI
           MOVE    ZERO                TO  LNK-SC90-GAIKEI
           MOVE    ZERO                TO  WRK-SYOKEIS3
      *
      * =============================================================
      *---// £±ÆüÊ¬¤ÎÅÀ¿ô¡¡15ÅÀ°Ê²¼ ¢Í ¡¡ZERO                       //
      *-- // £±ÆüÊ¬¤ÎÅÀ¿ô¡¡15ÅÀÄ¶²á ¢Í ¡Ê£±ÆüÊ¬¤ÎÅÀ¿ô¡Ý5¡Ë/ 10 + 1 //
      *==============================================================
      *
           IF      WRK-SYOKEIS2        <=  15
               MOVE    ZERO            TO  WRK-SYOKEIS3
           ELSE
               COMPUTE WRK-SYOKEIS3    =  (WRK-SYOKEIS2    - 5 )
                                       /   10
           END-IF
      *
           COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS3   + 0.999
      *
           .
       1004-SATU-ZAITENU-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃÄê´ïºàÅÀ¿ô·×»»¡Ê±ÇÁü¿ÇÃÇ¡Ë
      *****************************************************************
       1005-KIZAI-TENSU-SEC         SECTION.
      *
      *    ¶â³Û·×¡à£±£°¡¡»Í¼Î¸ÞÆþ
           COMPUTE WRK-SYOKEIS2        =   LNK-SC90-KIZKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-KIZTEN
      *
      *
           .
       1005-KIZAI-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥£¥ë¥àÅÀ¿ô·×»»½èÍý
      *****************************************************************
       1006-FIRTEN-SET-SEC             SECTION.
      *
      *    ÆýÍÄ»ù²Ã»»¤Î»»ÄêÍ­Ìµ
           IF     (WRK-NYU-BUI         =   "1" )  AND
                  (WRK-NYU-CD          =   "1" )  AND
                  (WRK-NYU-FIR         =   "1" )  AND
                  (LNK-ORCSC-NENREI-YY <   6   )
               CONTINUE
           ELSE
      *        ÆýÍÄ»ù²Ã»»¤Ê¤·
      *            ¥Õ¥£¥ë¥à¹ç·×ÅÀ¿ô
               COMPUTE WRK-TENSUKEIS       =  (WRK-NYU-TEN
                                           +   WRK-FIRTEN  )
                                           /   10
                                           +   0.5
               MOVE    WRK-TENSUKEIS       TO  WRK-TENSUKEI-G
               ADD     WRK-TENSUKEI-G      TO  WRK-FIRTEN-KEI
      *
               IF      FLG-END             =   ZERO
                   MOVE    ZERO                TO  WRK-NYU-TEN
                   MOVE    ZERO                TO  WRK-FIRTEN
               END-IF
      *
               GO      TO      1006-FIRTEN-SET-EXT
           END-IF
      *
      *    ÆýÍÄ»ù²Ã»»¤¢¤ê
           MOVE    "799990070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  IDX-S
      *
      *    ¼«Æ°²Ã»»¡¡Â¸ºß¥Á¥§¥Ã¥¯¡õÁÞÆþ°ÌÃÖ¥»¥Ã¥È
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-IDX
           PERFORM VARYING     IDX1    FROM    IDX-HOZ  BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )   OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
      *            ¥Õ¥£¥ë¥àÈ½Äê
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "7"  OR
                       WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059")  AND
                      (WRK-COM-DATAKBN (IDX1)      =   "3" )
                   MOVE    IDX1                TO  IDX-S
               END-IF
               IF      WRK-COM-INPUTCD (IDX1)  =   WRK-SRYCD
                   MOVE    IDX1                TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
      *
           MOVE    WRK-NYU-IDX         TO  IDX-S
           PERFORM 1002-JIDOUSET-SEC
           IF      FLG-ADD             =   1
               ADD     1                   TO  IDX-MAX
           END-IF
      *
      *    ²Ã»»ÎÁÊÔ½¸
           IF      WRK-COM-TENSIKIBETU(IDX-S)  =   5
               COMPUTE WRK-TENSUKEIS       =   WRK-NYU-TEN
                                           *   WRK-COM-KISOTEN (IDX-S)
                                           /   100
                                           +   0.5
               MOVE    WRK-TENSUKEIS       TO  WRK-TENSUKEI-9
           ELSE
               MOVE    ZERO                TO  WRK-TENSUKEI-9
           END-IF
           COMPUTE WRK-TENSUKEI-9      =   WRK-NYU-TEN
                                       +   WRK-TENSUKEI-9
      *
      *    ¥Õ¥£¥ë¥à¹ç·×ÅÀ¿ô
           COMPUTE WRK-TENSUKEIS       =  (WRK-TENSUKEI-9
                                       +   WRK-FIRTEN  )
                                       /   10
                                       +   0.5
           MOVE    WRK-TENSUKEIS       TO  WRK-TENSUKEI-G
           ADD     WRK-TENSUKEI-G      TO  WRK-FIRTEN-KEI
      *
           IF      FLG-END             =   ZERO
               MOVE    ZERO                TO  WRK-NYU-TEN
               MOVE    ZERO                TO  WRK-FIRTEN
           END-IF
      *
           .
       1006-FIRTEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-JIKASAN
           MOVE    ZERO                TO  IDX-GAI
           MOVE    ZERO                TO  IDX-CT
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "170016010"
                   MOVE    IDX-Z           TO  IDX-GAI
               END-IF
      *
      *        »þ´Ö³°²Ã»»¥Á¥§¥Ã¥¯¡Ê¿Ç»¡¡¢²Ã»»¡¢»þ´Ö²Ã»»¤Î¤â¤Î¡Ë
               IF     (WRK-COM-SRYKBN  (IDX-Z)     =   "11"  OR
                                                       "12"     ) AND
                      (WRK-COM-DATAKBN (IDX-Z)     =   "2"      ) AND
                      (WRK-COM-TIMEKSNKBN(IDX-Z)   NOT =   ZERO  AND
                                                           5    )
                   MOVE    1               TO  FLG-JIKASAN
               END-IF
      *
      *        £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯
               IF      WRK-COM-KNSJISGRPKBN (IDX-Z)    =  32
                   PERFORM 2001-CTMRI-CHK-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
      *    »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»Í­¤ê
           IF      IDX-GAI             >   ZERO
               MOVE    IDX-GAI             TO  IDX-Z
               IF     (FLG-JIKASAN         =   ZERO         )  AND
                      (WRK-COM-KZMFLG (IDX-Z)  =   SPACE    )  AND
                      (LNK-ORCSC-TOROKU    NOT =   "2"   )
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    "2"                 TO  WRK-COM-CUR (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
      *        »þ´ÖÆþÎÏÄÉ²Ã
               MOVE    "840000071"         TO  WRK-SRYCD
               IF      WRK-COM-INPUTCD(IDX-Z + 1)  =   WRK-SRYCD
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-SATU
                   MOVE    IDX-Z               TO  IDX-S
                   PERFORM 1003-JIDOU-CHK-SEC
                   PERFORM 1002-JIDOUSET-SEC
               END-IF
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *        
      *****************************************************************
      *    £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CTMRI-CHK-SEC                SECTION.
      *
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯
           INITIALIZE                      ORCSC91AREA
           MOVE    "3"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-COM-CHK-SRYCD (IDX-Z)
                                       TO  LNK-SC91-SRYCD
           MOVE    IDX-Z               TO  LNK-SC91-GMN-IDX
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "0001"
               MOVE    LNK-SC91-ERRCD      TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               GO      TO      2001-CTMRI-CHK-EXT
           END-IF
      *
      *    Åö·î»»Äê¤Ê¤·
           IF      LNK-SC91-CNT        =  ZERO
      *        £Ã£Ô¡¥£Í£Ò£É¤Î»»Äê¥Á¥§¥Ã¥¯
               IF      WRK-COM-INPUTCD (IDX-Z)     =   "170022290"  OR
                                                       "170022390"  OR
                                                       "170022490"  OR
                                                       "170701470"  OR
                                                       "170701570"  OR
                                                       "170701670"
                   IF     (WRK-COM-KZMFLG (IDX-Z)      =   SPACE ) AND 
                          (LNK-ORCSC-TOROKU        NOT =   "2"   )
                       MOVE    "K003"              TO  SPA-ERRCD
                       MOVE    "2"                 TO  WRK-COM-CUR
                                                               (IDX-Z)
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                               (IDX-Z)
                       MOVE    IDX-Z               TO  IDX-CT
                   END-IF
               END-IF
           ELSE
               IF      WRK-COM-INPUTCD (IDX-Z)     =   "170022290"  OR
                                                       "170022390"  OR
                                                       "170022490"  OR
                                                       "170701470"  OR
                                                       "170701570"  OR
                                                       "170701670"
                   CONTINUE
               ELSE
      *************MOVE    "0034"              TO  SPA-ERRCD
                   MOVE    "0078"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
                   MOVE    IDX-Z               TO  IDX-CT
                   MOVE    ZERO                TO  FLG-CHK
                   IF      LNK-SC91-CNT        =   1
      *                ²èÌÌÆâ¤Ë¤Î¤ßÂ¸ºß¤¹¤ë»þ¡¢¸å¤ò¥¨¥é¡¼¤È¤¹¤ë 
                       PERFORM VARYING     IDX-Z2  FROM   IDX-Z   BY  1
                               UNTIL       IDX-Z2  >   SPA-MAX-LINE
                           IF     (IDX-Z2      NOT =   IDX-Z  )  AND
                                  (WRK-COM-CHK-SRYCD (IDX-Z2)
                                                   =  WRK-COM-CHK-SRYCD
                                                               (IDX-Z))
                               MOVE    1              TO  FLG-CHK
                               MOVE    SPA-MAX-LINE   TO  IDX-Z2
                           END-IF
                       END-PERFORM
                       IF      FLG-CHK             =   1
                           MOVE    SPACE           TO  SPA-ERRCD
                           MOVE    ZERO            TO  IDX-CT
                       END-IF
                   END-IF
                   IF      IDX-CT          NOT =   ZERO
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-Z)
                   END-IF
               END-IF
           END-IF
           .
      *
       2001-CTMRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«Æ°»»Äê½èÍý
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
      *2001.3.21  ¼«Æ°»»Äê¤òÊÝÎ±»ö¹à¤È¤¹¤ë
           GO      TO      300-AUTO-SYORI-EXT.
      *
      *
           MOVE    ZERO                TO  FLG-CTMRI
           MOVE    ZERO                TO  FLG-XSEN
           MOVE    ZERO                TO  FLG-KAKUI
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
      *        £ØÀþÍ­¤ê
               IF      WRK-COM-TNSSRYSYUKBN(IDX-Z) =   "721"
                   MOVE    1                       TO  FLG-XSEN
               END-IF
      *
      *        ¸¡ººÅù¼Â»ÜÈ½Äê¥°¥ë¡¼¥×
               EVALUATE    WRK-COM-KNSJISGRPKBN (IDX-Z)
      *            ³Ë°å³Ø
                   WHEN    31
                       MOVE    1                   TO  FLG-KAKUI
      *            ¥³¥ó¥Ô¥å¡¼¥¿
                   WHEN    32
                       IF      WRK-COM-TNSSRYSYUKBN(IDX-Z)   =   "710"
      *                    Â¾°å»£±Æ
                           MOVE    2                   TO  FLG-CTMRI
                       ELSE
                           MOVE    1                   TO  FLG-CTMRI
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *
           MOVE    ZERO                TO  IDXS
      *
      *    ²èÁü¿ÇÃÇ´ÉÍý²Ã»»ÄÉ²Ã¡Ê¼Ì¿¿¿ÇÃÇ¡Ë
           IF      FLG-XSEN            =   1
               MOVE    "170020270"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
           END-IF
      *
      *    ³Ë°å³Ø¿ÇÃÇÎÁ²Ã»»Í­¤ê
           IF      FLG-KAKUI           =   1
      *        ³Ë°å³Ø¿ÇÃÇÎÁ²Ã»»
               MOVE    "170015310"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
      *        ²èÁü¿ÇÃÇ´ÉÍý²Ã»»¡Ê³Ë°å³Ø¡Ë
               MOVE    "170021370"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
           END-IF
      *
      *    ¥³¥ó¥Ô¥å¡¼¥¿¿ÇÃÇÎÁ²Ã»»Í­¤ê
           IF      FLG-CTMRI           =   1
      *        ¥³¥ó¥Ô¥å¡¼¥¿¿ÇÃÇÎÁ²Ã»»Í­¤ê
               IF      LNK-ORCSC-ROUJIN    =   ZERO
                   MOVE    "170015410"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD-2
               ELSE
                   MOVE    "170700710"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD-2
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
      *        ²èÁü¿ÇÃÇ´ÉÍý²Ã»»¡Ê¥³¥ó¥Ô¥å¡¼¥¿¿ÇÃÇ¡Ë
               IF      LNK-ORCSC-ROUJIN    =   ZERO
                   MOVE    "170021470"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD-2
               ELSE
                   MOVE    "170701270"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD-2
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
           END-IF
      *
      *    Â¾°å¥³¥ó¥Ô¥å¡¼¥¿¿ÇÃÇÎÁ²Ã»»Í­¤ê
           IF      FLG-CTMRI           =   2
               MOVE    "170019950"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
      *        ²èÁü¿ÇÃÇ´ÉÍý²Ã»»¡ÊÂ¾°å¥³¥ó¥Ô¥å¡¼¥¿¿ÇÃÇ¡Ë
               MOVE    "170021470"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD-2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "700"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (08 IDXS)
                   MOVE    "70"            TO  LNK-ORCSC-SRYKBN
                                                         (08 IDXS)
               END-IF
           END-IF
           .
      *
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²Ã»»ÎÁ»»Äê½èÍý
      *****************************************************************
       3002-KASAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    º£²óÆþÎÏ¤Ë¤Ê¤¤¤³¤È
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF      WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
      *        »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR             =   ZERO
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (08 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (08 IDXS)
      *
      *        ²Ã»»ÎÁ»»Äê
               IF      WRK-SRYCD-2     NOT =   SPACE
                   MOVE    WRK-SRYCD-2         TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU       =   ZERO
      *                »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 3001-SANTEI-CHK-SEC
                           IF      FLG-ERR             =   ZERO
                               MOVE    WRK-SRYCD-2
                                               TO  LNK-ORCSC-SRYCD-KS
                                                              (08 IDXS)
                               MOVE    TNS-TEN TO  LNK-ORCSC-TENSU-KS
                                                             (08 IDXS)
                           END-IF
                       END-IF 
                    END-IF
                    MOVE    ZERO           TO  FLG-ERR
                END-IF
           END-IF
      *
           .
       3002-KASAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÍúÎò¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       3001-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                      ORCSC91AREA
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"       USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-ERRCD      =   SPACE
      *        ÄÉ²Ã¤¢¤ê
               MOVE    ZERO            TO  FLG-ERR
           ELSE
      *        ÄÉ²Ã¤Ê¤·
               MOVE    1               TO  FLG-ERR
           END-IF
      *
           .
       3001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
