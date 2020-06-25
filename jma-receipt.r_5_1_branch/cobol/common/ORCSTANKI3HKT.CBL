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
       PROGRAM-ID.                 ORCSTANKI3HKT.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¶¦ÄÌ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Ã»´üÂÚºß£³Êñ³ç
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  15/08/28    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.08.00    NACL-Â¿¡¹Ç¼  17/04/XX  ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»Êñ³çÂÐ±þ
      *  04.08.00    NACL-Â¿¡¹Ç¼  17/06/XX  À¸²½¡Ê£²¡ËÈ½ÃÇÎÁÊñ³çÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/10/10  Ã»´üÂÚºß£³¤Î»»ÄêÆüÈ½Äê½¤Àµ
      *              NACL-¾®Æ¦Âô  18/10/11  Ã»´üÂÚºß£³¤ÎÊñ³çÈ½ÄêÄÉ²Ã
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TANKI3                  PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *
           03   CONS-TANKI3HKT-AREA.
      *      Ç¢¡¦ÊµÊØÅù¸¡ººÈ½ÃÇÎÁ
             05   CONS-HKT1                PIC X(09) VALUE "160061710".
      *      ·ì±Õ³ØÅª¸¡ººÈ½ÃÇÎÁ
             05   CONS-HKT2                PIC X(09) VALUE "160061810".
      *      À¸²½³ØÅª¸¡ºº¡Ê£±¡ËÈ½ÃÇÎÁ
             05   CONS-HKT3                PIC X(09) VALUE "160061910".
      *      À¸²½³ØÅª¸¡ºº¡Ê£²¡ËÈ½ÃÇÎÁ
             05   CONS-HKT4                PIC X(09) VALUE "160062010".
      *      ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
             05   CONS-HKT5                PIC X(09) VALUE "160062110".
      *      ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
             05   CONS-HKT6                PIC X(09) VALUE "160062210".
      *      ¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ
             05   CONS-HKT7                PIC X(09) VALUE "170015410".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±¡Ê¼Ì¿¿¿ÇÃÇ¡Ë
             05   CONS-HKT8                PIC X(09) VALUE "170025210".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±¡Ê´ðËÜÅª¥¨¥Ã¥¯¥¹Àþ¿ÇÃÇ¡Ë
             05   CONS-HKT9                PIC X(09) VALUE "170025310".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±¡Ê³Ë°å³Ø¿ÇÃÇ¡Ë
             05   CONS-HKT10               PIC X(09) VALUE "170025410".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±¡Ê¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ¡Ë
             05   CONS-HKT11               PIC X(09) VALUE "170025510".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£²¡Ê³Ë°å³Ø¿ÇÃÇ¡Ë
             05   CONS-HKT12               PIC X(09) VALUE "170025610".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£²¡Ê¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ¡Ë
             05   CONS-HKT13               PIC X(09) VALUE "170025710".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£³¡Ê³Ë°å³Ø¿ÇÃÇ¡Ë
             05   CONS-HKT14               PIC X(09) VALUE "170035710".
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£³¡Ê¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ¡Ë
             05   CONS-HKT15               PIC X(09) VALUE "170035810".
      *
           03  CONS-TANKI3HKT-AREA-R  REDEFINES  CONS-TANKI3HKT-AREA.
             05   CONS-TANKI3HKT-TBL      OCCURS  15.
                07   CONS-TANKI3HKT-SRYCD  PIC X(09).
      *
      *    ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»
           03   CONS-TANKI3KSN-AREA.
      *      ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»£±
             05   CONS-KSN1                PIC X(09) VALUE "160170170".
      *      ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»£²
             05   CONS-KSN2                PIC X(09) VALUE "160182770".
      *      ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»£³
             05   CONS-KSN3                PIC X(09) VALUE "160161610".
      *      ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»£´
             05   CONS-KSN4                PIC X(09) VALUE "160185770".
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
      *    Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³È½ÄêÍÑ
           03  WRK-TANKI3-DAY              PIC X(01)   OCCURS  31.
      *    »»ÄêÆüºÆÄêµÁ
           03  WRK-SANTEI-YMD.
               05  WRK-SANTEI-YY           PIC 9(04).
               05  WRK-SANTEI-MM           PIC 9(02).
               05  WRK-SANTEI-DD           PIC 9(02).
      *    ·îËöÆü¼èÆÀÍÑ
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSTANKI3HKT.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTANKI3HKT-AREA
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-TANKI3HKT-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-TANKI3HKT-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-TANKI3HKT-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-TANKI3HKT-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
      *    INITIALIZE                         PTINF-REC
      *    MOVE    LNK-TANKI3HKT-HOSPNUM  TO  PTINF-HOSPNUM
      *    MOVE    LNK-TANKI3HKT-PTID     TO  PTINF-PTID
      *    PERFORM 900-PTINF-READ-SEC
      *    IF      FLG-PTINF              NOT =   ZERO
      *        MOVE    1                  TO  LNK-TANKI3HKT-RC
      *        GO  TO  100-PRM-CHK-EXT
      *    END-IF
      *
      *    »»ÄêÇ¯·îÆü¥Á¥§¥Ã¥¯
           IF      LNK-TANKI3HKT-SANTEIYMD  =   SPACE
               MOVE    2                  TO  LNK-TANKI3HKT-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           EVALUATE    LNK-TANKI3HKT-SRYCD
      *        ¸¡ººÈ½ÃÇÎÁÊñ³çÂÐ¾Ý¥Á¥§¥Ã¥¯
               WHEN    CONS-TANKI3HKT-SRYCD(1)
               WHEN    CONS-TANKI3HKT-SRYCD(2)
               WHEN    CONS-TANKI3HKT-SRYCD(3)
               WHEN    CONS-TANKI3HKT-SRYCD(4)
               WHEN    CONS-TANKI3HKT-SRYCD(5)
               WHEN    CONS-TANKI3HKT-SRYCD(6)
      *        ¥³¥ó¥Ô¥å¡¼¥¿ÃÇÁØ¿ÇÃÇ¥Á¥§¥Ã¥¯
               WHEN    CONS-TANKI3HKT-SRYCD(7)
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£±
               WHEN    CONS-TANKI3HKT-SRYCD(8)
               WHEN    CONS-TANKI3HKT-SRYCD(9)
               WHEN    CONS-TANKI3HKT-SRYCD(10)
               WHEN    CONS-TANKI3HKT-SRYCD(11)
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£²
               WHEN    CONS-TANKI3HKT-SRYCD(12)
               WHEN    CONS-TANKI3HKT-SRYCD(13)
      *      ²èÁü¿ÇÃÇ´ÉÍý²Ã»»£³
               WHEN    CONS-TANKI3HKT-SRYCD(14)
               WHEN    CONS-TANKI3HKT-SRYCD(15)
                   MOVE    SPACE               TO  LNK-TANKI3HKT-KBN2
                   PERFORM 2001-MAIN-CHK-SEC
      *
      *        ¸¡ººÈ½ÃÇÎÁÊñ³ç¤Ë¤è¤ëÊñ³çÂÐ¾Ý
               WHEN    CONS-KSN1
               WHEN    CONS-KSN2
               WHEN    CONS-KSN3
               WHEN    CONS-KSN4
                   IF      LNK-TANKI3HKT-HKTKBN    =   "1"
                       MOVE    "1"             TO  LNK-TANKI3HKT-KBN
                   ELSE
                       MOVE    SPACE           TO  LNK-TANKI3HKT-KBN
                   END-IF
                   MOVE    SPACE               TO  LNK-TANKI3HKT-KBN2
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¡ººÈ½ÃÇÎÁÊñ³çÂÐ¾Ý¥Á¥§¥Ã¥¯
      *****************************************************************
       2001-MAIN-CHK-SEC            SECTION.
      *
             MOVE    LNK-TANKI3HKT-SANTEIYMD  TO  WRK-SANTEI-YMD
      *
      *      ·îËöÆü¤ò¼èÆÀ
             INITIALIZE                         STS-AREA-DAY
             INITIALIZE                         LNK-DAY7-AREA
             MOVE    "71"                   TO  LNK-DAY7-IRAI
             MOVE    LNK-TANKI3HKT-SANTEIYMD(1:6)
                                            TO  LNK-DAY7-YM
             CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
             IF      STS-DAY-RC1            =   ZERO
                 MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
             END-IF
      *
      *      Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¤ÎÆþ±¡Æü¤Ë¤Ä¤¤¤Æ¼èÆÀ
             INITIALIZE                         ORCSNYUINDAY-AREA
             MOVE    LNK-TANKI3HKT-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
             MOVE    LNK-TANKI3HKT-PTID     TO  LNK-NYUINDAY-PTID
             MOVE    LNK-TANKI3HKT-SANTEIYMD(1:6)
                                            TO  LNK-NYUINDAY-SANTEIYM
             CALL    "ORCSNYUINDAY"     USING   ORCSNYUINDAY-AREA
                                                SPA-AREA
      *
             MOVE     ZERO                  TO  FLG-TANKI3
             PERFORM     VARYING    IDX   FROM   1   BY   1
               UNTIL     IDX    >   WRK-GETSUMATSU-DD
                   IF  (LNK-NYUINDAY-KBN(IDX)          =  SPACE)  AND
                       (LNK-NYUINDAY-TANKI3-KBN(IDX)   =  "1"  OR  "2")
                      MOVE    1             TO  FLG-TANKI3
                   END-IF
                   IF    FLG-TANKI3         =   1
                      MOVE   "1"            TO  WRK-TANKI3-DAY(IDX)
                   END-IF
             END-PERFORM
      *
      *       PERFORM   VARYING  IDX   FROM  1  BY  1
      *         UNTIL   IDX   >   31
      *            DISPLAY  "IDX = " IDX
      *            DISPLAY  "LNK-NYUINDAY-KBN(IDX)        = "
      *                      LNK-NYUINDAY-KBN(IDX)
      *            DISPLAY  "LNK-NYUINDAY-TANKI-KBN(IDX)  = "
      *                      LNK-NYUINDAY-TANKI-KBN(IDX)
      *            DISPLAY  "LNK-NYUINDAY-TANKI3-KBN(IDX) = "
      *                      LNK-NYUINDAY-TANKI3-KBN(IDX)
      *            DISPLAY  "WRK-TANKI3-DAY(IDX)          = "
      *                      WRK-TANKI3-DAY(IDX)
      *       END-PERFORM
      *
             IF    WRK-TANKI3-DAY(WRK-SANTEI-DD)   =   "1"
                MOVE    "1"                 TO    LNK-TANKI3HKT-KBN
             END-IF
      *
           .
       2001-MAIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.
