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
       PROGRAM-ID.                 ORCSCHKTALL.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¿ÇÎÅ¹Ô°ÙÆþÎÏ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡ÎÁÊñ³ç¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯½èÍý¡Ê·î¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  08/12/18    NACL¡ÝÂ¿¡¹Ç¼  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.00.00    NACL-Â¿¡¹Ç¼  18/10/XX  Ã»½Ñ£³²èÁü¿ÇÃÇÅù¥Á¥§¥Ã¥¯ÂÐ±þ
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
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HKTSANTEI           PIC 9(01).
           03  FLG-HKTSRYCD            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-NYUINACT           PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK2                PIC 9(01).
           03  FLG-CHK3                PIC 9(01).
           03  FLG-HKTCHK              PIC 9(01).
           03  FLG-HOUKATU             PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-N                   PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-T                   PIC 9(04).
           03  IDX-SRY                 PIC 9(04).
      *
           03  IDX-D                   PIC 9(02).
           03  IDD                     PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
           03  WRK-IDX-N               PIC 9(04).
      *H30.10
           03  WRK-TANKI3-IDX          PIC 9(04).
           03  WRK-TANKI3-DAY          PIC 9(02).
           03  FLG-TANKHKT             PIC 9(01).
      *
           03  WRK-K-SRYCD             PIC X(09).
      *    Êñ³ç
           03  WRK-NYUHKT-AREA.
               05  WRK-NYUHKT-OCCURS   OCCURS   31.
                   07  WRK-NYUHKTCD        PIC X(03).
                   07  WRK-NYUSRYCD        PIC X(09).
      *    ¥Á¥§¥Ã¥¯ÍÑ
       01  WRK-TBL-AREA.
           03  WRK-KANRICD             PIC  X(03).
      *
      *Æþ±¡ÎÁ
       01  TBL-NYUAREA.
           03  TBL-NYUDATA-G              OCCURS   100.
               05  TBL-N-KANRICD             PIC  X(03).
               05  TBL-N-SANTEI-HOU          PIC  X(01).
               05  TBL-N-SANTEI-KBN          PIC  X(01).
               05  TBL-N-NYUGAIKBN           PIC  X(01).
               05  TBL-N-HKNCOMBIKBN         PIC  X(01).
               05  TBL-N-SRYKBN-G.
                   07  TBL-N-SRYKBN-KBN      PIC X(01)
                                             OCCURS   50.
               05  TBL-N-SRYCD-G.
                   07  TBL-N-SRYCD           PIC X(09)
                                             OCCURS   200.
           03  TBL-N-MAX                 PIC 9(04).
      *
           03  TBL-HKTCHKFLG-G.
               05  TBL-HKTCHKFLG-OCC        OCCURS   31.
                   07  TBL-HKTCHKFLG        PIC 9(01).
      *
       01  TBL-HKTSANTEI-AREA.
           03  TBL-HKTSANTEI-DATA-G        OCCURS   600.
               05  TBL-HKTSRYCD            PIC X(09).
               05  TBL-SANTEIKBN           PIC X(01).
               05  TBL-KENTAIKBN           PIC X(01).
      *
      *    ½ÐÍè¹â»»ÄêÆü
       01  TBL-DEKIDAKA-AREA.
           03  TBL-DEKIDAKA-G              OCCURS   31.
               05  TBL-DEKIDAKA            PIC 9(01).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSK1014.INC".
      *    Êñ³ç¥Á¥§¥Ã¥¯¥Þ¥¹¥¿
       01  HKTSANTEI-REC.
           COPY    "CPHKTSANTEI.INC".
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥É¥Þ¥¹¥¿
       01  HKTSRYCD-REC.
           COPY    "CPHKTSRYCD.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¹Ô°Ù¥Þ¥¹¥¿¡Ý
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ÅÅ»ÒÅÀ¿ôÉ½¥Þ¥¹¥¿¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSCETNSCHK.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ¿ÇÎÅ¹Ô°ÙÍÑ¶¦ÄÌ¥Ñ¥é¥á¥¿
           COPY    "K01COMMON-SPA".
      *
      *H30.10
      *    Ã»´üÂÚºß¼ê½Ñ£³¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSTANKI3HKT.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Êñ³ç¿ÇÎÅ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSCHKTALL.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCHKTALLAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
      *
           INITIALIZE                  ORCSCHKTALL-OUTAREA
      *
           MOVE    ORCSCHKTALL-SRYYMD  TO  WRK-SRYYMD
           IF     (ORCSCHKTALL-SRYDD   =   SPACE
                                       OR  "00" )
               MOVE    "01"            TO  WRK-SRYDD
           END-IF
      *
      *    Êñ³çÂÐ¾ÝÍ­Ìµ
           PERFORM 100-INIT-SYORI-SEC
      *    Æþ±¡ÎÁ¥Á¥§¥Ã¥¯½èÍý
           PERFORM 200-NYUIN-CHKSYORI-SEC
           EVALUATE    ORCSCHKTALL-KBN
               WHEN    "1"
      *            Æþ±¡ÎÁÊñ³çÀßÄêÍ­Ìµ¤Î¤ß
                   CONTINUE
               WHEN    OTHER
      *            Æþ±¡ÎÁÊñ³ç¥Á¥§¥Ã¥¯
                   PERFORM 300-HKTSRY-CHEK-SEC
           END-EVALUATE
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SYORI-SEC            SECTION.
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                  SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    WRK-SRYYMD          TO  SYS-1014-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1014-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-N
           INITIALIZE                  TBL-NYUAREA
           PERFORM UNTIL  (FLG-SYSKANRI    =   1  )
                       OR (IDX             >=  100 )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               IF     (SYS-1014-SANTEI-HOU     =   "1"
                                               OR  "2"  )
               AND    (SYS-1014-KBNCD (1:3)    >=  "500" )
      *            Æþ±¡ÎÁ¤ÎÊñ³ç»»ÄêÊýË¡¤¢¤ê
                   PERFORM 1002-KBNCD-TBLHEN-SEC
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸½èÍý
      *****************************************************************
       1002-KBNCD-TBLHEN-SEC            SECTION.
      *
      *    Æþ±¡ÎÁ
           ADD     1                   TO  IDX-N
           MOVE    SYS-1014-KBNCD (1:3)
                                       TO  TBL-N-KANRICD (IDX-N)
      *    Êñ³ç»»ÄêÊýË¡
           MOVE    SYS-1014-SANTEI-HOU TO  TBL-N-SANTEI-HOU (IDX-N)
           MOVE    SYS-1014-SANTEI-KBN TO  TBL-N-SANTEI-KBN (IDX-N)
           MOVE    SYS-1014-NYUGAIKBN  TO  TBL-N-NYUGAIKBN (IDX-N)
           MOVE    SYS-1014-HKNCOMBI-KBN
                                       TO  TBL-N-HKNCOMBIKBN (IDX-N)
           MOVE    SYS-1014-SRYKBN-G   TO  TBL-N-SRYKBN-G (IDX-N)
           PERFORM 1001-HKTSRYCD-HEN-SEC
           MOVE    IDX-N               TO  TBL-N-MAX
      *
      *H30.10
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡¡¤¢¤ê
           IF      SYS-1014-KBNCD (1:3)    =   "526"
               MOVE    IDX-N               TO  WRK-TANKI3-IDX
           END-IF
           .
       1002-KBNCD-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÊÔ½¸½èÍý
      *****************************************************************
       1001-HKTSRYCD-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                  HKTSRYCD-REC
           MOVE    SYS-1014-KBNCD (1:3)
                                       TO  HKTSRYCD-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOEDYMD
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL  (FLG-HKTSRYCD    =   1  )
                      OR  (IDY             >=  200 )
               ADD     1                   TO  IDY
               MOVE    HKTSRYCD-SRYCD      TO  TBL-N-SRYCD
                                                           (IDX-N IDY)
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-HKTSRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ»»ÄêÈ½Äê½èÍý
      *****************************************************************
       200-NYUIN-CHKSYORI-SEC         SECTION.
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    ORCSCHKTALL-PTID    TO  LNK-NYUINDAY-PTID
           MOVE    ORCSCHKTALL-SRYYM   TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    ORCSCHKTALL-PTID    TO  NACCT-PTID
           MOVE    ORCSCHKTALL-SRYYM   TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *    Æþ±¡ÎÁ»»Äê¤Ê¤·
           IF      FLG-NYUINACCT       =   1
               MOVE    1                   TO  ORCSCHKTALL-NYUERR
           ELSE
               MOVE    ZERO                TO  ORCSCHKTALL-NYUERR
           END-IF
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               PERFORM 5001-NYUINACT-SYORI-SEC
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-NYUIN-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       5001-NYUINACT-SYORI-SEC         SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-IDX-N
           PERFORM UNTIL     ( FLG-NYUINACT    =   1 )
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5  )  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE)
                   PERFORM 5001-SRYCD-CHK-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
      *        Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡¦£³È½Äê
               IF    ((WRK-IDX-N               >   ZERO  ) AND
                      (TBL-N-KANRICD (WRK-IDX-N)   =   "526"
                                                   OR  "525"))
                 AND ( LNK-NYUINDAY-TANKI2-KBN (IDX-D)
                                           NOT =   SPACE     )
                   MOVE    1               TO  NACCT-DAY (IDX-D)
      *H30.10
      *            Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡¡¤¢¤ê
                   IF     (TBL-N-KANRICD (WRK-IDX-N)   =   "526")
                       MOVE    IDX-D           TO  WRK-TANKI3-DAY
                   END-IF
      *
               END-IF
      *
               IF    ( NACCT-DAY (IDX-D)   NOT =   ZERO  )
                   IF      WRK-IDX-N       >   ZERO
                       MOVE    TBL-N-KANRICD (WRK-IDX-N)
                                           TO  WRK-NYUHKTCD (IDX-D)
                       MOVE     WRK-K-SRYCD
                                           TO  WRK-NYUSRYCD (IDX-D)
      *                Æþ±¡ÎÁÊñ³ç¤¢¤ê
                       MOVE    1           TO  ORCSCHKTALL-NYUHKTKBN
                   ELSE
                       MOVE    "XXX"       TO  WRK-NYUHKTCD (IDX-D)
                   END-IF
               END-IF
           END-PERFORM
           .
       5001-NYUINACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       5001-SRYCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-K-SRYCD
           PERFORM VARYING     IDX-N   FROM    1  BY  1
                   UNTIL      (IDX-N       >   TBL-N-MAX )
                           OR (FLG-CHK     =   1         )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   200   )  OR
                                  (TBL-N-SRYCD (IDX-N IDY) =   SPACE)
                   IF      NSRY-SRYCD(IDZ) =   TBL-N-SRYCD (IDX-N IDY)
                       MOVE    NSRY-SRYCD(IDZ)     TO  WRK-K-SRYCD
                       MOVE    1                   TO  FLG-CHK
                       MOVE    IDX-N               TO  WRK-IDX-N
                       MOVE    200                 TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       5001-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁÊñ³ç¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       300-HKTSRY-CHEK-SEC         SECTION.
      *
      *    ½ÐÍè¹â»»ÄêÆü¸¡º÷
           PERFORM 3009-SANTEI-CHK-SEC
      *    Æþ±¡ÎÁÊñ³ç¿ÇÎÅ¶èÊ¬ÊÔ½¸½èÍý
           PERFORM 3001-HKTSRYKBN-SET-SEC
      *    ÆüËè¤ÎÊñ³çÀßÄê
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF     (WRK-NYUHKTCD (IDX)  =   SPACE
                                           OR  "XXX"  )
                   MOVE    ZERO            TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
                   MOVE    SPACE           TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
               ELSE
                   IF     (IDX             >   1   )  AND
                          (WRK-NYUHKTCD (IDX)
                                           =   WRK-NYUHKTCD (IDX - 1))
      *!!!
      *            Æþ±¡ÎÁ¥³¡¼¥ÉËè¤Ë¥Á¥§¥Ã¥¯¤¹¤ë
                   AND     (WRK-NYUSRYCD (IDX)
                                           =   WRK-NYUSRYCD (IDX - 1))
      *!!!
                       MOVE    ORCSCHKTALL-HKTCHKFLG (IDX - 1)
                                               TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
                       MOVE    WRK-NYUHKTCD (IDX - 1)
                                               TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
                   ELSE
                       MOVE    WRK-NYUSRYCD (IDX)
                                           TO  WRK-K-SRYCD
                       PERFORM 3002-NYUHKTCD-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *TEST
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¥Á¥§¥Ã¥¯
           IF     (WRK-SRYYMD      >=  "20180401")
              AND (WRK-TANKI3-IDX  >   ZERO  )
              AND (WRK-TANKI3-DAY  >   ZERO  )
               IF      ORCSCHKTALL-HKTSRYKBN   =   "60"
                                               OR  "70"
                    PERFORM 200131-TANKI3HKT-CHEK-SEC
               END-IF
           END-IF
      *TEST
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
      *        ½ÐÍè¹â»»ÄêÆü
               IF     (TBL-DEKIDAKA (IDX)  =   1      )
                   MOVE    ZERO            TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
                   MOVE    SPACE           TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
               END-IF
           END-PERFORM
           .
       300-HKTSRY-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡ÎÁÊñ³ç¿ÇÎÅ¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       3001-HKTSRYKBN-SET-SEC         SECTION.
      *
           EVALUATE    ORCSCHKTALL-HKTSRYKBN(1:1)
               WHEN    "2"
                   MOVE    1               TO  IDX-SRY
               WHEN    "3"
                   MOVE    2               TO  IDX-SRY
               WHEN    "4"
                   MOVE    3               TO  IDX-SRY
               WHEN    "5"
                   MOVE    4               TO  IDX-SRY
               WHEN    "6"
                   IF      ORCSCHKTALL-HKTSRYKBN   =  "64"
                       MOVE    10              TO  IDX-SRY
                   ELSE
                       MOVE    5               TO  IDX-SRY
                   END-IF
               WHEN    "7"
                   MOVE    6               TO  IDX-SRY
               WHEN    "8"
                   EVALUATE    ORCSCHKTALL-HKTSRYSYUKBN
                       WHEN    "800"
                           MOVE    7               TO  IDX-SRY
                       WHEN    "830"
                           MOVE    8               TO  IDX-SRY
                       WHEN    "840"
                           MOVE    9               TO  IDX-SRY
                   END-EVALUATE
      *H25.7 ÄÉ²Ã
               WHEN    OTHER
                   EVALUATE    ORCSCHKTALL-HKTSRYKBN
                       WHEN    "13"
      *                »ØÆ³ÎÁ
                           MOVE    11              TO  IDX-SRY
                       WHEN    "14"
      *                ºßÂðÎÁ
                           MOVE    12              TO  IDX-SRY
                       WHEN    "11"
                       WHEN    "12"
                       WHEN    "90"
                       WHEN    "92"
      *                ½é¿Ç¡¦ºÆ¿Ç¡¦Æþ±¡
                           MOVE    13              TO  IDX-SRY
                   END-EVALUATE
           END-EVALUATE
           .
       3001-HKTSRYKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ½ÐÍè¹â»»ÄêÆü¸¡º÷¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       3009-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-DEKIDAKA-AREA
      *    »»ÄêÍúÎò¸¡º÷
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    ORCSCHKTALL-PTID    TO  SANTEI-PTID
           MOVE    ORCSCHKTALL-SRYYM   TO  SANTEI-SRYYM
           MOVE    "099999904"         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               MOVE    SANTEI-DAY (IDD)    TO  TBL-DEKIDAKA (IDD)
           END-PERFORM
      *
           .
       3009-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       3002-NYUHKTCD-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-N-MAX )
              MOVE     ZERO                TO  FLG-HKTCHK
              IF       TBL-N-KANRICD (IDY) =   WRK-NYUHKTCD (IDX)
      *
      *            Êñ³çÈ½Äê
                   PERFORM 30021-HKTSRYCD-CHK-SEC
                   IF      TBL-N-SANTEI-HOU (IDY)  =   "1"
      *                Êñ³çºîÀ®
                       IF      FLG-HKTCHK      =   1
                           MOVE    1           TO  ORCSCHKTALL-HKTCHKFLG
                                                                   (IDX)
                       END-IF
                   ELSE
      *                ¥¨¥é¡¼¡ÊºîÀ®¤Ê¤·¡Ë
                       IF      FLG-HKTCHK      =   1
                           MOVE    9           TO  ORCSCHKTALL-HKTCHKFLG
                                                                   (IDX)
                       END-IF
                   END-IF
                   IF      FLG-HKTCHK          =   1
                       MOVE    1               TO  ORCSCHKTALL-NYUHKTCHK
                   END-IF
                   MOVE    TBL-N-KANRICD (IDY) TO  ORCSCHKTALL-NYUHKTCD
                                                                  (IDX)
                   MOVE    TBL-N-MAX           TO  IDY
               END-IF
           END-PERFORM
           .
       3002-NYUHKTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÆþ±¡ÎÁ¤ÎÈ½Äê½èÍý
      *****************************************************************
       30021-HKTSRYCD-CHK-SEC         SECTION.
      *
      *    ¿ÇÎÅ¶èÊ¬Ëè
           IF      IDX-SRY             >   ZERO
               IF      TBL-N-SRYKBN-KBN (IDY IDX-SRY)  =   "1"
      *            Êñ³ç¤µ¤ì¤ë
                   MOVE    1                   TO  FLG-HKTCHK
               END-IF
           END-IF
      *    ¿ÇÎÅ¥³¡¼¥É
           IF      ORCSCHKTALL-HKTSRYCD-G  NOT =   SPACE
               MOVE    TBL-N-KANRICD (IDY)     TO  WRK-KANRICD
               PERFORM 30022-HKTSANTEI-CHK-SEC
           END-IF
           .
       30021-HKTSRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ÆþÎÏ¹àÌÜ¤ÎÊñ³ç¥Á¥§¥¯½èÍý
      *****************************************************************
       30022-HKTSANTEI-CHK-SEC            SECTION.
      *
           INITIALIZE                  TBL-HKTSANTEI-AREA
      *    ÅÅ»ÒÅÀ¿ôÉ½ÂÐ¾ÝÊ¬¤«¤éÂÐ¾Ý³°
           IF      WRK-SRYYMD          >=  "20100401"
               MOVE    1               TO  FLG-HKTSANTEI
           ELSE
      *
           MOVE    SPACE               TO  HKTSANTEI-REC
           INITIALIZE                  HKTSANTEI-REC
           MOVE    WRK-KANRICD         TO  HKTSANTEI-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOEDYMD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    HKTSANTEI-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
           INITIALIZE                  TBL-HKTSANTEI-AREA
           MOVE    ZERO                TO  IDX-H
      *
           PERFORM UNTIL  (FLG-HKTSANTEI   =   1  )
                      OR  (IDX-H           >=  600 )
               ADD     1                   TO  IDX-H
               MOVE    HKTSANTEI-SRYCD     TO  TBL-HKTSRYCD (IDX-H)
               MOVE    HKTSANTEI-SANTEIKBN TO  TBL-SANTEIKBN (IDX-H)
               MOVE    HKTSANTEI-KENTAIKBN TO  TBL-KENTAIKBN (IDX-H)
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
           PERFORM VARYING     IDX-T    FROM    1   BY  1
                   UNTIL      (IDX-T    >   10   )  OR
                              (ORCSCHKTALL-HKTSRYCD(IDX-T) =   SPACE)
               MOVE    ORCSCHKTALL-HKTSRYCD(IDX-T)  TO  WRK-SRYCD
               IF     (TBL-HKTSRYCD (1)     =   SPACE )
      *            ÅÅ»ÒÅÀ¿ôÉ½¤è¤ê¥Á¥§¥Ã¥¯
                   PERFORM 300222-ETENSU-CHK-SEC
               ELSE
      *
               PERFORM VARYING     IDX-H    FROM    1   BY  1
                       UNTIL      (IDX-H    >  600   )
                               OR (TBL-HKTSRYCD(IDX-H) =   SPACE)
                   IF      WRK-SRYCD           =   TBL-HKTSRYCD (IDX-H)
                       MOVE    1               TO  FLG-CHK2
                       IF      TBL-SANTEIKBN (IDX-H)   =   "1"
                           MOVE    2               TO  FLG-CHK2
                       END-IF
                       IF      TBL-KENTAIKBN (IDX-H)    =   "1"
                           MOVE    1               TO  FLG-CHK3
                       END-IF
                       MOVE    600                 TO  IDX-H
                   END-IF
               END-PERFORM
      *
               END-IF
           END-PERFORM
      *    Êñ³ç¤µ¤ì¤ë
           IF      FLG-CHK2            =   1
               MOVE    1                   TO  FLG-HKTCHK
           END-IF
           .
       30022-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÅ»ÒÅÀ¿ôÉ½¤è¤ê¥Á¥§¥Ã¥¯
      *****************************************************************
       300222-ETENSU-CHK-SEC         SECTION.
      *
      *????IF      FLG-HKTCHK          =   1
      *!!!!!!! ¿ÇÎÅ¶èÊ¬¤ÇÊñ³ç¤Î»þ¤Î¤ß
               INITIALIZE                  ORCSCETNSCHKAREA
               MOVE    "1"                 TO  ORCSCETNSCHK-SYORI
               MOVE    WRK-SRYYMD          TO  ORCSCETNSCHK-SRYYMD
               MOVE    WRK-K-SRYCD         TO  ORCSCETNSCHK-SRYCD1
               MOVE    WRK-SRYCD           TO  ORCSCETNSCHK-SRYCD2
               CALL    "ORCSCETNSCHK"      USING
                                           ORCSCETNSCHKAREA
                                           SPA-AREA
               IF      ORCSCETNSCHK-CHKKBN =   ZERO
      *            Êñ³çÂÐ¾Ý¤Ê¤·
                   MOVE    ZERO            TO  FLG-HKTCHK
               END-IF
               IF      ORCSCETNSCHK-CHKKBN =   1
      *            Êñ³ç¤¢¤ê
                   MOVE    1               TO  FLG-HKTCHK
               END-IF
      *****END-IF
           .
       300222-ETENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³½èÍý
      *****************************************************************
       200131-TANKI3HKT-CHEK-SEC         SECTION.
      *
           MOVE    ZERO                 TO  FLG-TANKHKT
           PERFORM VARYING     IDX-T    FROM    1   BY  1
                   UNTIL      (IDX-T    >   10   )
                         OR   (ORCSCHKTALL-HKTSRYCD(IDX-T) =   SPACE)
                         OR   (FLG-TANKHKT      =   1   )
               MOVE    ORCSCHKTALL-HKTSRYCD(IDX-T)  TO  WRK-SRYCD
      *
               INITIALIZE                  ORCSTANKI3HKT-AREA
      *        °åÎÅµ¡´ØÈÖ¹æ
               MOVE    SPA-HOSPNUM         TO  LNK-TANKI3HKT-HOSPNUM
      *        ´µ¼Ô£É£Ä
               MOVE    SPA-PTID            TO  LNK-TANKI3HKT-PTID
      *        »»ÄêÇ¯·îÆü
               MOVE    WRK-SRYYMD          TO  LNK-TANKI3HKT-SANTEIYMD
               MOVE    WRK-TANKI3-DAY      TO  LNK-TANKI3HKT-SANTEIYMD
                                                         (7:2)
      *        ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
               MOVE    WRK-SRYCD           TO  LNK-TANKI3HKT-SRYCD
      *
               CALL    "ORCSTANKI3HKT"     USING
                                           ORCSTANKI3HKT-AREA
                                           SPA-AREA
               IF      LNK-TANKI3HKT-KBN   =   "1"
      *             Êñ³ç¤¢¤ê¡¡»»ÄêÆü°Ê¹ß¤Ï¤¹¤Ù¤ÆÊñ³ç
                    MOVE    1               TO  FLG-TANKHKT
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    WRK-TANKI3-DAY   BY  1
                   UNTIL       IDX     >   31
      *        »»ÄêÆü°Ê¹ß¤Ï¤¹¤Ù¤ÆÊñ³ç
               IF     (FLG-TANKHKT         =   1  )
                   MOVE    1               TO  ORCSCHKTALL-HKTCHKFLG
                                                                  (IDX)
               END-IF
           END-PERFORM
           .
       200131-TANKI3HKT-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍýÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-HKTSRYCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
               MOVE    ZERO            TO  FLG-HKTSRYCD
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       910-HKTSRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    Êñ³ç¿ÇÎÅ¥³¡¼¥ÉÆÉ¤ß¹þ¤ß
      *****************************************************************
       910-HKTSANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSANTEI-REC
               MOVE    ZERO            TO  FLG-HKTSANTEI
           ELSE
               INITIALIZE                  HKTSANTEI-REC
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           .
       910-HKTSANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    »»ÄêÍúÎòÆÉ¤ß¹þ¤ß
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¼¡ÆÉ¹þ
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       930-NYUINACT-RAED-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       930-NYUINACT-RAED-EXT.
           EXIT.
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
       990-DBCLOSE-SEC         SECTION.
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
