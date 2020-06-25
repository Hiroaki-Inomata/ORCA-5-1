      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGSSUB05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 再計算サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/04/08    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.01    NACL-門脇    17/12/15  第三者行為現物給付に関わる
      *                                     負担金計算対応
      *  05.00.01    NACL-門脇    19/10/01  マル長対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ARI             PIC 9(01).
           03  FLG-GENBUTU         PIC 9(01).
           03  FLG-GENBUTU2        PIC 9(01).
           03  FLG-GENBUTU3        PIC 9(01).
           03  FLG-GENBUTU4        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-ZAIARI          PIC 9(01).
           03  FLG-CYOKI           PIC 9(01).
           03  FLG-CYOKI2          PIC 9(01).
           03  FLG-CYOKI3          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX3                PIC 9(05).
           03  IDX6                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM-END-TABLE.
               05  WRK-DENPNUM-END PIC 9(07)   OCCURS  100.
           03  WRK-IKT-TABLE       PIC X(64).
           03  WRK-IKT-PATHNAME    PIC X(64).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-TABLE2          PIC X(64).
           03  WRK-PATHNAME2       PIC X(64).
           03  WRK-TABLE3          PIC X(64).
           03  WRK-PATHNAME3       PIC X(64).
           03  WRK-IKTSORTKBN      PIC X(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1039.INC".
           COPY    "CPSK2010.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    負担金再計算用パラメタ
           COPY    "CPORCSSAIKEISAN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "CPSSUB05.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SSUB05-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    SSUB05-SYORIKBN
           WHEN    "1"
               PERFORM 440-SAIKEI-SEC
           WHEN    "2"
               PERFORM 440-IKATUSAIKEI-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      **************************************************************
      *    再計算処理
      **************************************************************
       440-SAIKEI-SEC           SECTION.
      *
      ******************************
      *    負担金再計算処理
      ******************************
           INITIALIZE                      LNK-ORCSSAIKEISAN-REC
      *
           MOVE     SPA-HOSPNUM        TO  LNK-ORCSSKS-HOSPNUM
           MOVE     SSUB05-NYUGAIKBN   TO  LNK-ORCSSKS-NYUGAIKBN
           MOVE     SSUB05-PTID        TO  LNK-ORCSSKS-PTID
           MOVE     SSUB05-DENPNUM     TO  LNK-ORCSSKS-DENPNUM
           MOVE     SSUB05-SRYKA       TO  LNK-ORCSSKS-SRYKA
           MOVE     SSUB05-SYSYMD      TO  LNK-ORCSSKS-SYSYMD
           MOVE     SSUB05-SYSHMS      TO  LNK-ORCSSKS-SYSHMS
      *
           CALL    "ORCSSAIKEISAN"     USING
                                       SPA-AREA
                                       LNK-ORCSSAIKEISAN-REC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE                  ORCSSYUACCAREA
               MOVE    "03"            TO  LNK-SYUACC-I-KBN
               MOVE    SSUB05-SKYYM    TO  LNK-SYUACC-I-SRYYM
               MOVE    SSUB05-NYUGAIKBN
                                       TO  SPA-NYUGAIKBN
               MOVE    SSUB05-PTID     TO  SPA-PTID
               MOVE    PTINF-BIRTHDAY  TO  SPA-BIRTHDAY
               CALL    "ORCSSYUKAMAIN"     USING
                                           SPA-AREA
                                           ORCSSYUACCAREA
           ELSE
               MOVE    "4008"          TO  SPA-ERRCD
           END-IF
           .
       440-SAIKEI-EXT.
           EXIT.
      **************************************************************
      *    外来分一括再計算処理
      **************************************************************
       440-IKATUSAIKEI-SEC           SECTION.
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SSUB05-SKYYM        TO  SYS-1039-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1039-STYUKYMD(7:2)
           MOVE    SYS-1039-STYUKYMD   TO  SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    SYS-1039-IKTSORTKBN TO  WRK-IKTSORTKBN
      *
           IF      SSUB05-SKYYM  < "202001"
             INITIALIZE                    SYS-2010-REC
           ELSE
             INITIALIZE                    SYS-2010-REC
             MOVE    "2010"            TO  SYS-2010-KANRICD
             MOVE    "972"             TO  SYS-2010-KBNCD
             MOVE    SSUB05-SKYYM      TO  SYS-2010-STYUKYMD(1:6)
             MOVE    "01"              TO  SYS-2010-STYUKYMD(7:2)
             MOVE    SYS-2010-STYUKYMD TO  SYS-2010-EDYUKYMD
             MOVE    SYS-2010-REC      TO  SYSKANRI-REC
             PERFORM 900-SYSKANRI-KEY10-SEL-SEC
             MOVE    SYSKANRI-REC      TO  SYS-2010-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX6
           INITIALIZE     WRK-DENPNUM-END-TABLE
      *
      ***  IF    ( SPA-GMN-NYUGAIKBN   =   "0" OR  "2" )
      ***      MOVE    "2"             TO  WRK-NYUGAIKBN
      ***  ELSE
      ***      GO      TO      440-IKATUSAIKEI-EXT
      ***  END-IF
           MOVE    "2"                 TO  WRK-NYUGAIKBN
      *
           PERFORM  4403-SRYACT-KENSAKU-SEC
      *
      *    現物給付計算：一括再計算対象の収納データがあるかチェック
      *
           MOVE    ZERO                TO  FLG-GENBUTU
           MOVE    ZERO                TO  FLG-GENBUTU2
           MOVE    ZERO                TO  FLG-GENBUTU3
           MOVE    ZERO                TO  FLG-GENBUTU4
           MOVE    ZERO                TO  FLG-CYOKI
           MOVE    ZERO                TO  FLG-CYOKI2
           MOVE    ZERO                TO  FLG-CYOKI3
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SSUB05-PTID         TO  SYU-PTID
           MOVE    SSUB05-SKYYM        TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    "tbl_syunou"        TO  WRK-TABLE
           MOVE    "key35"             TO  WRK-PATHNAME
           PERFORM 900-SYUNOU-SEL-IKT-SEC
           PERFORM   UNTIL   FLG-SYUNOU  =  1
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               IF    SYU-DENPJTIKBN  NOT =  "3"
      ***        IF   SYU-SAIKEISANKBN    = "1"
                 IF  (SYU-SYUHKNNUM   NOT = SPACE   )
                 AND (SYU-KOH1HKNNUM      = SPACE   )
                 AND (SYU-KOH2HKNNUM      = SPACE   )
                 AND (SYU-KOH3HKNNUM      = SPACE   )
                 AND (SYU-KOH4HKNNUM      = SPACE   )
                 AND (SYU-SRYYMD(1:6)    >= "201204")
                    MOVE    1     TO  FLG-GENBUTU2
                 END-IF
                 IF  (SYU-SYUHKNNUM   NOT = SPACE)
                 AND (SYU-KOH1HKNNUM  NOT = SPACE)
                 AND (SYU-KOH1HKNNUM  NOT = "970")
                    MOVE    1     TO  FLG-GENBUTU3
                 END-IF
                 IF   SYU-KOH1HKNNUM  = "970"
                    MOVE    1     TO  FLG-GENBUTU4
                 END-IF
                 IF  (SYU-KOH1HKNNUM    =   "972" OR "974")
                 AND (SYU-KOH2HKNNUM    =   SPACE         )
                 AND (SYU-KOH3HKNNUM    =   SPACE         )
                 AND (SYU-KOH4HKNNUM    =   SPACE         )
                 AND (SYU-SRYYMD(1:6)  >=   "202001"      )
                    MOVE    1     TO  FLG-CYOKI2
                 END-IF
                 IF  (SYU-KOH2HKNNUM    =   "972" OR "974")
                 OR  (SYU-KOH3HKNNUM    =   "972" OR "974")
                 OR  (SYU-KOH4HKNNUM    =   "972" OR "974")
                    MOVE    1     TO  FLG-CYOKI3
                 END-IF
               END-IF
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key35"         TO  WRK-PATHNAME
               PERFORM 900-SYUNOU-FET-SEC
           END-PERFORM
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key35"        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF     FLG-GENBUTU4  =   1
             IF   FLG-ZAIARI    =   1
             AND  FLG-GENBUTU2  =   1
             AND  FLG-GENBUTU3  =  ZERO
                    MOVE    3     TO  FLG-GENBUTU
             ELSE
                    MOVE    2     TO  FLG-GENBUTU
             END-IF
           ELSE
             IF  (FLG-GENBUTU2  =   1)
             AND (FLG-GENBUTU3  =   1)
                    MOVE    1     TO  FLG-GENBUTU
             END-IF
           END-IF
           IF    (FLG-CYOKI2    =   1)
           AND   (FLG-CYOKI3    =   1)
                    MOVE    1     TO  FLG-CYOKI
           END-IF
      *
           IF   (FLG-CYOKI              =  1 )
      ***  AND  (SYS-2010-CYOKI-FTNKBN  = "1")
             MOVE    "1"          TO  WRK-IKTSORTKBN
           END-IF
      *
           EVALUATE   FLG-GENBUTU
             WHEN        0
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key35"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key69"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
             WHEN        1
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key82"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key83"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
             WHEN        2
      *        通常診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key88"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key89"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
      *        第三者行為診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key90"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key91"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
             WHEN        3
      *        第三者行為診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key90"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key91"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
      *        通常診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key88"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key89"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  4402-IKATUSAIKEI-HENSYU-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD      NOT =   "0070" )
      *
               IF    SPA-ERRCD    NOT =  SPACE
                      MOVE    "0064"     TO  SPA-ERRCD
               ELSE
                  IF    WRK-DENPNUM-END(1)  =  ZERO
                      MOVE    "0060"     TO  SPA-ERRCD
                  END-IF
               END-IF
      *
               INITIALIZE                 ORCSSYUACCAREA
               MOVE    "03"           TO  LNK-SYUACC-I-KBN
               MOVE    SSUB05-SKYYM   TO  LNK-SYUACC-I-SRYYM
               MOVE    WRK-NYUGAIKBN  TO  SPA-NYUGAIKBN
               MOVE    SSUB05-PTID    TO  SPA-PTID
               MOVE    PTINF-BIRTHDAY TO  SPA-BIRTHDAY
               CALL    "ORCSSYUKAMAIN"    USING
                                          SPA-AREA
                                          ORCSSYUACCAREA
           END-IF
      *
           .
       440-IKATUSAIKEI-EXT.
           EXIT.
      **************************************************************
      *    外来分一括再計算編集処理
      **************************************************************
       4402-IKATUSAIKEI-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SSUB05-PTID         TO  SYU-PTID
           MOVE    SSUB05-SKYYM        TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    WRK-IKT-TABLE       TO  WRK-TABLE
           MOVE    WRK-IKT-PATHNAME    TO  WRK-PATHNAME
           PERFORM 900-SYUNOU-SEL-IKT-SEC
      *
           PERFORM   UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    MCPDATA-REC     TO   SYUNOU-REC
      *
               MOVE    ZERO            TO   FLG-ARI
               PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                   >  100 )
                         OR   (WRK-DENPNUM-END(IDX3)  =  ZERO)
                         OR   (FLG-ARI                =   1  )
                   IF  WRK-DENPNUM-END(IDX3)  =   SYU-DENPNUM
                         MOVE   1    TO   FLG-ARI
                   END-IF
               END-PERFORM
      *
               IF   SYU-DENPJTIKBN  NOT =   "3"
               AND  FLG-ARI             =   ZERO
                  INITIALIZE                   LNK-ORCSSAIKEISAN-REC
      *
                  MOVE     SYU-HOSPNUM    TO   LNK-ORCSSKS-HOSPNUM
                  MOVE     SYU-NYUGAIKBN  TO   LNK-ORCSSKS-NYUGAIKBN
                  MOVE     SYU-PTID       TO   LNK-ORCSSKS-PTID
                  MOVE     SYU-DENPNUM    TO   LNK-ORCSSKS-DENPNUM
                  MOVE     SYU-SRYKA      TO   LNK-ORCSSKS-SRYKA
                  MOVE     WRK-IKTSORTKBN TO   LNK-ORCSSKS-IKTSORTKBN
                  MOVE     SYS-1039-IKTKEISANKBN
                                          TO   LNK-ORCSSKS-IKTKEISANKBN
                  EVALUATE   FLG-GENBUTU
                    WHEN        1
                    WHEN        2
                           MOVE    "1"    TO   LNK-ORCSSKS-IKTKEISANKBN
                           MOVE    "1"    TO   LNK-ORCSSKS-IKTGENKEIFLG
                    WHEN        3
                           MOVE    "1"    TO   LNK-ORCSSKS-IKTKEISANKBN
                           MOVE    "2"    TO   LNK-ORCSSKS-IKTGENKEIFLG
                  END-EVALUATE
                  IF    FLG-CYOKI  =  1
                    MOVE     "1"          TO   LNK-ORCSSKS-IKTKEISANKBN
                    IF  SYS-2010-CYOKI-FTNKBN  = "1"
                           MOVE    "1"    TO   LNK-ORCSSKS-IKTCYOKIFLG
                    END-IF
                  END-IF
                  MOVE       "1"          TO   LNK-ORCSSKS-IKTJISSIFLG
                  MOVE     SSUB05-SYSYMD  TO   LNK-ORCSSKS-SYSYMD
                  MOVE     SSUB05-SYSHMS  TO   LNK-ORCSSKS-SYSHMS
      *
                  CALL    "ORCSSAIKEISAN"     USING
                                              SPA-AREA
                                              LNK-ORCSSAIKEISAN-REC
      *
                  PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                          >   15 )
                         OR   (LNK-ORCSSKS-DENPNUM-END(IDX3) =  ZERO)
                      ADD    1     TO   IDX6
                      IF    IDX6     <=  100
                        MOVE   LNK-ORCSSKS-DENPNUM-END(IDX3)
                                   TO   WRK-DENPNUM-END(IDX6)
                      END-IF
                  END-PERFORM
      *
               END-IF
      *
               MOVE    WRK-IKT-TABLE       TO  WRK-TABLE
               MOVE    WRK-IKT-PATHNAME    TO  WRK-PATHNAME
               PERFORM 900-SYUNOU-FET-SEC
      *
               IF    SPA-ERRCD   NOT =  SPACE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-IKT-TABLE       TO  MCP-TABLE
           MOVE    WRK-IKT-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4402-IKATUSAIKEI-HENSYU-EXT.
           EXIT.
      **************************************************************
      *    診療行為情報検索
      **************************************************************
       4403-SRYACT-KENSAKU-SEC           SECTION.
      *
           MOVE    ZERO             TO   FLG-ZAIARI
      *
           INITIALIZE                    SRYACT-REC
           MOVE    SPA-HOSPNUM      TO   SRY-HOSPNUM
           MOVE    WRK-NYUGAIKBN    TO   SRY-NYUGAIKBN
           MOVE    SSUB05-PTID      TO   SRY-PTID
           MOVE    SSUB05-SKYYM     TO   SRY-SRYYM
           MOVE    "099999931"      TO   SRY-SRYCD(1)
           MOVE    "099999931"      TO   SRY-SRYCD(2)
           MOVE    "099999931"      TO   SRY-SRYCD(3)
           MOVE    "099999931"      TO   SRY-SRYCD(4)
           MOVE    "099999931"      TO   SRY-SRYCD(5)
           MOVE    "tbl_sryact"     TO   WRK-TABLE
           MOVE    "key7"           TO   WRK-PATHNAME
           PERFORM  900-SRYACT-SEL-IKT-SEC
           PERFORM  UNTIL  FLG-SRYACT  =  1
                     OR    FLG-ZAIARI  =  1
             INITIALIZE                  SRYACCT-REC
             MOVE  SRY-HOSPNUM      TO   ACCT-HOSPNUM
             MOVE  SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
             MOVE  SRY-SRYKA        TO   ACCT-SRYKA
             MOVE  SRY-PTID         TO   ACCT-PTID
             MOVE  SRY-SRYYM        TO   ACCT-SRYYM
             MOVE  SRY-ZAINUM       TO   ACCT-ZAINUM
             MOVE  "tbl_sryacct"    TO   WRK-TABLE2
             MOVE  "key31"          TO   WRK-PATHNAME2
             PERFORM  900-SRYACCT-SEL-IKT-SEC
             IF    FLG-SRYACCT  =  ZERO
               IF  ACCT-ZAIKAISU  NOT =  ZERO
                 INITIALIZE                   HKNCOMBI-REC
                 MOVE  ACCT-HOSPNUM      TO   COMB-HOSPNUM
                 MOVE  ACCT-PTID         TO   COMB-PTID
                 MOVE  ACCT-HKNCOMBI     TO   COMB-HKNCOMBINUM
                 MOVE  "tbl_hkncombi"    TO   WRK-TABLE3
                 MOVE  "key"             TO   WRK-PATHNAME3
                 PERFORM  900-HKNCOMBI-SEL-IKT-SEC
                 IF  FLG-HKNCOMBI = ZERO
                   IF  COMB-KOH1HKNNUM  =  "970"
                     MOVE     1          TO   FLG-ZAIARI
                   END-IF
                 END-IF
                 MOVE    "tbl_hkncombi"  TO   MCP-TABLE
                 MOVE    "key"           TO   MCP-PATHNAME
                 PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
             END-IF
             MOVE    "tbl_sryacct"  TO   MCP-TABLE
             MOVE    "key31"        TO   MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *
             MOVE    "tbl_sryact"   TO   WRK-TABLE
             MOVE    "key7"         TO   WRK-PATHNAME
             PERFORM  900-SRYACT-FET-SEC
           END-PERFORM
           MOVE      "tbl_sryact"   TO   MCP-TABLE
           MOVE      "key7"         TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4403-SRYACT-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC                SECTION.
      *
           CALL    "ORCSDAILYKEY"      USING
                                       SDAILYKEYAREA
                                       SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理（一括再計算）
      **************************************************************
       900-SYUNOU-SEL-IKT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-IKT-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FET-EXT.
           EXIT.
      **************************************************************
      *    診療行為マスタ−検索処理（一括再計算）
      **************************************************************
       900-SRYACT-SEL-IKT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-SEL-IKT-EXT.
           EXIT.
      **************************************************************
      *    診療行為マスタ−FETCH処理
      **************************************************************
       900-SRYACT-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-FET-EXT.
           EXIT.
      **************************************************************
      *    診療会計マスタ−検索処理（一括再計算）
      **************************************************************
       900-SRYACCT-SEL-IKT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    WRK-TABLE2          TO  MCP-TABLE
           MOVE    WRK-PATHNAME2       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-SEL-IKT-EXT.
           EXIT.
      **************************************************************
      *    保険組み合わせ検索処理（一括再計算）
      **************************************************************
       900-HKNCOMBI-SEL-IKT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    WRK-TABLE3          TO  MCP-TABLE
           MOVE    WRK-PATHNAME3       TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-SEL-IKT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      **************************************************************
      *    患者情報検索処理(KEY)
      **************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SSUB05-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
