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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODR11S02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集サブ（保険情報チェック）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/05/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-HKNJAINF                PIC 9(01).
           03  FLG-HKNNUM                  PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-END                     PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-SPACE                   PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX-ITM                 PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-KANACHK-MAX             PIC 9(05).
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-LENGTH                  PIC S9(05).
           03  WRK-STR                     PIC X(100).
           03  WRK-TMP                     PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCSODR11S02.INC".
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SODR11S02-AREA
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SODR11S02-KANREN    =   SPACE )
               PERFORM 2001-KOBETSU-CHK-SEC
           ELSE
               PERFORM 2001-KANREN-CHK-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    個別項目チェック処理
      *****************************************************************
       2001-KOBETSU-CHK-SEC            SECTION.
      *
           EVALUATE    TRUE
           WHEN    ( SODR11S02-HKNJANUM    NOT =   SPACE )
               PERFORM 20011-HKNJANUM-CHK-SEC
           WHEN    ( SODR11S02-HONKZKKBN   NOT =   SPACE )
               PERFORM 20011-HONKZKKBN-CHK-SEC
           WHEN    ( SODR11S02-KIGO        NOT =   SPACE )
               PERFORM 20011-KIGO-CHK-SEC
           WHEN    ( SODR11S02-NUM         NOT =   SPACE )
               PERFORM 20011-NUM-CHK-SEC
           END-EVALUATE
      *
           .
       2001-KOBETSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       20011-HKNJANUM-CHK-SEC          SECTION.
      *
           MOVE    SPACE                   TO  SODR11S02-HKNJANAME
      *
           IF    ( SODR11S02-HKNJANUM    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO                TO  WRK-KANACHK-MAX
      *
      *        桁数チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SODR11S02-HKNJANUM
                                           TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SODR11S02-HKNJANUM
      *        半角で間に空白がないこと
               IF    ( KANACHK-RC      NOT =   ZERO  )
                OR   ( KANACHK-RC2     NOT =   ZERO  )
                OR   ( KANACHK-SYUBETU     =   2     )
                   MOVE    "0186"          TO  SODR11S02-ERRCD
               END-IF
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   IF    ( KANACHK-MAX     =   4  OR  6  OR  8 )
                       MOVE    KANACHK-MAX TO  WRK-KANACHK-MAX
                   ELSE
                       MOVE    "0186"      TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
      *
                   PERFORM 900-HKNJAINF-KEY-SEL-SEC
      *
      *            保険者番号あり
                   IF    ( FLG-HKNJAINF        =   ZERO )
                       MOVE    HKNJA-HKNJANAME TO  SODR11S02-HKNJANAME
                       MOVE    HKNJA-HKNNUM    TO  WRK-HKNNUM
      *                記号が空白以外の時、再編集
                       IF    ( HKNJA-KIGO  NOT =   SPACE )
                        AND  ( SODR11S02-KIGO  =   SPACE )
                           MOVE    HKNJA-KIGO  TO  SODR11S02-KIGO
                       END-IF
      *
                   ELSE
      *
      *                保険者番号モジュラス10チェック
                       IF    ( WRK-KANACHK-MAX >   4 )
      *
                           INITIALIZE                  LNK-CHKDGT-AREA
                           MOVE    2               TO  LNK-I-SYORIKBN
                           MOVE    SODR11S02-HKNJANUM
                                                   TO  LNK-I-CODE
                           CALL    "ORCSCHKDGT"        USING
                                                       LNK-CHKDGT-AREA
                           IF    ( LNK-O-ERRCD     NOT =   ZERO )
                               MOVE    "0186"      TO  SODR11S02-ERRCD
                           END-IF
                       END-IF
      *
                       IF    ( SODR11S02-ERRCD              =    SPACE )
      *
      *                    法より編集
                           EVALUATE    WRK-KANACHK-MAX
                           WHEN    4
      *                        政府管掌
                               MOVE    "001"       TO  WRK-HKNNUM
                           WHEN    6
      *                        国保
                               MOVE    "060"       TO  WRK-HKNNUM
                           WHEN    8
      *                        その他
                               PERFORM 200111-HKNNUM-HBTNUM-SEC
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-HKNNUM          TO  SODR11S02-HKNNUM
      *
               IF    ( SODR11S02-ERRCD     =   SPACE )
                   IF    ( WRK-HKNNUM      =   SPACE )
                       MOVE    "1"         TO  SODR11S02-KANYUKBN
                   ELSE
                       IF    ( WRK-HKNNUM  =   "039" OR "040" )
                           MOVE    "0187"  TO  SODR11S02-ERRCD
                       ELSE
                           IF    ( WRK-HKNNUM  =   "060" OR "067"
                                                    OR "068" OR "069" )
                               MOVE    "2" TO  SODR11S02-KANYUKBN
                           ELSE
                               MOVE    "1" TO  SODR11S02-KANYUKBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    法別番号チェック処理
      *****************************************************************
       200111-HKNNUM-HBTNUM-SEC         SECTION.
      *
           PERFORM 900-HKNNUM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      NOT =  ZERO )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" )
                   MOVE    HKN-HKNNUM      TO  WRK-HKNNUM
               END-IF
      *
               PERFORM 900-HKNNUM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-HKNNUM-HBTNUM-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分チェック処理
      *****************************************************************
       20011-HONKZKKBN-CHK-SEC         SECTION.
      *
           IF    ( SODR11S02-HONKZKKBN =   SPACE )
               CONTINUE
           ELSE
               IF    ( SODR11S02-HONKZKKBN =   "1" OR  "5" )
                   CONTINUE
               ELSE
                   MOVE    "0188"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           .
       20011-HONKZKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者証記号チェック処理
      *****************************************************************
       20011-KIGO-CHK-SEC              SECTION.
      *
           IF    ( SODR11S02-KIGO      =   SPACE )
               CONTINUE
           ELSE
               MOVE    SODR11S02-KIGO  TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR TO  SODR11S02-KIGO
               IF    ( FLG-ENTRYCHK    NOT =   ZERO )
                   MOVE    "0191"      TO  SODR11S02-ERRCD
               END-IF
      *
               IF    ( SODR11S02-ERRCD =   SPACE )
                   MOVE    ZERO            TO  CNT-SPACE
                   INSPECT SODR11S02-KIGO  TALLYING
                           CNT-SPACE       FOR ALL "　"
                   IF    ( CNT-SPACE       >   ZERO )
                       MOVE    "0189"  TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   MOVE    SODR11S02-KIGO
                                       TO  WRK-STR
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-LENGTH  >   38 )
                       MOVE    "0189"  TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       20011-KIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者証番号チェック処理
      *****************************************************************
       20011-NUM-CHK-SEC               SECTION.
      *
           IF    ( SODR11S02-NUM   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SODR11S02-NUM       TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR     TO  SODR11S02-NUM
               IF    ( FLG-ENTRYCHK    NOT =   ZERO )
                   MOVE    "0192"          TO  SODR11S02-ERRCD
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   MOVE    ZERO            TO  CNT-SPACE
                   INSPECT SODR11S02-NUM   TALLYING
                           CNT-SPACE       FOR ALL "　"
                   IF    ( CNT-SPACE       >   ZERO )
                       MOVE    "0190"      TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   MOVE    SODR11S02-NUM   TO  WRK-STR
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-LENGTH  >   38 )
                       MOVE    "0190"      TO  SODR11S02-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2001-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SODR11S02-HKNJANUM  =   SPACE )
               MOVE    "8018"          TO  SODR11S02-ERRCD
           END-IF
      *
           IF    ( SODR11S02-ERRCD               =   SPACE )
               IF    ( SODR11S02-HONKZKKBN =   SPACE )
                   MOVE    "8019"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           IF    ( SODR11S02-ERRCD           =   SPACE )
               IF    ( SODR11S02-NUM   =   SPACE )
                   MOVE    "8020"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           IF    ( SODR11S02-ERRCD           =   SPACE )
               MOVE    SPACE           TO  WRK-STR
               STRING  SODR11S02-KIGO  DELIMITED BY SPACE
                       SODR11S02-NUM   DELIMITED BY SPACE
               INTO    WRK-STR
               END-STRING
               PERFORM 800-LENGTH-CHK-SEC
               IF    ( WRK-LENGTH  >   38 )
                   MOVE    "8021"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           .
       2001-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    全角チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-STR   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    =   ZERO )
      *            全角でない場合はエラーとする。
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列長チェック
      *****************************************************************
       800-LENGTH-CHK-SEC              SECTION.
      *
           MOVE    1               TO  WRK-LENGTH
           MOVE    SPACE           TO  WRK-TMP
      *
           STRING  WRK-STR DELIMITED   BY  SPACE
           INTO    WRK-TMP
           WITH POINTER  WRK-LENGTH
           END-STRING
           COMPUTE WRK-LENGTH = WRK-LENGTH -   1
      *
           .
       800-LENGTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者情報検索処理
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
      *    保険者番号　検索チェック
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    SODR11S02-HKNJANUM  TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号検索処理
      *****************************************************************
       900-HKNNUM-KEY4-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    SODR11S02-HKNJANUM(1:2)
                                   TO  HKN-HBTNUM
           MOVE    SODR11S02-SRYYMD TO HKN-TEKSTYMD
           MOVE    SODR11S02-SRYYMD TO HKN-TEKEDYMD
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
                MOVE    1          TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号FETCH処理
      *****************************************************************
       900-HKNNUM-KEY4-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
                MOVE    1          TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
