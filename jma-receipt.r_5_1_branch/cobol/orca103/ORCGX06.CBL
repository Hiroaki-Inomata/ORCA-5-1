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
       PROGRAM-ID.                     ORCGX06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ登録
      *  コンポーネント名  : 例外コード登録画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/11/21    NACL−太田    新規作成
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
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    チェックマスタ情報用画面スパ
           COPY    "X01SCR-SPA".
      *
       01  CNT-AREA.
           03  CNT-SRYCD                   PIC 9(05).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-TEKCDDEL                PIC 9(01).
           03  FLG-KANJI                   PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-OK                      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-GMN                     PIC 9(05).
           03  IDX-SPA                     PIC 9(05).
           03  IDX-ERR                     PIC 9(05).
           03  IDX-CHK                     PIC 9(05).
           03  IDX-SRYCD                   PIC 9(05).
           03  IDX-TBL                     PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-SRYCD                   PIC X(20).
           03  WRK-CDSYUBETU               PIC X(01).
           03  WRK-TEKCD                   PIC X(20).
           03  WRK-INPUTCD                 PIC X(20).
           03  WRK-MOJI                    PIC 9(03).
           03  WRK-BASEPOS                 PIC 9(05).
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-NAME                PIC X(100).
           03  WRK-NAME                    PIC X(100).
           03  WRK-LINE-MAX                PIC 9(05).
           03  WRK-LINE-MIN                PIC 9(05).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-ERR-NAME                PIC X(100).
           03  WRK-MAX                     PIC 9(05).
           03  WRK-CURSOR                  PIC 9(05).
           03  WRK-PAGE                    PIC 9(05).
           03  WRK-CUR-G.
               05  WRK-CUR                 PIC 9(02).
      *
       01  TBL-AREA.
           03  TBL-MAX                     PIC 9(05).
           03  TBL-OCC                     OCCURS  100.
               05  TBL-TEKCD               PIC X(20).
               05  TBL-TEKNAME             PIC X(100).
               05  TBL-TEKCDBF             PIC X(20).
      *
       01  CONST-AREA.
           03  CONST-TEKCD-MAX             PIC 9(05)   VALUE   100.
           03  CONST-LINE-MAX              PIC 9(03)   VALUE   20. 
           03  CONST-PAGE-MAX              PIC 9(02)   VALUE   5.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    自院マスター
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"           ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "X06"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "X98"
               PERFORM 320-X98SPASET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-X06-AREA
           MOVE    SPA-X01-SRYCD       TO  SPA-GMN-X06-SRYCD
           MOVE    SPA-X01-NAME        TO  SPA-GMN-X06-NAME
      *
           MOVE    SPA-X06-MAX         TO  SPA-GMN-X06-TEK-MAX
           PERFORM VARYING IDX-SPA     FROM    1   BY  1
                   UNTIL ( IDX-SPA     >   SPA-GMN-X06-TEK-MAX )
      *
               MOVE    SPA-X06-TEKCD (IDX-SPA)
                                       TO  SPA-GMN-X06-TEKCD  (IDX-SPA)
                                           SPA-GMN-X06-TEKCDBF(IDX-SPA)
      *
               PERFORM 3101-TENNAME-SEC
      *
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-X06-PAGE
      *
           MOVE    1                   TO  SPA-GMN-X06-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       3101-TENNAME-SEC          SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-X06-TEKCD (IDX-SPA)
                                       TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY4-SEL-SEC
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-NAME
           PERFORM UNTIL ( FLG-TENSU   =   1 )
      *
               MOVE    SPACE           TO  WRK-TNS-NAME
      *
               IF    ( TNS-SRYCD(1:1)      =   "1") AND
                     ( TNS-ROUTEKKBN       =   2  )
      *
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    WRK-TNS-NAME
                   END-STRING
      *
               ELSE
                   MOVE    TNS-NAME    TO  WRK-TNS-NAME
               END-IF
      *
      *        有効終了日が対象のマスタを対象とする
               IF      (SPA-X01-KJNYMD     <=  TNS-YUKOEDYMD )
                   MOVE    WRK-TNS-NAME
                                       TO  SPA-GMN-X06-TEKNAME (IDX-SPA)
                   MOVE    1           TO  FLG-TENSU
                   MOVE    1           TO  FLG-OK
               ELSE
                   MOVE    WRK-TNS-NAME
                                       TO  WRK-NAME
               END-IF
      *
               PERFORM 900-TENSU-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-OK              =   ZERO )
            AND  ( WRK-NAME        NOT =   SPACE)
      *
               MOVE    SPACE           TO  SPA-GMN-X06-TEKNAME (IDX-SPA)
               STRING  "★削除済★"    DELIMITED   BY  SIZE
                       WRK-NAME        DELIMITED   BY  SIZE
               INTO    SPA-GMN-X06-TEKNAME (IDX-SPA)
               END-STRING
      *
           END-IF
      *
           .
       3101-TENNAME-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-X98SPASET-SEC               SECTION.
      *
           MOVE    ZERO            TO  CNT-SRYCD
           INITIALIZE                  TBL-AREA
      *
           IF    ( SPA-X98-SRYCD (1)   =   SPACE )
               GO  TO  320-X98SPASET-EXT
           END-IF
      *
           IF    ( SPA-X98-SYORIKBN    =   "1" )
               MOVE    SPA-X98-CHK-IDX TO  IDX-SPA
           ELSE
               COMPUTE IDX-SPA =   SPA-GMN-X06-TEK-MAX +   1
           END-IF
      *
           PERFORM VARYING IDX-SRYCD    FROM   1   BY  1
                   UNTIL ( IDX-SRYCD    >   5 )
      *
               IF    ( SPA-X98-SRYCD (IDX-SRYCD)   NOT =   SPACE )
                   IF    ( IDX-SPA     <=  CONST-TEKCD-MAX )
                       MOVE    SPA-X98-SRYCD (IDX-SRYCD)
                                       TO  TBL-TEKCD (IDX-SPA)
                       COMPUTE IDX-SPA    =   IDX-SPA    +   1
                       COMPUTE CNT-SRYCD  =   CNT-SRYCD   +   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  IDX-SPA
                                       SPA-GMN-X06-TEK-MAX
      *
           PERFORM VARYING IDX-TBL     FROM    1   BY  1
                   UNTIL ( IDX-TBL     >   CONST-TEKCD-MAX )
      *
               IF    ( TBL-TEKCD (IDX-TBL)     =   SPACE )
      *
                   COMPUTE IDX-SPA =   IDX-SPA +   1
      *
                   IF    ( SPA-X98-SYORIKBN    =   "1" )
                    AND  ( SPA-X98-CHK-IDX     =   IDX-SPA )
                       COMPUTE IDX-SPA =   IDX-SPA +   1
                   END-IF
      *
                   IF    ( IDX-SPA    <=  CONST-TEKCD-MAX )
                       IF    ( SPA-GMN-X06-TEKCD (IDX-SPA)
                                                   NOT =   SPACE )
                           MOVE    SPA-GMN-X06-TEK-OCC (IDX-SPA)
                                   TO  TBL-OCC (IDX-TBL)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    TBL-AREA        TO  SPA-GMN-X06-TEK-G
      *
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               IF    ( SPA-X98-SYORIKBN    =   "1" )
                   COMPUTE WRK-CURSOR  =   SPA-X98-CHK-IDX +   CNT-SRYCD
               ELSE
                   COMPUTE WRK-CURSOR  =   SPA-GMN-X06-TEK-MAX +   1
               END-IF
      *
               COMPUTE WRK-PAGE
                   = ( WRK-CURSOR       -   1 )
                   /   CONST-LINE-MAX   +   1
      *
               COMPUTE SPA-GMN-X06-CUR
                   =   WRK-CURSOR
                   - ( SPA-GMN-X06-PAGE    -   1 )
                   *   CONST-LINE-MAX
      *
               IF    ( WRK-PAGE    >   SPA-GMN-X06-PAGE )
                   IF    ( SPA-GMN-X06-PAGE    =   CONST-PAGE-MAX )
                       MOVE    212             TO  SPA-GMN-X06-CUR
                   ELSE
                       MOVE    207             TO  SPA-GMN-X06-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       320-X98SPASET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *    戻る
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 210-BACK
      *    前頁
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 4101-PREV-PAGE-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 4101-NEXT-PAGE-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-X06-CUR
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           PERFORM VARYING IDX-GMN FROM    1   BY  1
                   UNTIL ( IDX-GMN >   CONST-LINE-MAX )
      *
               COMPUTE IDX-SPA
                   =   IDX-GMN
                   +  (SPA-GMN-X06-PAGE - 1 )  *   CONST-LINE-MAX
      *
               MOVE    X06-TEKCD (IDX-GMN)
                                       TO  SPA-GMN-X06-TEKCD (IDX-SPA)
      *
               MOVE    SPA-GMN-X06-TEKCD (IDX-SPA)
                                       TO  WRK-TEKCD
               INSPECT WRK-TEKCD       REPLACING   ALL "-"  BY  " "
               IF    ( SPA-GMN-X06-TEKCD (IDX-SPA) NOT =   WRK-TEKCD )
                   MOVE    SPACE       TO  SPA-GMN-X06-TEKCD (IDX-SPA)
               END-IF
      *
      *        全角入力の時、１０文字まで
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    SPA-GMN-X06-TEKCD (IDX-SPA)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-X06-TEKCD (IDX-SPA)
      *
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    適用コード欄空行削除
           PERFORM 41011-CLEAN-BLANK-LINE-SEC
      *
           PERFORM VARYING IDX-SPA     FROM    1   BY  1
                   UNTIL ( IDX-SPA     >   SPA-GMN-X06-TEK-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
      *            適用コード
                   PERFORM 4101-TEKCD-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-SPA     FROM    1   BY  1
                   UNTIL ( IDX-SPA     >   SPA-GMN-X06-TEK-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
      *            同一コードチェック
                   PERFORM 41014-JYUFUKU-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    空白削除処理
      *****************************************************************
       41011-CLEAN-BLANK-LINE-SEC      SECTION.
      *
           MOVE    SPA-GMN-X06-TEK-G   TO  TBL-AREA
      *
           INITIALIZE                      SPA-GMN-X06-TEK-G
      *
           PERFORM VARYING IDX-SPA     FROM    1   BY  1
                   UNTIL ( IDX-SPA     >   CONST-TEKCD-MAX )
      *
               IF    ( TBL-TEKCD (IDX-SPA)
                                       NOT =   SPACE )
                   COMPUTE SPA-GMN-X06-TEK-MAX
                       =   SPA-GMN-X06-TEK-MAX     +   1
                   MOVE    TBL-OCC (IDX-SPA)
                                       TO  SPA-GMN-X06-TEK-OCC
                                               (SPA-GMN-X06-TEK-MAX)
               END-IF
      *
           END-PERFORM
      *
           IF    ( TBL-MAX         >   SPA-GMN-X06-TEK-MAX )
               MOVE    1               TO  FLG-TEKCDDEL
           END-IF
      *
           .
       41011-CLEAN-BLANK-LINE-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードチェック
      *****************************************************************
       4101-TEKCD-CHK-SEC                         SECTION.
      *
           MOVE    ZERO            TO  IDX-ERR
      *
           IF    ( SPA-GMN-X06-TEKCD (IDX-SPA)(1:1)
                                   =   "/" )
      *
               COMPUTE SPA-GMN-X06-CUR
                   =   IDX-SPA
                   -  ((SPA-GMN-X06-PAGE  -  1)
                   *   CONST-LINE-MAX )
      *
               MOVE    SPACE       TO  WRK-INPUTCD
               MOVE    ZERO        TO  WRK-MOJI
               MOVE    IDX-SPA     TO  IDX-CHK
               PERFORM 409-KENSAKU-SEC
           ELSE
               IF    ( SPA-GMN-X06-TEKCD(IDX-SPA)
                                   NOT =  SPA-GMN-X06-TEKCDBF (IDX-SPA))
                   PERFORM 41013-TENSU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-TEKCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                      SPA-X98-AREA
           MOVE    "2"                 TO  SPA-X98-TYPE
      *
           MOVE    IDX-CHK             TO  SPA-X98-CHK-IDX
      *
           INITIALIZE                      SPA-X98GMN-AREA
      *
           MOVE    "1"                 TO  SPA-X98-SYORIKBN
      *
           IF    ( WRK-INPUTCD     NOT =   SPACE )
      *
               EVALUATE    SPA-X01-CHKKBN
               WHEN    "1"
                   MOVE    "1"             TO  SPA-X98-GMN-KBN
                   MOVE    SPACE           TO  SPA-X98-GMN-SYORI
               END-EVALUATE
      *
               MOVE    WRK-INPUTCD     TO  SPA-X98-INPUTCD
               IF    ( SPA-X98-GMN-KBN =   "2" )
                   MOVE    "1"         TO  SPA-X98-TYPE
               ELSE
                   MOVE    SPACE       TO  SPA-X98-TYPE
               END-IF
      *
               IF    ( FLG-KANJI       =   1 )
                   MOVE    "J"         TO  SPA-X98-CDSYUBETU
               ELSE
                   IF    ( WRK-INPUTCD(1:1)    NUMERIC )
                       EVALUATE    WRK-MOJI
                       WHEN    3
                       WHEN    6
                           MOVE    "6" TO  SPA-X98-CDSYUBETU
                       WHEN    4
                           MOVE    "4" TO  SPA-X98-CDSYUBETU
                       WHEN    5
                           MOVE    "5" TO  SPA-X98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"     TO  SPA-X98-CDSYUBETU
                   END-IF
               END-IF
           END-IF
      *
      *    画面編集
           PERFORM 41011-X98-SET-SEC
      *
           MOVE    "X06"               TO  SPA-MOTOPG
           MOVE    "X98"               TO  SPA-SAKIPG
           MOVE    "X06"               TO  SPA-X98-MOTOPG
      *
           IF    ( SPA-X98-GMN-MAX     =   ZERO )
               MOVE    "INPUT"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"       TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "X98    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-X98-SET-SEC                           SECTION.
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGX98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
      *
           .
       41011-X98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタチェック処理
      *****************************************************************
       41013-TENSU-CHK-SEC             SECTION.
      *
      *    セットコードでの入力はエラーとする。
           PERFORM 410131-SETCD-CHK-SEC
      *
           IF  ( SPA-ERRCD             =   SPACE )
      *        入力コードマスタ−検索条件編集処理
               PERFORM 410131-INPUTCD-SELECT-EDIT-SEC
           END-IF
      *
           IF  ( SPA-ERRCD             =   SPACE )
               IF  ( WRK-MOJI              >   ZERO )
                   PERFORM 410131-TENSU-CHK-SEC
               END-IF
           END-IF
      *
           .
       41013-TENSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタチェック処理
      *****************************************************************
       410131-TENSU-CHK-SEC             SECTION.
      *
           INITIALIZE              INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    WRK-CDSYUBETU   TO  ICD-CDSYU
           MOVE    WRK-SRYCD       TO  ICD-INPUTCD
      *
           PERFORM 900-INPUTCD-SEL-KEY2-SEC
           IF  (   FLG-INPUTCD     =   ZERO )
               MOVE   ICD-SRYCD    TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   1 )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    IDX-SPA     TO  IDX-ERR
               END-IF
           ELSE
               IF    ( WRK-MOJI              =   9 )
                AND  ( FLG-KANJI             =   ZERO )
      *
                   MOVE   WRK-SRYCD    TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF  ( FLG-TENSU         =   1 )
                       MOVE    "0001"      TO  SPA-ERRCD
                       MOVE    IDX-SPA     TO  IDX-ERR
                   END-IF
               ELSE
                   COMPUTE SPA-GMN-X06-CUR
                       =   IDX-SPA
                       -  ((SPA-GMN-X06-PAGE  -  1)
                       *   CONST-LINE-MAX )
                   MOVE    WRK-SRYCD       TO  WRK-INPUTCD
                   MOVE    IDX-SPA         TO  IDX-CHK
                   PERFORM 409-KENSAKU-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
            AND  ( FLG-END                 =   ZERO )
               IF    ( TNS-SRYCD(1:1)      =   "1")
                AND  ( TNS-ROUTEKKBN       =   2  )
                   MOVE    SPACE       TO  WRK-NAME
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    WRK-NAME
                   END-STRING
               ELSE
                   MOVE    TNS-NAME    TO  WRK-NAME
               END-IF
      *
               MOVE    TNS-SRYCD       TO  SPA-GMN-X06-TEKCD  (IDX-SPA)
               MOVE    WRK-NAME        TO  SPA-GMN-X06-TEKNAME(IDX-SPA)
      *
      *        診療行為コードのチェック
               EVALUATE    SPA-X01-CHKKBN
               WHEN    "1"
                   IF    ( SPA-GMN-X06-TEKCD (IDX-SPA) (1:1)
                                       NOT =   "6" )
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    IDX-SPA TO  IDX-ERR
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( FLG-END             =   ZERO )
               MOVE    SPA-GMN-X06-TEKCD (IDX-SPA)
                                       TO  SPA-GMN-X06-TEKCDBF (IDX-SPA)
           END-IF
      *
           .
       410131-TENSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    セットコードチェック処理
      *****************************************************************
       410131-SETCD-CHK-SEC            SECTION.
      *
           IF  ( SPA-GMN-X06-TEKCD (IDX-SPA)(1:1)  =   "P"  OR  "S" )
               MOVE    "0012"      TO  SPA-ERRCD
               MOVE    IDX-SPA     TO  IDX-ERR
           END-IF
      *
           .
       410131-SETCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスタ−検索条件編集処理
      *****************************************************************
       410131-INPUTCD-SELECT-EDIT-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  WRK-MOJI
      *
           MOVE    SPA-GMN-X06-TEKCD (IDX-SPA)
                                       TO  WRK-SRYCD
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL  (IDX0    >   20  )
                    OR    (WRK-SRYCD (IDX0:)   =   SPACE )
      *
               COMPUTE WRK-MOJI    =   WRK-MOJI    +   1
      *
           END-PERFORM
      *
           IF  ( WRK-MOJI          >   ZERO )
      *
      *        全角半角混在チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-SRYCD       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      ( KANACHK-RC    NOT =   ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    IDX-SPA     TO  IDX-ERR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   EVALUATE    KANACHK-SYUBETU
      *            半角入力
                   WHEN    1
                       MOVE    ZERO        TO  FLG-KANJI
      *
                       IF  ( WRK-SRYCD (1:WRK-MOJI) IS  NUMERIC )
                           EVALUATE    WRK-MOJI
                           WHEN    4
                               MOVE    "4" TO  WRK-CDSYUBETU
                           WHEN    5
                               MOVE    "5" TO  WRK-CDSYUBETU
                           WHEN    6
                           WHEN    3
                               MOVE   "6"  TO  WRK-CDSYUBETU
                           WHEN    OTHER
                               MOVE    "6" TO  WRK-CDSYUBETU
                           END-EVALUATE
                       ELSE
                           MOVE    "A"     TO  WRK-CDSYUBETU
                       END-IF
      *            全角入力
                   WHEN    2
                       MOVE    1           TO  FLG-KANJI
                       MOVE    "J"         TO  WRK-CDSYUBETU
                   WHEN    OTHER
                       MOVE    "0001"      TO  SPA-ERRCD
                       MOVE    IDX-SPA     TO  IDX-ERR
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
      *
       410131-INPUTCD-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同一コードチェック処理
      *****************************************************************
       41014-JYUFUKU-CHK-SEC           SECTION.
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-X06-PAGE    *   CONST-LINE-MAX
      *
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           COMPUTE WRK-STIDX   =   IDX-SPA +   1
      *
           PERFORM VARYING     IDX0    FROM    WRK-STIDX   BY  1
                   UNTIL     ( IDX0            >   SPA-GMN-X06-TEK-MAX )
                    OR       ( SPA-ERRCD   NOT =   SPACE)
      *
               IF    ( SPA-GMN-X06-TEKCD (IDX-SPA)
                                           =   SPA-GMN-X06-TEKCD (IDX0))
      *
                   MOVE    SPA-GMN-X06-TEKNAME (IDX-SPA)
                                           TO  WRK-ERR-NAME
                   MOVE    "0005"          TO  SPA-ERRCD
      *
                   IF    ( IDX-SPA        >=  WRK-LINE-MIN )
                    AND  ( IDX-SPA        <=  WRK-LINE-MAX )
                       MOVE    IDX-SPA     TO  IDX-ERR
                   ELSE
                       MOVE    IDX0        TO  IDX-ERR
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       41014-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適用欄エラー時カーソル位置決定処理
      *****************************************************************
       4101-ERRCUR-POS-SEC             SECTION.
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-X06-PAGE    *   CONST-LINE-MAX
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           EVALUATE    TRUE
           WHEN  ( IDX-ERR     <   WRK-LINE-MIN )
      *
               MOVE    206     TO  SPA-GMN-X06-CUR
      *
           WHEN  ( IDX-ERR     >   WRK-LINE-MAX )
      *
               MOVE    207     TO  SPA-GMN-X06-CUR
      *
           WHEN    OTHER
      *
               COMPUTE SPA-GMN-X06-CUR
                   =   IDX-ERR
                   -   WRK-LINE-MIN +   1
                   +   WRK-BASEPOS
      *
           END-EVALUATE
      *
           .
       4101-ERRCUR-POS-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       4101-PREV-PAGE-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           COMPUTE SPA-GMN-X06-PAGE
               =   SPA-GMN-X06-PAGE   -   1
      *
           MOVE    1           TO  SPA-GMN-X06-CUR
      *
           .
       4101-PREV-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    後頁処理
      *****************************************************************
       4101-NEXT-PAGE-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           COMPUTE SPA-GMN-X06-PAGE    =   SPA-GMN-X06-PAGE +   1
      *
           COMPUTE IDX-SPA
               = ( SPA-GMN-X06-PAGE    -   1 )
               *   CONST-LINE-MAX
               +   1
      *
           PERFORM VARYING IDX-GMN     FROM    1   BY  1
                   UNTIL ( IDX-GMN     >   CONST-LINE-MAX )
                    OR   ( SPA-GMN-X06-TEKCD (IDX-SPA) =   SPACE )
      *
               COMPUTE IDX-SPA =   IDX-SPA +   1
      *
           END-PERFORM
      *
           IF    ( IDX-GMN        >    CONST-LINE-MAX )
               MOVE    1               TO  SPA-GMN-X06-CUR
           ELSE
               MOVE    IDX-GMN         TO  SPA-GMN-X06-CUR
           END-IF
      *
           .
       4101-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "X04"               TO  SPA-SAKIPG
           MOVE    "X06"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4901-KAKUTEI-SEC
           END-IF
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       4901-KAKUTEI-SEC             SECTION.
      *
           MOVE    SPA-GMN-X06-TEK-MAX     TO  SPA-X06-MAX
           PERFORM VARYING IDX-SPA  FROM   1   BY  1
                   UNTIL ( IDX-SPA  >   SPA-GMN-X06-TEK-MAX )
               MOVE    SPA-GMN-X06-TEKCD (IDX-SPA)
                                           TO  SPA-X06-TEKCD (IDX-SPA)
           END-PERFORM
      *
           MOVE    "X04"               TO  SPA-SAKIPG
           MOVE    "X06"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4901-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE           TO  X06
           INITIALIZE                  X06
      *
           MOVE    SPA-GMN-X06-SRYCD   TO  X06-SRYCD
           MOVE    SPA-GMN-X06-NAME    TO  X06-NAME
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-X06-PAGE    *   CONST-LINE-MAX
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           MOVE    ZERO                TO  IDX-GMN
           MOVE    ZERO                TO  WRK-MAX
      *
           PERFORM VARYING IDX-SPA FROM    WRK-LINE-MIN   BY  1
                   UNTIL ( IDX-SPA >       WRK-LINE-MAX )
      *
               COMPUTE IDX-GMN =   IDX-GMN +   1
      *
               MOVE    IDX-SPA         TO  X06-NO      (IDX-GMN)
               MOVE    SPA-GMN-X06-TEKCD  (IDX-SPA)
                                       TO  X06-TEKCD   (IDX-GMN)
               MOVE    SPA-GMN-X06-TEKNAME   (IDX-SPA)
                                       TO  X06-TEKNAME (IDX-GMN)
      *
               IF    ( SPA-GMN-X06-TEKCD (IDX-SPA)     NOT =   SPACE )
                   MOVE    IDX-GMN     TO  WRK-MAX
               END-IF
      *
           END-PERFORM
      *
           MOVE    "blue"              TO  X06-LBLTITLE-STYLE
      *
           MOVE    WIDGET-NORMAL       TO  X06-B06-STATE
                                           X06-B07-STATE
      *
           IF    ( SPA-GMN-X06-PAGE    =   1 )
               MOVE    WIDGET-INSENSITIVE  TO  X06-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-X06-PAGE   >=   CONST-PAGE-MAX )
                   MOVE    WIDGET-INSENSITIVE  TO  X06-B07-STATE
           ELSE
               COMPUTE IDX-SPA
                   =   SPA-GMN-X06-PAGE    *   CONST-LINE-MAX
               IF    ( SPA-GMN-X06-TEKCD (IDX-SPA) =   SPACE )
                   MOVE    WIDGET-INSENSITIVE  TO  X06-B07-STATE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-X06-CUR     =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-X06-CUR
           WHEN   1    THRU    20
               MOVE    SPA-GMN-X06-CUR
                                   TO  WRK-CUR
               MOVE    SPACE       TO  MCP-WIDGET
               STRING  "TEKCD"     DELIMITED   BY  SIZE
                       WRK-CUR-G   DELIMITED   BY  SIZE
               INTO    MCP-WIDGET
               END-STRING
           WHEN   206
                MOVE  "B06"        TO  MCP-WIDGET
           WHEN   207
                MOVE  "B07"        TO  MCP-WIDGET
           WHEN   212
                MOVE  "B12"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-MCP-WIDGET  >=  "TEKCD01" )
             AND ( WRK-MCP-WIDGET  <=  "TEKCD20" )
      *
               MOVE    WRK-MCP-WIDGET (6:2)
                                   TO  WRK-CUR-G
      *
               IF    ( WRK-MAX    <    WRK-CUR )
                   COMPUTE SPA-GMN-X06-CUR
                       =   WRK-MAX     +   1
               ELSE
                   IF    ( FLG-TEKCDDEL   =    ZERO )
                       COMPUTE SPA-GMN-X06-CUR
                           =   WRK-CUR
                           +   1
                   ELSE
                       MOVE    WRK-CUR    TO  SPA-GMN-X06-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-X06-CUR >  CONST-LINE-MAX )
                   MOVE    207            TO  SPA-GMN-X06-CUR
               END-IF
      *
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "点数マスタにありません"
                                           TO  SPA-ERRMSG
           WHEN    "0005"
      *
               IF    ( WRK-ERR-NAME    NOT =   SPACE )
                   MOVE    SPACE       TO  SPA-ERRMSG
                   STRING  "適用コードが重複しています【"
                                               DELIMITED BY SIZE
                           WRK-ERR-NAME        DELIMITED BY SPACE
                           "】"                DELIMITED BY SIZE
                   INTO    SPA-ERRMSG
                   END-STRING
               ELSE
                   MOVE    "適用コードが重複しています"
                                           TO  SPA-ERRMSG
               END-IF
      *
           WHEN    "0008"
               MOVE    "チェック区分に対して入力できないコードです"
                                           TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "セットコードは入力できません"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X06"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター検索（KEY）
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    SPA-X01-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索（KEY4）
      *****************************************************************
       900-TENSU-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（KEY4）
      *****************************************************************
       900-TENSU-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込処理
      *****************************************************************
       900-INPUTCD-SEL-KEY2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTCD-REC
           ELSE
               MOVE    1           TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-INPUTCD-SEL-KEY2-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
