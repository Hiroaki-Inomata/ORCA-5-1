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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI000R4V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : APIバージョン取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/01/25    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-MATCHED             PIC 9(01).
           03  FLG-UNMATCHED           PIC 9(01).
           03  FLG-WILDCARD            PIC 9(01).
           03  FLG-TEXT-ERR            PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-RESPONSE            PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXP                    PIC 9(05).
           03  IDXT                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-TEXT                PIC X(512).
           03  WRK-DATETIME.
               05  WRK-DATETIME-DATE   PIC X(10).
               05  WRK-DATETIME-FL     PIC X(01).
               05  WRK-DATETIME-TIME   PIC X(05).
           03  WRK-PATTERN             PIC X(32).
      *
           COPY    "CPAPIVERSIONGETV2REQ.INC".
           COPY    "CPAPIVERSIONGETV2RES.INC".
           COPY    "CPAPIINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    文字列チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1010.INC".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "************************"
           DISPLAY   "* apiversiongetv2 start*"
           DISPLAY   "************************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  XML-AVGREQ
           INITIALIZE                  XML-AVGRES
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***********************"
           DISPLAY   "* apiversiongetv2 end *"
           DISPLAY   "***********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"           TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  SPA-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  SPA-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  SPA-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  SPA-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  SPA-ERRCD
               END-EVALUATE
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-ID-SEC
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ＩＤチェック処理
      *****************************************************************
       10011-ID-SEC                    SECTION.
      *
           IF    ( AVGREQ-ID           NOT =   SPACE )
               MOVE    AVGREQ-ID       TO  WRK-TEXT
               PERFORM 800-HANKAKU-CHK-SEC
               IF    ( FLG-TEXT-ERR    NOT =   ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       10011-ID-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-RESPONSE-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2001-RESPONSE-EDIT-SEC                  SECTION.
      *
           MOVE    ZERO                TO  CNT-RESPONSE
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       APIINF-MAX )
                    OR   ( CNT-RESPONSE   >=   200 )
                    OR   ( SPA-ERRCD  NOT  =   SPACE )
      *
               IF    ( AVGREQ-ID      NOT  =   SPACE )
                   MOVE    AVGREQ-ID   TO  WRK-PATTERN
                   MOVE    APIINF-ID (IDX1)
                                       TO  WRK-TEXT
                   PERFORM 20011-MATCHING-SEC
                   IF    ( FLG-MATCHED   =     1 )
                       PERFORM 20011-RESPONSE-EDIT-SEC
                   END-IF
               ELSE
                   PERFORM 20011-RESPONSE-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( CNT-RESPONSE    =   ZERO )
                   MOVE    "0002"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20011-RESPONSE-EDIT-SEC             SECTION.
      *
           COMPUTE CNT-RESPONSE =  CNT-RESPONSE  + 1
      *
           STRING  "/"                 DELIMITED BY SIZE
                   APIINF-LD (IDX1)    DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   APIINF-ID (IDX1)    DELIMITED BY SPACE
           INTO    AVGRES-URL (CNT-RESPONSE)
           END-STRING
      *
           MOVE    APIINF-NAME (IDX1)
                                   TO  AVGRES-NAME (CNT-RESPONSE)
           MOVE    APIINF-VERSION (IDX1)
                                   TO  AVGRES-VERSION (CNT-RESPONSE)
           MOVE    APIINF-HAORI-NAME (IDX1)
                                   TO  AVGRES-HAORI-NAME (CNT-RESPONSE)
      *
           .
       20011-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    マッチング処理
      *****************************************************************
       20011-MATCHING-SEC                  SECTION.
      *
           MOVE   ZERO            TO  FLG-MATCHED
           MOVE   ZERO            TO  FLG-UNMATCHED
           MOVE   1               TO  IDXP
           MOVE   1               TO  IDXT
           MOVE   1               TO  IDXB
           MOVE   ZERO            TO  FLG-WILDCARD
      *
           PERFORM UNTIL ( FLG-MATCHED     NOT =   ZERO )
                    OR   ( FLG-UNMATCHED   NOT =   ZERO )
      *
               EVALUATE    TRUE
               WHEN  ( IDXP    >   FUNCTION LENGTH (WRK-PATTERN) )
                OR   ( WRK-PATTERN (IDXP : )   =   SPACE )
                   IF    ( IDXT    >   FUNCTION LENGTH (WRK-TEXT))
                    OR   ( WRK-TEXT (IDXT : )  NOT =   SPACE )
                       IF    ( IDXP     >   1 )
                        AND  ( WRK-PATTERN (IDXP - 1 : 1)  =   "*" )
                           MOVE    1    TO  FLG-MATCHED
                       ELSE
                           MOVE    1    TO  FLG-UNMATCHED
                       END-IF
                   ELSE
                       MOVE    1        TO  FLG-MATCHED
                   END-IF
               WHEN  ( WRK-PATTERN (IDXP : 1 ) =   "*" )
                   MOVE    1       TO  FLG-WILDCARD
                   COMPUTE IDXP    =   IDXP    +   1
                   MOVE    IDXP    TO  IDXB
               WHEN    OTHER
                   IF    ( IDXT    >   FUNCTION LENGTH (WRK-TEXT))
                    OR   ( WRK-TEXT (IDXT : )  =   SPACE )
                       MOVE    1       TO  FLG-UNMATCHED
                   ELSE
                       IF    ( WRK-TEXT (IDXT : 1)
                                           =   WRK-PATTERN (IDXP : 1))
                           COMPUTE IDXP    =   IDXP + 1
                           COMPUTE IDXT    =   IDXT + 1
                       ELSE
                           IF    ( FLG-WILDCARD    =   ZERO )
                               MOVE    1       TO  FLG-UNMATCHED
                           ELSE
                               COMPUTE IDXT =  IDXT - (IDXP - IDXB) + 1
                               MOVE    IDXB    TO  IDXP
                           END-IF
                       END-IF
                   END-IF
               END-EVALUATE
           END-PERFORM
      *
           .
       20011-MATCHING-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"        TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
           IF    ( STS-DAY-RC1     =   ZERO )
               MOVE    LNK-DAY2-EDTYMD1
                               TO  WRK-EDTYMD1
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TEXT-ERR
      *
           IF  ( WRK-TEXT          NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-TEXT        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-TEXT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-TEXT-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-TEXT-ERR
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC               SECTION.
      *
           MOVE        SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0000"
               MOVE    "処理終了"      TO  SPA-ERRMSG
           WHEN    "0001"
               MOVE    "ＩＤに半角以外の文字が入力されています。"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "対象となる日レセＡＰＩが存在しませんでした。"
                                       TO  SPA-ERRMSG
      *共通エラー
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST38-BODY    NOT =   LOW-VALUE )
               DISPLAY "apiversiongetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_apiversiongetv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST38-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "apiversiongetv2req"    TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"               TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE               TO  APILST38-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  AVGREQ-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC           SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  AVGRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  AVGRES-INFORMATION-TIME
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  SPA-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD       TO  AVGRES-RESULT
           MOVE    SPA-ERRMSG      TO  AVGRES-RESULT-MSG
      *
           MOVE    XML-AVGRES      TO  XML-APIREQ
      *
           MOVE    1               TO  APIREQ-MODE
      *
           MOVE    "xml_apiversiongetv2res"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "apiversiongetv2res"
                                   TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  APILST38-BODY
               MOVE    "application/xml"
                                       TO  APILST38-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  APILST38-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
