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
       PROGRAM-ID.         ORAPI001R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 帳票データ取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/11/14    NACL−太田　新規作成
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  STS-AREA.
           03  STS-FORM              PIC X(02).
           03  STS-BODY                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-IN01                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
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
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPFORMDATAGETV2REQ.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    一時ディレクトリ作成作成
           COPY    "CPORCSTEMPDIR.INC".
      *
      *    API用帳票データレスポンス作成
           COPY    "CPORCSAPIFM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY     "API01RV2SCRAREA.INC".
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
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
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
               MOVE   "0099"       TO  WRK-ERRCD
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
      *
      *        ＳＰＡ共通設定
               INITIALIZE              SYS-1010-REC
               INITIALIZE              ORCSCOMMONAREA
               MOVE    "M00"       TO  ORCSCOMMON-PG
               CALL    "ORCSCOMMON"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE   "0089"   TO  WRK-ERRCD
               END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           IF    ( WRK-ERRCD       =   SPACE )
      *        XML情報取り出し
               PERFORM 900-XML-READ-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  SAPIFM-AREA
           MOVE    WRK-ERRCD       TO  SAPIFM-ERRCD
           MOVE    FDGREQ-DATAID   TO  SAPIFM-DATAID
           CALL    "ORCSAPIFM"         USING
                                       SAPIFM-AREA
                                       SPA-AREA
           IF    ( SAPIFM-OBJECT   NOT =   LOW-VALUE )
               MOVE    SAPIFM-OBJECT
                                   TO  APILST25-BODY
               MOVE    "application/json"
                                   TO  APILST25-CONTENT-TYPE
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST25-BODY    NOT =   LOW-VALUE )
               DISPLAY "formdatagetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_formdatagetv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST25-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "data"                  TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST25-BODY
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
                                       TO  FDGREQ-DATA
      *
           .
       9001-XML-FILTER-EXT.
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
