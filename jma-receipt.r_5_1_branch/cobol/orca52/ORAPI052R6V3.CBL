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
       PROGRAM-ID.         ORAPI052R6V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : マスタ最終更新日返却
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/11/12    NACL−太田　新規作成
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
       01  FLG-AREA.
           03  FLG-TENSU               PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-REQUESTED           PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
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
           03  WRK-HISTORY-G.
               05  WRK-HLASTYMD        PIC X(8).
               05  WRK-HCOUNT          PIC 9(8).
           03  WRK-COUNT-Z9            PIC Z(7)9.
      *
       01  CONST-AREA.
           03  CONST-TENSU             PIC X(32) VALUE
                                       "medication_master".
           03  CONST-BYOMEI            PIC X(32) VALUE
                                       "disease_master".
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
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
      *
      *    API XML読み込み用定義体
           COPY    "CPAPIV2REQ.INC".
           COPY    "CPMLUV3REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMLUV3RES.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    病名マスタ
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
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
           DISPLAY   "**************************"
           DISPLAY   "* masterlastupdate start *"
           DISPLAY   "**************************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "**************************"
           DISPLAY   "* masterlastupdate end   *"
           DISPLAY   "**************************"
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
           INITIALIZE                  XML-MLURES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"           TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
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
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-REQUESTED
           MOVE    ZERO                TO  IDXL
           MOVE    ZERO                TO  IDXL
      *
           IF    ( MLUREQ-MSTID        =   SPACE OR CONST-TENSU )
               MOVE    1               TO  FLG-REQUESTED
               PERFORM 20031-TENSU-SEC
           END-IF
      *
           IF    ( MLUREQ-MSTID        =   SPACE OR CONST-BYOMEI )
               MOVE    1               TO  FLG-REQUESTED
               PERFORM 20031-BYOMEI-SEC
           END-IF
      *
           IF    ( FLG-REQUESTED       =   ZERO )
               MOVE    "E01"           TO  SPA-ERRCD
           END-IF
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ最終更新日取得処理
      *****************************************************************
       20031-TENSU-SEC                 SECTION.
      *
           IF    ( MLUREQ-MSTID        =   SPACE )
               PERFORM 900-TENSU-LAST-UPDATE1-SEL-SEC
      *
               COMPUTE IDXL = IDXL + 1
      *
               MOVE    TNS-TERMID(1:8) TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    TNS-TERMID(9:6) TO  WRK-HMS
               PERFORM 800-TIME-SEC
      *
               MOVE    CONST-TENSU     TO  MLURES-MSTID (IDXL)
               MOVE    WRK-DATE        TO  MLURES-LASTYMD (IDXL)
           ELSE
               MOVE    CONST-TENSU     TO  MLURES-SOLO-MSTID
               PERFORM 900-TENSU-LAST-UPDATE2-SEL-SEC
               PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
      *
                   COMPUTE IDXL = IDXL + 1
      *
                   MOVE    TNS-TERMID      TO  WRK-HISTORY-G
                   MOVE    WRK-HLASTYMD    TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE        TO  MLURES-HLASTYMD (IDXL)
                   IF    ( IDXL        =   1 )
                       MOVE    WRK-DATE    TO  MLURES-SOLO-LASTYMD
                   END-IF
                   MOVE    WRK-HCOUNT      TO  WRK-COUNT-Z9
                   MOVE    FUNCTION TRIM(WRK-COUNT-Z9)
                                           TO  MLURES-HCOUNT (IDXL)
                   PERFORM 900-TENSU-LAST-UPDATE2-FET-SEC
               END-PERFORM
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "last_update2"      TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       20031-TENSU-EXT.
           EXIT.
      *****************************************************************
      *    病名マスタ最終更新日取得処理
      *****************************************************************
       20031-BYOMEI-SEC                SECTION.
      *
           IF    ( MLUREQ-MSTID        =   SPACE )
               PERFORM 900-BYOMEI-LAST-UPDATE1-SEL-SEC
      *
               COMPUTE IDXL = IDXL + 1
      *
               MOVE    BYO-TERMID(1:8) TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    BYO-TERMID(9:6) TO  WRK-HMS
               PERFORM 800-TIME-SEC
      *
               MOVE    CONST-BYOMEI    TO  MLURES-MSTID (IDXL)
               MOVE    WRK-DATE        TO  MLURES-LASTYMD (IDXL)
           ELSE
               MOVE    CONST-BYOMEI    TO  MLURES-SOLO-MSTID
               PERFORM 900-BYOMEI-LAST-UPDATE2-SEL-SEC
               PERFORM UNTIL ( FLG-BYOMEI  NOT =   ZERO )
      *
                   COMPUTE IDXL = IDXL + 1
      *
                   MOVE    BYO-TERMID      TO  WRK-HISTORY-G
                   MOVE    WRK-HLASTYMD    TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE        TO  MLURES-HLASTYMD (IDXL)
                   IF    ( IDXL        =   1 )
                       MOVE    WRK-DATE    TO  MLURES-SOLO-LASTYMD
                   END-IF
                   MOVE    WRK-HCOUNT      TO  WRK-COUNT-Z9
                   MOVE    FUNCTION TRIM(WRK-COUNT-Z9)
                                           TO  MLURES-HCOUNT (IDXL)
                   PERFORM 900-BYOMEI-LAST-UPDATE2-FET-SEC
               END-PERFORM
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "last_update2"      TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       20031-BYOMEI-EXT.
           EXIT.
      *****************************************************************
      *    病名マスタ最終更新日編集処理
      *****************************************************************
       200311-BYOMEI-SEC               SECTION.
      *
           MOVE    BYO-TERMID(1:8) TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    BYO-TERMID(9:6) TO  WRK-HMS
           PERFORM 800-TIME-SEC
      *
           .
       200311-BYOMEI-EXT.
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
           IF    ( WRK-SYMD            =   SPACE )
               MOVE    "00000000"      TO  WRK-SYMD
           END-IF
      *
           INITIALIZE                      WRK-DATE
           MOVE    WRK-SYY             TO  WRK-DATE-YY
           MOVE    WRK-SMM             TO  WRK-DATE-MM
           MOVE    WRK-SDD             TO  WRK-DATE-DD
           MOVE    "-"                 TO  WRK-DATE-FL1
           MOVE    "-"                 TO  WRK-DATE-FL2
      *
           EVALUATE    WRK-SYMD
           WHEN    "00000000"
               MOVE    01              TO  WRK-DATE-MM
               MOVE    01              TO  WRK-DATE-DD
           WHEN    "99999999"
               MOVE    12              TO  WRK-DATE-MM
               MOVE    31              TO  WRK-DATE-DD
           END-EVALUATE
      *
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
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
           IF    ( WRK-HMS             =   SPACE )
               MOVE    "000000"        TO  WRK-HMS
           END-IF
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0000"
               MOVE    "正常終了"
                                       TO  SPA-ERRMSG
               MOVE    "000"           TO  SPA-ERRCD
           WHEN    "E01"
               MOVE    "該当するマスタが存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "E89"           TO  SPA-ERRCD
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-ERRMSG
               MOVE    "E97"           TO  SPA-ERRCD
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-ERRMSG
               MOVE    "E98"           TO  SPA-ERRCD
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-ERRMSG
               MOVE    "E99"           TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST06-BODY     NOT =   LOW-VALUE )
               DISPLAY "masterlastupdatev3 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_masterlastupdatev3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "masterlastupdatev3req"
                                       TO  APIREQ-RECORDNAME
           MOVE    "XMLREAD"           TO  MCP-FUNC
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE     XML-APIREQ     TO  XML-MLUREQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"           TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST06-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  MLURES-API-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  MLURES-API-INFORMATION-TIME
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  SPA-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD       TO  MLURES-API-RESULT
           MOVE    SPA-ERRMSG      TO  MLURES-API-RESULT-MSG
      *
           MOVE    "xml_masterlastupdatev3res"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    XML-MLURES      TO  XML-APIREQ
           MOVE    1               TO  APIREQ-MODE
           MOVE    "masterlastupdatev3res"   TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  APILST06-BODY
               MOVE    "application/xml"
                                       TO  APILST06-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-LAST-UPDATE1-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-LAST-UPDATE1-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-LAST-UPDATE2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-LAST-UPDATE2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-LAST-UPDATE2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-LAST-UPDATE1-FET-EXT.
           EXIT.
      *****************************************************************
      *    病名マスタ検索処理
      *****************************************************************
       900-BYOMEI-LAST-UPDATE1-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update1"      TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BYOMEI-LAST-UPDATE2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名マスタ検索処理
      *****************************************************************
       900-BYOMEI-LAST-UPDATE2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           .
      *
       900-BYOMEI-LAST-UPDATE2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名マスタ検索処理
      *****************************************************************
       900-BYOMEI-LAST-UPDATE2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BYOMEI
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "last_update2"      TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           .
      *
       900-BYOMEI-LAST-UPDATE2-FET-EXT.
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
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
               MOVE    404             TO  APILST06-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
