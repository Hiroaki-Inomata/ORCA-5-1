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
       PROGRAM-ID.         ORCGI4API03.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院会計最終診療年月返却
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/10/26    NACL−太田　新規作成
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
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-NACCT               PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
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
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPAPIV2REQ.INC".
           COPY    "CPHSPMMV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSPMMV2RES.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号情報
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院会計
       01  NACCT-REC.
           COPY    "CPNYUINACCT.INC".
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
           COPY    "ORCA31SCRAREA.INC".
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
           INITIALIZE                  XML-HSPMMRES
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
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( HSPMMREQ-PTID   NOT =   SPACE )
                   PERFORM 2001-PTNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( HSPMMREQ-SRYYM  NOT =   SPACE )
                   PERFORM 2001-SRYYM-CHK-SEC
               END-IF
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       2001-PTNUM-CHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    HSPMMREQ-PTID
                                   TO  SPTID-PTNUM
           CALL    "ORCSPTID"      USING
                                   ORCSPTIDAREA
                                   SPA-AREA
           IF    ( SPTID-RC        =   ZERO )
               MOVE    SPTID-PTNUM TO  SPA-PTNUM
               MOVE    SPTID-PTID  TO  SPA-PTID
           ELSE
               MOVE    "0001"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       2001-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    HSPMMREQ-SRYYM  TO  WRK-DATE
           PERFORM 800-SYMD-SEC
           MOVE    "01"            TO  WRK-SYMD(7:)
           PERFORM 800-HIZUKE-SEC
           IF    ( WRK-EDTYMD1     =   SPACE )
               MOVE    "0002"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD    TO  SPA-SRYYMD
           END-IF
      *
           .
       2001-SRYYM-CHK-EXT.
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
       2003-RESPONSE-EDIT-SEC              SECTION.
      *
           IF    ( HSPMMREQ-PTID   NOT =   SPACE )
            AND  ( HSPMMREQ-SRYYM  NOT =   SPACE )
               PERFORM 20031-RESPONSE-EDIT-SEC
           ELSE
               PERFORM 20032-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-EDIT-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
           IF    ( FLG-NACCT       =   ZERO )
      *
               MOVE    1           TO  IDXL
      *
               PERFORM 200311-RESPONSE-EDIT-SEC
           ELSE
               MOVE    "0104"      TO  SPA-ERRCD
           END-IF
      *
      *
           .
       20031-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20032-RESPONSE-EDIT-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY58-SEL-SEC
      *
           MOVE    ZERO                TO  IDXL
      *
           PERFORM UNTIL ( FLG-NACCT   NOT =   ZERO )
      *
               COMPUTE IDXL = IDXL + 1
      *
               PERFORM 200311-RESPONSE-EDIT-SEC
      *
               PERFORM 900-NYUINACCT-KEY58-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key58"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXL    =   ZERO )
               EVALUATE    TRUE
               WHEN  ( SPA-SRYYMD  NOT =   SPACE )
                   MOVE    "0103"      TO  SPA-ERRCD
               WHEN  ( SPA-PTNUM   NOT =   SPACE )
                   MOVE    "0102"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "0101"      TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       20032-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       200311-RESPONSE-EDIT-SEC        SECTION.
      *
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    PTNUM-PTNUM TO  HSPMMRES-PTNUM (IDXL)
      *
           MOVE    NACCT-SRYYM TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE    TO  HSPMMRES-SRYYM (IDXL)
      *
           .
       200311-RESPONSE-EDIT-EXT.
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
           END-IF
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
           WHEN    "0001"
               MOVE    "患者番号の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "診療年月の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "入院中の患者情報が存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "入院会計が存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "未作成の入院会計は存在しませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "入院会計が存在しません。"
                                       TO  SPA-ERRMSG
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
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( I4API03-BODY     NOT =   LOW-VALUE )
               DISPLAY "hspmmv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hspmmv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    I4API03-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "hspmmv2req"        TO  APIREQ-RECORDNAME
           MOVE    "XMLREAD"           TO  MCP-FUNC
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC               =   ZERO  OR  1 )
               MOVE     XML-APIREQ      TO  XML-HSPMMREQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  SPA-ERRCD
           END-IF
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
           MOVE    WRK-DATE        TO  HSPMMRES-API-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  HSPMMRES-API-INFORMATION-TIME
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  SPA-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD       TO  HSPMMRES-API-RESULT
           MOVE    SPA-ERRMSG      TO  HSPMMRES-API-RESULT-MSG
      *
           MOVE    "xml_hspmmv2res"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    XML-HSPMMRES    TO  XML-APIREQ
           MOVE    1               TO  APIREQ-MODE
           MOVE    "hspmmv2res"    TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  I4API03-BODY
               MOVE    "application/xml"
                                       TO  I4API03-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           INITIALIZE                      NACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NACCT-REC
           ELSE
               MOVE    1               TO  FLG-NACCT
               INITIALIZE                  NACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY58-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
      *
           INITIALIZE                      NACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           IF    ( SPA-PTID            NOT =   ZERO )
               MOVE    SPA-PTID        TO  NACCT-PTID
               MOVE    "99999999"      TO  NACCT-UPYMD
           END-IF
           MOVE    NACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key58"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-NYUINACCT-KEY58-FET-SEC
           ELSE
               MOVE    1               TO  FLG-NACCT
               INITIALIZE                  NACCT-REC
           END-IF
      *
           .
      *
       900-NYUINACCT-KEY58-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY58-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NACCT
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-NACCT   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key58"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  NACCT-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( SPA-SRYYMD      NOT =   SPACE )
                    AND  ( SPA-SRYYMD (1:6)   <=   NACCT-SRYYM )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-NACCT
                   INITIALIZE                  NACCT-REC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-NYUINACCT-KEY58-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    NACCT-PTID          TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    NACCT-PTID          TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
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
               MOVE    404             TO  I4API03-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
