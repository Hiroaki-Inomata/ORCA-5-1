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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGMLOG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : メニュー
      *  コンポーネント名  : ログインユーザ管理(MLOG)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/11/26    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *
       01  SPA-MLOG.
           03  SPA-GMN-CUR         PIC 9(02).
      *
           03  SPA-GMN-AREA.
               05  SPA-GMN-IRYONO.
                   07  SPA-GMN-IRYO    PIC X(02).
                   07  SPA-GMN-IRYOMEI PIC X(98).
               05  SPA-GMN-SELNUM      PIC 9(03).
               05  SPA-GMN-MEMO        PIC X(200).
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-USER        PIC X(01).
               05  SPA-NAI-TBL.
                   07  SPA-NAI-OCC             OCCURS  100.
                       09  SPA-NAI-TERMID      PIC X(64).
                       09  SPA-NAI-PROCESS-C   PIC X(64).
                       09  SPA-NAI-PROCESS-N   PIC X(64).
                       09  SPA-NAI-WINDOW-C    PIC X(64).
                       09  SPA-NAI-WINDOW-N    PIC X(64).
                       09  SPA-NAI-USER-C      PIC X(64).
                       09  SPA-NAI-USER-N      PIC X(64).
                       09  SPA-NAI-EVENT-C     PIC X(64).
                       09  SPA-NAI-EVENT-N     PIC X(64).
                       09  SPA-NAI-ACCESST-C   PIC X(64).
                       09  SPA-NAI-ACCESST-N   PIC X(64).
                       09  SPA-NAI-PROCESST-C  PIC X(64).
                       09  SPA-NAI-PROCESST-N  PIC X(64).
                       09  SPA-NAI-TOTALT-C    PIC X(64).
                       09  SPA-NAI-TOTALT-N    PIC X(64).
                       09  SPA-NAI-HOST-C      PIC X(64).
                       09  SPA-NAI-HOST-N      PIC X(64).
                       09  SPA-NAI-WIDGET-C    PIC X(64).
                       09  SPA-NAI-WIDGET-N    PIC X(64).
                       09  SPA-NAI-COUNT-C     PIC X(64).
                       09  SPA-NAI-COUNT-N     PIC X(64).
                       09  SPA-NAI-LOGIN-C     PIC X(64).
                       09  SPA-NAI-LOGIN-N     PIC X(64).
                       09  SPA-NAI-HOSP-N      PIC X(02).
                   07  SPA-NAI-CNT             PIC 9(03).
               05  SPA-NAI-SYS-TBL.
                   07  SPA-NAI-SYSBASE         OCCURS  100.
                       09  SPA-NAI-BGRPNUM     PIC 9(02).
                       09  SPA-NAI-BHOSPNUM    PIC 9(02).
                       09  SPA-NAI-BGRP        PIC 9(02).
                       09  SPA-NAI-BKBN        PIC 9(02).
                       09  SPA-NAI-HOSPNUM     PIC X(02).
                       09  SPA-NAI-HOSPNM      PIC X(40).
                   07  SPA-NAI-SYS-CNT         PIC 9(03).
      *
           COPY    "ENUM-VALUE".
      *
           COPY    "CPLOCKCONST.INC".
      *
      *    コンスタント領域
       01  WRK-CONST-AREA.
           03  WRK-CONST-STOP1    PIC X(07)
               VALUE "ユーザ(".
           03  WRK-CONST-STOP2    PIC X(49)
           VALUE ")の指示により、このクライアントを強制終了します。".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSBASE         PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  SGYOUMU-CODE        PIC 9(03).
      *
           03  WRK-SYSUSER         PIC X(16).
           03  WRK-HOSPNUM         PIC 9(02).
           03  WRK-MCP-HOSPNUM     PIC 9(02).
      *
           03  WRK-NUM             PIC Z9.
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-SELNUM          PIC 9(03). 
           03  WRK-MSG1            PIC X(20).
           03  WRK-MSG2            PIC X(20).
           03  WRK-MSG3            PIC X(20).
           03  WRK-MSG4            PIC X(20).
           03  WRK-MSG5            PIC X(20).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザ
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    日付変換サブ
      *     COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA". 
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
      *
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-ERRCD
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "MLOG   "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *         MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           END-IF
      *
      *    スパ初期編集処理
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    シスベース編集処理
      *****************************************************************
       3101-SYSBASE-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSBASE
           MOVE    ZERO            TO  SPA-NAI-SYS-CNT
           MOVE    ZERO            TO  IDX
      *
           INITIALIZE                  SYSBASE-REC
           MOVE    SYSBASE-REC     TO  MCPDATA-REC
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM UNTIL   FLG-SYSBASE     =   1
                   MOVE    "tbl_sysbase"   TO  MCP-TABLE
                   MOVE    "all"           TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    MCPDATA-REC     TO  SYSBASE-REC
                       ADD     1               TO  IDX
                       MOVE    SYSBASE-GRPNUM  TO  SPA-NAI-BGRPNUM(IDX)
                       MOVE    SYSBASE-HOSPNUM TO  SPA-NAI-BHOSPNUM(IDX)
                       MOVE    SYSBASE-HOSPNUM TO  SPA-NAI-HOSPNUM(IDX)
                       MOVE    SYSBASE-HONBUNGRP   TO  SPA-NAI-BGRP(IDX)
                       MOVE    SYSBASE-HONBUNKBN   TO  SPA-NAI-BKBN(IDX)
      *                医療機関名称編集
                       PERFORM 3101-HOSPNAME-SEC
                   ELSE
                       MOVE    1           TO  FLG-SYSBASE
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-NAI-SYS-CNT
      *
          IF      SPA-NAI-SYS-CNT      >   1
               COMPUTE SPA-NAI-SYS-CNT = SPA-NAI-SYS-CNT + 1
               PERFORM VARYING IDY FROM SPA-NAI-SYS-CNT BY -1
                       UNTIL   IDX =   0
                   MOVE    SPA-NAI-BGRPNUM(IDX)
                                               TO  SPA-NAI-BGRPNUM(IDY)
                   MOVE    SPA-NAI-BHOSPNUM(IDX)
                                               TO  SPA-NAI-BHOSPNUM(IDY)
                   MOVE    SPA-NAI-BHOSPNUM(IDX)
                                               TO  SPA-NAI-HOSPNUM(IDY)
                   MOVE    SPA-NAI-BGRP(IDX)   TO  SPA-NAI-BGRP(IDY)
                   MOVE    SPA-NAI-BKBN(IDX)   TO  SPA-NAI-BKBN(IDY)
                   MOVE    SPA-NAI-HOSPNM(IDX) TO  SPA-NAI-HOSPNM(IDY)
                   COMPUTE IDX     =   IDX     -   1
               END-PERFORM
               MOVE    00                  TO  SPA-NAI-BHOSPNUM(1)
               MOVE    "00"                TO  SPA-NAI-HOSPNUM(1)
               MOVE    "全医療機関"        TO  SPA-NAI-HOSPNM(1)
           END-IF
      *
           .
       3101-SYSBASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-HOSPNAME-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1001-EDYUKYMD
           MOVE    SPA-NAI-BHOSPNUM(IDX)
                                   TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  SYSKANRI-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
               MOVE    SYS-1001-TANHOSPNAME
                                           TO  SPA-NAI-HOSPNM(IDX)
               IF      SYS-1001-TANHOSPNAME    =   SPACE
                   MOVE    SYS-1001-HOSPNAME
                                           TO  SPA-NAI-HOSPNM(IDX)
               END-IF
           ELSE
               MOVE    "1015"              TO  SPA-ERRCD
           END-IF
      *
           .
       3101-HOSPNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー管理権限編集処理
      *****************************************************************
       3101-USER-KANRI-SEC         SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-USER
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           STRING  MCP-USER            DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                               INTO    SYS-1010-TBL
           END-STRING
           MOVE    SPA-SYSYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1010-EDYUKYMD
           MOVE    WRK-MCP-HOSPNUM TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  SYSKANRI-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-USER-KANRI TO  SPA-NAI-USER
           ELSE
               MOVE    "1015"              TO  SPA-ERRCD
           END-IF
      *
           .
       3101-USER-KANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-AREA
           INITIALIZE              SPA-NAI-AREA
           MOVE    1               TO  SPA-GMN-CUR
      *
      *    シスベースコンボセット
           PERFORM 3101-SYSBASE-SEC
      *
           PERFORM 3101-LISTENTRY-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    TermID一覧取得処理
      *****************************************************************
       3101-LISTENTRY-SEC          SECTION.
      *
           PERFORM 420-CLEAR-SEC
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SLDUSER-AREA
           MOVE    1               TO  SLDUSER-KBN
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
           PERFORM VARYING  IDX    FROM    1   BY  1
                   UNTIL    IDX    >   SLDUSER-MAX
               PERFORM 3101-GETVALUE-SEC
           END-PERFORM
      *
           .
       3101-LISTENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    TermIDの内容取得処理
      *****************************************************************
       3101-GETVALUE-SEC           SECTION.
      *
           MOVE    SLDUSER-TERMID(IDX)
                                   TO  SPA-NAI-TERMID(IDX)
           MOVE    SLDUSER-IN-PROCESS(IDX)
                                   TO  SPA-NAI-PROCESS-N(IDX)
           MOVE    SLDUSER-WINDOW(IDX)
                                   TO  SPA-NAI-WINDOW-N(IDX)
           MOVE    SLDUSER-USER(IDX)
                                   TO  SPA-NAI-USER-N(IDX)
                                       WRK-SYSUSER
      *
           PERFORM 900-SYSUSER-READ-SEC
           MOVE    WRK-HOSPNUM     TO  SPA-NAI-HOSP-N(IDX)
           MOVE    SLDUSER-EVENT(IDX)
                                   TO  SPA-NAI-EVENT-N(IDX)
           MOVE    SLDUSER-ACCESS-TIME(IDX)
                                   TO  SPA-NAI-ACCESST-N(IDX)
           UNSTRING
                   SLDUSER-ACCESS-TIME(IDX)
                                           DELIMITED  BY  " "
                                  INTO     WRK-MSG1
                                           WRK-MSG2
                                           WRK-MSG3
                                           WRK-MSG4
                                           WRK-MSG5
           END-UNSTRING
           MOVE    WRK-MSG4            TO  SPA-NAI-PROCESST-N(IDX)
           MOVE    SLDUSER-TOTAL-PROC(IDX)
                                       TO  SPA-NAI-TOTALT-N(IDX)
           MOVE    SLDUSER-HOST(IDX)   TO  SPA-NAI-HOST-N(IDX)
           MOVE    SLDUSER-WIDGET(IDX) TO  SPA-NAI-WIDGET-N(IDX)
           UNSTRING
                   SLDUSER-CREATE-TIME(IDX)
                                           DELIMITED  BY  " "
                                  INTO     WRK-MSG1
                                           WRK-MSG2
                                           WRK-MSG3
                                           WRK-MSG4
                                           WRK-MSG5
           END-UNSTRING
           MOVE    WRK-MSG4        TO  SPA-NAI-LOGIN-N(IDX)
      *
           .
       3101-GETVALUE-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        配信
               WHEN    "CLICKED"   ALSO    "B05"
                   PERFORM 420-SENDMSG-SEC
      *        配信(全端末)
               WHEN    "CLICKED"   ALSO    "B06"
                   PERFORM 420-SENDALL-SEC
      *        強制終了
               WHEN    "CLICKED"   ALSO    "B09"
                   PERFORM 420-TERMSTOP-SEC
                   PERFORM 3101-LISTENTRY-SEC
      *        全端末終了
               WHEN    "CLICKED"   ALSO    "B10"
                   PERFORM 420-ALLSTOP-SEC
                   PERFORM 3101-LISTENTRY-SEC
      *        一覧更新
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 3101-LISTENTRY-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"  ALSO    "SELNUM01"
                   PERFORM 201-SELNUM-SEC
      *        メッセージ入力
               WHEN    "ACTIVATE"  ALSO    "MESSAGE"
                   PERFORM 202-MEMO-SEC
      *        医療機関コンボ選択
               WHEN    "ACTIVATE"  ALSO    "IRYO_NO"
                   PERFORM 203-IRYO-NO-SEC
      *        行選択
               WHEN    ANY         ALSO    "LIST"
                   PERFORM 201-SELECT-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELNUM-SEC              SECTION.
      *
           MOVE    MLOG-SEL1       TO  SPA-GMN-SELNUM
           IF      SPA-GMN-SELNUM  >   MLOG-LIST-COUNT
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
           .
       201-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-SELNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   100
               IF      MLOG-ITEM-SELECT(IDX)   =  "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           .
       201-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ入力処理
      *****************************************************************
       202-MEMO-SEC                SECTION.
      *
           MOVE    MLOG-MES1       TO  SPA-GMN-MEMO
      *
           IF      SPA-GMN-SELNUM  NOT =   ZERO
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
      *
           .
       201-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関コンボ選択処理
      *****************************************************************
       203-IRYO-NO-SEC             SECTION.
      *
           MOVE    MLOG-IRYONO     TO  SPA-GMN-IRYONO
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    SPACE           TO  SPA-GMN-MEMO
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       203-IRYO-NO-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "MLOG"          TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  SPA-WORK
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    "M00"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    SPACE           TO  SPA-GMN-MEMO
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    端末強制終了処理
      *****************************************************************
       420-TERMSTOP-SEC            SECTION.
      *
           IF      SPA-NAI-TERMID(SPA-GMN-SELNUM) =   MCP-TERM
               GO  TO  420-TERMSTOP-EXT
           END-IF
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    8               TO  SLDUSER-KBN
           MOVE    SPA-NAI-TERMID(SPA-GMN-SELNUM)
                                   TO  SLDUSER-ID
           STRING  WRK-CONST-STOP1     DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SPACE
                   WRK-CONST-STOP2     DELIMITED  BY  SIZE
                               INTO    SLDUSER-MESSAGE
           END-STRING
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       420-TERMSTOP-EXT.
           EXIT.
      *
      *****************************************************************
      *    全端末強制終了処理
      *****************************************************************
       420-ALLSTOP-SEC             SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    7               TO  SLDUSER-KBN
           STRING  WRK-CONST-STOP1     DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SPACE
                   WRK-CONST-STOP2     DELIMITED  BY  SIZE
                               INTO    SLDUSER-MESSAGE
           END-STRING
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       420-ALLSTOP-EXT.
           EXIT.
      *
      *****************************************************************
      *    メッセージ配信処理
      *****************************************************************
       420-SENDMSG-SEC             SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    6               TO  SLDUSER-KBN
           MOVE    SPA-NAI-TERMID(SPA-GMN-SELNUM)
                                   TO  SLDUSER-ID
           MOVE    MLOG-MES1       TO  SLDUSER-MESSAGE
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       420-SENDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    一斉メッセージ配信処理
      *****************************************************************
       420-SENDALL-SEC             SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    5               TO  SLDUSER-KBN
           MOVE    SPA-NAI-TERMID(SPA-GMN-SELNUM)
                                   TO  SLDUSER-ID
           MOVE    MLOG-MES1       TO  SLDUSER-MESSAGE
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       420-SENDALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC         SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE           TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)   TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)   TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)   TO  WRK-HDD
               MOVE    WRK-HENYMD      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
      **** MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "MLOG    "      TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    MLOG-IRYONO     TO  SPA-GMN-IRYONO
           INITIALIZE                  MLOG
      *
           MOVE    MCP-USER        TO  WRK-SYSUSER
           PERFORM 900-SYSUSER-READ-SEC
           MOVE    WRK-HOSPNUM     TO  WRK-MCP-HOSPNUM
      *
      *    ユーザー権限検索
           PERFORM 3101-USER-KANRI-SEC
      *
           MOVE    ZERO            TO  IDY
           IF    ( SPA-NAI-USER    =   "1" OR "2" OR "3" OR "4" )
      *        医療機関コンボ
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-NAI-SYS-CNT
                   MOVE    1               TO  FLG-OK
                   IF   (( SPA-NAI-USER    =   "1" OR "3" ) AND
                         ( WRK-MCP-HOSPNUM 
                                       NOT =   SPA-NAI-HOSPNUM(IDX)))
                        MOVE    ZERO             TO  FLG-OK
                   END-IF
                   IF      FLG-OK          =   1
                       COMPUTE IDY = IDY + 1
                       MOVE    SPA-NAI-HOSPNUM(IDX)    TO
                                           MLOG-IRYONO-ITEM(IDY)(1:2)
                       MOVE    SPA-NAI-HOSPNM(IDX)     TO
                                           MLOG-IRYONO-ITEM(IDY)(3:)
                   END-IF
               END-PERFORM
      *
               IF      SPA-GMN-IRYONO  =   SPACE
                   MOVE    MLOG-IRYONO-ITEM(1) TO  SPA-GMN-IRYONO
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-IRYONO  TO  MLOG-IRYONO
      *
           MOVE    IDY         TO  MLOG-IRYONO-COUNT
      *
      *    医療機関が１つしかない場合は非活性
           IF      MLOG-IRYONO-COUNT   >   1
               MOVE    WIDGET-NORMAL       TO  MLOG-IRYONO-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  MLOG-IRYONO-STATE
               IF      SPA-GMN-CUR  =   1
                   MOVE    2        TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    ユーザーリスト
           MOVE    ZERO            TO  IDY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   100 
               IF      SPA-NAI-TERMID(IDX) NOT =   SPACE
                   MOVE    1               TO  FLG-OK
                   IF    ( SPA-GMN-IRYO    NOT =   "00" )  AND
                         ( SPA-GMN-IRYO    NOT =   SPA-NAI-HOSP-N(IDX))
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
                   IF      FLG-OK          =   1
                       ADD     1           TO  IDY
                       MOVE    IDY         TO  WRK-NUM
                       MOVE    WRK-NUM     TO  MLOG-ITEM-TNO(IDY)
                       MOVE    SPA-NAI-WINDOW-N(IDX)
                                           TO  MLOG-ITEM-SCREEN(IDY)
                       IF      SPA-NAI-TERMID(IDX) =   MCP-TERM
                           MOVE    "○"    TO  MLOG-ITEM-MINE(IDY)
                       END-IF
                       MOVE    SPA-NAI-USER-N(IDX)
                                           TO  MLOG-ITEM-USER(IDY)
                       MOVE    SPA-NAI-HOSP-N(IDX)
                                           TO  MLOG-ITEM-HOSPNUM(IDY)
                       MOVE    SPA-NAI-PROCESST-N(IDX)
                                           TO  MLOG-ITEM-LAST(IDY)
                       MOVE    SPA-NAI-HOST-N(IDX)
                                           TO  MLOG-ITEM-HOST(IDY)
                       MOVE    SPA-NAI-WINDOW-N(IDX)
                                           TO  MLOG-ITEM-SCREEN(IDY)
                       MOVE    SPA-SAKIPG  TO  WRK-SAKIPG
                       MOVE    SPA-NAI-WINDOW-N(IDX)
                                           TO  SPA-SAKIPG
                       PERFORM 5001-GYOUMUSET-SEC
                       MOVE    WRK-SAKIPG  TO  SPA-SAKIPG
                       MOVE    SPA-NAI-LOGIN-N(IDX)
                                           TO  MLOG-ITEM-START(IDY)
                       MOVE    IDY         TO  MLOG-LIST-COUNT
                   END-IF
               END-IF
           END-PERFORM
      *
      *    選択番号、メモ
           IF      SPA-GMN-SELNUM  NOT =   ZERO
               MOVE    SPA-GMN-SELNUM  TO  MLOG-SEL1
               MOVE    "T"             TO  MLOG-ITEM-SELECT(MLOG-SEL1)
           END-IF
           MOVE    SPA-GMN-MEMO    TO  MLOG-MES1
      *
      *    閲覧のみの場合は非活性にする
           IF    ( SPA-NAI-USER        =   SPACE OR "0" OR "1" OR  "2" )
               MOVE    WIDGET-INSENSITIVE  TO  MLOG-B05-STATE
               MOVE    WIDGET-INSENSITIVE  TO  MLOG-B06-STATE
               MOVE    WIDGET-INSENSITIVE  TO  MLOG-B09-STATE
               MOVE    WIDGET-INSENSITIVE  TO  MLOG-B10-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  MLOG-B05-STATE
               MOVE    WIDGET-NORMAL       TO  MLOG-B06-STATE
               MOVE    WIDGET-NORMAL       TO  MLOG-B09-STATE
               MOVE    WIDGET-NORMAL       TO  MLOG-B10-STATE
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
       5001-GYOUMUSET-SEC          SECTION.
      *
           MOVE    ZERO            TO  SGYOUMU-CODE
      *    "X" は、一律チェックマスタとなってしまう
           IF    SPA-SAKIPG(1:1)       =   "X"
               MOVE    "?"             TO  SPA-SAKIPG(1:1)
           END-IF
      *
           CALL    "ORCSGYOUMUGET"     USING
                                       SPA-AREA
                                       SGYOUMU-CODE
           MOVE    WRK-SAKIPG          TO  SPA-SAKIPG
           SET     TBLLOCK-IDX         TO  1
           SEARCH  ORC-LOCKCONST-TX    VARYING  TBLLOCK-IDX
               AT  END
                   MOVE    "その他"        TO  MLOG-ITEM-GYOMU(IDY)
               WHEN    SGYOUMU-CODE    =
                                       ORC-LOCKCONST-GNUM(TBLLOCK-IDX)
                   MOVE   ORC-LOCKCONST-NAME(TBLLOCK-IDX)
                                           TO  MLOG-ITEM-GYOMU(IDY)
           END-SEARCH
      *
           .
       5001-GYOUMUSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR        =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "IRYO_NO"      TO  MCP-WIDGET
               WHEN    2
                   MOVE    "SELNUM01"       TO  MCP-WIDGET
               WHEN    3
                   MOVE    "MESSAGE"         TO  MCP-WIDGET
               WHEN    4 
                   MOVE    "B05"          TO  MCP-WIDGET
               WHEN    5 
                   MOVE    "B06"          TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"          TO  MCP-WIDGET
           END-EVALUATE
      *
          .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
           EVALUATE    MCP-WIDGET
               WHEN    "IRYO_NO"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "SELNUM01"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "MESSAGE"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "B05"
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    "B06"
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN   "B12"
                   MOVE     1          TO  SPA-GMN-CUR
           END-EVALUATE
      *
          .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の選択番号はありません"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "頁の入力が違います"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "未入力の項目があります"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "頁の入力が違います"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "該当の帳票はありません"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "該当の帳票はありません"
                                           TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "帳票管理ＤＢが削除できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "帳票ＤＢが削除できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  MERR
           MOVE    SPA-ERRCD       TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "MLOG"          TO  SPA-MOTOPG
           MOVE    "MERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ環境取得処理
      *****************************************************************
       900-SYSUSER-READ-SEC        SECTION.
      *
           INITIALIZE                  SYSUSER-REC
           MOVE    WRK-SYSUSER     TO  SYSUSER-USERID
           MOVE    SYSUSER-REC     TO  MCPDATA-REC
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSUSER-REC
                   MOVE    ZERO            TO  FLG-SYSUSER
               ELSE
                   MOVE    1               TO  FLG-SYSUSER
               END-IF
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
      *
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSUSER         =   ZERO
      *        医療機関識別番号
               MOVE    SYSUSER-HOSPNUM TO  WRK-HOSPNUM
           ELSE
               MOVE    ZERO            TO  WRK-HOSPNUM
           END-IF
      *
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルFETCH処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
