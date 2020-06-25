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
       PROGRAM-ID.                 ORCGXG01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 統計ＣＳＶ出力
      *  コンポーネント名  : ＣＳＶ出力選択画面（ＸＧ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/09/30    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    10/03/23  ＨＯＳＴ名取得方法の変更
      *
      *  04.07.01    NACL-藤原    14/03/24  スクリプト起動パスサーチ対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-森脇    16/11/29  チェックマスタCSV出力対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TOUKEICSVERR//.
      *
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ＣＳＶ出力指定処理共通パラメタ
           COPY    "XG01COMMON-SPA".
      *
           COPY    "XG01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *   
           COPY    "CMJOBNAME.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEICSVERR    PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TOUKEICSV-H     PIC 9(01).
      *
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TOUKEICSV-H-TBL-GROUP
                                   PIC X(14).
           03  WRK-TOUKEICSV-H-RENNUM
                                   PIC 9(04).
           03  WRK-TOUKEICSVERR    PIC X(200).
           03  WRK-XGIDMSG         PIC X(70).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SELNUM          PIC 9(03). 
      *
           03  WRK-ZZ              PIC ZZ9.
           03  WRK-KENSU           PIC ZZZZ9.
           03  WRK-MOTOPG          PIC X(08).
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
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-ROW.
           05  WRK-LIST-ROW        PIC S9(9)   BINARY.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PUTCSV".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
       01  TOUKEICSV-H-REC.
           COPY    "CPTOUKEICSV-H.INC".
      *
       01  TOUKEICSV-B-REC.
           COPY    "CPTOUKEICSV-B.INC".          
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
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
           COPY    "ORCA07SCRAREA.INC".
      *
      ******************************************************************
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *     
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XG01FREE
           MOVE    SPA-WORK        TO  SPA-XG01KYOUTU 
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-XG01KYOUTU  TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XG01FREE    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面からの戻り時
           IF      SPA-MOTOPG      =   "XGERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面処理
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集処理
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD   NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "XG01"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-MOTOPG
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "XG99" 
                   PERFORM 340-XG99-SET-SEC
                   PERFORM 310-SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "XGID1"
                   PERFORM 330-XGID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "XG02"
                   PERFORM 350-XG02-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               EVALUATE    WRK-MOTOPG
               WHEN    "XB01"
                   MOVE    SPA-WORK        TO   WRK-MOTOPG
                   INITIALIZE                   SPA-XG01FREE
                                                SPA-XG01KYOUTU
                   MOVE    WRK-MOTOPG      TO   SPA-XG01-MOTOPG
                   PERFORM 310-SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           INITIALIZE                  SPA-XG01FREE
                                       SPA-XG01KYOUTU
      *
           MOVE    WRK-MOTOPG      TO  SPA-XG01-MOTOPG
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA
                                           SPA-NAI-AREA
      *
           INITIALIZE                      TOUKEICSV-H-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-H-HOSPNUM
           EVALUATE    SPA-XG01-MOTOPG
               WHEN    "L01"
                   MOVE    "DAILY"         TO  TOUKEICSV-H-TBL-KEY
               WHEN    "G01"
                   MOVE    "MONTHLY"       TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-START---
               WHEN    "X91"
                   MOVE    "CHKMST"        TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-END-----
           END-EVALUATE 
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-TOUKEICSV-H-READ-N-SEC
           ELSE 
               MOVE    1                   TO  FLG-TOUKEICSV-H
           END-IF
      *
           MOVE    SPACE               TO  WRK-TOUKEICSV-H-TBL-GROUP
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-TOUKEICSV-H =   1
                               OR      IDX             =   100
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-GMN-TNO        (IDX)
               MOVE    TOUKEICSV-H-TBL-KEY TO  SPA-NAI-TTBL-KEY   (IDX)
               MOVE    TOUKEICSV-H-TBL-GROUP
                                           TO  SPA-NAI-TTBL-GROUP (IDX)
               IF      TOUKEICSV-H-TBL-GROUP
                                       NOT =   WRK-TOUKEICSV-H-TBL-GROUP
                   MOVE    TOUKEICSV-H-TBL-GROUP
                                               TO
                                               SPA-GMN-TTBL-GROUP (IDX)
                                               WRK-TOUKEICSV-H-TBL-GROUP
                   MOVE    TOUKEICSV-H-SRYYM   TO  SPA-NAI-TSRYYM (IDX)
                   IF      TOUKEICSV-H-SRYYM
                                           NOT =   SPACE   AND ZERO
                       MOVE    TOUKEICSV-H-SRYYM
                                               TO  WRK-SYMD (1:6)
                       MOVE    "01"            TO  WRK-SYMD (7:2)
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG(1:6)
                                               TO  SPA-GMN-TSRYYM (IDX)
                   END-IF
                   MOVE    TOUKEICSV-H-SRYYMD  TO  SPA-NAI-TSRYYMD(IDX)
                   IF      TOUKEICSV-H-SRYYMD
                                           NOT =   SPACE   AND ZERO
                       MOVE    TOUKEICSV-H-SRYYMD
                                               TO  WRK-SYMD 
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD(IDX)
                   END-IF
                   EVALUATE    TOUKEICSV-H-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN
                                                                  (IDX)
                       WHEN    "2"
                           MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN
                                                                  (IDX)
                   END-EVALUATE
                   SET     TBL-IDX             TO  1
                   SEARCH      MNAME-TBL-OCC      VARYING   TBL-IDX
                       AT   END
                           MOVE    SPACE       TO
                                           SPA-GMN-TSHORI-TITLE (IDX)
                       WHEN    TOUKEICSV-H-TBL-KEY =   MNAME-SHELLID
                                                       (TBL-IDX)
                           MOVE    MNAME-JOBNAME (TBL-IDX)
                                               TO
                                           SPA-GMN-TSHORI-TITLE (IDX)
                   END-SEARCH
               END-IF                                 
               MOVE    TOUKEICSV-H-RENNUM  TO  SPA-GMN-TRENNUM (IDX)
               MOVE    TOUKEICSV-H-SHORI-RENNUM
                                           TO  SPA-GMN-TSHORI-RENNUM
                                                                  (IDX)
               MOVE    TOUKEICSV-H-CNT     TO  SPA-GMN-TKENSU     (IDX)
               MOVE    TOUKEICSV-H-TITLE   TO  SPA-GMN-TPRT-TITLE (IDX)
               MOVE    TOUKEICSV-H-PUTFLG  TO  SPA-NAI-TPUTFLG    (IDX)
               IF      SPA-NAI-TPUTFLG (IDX)   =   SPACE
                   MOVE    "未"                TO  SPA-GMN-TPUTFLG (IDX)
               END-IF
               IF      TOUKEICSV-H-TO-FOLDER
                                           =   SPACE
                   MOVE    "/tmp/"         TO  SPA-NAI-TTO-FOLDER (IDX)
               ELSE
                   MOVE    TOUKEICSV-H-TO-FOLDER
                                           TO  SPA-NAI-TTO-FOLDER (IDX)
               END-IF
               MOVE    TOUKEICSV-H-TO-DATA
                                           TO  SPA-NAI-TTO-DATA   (IDX)
               MOVE    TOUKEICSV-H-CODE-TYPE
                                           TO  SPA-NAI-TCODE-TYPE (IDX)
      *
               MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-TOUKEICSV-H-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *                                 
           MOVE    IDX                 TO  SPA-GMN-MAX
      *                                 
           MOVE    1                   TO  FLG-LIST-ROW
      *                                 
           MOVE    1                   TO  SPA-GMN-CUR                                                 
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面処理
      *****************************************************************
       330-XGID1-SET-SEC            SECTION.
      *
           IF      SPA-XGID1-FLG    =   "OK"
               EVALUATE    SPA-XGIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1002"
                   WHEN    "1003"
                       PERFORM 430-DELETE-SEC
               END-EVALUATE        
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-XGID1-FLG
                                       SPA-XG99-FLG
           MOVE    SPACE           TO  SPA-XGIDCD
           .
       330-XGID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-XG99-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-XGID1-FLG
                                       SPA-XG99-FLG
           MOVE    SPACE           TO  SPA-XGIDCD
      *
           INITIALIZE                  SPA-XG99-AREA
           .
       340-XG99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ作成処理
      *****************************************************************
       350-XG02-SET-SEC            SECTION.
      *
           IF      SPA-XGID1-FLG   =   "OK"
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD      =   SPACE
                   PERFORM 3501-CSV-HENSYU-SEC
                   GO  TO  350-XG02-SET-EXT
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPA-XG01-MOTOPG TO  WRK-MOTOPG
           INITIALIZE                  SPA-XG01KYOUTU
           MOVE    WRK-MOTOPG      TO  SPA-XG01-MOTOPG  
           .
       350-XG02-SET-EXT.
      *
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       3501-CSV-HENSYU-SEC                   SECTION.
      *
           MOVE    ZERO            TO  SPA-XG99-FILESV-CHK-FLG
      *
      *    ジョブ開始セット
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           PERFORM 800-ORCSJOB-SEC
      *
      *    エラーファイル名セット
           MOVE    SPA-HOSPNUM         TO  TOUKEICSVERR-HOSPNUM
           MOVE    "OR007ERR"          TO  TOUKEICSVERR-FILE-ID
           MOVE    SPA-TERMID          TO  TOUKEICSVERR-TERMID
           MOVE    1                   TO  TOUKEICSVERR-SYOKBN1
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "orc_putcsv.sh"     TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           EVALUATE    SPA-XG01-MOTOPG
               WHEN    "L01"
                   MOVE    "DAILY"         TO  SHELLTBL-ARG2
               WHEN    "G01"
                   MOVE    "MONTHLY"       TO  SHELLTBL-ARG2
      *----(04.08.02)--ADD-START---
               WHEN    "X91"
                   MOVE    "CHKMST"        TO  SHELLTBL-ARG2
      *----(04.08.02)--ADD-END-----
           END-EVALUATE 
           MOVE    SPA-GMN-TBL-GROUP   TO  SHELLTBL-ARG3
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    SPA-GMN-RENNUM      TO  SHELLTBL-ARG5
           IF      SPA-GMN-XG02-FILE   =   SPACE
               MOVE    "9"                 TO  SHELLTBL-ARG6
           ELSE
               MOVE    SPA-GMN-XG02-FILE   TO  SHELLTBL-ARG6
           END-IF
           IF      SPA-GMN-XG02-FILE   =   SPACE
               STRING  SPA-NAI-XG02-TO-FOLDER
                                           DELIMITED   BY  SPACE
                       "/"                 DELIMITED   BY  SIZE 
                                           INTO  SHELLTBL-ARG7
               END-STRING
           END-IF
           MOVE    SPA-NAI-XG02-TO-DATA
                                       TO  SHELLTBL-ARG8
      *    ipアドレス取得
      *****PERFORM 900-SYSTEM-GET-SEC
      *****MOVE    SLDUSER-HOST(1) TO  SHELLTBL-ARG9
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG10
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG11
           MOVE    TOUKEICSVERR-BASENAME
                                   TO  SHELLTBL-ARG12
           MOVE    SPA-GMN-XG02-FILENAME
                                   TO  SHELLTBL-ARG13
           MOVE    SPA-GMN-XG02-CDKBN
                                   TO  SHELLTBL-ARG14
           MOVE    SPA-GMN-TPRT-TITLE(SPA-NAI-SELNUM)
                                   TO  SHELLTBL-ARG15
      *
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG16
           MOVE    SPA-OPID        TO  SHELLTBL-ARG17
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF    ( SPA-GMN-XG02-FILE   =   "1" )
               MOVE    "SV"            TO  SPA-XGID1-FLG
           ELSE
               MOVE    SPACE           TO  SPA-XGID1-FLG
           END-IF
           MOVE    SPACE           TO  SPA-XGIDCD
                                       SPA-XG99-FLG
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       3501-CSV-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE   "CHANGE"     TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"   ALSO    "B03"
                   PERFORM 430-DELETE-GID-SEC
      *        情報削除
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        ＣＳＶ出力
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 450-KAKUTEI-GID-SEC
      *        処理確認
               WHEN    "CLICKED"   ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
                   PERFORM 201-SELNUM-SEC
      *        行選択
               WHEN    ANY         ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELNUM-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-BODY
                                       SPA-NAI-BODY
      *      
           MOVE    XG01-SELNUM     TO  SPA-GMN-SELNUM
      *     
           IF      SPA-GMN-SELNUM  =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-SELNUM  >   SPA-GMN-MAX
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-SELNUM  TO  SPA-NAI-SELNUM     
                   PERFORM 2011-SELECT-HENSYU-SEC
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               MOVE    99              TO  SPA-GMN-CUR
           END-IF    
           .
       201-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択行編集処理
      *****************************************************************
       2011-SELECT-HENSYU-SEC      SECTION.
      *     
           MOVE    SPA-NAI-TTBL-GROUP    (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-TBL-GROUP
           MOVE    SPA-GMN-TRENNUM       (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-RENNUM
           MOVE    SPA-GMN-TSHORI-RENNUM (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-SHORI-RENNUM
           .
       2011-SELECT-HENSYU-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       203-SELECT-SEC              SECTION.
      *          
      *    行選択
           MOVE    ZERO            TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX FROM    1   BY  1
                   UNTIL       IDX >   SPA-GMN-MAX
               IF      XG01-SELECT (IDX)   =   "T"
                   IF      IDX                 =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                  SPA-GMN-BODY
      *
           MOVE    SPA-NAI-SELNUM      TO  SPA-GMN-SELNUM
      *                                 
           PERFORM 2011-SELECT-HENSYU-SEC
      *
           MOVE    99                  TO  SPA-GMN-CUR         
           .
       203-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る前チェック処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "6666"          TO  SPA-XGIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC          SECTION.
      *
           MOVE    SPA-XG01-MOTOPG TO  SPA-SAKIPG
           MOVE    "XG01"          TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                SECTION.
      *
           INITIALIZE               SPA-GMN-BODY
                                    SPA-NAI-BODY
      *
           MOVE    1                TO  SPA-GMN-CUR        
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除前チェック処理
      *****************************************************************
       430-DELETE-GID-SEC          SECTION.
      *         
      *    統計ＣＳＶ管理ＤＢのチェック
           INITIALIZE                      TOUKEICSV-H-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-H-HOSPNUM
           EVALUATE    SPA-XG01-MOTOPG
               WHEN    "L01"
                   MOVE    "DAILY"         TO  TOUKEICSV-H-TBL-KEY
               WHEN    "G01"
                   MOVE    "MONTHLY"       TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-START---
               WHEN    "X91"
                   MOVE    "CHKMST"        TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-END-----
           END-EVALUATE 
           MOVE    SPA-GMN-TBL-GROUP   TO  TOUKEICSV-H-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  TOUKEICSV-H-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  TOUKEICSV-H-SHORI-RENNUM
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TOUKEICSV-H-READ-N-SEC
           ELSE 
               MOVE    1                   TO  FLG-TOUKEICSV-H
           END-IF
      *     
           IF      FLG-TOUKEICSV-H     =   1
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
      *****    未処理のデータのとき
      *****    IF      TOUKEICSV-H-PUTFLG  =   SPACE
      *****        MOVE    "1003"              TO  SPA-XGIDCD
      *****    ELSE
                   MOVE    "1002"              TO  SPA-XGIDCD
      *****    END-IF
           END-IF
      *
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       430-DELETE-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC              SECTION.
      *         
           INITIALIZE                  TOUKEICSV-H-REC
           MOVE    SPA-HOSPNUM     TO  TOUKEICSV-H-HOSPNUM
           EVALUATE    SPA-XG01-MOTOPG
               WHEN    "L01"
                   MOVE    "DAILY"         TO  TOUKEICSV-H-TBL-KEY
               WHEN    "G01"
                   MOVE    "MONTHLY"       TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-START---
               WHEN    "X91"
                   MOVE    "CHKMST"        TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-END-----
           END-EVALUATE 
           MOVE    SPA-GMN-TBL-GROUP   TO  TOUKEICSV-H-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  TOUKEICSV-H-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  TOUKEICSV-H-SHORI-RENNUM
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *         
           INITIALIZE                      TOUKEICSV-B-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-B-HOSPNUM
           EVALUATE    SPA-XG01-MOTOPG
               WHEN    "L01"
                   MOVE    "DAILY"         TO  TOUKEICSV-H-TBL-KEY
               WHEN    "G01"
                   MOVE    "MONTHLY"       TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-START---
               WHEN    "X91"
                   MOVE    "CHKMST"        TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-END-----
           END-EVALUATE 
           MOVE    SPA-GMN-TBL-GROUP   TO  TOUKEICSV-B-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  TOUKEICSV-B-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  TOUKEICSV-B-SHORI-RENNUM
           MOVE    TOUKEICSV-B-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC         SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "XG01"              TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-XG01-MOTOPG     TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       450-KAKUTEI-GID-SEC            SECTION.
      *
      *    入力チェック
           IF      SPA-NAI-SELNUM  =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  450-KAKUTEI-GID-EXT               
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE                 
               GO  TO  450-KAKUTEI-GID-EXT               
           END-IF
      *
      *    統計ＣＳＶ管理ＤＢのチェック
           INITIALIZE                      TOUKEICSV-H-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-H-HOSPNUM
           EVALUATE    SPA-XG01-MOTOPG
               WHEN    "L01"
                   MOVE    "DAILY"         TO  TOUKEICSV-H-TBL-KEY
               WHEN    "G01"
                   MOVE    "MONTHLY"       TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-START---
               WHEN    "X91"
                   MOVE    "CHKMST"        TO  TOUKEICSV-H-TBL-KEY
      *----(04.08.02)--ADD-END-----
           END-EVALUATE 
           MOVE    SPA-GMN-TBL-GROUP   TO  TOUKEICSV-H-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  TOUKEICSV-H-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  TOUKEICSV-H-SHORI-RENNUM
           MOVE    TOUKEICSV-H-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TOUKEICSV-H-READ-N-SEC
           ELSE 
               MOVE    1                   TO  FLG-TOUKEICSV-H
           END-IF
      *
           MOVE    "tbl_toukeicsv_h"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *     
           IF      FLG-TOUKEICSV-H     =   1
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  450-KAKUTEI-GID-EXT
           END-IF
      *
      *    出力指示画面遷移処理
           PERFORM 4502-CSV-SIJI-CALL-SEC
           .
       450-KAKUTEI-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE           TO  WRK-TOUKEICSVERR
                   STRING  "ＣＳＶ出力処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE
                                             INTO  WRK-TOUKEICSVERR
                   END-STRING
               END-IF
           END-IF
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力指示画面遷移処理
      *****************************************************************
       4502-CSV-SIJI-CALL-SEC      SECTION.
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-XG01KYOUTU  TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XG01FREE    TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGXG02"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XG01FREE
           MOVE    SPA-WORK        TO  SPA-XG01KYOUTU 
      *
           MOVE  "FILENAME"        TO  MCP-WIDGET
      *
           MOVE  "XG01"            TO  SPA-MOTOPG
           MOVE  "XG02"            TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  SPA-SAKIPG        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
           .
      *
       4502-CSV-SIJI-CALL-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      XG99
           MOVE    ZERO                TO  XG99-COUNT
      *
           MOVE    5                   TO  XG99-DURATION
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX
               MOVE    JOB-JOBID       TO  XG99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  XG99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  XG99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  XG99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  XG99-ERRMSG   (IDX)
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  XG99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPACE       TO  XG99-MSG
                       PERFORM 4901-JOB-ERRSET-SEC
                       IF      XG99-MSG     =   SPACE
                           STRING  JOB-YOBI        DELIMITED BY SPACE
                                   "【"            DELIMITED BY SIZE
                                   JOB-SHELLMSG    DELIMITED BY SPACE
                                   "】"            DELIMITED BY SIZE
                                                   INTO  XG99-MSG
                           END-STRING
                       END-IF
                   END-IF
               END-IF
               MOVE    IDX                     TO  XG99-COUNT
           END-IF
      *
           IF      XG99-MSG              =   SPACE
               IF      XG99-COUNT            >   ZERO
                   MOVE    "処理は正常に終了しました"
                                                TO  XG99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  XG99-MSG
               END-IF
               MOVE    "B01"                TO  MCP-WIDGET
           ELSE
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "XG01"              TO  SPA-MOTOPG
           MOVE    "XG99"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XG99"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       4901-JOB-ERRSET-SEC             SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】"
                                               TO  XG99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】"
                                               TO  XG99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】"
                                               TO  XG99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】"
                                               TO  XG99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】"
                                               TO  XG99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】"
                                               TO  XG99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】"
                                               TO  XG99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC         SECTION.
      *
           IF      WRK-SYMD        =   ALL "9"  OR   SPACE
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
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
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
           IF      SPA-ERRCD   NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-XGIDCD   NOT =   SPACE
               PERFORM 520-XGIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "XG01"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    XG01-ROW        TO  WRK-LIST-ROW
      *
           INITIALIZE                  XG01
      *
           EVALUATE    FLG-LIST-ROW
               WHEN    1
                   MOVE    ZERO            TO  XG01-ROW
                   MOVE    ZERO            TO  XG01-ROWATTR
               WHEN    OTHER
                   MOVE    WRK-LIST-ROW    TO  XG01-ROW
                   MOVE    ZERO            TO  XG01-ROWATTR
           END-EVALUATE
      *     
           MOVE    SPA-GMN-SELNUM      TO  XG01-SELNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  XG01-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  XG01-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM 
                                       TO  XG01-SHORI-RENNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-TNO       (IDX) TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  XG01-TNO       (IDX)
               MOVE    SPA-GMN-TTBL-GROUP(IDX) TO  XG01-TTBL-GROUP(IDX)
               MOVE    SPA-GMN-TRENNUM   (IDX) TO  XG01-TRENNUM   (IDX)
               MOVE    SPA-GMN-TSHORI-RENNUM(IDX)
                                               TO
                                               XG01-TSHORI-RENNUM (IDX)
               MOVE    SPA-GMN-TKENSU    (IDX) TO  WRK-KENSU
               MOVE    WRK-KENSU               TO  XG01-TKENSU    (IDX)
               MOVE    SPA-GMN-TSRYYM    (IDX) TO  XG01-TSRYYM    (IDX)
               MOVE    SPA-GMN-TSRYYMD   (IDX) TO  XG01-TSRYYMD   (IDX)
               MOVE    SPA-GMN-TNYUGAIKBN(IDX) TO  XG01-TNYUGAIKBN(IDX)
               MOVE    SPA-GMN-TSHORI-TITLE (IDX)  TO
                                               XG01-TSHORI-TITLE  (IDX)
               MOVE    SPA-GMN-TPRT-TITLE(IDX) (1:60)
                                               TO  XG01-TPRT-TITLE(IDX)
               MOVE    SPA-GMN-TPUTFLG   (IDX) TO  XG01-TPUTFLG   (IDX)
      *       
               IF      IDX             =   SPA-NAI-SELNUM
                   MOVE    "T"             TO  XG01-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  XG01-SELECT (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  XG01-COUNT
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
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"          TO  MCP-WIDGET
           END-EVALUATE
          .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の選択番号はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0005"
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
               WHEN    "7777"
                   MOVE    WRK-TOUKEICSVERR    TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-TOUKEICSVERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XGERR
           MOVE    SPA-ERRCD            TO  XGERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XGERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "XG01"                TO  SPA-MOTOPG
           MOVE    "XGERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "XGERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-XGIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-XGIDMSG
      *
           EVALUATE    SPA-XGIDCD
               WHEN    "1002"
                   MOVE    "該当のデータの削除処理を行います"
                                       TO  WRK-XGIDMSG
               WHEN    "1003"
                   MOVE
                   "処理中の可能性がありますが削除処理を行いますか"
                                       TO  WRK-XGIDMSG
               WHEN    "6666"
                   MOVE    "処理中です"
                                       TO  WRK-XGIDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  XGID1
           INITIALIZE                  XGID1
           MOVE    SPA-XGIDCD      TO  XGID1-ID1CODE
           MOVE    WRK-XGIDMSG     TO  XGID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "XG01"          TO  SPA-MOTOPG
           MOVE    "XGID1"         TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "XGID1"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END              
      *
           .
       520-XGIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ipアドレス取得
      *****************************************************************
       900-SYSTEM-GET-SEC          SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    2               TO  SLDUSER-KBN
           MOVE    MCP-TERM        TO  SLDUSER-ID
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       900-SYSTEM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *     統計ＣＳＶ管理ＤＢ読込
      *****************************************************************
       900-TOUKEICSV-H-READ-N-SEC  SECTION.
      *
               PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  TOUKEICSV-H-REC
               MOVE    ZERO            TO  FLG-TOUKEICSV-H
           ELSE
               MOVE    1               TO  FLG-TOUKEICSV-H
           END-IF
           .
       900-TOUKEICSV-H-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
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
      *    ＤＢＦＥＴＣＨ処理
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
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
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
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
