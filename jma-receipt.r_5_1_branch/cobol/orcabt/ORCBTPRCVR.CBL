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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBTPRCVR.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 改定処理
      *  コンポーネント名  : 点数付加情報引き継ぎ漏れ一括作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/04/05    NACL−伊藤  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    確認一覧（縦）
      **** COPY    "HCMSL80.INC".
      *    エラー一覧（横）
      **** COPY    "HCMSL55.INC".
      *    帳票
           COPY    "HCPOM2.INC".
      *
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE1               PIC 9(05).
           03  CNT-OK                  PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-P                   PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH             PIC 9(02).
               05  SYS-TMM             PIC 9(02).
               05  SYS-TSS             PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-SRYYMD         PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYSYMDG             PIC X(22).
           03  WRK-MAE-SRYYMD          PIC X(08).
           03  WRK-STR-SRYYMD          PIC X(08).
           03  WRK-HEN-MAX             PIC 9(04).
      *
      *    リスト領域
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1N.
               05  FILLER              PIC X(05)   VALUE   SPACE.
               05  FILLER              PIC X(24)   VALUE   
                   "＃　点数付加引継ぎ作成　".
               05  PRT-TAITOL          PIC X(20).
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE
                   "開始日：".
               05  PRT-SRYYMD          PIC X(22).
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(82)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(02)   VALUE   "P.".
               05  PRT-PAGE            PIC ZZ9.
           03  LST-PRT-HEAD3.
               05  FILLER              PIC X(12)   VALUE
                   "コード".
               05  FILLER              PIC X(12)   VALUE
                   "有効開始日".
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(46)   VALUE
                   "名称".
      *
           03  LST-PRT-TAIL1.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "更新件数：".
               05  PRT-UPDCNT          PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "　件".
      *
       01  LST-PRT-BODY.
           03  PRT-SRYCD               PIC X(09).
           03  FILLER                  PIC X(02).
           03  PRT-EDYMD               PIC X(09).
      *
       01  LST-PRT-BODYN.
           03  PRT-F1                  PIC X(26).
           03  PRT-NAME                PIC X(70).
      *
       01  LINE-MAX                    PIC 9(03)   VALUE   69.
      *
      *    内容
       01  WRK-PRTHEN-AREA.
      *    半角
           03  WRK-TBL-REC             PIC X(90)   OCCURS   800.
      *    全角
           03  WRK-TBL-REC2            PIC X(108)  OCCURS   800.
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
               05  FILLER          PIC X(01)   VALUE   "(".
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ")".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ付加コード
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    点数マスタ付加コードワーク
       01  HZN-TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC"    REPLACING
                                       //TNSP-// BY //HZN-TNSP-//.
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  MCPAREA
           MOVE    SPACE           TO  WRK-PRTHEN-AREA
           INITIALIZE                  LST-PRT-BODY
           INITIALIZE                  LST-PRT-BODYN
           MOVE    ZERO            TO  CNT-AREA
           INITIALIZE                  RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SRYYMD
           END-UNSTRING
      *
      *    ＳＰＡ設定
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE  SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBTPRCVR"        TO  JOB-PGID
           MOVE    "点数付加引き継ぎ"  TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期値セット
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理時間
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
      ***  MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *ＳＰＡ設定
      *    改正日
           IF      WRK-PARA-SRYYMD     =   SPACE
              MOVE    "20140401"           TO  WRK-PARA-SRYYMD
           END-IF
           IF      WRK-PARA-SRYYMD     <   "20140401"
              MOVE    "20140401"           TO  WRK-PARA-SRYYMD
           END-IF
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           MOVE    WRK-PARA-SRYYMD     TO  WRK-STR-SRYYMD
      *
      *    診療日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-STR-SRYYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  PRT-SRYYMD
      *
      *    診療日の前日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-STR-SRYYMD      TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-MAE-SRYYMD
           ELSE
               MOVE    "開始日の前日がエラーです。処理できません。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO      110-SYOKI-SET-EXT
           END-IF
      *
           MOVE    ALL "*"             TO  SPA-TERMID
      *    オペレータＩＤ
           MOVE    LNK-PRTKANRI-OPID   TO  SPA-OPID
           INSPECT SPA-OPID            REPLACING  ALL " "  BY  "*"
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    処理概要
      *    改正日の前日が期限の点数付加を取得（終了日が前日）
      *    その診療コードから改正日に有効な点数付加の有無を確認
      *    無い場合
      *    その診療コードから改正日に有効な点数の有無を確認
      *    （開始日が改正日であること）
      *    点数マスタの有効期間で点数付加を作成
      *
           INITIALIZE  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
      *
           PERFORM UNTIL    (FLG-TENSUPLUS =   1)
      *        改正日の前日が終了日のレコード
               IF      TNSP-YUKOEDYMD      =   WRK-MAE-SRYYMD
                   MOVE    TENSUPLUS-REC       TO  HZN-TENSUPLUS-REC
                   PERFORM 2001-TENSUPLUS-CHECK-SEC
               END-IF
               MOVE    ZERO                TO  FLG-TENSUPLUS
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-TENSUPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 2903-END-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加マスタ検索処理
      *****************************************************************
       2001-TENSUPLUS-CHECK-SEC    SECTION.
      *
      *    改正日に有効なレコードの存在を確認
           MOVE    WRK-STR-SRYYMD      TO  TNSP-YUKOSTYMD
           MOVE    WRK-STR-SRYYMD      TO  TNSP-YUKOEDYMD
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSUPLUS       =   1
               PERFORM 20011-TENSU-CHECK-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 20012-TENSUPLUS-COPY-SEC
               END-IF
           END-IF
           .
       2001-TENSUPLUS-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       20011-TENSU-CHECK-SEC       SECTION.
      *
      *    改正日に有効なレコードの存在を確認
           INITIALIZE  TNS-TENSU-REC
           MOVE    HZN-TNSP-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    HZN-TNSP-SRYCD      TO  TNS-SRYCD
           MOVE    WRK-STR-SRYYMD      TO  TNS-YUKOSTYMD
           MOVE    WRK-STR-SRYYMD      TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               IF      TNS-YUKOSTYMD   NOT =   WRK-STR-SRYYMD
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           END-IF
           .
       20011-TENSU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加引き継ぎ作成処理
      *****************************************************************
       20012-TENSUPLUS-COPY-SEC    SECTION.
      *
           MOVE    HZN-TENSUPLUS-REC   TO  TENSUPLUS-REC
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
                                           TNSP-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           MOVE    SPACE               TO  WRK-RECEERR
           IF      MCP-RC              NOT =   ZERO
               DISPLAY "ORCBTPRCVR TENSUPLUS INS ERR:" MCP-RC
                           ",KEY:" TNSP-KEY
               MOVE    SPACE               TO  WRK-RECEERR
               STRING  "点数付加追加でエラーとなりました。"
                       "処理を終了します。"
                                   DELIMITED  BY  SIZE
                                   INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-RECEERR         =   SPACE
               ADD     1               TO  CNT-OK
               PERFORM 700-PTNUM-PRINT-SEC
           END-IF
           .
       20012-TENSUPLUS-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理件数編集印刷処理
      *****************************************************************
       2903-END-HENSYU-SEC         SECTION.
      *
           IF      IDX-P               =   ZERO
               PERFORM 7001-PRT-INIT-SEC
           END-IF
      *    確認件数
           ADD     3                   TO  IDX-P
           IF      IDX-P               >   LINE-MAX
      *        印刷処理
               PERFORM 400-HCPOM2-PRINT-SEC
               MOVE    1               TO  IDX-P
           END-IF
      *
           MOVE    CNT-OK              TO  PRT-UPDCNT
           MOVE    LST-PRT-TAIL1       TO   HCPOM2-MOJI (IDX-P)
      *    印刷処理
           PERFORM 400-HCPOM2-PRINT-SEC
           .
       2903-END-HENSYU-EXIT.
           EXIT.
      *
      *****************************************************************
      *    登録確認メッセージ処理
      *****************************************************************
       700-PTNUM-PRINT-SEC         SECTION.
      *
           IF      CNT-OK              =   1
               PERFORM 7001-PRT-INIT-SEC
           END-IF
      *
           INITIALIZE  WRK-PRTHEN-AREA
           MOVE    ZERO                TO  WRK-HEN-MAX
           INITIALIZE  LST-PRT-BODY
           INITIALIZE  LST-PRT-BODYN
      *
           MOVE    HZN-TNSP-SRYCD      TO  PRT-SRYCD
      *    開始日
           MOVE    HZN-TNSP-YUKOSTYMD  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-EDYMD
      *
           MOVE    TNS-NAME            TO  PRT-NAME
           ADD     1                   TO  WRK-HEN-MAX
           MOVE    LST-PRT-BODY        TO  WRK-TBL-REC  (WRK-HEN-MAX)
           MOVE    LST-PRT-BODYN       TO  WRK-TBL-REC2 (WRK-HEN-MAX)
      *    編集処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-HEN-MAX
               IF      IDX-P               >=  LINE-MAX
      *            印刷処理
                   PERFORM 400-HCPOM2-PRINT-SEC
      *            見出し編集
                   PERFORM 7001-PRT-INIT-SEC
               END-IF
               ADD     1                   TO  IDX-P
               MOVE    WRK-TBL-REC (IDX)   TO  HCPOM2-MOJI  (IDX-P)
               MOVE    WRK-TBL-REC2(IDX)   TO  HCPOM2-MOJI2 (IDX-P)
           END-PERFORM
      *
           INITIALIZE  WRK-PRTHEN-AREA
           MOVE    ZERO                TO  WRK-HEN-MAX
           .
       700-PTNUM-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    印刷初期処理
      *****************************************************************
       7001-PRT-INIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-P
           MOVE    SPACE               TO  HCPOM2
      *    見出し編集
           ADD     1                   TO  CNT-PAGE1
           MOVE    CNT-PAGE1           TO  PRT-PAGE
           MOVE    LST-PRT-HEAD1N      TO  HCPOM2-MOJI (1)
           MOVE    LST-PRT-HEAD1       TO  HCPOM2-MOJI2 (1)
           MOVE    LST-PRT-HEAD3       TO  HCPOM2-MOJI (3)
           MOVE    4                   TO  IDX-P
           .
       7001-PRT-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-HCPOM2-PRINT-SEC        SECTION.
      *
           INITIALIZE  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCPOM2.red"        TO  SPRT-PRTID
           MOVE    "改正点数付加引き継ぎ作成確認リスト"
                                       TO  SPRT-TITLE
           MOVE    HCPOM2              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-HCPOM2-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      ***  PERFORM 3001-PARA-DEL-SEC
      *
           DISPLAY "*** ORCBTPRCVR OK :"  CNT-OK   " ***"
           DISPLAY "*** ORCBTPRCVR END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-OK              TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               CALL    "ORCSJOB"           USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
                                       OR  ZERO
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       910-TENSUPLUS-READ-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               MOVE    ZERO                TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           .
       910-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　更新処理
      *****************************************************************
       990-DBCOMMIT-SEC                SECTION.
      *
      *    ＤＢ　クローズ処理
           PERFORM 900-DBDISCONNECT-SEC
      *    ＤＢ　オープン処理
           PERFORM 100-DBOPEN-SEC
           .
       990-DBCOMMIT-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
