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
       PROGRAM-ID.             ORCBRSICHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 随時帳票
      *  コンポーネント名  : 労災レセ電コード対応一覧印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/03/28    NACL−多々納  新規作成
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
           COPY    "CPRECEERR.INC".
      *
      *    帳票
           COPY    "HCPOM2.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-INPUTSET            PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
      *
           03  FLG-OK                  PIC 9(01).
           03  FLG-KEY                 PIC 9(01).
           03  FLG-PRINT               PIC 9(01).
           03  FLG-SETCD               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(03).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-CDCNT               PIC 9(05).
           03  CNT-SETCNT              PIC 9(05).
           03  CNT-SYOCNT              PIC 9(05).
      *
           03  CNT-PAGE1               PIC 9(05).
           03  CNT-PAGE2               PIC 9(05).
           03  CNT-PAGE3               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX2                    PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
           03  SYS-TIME.
               05  SYS-THH              PIC 9(02).
               05  SYS-TMM              PIC 9(02).
               05  SYS-TSS              PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-SYORIKBN       PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-ICD-SRYCD           PIC X(09).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-YMD                 PIC X(08).
      *
           03  WRK-KEY-SRYCD           PIC X(09).
           03  WRK-ATO-SRYCD           PIC X(09).
      *
           03  WRK-KEY-SET.
               05  WRK-KEY-SETCD       PIC X(06).
               05  WRK-KEY-YUKOSTYMD   PIC X(08).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-INPUTCD-G.
               05  WRK-INPUTCD         PIC X(20)   OCCURS  5.
           03  WRK-HEN-INPUTCD         PIC X(50).
           03  WRK-HEN-INPUTCD-N       PIC X(50).
      *
      *    内容
       01  WRK-HEN-AREA.
      *    半角
           03  WRK-TBL-REC             PIC X(108)  OCCURS   300.
      *    全角
           03  WRK-TBL-REC2            PIC X(108)  OCCURS   300.
      *
           03  WRK-PAGE-MAX            PIC 9(03).
      *
      *
       01  MAX-LINE                    PIC 9(02)  VALUE   69.
      *
      *    入力ＣＤ保存
       01  TBL-INPUTCD-AREA.
           03  TBL-INPUTCD-SRYCD       PIC X(09).
           03  TBL-INPUTCD-TBL         OCCURS   5.
               05  TBL-INPUTCD         PIC X(20).
      *
      *    印刷内容
      *    見出し
       01  HEAD-01.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  H01-MIDASI              PIC X(68).
           03  FILLER                  PIC X(03)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-YY                  PIC 9(04).
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  H01-MM                  PIC Z9.
           03  FILLER                  PIC X(01)   VALUE   ".".
           03  H01-DD                  PIC Z9.
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(01)   VALUE   "(".
           03  H01-THH                 PIC 99.
           03  FILLER                  PIC X(01)   VALUE   ":".
           03  H01-TMM                 PIC 99.
           03  FILLER                  PIC X(01)   VALUE   ")".
           03  FILLER                  PIC X(04)   VALUE   "  P-".
           03  H01-PCNT                PIC ZZ9.
      *    入力ＣＤ一覧
       01  HEAD1-01.
           03  FILLER                  PIC X(20)   VALUE  "入力コード".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE  "労災コード".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(30)   VALUE  "名称".
       01  BODY1-01.
           03  B101-INPUTCD            PIC X(20).
           03  FILLER                  PIC X(01).
           03  B101-SRYCD              PIC X(09).
           03  FILLER                  PIC X(01).
           03  B101-MIDASI             PIC X(12).
           03  B101-NAME               PIC X(50).
       01  BODY1-02.
           03  B102-INPUTCD            PIC X(20).
           03  FILLER                  PIC X(10).
           03  FILLER                  PIC X(01).
           03  B102-MIDASI             PIC X(12).
           03  B102-NAME               PIC X(50).
       01  BODY1-01N.
           03  B101N-INPUTCD           PIC X(20).
      *
      *    商品名一覧
       01  HEAD2-01.
           03  FILLER              PIC X(40)   VALUE
               "商品名コード・名称".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(20)   VALUE
               "有効期間".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(10)   VALUE  "労災コード".
           03  FILLER              PIC X(11)   VALUE   SPACE.
           03  FILLER              PIC X(10)   VALUE  "名称".
       01  BODY2-01.
           03  B201-SRYCD              PIC X(09).
           03  FILLER                  PIC X(01).
           03  B201-NAME               PIC X(30).
           03  FILLER                  PIC X(01).
           03  B201-YUKOSTYMD          PIC X(08).
           03  B201-Y1                 PIC X(01).
           03  B201-YUKOEDYMD          PIC X(08).
           03  FILLER                  PIC X(01).
           03  B201-MAE-SRYCD          PIC X(09).
           03  B201-K1                 PIC X(01).
           03  B201-ATO-SRYCD          PIC X(09).
           03  B201-K2                 PIC X(01).
           03  FILLER                  PIC X(01).
           03  B201-MAE-NAME           PIC X(30).
      *
      *    セット一覧
       01  HEAD3-01.
           03  FILLER              PIC X(40)   VALUE
               "セットコード・名称".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(20)   VALUE
               "有効期間".
           03  FILLER              PIC X(01)   VALUE   SPACE.
           03  FILLER              PIC X(10)   VALUE  "入力ＣＤ".
           03  FILLER              PIC X(11)   VALUE   SPACE.
      *
       01  BODY3-01.
           03  B301-SETCD              PIC X(06).
           03  FILLER                  PIC X(01).
           03  B301-SETNAME            PIC X(30).
           03  FILLER                  PIC X(04).
           03  B301-YUKOSTYMD          PIC X(08).
           03  B301-Y1                 PIC X(01).
           03  B301-YUKOEDYMD          PIC X(08).
           03  FILLER                  PIC X(01).
           03  B301-SET-INPSET         PIC X(48).
       01  BODY3-02.
           03  FILLER                  PIC X(20).
           03  B302-RSIKBN             PIC X(08).
           03  B302-MAE-SRYCD          PIC X(09).
           03  B302-K1                 PIC X(01).
           03  B302-ATO-SRYCD          PIC X(09).
           03  B302-K2                 PIC X(01).
           03  FILLER                  PIC X(03).
           03  B302-MAE-NAME           PIC X(50).
      *
       01  FOOT-01.
           03  FILLER                  PIC X(20).
           03  F01-MIDASI              PIC X(50).
      *
      *
       01  COMS-MID1.
           03  FILLER                  PIC X(06)   VALUE
               "＊＊　".
           03  FILLER                  PIC X(36)   VALUE
               "労災コード変更分の入力ＣＤ登録リスト".
           03  FILLER                  PIC X(06)   VALUE
               "　＊＊".
      *
       01  COMS-MID2.
           03  FILLER                  PIC X(06)   VALUE
               "＊＊　".
           03  FILLER                  PIC X(34)   VALUE
               "労災コード変更分のセット登録リスト".
           03  FILLER                  PIC X(06)   VALUE
               "　＊＊".
      *
       01  COMS-MID3.
           03  FILLER                  PIC X(06)   VALUE
               "＊＊　".
           03  FILLER                  PIC X(42)   VALUE
              "労災コード変更分の特定器材商品名登録リスト".
           03  FILLER                  PIC X(06)   VALUE
               "　＊＊".
      *
       01  COMS-MID4.
           03  FILLER                  PIC X(06)   VALUE
               "＊＊　".
           03  FILLER                  PIC X(36)   VALUE
               "労災コード変更分の入力ＣＤ削除リスト".
           03  FILLER                  PIC X(06)   VALUE
               "　＊＊".
      *
      *
      *    労災コード置換えテーブル 
           COPY   "CMRSICNGTBL.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
      *     COPY    "CPSK1009.INC".
      *    帳票編集区分情報
      *     COPY    "CPSK1030.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力コードマスタ−（保存）
       01  HZN-INPUTCD-REC.
           COPY    "CPINPUTCD.INC"     REPLACING
                                       //ICD-// BY //HZN-ICD-//.
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
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
      *    全角変換
           COPY    "CPORCSKANACHK.INC".
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
      *    ファイル削除パラメタ
      *    COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
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
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
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
           INITIALIZE                  RECEERR
           MOVE    ZERO            TO  CNT-AREA
      *
           MOVE    SPACE               TO  HCPOM2
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
                                       WRK-PARA-SYORIKBN
           END-UNSTRING
      *
      *    ＳＰＡ設定
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBRSICHG"        TO  JOB-PGID
           MOVE    "労災コード変更一覧表"
                                       TO  JOB-SHELLMSG
           IF      WRK-PARA-SYORIKBN   =   "9"
               MOVE    "労災入力ＣＤ削除処理"
                                       TO  JOB-SHELLMSG
           END-IF
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    3               TO  JOB-STOPFLG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
       *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
     *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
      *
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
      *    発行日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
      *
           MOVE    WRK-SYSYY           TO  H01-YY
           MOVE    WRK-SYSMM           TO  H01-MM
           MOVE    WRK-SYSDD           TO  H01-DD
      *
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SYS-THH             TO  H01-THH
           MOVE    SYS-TMM             TO  H01-TMM
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           IF      WRK-PARA-SYORIKBN   =   "9"
      *        入力ＣＤリスト印刷処理
               PERFORM 2001-INPUTCD-LIST-SEC
           ELSE
      *        入力ＣＤリスト印刷処理
               PERFORM 2001-INPUTCD-LIST-SEC
      *        セット一覧印刷
               IF      FLG-END             =   ZERO
                   PERFORM 2002-SET-LIST-SEC
               END-IF
      *        商品名一覧印刷
               IF      FLG-END             =   ZERO
                   PERFORM 2003-SYOHIN-LIST-SEC
               END-IF
           END-IF
      *
             .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録リスト印刷処理
      *****************************************************************
       2001-INPUTCD-LIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *    入力ＣＤ一覧初期処理
           PERFORM 20001-INPUTCD-PRT-SEC
      *
      *    診療コード
           MOVE    "101%"              TO  WRK-ICD-SRYCD
           PERFORM 20011-INPUTCD-HEN-SEC
           IF      FLG-END             =   1
               GO      TO      2001-INPUTCD-LIST-EXT
           END-IF
      *    器材コード
           MOVE    "788888%"           TO  WRK-ICD-SRYCD
           PERFORM 20011-INPUTCD-HEN-SEC
           IF      FLG-END             =   1
               GO      TO      2001-INPUTCD-LIST-EXT
           END-IF
      *    対象なし
           IF      CNT-CDCNT           =   ZERO
                MOVE    SPACE              TO  FOOT-01
                MOVE    "入力ＣＤの設定はありません"
                                           TO  F01-MIDASI
               ADD     1                   TO  CNT-LINE
               MOVE    FOOT-01             TO  HCPOM2-MOJI (CNT-LINE)
           END-IF
      *    帳票印刷
           PERFORM 400-PRINT-OUT-SEC
           .
       2001-INPUTCD-LIST-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ検索編集処理
      *****************************************************************
       20011-INPUTCD-HEN-SEC                 SECTION.
      *
      *    入力ＣＤ検索
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-ICD-SRYCD       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    ZERO                TO  IDX
           INITIALIZE                  TBL-INPUTCD-AREA
           PERFORM
               UNTIL    (FLG-INPUTCD       =   1   )
                    OR  (FLG-END           =   1   )
      *        対象チェック
               MOVE    ICD-SRYCD           TO  WRK-SRYCD
               PERFORM 290-SRYCD-CHK-SEC
               IF      FLG-OK              =   1
      *            振替対象
                   IF     (ICD-DSPSEQ          =   1  )
                      OR  (ICD-SRYCD       NOT =   TBL-INPUTCD-SRYCD)
                       IF      IDX             >   ZERO
                           PERFORM 200111-INPUTCD-TBLHEN-SEC
                       END-IF
                       MOVE    ZERO            TO  IDX
                       INITIALIZE              TBL-INPUTCD-AREA
                       MOVE    ICD-SRYCD       TO  TBL-INPUTCD-SRYCD
      *
                       ADD     1               TO  CNT-CDCNT
                   END-IF
                   ADD     1                   TO  IDX
                   MOVE    ICD-INPUTCD         TO  TBL-INPUTCD (IDX)
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTCD-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-END             =   1
               GO      TO      20011-INPUTCD-HEN-EXT
           END-IF
           IF      IDX                 >   ZERO
               PERFORM 200111-INPUTCD-TBLHEN-SEC
           END-IF
           .
       20011-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災コード対象判定
      *****************************************************************
       290-SRYCD-CHK-SEC           SECTION.
      *
      *    労災のコードの判定
           SET     TBL-RCN             TO  1
           SEARCH  TBL-RSICHG-OCC      VARYING   TBL-RCN
               AT  END
                   MOVE    ZERO            TO  FLG-OK
               WHEN   (WRK-SRYCD           =   TBL-RSICHG-ATO-SRYCD
                                                          (TBL-RCN))
               OR     (WRK-SRYCD           =   TBL-RSICHG-MAE-SRYCD
                                                          (TBL-RCN))
                   IF      TBL-RSICHG-ATO-SRYCD (TBL-RCN)
                                           =   TBL-RSICHG-MAE-SRYCD
                                                         (TBL-RCN)
                       MOVE    ZERO            TO  FLG-OK
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
           END-SEARCH
      *
           .
       290-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ印刷処理
      *****************************************************************
       200111-INPUTCD-TBLHEN-SEC           SECTION.
      *
           INITIALIZE                  WRK-HEN-AREA
      *    １行目
           MOVE    SPACE               TO  BODY1-01
           MOVE    SPACE               TO  BODY1-01N
           MOVE    TBL-INPUTCD-SRYCD   TO  B101-SRYCD
           MOVE    "（６月まで）"      TO  B101-MIDASI
           MOVE    TBL-INPUTCD-SRYCD   TO  WRK-SRYCD
           MOVE    "20130601"          TO  WRK-YMD
           PERFORM 910-TENSU-READKEY-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  B101-NAME
           END-IF
           MOVE    TBL-INPUTCD (1)     TO  B101-INPUTCD
           MOVE    BODY1-01            TO  WRK-TBL-REC (1)
           MOVE    BODY1-01N           TO  WRK-TBL-REC2(1)
      *    ２行目
           MOVE    SPACE               TO  BODY1-02
           MOVE    "（７月から）"      TO  B102-MIDASI
           MOVE    TBL-INPUTCD-SRYCD   TO  WRK-SRYCD
           MOVE    "20130701"          TO  WRK-YMD
           PERFORM 910-TENSU-READKEY-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  B102-NAME
           END-IF
           MOVE    TBL-INPUTCD (2)     TO  B102-INPUTCD
           MOVE    BODY1-02            TO  WRK-TBL-REC (2)
      *    ３行目から
           MOVE    2                   TO  IDY
           PERFORM VARYING     IDX     FROM    3   BY  1
                   UNTIL      (IDX     >   5   )
                   OR         (TBL-INPUTCD (IDX)   =   SPACE )
               ADD     1                   TO  IDY
               MOVE    SPACE               TO  BODY1-02
               MOVE    TBL-INPUTCD (IDX)   TO  B102-INPUTCD
               MOVE    BODY1-02            TO  WRK-TBL-REC (IDY)
           END-PERFORM
      *    帳票編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   IDY )
                          OR  (FLG-END     =   1   )
               ADD     1               TO  CNT-LINE
               IF      CNT-LINE       >   MAX-LINE
      *            帳票印刷
                   PERFORM 400-PRINT-OUT-SEC
      *            入力ＣＤ一覧初期処理
                   PERFORM 20001-INPUTCD-PRT-SEC
               END-IF
               MOVE    WRK-TBL-REC (IDX)   TO  HCPOM2-MOJI (CNT-LINE)
               MOVE    WRK-TBL-REC2 (IDX)  TO  HCPOM2-MOJI2(CNT-LINE)
           END-PERFORM
           IF      CNT-LINE       <   MAX-LINE
               ADD     1                   TO  CNT-LINE
               MOVE    ALL "-"             TO  HCPOM2-MOJI (CNT-LINE)
           END-IF
           INITIALIZE                  WRK-HEN-AREA
      *
      *    入力ＣＤ削除
           IF      WRK-PARA-SYORIKBN   =   "9"
               PERFORM 2009-INPUTCD-DELETE-SEC
           END-IF
           .
       200111-INPUTCD-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ削除処理
      *****************************************************************
       2009-INPUTCD-DELETE-SEC           SECTION.
      *
           MOVE    INPUTCD-REC         TO  HZN-INPUTCD-REC
      *
      *    入力コード削除
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    TBL-INPUTCD-SRYCD   TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "del22"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "INPUTCD DEL ERR:"  MCP-RC
               MOVE    "入力コードの削除ができませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    HZN-INPUTCD-REC     TO  INPUTCD-REC
           .
       2009-INPUTCD-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ一覧初期処理
      *****************************************************************
       20001-INPUTCD-PRT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *    見出し編集
           ADD     1                   TO  CNT-PAGE1
           MOVE    CNT-PAGE1           TO  H01-PCNT
           MOVE    COMS-MID1           TO  H01-MIDASI
      *    入力ＣＤ削除
           IF      WRK-PARA-SYORIKBN   =   "9"
               MOVE    COMS-MID4           TO  H01-MIDASI
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    HEAD-01             TO  HCPOM2-MOJI (CNT-LINE)
           ADD     2                   TO  CNT-LINE
           MOVE    HEAD1-01            TO  HCPOM2-MOJI (CNT-LINE)
           ADD     1                   TO  CNT-LINE
           .
       20001-INPUTCD-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    商品名一覧印刷処理
      *****************************************************************
       2003-SYOHIN-LIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           MOVE    SPACE               TO  WRK-KEY-SRYCD
      *    商品名一覧初期処理
           PERFORM 20031-SYOHIN-PRT-SEC
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    "058%"              TO  TNS-SRYCD
           MOVE    "788888%"           TO  TNS-IDOKANREN
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           PERFORM UNTIL      (FLG-TENSU   =   1  )
                         OR   (FLG-END     =   1  )
      *        対象チェック
               MOVE    TNS-IDOKANREN       TO  WRK-SRYCD
               PERFORM 290-SRYCD-CHK-SEC
               IF     (FLG-OK              =   1 )
               OR     (TNS-YUKOEDYMD       >=  "20130630")
                   PERFORM 20032-SYOHIN-HEN-SEC
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    対象なし
           IF      CNT-SYOCNT          =   ZERO
               MOVE    SPACE              TO  FOOT-01
               MOVE    "商品名の設定はありません"
                                           TO  F01-MIDASI
               ADD     1                   TO  CNT-LINE
               MOVE    FOOT-01             TO  HCPOM2-MOJI (CNT-LINE)
           ELSE
      *        最後
               IF      CNT-LINE        <   MAX-LINE
                   ADD     1               TO  CNT-LINE
                   MOVE    ALL "-"         TO  HCPOM2-MOJI (CNT-LINE)
               END-IF
           END-IF
      *    帳票印刷
           PERFORM 400-PRINT-OUT-SEC
           .
       2003-SYOHIN-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    商品名一覧印刷処理
      *****************************************************************
       20032-SYOHIN-HEN-SEC           SECTION.
      *
           ADD     1                   TO  CNT-SYOCNT
      *
           IF      TNS-SRYCD           =   WRK-KEY-SRYCD
               MOVE    1               TO  FLG-KEY
           ELSE
               MOVE    ZERO            TO  FLG-KEY
               MOVE    TNS-SRYCD       TO  WRK-KEY-SRYCD
           END-IF
      *
           INITIALIZE                  WRK-HEN-AREA
           MOVE    SPACE               TO  BODY2-01
           IF      FLG-KEY             =   ZERO
               MOVE    TNS-SRYCD           TO  B201-SRYCD
               MOVE    TNS-NAME            TO  B201-NAME
           END-IF
           MOVE    TNS-YUKOSTYMD       TO  B201-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  B201-YUKOEDYMD
           MOVE    "-"                 TO  B201-Y1
           MOVE    TNS-IDOKANREN       TO  B201-MAE-SRYCD
      *
           MOVE    TNS-IDOKANREN       TO  WRK-SRYCD
           IF      TNS-YUKOSTYMD       <   "20130701"
                MOVE    "20130630"          TO  WRK-YMD
           ELSE
                MOVE    "20130701"          TO  WRK-YMD
           END-IF
           PERFORM 910-TENSU-READKEY-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  B201-MAE-NAME
           END-IF
           IF      TNS-YUKOSTYMD       <   "20130701"
      *        変換コード判定
               PERFORM 2902-SRYCD-CHK-SEC
               IF      WRK-ATO-SRYCD       NOT =   SPACE
                   MOVE    WRK-ATO-SRYCD       TO  B201-ATO-SRYCD
                   MOVE    "("                 TO  B201-K1
                   MOVE    ")"                 TO  B201-K2
              END-IF
           END-IF
      *
           MOVE    BODY2-01            TO  WRK-TBL-REC (1)
      *    帳票編集
           IF     (CNT-LINE       >   5  )
              AND (CNT-LINE       <   MAX-LINE  )
              AND (FLG-KEY         =   ZERO  )
               ADD     1                   TO  CNT-LINE
               MOVE    ALL "-"             TO  HCPOM2-MOJI (CNT-LINE)
           END-IF
      *
           ADD     1               TO  CNT-LINE
           IF      CNT-LINE       >   MAX-LINE
      *        帳票印刷
               PERFORM 400-PRINT-OUT-SEC
      *        商品名一覧初期処理
               PERFORM 20031-SYOHIN-PRT-SEC
           END-IF
           MOVE    WRK-TBL-REC (1)     TO  HCPOM2-MOJI (CNT-LINE)
           MOVE    WRK-TBL-REC2(1)     TO  HCPOM2-MOJI2(CNT-LINE)
      *
           .
       20032-SYOHIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    商品名一覧初期処理
      *****************************************************************
       20031-SYOHIN-PRT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *    見出し編集
           ADD     1                   TO  CNT-PAGE2
           MOVE    CNT-PAGE2           TO  H01-PCNT
           MOVE    COMS-MID3           TO  H01-MIDASI
           ADD     1                   TO  CNT-LINE
           MOVE    HEAD-01             TO  HCPOM2-MOJI (CNT-LINE)
           ADD     2                   TO  CNT-LINE
           MOVE    HEAD2-01            TO  HCPOM2-MOJI (CNT-LINE)
           ADD     1                   TO  CNT-LINE
           .
       20031-SYOHIN-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災コード対象判定
      *****************************************************************
       2902-SRYCD-CHK-SEC           SECTION.
      *
      *    労災のコードの判定
           SET     TBL-RCN             TO  1
           SEARCH  TBL-RSICHG-OCC      VARYING   TBL-RCN
               AT  END
                   MOVE    SPACE              TO  WRK-ATO-SRYCD
               WHEN   (WRK-SRYCD           =   TBL-RSICHG-MAE-SRYCD
                                                          (TBL-RCN))
                   MOVE    TBL-RSICHG-ATO-SRYCD (TBL-RCN)
                                               TO  WRK-ATO-SRYCD
           END-SEARCH
      *
           .
       290-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット一覧印刷処理
      *****************************************************************
       2002-SET-LIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *    セット一覧初期処理
           PERFORM 20021-SETCD-PRT-SEC
      *
      *    入力セット検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               INITIALIZE                  INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           MOVE    SPACE               TO  WRK-KEY-SET
           PERFORM UNTIL      (FLG-INPUTSET    =   1  )
                         OR   (FLG-END         =   1  )
      *        対象チェック
               IF     (ISET-SETCD          =   WRK-KEY-SETCD )
                 AND  (ISET-YUKOSTYMD      =   WRK-KEY-YUKOSTYMD)
                   CONTINUE
               ELSE
                   IF      (ISET-SETCD         =   WRK-KEY-SETCD)
                       MOVE    1                   TO  FLG-SETCD
                   ELSE
                       MOVE    ZERO                TO  FLG-SETCD
                   END-IF
                   MOVE    ISET-INPUTCD        TO  WRK-SRYCD
                   PERFORM 290-SRYCD-CHK-SEC
                   IF     (FLG-OK              =   1 )
                       MOVE    ISET-SETCD          TO  WRK-KEY-SETCD
                       MOVE    ISET-YUKOSTYMD      TO  WRK-KEY-YUKOSTYMD
                       PERFORM 20022-INPUTSET-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    対象なし
           IF      CNT-SETCNT          =   ZERO
                MOVE    SPACE              TO  FOOT-01
                MOVE    "対象のセットはありません"
                                           TO  F01-MIDASI
               ADD     1                   TO  CNT-LINE
               MOVE    FOOT-01             TO  HCPOM2-MOJI (CNT-LINE)
           ELSE
      *        最後
               IF      CNT-LINE        <   MAX-LINE
                   ADD     1               TO  CNT-LINE
                   MOVE    ALL "-"         TO  HCPOM2-MOJI (CNT-LINE)
               END-IF
           END-IF
      *    帳票印刷
           PERFORM 400-PRINT-OUT-SEC
           .
       2002-SET-LIST-EXT.
           EXIT.
      *****************************************************************
      *    セット一覧初期処理
      *****************************************************************
       20021-SETCD-PRT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *    見出し編集
           ADD     1                   TO  CNT-PAGE3
           MOVE    CNT-PAGE3           TO  H01-PCNT
           MOVE    COMS-MID2           TO  H01-MIDASI
           ADD     1                   TO  CNT-LINE
           MOVE    HEAD-01             TO  HCPOM2-MOJI (CNT-LINE)
           ADD     2                   TO  CNT-LINE
           MOVE    HEAD3-01            TO  HCPOM2-MOJI (CNT-LINE)
           ADD     1                   TO  CNT-LINE
           .
       20021-SETCD-PRT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    セット一覧処理
      *****************************************************************
       20022-INPUTSET-HEN-SEC          SECTION.
      *
           ADD     1                   TO  CNT-SETCNT
      *    入力セット検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-KEY-SETCD       TO  ISET-SETCD
           MOVE    WRK-KEY-YUKOSTYMD   TO  ISET-YUKOSTYMD
           MOVE    WRK-KEY-YUKOSTYMD   TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               INITIALIZE                  INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                    TO  IDX
           INITIALIZE                      WRK-HEN-AREA
      *
           PERFORM UNTIL      (FLG-INPUTSET    =   1  )
                         OR   (FLG-END         =   1  )
               IF      ISET-SETSEQ         =   1
      *            セット名称編集
                   PERFORM 200321-SETNAME-HEN-SEC
               END-IF
      *        明細編集
               PERFORM 20022-INPUTSET-MEISAI-SEC
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-INPUTSET
      *    帳票編集
           IF     (CNT-LINE       >   5  )
              AND (CNT-LINE       <   MAX-LINE  )
               ADD     1                   TO  CNT-LINE
               IF      FLG-SETCD           =   ZERO
                   MOVE    ALL "-"         TO  HCPOM2-MOJI (CNT-LINE)
               END-IF
           END-IF
      *
      *    編集処理
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   IDX
               ADD     1               TO  CNT-LINE
               IF      CNT-LINE        >   MAX-LINE
      *            帳票印刷
                   PERFORM 400-PRINT-OUT-SEC
      *            セット一覧初期処理
                   PERFORM 20021-SETCD-PRT-SEC
               END-IF
               MOVE    WRK-TBL-REC (IDY)   TO  HCPOM2-MOJI  (CNT-LINE)
               MOVE    WRK-TBL-REC2(IDY)   TO  HCPOM2-MOJI2 (CNT-LINE)
           END-PERFORM
           .
       20022-INPUTSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット名称編集処理
      *****************************************************************
       200321-SETNAME-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  BODY3-01
           MOVE    ISET-SETCD          TO  B301-SETCD
           MOVE    SPACE               TO  WRK-INPUTCD-G
           MOVE    ZERO                TO  IDY
      *
      *    セット名称編集
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    ISET-SETCD          TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           PERFORM
               UNTIL     FLG-INPUTCD       =   1
               IF      ICD-INPUTCD         =   ISET-SETCD
      *            セット名称　編集
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    ICD-DSPNAME         TO  KANACHK-MAE-INPUT
                   MOVE    30                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  B301-SETNAME
               ELSE
                   ADD     1                   TO  IDY
                   IF      IDY                 <=  5
                       MOVE    ICD-INPUTCD     TO  WRK-INPUTCD (IDY)
                   END-IF
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-INPUTCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 200321-INPUTCD-HEN-SEC
           MOVE    ISET-YUKOSTYMD      TO  B301-YUKOSTYMD
           MOVE    ISET-YUKOEDYMD      TO  B301-YUKOEDYMD
           MOVE    "-"                 TO  B301-Y1
      *
           ADD     1                   TO  IDX
           MOVE    BODY3-01            TO  WRK-TBL-REC (IDX)
           .
       200321-SETNAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ編集処理
      *****************************************************************
       200321-INPUTCD-HEN-SEC          SECTION.
      *
      *
           MOVE    SPACE                TO  B301-SET-INPSET
           MOVE    SPACE                TO  WRK-HEN-INPUTCD
           MOVE    SPACE                TO  WRK-HEN-INPUTCD-N
           PERFORM VARYING     IDX2     FROM    1   BY  1
                   UNTIL       IDX2     >  IDY
               IF      IDX2                >   1
                   MOVE    WRK-HEN-INPUTCD     TO  WRK-HEN-INPUTCD-N
                   MOVE    SPACE               TO  WRK-HEN-INPUTCD
                   STRING  WRK-HEN-INPUTCD-N
                           "," 
                           WRK-INPUTCD (IDX2)   DELIMITED  BY  SPACE
                                                INTO   WRK-HEN-INPUTCD
                   END-STRING
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HEN-INPUTCD     TO  KANACHK-MAE-INPUT
                   MOVE    48                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-HEN-INPUTCD
               ELSE
                   STRING  "【"
                           WRK-INPUTCD (IDX2)   DELIMITED  BY  SPACE
                                                INTO   WRK-HEN-INPUTCD
                   END-STRING
               END-IF
           END-PERFORM
           IF     (WRK-HEN-INPUTCD     NOT =   SPACE )
              AND (WRK-HEN-INPUTCD(45:4)   =   SPACE )
               MOVE    WRK-HEN-INPUTCD     TO  WRK-HEN-INPUTCD-N
               MOVE    SPACE               TO  WRK-HEN-INPUTCD
               STRING  WRK-HEN-INPUTCD-N
                       "】"                DELIMITED   BY  SPACE
                                                INTO   WRK-HEN-INPUTCD
               END-STRING
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-HEN-INPUTCD     TO  KANACHK-MAE-INPUT
               MOVE    48                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-HEN-INPUTCD
           END-IF
           MOVE    WRK-HEN-INPUTCD     TO  B301-SET-INPSET
      *
           .
       200321-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット明細編集処理
      *****************************************************************
       20022-INPUTSET-MEISAI-SEC           SECTION.
      *
           MOVE    SPACE               TO  BODY3-02
           MOVE    ISET-INPUTCD        TO  B302-MAE-SRYCD
           MOVE    ISET-INPUTCD        TO  WRK-SRYCD
           IF      ISET-INPUTCD(1:3)   =   "101"
                                       OR  "788"
               PERFORM 290-SRYCD-CHK-SEC
           ELSE
               MOVE    ZERO                TO  FLG-OK
           END-IF
      *
           IF     (FLG-OK              =   1 )
      *        労災振替分
               IF      ISET-YUKOSTYMD      <   "20130701"
                   MOVE    "20130630"          TO  WRK-YMD
               ELSE
                   MOVE    "20130701"          TO  WRK-YMD
               END-IF
               PERFORM 910-TENSU-READKEY-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  B302-MAE-NAME
               END-IF
               MOVE    "（労）"            TO  B302-RSIKBN
               IF      ISET-YUKOSTYMD      <   "20130701"
      *            変換コード判定
                   PERFORM 2902-SRYCD-CHK-SEC
                   IF      WRK-ATO-SRYCD       NOT =   SPACE
                       MOVE    WRK-ATO-SRYCD       TO  B302-ATO-SRYCD
                       MOVE    "("                 TO  B302-K1
                       MOVE    ")"                 TO  B302-K2
                  END-IF
               END-IF
           ELSE
               EVALUATE    ISET-INPUTCD(1:1)
               WHEN    "."
                   PERFORM 20022-SRYSYUKBN-HEN-SEC
               WHEN    "S"
                   PERFORM 20021-YAKUSOKU-HEN-SEC
               WHEN    OTHER
                   IF      ISET-YUKOSTYMD      <   "20130701"
                       MOVE    "20130630"          TO  WRK-YMD
                   ELSE
                       MOVE    "20130701"          TO  WRK-YMD
                   END-IF
                   MOVE    ISET-INPUTCD        TO  WRK-SRYCD
                   PERFORM 910-TENSU-READKEY-SEC
                   MOVE    TNS-NAME            TO  B302-MAE-NAME
                   IF      ISET-COMMENT        NOT =   SPACE
                       MOVE    ISET-COMMENT        TO  B302-MAE-NAME
                   END-IF
                   IF      FLG-TENSU           =   1
                       MOVE    "【削】"        TO  B302-RSIKBN
                   END-IF
               END-EVALUATE
           END-IF
      *
           ADD     1                   TO  IDX
           MOVE    BODY3-02            TO  WRK-TBL-REC (IDX)
      *
          .
       20022-INPUTSET-MEISAI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別処理
      *****************************************************************
       20022-SRYSYUKBN-HEN-SEC          SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  B302-MAE-NAME
                   WHEN    ISET-INPUTCD(2:3)   =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  B302-MAE-NAME
           END-SEARCH 
           .
       20022-SRYSYUKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    約束セット処理
      *****************************************************************
       20021-YAKUSOKU-HEN-SEC          SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    ISET-INPUTCD        TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
               INITIALIZE                      INPUTCD-REC
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-INPUTCD         =   ZERO
      *        セット名称　編集
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    ICD-DSPNAME         TO  KANACHK-MAE-INPUT
               MOVE    50                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  B302-MAE-NAME
      *
           END-IF
           .
       20021-YAKUSOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
           ADD     1                   TO  CNT-PAGE
      *
           INITIALIZE                      ORCSPRTAREA
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
           MOVE    "労災コード変更分一覧表"
                                       TO  SPRT-TITLE
           IF      WRK-PARA-SYORIKBN   =   "9"
               MOVE    "労災コード変更分入力ＣＤ削除一覧表"
                                       TO  SPRT-TITLE
           END-IF
           MOVE    HCPOM2              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    SPACE               TO  HCPOM2
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-PAGE            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBRSICHG PAGE:" CNT-PAGE  " ***"
           DISPLAY "*** ORCBRSICHG END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
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
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ読み込み
      *****************************************************************
       900-INPUTCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
               MOVE    ZERO            TO  FLG-INPUTCD
           ELSE
               MOVE    1               TO  FLG-INPUTCD
               INITIALIZE                  INPUTCD-REC
           END-IF
      *
           .
       900-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    セット読み込み
      *****************************************************************
       900-INPUTSET-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTSET-REC
               MOVE    ZERO            TO  FLG-INPUTSET
           ELSE
               MOVE    1               TO  FLG-INPUTSET
               INITIALIZE                  INPUTSET-REC
           END-IF
      *
           .
       900-INPUTSET-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込み
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READKEY-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-YMD             TO  TNS-YUKOSTYMD
           MOVE    WRK-YMD             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
          .
       910-TENSU-READKEY-EXT.
           EXIT.
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
      *
           .
       910-DBSELECT-EXT.
           EXIT.
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
      *
           .
      *
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
      *
           .
       990-DBCLOSE-EXT.
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
