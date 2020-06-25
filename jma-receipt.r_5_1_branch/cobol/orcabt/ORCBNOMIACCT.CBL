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
       PROGRAM-ID.                 ORCBNOMIACCT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 診療会計テーブルレイアウト変更
      *                      診療会計テーブルの連番４以上を受診履歴連番への変更
      *                      患者ＩＤ，診療科毎に１月分の処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/01/09    NACL−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-多々納  14/07/18  一時ファイル対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
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
           COPY    "HCMSL80.INC".
      *    エラー一覧（横）
           COPY    "HCMSL55.INC".
      *
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-UPD             PIC 9(01).
           03  FLG-ACCTUPD         PIC 9(01).
           03  FLG-ACCTCHK         PIC 9(01).
           03  FLG-END-CNT         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHKFLG          PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(05).
           03  CNT-OK                  PIC 9(05).
           03  CNT-ERR                 PIC 9(05).
           03  CNT-UPD                 PIC 9(05).
           03  CNT-OUT                 PIC 9(05).
           03  CNT-LINE                PIC 9(03).
           03  CNT-PAGE                PIC 9(05).
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
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(04).
           03  IDJ2                PIC 9(04).
           03  IDJ3                PIC 9(04).
           03  IDD                 PIC 9(02).
           03  IDD2                PIC 9(02).
      *
           03  IDE1                PIC 9(02).
           03  IDE3                PIC 9(02).
           03  IDH1                PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
      **** 03  RECEERR                 PIC X(100).
           03  WRK-PARA-SYOKBN         PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-KEY-G.
               05  WRK-KEY-PTID        PIC 9(11).
               05  WRK-KEY-NYUGAIKBN   PIC X(01).
               05  WRK-KEY-SRYYMD.
                   07  WRK-KEY-SRYYM       PIC X(06).
                   07  WRK-KEY-SRYDD       PIC X(02).
               05  WRK-KEY-SRYKA       PIC X(02).
      *
           03  WRK-KAISU           PIC 9(03).
           03  WRK-AMARI           PIC 9(03).
           03  WRK-ADD-KAISU           PIC 9(03).
           03  WRK-GOK-KAISU           PIC 9(03).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-PTNUM               PIC X(20).
      *
       01  TBL-SYORI-AREA.
           03  TBL-SYORI-OCC           OCCURS   500.
               05  TBL-ZAINUM          PIC 9(08).
               05  TBL-HKNCOMBI        PIC X(04).
               05  TBL-DAY-OCC         OCCURS   31.
                   07  TBL-CNT             PIC 9(02).
                   07  TBL-RENNUM          PIC 9(01)
                                           OCCURS   9.
      *
       01  TBL-LIST-AREA.
           03  TBL-LIST-OCC            OCCURS   31.
               05  TBL-HEN-DAY         PIC 9(01).
               05  TBL-ERR-DAY         PIC 9(01).
               05  TBL-ERR-DAY2        PIC 9(01).
      *
           03  PRT-ERR-AREA.
               05  PRT-ERR-LST             OCCURS   31.
                   07  PRT-ERR-DAY             PIC X(02).
                   07  PRT-ERR-H               PIC X(01).
           03  PRT-HEN-AREA.
               05  PRT-HEN-LST             OCCURS   31.
                   07  PRT-HEN-DAY             PIC X(02).
                   07  PRT-HEN-H               PIC X(01).
           03  PRT-ERR-AREA2.
               05  PRT-ERR-LST2             OCCURS   31.
                   07  PRT-ERR-DAY2            PIC X(02).
                   07  PRT-ERR-H2              PIC X(01).
      *
      *    リスト領域
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  LST-HAED            PIC X(44)   VALUE   
                   "＃＃受診履歴連番４以上更新変換リスト＃＃".
               05  FILLER              PIC X(06)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  PRT-TIME            PIC X(07)   VALUE   SPACE.
               05  PRT-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(20)   VALUE
                   "患者番号".
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(50)   VALUE
                   "氏名".
               05  FILLER              PIC X(06)   VALUE
                   "科".
               05  FILLER              PIC X(13)   VALUE
                   "診療月".
               05  FILLER              PIC X(40)   VALUE
                   "日".
           03  LST-PRT-BODY-G.
               05  LST-PRT-BODY        OCCURS   50.
                   07  PRT-PTNUM           PIC X(20).
                   07  PRT-NAME            PIC X(50).
                   07  FILLER              PIC X(01).
                   07  PRT-SRYKA           PIC X(02).
                   07  FILLER              PIC X(06).
                   07  PRT-SRYYM           PIC X(07).
                   07  FILLER              PIC X(06).
                   07  PRT-MSG             PIC X(10).
                   07  PRT-DAY-G           PIC X(48).
      *
           03  LST-ERR-TAIL1.
               05  FILLER              PIC X(20)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "更新件数：".
               05  ERR-CNT             PIC ZZ,ZZ9.
               05  FILLER              PIC X(04)   VALUE
                   "　件".
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK1009.INC".
      *
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC           OCCURS   500.
           COPY    "CPJYURRK.INC"       REPLACING
                                       //JYURRK-// BY //TBL-JYURRK-//.
      *
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"       REPLACING
                                       //JYURRK-// BY //HZN-JYURRK-//.
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
      *01  PRTKANRI-REC.
      *    COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
      *01  PRTDATA-REC.
      *    COPY    "CPPRTDATA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
      *    オンライン帳票名・出力先プリンタ名取得パラ
      **   COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ****************************************************************
      *
       PROCEDURE                   DIVISION    USING
           COMMAND-PARAM
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
           INTO                        LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-SYOKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBNOMIACCT"  TO  JOB-PGID
           MOVE    "同日受診４回以降回数変換"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
      *
      *    確認のみ（ＤＢ更新なし）
           IF      WRK-PARA-SYOKBN NOT =   "1"
               MOVE    "＃＃受診履歴連番４以上チェックリスト＃＃"
                                       TO  LST-HAED
           END-IF
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
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理時間
           MOVE    SMCNDATE-HMS        TO  SYS-TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
           MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *ＳＰＡ設定
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *        病床数
               MOVE    SYS-1001-BEDSU      TO  SPA-BEDSU
      *        病床数（一般）
               IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                      (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                   MOVE    ZERO                TO  SPA-BEDSUIPN
               ELSE
                   MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
               END-IF
      *        県番号
               MOVE    SYS-1001-PREFNUM    TO  SPA-PREFNUM
      *        有床区分
               MOVE    1                   TO  SPA-BEDKBN
           END-IF
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    連番＞３の受診履歴を検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key50"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  WRK-KEY-G
           MOVE    ZERO                TO  IDX-J
           INITIALIZE                  TBL-JYURRK-AREA
           PERFORM UNTIL    FLG-JYURRK     =   1
               IF    ((WRK-KEY-PTID        =   JYURRK-PTID )
                  AND (WRK-KEY-NYUGAIKBN   =   JYURRK-NYUGAIKBN)
                  AND (WRK-KEY-SRYKA       =   JYURRK-SRYKA )
                  AND (WRK-KEY-SRYYM       =   JYURRK-SRYYMD(1:6)) )
                 OR  ( IDX-J               =   ZERO         )
                   CONTINUE
               ELSE
                   MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
                   PERFORM 200-JYURRK-HENSYU-SEC
                   MOVE    HZN-JYURRK-REC      TO  JYURRK-REC
                   MOVE    ZERO                TO  IDX-J
                   INITIALIZE                  TBL-JYURRK-AREA
               END-IF
               ADD     1               TO  IDX-J
               IF      IDX-J           <=  500
                   MOVE    JYURRK-REC      TO  TBL-JYURRK-REC (IDX-J)
               END-IF
               MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
               MOVE    JYURRK-NYUGAIKBN    TO  WRK-KEY-NYUGAIKBN
               MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
               MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key50"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    対象なし
           IF      (IDX-J              =   ZERO )
               AND (WRK-KEY-PTID       =   ZERO )
               ADD     1                   TO  CNT-LINE
               MOVE    "同日４回の受診はありませんでした"
                                           TO  PRT-NAME (CNT-LINE)
           END-IF
      *
           IF      (IDX-J              >   ZERO )
               PERFORM 200-JYURRK-HENSYU-SEC
           END-IF
      *
      *    対象なし
           IF     (CNT-UPD             =   ZERO )
              AND (FLG-ACCTCHK         =   1    )
               ADD     1                   TO  CNT-LINE
               MOVE    "更新対象の受診履歴はありません"
                                           TO  PRT-NAME (CNT-LINE)
           END-IF
      *
      *    確認リスト処理
           IF      CNT-LINE            >   ZERO
               PERFORM 2902-HCML55-PRT02-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       200-JYURRK-HENSYU-SEC                SECTION.
      *TEST
            DISPLAY "WRK-KEY-PTID:" WRK-KEY-PTID
                 ",SRYKA:" WRK-KEY-SRYKA
                 ",SRYYM:" WRK-KEY-SRYYM
      *TEST
      *
           INITIALIZE                  TBL-SYORI-AREA
           INITIALIZE                  TBL-LIST-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-J
               IF      (IDX            =   1   )
                   OR  (TBL-JYURRK-SRYYMD (IDX)
                                   NOT =   TBL-JYURRK-SRYYMD(IDX - 1))
      *            連番＝３を検索する
                   PERFORM 2001-JYURRK-REN03-SEC
               END-IF
               MOVE    TBL-JYURRK-REC (IDX)    TO  JYURRK-REC
               PERFORM 2001-ZAINUM-HEN-SEC
           END-PERFORM
      *    診療会計テーブル更新
           MOVE    ZERO                TO  FLG-UPD
           PERFORM VARYING     IDJ2    FROM    1   BY  1
                   UNTIL      (IDJ2    >   500    )
                           OR (TBL-ZAINUM (IDJ2)   =   ZERO )
      *        診療会計テーブル検索
               PERFORM 2001-SRYACCT-HEN-SEC
           END-PERFORM
      *
           IF      FLG-UPD             =   1
      *        一覧
               PERFORM 2009-UPD-PRINT-SEC
           END-IF
      *
           .
       200-JYURRK-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       2001-JYURRK-REN03-SEC                SECTION.
      *
      *    連番３を検索
           MOVE    TBL-JYURRK-REC (IDX)    TO  JYURRK-REC
           MOVE    3                       TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM UNTIL    FLG-JYURRK     =   1
               PERFORM 2001-ZAINUM-HEN-SEC
      *
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2001-JYURRK-REN03-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       2001-ZAINUM-HEN-SEC                SECTION.
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDD2
      *
           PERFORM VARYING     IDJ     FROM    1   BY  1
                   UNTIL       IDJ     >   15
               IF      JYURRK-ZAINUM (IDJ) NOT =   ZERO
                   PERFORM VARYING     IDJ2    FROM    1   BY  1
                           UNTIL       IDJ2    >   500
                       IF      TBL-ZAINUM (IDJ2)
                                               =   ZERO
                           MOVE    JYURRK-ZAINUM (IDJ) 
                                               TO  TBL-ZAINUM (IDJ2)
                           MOVE    JYURRK-HKNCOMBINUM
                                               TO  TBL-HKNCOMBI(IDJ2)
                       END-IF
                       IF     (TBL-ZAINUM (IDJ2)
                                               =   JYURRK-ZAINUM (IDJ) )
                          AND (TBL-HKNCOMBI(IDJ2)
                                               =   JYURRK-HKNCOMBINUM )
                           ADD     1           TO  TBL-CNT (IDJ2 IDD2)
                           MOVE    TBL-CNT (IDJ2 IDD2)   TO  IDD
                           MOVE    JYURRK-RENNUM
                                               TO  TBL-RENNUM
                                                        (IDJ2 IDD2 IDD)
                           MOVE    500         TO  IDJ2
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       2001-ZAINUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       2001-SRYACCT-HEN-SEC                SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    WRK-KEY-PTID        TO  ACCT-PTID
           MOVE    WRK-KEY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    WRK-KEY-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-KEY-SRYKA       TO  ACCT-SRYKA
           MOVE    TBL-HKNCOMBI(IDJ2)  TO  ACCT-HKNCOMBI
           MOVE    TBL-ZAINUM (IDJ2)   TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACCT         =   1
      *        対象なし
               GO      TO      201-SRYACCT-HENSYU-EXT
           END-IF
      *TEST
           DISPLAY "ACCT-PTID:" ACCT-PTID
               ",ACCT-SRYYM:" ACCT-SRYYM
               ",ACCT-ZAINUM:" ACCT-ZAINUM
      *TEST
      *
           MOVE    ZERO                TO  FLG-ACCTUPD
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               MOVE    ZERO                TO  FLG-CHKFLG
               IF     (ACCT-DAY (1 IDD)    =   ZERO )
      *            登録なし
                   MOVE    1                   TO  FLG-CHKFLG
               END-IF
               IF     (ACCT-DAY (5 IDD)    >   ZERO )
                 OR   (ACCT-DAY (6 IDD)    >   ZERO )
                 OR   (ACCT-DAY (7 IDD)    >   ZERO )
                 OR   (ACCT-DAY (8 IDD)    >   ZERO )
                 OR   (ACCT-DAY (9 IDD)    >   ZERO )
                 OR   (ACCT-DAY (10 IDD)   >   ZERO )
      *            変換済み
                   MOVE    1                   TO  FLG-CHKFLG
                   MOVE    1                   TO  FLG-ACCTCHK
               END-IF
               IF      (FLG-CHKFLG         =   ZERO )
      *            連番＝３　の回数なし（変換済み以外は
                   IF     (ACCT-DAY (4 IDD)    =   ZERO )
                     OR   (TBL-CNT (IDJ2 IDD)  =   ZERO )
                       MOVE    1                   TO  FLG-CHKFLG
                   END-IF
               END-IF
               IF      (FLG-CHKFLG         =   ZERO )
                   IF     (TBL-CNT (IDJ2 IDD)      =   1  )
                     AND  (TBL-RENNUM (IDJ2 IDD 1) =   3  )
      *                連番＝３のみ変換なし
                       MOVE    1                   TO  FLG-CHKFLG
                   END-IF
               END-IF
      *
               IF      (FLG-CHKFLG         =   ZERO )
      *TEST
           DISPLAY "IDD:"  IDD
                  ",DAY4:"  ACCT-DAY (4 IDD)
                  ",TBL-CNT:"  TBL-CNT (IDJ2 IDD)
      *TEST
                   MOVE    1                   TO  FLG-ACCTUPD
      *
                   MOVE    1                   TO  TBL-HEN-DAY (IDD)
      *
                   MOVE    ZERO                TO  WRK-KAISU
                   MOVE    ZERO                TO  WRK-AMARI
                   MOVE    ACCT-DAY (4 IDD)    TO  WRK-GOK-KAISU
      *
                   IF     (TBL-CNT (IDJ2 IDD)  >   1               )
                     AND  (TBL-CNT (IDJ2 IDD)  <=  ACCT-DAY (4 IDD))
                       COMPUTE WRK-KAISU       =   ACCT-DAY (4 IDD)
                                               /   TBL-CNT (IDJ2 IDD)
                       IF      ACCT-DAY (4 IDD)    NOT =
                                             ( WRK-KAISU
                                              *   TBL-CNT (IDJ2 IDD))
                           COMPUTE WRK-AMARI    =   ACCT-DAY (4 IDD) 
                                                -   ( WRK-KAISU
                                                *   TBL-CNT (IDJ2 IDD))
      *                    案分あり
                           MOVE    1            TO  TBL-ERR-DAY (IDD)
      *TEST
           DISPLAY "IDD:"  IDD
                  ",TBL-ERR-DAY:"  TBL-ERR-DAY (IDD)
      *TEST
                       END-IF
                   ELSE
      *                合計回数が剤回数以下の時、最初の連番のみ編集
                       MOVE    ACCT-DAY (4 IDD)    TO  WRK-KAISU
                       IF      TBL-CNT (IDJ2 IDD)  >   1
                           MOVE    1               TO  TBL-ERR-DAY2(IDD)
      *TEST
           DISPLAY "ERR1 IDD:"  IDD
      *TEST
                       END-IF
                   END-IF
                   MOVE    ZERO                TO  ACCT-DAY (4 IDD)
                   MOVE    ZERO                TO  WRK-ADD-KAISU
                   MOVE    ZERO                TO  FLG-END-CNT
                   PERFORM VARYING     IDJ3    FROM    1   BY  1
                           UNTIL      (IDJ3    >   TBL-CNT (IDJ2 IDD))
                                   OR (FLG-END-CNT     =   1     )
                       MOVE    TBL-RENNUM (IDJ2 IDD IDJ3 )  TO  IDX
                       ADD     1                   TO  IDX
                       IF      IDJ3            =   1
                           COMPUTE ACCT-DAY (IDX IDD)  =  (WRK-KAISU
                                                       +   WRK-AMARI )
                       ELSE
                           MOVE    WRK-KAISU   TO  ACCT-DAY (IDX IDD)
                       END-IF
      *TEST
           DISPLAY "IDX:"  IDX
                  ",DAY:"  ACCT-DAY (IDX IDD)
      *TEST
                       ADD     ACCT-DAY (IDX IDD)  TO  WRK-ADD-KAISU
                       IF      WRK-ADD-KAISU   =   WRK-GOK-KAISU
                           MOVE    1               TO  FLG-END-CNT
                       END-IF
                   END-PERFORM
      *
                   IF      FLG-END-CNT     =   ZERO
      *              エラー
                       MOVE    1           TO  TBL-ERR-DAY2(IDD)
                       MOVE    1           TO  FLG-ERR
      *TEST
           DISPLAY "ERR2 IDD:"  IDD
      *TEST
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (FLG-ACCTUPD         =   1   )
               IF      WRK-PARA-SYOKBN     =   "1"
      *            ＤＢ更新
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
      *
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "SRYACCT UPD ERR:" MCP-RC
                   END-IF
                   ADD     1                   TO  CNT-UPD
                   MOVE    1                   TO  FLG-UPD
               ELSE
                   ADD     1                   TO  CNT-UPD
                   MOVE    1                   TO  FLG-UPD
               END-IF
           END-IF
      *
           .
       201-SRYACCT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    登録一覧処理
      *****************************************************************
       2009-UPD-PRINT-SEC          SECTION.
      *
           ADD     1                   TO  CNT-OUT
      *
           ADD     1                   TO  CNT-LINE
           IF      CNT-LINE             >   50
               PERFORM 2902-HCML55-PRT02-SEC
               MOVE    1               TO  CNT-LINE
           END-IF
           PERFORM 2001-PTID-CHKSYORI-SEC
           MOVE    WRK-PTNUM           TO  PRT-PTNUM (CNT-LINE)
           MOVE    PTINF-NAME          TO  PRT-NAME  (CNT-LINE)
           MOVE    WRK-KEY-SRYKA       TO  PRT-SRYKA (CNT-LINE)
           MOVE    WRK-KEY-SRYYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  PRT-SRYYM  (CNT-LINE)
      *
           INITIALIZE                  PRT-ERR-AREA
           INITIALIZE                  PRT-ERR-AREA2
           INITIALIZE                  PRT-HEN-AREA
           MOVE    ZERO                TO  IDE1
           MOVE    ZERO                TO  IDE3
           MOVE    ZERO                TO  IDH1
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD     >   31   )
               IF      TBL-ERR-DAY (IDD)   =   1
                   IF      IDE1            >   1
                       MOVE    ","         TO  PRT-ERR-H (IDE1)
                   END-IF
                   ADD     1               TO  IDE1
                   MOVE    IDD             TO  PRT-ERR-DAY (IDE1)
               ELSE
                   IF      TBL-HEN-DAY (IDD)   =   1
                       IF      IDH1            >   1
                           MOVE    ","         TO  PRT-HEN-H (IDH1)
                       END-IF
                       ADD     1               TO  IDH1
                       MOVE    IDD             TO  PRT-HEN-DAY (IDH1)
                   END-IF
               END-IF
               IF      TBL-ERR-DAY2(IDD)   =   1
                   IF      IDE3            >   1
                       MOVE    ","         TO  PRT-ERR-H2(IDE3)
                   END-IF
                   ADD     1               TO  IDE3
                   MOVE    IDD             TO  PRT-ERR-DAY2(IDE3)
               END-IF
           END-PERFORM
           IF      IDH1                >   ZERO
               MOVE    "変更日："          TO  PRT-MSG (CNT-LINE)
               MOVE    PRT-HEN-AREA        TO  PRT-DAY-G (CNT-LINE)
               IF      PRT-HEN-AREA (49:)  NOT =   SPACE
                   PERFORM 29021-LINECNT-SEC
                   MOVE    PRT-HEN-AREA (49:)  TO  PRT-DAY-G(CNT-LINE)
               END-IF
           END-IF
           IF      IDE1                >   ZERO
               IF      IDH1            >   ZERO
                   PERFORM 29021-LINECNT-SEC
               END-IF
               MOVE    "按分日："          TO  PRT-MSG (CNT-LINE)
               MOVE    PRT-ERR-AREA        TO  PRT-DAY-G (CNT-LINE)
               IF      PRT-ERR-AREA (49:)  NOT =   SPACE
                   PERFORM 29021-LINECNT-SEC
                   MOVE    PRT-ERR-AREA (49:)  TO  PRT-DAY-G(CNT-LINE)
               END-IF
           END-IF
           IF      IDE3                >   ZERO
               IF     (IDH1            >   ZERO  )  OR
                      (IDE1            >   ZERO  )
                   PERFORM 29021-LINECNT-SEC
               END-IF
               MOVE    "エラー："          TO  PRT-MSG (CNT-LINE)
               MOVE    PRT-ERR-AREA2       TO  PRT-DAY-G (CNT-LINE)
               IF      PRT-ERR-AREA2(49:)  NOT =   SPACE
                   PERFORM 29021-LINECNT-SEC
                   MOVE    PRT-ERR-AREA2(49:)  TO  PRT-DAY-G(CNT-LINE)
               END-IF
           END-IF
      *
           .
       2009-UPD-PRINT-EXIT.
           EXIT.
      *****************************************************************
      *    確認リスト印刷処理
      *****************************************************************
       29021-LINECNT-SEC          SECTION.
      *
           ADD     1                   TO  CNT-LINE
           IF      CNT-LINE             >   50
               PERFORM 2902-HCML55-PRT02-SEC
               MOVE    1               TO  CNT-LINE
           END-IF
           .
       29021-LINECNT-EXIT.
           EXIT.
      *****************************************************************
      *    確認リスト印刷処理
      *****************************************************************
       2902-HCML55-PRT02-SEC          SECTION.
      *
           MOVE    SPACE               TO  HCMSL55
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL55-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL55-MOJI (3)
      *
           MOVE    4                  TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   50 )    OR
                              (IDX     >=  55 )
               ADD     1                   TO  IDX
               MOVE    LST-PRT-BODY (IDY)
                                       TO  HCMSL55-MOJI (IDX)
           END-PERFORM
           PERFORM 2902-PRINT02-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE
      *
           INITIALIZE                      LST-PRT-BODY-G
           .
       2902-HCML55-PRT02-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2902-PRINT02-SEC          SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"            TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                    TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                    TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                    TO  SPRT-SHELLID
      *****MOVE    LNK-PRTKANRI-SHORI-RENNUM
           MOVE    2
                                    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                    TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "同日受診４回以降回数変換リスト"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2902-PRINT02-EXIT.
           EXIT.
      *****************************************************************
      *    患者ＩＤチェック処理
      *****************************************************************
       2001-PTID-CHKSYORI-SEC          SECTION.
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-KEY-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "????"              TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-KEY-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           .
       2001-PTID-CHKSYORI-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBNOMIACCT UPD:"  CNT-UPD  " ***"
           DISPLAY "*** ORCBNOMIACCT ERR:"  CNT-ERR  " ***"
           DISPLAY "*** ORCBNOMIACCT END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
     ***** MOVE    CNT-UPD         TO  JOB-UPDCNT
           MOVE    CNT-OUT         TO  JOB-UPDCNT
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
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
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
      *    診療会計マスター次読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター次読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               INITIALIZE                      JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
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
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
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
      *
