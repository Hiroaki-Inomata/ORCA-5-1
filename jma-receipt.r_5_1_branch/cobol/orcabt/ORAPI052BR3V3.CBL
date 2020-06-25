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
       PROGRAM-ID.             ORAPI052BR3V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 在庫管理データ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/01/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  18/01/12  自費コード対象追加
      *  05.00.00    NACL-多々納  18/01/24  残量廃棄対応他
      *  05.00.00    NACL-多々納  19/12/XX  入院分対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
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
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACCTPLUS         PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
           03  FLG-HAIKI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT                 PIC 9(10).
           03  CNT-OUT1                PIC 9(10).
           03  CNT-XMLCOUNT            PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDK                     PIC 9(04).
           03  IDXM                    PIC 9(04).
           03  IDXR                    PIC 9(04).
           03  IDXA                    PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-J                   PIC 9(04).
      *
           03  IDXX                    PIC 9(04).
      *
           03  IDX-REN                 PIC 9(02).
           03  IDX-ACT                 PIC 9(02).
      *
           03  IDX-SRY                 PIC 9(04).
           03  IDX1                    PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-TERMID         PIC X(64).
           03  WRK-PARA-BASE-YMD       PIC X(08).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
      *    テスト患者区分
           03  WRK-PARA-TESTKBN        PIC X(01).
           03  WRK-PARA-ERRFILE        PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-KEY-DENPNUM         PIC X(07).
           03  WRK-KEY-SRYYMD          PIC X(08).
           03  WRK-KEY-SRYKA           PIC X(02).
      *R01.12
           03  WRK-KEY-PTID            PIC 9(10).
           03  WRK-KEY-HKNCOMBI        PIC X(04).
           03  WRK-KEY-RENNUM          PIC X(01).
      *
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-SRYKA-NAME          PIC X(20).
      *
           03   WRK-GOK-SURYO          PIC 9(09)V9(5).
      *
           03   WRK-SRYSURYO           PIC 9(09)V9(5).
           03   WRK-SURYO-CHU9         PIC 9(09).
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *    回数
           03  WRK-KAISU               PIC 9(03).
      *
           03  TMPFILE                 PIC X(100).
      *
       01  ZAIKOUT-REC.
           03  ZAIKOUT-OCC             OCCURS  68.
      *        伝票番号
               05  ZAIKOUT-INVOICD-NUMBER      PIC X(07).
      *        診療日付
               05  ZAIKOUT-PERFORM-DATE        PIC X(10).
      *        診療科
               05  ZAIKOUT-DEP-CODE            PIC X(02).
               05  ZAIKOUT-DEP-NAME            PIC X(20).
               05  ZAIKOUT-MEDICAL-INF         OCCURS   200.
      *            診療コード
                   07  ZAIKOUT-PRSCRPT-CODE    PIC X(9).
      *            診療名称
                   07  ZAIKOUT-PRSCRPT-NAME    PIC X(80).
      *            診療カナ名称
                   07  ZAIKOUT-PRSCRPT-KANANAME
                                               PIC X(80).
      *            数量
                   07  ZAIKOUT-PRSCRPT-NUMBER  PIC X(15).
      *            単価
                   07  ZAIKOUT-PRICD-MONEY     PIC X(9).
      *            単位コード
                   07  ZAIKOUT-TANNICD         PIC X(3).
      *            単位名称
                   07  ZAIKOUT-TANICD-NAME     PIC X(20).
      *
       01  WRK-ZAIKOUT-REC.
      *    伝票番号
           03  WRK-ZAIKOUT-INVOICD-NUMBER      PIC X(07).
      *    診療日付
           03  WRK-ZAIKOUT-PERFORM-DATE        PIC X(10).
      *    診療科
           03  WRK-ZAIKOUT-DEP-CODE            PIC X(02).
           03  WRK-ZAIKOUT-DEP-NAME            PIC X(20).
           03  WRK-ZAIKOUT-MEDICAL-INF         OCCURS   200.
      *        診療コード
               05  WRK-ZAIKOUT-PRSCRPT-CODE    PIC X(9).
      *        診療名称
               05  WRK-ZAIKOUT-PRSCRPT-NAME    PIC X(80).
      *        診療カナ名称
               05  WRK-ZAIKOUT-PRSCRPT-KANANAME
                                               PIC X(80).
      *        数量
               05  WRK-ZAIKOUT-PRSCRPT-NUMBER  PIC X(15).
      *        単価
               05  WRK-ZAIKOUT-PRICD-MONEY     PIC X(9).
      *        単位コード
               05  WRK-ZAIKOUT-TANNICD         PIC X(3).
      *        単位名称
               05  WRK-ZAIKOUT-TANICD-NAME     PIC X(20).
      *
      *    診療内容テーブル
       01  TBL-SRYACT-AREA.
           03  TBL-SRYACT-OCC          OCCURS   200.
               05  TBL-SRYCD                   PIC X(09).
               05  TBL-SURYO                   PIC 9(09)V9(5).
      *    診療科テーブル
       01  TBL-SRYKACD-AREA.
           03  TBL-SRYKACD-OCC         OCCURS   99.
               05  TBL-SRYKACD         PIC X(02).
               05  TBL-SRYKA-NAME      PIC X(20).
      *
      *
      *    診療行為マスタ−
       01  HZN-SRYACT-AREA.
         02  HZN-SRYACT-REC            OCCURS   10.
           COPY    "CPSRYACT.INC"      REPLACING
                                     //SRY-// BY //HZN-SRY-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
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
                   UNTIL        FLG-END    =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TBL-SRYACT-AREA
           INITIALIZE                  TBL-SRYKACD-AREA
           MOVE    ZERO            TO  CNT-AREA
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-BASE-YMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-TESTKBN
           END-UNSTRING
      *
           DISPLAY "NYUGAIKBN =" WRK-PARA-NYUGAIKBN
      *
      *    ＳＰＡ設定
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR3V3"        TO  JOB-PGID
           MOVE    "在庫管理データ作成"
                                       TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
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
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    入外区分
           MOVE    WRK-PARA-NYUGAIKBN  TO  SPA-NYUGAIKBN
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *R01.12
      *    入院分
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 2100-NYUIN-MAIN-SEC
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    受診歴を検索する  医療機関、基準日以降
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-BASE-YMD   TO  JYURRK-UPYMD
      *
           IF      WRK-PARA-TESTKBN    =   "1"
      *        テスト患者を含める
               MOVE    "key66"             TO  WRK-MCP-PATHNAME
           ELSE
      *        テスト患者を含めない
               MOVE    "key71"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    在庫管理編集処理
           PERFORM     UNTIL   FLG-JYURRK  =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  ZAIKOUT-REC
               INITIALIZE                      ZAIKOUT-REC
      *
               PERFORM         UNTIL   FLG-JYURRK  =   1
                               OR      CNT-OUT1    >=  68
                   PERFORM 2001-ZAIKO-HENSYU-SEC
               END-PERFORM
      *
               ADD     1                   TO  CNT-XMLCOUNT
               PERFORM 910-XMLEXPORT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                  TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    在庫管理編集処理
      *****************************************************************
       2001-ZAIKO-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-S
           INITIALIZE                  TBL-SRYACT-AREA
      *
           PERFORM
               UNTIL  ( FLG-JYURRK         =   1    )
                  OR  ( FLG-ERR        NOT =   ZERO )
      *H30.10
                  OR  ( CNT-OUT1           >=  68  )
               INITIALIZE                  TBL-SRYACT-AREA
               MOVE    ZERO                TO  IDX-S
               MOVE    JYURRK-DENPNUM      TO  WRK-KEY-DENPNUM
               MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
               MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
      *
               PERFORM
                   UNTIL  ( FLG-JYURRK         =   1               )
                      OR  ( FLG-ERR        NOT =   ZERO            )
                      OR  ( JYURRK-DENPNUM NOT =   WRK-KEY-DENPNUM )
      *
                   IF      FLG-ERR             =   ZERO
                       PERFORM 20021-JYURRK-SYORI-SEC
                   END-IF
      *
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-PERFORM
      *
               IF      IDX-S               >   ZERO
                   PERFORM 20022-STOCKRES-HEN-SEC
               END-IF
           END-PERFORM
           .
      *
       2001-ZAIKO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容編集処理
      *****************************************************************
       20021-JYURRK-SYORI-SEC          SECTION.
      *
           PERFORM VARYING     IDX-J   FROM    1   BY  1
                   UNTIL      (IDX-J       >   15      )
                           OR (FLG-ERR     NOT =   ZERO)
               IF      JYURRK-ZAINUM (IDX-J)   NOT =   ZERO
                   PERFORM 20034-ZAISET-HEN-SEC
               END-IF
           END-PERFORM
           .
       20021-JYURRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容編集処理
      *****************************************************************
       20034-ZAISET-HEN-SEC          SECTION.
      *
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM (IDX-J)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                      SRYACCT-REC
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-ACT
      *    受診連番
           COMPUTE IDX-REN             =   JYURRK-RENNUM
                                       +   1
      *
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *
           MOVE    ZERO                TO  FLG-CHK
      *    回数ゼロは対象外
           IF     (WRK-KAISU           =   ZERO )
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
      *    薬剤がない時
           IF      (SRY-SRYKBN         =   "11"
                                       OR  "12"
                                       OR  "13"
                                       OR  "99"
                                       OR  "97"
                                       OR  "98" )
               MOVE    1                   TO  FLG-CHK
           END-IF
           IF     (FLG-CHK             =   1     )
               GO      TO      20034-ZAISET-HEN-EXT
           END-IF
      *    診療行為テーブル検索
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *    院外投薬は対象外
           IF      (SRY-SRYKBN         =   "21"
                                       OR  "22"
                                       OR  "23" )
              AND  (ACCT-ZAITEN        =   ZERO )
               IF      SRY-SRYSYUKBN       =   "213"
                                           OR  "223"
                                           OR  "233"
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-IF
      *    在宅（院外）
           IF      SRY-SRYSYUKBN       =   "148"
                                       OR  "149"
               MOVE    1               TO  FLG-CHK
           END-IF
      *R01.12
      *    持参薬は対象外
           IF     (ACCT-NYUGAIKBN      =   "1"  )
              AND (ACCT-ZAIKBN         =   3
                                       OR  4    )
               MOVE    1               TO  FLG-CHK
           END-IF
      *
           INITIALIZE                  HZN-SRYACT-AREA
           MOVE    ZERO                TO  IDX-SRY
           PERFORM
               UNTIL  ( FLG-SRYACT     =   1  )
                  OR  ( FLG-CHK        =   1  )
                  OR  ( IDX-SRY        >=  10 )
               ADD     1                   TO  IDX-SRY
               MOVE    SRYACT-REC          TO  HZN-SRYACT-REC
                                                     (IDX-SRY)
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-SRY         >   ZERO
               PERFORM 200341-SRYACT-HEN-SEC
           END-IF
           .
       20034-ZAISET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200341-SRYACT-HEN-SEC          SECTION.
      *
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   IDX-SRY  )
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   5   )   OR
                                  (HZN-SRY-SRYCD (IDX1 IDXA)
                                           =   SPACE)
      *            薬剤
                   MOVE    ZERO            TO  FLG-HAIKI
                   IF      (HZN-SRY-SRYCD(IDX1 IDXA)(1:1)   =   "6" )
      *                次行の残量廃棄コード判定
                       IF     (IDXA            =   5  )
                          AND (IDX1            <   IDX-SRY)
                           IF      HZN-SRY-SRYCD (IDX1 + 1 1)
                                               =   "099309901"
                               MOVE    1       TO  FLG-HAIKI
                           END-IF
                       END-IF
                       IF      IDXA            <   5
                           IF      HZN-SRY-SRYCD (IDX1 IDXA + 1)
                                               =   "099309901"
                               MOVE    1       TO  FLG-HAIKI
                           END-IF
                       END-IF
      *                明細編集処理
                       PERFORM 200341-MEISE-HEN-SEC
                   END-IF
      *            自費コードはすべて対象
                   IF     ((HZN-SRY-SRYKBN(IDX1)   =   "95"
                                                   OR  "96" ) AND
                           (HZN-SRY-SRYCD(IDX1 IDXA)(1:3)
                                                   =   "095"
                                                   OR  "096" ))
      *                明細編集処理
                       PERFORM 200341-MEISE-HEN-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       200341-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200341-MEISE-HEN-SEC          SECTION.
      *
           ADD     1                   TO  IDX-S
           IF      IDX-S               >   200
               GO      TO      200341-MEISE-HEN-EXT
           END-IF
      *
           MOVE    HZN-SRY-SRYCD (IDX1 IDXA)
                                      TO  TBL-SRYCD (IDX-S)
      *    残量廃棄あり
           IF      FLG-HAIKI           =   1
               COMPUTE WRK-SURYO-CHU9      =   HZN-SRY-SRYSURYO
                                                  (IDX1 IDXA)
                                           +   0.99999
               MOVE    WRK-SURYO-CHU9      TO  WRK-SRYSURYO
           ELSE
               MOVE    HZN-SRY-SRYSURYO(IDX1 IDXA)
                                           TO  WRK-SRYSURYO
           END-IF
      *    数量×回数
           COMPUTE WRK-GOK-SURYO       =   WRK-SRYSURYO
                                       *   WRK-KAISU
           MOVE    WRK-GOK-SURYO       TO  TBL-SURYO (IDX-S)
      *
           .
       200341-MEISE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20022-STOCKRES-HEN-SEC        SECTION.
      *
           INITIALIZE                     WRK-ZAIKOUT-REC
      *    伝票番号
           MOVE    WRK-KEY-DENPNUM     TO  WRK-ZAIKOUT-INVOICD-NUMBER
      *    診療日付
           MOVE    WRK-KEY-SRYYMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-ZAIKOUT-PERFORM-DATE
      *
           MOVE    WRK-KEY-SRYYMD      TO  WRK-SRYYMD
      *    診療科
           MOVE    WRK-KEY-SRYKA       TO  WRK-ZAIKOUT-DEP-CODE
           PERFORM 20011-SRYKAMEI-HEN-SEC
           MOVE    WRK-SRYKA-NAME      TO  WRK-ZAIKOUT-DEP-NAME
      *
      *    明細
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   200)
                        OR    (TBL-SRYCD (IDX)     =   SPACE )
               MOVE    TBL-SRYCD (IDX)     TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      TNS-TENSIKIBETU     =   "7"
      *            減点は対象外
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDY
      *
                   MOVE    TBL-SRYCD (IDX)     TO
                                       WRK-ZAIKOUT-PRSCRPT-CODE    (IDY)
                   MOVE    TNS-NAME            TO
                                       WRK-ZAIKOUT-PRSCRPT-NAME    (IDY)
                   MOVE    TNS-KANANAME        TO
                                       WRK-ZAIKOUT-PRSCRPT-KANANAME(IDY)
      *            数量×回数
                   MOVE    TBL-SURYO(IDX)      TO  WRK-SURYO
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX        TO
                                       WRK-ZAIKOUT-PRSCRPT-NUMBER  (IDY)
      *            単価
                   IF     (TNS-TENSIKIBETU     =   "4"  )
                     AND  (TNS-TEN             >   ZERO )
                       COMPUTE WRK-SURYO       =   TNS-TEN   /  10
                   ELSE
                       MOVE    TNS-TEN             TO  WRK-SURYO
                   END-IF
                   PERFORM 810-NUMHENKAN-SEC
                   MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                               TO
                                       WRK-ZAIKOUT-PRICD-MONEY     (IDY)
      *            単位
                   IF     (TNS-SRYCD (1:1)     =   "6"   )
                     OR  ((TNS-SRYCD (1:3)     =   "095"
                                               OR  "096" ) AND
                          (TNS-YKZKBN       NOT =   ZERO ))
      *                薬剤、自費薬剤のみ
                       MOVE    TNS-TANICD          TO
                                       WRK-ZAIKOUT-TANNICD         (IDY)
                       MOVE    TNS-TANINAME        TO
                                       WRK-ZAIKOUT-TANICD-NAME     (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDY                 =   ZERO
               INITIALIZE                  WRK-ZAIKOUT-REC
           ELSE
               ADD     1                   TO  CNT-OUT1
                                               CNT-OUT
               MOVE    CNT-OUT1            TO  IDXX
               MOVE    WRK-ZAIKOUT-REC         TO  ZAIKOUT-OCC (IDXX)
           END-IF
           .
       20022-STOCKRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       20011-SRYKAMEI-HEN-SEC          SECTION.
      *
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   99 )
                         OR   (TBL-SRYKACD (IDK)   =   SPACE )
               IF     (WRK-KEY-SRYKA
                                       =  TBL-SRYKACD (IDK))
                   MOVE    IDK            TO  IDX-H
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        診療科検索
               PERFORM 20011-SRYKA-CHK-SEC
               ADD     1                   TO  IDX-H
               MOVE    WRK-KEY-SRYKA       TO  TBL-SRYKACD (IDX-H)
               MOVE    WRK-SRYKA-NAME      TO  TBL-SRYKA-NAME (IDX-H)
           ELSE
               MOVE    TBL-SRYKA-NAME(IDX-H)
                                           TO  WRK-SRYKA-NAME
           END-IF
           .
       20011-SRYKAMEI-HEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-KEY-SRYKA       TO  SYS-1005-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    SPACE               TO  WRK-SRYKA-NAME
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *R01.12
      *****************************************************************
      *    入院分　主処理
      *****************************************************************
       2100-NYUIN-MAIN-SEC             SECTION.
      *
      *    受診歴を検索する  医療機関、基準月の受診履歴のみ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           STRING  WRK-PARA-BASE-YMD(1:6)
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    JYURRK-SRYYMD
           END-STRING
      *
           IF      WRK-PARA-TESTKBN    =   "1"
      *        テスト患者を含める
               MOVE    "key74"             TO  WRK-MCP-PATHNAME
           ELSE
      *        テスト患者を含めない
               MOVE    "key75"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    在庫管理編集処理
           PERFORM     UNTIL   FLG-JYURRK  =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  ZAIKOUT-REC
               INITIALIZE                      ZAIKOUT-REC
      *
               PERFORM         UNTIL   FLG-JYURRK  =   1
                               OR      CNT-OUT1    >=  68
                   PERFORM 2100-NYUIN-ZAIKO-HENSYU-SEC
               END-PERFORM
      *
               ADD     1                   TO  CNT-XMLCOUNT
               PERFORM 910-XMLEXPORT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                  TO  FLG-END
           .
       2100-NYUIN-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    在庫管理編集処理（入院分）
      *****************************************************************
       2100-NYUIN-ZAIKO-HENSYU-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-S
           INITIALIZE                  TBL-SRYACT-AREA
      *
           PERFORM
               UNTIL  ( FLG-JYURRK         =   1    )
                  OR  ( FLG-ERR        NOT =   ZERO )
                  OR  ( CNT-OUT1           >=  68  )
               INITIALIZE                  TBL-SRYACT-AREA
               MOVE    ZERO                TO  IDX-S
               MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
               MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
               MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-KEY-HKNCOMBI
               MOVE    JYURRK-RENNUM       TO  WRK-KEY-RENNUM
      *
               PERFORM
                   UNTIL  ( FLG-JYURRK         =   1               )
                      OR  ( FLG-ERR        NOT =   ZERO            )
                      OR  ( JYURRK-PTID        NOT =   WRK-KEY-PTID )
                      OR  ( JYURRK-SRYYMD      NOT =   WRK-KEY-SRYYMD)
                      OR  ( JYURRK-SRYKA       NOT =   WRK-KEY-SRYKA )
                      OR  ( JYURRK-HKNCOMBINUM NOT =
                                                   WRK-KEY-HKNCOMBI)
                      OR  ( JYURRK-RENNUM      NOT =   WRK-KEY-RENNUM )
      *
                   IF      FLG-ERR             =   ZERO
                       PERFORM 20021-JYURRK-SYORI-SEC
                   END-IF
      *
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-PERFORM
      *
               IF      IDX-S               >   ZERO
                   PERFORM 20022-STOCKRES-HEN-SEC
               END-IF
           END-PERFORM
           .
      *
       2100-NYUIN-ZAIKO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC          SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE    ZAIKOUT-REC     TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_stock1"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "stock1"        TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  "stock1"        DELIMITED   BY  SIZE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    "stock1"        TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    "2"             TO  SAPIFILE-IN-KBN
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "stock1.xml"    TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
                   CONTINUE
               ELSE
                   STRING   "ｘｍｌファイル作成エラー RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI052BR3V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR3V3 END ***"
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORAPI052BR3V3 "    DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位反映
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )
             AND  (FLG-TENSU           =   ZERO )
               PERFORM 9101-TENSUPLUS-READ-SEC
               IF      (FLG-TENSUPLUS          =   ZERO )
                 AND   (TNSP-USERTANICD    NOT =   SPACE)
                    MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                    MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
               END-IF
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *R02.2
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
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
