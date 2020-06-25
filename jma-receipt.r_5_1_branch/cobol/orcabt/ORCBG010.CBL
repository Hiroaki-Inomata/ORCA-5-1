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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次処理
      *  コンポーネント名  : 未収金一覧（集計表）
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/10/03    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 01.00.01     NACL-森脇     03/01/06  最終受診日セット修正
      * 01.00.02     NACL-森脇     04/02/02  退院日ラベル表示、診療科
      * 01.00.03     NACL-太田     04/02/04  最終入金日を追加
      * 03.05.01     NACL-森脇     07/06/05  グループ診療対応
      * 04.08.01     NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    並び替え用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    並び替え用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-PTNUM         PIC X(20).
           03  TOKEI-PTID              PIC 9(10).
           03  TOKEI-MISYU             PIC S9(10).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    統計ファイル
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    印刷用領域
           COPY    "HCM38.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-BD003               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                PIC 9(08).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(06).
           03  CNT-TOKEI               PIC 9(04).
           03  CNT-KENSU               PIC 9(05).
           03  CNT-TAIIN               PIC 9(05).
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  PARA-JOBID              PIC 9(07).
           03  PARA-SHELLID            PIC X(08).
           03  PARA-HOSPNUM            PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  PARA-SITEIKBN           PIC X(01).
           03  PARA-YUKOSTRYM          PIC X(06).
           03  PARA-YUKOENDYM          PIC X(06).
           03  PARA-SYORIKBN           PIC X(01).
           03  PARA-MISYUKBN           PIC X(01).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-TOUKEIERR           PIC X(200).
           03  WRK-REC                 PIC X(20000).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD              PIC 9(08).
           03  WRK-HENYMD              PIC X(09).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-YUKOSTRYM           PIC X(16).
           03  WRK-YUKOENDYM           PIC X(16).
           03  WRK-YUKOSTRYMD          PIC X(08).
           03  WRK-YUKOENDYMD          PIC X(08).
      *
           03  WRK-PTNUM               PIC X(20).
           03  WRK-STR                 PIC X(200).
           03  WRK-LEN                 PIC 9(05).
           03  WRK-SHO                 PIC 9(05).
           03  WRK-AMARI               PIC 9(05).
      *
           03  WRK-SURYO               PIC S9(10).
           03  WRK-GSURYO              PIC S9(10).
      *
           03  WRK-Z5                  PIC ZZZZ9.
           03  WRK-Z6                  PIC ZZZZZ9.
           03  WRK-Z11                 PIC ---,---,--9.
      *     03  WRK-CSV                 PIC X(09).
           03  WRK-HENSURYO            PIC X(10).
           03  WRK-CSV                 PIC ---------9.
      *
           03  WRK-SAKUSEIBI           PIC X(22).
           03  WRK-KIKAN               PIC X(40).
           03  WRK-MES                 PIC X(50).
           03  WRK-BIKOU               PIC X(30).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-LASTYMD             PIC X(10).
           03  WRK-NYUHENYMD           PIC X(10).
           03  WRK-TABLE               PIC X(64).
           03  WRK-PATHNAME            PIC X(64).
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(15)   VALUE
                                                   "misyuukin_list_".
               05  WRK-TO-DATA-KIKAN   PIC X(21).
           03  WRK-TO-DATA1.
               05  WRK-TO-DATA-YMD     PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
           03  WRK-TO-DATA2.
               05  WRK-TO-DATA-STRYMD  PIC X(06).
               05  FILLER              PIC X(01)   VALUE   "-".
               05  WRK-TO-DATA-ENDYMD  PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "未収金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "最終入金日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "最終受診日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "最終受診科名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "電話番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "備考".
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                PIC 9(10).
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(80).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
      *    未収金一覧用収納明細
       01  BD003-REC.
           COPY    "CPBD003.INC".
      *
      *    患者番号情報
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    最終受診日取得
           COPY    "CPORCSLASTYMD.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
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
      ****************************************************************
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
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  PARA-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ERR-EDIT-AREA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    ＤＢ　オープン処理
           PERFORM 100-DBOPEN-SEC
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
                                       PARA-JOBID
                                       PARA-SHELLID
                                       PARA-HOSPNUM
                                       RECEERR
                                       PARA-SITEIKBN
                                       PARA-YUKOSTRYM
                                       PARA-YUKOENDYM
                                       PARA-SYORIKBN
                                       PARA-MISYUKBN
           END-UNSTRING
           MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPA-SYSYMD
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    PARA-JOBID      TO  JOB-JOBID
           MOVE    PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBG010"      TO  JOB-PGID
           MOVE    "未収金一覧（集計表）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *    ファイル名編集
           MOVE    "BG01001"       TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM     TO  TOKEIPARA-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( SYS-1001-BEDSU  >   ZERO )
               MOVE    1           TO  SPA-BEDFLG
           ELSE
               MOVE    ZERO        TO  SPA-BEDFLG
           END-IF
      *
      *    作成日セット
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SAKUSEIBI
      *
      *    区分チェック
           PERFORM 120-PARA-CHECK-SEC
      *
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分チェック処理
      *****************************************************************
       120-PARA-CHECK-SEC          SECTION.
      *
      *    期間指定区分チェック
           EVALUATE    PARA-SITEIKBN
           WHEN    "0"
               IF   (( PARA-YUKOSTRYM  NOT =   SPACE ) AND
                     ( PARA-YUKOSTRYM  NOT =   "000000" ))
               OR   (( PARA-YUKOENDYM  NOT =   SPACE ) AND
                     ( PARA-YUKOENDYM  NOT =   "999999" ))
                   MOVE
                   "期間指定なしの場合は期間を入力しないでください"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  120-PARA-CHECK-EXT
               END-IF
               MOVE    "00000000"      TO  WRK-YUKOSTRYMD
               MOVE    "99999999"      TO  WRK-YUKOENDYMD
               MOVE    "1"             TO  PARA-SYORIKBN
           WHEN    "1"
      *        期間開始年月チェック
               PERFORM 1201-PARA-STR-CHECK-SEC
               IF          FLG-END         NOT =   ZERO
                   GO  TO  120-PARA-CHECK-EXT
               END-IF
      *
      *        期間終了年月チェック
               PERFORM 1201-PARA-END-CHECK-SEC
               IF      FLG-END         NOT =   ZERO
                   GO  TO  120-PARA-CHECK-EXT
               END-IF
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "期間指定区分が誤りです［"
                                                   DELIMITED   BY  SIZE
                       PARA-SITEIKBN               DELIMITED   BY  SIZE
                       "］"                        DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  120-PARA-CHECK-EXT
           END-EVALUATE
      *
      *    期間範囲チェック
           PERFORM 1201-PARA-KIKAN-CHECK-SEC
           IF      FLG-END         NOT =   ZERO
               MOVE    "期間を正しく入力してください"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  120-PARA-CHECK-EXT
           END-IF
      *
      *    期間処理区分チェック
           IF      WRK-YUKOENDYMD  NOT =   "99999999"
               PERFORM 1201-PARA-SYORIKBN-CHECK-SEC
           END-IF
      *
           MOVE    SPACE           TO  WRK-TO-DATA-KIKAN
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
           IF      WRK-YUKOSTRYMD(1:6) =   WRK-YUKOENDYMD(1:6) 
               MOVE    WRK-YUKOSTRYMD(1:6) TO  WRK-TO-DATA-YMD
               MOVE    WRK-TO-DATA1    TO  WRK-TO-DATA-KIKAN
           ELSE
               MOVE    WRK-YUKOSTRYMD(1:6) TO  WRK-TO-DATA-STRYMD
               MOVE    WRK-YUKOENDYMD(1:6) TO  WRK-TO-DATA-ENDYMD
               MOVE    WRK-TO-DATA2    TO  WRK-TO-DATA-KIKAN
           END-IF
           .
      *
       120-PARA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始期間チェック処理
      *****************************************************************
       1201-PARA-STR-CHECK-SEC     SECTION.
      *
      *    期間開始年月チェック
           EVALUATE    PARA-YUKOSTRYM
           WHEN    SPACE
               MOVE    "開始年月が未設定です"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           WHEN    "000000"
               MOVE    "00000000"      TO  WRK-YUKOSTRYMD
           WHEN    OTHER
      *        日付変換
               MOVE    PARA-YUKOSTRYM  TO  WRK-SYMD(1:6)
               MOVE    "01"            TO  WRK-SYMD(7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               IF          STS-DAY-RC1     =   ZERO
                   MOVE    WRK-HENYMDG(1:16)
                                           TO  WRK-YUKOSTRYM
                   MOVE    WRK-SYMD        TO  WRK-YUKOSTRYMD
               ELSE
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "開始年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       PARA-YUKOSTRYM          DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
               END-IF
           END-EVALUATE
      *
           .
      *
       1201-PARA-STR-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了期間チェック処理
      *****************************************************************
       1201-PARA-END-CHECK-SEC     SECTION.
      *
      *    期間終了年月チェック
           EVALUATE    PARA-YUKOENDYM
           WHEN    "999999"
               MOVE    "99999999"      TO  WRK-YUKOENDYMD
           WHEN    OTHER
      *        月末日取得
               IF          PARA-YUKOENDYM  =   SPACE
                   MOVE    PARA-YUKOSTRYM  TO  WRK-SYMD(1:6)
               ELSE
                   MOVE    PARA-YUKOENDYM  TO  WRK-SYMD(1:6)
               END-IF
               PERFORM 20021-GETUMATU-GET-SEC
               IF          STS-DAY-RC1     NOT =   ZERO
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "終了年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       PARA-YUKOENDYM          DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1201-PARA-END-CHECK-EXT
               END-IF
      *
               PERFORM 31012-SEIWA-HEN-SEC
               IF          STS-DAY-RC1     =   ZERO
                   MOVE    WRK-HENYMDG(1:16)
                                           TO  WRK-YUKOENDYM
                   MOVE    WRK-SYMD        TO  WRK-YUKOENDYMD
               ELSE
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "終了年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       PARA-YUKOENDYM          DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1201-PARA-END-CHECK-EXT
               END-IF
           END-EVALUATE
      *
           IF      WRK-YUKOSTRYMD  <=   WRK-YUKOENDYMD
               CONTINUE
           ELSE
               MOVE    "開始年月＞終了年月となっています"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1201-PARA-END-CHECK-EXT
           END-IF
      *
           .
      *
       1201-PARA-END-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間範囲チェック処理
      *****************************************************************
       1201-PARA-KIKAN-CHECK-SEC   SECTION.
      *
      *    範囲チェック
           IF          WRK-YUKOSTRYMD(1:6)
                                   =   WRK-YUKOENDYMD(1:6)
               MOVE    WRK-YUKOSTRYM   TO  WRK-KIKAN
           ELSE
               IF        ( WRK-YUKOSTRYMD  =   "00000000" )
                   AND   ( WRK-YUKOENDYMD  =   "99999999" )
                   MOVE    "指定なし"      TO  WRK-KIKAN
               ELSE
                   IF          WRK-YUKOSTRYMD  =   "OOOOOO00"
                       MOVE    SPACE           TO  WRK-KIKAN(1:16)
                   ELSE
                       MOVE    WRK-YUKOSTRYM   TO  WRK-KIKAN(1:16)
                   END-IF
                   IF          WRK-YUKOENDYMD  =   "99999999"
                       MOVE    SPACE           TO  WRK-KIKAN(19:16)
                   ELSE
                       MOVE    WRK-YUKOENDYM   TO  WRK-KIKAN(19:16)
                   END-IF
                   MOVE    "〜"            TO  WRK-KIKAN(17:2)
               END-IF
           END-IF
      *
           .
      *
       1201-PARA-KIKAN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間処理区分チェック処理
      *****************************************************************
       1201-PARA-SYORIKBN-CHECK-SEC    SECTION.
      *
      *    期間処理区分チェック
           EVALUATE    PARA-SYORIKBN
           WHEN    "1"
               MOVE    "期間内の診療分（期間外の訂正・入金を含む）"
                                       TO  WRK-MES
           WHEN    "2"
               MOVE    "期間内の診療分（期間外の訂正・入金を含まない）"
                                       TO  WRK-MES
           WHEN    "3"
               MOVE    "期間内の請求分（期間外の訂正・入金を含む）"
                                       TO  WRK-MES
           WHEN    "4"
               MOVE    "期間内の請求分（期間外の訂正・入金を含まない）"
                                       TO  WRK-MES
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "処理区分が誤りです［"
                                                   DELIMITED   BY  SIZE
                       PARA-SYORIKBN               DELIMITED   BY  SIZE
                       "］"                        DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-EVALUATE
       .
      *
       1201-PARA-SYORIKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           OPEN    OUTPUT              TOKEI-FILE
      *
      *    統計ファイルセット
           PERFORM 210-TOKEI-SET-SEIKYU-SEC
      *
           CLOSE                       TOKEI-FILE
      *
           OPEN    INPUT               TOKEI-FILE
      *
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
           IF      FLG-TOKEI   =   ZERO
      *        ＣＳＶ出力処理
               INITIALIZE                  WRK-REC
               MOVE    CSV-HEAD-01     TO  WRK-REC
               PERFORM 500-TOUKEICSV-SEC
           END-IF
      *    統計データがなくなるまで
           PERFORM UNTIL   FLG-TOKEI   =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE                       TOKEI-FILE
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           MOVE    1               TO  FLG-END
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイルセット
      *****************************************************************
       210-TOKEI-SET-SEIKYU-SEC    SECTION.
      *
           MOVE    ZERO            TO  WRK-GSURYO
      *
      *    未収金一覧用収納明細情報読み込み
           IF    ( PARA-SYORIKBN   =   "3" OR "4" )
               MOVE    "view_bd003"    TO  WRK-TABLE
               MOVE    "key5"          TO  WRK-PATHNAME
           ELSE
               MOVE    "view_bd003"    TO  WRK-TABLE
               MOVE    "key"           TO  WRK-PATHNAME
           END-IF
           PERFORM 900-BD003-SELECT-SEC
      *
      *    未収金一覧用収納明細情報セット
           PERFORM UNTIL   FLG-BD003   =   1
      *
               IF    ( PARA-MISYUKBN   =   "1" )
                   IF    ( BD003-SKYMONEY  >   BD003-NYUHEN-MONEY )
                       PERFORM 2101-TOKEI-SET-SEIKYU-SEC
                   END-IF
               ELSE
                   PERFORM 2101-TOKEI-SET-SEIKYU-SEC
               END-IF
      *
      *        未収金一覧用収納明細情報読み込み
               PERFORM 900-BD003-FETCH-SEC
           END-PERFORM
      *
      *    未収金一覧用収納明細情報読み込み
           PERFORM 900-BD003-CLOSE-SEC
      *
           .
       210-TOKEI-SET-SEIKYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイルセット
      *****************************************************************
       2101-TOKEI-SET-SEIKYU-SEC    SECTION.
      *
      *    患者番号取得
           PERFORM 20011-PTNUM-GET-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  2101-TOKEI-SET-SEIKYU-EXT
           END-IF
      *
      *    統計データ作成
           INITIALIZE                  TOKEI-REC
           MOVE    BD003-PTID      TO  TOKEI-PTID
           MOVE    SPTID-PTNUM     TO  TOKEI-PTNUM
           COMPUTE WRK-SURYO       =   BD003-SKYMONEY
                                   -   BD003-NYUHEN-MONEY
           MOVE    WRK-SURYO       TO  TOKEI-MISYU
           WRITE   TOKEI-REC
      *
      *    統計データカウント
           ADD     1               TO  CNT-TOKEI
      *
           .
       2101-TOKEI-SET-SEIKYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
      *    帳票領域クリア
           INITIALIZE                  HCM38
           MOVE    SPACE           TO  HCM38
      *
      *    明細クリア・頁カウント
           MOVE    ZERO            TO  CNT-LINE
           MOVE    ZERO            TO  CNT-TAIIN
           ADD     1               TO  CNT-PAGE
      *
      *    頁セット
           MOVE    CNT-PAGE        TO  WRK-Z6
           MOVE    WRK-Z6          TO  HCM38-PAGE
      *
      *    作成日セット
           MOVE    WRK-SAKUSEIBI   TO  HCM38-SAKUSEIBI
      *
      *    期間範囲セット
           MOVE    WRK-KIKAN       TO  HCM38-KIKAN
      *
      *    処理区分メッセージセット
           MOVE    WRK-MES         TO  HCM38-MES
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
      *    ３０明細を超えるまたは統計データがなくなるまでセット
           PERFORM UNTIL ( CNT-LINE    >=  30 )
                   OR    ( FLG-TOKEI   =   1 )
      *        明細カウント
               ADD     1               TO  CNT-LINE
               ADD     1               TO  CNT-KENSU
      *
      *        番号セット
               MOVE    CNT-KENSU       TO  WRK-Z5
               MOVE    WRK-Z5          TO  HCM38-CD(CNT-LINE)
      *
      *        患者番号編集
               PERFORM 3201-BODY-PTNUM-HEN-SEC
      *
      *        患者氏名・自宅電話番号編集
               PERFORM 3201-BODY-NAMETEL-HEN-SEC
      *
      *        最終受診日編集
               PERFORM 3201-BODY-LASTYMD-HEN-SEC
      *
      *        診療科名編集
               PERFORM 3201-BODY-SRYKA-HEN-SEC
      *
      *        最終入金日編集
               PERFORM 3201-BODY-LASTNYKNYMD-HEN-SEC
      *
      *        未収金額セット
               MOVE    TOKEI-MISYU     TO  WRK-Z11
               MOVE    WRK-Z11         TO  HCM38-MISYU(CNT-LINE)
               MOVE    TOKEI-MISYU     TO  WRK-CSV
               PERFORM 400-ZHEN-SURYO-SET-SEC
      *         MOVE    WRK-HENSURYO    TO  WRK-CSV
      *
               COMPUTE WRK-GSURYO      =   WRK-GSURYO
                                       +   TOKEI-MISYU
      *
               MOVE    BD003-NYUHEN-YMD
                                       TO  WRK-SYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-NYUHENYMD
               MOVE    ORCSLASTYMD-LASTYMD
                                       TO  WRK-SYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-LASTYMD
               INITIALIZE                  WRK-REC
               STRING  PTNUM-PTNUM         DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       PTINF-NAME          DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       PTINF-KANANAME      DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       WRK-HENSURYO        DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       WRK-NYUHENYMD       DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       WRK-LASTYMD         DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       SYS-1005-SRYKANAME1(1:8)
                                           DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       PTINF-HOME-TEL1     DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       WRK-BIKOU           DELIMITED  BY  SPACE
                       INTO                WRK-REC
               END-STRING
               PERFORM 500-TOUKEICSV-SEC
      *
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
      *
           END-PERFORM
      *
           IF      CNT-TAIIN           >   ZERO
               MOVE    "*退院日"       TO  HCM38-TAIIN
           END-IF
      *
           IF      FLG-TOKEI           =   1
               MOVE    "合計"          TO  HCM38-TITLE2
               MOVE    WRK-GSURYO      TO  WRK-Z11
               MOVE    WRK-Z11         TO  HCM38-GMISYU
           END-IF
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
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
           MOVE    "HCM38.red"         TO  SPRT-PRTID
           MOVE    "未収金一覧（集計表）"
                                       TO  SPRT-TITLE
           MOVE    HCM38               TO  SPRT-PRTDATA
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
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   10
               IF      WRK-CSV(CNT-KETA2:1)    NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE    WRK-CSV(CNT-KETA2:1)
                                           TO  WRK-HENSURYO(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF          STS-RECEERR     =   ZERO
               CONTINUE
           ELSE
      *
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
      *
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    PARA-JOBID      TO  JOB-JOBID
               MOVE    PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-TOKEI       =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG010 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    PARA-JOBID      TO  JOB-JOBID
           MOVE    PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
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
      *    月末日取得処理
      *****************************************************************
       20021-GETUMATU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD(1:6)   TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-SYMD
      *
           .
       20021-GETUMATU-GET-EXT.
            EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMD
                                       WRK-HENYMDG
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF    ( STS-DAY-RC1     =    ZERO  )
               MOVE    LNK-DAY2-EDTYMD1
                                  TO   WRK-HENYMD
               MOVE    LNK-DAY2-EDTYMD3
                                  TO   WRK-HENYMDG
           END-IF
      *
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       3201-BODY-PTNUM-HEN-SEC     SECTION.
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
      *
      *    患者番号セット
           IF          PTNUM-PTNUM(11:)    =   SPACE
               MOVE    PTNUM-PTNUM     TO  HCM38-PTNUM(CNT-LINE)
           ELSE
               MOVE    PTNUM-PTNUM(1:10)
                                       TO  HCM38-PTNUMUP(CNT-LINE)
               MOVE    PTNUM-PTNUM(11:10)
                                       TO  HCM38-PTNUMDN(CNT-LINE)
           END-IF
      *
           .
       3201-BODY-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終受診日、診療科編集処理
      *****************************************************************
       3201-BODY-LASTYMD-HEN-SEC   SECTION.
      *
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    TOKEI-PTID      TO  ORCSLASTYMD-PTID
           MOVE    "1"             TO  ORCSLASTYMD-SRYKAFLG
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
           MOVE    ORCSLASTYMD-LASTYMD
                                       TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMD      TO  HCM38-LASTYMD(CNT-LINE)
      *
           MOVE    SPACE           TO  WRK-BIKOU
      *    入院中
           IF    ( ORCSLASTYMD-BRMNUM  NOT =   SPACE )
               IF      ORCSLASTYMD-LASTYMD     =   SPACE
                   MOVE    SPACE           TO  WRK-BIKOU
                   MOVE    "入院中　"      TO  WRK-BIKOU(1:8)
                   MOVE    ORCSLASTYMD-BRMNUM
                                           TO  WRK-BIKOU(9:9)
      *            全角変換
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-BIKOU       TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF      KANACHK-RC     =   ZERO
                       MOVE    KANACHK-OUT-INPUT
                                           TO  HCM38-BIKO(CNT-LINE)
                   END-IF
               ELSE
                   MOVE    "*"         TO  HCM38-MARK(CNT-LINE)
                   COMPUTE CNT-TAIIN   =   CNT-TAIIN   +   1
               END-IF
           ELSE
               IF      ORCSLASTYMD-TAIINYMD    =   ORCSLASTYMD-LASTYMD
                   MOVE    "*"         TO  HCM38-MARK(CNT-LINE)
                   COMPUTE CNT-TAIIN   =   CNT-TAIIN   +   1
               END-IF
           END-IF
      *
           MOVE    ORCSLASTYMD-SRYKA   TO  WRK-SRYKA
      *
           .
       3201-BODY-LASTYMD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       3201-BODY-SRYKA-HEN-SEC     SECTION.
      *
      *    診療科読み込み
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA       TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
      *
      *    システム管理読み込み
           PERFORM 800-SYSKANRI-READ-SEC
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               GO  TO  3201-BODY-SRYKA-HEN-EXT
           END-IF
      *
      *    診療科名セット
           MOVE    SYS-1005-SRYKANAME1(1:8)
                                   TO  HCM38-SRYKA(CNT-LINE)
      *
           .
       3201-BODY-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終入金日編集処理
      *****************************************************************
       3201-BODY-LASTNYKNYMD-HEN-SEC   SECTION.
      *
      *    最終入金日を求める
           PERFORM 900-BD003-KEY3-SEL-SEC
           IF    ( FLG-BD003         =   ZERO )
               MOVE    BD003-NYUHEN-YMD
                                       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMD      TO  HCM38-LASTNYKNYMD(CNT-LINE)
           END-IF
      *
           .
       3201-BODY-LASTNYKNYMD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者氏名・自宅電話番号編集処理
      *****************************************************************
       3201-BODY-NAMETEL-HEN-SEC   SECTION.
      *
      *    患者氏名セット
           IF    ( PTINF-NAME (31:)    =   SPACE )
               MOVE    PTINF-NAME      TO  HCM38-NAME(CNT-LINE)
           ELSE
               MOVE    PTINF-NAME      TO  HCM38-NAME2(CNT-LINE)
           END-IF
      *
      *    半角全角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    PTINF-HOME-TEL1 TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC      NOT =   ZERO
               GO  TO  3201-BODY-NAMETEL-HEN-EXT
           END-IF
      *
      *    自宅電話番号セット
           IF      KANACHK-SYUBETU     =   1
               MOVE    PTINF-HOME-TEL1 TO  HCM38-HOME-TEL1(CNT-LINE)
           ELSE
               MOVE    PTINF-HOME-TEL1(1:14)
                                       TO  HCM38-HOME-TEL1(CNT-LINE)
           END-IF
      *
           .
       3201-BODY-NAMETEL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1               TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO            TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1               TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO            TO  FLG-TOKEI
      *            患者情報読み込み
                   PERFORM 900-PTINF-READ-SEC
      *
                   IF        ( FLG-PTINF       NOT =   ZERO )
                       OR    ( PTINF-TSTPTNUMKBN   =   "1" )
                       GO  TO  900-TOKEI-NEXT-SEC
                   END-IF
           END-READ
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスタより患者番号取得処理
      *****************************************************************
       20011-PTNUM-GET-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-PTNUM
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    BD003-PTID      TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                       ORCSPTIDAREA
                                       SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               INITIALIZE              ERR-EDIT-AREA
               MOVE    BD003-PTID      TO  ERR-PTID
               MOVE    "患者番号の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20011-PTNUM-HEN-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM     TO  WRK-PTNUM
      *
           .
       20011-PTNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC         SECTION.
      *
           MOVE    SPACE           TO  ERR-HEN-ERRMSG
      *
           IF      ERR-PTNUM       =   SPACE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    ERR-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM     TO  ERR-PTNUM
               ELSE
                   STRING  "?(ID:"         DELIMITED   BY  SIZE
                           ERR-PTID        DELIMITED   BY  SIZE
                           ")"             DELIMITED   BY  SIZE
                                    INTO   ERR-PTNUM
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    1               TO  WRK-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  ERR-PTNUM       DELIMITED   BY  SPACE
               INTO    WRK-STR     WITH    POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN         =   WRK-LEN -   1
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号："      DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           ELSE
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号:"       DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           END-IF
      *
           .
       800-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    PARA-YUKOSTRYM  TO  STOUKEICSV-SRYYM
      *     MOVE    WRK-PARA-NYUGAIKBN
      *                             TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "未収金一覧表ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG010 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号読み込み
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    TOKEI-PTID      TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTNUM
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
            .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読み込み
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    TOKEI-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
            .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金一覧用収納明細読み込み
      *****************************************************************
       900-BD003-SELECT-SEC        SECTION.
      *
           INITIALIZE                  BD003-REC
           MOVE    SPA-HOSPNUM     TO  BD003-HOSPNUM
           MOVE    WRK-YUKOSTRYMD  TO  BD003-STSRYYMD
           MOVE    WRK-YUKOENDYMD  TO  BD003-EDSRYYMD
      *
           IF      PARA-SYORIKBN   =   "1" OR "3"
               MOVE    "99999999"      TO  BD003-NYUHEN-YMD
           ELSE
               MOVE    WRK-YUKOENDYMD  TO  BD003-NYUHEN-YMD
           END-IF
      *
           MOVE    BD003-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC      =   ZERO
               PERFORM 900-BD003-FETCH-SEC
           ELSE
               INITIALIZE                  BD003-REC
               MOVE    1               TO  FLG-BD003
           END-IF
           .
       900-BD003-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金一覧用収納明細読み込み
      *****************************************************************
       900-BD003-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF          MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-BD003
               MOVE    MCPDATA-REC     TO  BD003-REC
           ELSE
               MOVE    1               TO  FLG-BD003
           END-IF
           .
       900-BD003-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金一覧用収納明細読み込み
      *****************************************************************
       900-BD003-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-BD003-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金一覧用収納明細読み込み
      *****************************************************************
       900-BD003-KEY3-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-BD003
      *
           IF    ( PARA-SYORIKBN   =   "1" OR "2" )
              MOVE    "view_bd003" TO  WRK-TABLE
              MOVE    "key3"       TO  WRK-PATHNAME
           ELSE
              MOVE    "view_bd003" TO  WRK-TABLE
              MOVE    "key7"       TO  WRK-PATHNAME
           END-IF
      *
           INITIALIZE                  BD003-REC
           MOVE    SPA-HOSPNUM     TO  BD003-HOSPNUM
           MOVE    TOKEI-PTID      TO  BD003-PTID
           IF      PARA-SYORIKBN   =   "1" OR "3"
               MOVE    "99999999"  TO  BD003-NYUHEN-YMD
           ELSE
               MOVE    WRK-YUKOENDYMD
                                   TO  BD003-NYUHEN-YMD
           END-IF
           MOVE    WRK-YUKOSTRYMD  TO  BD003-STSRYYMD
           MOVE    WRK-YUKOENDYMD  TO  BD003-EDSRYYMD 
           MOVE    BD003-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  BD003-REC
               ELSE
                   MOVE    1               TO  FLG-BD003
                   INITIALIZE                  BD003-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-BD003
               INITIALIZE                  BD003-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-BD003-KEY3-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
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
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
