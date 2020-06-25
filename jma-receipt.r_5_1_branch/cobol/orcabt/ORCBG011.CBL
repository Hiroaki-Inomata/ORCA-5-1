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
       PROGRAM-ID.             ORCBG011.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次処理
      *  コンポーネント名  : 未収金一覧表（伝票別）
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/10/03    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 01.00.01     NACL-森脇     04/01/05  番号を患者ごとにカウント
      * 01.00.02     NACL-森脇     04/01/06  期間表示変更
      * 01.00.03     NACL-森脇     04/10/22  CLOSE追加
      * 03.05.01     NACL-森脇     07/06/05  グループ診療対応
      *  04.08.01    NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    処理対象患者ファイル
           SELECT  BG01101-FILE    ASSIGN  BG01101PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BG01101-KEY
                                   FILE    STATUS  IS  STS-BG01101.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    処理対象患者ファイル
       FD  BG01101-FILE.
       01  BG01101-REC. 
           03  BG01101-KEY.
               05  BG01101-HOSPNUM         PIC 9(02).
               05  BG01101-NYUGAIKBN       PIC X(01).
               05  BG01101-PTNUM           PIC X(20).
               05  BG01101-DENPNUM         PIC 9(07).
           03  BG01101-PTID                PIC 9(10).
           03  BG01101-SKYMONEY            PIC S9(10).
           03  BG01101-NYUHEN-MONEY        PIC S9(10).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    処理対象患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //BG01101//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                      PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    印刷用領域
           COPY    "HCM39.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-BG01101CL               PIC X(02).
           03  STS-BG01101                 PIC X(02).
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-BD003                   PIC 9(01).
           03  FLG-BG01101                 PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-SKIP                    PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDXE                        PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(02).
           03  CNT-PAGE                    PIC 9(04).
           03  CNT-PTSYU                   PIC 9(04).
           03  CNT-KNS                     PIC 9(05).
           03  CNT-BG011                   PIC 9(05).
           03  CNT-KETA                    PIC 9(02).
           03  CNT-KETA2                   PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID              PIC 9(07).
           03  WRK-PARA-SHELLID            PIC X(08).
           03  WRK-PARA-HOSPNUM            PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR                     PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SITEIKBN           PIC X(01).
           03  WRK-PARA-YUKOSTRYM          PIC X(06).
           03  WRK-PARA-YUKOENDYM          PIC X(06).
           03  WRK-PARA-SYORIKBN           PIC X(01).
           03  WRK-PARA-NYUGAIKBN          PIC X(01).
           03  WRK-PARA-MISYUKBN           PIC X(01).
      *
      *    キー領域
       01  KEY-AREA.
           03  KEY-HOSPNUM                 PIC 9(02).
           03  KEY-PTNUM                   PIC X(20).
           03  KEY-NYUGAIKBN               PIC X(1).
           03  KEY-PTID                    PIC 9(10).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-HENYMD              PIC X(09).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-PTNUM                           PIC X(20).
           03  WRK-TITLE                           PIC X(100).
           03  WRK-STR                             PIC X(200).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05  WRK-STR-LEFT-FL                 PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
           03  WRK-SRYKALST-G.
               05  WRK-SRYKALST-MAX                PIC 9(05).
               05  WRK-SRYKALST-OCC                OCCURS 50.
                   07  WRK-SRYKAL                  PIC X(02).
                   07  WRK-SRYKAL-STYUKYMD         PIC X(08).
                   07  WRK-SRYKAL-EDYUKYMD         PIC X(08).
                   07  WRK-SRYKAMEIL               PIC X(20).
           03  WRK-KKN-HEN.
               05  WRK-KKNSTYMD                    PIC X(16).
               05  WRK-KKNMID                      PIC X(02).
               05  WRK-KKNEDYMD                    PIC X(16).
           03  WRK-NYU-SKYYMD-HEN.
               05  WRK-NYU-SKYSTYMD                PIC X(09).
               05  WRK-NYU-SKYYMD-MID              PIC X(01).
               05  WRK-NYU-SKYEDYMD                PIC X(09).
           03  WRK-ZZZZ9                           PIC ZZZZ9.
           03  WRK-PAGE                            PIC ZZ,ZZ9.
           03  WRK-SKYMONEYF                       PIC ---,---,--9.
           03  WRK-MISYUF                          PIC ---,---,--9.
           03  WRK-NYUKINF                         PIC ---,---,--9.
           03  WRK-MISYU                           PIC S9(10).
           03  WRK-SKYMONEY-TTL                    PIC S9(10).
           03  WRK-MISYU-TTL                       PIC S9(10).
           03  WRK-NYUKIN-TTL                      PIC S9(10).
      *
           03  WRK-SAKUSEIBI           PIC X(22).
           03  WRK-KIKAN               PIC X(40).
           03  WRK-MES                 PIC X(50).
           03  WRK-TOUKEIERR       PIC X(200).
           03  WRK-YUKOSTRYM           PIC X(16).
           03  WRK-YUKOENDYM           PIC X(16).
           03  WRK-YUKOSTRYMD          PIC X(08).
           03  WRK-YUKOENDYMD          PIC X(08).
           03  WRK-SRYSTRYMD          PIC X(10).
           03  WRK-SRYENDYMD          PIC X(10).
           03  WRK-HENSURYO        PIC X(10).
           03  WRK-MISYUCSV        PIC X(10).
           03  WRK-SKYCSV          PIC X(10).
           03  WRK-NYUHENCSV       PIC X(10).
           03  WRK-CSV             PIC ---------9.
           03  WRK-REC             PIC X(20000).
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
               05  FILLER              PIC X(17)   VALUE
                                       "misyuukin_denpyo_".
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
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "入外区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "伝票番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療科名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SRYYMD          PIC X(08)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "未収金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "請求金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "入金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "備考".
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "HCM39.INC"             REPLACING   //HCM39//
                                           BY          //WKHCM39//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1009.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    未収金一覧ビュー
       01  BD003-REC.
           COPY    "CPBD003.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           02  FILLER                  PIC X(1000).
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
           PERFORM UNTIL ( FLG-END NOT =   ZERO )
               PERFORM 200-MAIN-SEC
           END-PERFORM
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  ERR-EDIT-AREA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
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
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SITEIKBN
                                       WRK-PARA-YUKOSTRYM
                                       WRK-PARA-YUKOENDYM
                                       WRK-PARA-SYORIKBN
                                       WRK-PARA-NYUGAIKBN
                                       WRK-PARA-MISYUKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  WRK-TITLE
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
             OR  ( WRK-PARA-NYUGAIKBN  =   "2" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
           WHEN    "1"
               MOVE    "未収金一覧表−入院（伝票別）"
                                       TO  WRK-TITLE
           WHEN    "2"
               MOVE    "未収金一覧表−外来（伝票別）"
                                       TO  WRK-TITLE
           WHEN    OTHER
               MOVE    "未収金一覧表（伝票別）"
                                       TO  WRK-TITLE
           END-EVALUATE
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG011"          TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG01101"           TO  BG01101PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  BG01101PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  BG01101PARA-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BG01101PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BG01101PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメータチェック処理
           PERFORM 1001-PARA-CHK-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    患者番号構成情報編集処理
           PERFORM 1001-PTNUMINF-HEN-SEC
      *
      *    診療科ワーク編集処理
           PERFORM 1001-SRYKALST-HEN-SEC
      *
      *    ヘッダー編集処理
           PERFORM 1001-HEADER-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータチェック処理
      *****************************************************************
       1001-PARA-CHK-SEC               SECTION.
      *
      *    期間指定区分チェック
           EVALUATE    WRK-PARA-SITEIKBN 
           WHEN    "0"
               IF   (( WRK-PARA-YUKOSTRYM  NOT =   SPACE ) AND
                     ( WRK-PARA-YUKOSTRYM  NOT =   "000000" ))
               OR   (( WRK-PARA-YUKOENDYM  NOT =   SPACE ) AND
                     ( WRK-PARA-YUKOENDYM  NOT =   "999999" ))
                   MOVE
                   "期間指定なしの場合は期間を入力しないでください"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1001-PARA-CHK-EXT
               END-IF
               MOVE    "00000000"      TO  WRK-YUKOSTRYMD
               MOVE    "99999999"      TO  WRK-YUKOENDYMD
               MOVE    "1"             TO  WRK-PARA-SYORIKBN
           WHEN    "1"
      *        期間開始年月チェック
               PERFORM 1001-KKNSTYM-CHK-SEC
               IF    ( FLG-END         NOT =   ZERO )
                   GO  TO  1001-PARA-CHK-EXT
               END-IF
      *
      *        期間終了年月チェック
               PERFORM 1001-KKNEDYM-CHK-SEC
               IF    ( FLG-END         NOT =   ZERO )
                   GO  TO  1001-PARA-CHK-EXT
               END-IF
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "期間指定区分が誤りです［"
                                                   DELIMITED   BY  SIZE
                       WRK-PARA-SITEIKBN          DELIMITED   BY  SIZE
                       "］"                        DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1001-PARA-CHK-EXT
           END-EVALUATE
      *
      *    期間範囲チェック
           PERFORM 1201-PARA-KIKAN-CHECK-SEC
           IF      FLG-END         NOT =   ZERO
               MOVE    "期間を正しく入力してください"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1001-PARA-CHK-EXT
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
      *
           .
       1001-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間開始年月チェック
      *****************************************************************
       1001-KKNSTYM-CHK-SEC            SECTION.
      *
      *    期間開始年月チェック
           EVALUATE    WRK-PARA-YUKOSTRYM
           WHEN    SPACE
               MOVE    "開始年月が未設定です"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           WHEN    "000000"
               MOVE    "00000000"      TO  WRK-YUKOSTRYMD
           WHEN    OTHER
               MOVE    WRK-PARA-YUKOSTRYM  TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               IF          STS-DAY-RC1     =   ZERO
                   MOVE    WRK-HENYMDG(1:16)
                                           TO  WRK-YUKOSTRYM
                   MOVE    WRK-SYMD        TO  WRK-YUKOSTRYMD
               ELSE
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "開始年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       WRK-PARA-YUKOSTRYM        DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-EVALUATE
      *
           .
       1001-KKNSTYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間終了年月チェック
      *****************************************************************
       1001-KKNEDYM-CHK-SEC            SECTION.
      *
      *    期間終了年月チェック
           EVALUATE    WRK-PARA-YUKOENDYM
           WHEN    "999999"
               MOVE    "99999999"        TO  WRK-YUKOENDYMD
           WHEN    OTHER
      *        月末日取得
               IF          WRK-PARA-YUKOENDYM  =   SPACE
                   MOVE    WRK-PARA-YUKOSTRYM  TO  WRK-SYMD(1:6)
               ELSE
                   MOVE    WRK-PARA-YUKOENDYM  TO  WRK-SYMD(1:6)
               END-IF
               PERFORM 20021-GETUMATU-GET-SEC
               IF          STS-DAY-RC1     NOT =   ZERO
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "終了年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       WRK-PARA-YUKOENDYM      DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1001-KKNEDYM-CHK-EXT
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
                       WRK-PARA-YUKOENDYM      DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1001-KKNEDYM-CHK-EXT
               END-IF
           END-EVALUATE
      *
           IF      WRK-YUKOSTRYMD  <=   WRK-YUKOENDYMD
               CONTINUE
           ELSE
               MOVE    "開始年月＞終了年月となっています"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1001-KKNEDYM-CHK-EXT
           END-IF
      *
           .
       1001-KKNEDYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間範囲チェック処理
      *****************************************************************
       1201-PARA-KIKAN-CHECK-SEC   SECTION.
      *
           DISPLAY "WRK-YUKOSTRYM=" WRK-YUKOSTRYMD(1:6)
           DISPLAY "WRK-YUKOENDYM=" WRK-YUKOENDYMD(1:6)
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
           EVALUATE    WRK-PARA-SYORIKBN
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
                       WRK-PARA-SYORIKBN           DELIMITED   BY  SIZE
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
      *************************************************************
      *    患者番号構成情報編集処理
      *************************************************************
       1001-PTNUMINF-HEN-SEC           SECTION.
      *
      *    患者番号構成管理情報編集
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       1001-PTNUMINF-HEN-EXT.
           EXIT.
      *************************************************************
      *    診療科ワーク編集処理
      *************************************************************
       1001-SRYKALST-HEN-SEC           SECTION.
      *
           INITIALIZE  WRK-SRYKALST-G
      *
           INITIALIZE  SYS-1005-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( WRK-SRYKALST-MAX   >=   50   )
      *
               COMPUTE WRK-SRYKALST-MAX
                   =   WRK-SRYKALST-MAX    +   1
      *
               MOVE    WRK-SRYKALST-MAX    TO  IDX1
      *
               MOVE    SYS-1005-KBNCD      TO  WRK-SRYKAL    (IDX1)
               MOVE    SYS-1005-STYUKYMD   TO  WRK-SRYKAL-STYUKYMD
                                                             (IDX1)
               MOVE    SYS-1005-EDYUKYMD   TO  WRK-SRYKAL-EDYUKYMD
                                                             (IDX1)
               MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKAMEIL (IDX1)
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC        NOT =   ZERO )
                   MOVE    1               TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       1001-SRYKALST-HEN-EXT.
           EXIT.
      *************************************************************
      *    ヘッダー編集処理
      *************************************************************
       1001-HEADER-HEN-SEC             SECTION.
      *
           INITIALIZE                      WKHCM39
      *
      *    タイトル
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "請求期間"      TO  WKHCM39-DAI
                                           CSV-SRYYMD
           ELSE
               MOVE    "診療日"        TO  WKHCM39-DAI
                                           CSV-SRYYMD
           END-IF
      *
      *    作成日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WKHCM39-SAKUSEIBI
      *
      *    期間
           MOVE    WRK-KIKAN           TO  WKHCM39-KIKAN
      *
      *    処理区分
           MOVE    WRK-MES             TO  WKHCM39-MES
      *
           .
       1001-HEADER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    未収一覧ビューより抽出処理
           PERFORM 2001-BD003-HEN-SEC
      *
           IF      CNT-BG011       NOT =   ZERO
      *        ＣＳＶ出力処理
               INITIALIZE                  WRK-REC
               MOVE    CSV-HEAD-01     TO  WRK-REC
               PERFORM 500-TOUKEICSV-SEC
           END-IF
           IF      CNT-BG011       NOT =   ZERO
      *        未収患者一覧編集処理
               PERFORM 2001-MISYU-HEN-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収一覧ビューより抽出処理
      *****************************************************************
       2001-BD003-HEN-SEC              SECTION.
      *
           MOVE    ZERO                    TO  CNT-BG011
      *
      *    処理対象患者ファイル初期化
           OPEN    OUTPUT                  BG01101-FILE
           CLOSE   BG01101-FILE
      *
           OPEN    OUTPUT                  BG01101-FILE
      *
           EVALUATE    WRK-PARA-SYORIKBN
           WHEN  "3"
           WHEN  "4"
               MOVE    "view_bd003"    TO  WRK-TABLE
               MOVE    "key6"          TO  WRK-PATHNAME
           WHEN  OTHER
               MOVE    "view_bd003"    TO  WRK-TABLE
               MOVE    "key2"          TO  WRK-PATHNAME
           END-EVALUATE
      *
           PERFORM 20011-BD003-SEL-SEC
      *
           PERFORM UNTIL ( FLG-BD003   NOT =   ZERO )
      *
               IF    ( WRK-PARA-MISYUKBN   =   "1" )
                   IF    ( BD003-SKYMONEY  >   BD003-NYUHEN-MONEY )
                       PERFORM 2101-TOKEI-SET-SEIKYU-SEC
                   END-IF
               ELSE
                   PERFORM 2101-TOKEI-SET-SEIKYU-SEC
               END-IF
      *
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1               TO  FLG-BD003
               ELSE
                   MOVE    MCPDATA-REC     TO  BD003-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           CLOSE   BG01101-FILE
      *
           .
       2001-BD003-HEN-EXT.
           EXIT.
      *****************************************************************
      *    未収一覧ビュー検索処理
      *****************************************************************
       20011-BD003-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-BD003
      *
           INITIALIZE                      BD003-REC
           MOVE    SPA-HOSPNUM         TO  BD003-HOSPNUM
           IF    ( WRK-PARA-NYUGAIKBN  =   "0" )
               MOVE    "%"             TO  BD003-NYUGAIKBN
           ELSE
               MOVE    WRK-PARA-NYUGAIKBN
                                       TO  BD003-NYUGAIKBN
           END-IF
           MOVE    WRK-YUKOSTRYMD      TO  BD003-STSRYYMD
           MOVE    WRK-YUKOENDYMD      TO  BD003-EDSRYYMD
      *
           IF    ( WRK-PARA-SYORIKBN     =   "1" OR "3" )
               MOVE    "99999999"          TO  BD003-NYUHEN-YMD
           ELSE
               MOVE    BD003-EDSRYYMD      TO  BD003-NYUHEN-YMD
           END-IF
      *
           MOVE    BD003-REC               TO  MCPDATA-REC
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1                   TO  FLG-BD003
           ELSE
               MOVE    MCPDATA-REC         TO  BD003-REC
           END-IF
      *
           .
       20011-BD003-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイルセット
      *****************************************************************
       2101-TOKEI-SET-SEIKYU-SEC    SECTION.
      *
           INITIALIZE  BG01101-REC
      *
           MOVE    SPA-HOSPNUM         TO  BG01101-HOSPNUM
           MOVE    BD003-NYUGAIKBN     TO  BG01101-NYUGAIKBN
           MOVE    BD003-DENPNUM       TO  BG01101-DENPNUM
           MOVE    BD003-PTID          TO  BG01101-PTID
           MOVE    BD003-SKYMONEY      TO  BG01101-SKYMONEY
           MOVE    BD003-NYUHEN-MONEY  TO  BG01101-NYUHEN-MONEY
      *
      *    患者番号取得
           PERFORM 20011-PTNUM-GET-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  2101-TOKEI-SET-SEIKYU-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  BG01101-PTNUM
           WRITE   BG01101-REC
      *
           COMPUTE CNT-BG011   =   CNT-BG011   +   1
      *
           .
       2101-TOKEI-SET-SEIKYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスタより患者番号取得処理
      *****************************************************************
       20011-PTNUM-GET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-PTNUM
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    BD003-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF    ( SPTID-RC        NOT =   ZERO )
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
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
      *
           .
       20011-PTNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    未収金一覧表編集処理
      *****************************************************************
       2001-MISYU-HEN-SEC              SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
      *
           MOVE    ZERO            TO  FLG-BG01101
           OPEN    INPUT               BG01101-FILE
      *
      *    処理対象患者ファイル読み込み処理
           PERFORM 20011-BG01101-REAC-SEC
      *
           PERFORM UNTIL ( FLG-BG01101 NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               MOVE    BG01101-NYUGAIKBN
                                       TO  KEY-NYUGAIKBN
      *
               MOVE    LOW-VALUE       TO  KEY-HOSPNUM
                                           KEY-PTNUM
               MOVE    ZERO            TO  KEY-PTID
      *
               MOVE    ZERO            TO  CNT-PTSYU
                                           CNT-KNS
      *
               MOVE    ZERO            TO  WRK-SKYMONEY-TTL
                                           WRK-MISYU-TTL
                                           WRK-NYUKIN-TTL
      *
               MOVE    ZERO            TO  FLG-SKIP
      *
               PERFORM UNTIL ( FLG-BG01101 NOT =   ZERO )
                        OR   ( FLG-END     NOT =   ZERO )
                        OR   ( BG01101-NYUGAIKBN
                                           NOT =   KEY-NYUGAIKBN )
      *
                   INITIALIZE                  HCM39
                                               CNT-LINE
      *
                   IF    ( BG01101-NYUGAIKBN   =   "1" )
                       MOVE    "　未収金一覧表−入院（伝票別）　"
                                       TO  WKHCM39-TITLE
                   ELSE
                       MOVE    "　未収金一覧表−外来（伝票別）　"
                                       TO  WKHCM39-TITLE
                   END-IF
      *
                   PERFORM UNTIL ( FLG-BG01101 NOT =   ZERO )
                            OR   ( FLG-END     NOT =   ZERO )
                            OR   ( BG01101-NYUGAIKBN
                                               NOT =   KEY-NYUGAIKBN )
                            OR   ( CNT-LINE       >=   30 )
      *
                       IF    ( BG01101-HOSPNUM     =   KEY-HOSPNUM )
                        AND  ( BG01101-PTNUM       =   KEY-PTNUM  )
                           CONTINUE
                       ELSE
                           MOVE    BG01101-HOSPNUM TO  KEY-HOSPNUM
                           MOVE    BG01101-PTNUM   TO  KEY-PTNUM
                           MOVE    BG01101-PTID    TO  KEY-PTID
                           MOVE    ZERO            TO  CNT-PTSYU
                           PERFORM 20011-PTINF-HEN-SEC
                       END-IF
      *
                       IF    ( FLG-SKIP            =   ZERO )
      *
                           PERFORM 20011-MISYU-HEN-SEC
                       END-IF
      *
      *                処理対象患者ファイル読み込み処理
                       PERFORM 20011-BG01101-REAC-SEC
      *
                   END-PERFORM
      *
                   PERFORM 20011-PRINT-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       2001-MISYU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル読み込み処理
      *****************************************************************
       20011-BG01101-REAC-SEC          SECTION.
      *
           READ    BG01101-FILE    NEXT
           AT  END
               MOVE    1           TO  FLG-BG01101
           NOT AT  END
               MOVE    ZERO        TO  FLG-BG01101
           END-READ
      *
           .
       20011-BG01101-REAC-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20011-PTINF-HEN-SEC             SECTION.
      *
           INITIALIZE                      PTINF-REC
      *
           MOVE    KEY-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    KEY-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    SPACE           TO  PTINF-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( PTINF-TSTPTNUMKBN   =   1 )
               MOVE    1               TO  FLG-SKIP
           ELSE
               MOVE    ZERO            TO  FLG-SKIP
           END-IF
      *
           INITIALIZE                      WKHCM39-MEISAI (1)
      *
           IF    ( BG01101-PTNUM (11:) =   SPACE )
               MOVE    BG01101-PTNUM   TO  WKHCM39-PTNUM  (1)
           ELSE
               MOVE    BG01101-PTNUM (1:10)
                                       TO  WKHCM39-PTNUMUP(1)
               MOVE    BG01101-PTNUM (11:)
                                       TO  WKHCM39-PTNUMDN(1)
           END-IF
      *
           IF    ( PTINF-NAME (21:)    =   SPACE )
               MOVE    PTINF-NAME      TO  WKHCM39-NAME   (1)
           ELSE
               MOVE    PTINF-NAME      TO  WKHCM39-NAME2  (1)
           END-IF
      *
           .
       20011-PTINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    未収金一覧編集処理
      *****************************************************************
       20011-MISYU-HEN-SEC                 SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           COMPUTE CNT-PTSYU   =   CNT-PTSYU   +   1
      *
      *    収納マスター検索
           PERFORM 200111-SYUNOU-SEL-SEC
      *
      *    番号
           IF      CNT-PTSYU           =   1
               COMPUTE CNT-KNS     =   CNT-KNS     +   1
               MOVE    CNT-KNS                 TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9               TO  HCM39-CD  (CNT-LINE)
           END-IF
      *
      *    患者情報（患者番号・氏名・電話番号（自宅））
      *    １件目か、改ページ後１行目のみ編集
           IF    ( CNT-LINE            =   1 )
            OR   ( CNT-PTSYU           =   1 )
      *
               MOVE    WKHCM39-PTNUM  (1)  TO  HCM39-PTNUM  (CNT-LINE)
               MOVE    WKHCM39-PTNUMUP(1)  TO  HCM39-PTNUMUP(CNT-LINE)
               MOVE    WKHCM39-PTNUMDN(1)  TO  HCM39-PTNUMDN(CNT-LINE)
               MOVE    WKHCM39-NAME   (1)  TO  HCM39-NAME   (CNT-LINE)
               MOVE    WKHCM39-NAME2  (1)  TO  HCM39-NAME2  (CNT-LINE)
      *
           END-IF
      *
      *    伝票番号
           MOVE    BG01101-DENPNUM         TO  HCM39-DENPNUM(CNT-LINE)
      *
      *    診療科
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       WRK-SRYKALST-MAX )
      *
               IF    ( SYU-SRYKA       =   WRK-SRYKAL (IDX1) )
                AND  ( SYU-SRYYMD     >=   WRK-SRYKAL-STYUKYMD (IDX1) )
                AND  ( SYU-SRYYMD     <=   WRK-SRYKAL-EDYUKYMD (IDX1) )
                   MOVE    WRK-SRYKAMEIL (IDX1)
                                           TO  HCM39-SRYKA (CNT-LINE)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-SRYSTRYMD
                                           WRK-SRYENDYMD
      *    診療日
           IF    ( SYU-NYUGAIKBN       =   "1" )
      *
               INITIALIZE                      WRK-NYU-SKYYMD-HEN
      *
               MOVE    SYU-SKYSTYMD        TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMD         TO  WRK-NYU-SKYSTYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-SRYSTRYMD
      *
               MOVE    "-"                 TO  WRK-NYU-SKYYMD-MID
      *
               MOVE    SYU-SKYEDYMD        TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMD         TO  WRK-NYU-SKYEDYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-SRYENDYMD
      *
               MOVE    WRK-NYU-SKYYMD-HEN  TO  HCM39-SRYYMDN (CNT-LINE)
      *
           ELSE
               MOVE    SYU-SRYYMD          TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMD         TO  HCM39-SRYYMDG (CNT-LINE)
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-SRYSTRYMD
           END-IF
      *    未収額
           COMPUTE WRK-MISYU   =   BG01101-SKYMONEY
                               -   BG01101-NYUHEN-MONEY
           MOVE    WRK-MISYU               TO  WRK-MISYUF
           MOVE    WRK-MISYUF              TO  HCM39-MISYU  (CNT-LINE)
           MOVE    WRK-MISYU       TO  WRK-CSV
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO    TO  WRK-MISYUCSV
      *
      *    請求額
           MOVE    BG01101-SKYMONEY        TO  WRK-SKYMONEYF
           MOVE    WRK-SKYMONEYF           TO  HCM39-SEIKYU (CNT-LINE)
           MOVE    BG01101-SKYMONEY        TO  WRK-CSV
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO    TO  WRK-SKYCSV
      *
      *    請求額
           MOVE    BG01101-NYUHEN-MONEY    TO  WRK-NYUKINF
           MOVE    WRK-NYUKINF             TO  HCM39-NYUKIN (CNT-LINE)
           MOVE    BG01101-NYUHEN-MONEY    TO  WRK-CSV
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO    TO  WRK-NYUHENCSV
      *
      *    備考
           IF    ( SYU-NYUGAIKBN       =   "1" )
               EVALUATE    SYU-CREATEKBN
               WHEN    "1"
                   MOVE    "退院時請求"    TO  HCM39-BIKO (CNT-LINE)
               WHEN    "2"
                   MOVE    "定期請求"      TO  HCM39-BIKO (CNT-LINE)
               END-EVALUATE
           END-IF
      *
      *
           COMPUTE WRK-MISYU-TTL       =   WRK-MISYU-TTL
                                       +   WRK-MISYU
      *
           COMPUTE WRK-SKYMONEY-TTL    =   WRK-SKYMONEY-TTL
                                       +   BG01101-SKYMONEY
      *
           COMPUTE WRK-NYUKIN-TTL      =   WRK-NYUKIN-TTL
                                       +   BG01101-NYUHEN-MONEY
      *
           INITIALIZE                      WRK-REC
      *
           IF    ( SYU-NYUGAIKBN       =   "1" )
               STRING  SYU-NYUGAIKBN           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       "入"                    DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       BG01101-PTNUM           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       PTINF-NAME              DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       PTINF-KANANAME          DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       BG01101-DENPNUM         DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       HCM39-SRYKA(CNT-LINE)   DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-SRYSTRYMD          DELIMITED  BY  SPACE
                       "-"                     DELIMITED  BY  SIZE
                       WRK-SRYENDYMD          DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-MISYUCSV            DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-SKYCSV              DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-NYUHENCSV           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       HCM39-BIKO(CNT-LINE)    DELIMITED  BY  SPACE
                       INTO                    WRK-REC
               END-STRING
           ELSE
               STRING  SYU-NYUGAIKBN           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       "外"                    DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       BG01101-PTNUM           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       PTINF-NAME              DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       PTINF-KANANAME          DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       BG01101-DENPNUM         DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       HCM39-SRYKA(CNT-LINE)   DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-SRYSTRYMD          DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-MISYUCSV            DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-SKYCSV              DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-NYUHENCSV           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       HCM39-BIKO(CNT-LINE)    DELIMITED  BY  SPACE
                       INTO                    WRK-REC
               END-STRING
           END-IF
      *
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       20011-MISYU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター検索処理
      *****************************************************************
       200111-SYUNOU-SEL-SEC       SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    BG01101-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    BG01101-PTID        TO  SYU-PTID
           MOVE    BG01101-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE                  SYUNOU-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-TBL-CLOSE-SEC
           .
      *
       200111-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル削除処理
      *****************************************************************
       2101-BG01101-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BG01101PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       2101-BG01101-DEL-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       20011-PRINT-SEC             SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
      *    期間
           MOVE    WKHCM39-KIKAN       TO  HCM39-KIKAN
      *    表題
           MOVE    WKHCM39-TITLE       TO  HCM39-TITLE
      *    作成日
           MOVE    WKHCM39-SAKUSEIBI   TO  HCM39-SAKUSEIBI
      *    ページ
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HCM39-PAGE
      *    期間処理内容
           MOVE    WKHCM39-MES         TO  HCM39-MES
      *    ラベル設定
           MOVE    WKHCM39-DAI         TO  HCM39-DAI
      *
           IF    ( FLG-BG01101     NOT =   ZERO )
            OR   ( BG01101-NYUGAIKBN
                                   NOT =   KEY-NYUGAIKBN )
               MOVE    "合計"          TO  HCM39-GTTL
               MOVE    WRK-MISYU-TTL   TO  WRK-MISYUF
               MOVE    WRK-MISYUF      TO  HCM39-MISYUG
               MOVE    WRK-MISYU-TTL   TO  WRK-CSV
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-MISYUCSV
      *
               MOVE    WRK-SKYMONEY-TTL
                                       TO  WRK-SKYMONEYF
               MOVE    WRK-SKYMONEYF   TO  HCM39-SEIKYUG
               MOVE    WRK-SKYMONEY-TTL    TO  WRK-CSV
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-SKYCSV
      *
               MOVE    WRK-NYUKIN-TTL  TO  WRK-NYUKINF
               MOVE    WRK-NYUKINF     TO  HCM39-NYUKING
               MOVE    WRK-NYUKIN-TTL  TO  WRK-CSV
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-NYUHENCSV
      *
               INITIALIZE                      WRK-REC
               STRING  ",,,,,,,合計,"          DELIMITED  BY  SIZE
                       WRK-MISYUCSV            DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-SKYCSV              DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       WRK-NYUHENCSV           DELIMITED  BY  SPACE
                       ","                     DELIMITED  BY  SIZE
                       INTO                    WRK-REC
               END-STRING
               PERFORM 500-TOUKEICSV-SEC
           END-IF
      *
           PERFORM 250-PRINT-SEC
           IF    ( FLG-END NOT =   ZERO )
               GO  TO  20011-PRINT-EXT
           END-IF
      *
           INITIALIZE                      HCM39
           MOVE    ZERO                TO  CNT-LINE
      *
           .
       20011-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       250-PRINT-SEC               SECTION.
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
           MOVE    "HCM39.red"         TO  SPRT-PRTID
           MOVE    WRK-TITLE           TO  SPRT-TITLE
           MOVE    HCM39               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       250-PRINT-EXT.
           EXIT.
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
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
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
           MOVE    WRK-PARA-YUKOSTRYM  TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "未収金一覧表（伝票別）ＣＳＶ作成"
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
           STRING  "ORCBG011"      DELIMITED  BY  SIZE
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 2101-BG01101-DEL-SEC
      *
           IF    ( CNT-KNS         =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG011 IN " CNT-BG011  " ***"
           DISPLAY "*** ORCBG011 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
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
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC  SECTION.
      *
           MOVE    WRK-STR-LEFT    TO  IDXE
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                           TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                           TO  WRK-STR-LEFT
                   MOVE    500             TO  IDXE
               END-IF
      *
           END-PERFORM
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
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
       31012-SEIWA-HEN-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMD
                                       WRK-HENYMDG
      *    日付画面チェックサブ
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF        ( STS-DAY-RC1     =   ZERO )
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
               INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC         SECTION.
      *
           MOVE    SPACE           TO  ERR-HEN-ERRMSG
      *
           IF    ( ERR-PTNUM       =   SPACE )
               INITIALIZE              ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    ERR-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
               IF    ( SPTID-RC            =   ZERO )
                   MOVE    SPTID-PTNUM TO  ERR-PTNUM
               ELSE
                   STRING "?(ID:"      DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
                   INTO   ERR-PTNUM
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    1               TO  WRK-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
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
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    MCP-TABLE     TO  MCP-TABLE
           MOVE    MCP-PATHNAME      TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               MOVE    MCP-TABLE     TO  MCP-TABLE
               MOVE    MCP-PATHNAME      TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF        ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-TBL-CLOSE-EXT.
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
      *
