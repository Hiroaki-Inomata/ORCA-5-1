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
       PROGRAM-ID.             A00000C117.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : (地区別)入院患者数集計表
      *  管理者            :
      *  作成日付    作業者       記述
      *  04/02/24    @@@@-有村    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/10/18  システム管理のDB検索処理を変更(key→key10)
      *  01.00.03    @@@@-田中    12/01/18  CSVファイル作成機能追加
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    門間         16/02/01  一時ディレクトリ対応(エラーファイル)
      *  02.00.03    門間         16/07/20  CSVファイル作成機能修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
           SELECT  POST-FILE     ASSIGN FILE-NAME
                                 ORGANIZATION   IS  LINE SEQUENTIAL
                                 ACCESS MODE    IS  SEQUENTIAL
                                 FILE   STATUS  IS  STS-POST.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                PIC X(200).
      *
      *    地区名,郵便番号登録ファイル
       FD  POST-FILE.
       01  POST-REC.
           03  POST-TIKUNAME          PIC X(20).
           03  FILLER                 PIC X(01).
           03  POST-NUM-TBL           OCCURS 50.
             05  POST-NUM             PIC X(07).
             05  FILLER               PIC X(01).
      *
       WORKING-STORAGE              SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000C100N.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR            PIC X(02).
           03  STS-POST               PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKA-ALL          PIC 9(01).
           03  FLG-GET                PIC 9(01).
           03  FLG-END                PIC 9(01).
           03  FLG-SYSKANRI           PIC 9(01).
           03  FLG-SYSKANRI2          PIC 9(01).
           03  FLG-SYSKANRI3          PIC 9(01).
           03  FLG-INPUT              PIC 9(01).
           03  FLG-HKNCOMBI           PIC 9(01).
           03  FLG-PTINF              PIC 9(01).
           03  FLG-PTHKNINF           PIC 9(01).
           03  FLG-SYUMEI             PIC 9(01).
           03  FLG-SYUNOU             PIC 9(01).
           03  FLG-SRYACCT            PIC 9(01).
           03  FLG-SRYACT             PIC 9(01).
           03  FLG-JYURRK             PIC 9(01).
           03  FLG-HKNNUM             PIC 9(01).
           03  FLG-PTNYUINRRK         PIC 9(01).
           03  FLG-PTNYUINRRK-M       PIC 9(01).
           03  FLG-PTNYUINRRK-M2      PIC 9(01).
           03  INP-STS                PIC 9(04).
           03  FLG-11DUMMY            PIC 9(01).
           03  FLG-12DUMMY            PIC 9(01).
           03  FLG-ZAIIN              PIC 9(01).
           03  FLG-OK                 PIC 9(01).
           03  FLG-NG                 PIC 9(01).
           03  FLG-INI                PIC 9(01).
           03  FLG-PRGOPTION          PIC 9(01).
           03  FLG-DATEKBN            PIC 9(01).
           03  FLG-DATEKBN2           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE              PIC 9(02).
           03  CNT-PAGE              PIC 9(06).
           03  CNT-DAY               PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
             05  SYS-YY              PIC 9(02).
             05  SYS-MM              PIC 9(02).
             05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  SELKA-IDX             PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDX4                  PIC 9(04).
           03  IDX5                  PIC 9(04).
           03  IDX6                  PIC 9(04).
           03  IDX7G.
             05  IDX7                PIC 9(02).
           03  IDX8G.
             05  IDX8                PIC 9(02).
           03  DIDXG.
             05  DIDX                PIC 9(02).
           03  IDX9                  PIC 9(04).
           03  IDAG.
             05  IDAY                PIC 9(04).
             05  IDA1                PIC 9(02).
             05  IDA2                PIC 9(02).
           03  IDBG.
             05  IDBY                PIC 9(04).
             05  IDB1                PIC 9(02).
             05  IDB2                PIC 9(02).
           03  IDX                   PIC 9(04).
           03  IDY                   PIC 9(04).
           03  IDZ                   PIC 9(04).
           03  IDX-S                 PIC 9(07).
           03  IDX-N                 PIC 9(07).
           03  IDX-MAX               PIC 9(07).
      *
           03  JIHIIDX               PIC 9(04).
           03  NG                    PIC 9(04).
           03  IDX12                 PIC 9(03).
           03  IDX13                 PIC 9(02).
           03  IDX14                 PIC 9(05).
           03  IDX-LINE              PIC 9(05).
           03  IDX-CHIKU             PIC 9(02).
           03  IDX-ST                PIC 9(03).
           03  IDX-ED                PIC 9(03).
           03  IDX-LEN               PIC 9(03).
           03  IDXP                  PIC 9(05).
      *
       01  WRK-POST-AREA.
           03  WRK-POST-TBL OCCURS 52.
             05  WRK-POST-TIKUNAME   PIC X(20).
             05  FILLER              PIC X(01).
             05  WRK-POST-NUM-TBL    OCCURS 50.
               07  WRK-POST-NUM      PIC X(07).
               07  FILLER            PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
           03  WRK-RECEERR           PIC X(200).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-YMD1WH            PIC X(22).
           03  WRK-YMD2WH            PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
      *
           03  WRK-SYSYMD.
             05  WRK-SYSYY           PIC 9(04).
             05  WRK-SYSMM           PIC 9(02).
             05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYMD.
             05  WRK-SYY             PIC 9(04).
             05  WRK-SMM             PIC 9(02).
             05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-SYUEKI            PIC 9(08).
      *    ファイル名称用エリア
           03  FILE-NAME             PIC X(512).
           03  WRK-POST              PIC X(07).
      *
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-TJIHI-G           OCCURS 5.
             05  WRK-TJIHI           PIC X(08).
             05  WRK-TJIHI-FLG       PIC 9(01).
           03  WRK-TJIHIN-G          OCCURS 5.
             05  WRK-TJIHIN          PIC X(08).
             05  WRK-TJIHIN-FLG      PIC 9(01).
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-S9-3              PIC --9.
           03  WRK-Z9-5              PIC ZZ,ZZ9.
           03  WRK-Z9-5-2            PIC ZZZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZZZ.
           03  WRK-S9                PIC -----------9.
           03  WRK-SZ                PIC -----------Z.
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-AGE               PIC 9(04).
           03  WRK-ZENSRYYM          PIC X(06).
           03  WRK-NENREI-TBL1.
             05  WRK-NENREIY1        PIC 9(04).
             05  WRK-NENREIM1        PIC 9(02).
           03  WRK-NENREI-TBL2.
             05  WRK-NENREIY2        PIC 9(04).
             05  WRK-NENREIM2        PIC 9(02).
           03  WRK-STYMD             PIC X(08).
           03  WRK-EDYMD             PIC X(08).
           03  WRK-LASTYMD.
             05  WRK-LASTYY          PIC 9(04).
             05  WRK-LASTMM          PIC 9(02).
             05  WRK-LASTDD          PIC 9(02).
           03  WRK-SRYYMD            PIC X(08).
           03  WRK-SRYYM             PIC X(06).
           03  DEBUG-CNT             PIC 9(03).
           03  WRK-PR-SRYYM          PIC X(06).
           03  WRK-TMP-YMD.
             05  WRK-TMP-YY          PIC 9(04).
             05  WRK-TMP-MM          PIC 9(02).
             05  WRK-TMP-DD          PIC 9(02).
           03  WRK-TMP-LYMD.
             05  WRK-TMP-LYY         PIC 9(04).
             05  WRK-TMP-LMM         PIC 9(02).
             05  WRK-TMP-LDD         PIC 9(02).
           03  SUM-GZAIIN-G          OCCURS 1000.
             05  SUM-GZAIIN          PIC S9(05).
             05  WRK-S9-5            PIC S9(05).
           03  WRK-OPT-SIZE          PIC 9(05).
           03  WRK-LINE              PIC X(44000).
           03  WRK-DATA              PIC X(430).
      *
       01  SUM-AREA                  VALUE ZERO.
           03  SUM-SRYKAMAX          PIC 9(03).
           03  SUM-SRYYMDIMAX        PIC 9(05).
           03  SUM-SRYYMD-TBL        OCCURS 1000.
             05  SUM-SRYYMD          PIC X(08).
           03  SUM-SRYKA-TBL         OCCURS 100.
             05  SUM-SRYKA           PIC X(02).
             05  SUM-SRYKANAME       PIC X(20).
             05  SUM-SRYKA-ZAN       PIC S9(05).
             05  SUM-ZENZAN-G.
               07  SUM-ZENZAN-TBL    OCCURS 1000.
                 09  SUM-ZENZAN      PIC S9(05).
             05  SUM-NYUIN-G.
               07  SUM-NYUIN-TBL     OCCURS 1000.
                 09  SUM-NYUIN       PIC S9(05).
             05  SUM-TAIIN-G.
               07  SUM-TAIIN-TBL     OCCURS 1000.
                 09  SUM-TAIIN       PIC S9(05).
             05  SUM-TOUZAN-G.
               07  SUM-TOUZAN-TBL    OCCURS 1000.
                 09  SUM-TOUZAN      PIC S9(05).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM      PIC X(03)  VALUE "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM    PIC X(03)  VALUE "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM     PIC X(03)  VALUE "973".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-TIKU-REC          OCCURS 50.
               05  PARA-TIKUNAME      PIC X(20).
               05  FILLER             PIC X(01).
               05  PARA-TIKUNUM       PIC X(400).
      *
      *    CSV領域
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE          PIC 9(03)  VALUE 100.
           03  WRK-ITEM               PIC X(100)  VALUE SPACE.
           03  WRK-CSVDATA            PIC X(20000)  VALUE SPACE.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    自費管理情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災情報取得
           COPY    "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為情報
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    プログラムオプション追加
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
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
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000C100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000C100-LNK.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           IF  INP-STS  =  ZERO
      *    正常主処理
             PERFORM  200-MAIN-SEC
           ELSE
      *    エラー処理
             PERFORM  210-ERR-SEC
           END-IF
      *
           PERFORM  300-END-SEC
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SPA-AREA
      *
           PERFORM  100-DBOPEN-SEC
           MOVE    C100LNK-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO  SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
           MOVE    C100LNK-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    C100LNK-JOBID      TO  JOB-JOBID
           MOVE    C100LNK-SHELLID    TO  JOB-SHELLID
           MOVE    "A00000C117"       TO  JOB-PGID
           MOVE    "(地区別)入院患者数集計表"
                                      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
           INITIALIZE                     SGETTEMP-AREA
           MOVE    RECEERR                TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"          USING  SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)  TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付取得／和暦変換
           MOVE    C100LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH
      *
      *    集計対象期間開始日取得／和暦変換
           MOVE    C100LNK-YMD1        TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YMD1WH
      *
      *    集計対象期間開始日取得／和暦変換
           MOVE    C100LNK-YMD2        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YMD2WH
      *
      *    画面入力値のチェック
      *    集計区分のチェック
           IF   C100LNK-MDKBN  NOT =  SPACE
               AND C100LNK-MDKBN  NOT =  "0"
             MOVE    10      TO  INP-STS
             GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    集計対象か期間のチェック
           IF   C100LNK-MDKBN  =  SPACE
             IF   C100LNK-YMD1  >  C100LNK-YMD2
               MOVE    20      TO  INP-STS
               GO              TO  110-PARA-HENSYU-EXT
             END-IF
           ELSE
             IF   C100LNK-YMD1(1:6)  >  C100LNK-YMD2(1:6)
               MOVE    20      TO  INP-STS
               GO              TO  110-PARA-HENSYU-EXT
             END-IF
           END-IF
      *
           IF   C100LNK-YMD1(1:4)  NOT =  C100LNK-YMD2(1:4)
             MOVE    21        TO  INP-STS
             GO                TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    入外区分のチェック
           IF   C100LNK-NYUGAIKBN  NOT =  "1"
               AND  C100LNK-NYUGAIKBN  NOT =  "2"
             MOVE    30     TO  INP-STS
             GO             TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    C100LNK-YMD2        TO  SYS-1001-STYUKYMD
           MOVE    C100LNK-YMD1        TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC       TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM  TO  C100LNK-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                       TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    プログラムオプション取得処理
           PERFORM 110-GET-PRGOPTION-SEC
      *
           IF  C100LNK-MDKBN  =  SPACE
      *    印刷年月日取得
             MOVE     1                TO  IDX1
             MOVE     C100LNK-YMD1     TO  WRK-TMP-YMD
             INITIALIZE  WRK-SYMD
             MOVE    WRK-TMP-YMD       TO  WRK-SYMD(1:6)
             PERFORM  31012-LASTDAY-GET-SEC
             MOVE    WRK-SYMD          TO   WRK-TMP-LYMD
      *
             PERFORM  UNTIL  WRK-TMP-YMD  >  C100LNK-YMD2
                      OR  IDX1  >  1000
              IF   WRK-TMP-YMD  >  WRK-TMP-LYMD
                IF   WRK-TMP-MM  >=  12
                  ADD  1        TO  WRK-TMP-YY
                  MOVE    1     TO  WRK-TMP-MM
                  MOVE    1     TO  WRK-TMP-DD
                ELSE
                  ADD  1        TO  WRK-TMP-MM
                  MOVE    1     TO  WRK-TMP-DD
                END-IF
                INITIALIZE  WRK-SYMD
                MOVE    WRK-TMP-YMD    TO  WRK-SYMD(1:6)
                PERFORM 31012-LASTDAY-GET-SEC
                MOVE    WRK-SYMD       TO  WRK-TMP-LYMD
              END-IF
              MOVE    WRK-TMP-YMD      TO  SUM-SRYYMD(IDX1)
              ADD  1            TO  WRK-TMP-DD
              ADD  1            TO  IDX1
             END-PERFORM
             MOVE     "計"      TO  SUM-SRYYMD(IDX1)
             MOVE     IDX1      TO  SUM-SRYYMDIMAX
           ELSE
      *    印刷年月取得
             MOVE   1           TO  IDX1
             MOVE   C100LNK-YMD1(1:6)   TO  WRK-TMP-YMD(1:6)
             MOVE   "01"        TO  WRK-TMP-YMD(7:2)
      *
             PERFORM UNTIL  WRK-TMP-YMD  >  C100LNK-YMD2
                        OR  IDX1  >  1000
              IF  WRK-TMP-YMD  >  WRK-TMP-LYMD
                IF  WRK-TMP-MM  >  12
                  ADD  1        TO  WRK-TMP-YY
                  MOVE    1     TO  WRK-TMP-MM
                END-IF
              END-IF
              MOVE    WRK-TMP-YMD(1:6)  TO  SUM-SRYYMD(IDX1)
              ADD  1            TO  WRK-TMP-MM
              ADD  1            TO  IDX1
             END-PERFORM
             MOVE    "計"       TO  SUM-SRYYMD(IDX1)
             MOVE    IDX1       TO  SUM-SRYYMDIMAX
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    INIファイル読み込み処理(未使用)
      *****************************************************************
       110-INI-FILE-SEC                SECTION.
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE           ORCSMKPASSAREA
           MOVE    "data/A00000C117.INI"    TO  MKPASS-IN-01
           CALL    "ORCSMKPASSSITE"     USING
                                        ORCSMKPASSAREA
           MOVE    MKPASS-OT-01  TO  FILE-NAME
      *
           INITIALIZE   POST-REC
      *
           OPEN  INPUT  POST-FILE
      *
           MOVE    ZERO          TO  FLG-INI
                                     IDX1
           PERFORM  UNTIL  FLG-INI  =  1
             READ  POST-FILE
               AT  END
                 MOVE    1       TO  FLG-INI
               NOT  AT  END
                 IF   POST-TIKUNAME(1:4)  NOT =  "終了"
                     AND  POST-TIKUNAME(1:8)  NOT =  "サンプル"
                   ADD  1        TO  IDX1
                   MOVE    POST-REC       TO  WRK-POST-TBL(IDX1)
                   MOVE    POST-TIKUNAME  TO  SUM-SRYKANAME(IDX1)
                 END-IF
               IF  IDX1   =  50
                   OR   POST-TIKUNAME(1:4)  =  "終了"
                 MOVE    1       TO  FLG-INI
               END-IF
             END-READ
           END-PERFORM
      *
           ADD  1                TO  IDX1
           MOVE    "その他"      TO  SUM-SRYKANAME(IDX1)
           MOVE    "*******"     TO  WRK-POST-NUM(IDX1 1)
           ADD  1                TO  IDX1
           MOVE    "全地区合計"  TO  SUM-SRYKANAME(IDX1)
           MOVE    IDX1          TO  SUM-SRYKAMAX
      *
           CLOSE POST-FILE
      *
           MOVE    ZERO          TO  FLG-INI
      *
           .
       110-INI-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取得処理
      *****************************************************************
       110-GET-PRGOPTION-SEC           SECTION.
      *
           PERFORM   900-PRGOPTION-KEY-SEL-SEC
      *
           MOVE    1             TO  IDX13
           MOVE    1             TO  IDX9
           MOVE    1             TO  WRK-OPT-SIZE
           STRING  PRGOPTION-OPTION  DELIMITED  BY  SIZE
                                     INTO  WRK-LINE
                                     WITH  POINTER  WRK-OPT-SIZE
           END-STRING
           COMPUTE  WRK-OPT-SIZE  =  WRK-OPT-SIZE  -  1
      *
           MOVE    1             TO  IDX14
           PERFORM  UNTIL  IDX14  >  WRK-OPT-SIZE
                    OR  PRGOPTION-OPTION(IDX14:)  =  SPACE
                    OR  IDX13  >  50
                    OR  FLG-DATEKBN  =  1
      *
             MOVE    SPACE       TO  WRK-LINE
      *
             UNSTRING    PRGOPTION-OPTION  DELIMITED   BY X"0A"
                                           INTO  WRK-DATA
                                           WITH  POINTER  IDX14
             END-UNSTRING
             IF  ( WRK-DATA(9:20)  =  SPACE )
               MOVE    1         TO  FLG-DATEKBN
               MOVE    1         TO  FLG-DATEKBN2
             END-IF
      *
             IF   ( WRK-DATA(1:1)  =  "#" )
               CONTINUE
             ELSE
               IF  ( WRK-DATA(1:5)  =  "CHIKU" )
                 IF   FLG-DATEKBN2  =  1
                   COMPUTE  IDX13  =  IDX13  -  1
                 ELSE
                   MOVE    WRK-DATA(9:20)   TO  SUM-SRYKANAME(IDX13)
                 END-IF
                 PERFORM  VARYING  IDX12  FROM  30  BY  8
                          UNTIL  IDX12  >  429
                          OR  IDX9  >  50
                          OR  FLG-DATEKBN2  =  1
                   IF   WRK-DATA(IDX12:1)  =  SPACE
                     MOVE    51   TO  IDX9
                     MOVE    1    TO  FLG-DATEKBN2
                   ELSE
                     MOVE    WRK-DATA(IDX12:7)
                                  TO  WRK-POST-NUM(IDX13 IDX9)
                     ADD    1     TO  IDX9
                   END-IF
                 END-PERFORM
               ELSE
                 MOVE    1        TO  FLG-DATEKBN
                 COMPUTE  IDX13  =  IDX13  -  1
               END-IF
             END-IF
             ADD  1               TO  IDX13
             MOVE    1            TO  IDX9
             INITIALIZE           FLG-DATEKBN2
      *
           END-PERFORM
      *
           MOVE    "その他"       TO  SUM-SRYKANAME(IDX13)
           MOVE    "*******"      TO  WRK-POST-NUM(IDX13 1)
           ADD  1                 TO  IDX13
           MOVE    "全地区合計"   TO  SUM-SRYKANAME(IDX13)
           MOVE    IDX13          TO  SUM-SRYKAMAX
      *
          .
       110-GET-PRGOPTION-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション検索処理
      *****************************************************************
       900-PRGOPTION-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO              TO  FLG-PRGOPTION
      *
           MOVE    SPA-HOSPNUM       TO  PRGOPTION-HOSPNUM
           MOVE    "A00000C117"      TO  PRGOPTION-PRGID
           MOVE    "*"               TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC     TO  MCPDATA-REC
           MOVE    "tbl_prgoption"   TO  MCP-TABLE
           MOVE    "key"             TO  MCP-PATHNAME
           PERFORM  910-DBSELECT-SEC
           IF   (MCP-RC  =  ZERO)
             MOVE    MCPDATA-REC     TO  PRGOPTION-REC
           ELSE
             INITIALIZE              PRGOPTION-REC
             MOVE    1               TO  FLG-PRGOPTION
           END-IF
      *
           MOVE    "tbl_prgoption"   TO  MCP-TABLE
           MOVE    "key"             TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
           .
       900-PRGOPTION-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC              SECTION.
      *
           MOVE    "DBSELECT"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   (MCP-RC  =  ZERO )
             PERFORM  910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO   MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF  C100LNK-MDKBN  =  SPACE
      *    (日別)入院数、退院数、在院中集計処理
             PERFORM  320-NYURRK-CNT-SEC
      *
      *    (日別)入院数、退院数、在院中印刷処理
             PERFORM  330-TERM-HEN-SEC
           ELSE
      *    (月別)入院数、退院数、在院中集計処理
             PERFORM  320-NYURRK-M-CNT-SEC
      *
      *    (月別)入院数、退院数、在院中印刷処理
             PERFORM  330-TERM-M-HEN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値阿エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE     C100N
      *
           MOVE    WRK-SYSYMDWH     TO  C100N-PRTYMD
      *
           ADD  1                   TO  CNT-PAGE
           MOVE    CNT-PAGE         TO  WRK-PAGE
           MOVE    WRK-PAGE         TO  C100N-PAGE
      *
           EVALUATE  INP-STS
             WHEN  10
               MOVE    "入力された集計区分の値に誤りがあります"
                                    TO  C100N-JOBMSG
             WHEN  20
               MOVE
                  "入力された集計対象期間の範囲指定に誤りがあります"
                                    TO  C100N-JOBMSG
             WHEN  21
               MOVE
                  "入力する集計対象期間の範囲は同年内で指定をして下さい"
                                    TO  C100N-JOBMSG
             WHEN  30
               MOVE    "入力された入外区分の値に誤りがあります"
                                    TO  C100N-JOBMSG
           END-EVALUATE
      *
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    (日別)入院数、退院数、在院中集計処理
      *****************************************************************
       320-NYURRK-CNT-SEC      SECTION.
      *
           MOVE    SUM-SRYKAMAX       TO  IDX3
           MOVE    SUM-SRYYMDIMAX     TO  IDX5
      *
           INITIALIZE                 PTNYUINRRK-REC
           MOVE    C100LNK-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    C100LNK-YMD2       TO  PTNYUINRRK-NYUINYMD
           MOVE    C100LNK-YMD1       TO  PTNYUINRRK-TAIINYMD
      *
           PERFORM  900-NYURRK-SELECT-SEC
           PERFORM  UNTIL  FLG-PTNYUINRRK  =  1
      *
      *    集計場所設定
             PERFORM  320-SRYKA-GET-IDX-SEC
      *
      *    加算開始年月日変換及び入院加算
             IF   PTNYUINRRK-NYUINYMD  <  C100LNK-YMD1
               MOVE    C100LNK-YMD1        TO  WRK-STYMD
             ELSE
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-STYMD
               MOVE    WRK-STYMD           TO  WRK-SRYYMD
               PERFORM  320-SRYYMD-GET-IDX-SEC
               ADD  1       TO  SUM-NYUIN(IDX1 IDX6)
               ADD  1       TO  SUM-NYUIN(IDX1 IDX5)
               ADD  1       TO  SUM-NYUIN(IDX3 IDX6)
               ADD  1       TO  SUM-NYUIN(IDX3 IDX5)
             END-IF
      *    加算終了年月日変換及び退院加算
             IF   PTNYUINRRK-TAIINYMD  >  C100LNK-YMD2
               MOVE    C100LNK-YMD2        TO  WRK-EDYMD
             ELSE
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-EDYMD
               MOVE    WRK-EDYMD           TO  WRK-SRYYMD
               PERFORM  320-SRYYMD-GET-IDX-SEC
               ADD  1       TO  SUM-TAIIN(IDX1 IDX6)
               ADD  1       TO  SUM-TAIIN(IDX1 IDX5)
               ADD  1       TO  SUM-TAIIN(IDX3 IDX6)
               ADD  1       TO  SUM-TAIIN(IDX3 IDX5)
             END-IF
      *
      *    年月日をIDXに変換
             MOVE    ZERO        TO    IDAG
                                       IDBG
             MOVE    WRK-STYMD   TO    IDAG
             MOVE    WRK-EDYMD   TO    IDBG
      *    開始年月日から終了年月日まで、データ加算
             PERFORM  UNTIL    IDAG  >     IDBG
      *
      *    最終日取得
                 INITIALIZE          WRK-SYMD
                 MOVE    IDAG(1:6)   TO  WRK-SYMD(1:6)
                 PERFORM  31012-LASTDAY-GET-SEC
                 MOVE    WRK-SYMD    TO  WRK-LASTYMD
      *
      *    加算処理
                 PERFORM  3201-KASAN-SEC
      *
             END-PERFORM
      *
             PERFORM  900-NYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   900-NYURRK-CLOSE-SEC
      *
           .
       320-NYURRK-CNT-EXT.
           EXIT.
      *****************************************************************
      *    (月別)入院数、退院数、在院中集計処理
      *****************************************************************
       320-NYURRK-M-CNT-SEC      SECTION.
      *
           MOVE    SUM-SRYKAMAX        TO  IDX3
           MOVE    SUM-SRYYMDIMAX      TO  IDX5
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    C100LNK-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    C100LNK-YMD2(1:6)   TO  PTNYUINRRK-NYUINYMD(1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    C100LNK-YMD1(1:6)   TO  PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD(7:2)
           PERFORM  900-NYURRK-SELECT-SEC
           PERFORM  UNTIL   FLG-PTNYUINRRK  =  1
      *
      *    集計場所設定
             PERFORM  320-SRYKA-GET-IDX-SEC
      *
      *    加算開始年月日変換及び入院加算
             IF    PTNYUINRRK-NYUINYMD(1:6)  <  C100LNK-YMD1(1:6)
               MOVE    C100LNK-YMD1(1:6)   TO  WRK-STYMD(1:6)
             ELSE
               MOVE    PTNYUINRRK-NYUINYMD(1:6)  TO  WRK-STYMD
               MOVE   WRK-STYMD(1:6)       TO  WRK-SRYYM
               PERFORM  320-SRYYM-GET-IDX-SEC
               ADD  1       TO  SUM-NYUIN(IDX1 IDX6)
               ADD  1       TO  SUM-NYUIN(IDX1 IDX5)
               ADD  1       TO  SUM-NYUIN(IDX3 IDX6)
               ADD  1       TO  SUM-NYUIN(IDX3 IDX5)
             END-IF
      *    加算終了年月日変換及び退院加算
             IF    PTNYUINRRK-TAIINYMD(1:6)  >  C100LNK-YMD2(1:6)
               MOVE    C100LNK-YMD2(1:6)   TO  WRK-EDYMD(1:6)
             ELSE
               MOVE    PTNYUINRRK-TAIINYMD(1:6)  TO  WRK-EDYMD(1:6)
               MOVE    WRK-EDYMD(1:6)            TO  WRK-SRYYM
               PERFORM  320-SRYYM-GET-IDX-SEC
               ADD  1       TO  SUM-TAIIN(IDX1 IDX6)
               ADD  1       TO  SUM-TAIIN(IDX1 IDX5)
               ADD  1       TO  SUM-TAIIN(IDX3 IDX6)
               ADD  1       TO  SUM-TAIIN(IDX3 IDX5)
             END-IF
      *
      *    年月をIDXに変換
             MOVE    ZERO             TO  IDAG
                                          IDBG
             MOVE    WRK-STYMD(1:6)   TO  IDAG(1:6)
             MOVE    WRK-EDYMD(1:6)   TO  IDBG(1:6)
      *
      *    加算処理
             PERFORM  3201-KASAN-M-SEC
      *
             PERFORM  900-NYURRK-FETCH-SEC
           END-PERFORM
           PERFORM  900-NYURRK-CLOSE-SEC
      *
           .
       320-NYURRK-M-CNT-EXT.
           EXIT.
      *****************************************************************
      *    地区INDEX判別処理
      *****************************************************************
       320-SRYKA-GET-IDX-SEC      SECTION.
      *
           MOVE    PTINF-HOME-POST   TO  WRK-POST
           MOVE    ZERO              TO  IDX1
                                         FLG-OK
           MOVE    SUM-SRYKAMAX      TO  IDX2
      *    郵便番号が空欄の場合は「その他」に分類
           IF    WRK-POST  =  SPACE
             COMPUTE  IDX1  =  IDX2 - 1
           ELSE
             PERFORM  UNTIL  IDX1  >  IDX2
                      OR  FLG-OK  =  1
               ADD  1                TO  IDX1
               PERFORM  VARYING  IDY  FROM  1  BY  1
                        UNTIL  IDY  >  50
                        OR  WRK-POST-NUM(IDX1 IDY)  =  SPACE
                        OR  FLG-OK  =  1
                 IF   WRK-POST  =  WRK-POST-NUM(IDX1 IDY)
                   MOVE    1         TO  FLG-OK
                 ELSE
                   MOVE    ZERO      TO  FLG-NG
                   PERFORM  VARYING  IDZ  FROM  1  BY  1
                            UNTIL  IDZ  >  7
                            OR  FLG-NG  =  1
                     IF  WRK-POST-NUM(IDX1 IDY)(IDZ:1)  NOT =  "*"
                         AND  WRK-POST(IDZ:1)
                       MOVE    1      TO  FLG-NG
                     END-IF
                   END-PERFORM
                   IF   FLG-NG  =  ZERO
                     MOVE    1        TO  FLG-OK
                   END-IF
                 END-IF
               END-PERFORM
             END-PERFORM
           END-IF
           .
       320-SRYKA-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日INDEX判定処理
      *****************************************************************
       320-SRYYMD-GET-IDX-SEC      SECTION.
      *
           MOVE    1               TO  IDX6
           MOVE    SUM-SRYYMDIMAX  TO  IDX2
           PERFORM  UNTIL  IDX6   >  IDX2
             IF    SUM-SRYYMD(IDX6)  = WRK-SRYYMD
               MOVE    ZERO        TO  IDX2
             ELSE
               ADD  1              TO  IDX6
             END-IF
           END-PERFORM
      *
           .
       320-SRYYMD-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療月INDEX判定処理
      *****************************************************************
       320-SRYYM-GET-IDX-SEC      SECTION.
      *
           MOVE    1               TO  IDX6
           MOVE    SUM-SRYYMDIMAX  TO  IDX2
           PERFORM  UNTIL  IDX6   >  IDX2
             IF    SUM-SRYYMD(IDX6)(1:6)  =  WRK-SRYYM
               MOVE    ZERO        TO  IDX2
             ELSE
               ADD  1              TO  IDX6
             END-IF
           END-PERFORM
      *
           .
       320-SRYYM-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *   日別在院加算処理
      *****************************************************************
       3201-KASAN-SEC      SECTION.
      *
      *    月の最終日を越えるか、終了年月日を越えるまで加算
           PERFORM  UNTIL  IDA2  >  WRK-LASTDD
                    OR  IDAG  >  IDBG
      *    加算日付IDX設定
             MOVE    IDAG         TO  WRK-SRYYMD
             PERFORM 320-SRYYMD-GET-IDX-SEC
      *
      *    退院日の時は加算しない
             ADD  1        TO  SUM-TOUZAN(IDX1 IDX6)
             ADD  1        TO  SUM-TOUZAN(IDX1 IDX5)
             ADD  1        TO  SUM-TOUZAN(IDX3 IDX6)
             ADD  1        TO  SUM-TOUZAN(IDX3 IDX5)
      *
             ADD  1        TO  IDA2
           END-PERFORM
      *
      *    終了年月日を越えてない時は月を加算し、日を初日に設定する
           IF     IDAG  <    IDBG
             ADD  1        TO  IDA1
             MOVE    1     TO  IDA2
           END-IF
      *
           .
       3201-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *   月別在院加算処理
      *****************************************************************
       3201-KASAN-M-SEC      SECTION.
      *
      *    終了年月を越えるまで加算
           PERFORM  UNTIL  IDA1  >  IDB1
      *
      *    加算月設定
             MOVE    IDAG(1:6)   TO  WRK-SRYYM
             PERFORM  320-SRYYM-GET-IDX-SEC
      *
      *    加算年齢IDX設定
             MOVE    IDAG(1:6)   TO  WRK-NENREI-TBL1(1:6)
             PERFORM  320-SRYKA-GET-IDX-SEC
      *
      *    退院月の時は加算しない
             ADD  1       TO  SUM-TOUZAN(IDX1 IDX6)
             ADD  1       TO  SUM-TOUZAN(IDX1 IDX5)
             ADD  1       TO  SUM-TOUZAN(IDX3 IDX6)
             ADD  1       TO  SUM-TOUZAN(IDX3 IDX5)
      *
             ADD  1       TO  IDA1
           END-PERFORM
      *
           .
       3201-KASAN-M-EXT.
           EXIT.
      *
      *****************************************************************
      *    (日別)入院数、退院数、在院中印刷処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           MOVE    1         TO  IDX5
           PERFORM  UNTIL   IDX5  >  SUM-SRYYMDIMAX
             MOVE    1       TO  IDX2
             PERFORM  UNTIL  IDX2  >  SUM-SRYKAMAX
      *
               INITIALIZE     C100N
      *
      *    帳票タイトル
               MOVE    "(地区別)  入  院  患  者  数  集  計  表"
                                      TO  C100N-MIDASHI
      *    出力年月日
               MOVE    WRK-SYSYMDWH   TO  C100N-PRTYMD
      *    集計対象期間開始日
               MOVE    WRK-YMD1WH     TO  C100N-SRYMD-FR
      *    集計対象期間終了日
               MOVE    WRK-YMD2WH(11:)
                                      TO  C100N-SRMD-TO
      *    集計単位
               MOVE    "月  日"       TO  C100N-TANI
      *
               ADD  1                 TO  CNT-PAGE
               MOVE    CNT-PAGE       TO  WRK-PAGE
               MOVE    WRK-PAGE       TO  C100N-PAGE
      *
               MOVE    1              TO  IDX3
               PERFORM  UNTIL  IDX3  >  7
                        OR  IDX2  >  SUM-SRYKAMAX
      *    年齢名称を集計エリアに転送
                MOVE    SUM-SRYKANAME(IDX2)
                                       TO  C100N-SRYKA(IDX3)
      *    件数名称を転送
                MOVE    "入  院"       TO  C100N-KENSU-1(IDX3)
                MOVE    "退  院"       TO  C100N-KENSU-2(IDX3)
                MOVE    "在院中"       TO  C100N-KENSU-3(IDX3)
      *    日別集計数の集計値を転送
                MOVE    1              TO  IDX6
                MOVE    IDX5           TO  IDX4
                MOVE    SUM-SRYYMD(IDX4)(1:6)  TO  WRK-PR-SRYYM
                PERFORM  UNTIL  IDX4  >  SUM-SRYYMDIMAX
                         OR  IDX6  >  32
                         OR  ( SUM-SRYYMD(IDX4)  NOT =  "計"
                         AND  SUM-SRYYMD(IDX4)(1:6) NOT = WRK-PR-SRYYM )
                 IF  IDX4  =  SUM-SRYYMDIMAX
                   MOVE    "計"         TO  C100N-MD(32)
      *
                   MOVE    SUM-NYUIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5     TO  C100N-11-CNT(IDX3 32)
                   MOVE    SUM-TAIIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5     TO  C100N-12-CNT(IDX3 32)
                   MOVE    SUM-GZAIIN(IDX2)      TO  WRK-Z9-5
                   MOVE    WRK-Z9-5     TO  C100N-99-CNT(IDX3 32)
                 ELSE
                   MOVE    SUM-SRYYMD(IDX4)(5:2)
                                        TO  C100N-MD(IDX6)(1:2)
                   MOVE    "/"          TO  C100N-MD(IDX6)(3:1)
                   MOVE    SUM-SRYYMD(IDX4)(7:2)
                                        TO  C100N-MD(IDX6)(4:2)
                   MOVE    SUM-NYUIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5     TO  C100N-11-CNT(IDX3 IDX6)
                   MOVE    SUM-TAIIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5     TO  C100N-12-CNT(IDX3 IDX6)
                   MOVE    SUM-TOUZAN(IDX2 IDX4) TO  WRK-Z9-5
                   MOVE    WRK-Z9-5     TO  C100N-99-CNT(IDX3 IDX6)
                   ADD   SUM-TOUZAN(IDX2 IDX4)   TO  SUM-GZAIIN(IDX2)
                 END-IF
                 ADD  1        TO  IDX4
                 ADD  1        TO  IDX6
                END-PERFORM
      *
                ADD  1         TO  IDX2
                ADD  1         TO  IDX3
               END-PERFORM
      *
               PERFORM 340-PRINT-OUT-SEC
      *
             END-PERFORM
             COMPUTE  IDX5  =  IDX5  +  (IDX6  -  1)
             MOVE    1         TO  FLG-ZAIIN
           END-PERFORM
      *
      *    CSVヘッダ編集処理
           PERFORM  400-CSV-HEAD-SEC
      *    CSVデータ編集処理
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    (月別)入院数、退院数、在院中印刷処理
      *****************************************************************
       330-TERM-M-HEN-SEC         SECTION.
      *
           MOVE    1         TO  IDX5
           PERFORM  UNTIL   IDX5  >  SUM-SRYYMDIMAX
             MOVE    1       TO  IDX2
             PERFORM  UNTIL  IDX2  >  SUM-SRYKAMAX
      *
               INITIALIZE     C100N
      *
      *    帳票タイトル
               MOVE    "(地区別)  入  院  患  者  数  集  計  表"
                                      TO  C100N-MIDASHI
      *    出力年月日
               MOVE    WRK-SYSYMDWH   TO  C100N-PRTYMD
      *    集計対象期間開始日
               MOVE    WRK-YMD1WH(1:16)
                                      TO  C100N-SRYMD-FR
      *    集計対象期間終了日
               MOVE    WRK-YMD2WH(11:6)
                                      TO  C100N-SRMD-TO
      *    集計単位
               MOVE    "  月"         TO  C100N-TANI
      *
               ADD  1                 TO  CNT-PAGE
               MOVE    CNT-PAGE       TO  WRK-PAGE
               MOVE    WRK-PAGE       TO  C100N-PAGE
      *
               MOVE    1              TO  IDX3
               PERFORM  UNTIL  IDX3  >  7
                        OR  IDX2  >  SUM-SRYKAMAX
      *    診療科名称を転送
                MOVE    SUM-SRYKANAME(IDX2)   TO  C100N-SRYKA(IDX3)
      *    件数名称を転送
                MOVE    "入  院"      TO  C100N-KENSU-1(IDX3)
                MOVE    "退  院"      TO  C100N-KENSU-2(IDX3)
                MOVE    "在院中"      TO  C100N-KENSU-3(IDX3)
      *
      *    月別集計数の集計値を転送
                MOVE    1             TO  IDX6
                MOVE    IDX5          TO  IDX4
                PERFORM  UNTIL  IDX4  >  SUM-SRYYMDIMAX
                         OR  IDX6  >  32
                 IF   IDX4   =     SUM-SRYYMDIMAX
                   MOVE    "計"       TO  C100N-MD(32)
                   MOVE    SUM-NYUIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5   TO  C100N-11-CNT(IDX3 32)
                   MOVE    SUM-TAIIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5   TO  C100N-12-CNT(IDX3 32)
                   MOVE    SUM-GZAIIN(IDX2)      TO  WRK-Z9-5
                   MOVE    WRK-Z9-5   TO  C100N-99-CNT(IDX3 32)
                 ELSE
                   MOVE    SUM-SRYYMD(IDX4)(5:2)
                                      TO  C100N-MD(IDX6)(4:2)
                   MOVE    SUM-NYUIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5   TO  C100N-11-CNT(IDX3 IDX6)
                   MOVE    SUM-TAIIN(IDX2 IDX4)  TO  WRK-Z9-5
                   MOVE    WRK-Z9-5   TO  C100N-12-CNT(IDX3 IDX6)
                   MOVE    SUM-TOUZAN(IDX2 IDX4) TO  WRK-Z9-5
                   MOVE    WRK-Z9-5   TO  C100N-99-CNT(IDX3 IDX6)
                   ADD  SUM-TOUZAN(IDX2 IDX4)    TO  SUM-GZAIIN(IDX2)
                 END-IF
                 ADD  1        TO  IDX4
                 ADD  1        TO  IDX6
                END-PERFORM
      *
                ADD  1         TO  IDX2
                ADD  1         TO  IDX3
               END-PERFORM
      *
               PERFORM 340-PRINT-OUT-SEC
      *
             END-PERFORM
             COMPUTE  IDX5  =  IDX5  +  (IDX6  -  1)
             MOVE    1         TO  FLG-ZAIIN
           END-PERFORM
      *
      *    CSVヘッダ編集処理
           PERFORM  400-CSV-HEAD-SEC
      *    CSVデータ編集処理
           PERFORM  400-CSV-DATA-SEC
      *
          .
       330-TERM-M-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    C100LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    C100LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    C100LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    C100LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    C100LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    C100LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    C100LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    C100LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE   "A00000C100N.red"        TO  SPRT-PRTID
           MOVE   "(地区別)入院患者数集計表"      TO  SPRT-TITLE
           MOVE    C100N                          TO  SPRT-PRTDATA
           MOVE    C100LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    C100LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    C100LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                      USING
                                                  ORCSPRTAREA
                                                  SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR  =  ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT           RECEERR-FILE
      *
             MOVE    WRK-RECEERR      TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"            TO  SJOBKANRI-MODE
             INITIALIZE               JOBKANRI-REC
             MOVE    SPA-HOSPNUM      TO  JOB-HOSPNUM
             MOVE    C100LNK-JOBID    TO  JOB-JOBID
             MOVE    C100LNK-SHELLID  TO  JOB-SHELLID
             MOVE    WRK-RECEERR      TO  JOB-YOBI
             MOVE    "9999"           TO  JOB-ERRCD
             CALL    "ORCSJOB"        USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
           END-IF
      *
           MOVE    1         TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       400-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                 TO  IDXP
      *    対象年月
           INITIALIZE                WRK-ITEM
           STRING    C100N-SRYMD-FR  DELIMITED  BY  SIZE
                     "〜"            DELIMITED  BY  SIZE
                     C100N-SRMD-TO   DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    作成日セット
           INITIALIZE                WRK-ITEM
           MOVE    C100N-PRTYMD      TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    診療科名称セット
           INITIALIZE                   WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-CHIKU  FROM  1  BY  1
                    UNTIL  IDX-CHIKU  >  SUM-SRYKAMAX
             INITIALIZE            WRK-ITEM
             STRING    SUM-SRYKANAME(IDX-CHIKU)  DELIMITED  BY  SPACE
                       ",,"                      DELIMITED  BY  SIZE
                                                 INTO  WRK-ITEM
             END-STRING
             IF  IDX-CHIKU  =  SUM-SRYKAMAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    入院、退院、在院セット
           IF  C100LNK-MDKBN  =  SPACE
             MOVE    "月日"          TO  WRK-ITEM
           ELSE
             MOVE    "月"            TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
           PERFORM  VARYING  IDX-CHIKU  FROM  1  BY  1
                    UNTIL  IDX-CHIKU  >  SUM-SRYKAMAX
             MOVE    "入　院 "       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
             MOVE    "退　院"        TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
             MOVE    "在院中"        TO  WRK-ITEM
             IF  IDX-CHIKU  =  SUM-SRYKAMAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    CSV出力処理
           PERFORM  230-TOUKEICSV-SEC
      *
           .
      *
       400-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           PERFORM  VARYING  IDX-LINE  FROM  1  BY  1
                    UNTIL  IDX-LINE  >  SUM-SRYYMDIMAX
      *
             MOVE    1                 TO  IDXP
      *
      *    日付
             IF   IDX-LINE  =  SUM-SRYYMDIMAX
               MOVE    "計"           TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             ELSE
               IF  C100LNK-MDKBN  =  SPACE
                 INITIALIZE            WRK-ITEM
                 STRING   SUM-SRYYMD(IDX-LINE)(5:2)  DELIMITED  BY  SIZE
                           "/"                       DELIMITED  BY  SIZE
                          SUM-SRYYMD(IDX-LINE)(7:2)  DELIMITED  BY  SIZE
                                                     INTO  WRK-ITEM
                 END-STRING
               ELSE
                 MOVE    SUM-SRYYMD(IDX-LINE)(5:2)    TO  WRK-ITEM
               END-IF
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
             PERFORM  VARYING  IDX-CHIKU  FROM  1  BY  1
                      UNTIL  IDX-CHIKU  >  SUM-SRYKAMAX
      *    入院数
               MOVE    SUM-NYUIN(IDX-CHIKU IDX-LINE)  TO  WRK-Z9-5-2
               MOVE    WRK-Z9-5-2            TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
      *    退院数
               MOVE    SUM-TAIIN(IDX-CHIKU IDX-LINE)  TO  WRK-Z9-5-2
               MOVE    WRK-Z9-5-2            TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
      *    在院数
               IF   IDX-LINE  =  SUM-SRYYMDIMAX
                 MOVE    SUM-GZAIIN(IDX-CHIKU)    TO  WRK-Z9-5-2
                 MOVE    WRK-Z9-5-2          TO  WRK-ITEM
               ELSE
                 MOVE    SUM-TOUZAN(IDX-CHIKU IDX-LINE)  TO  WRK-Z9-5-2
                 MOVE    WRK-Z9-5-2          TO  WRK-ITEM
               END-IF
               IF  IDX-CHIKU  =  SUM-SRYKAMAX
                 PERFORM  810-CSVDATA-EDIT-SEC
               ELSE
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
      *
             END-PERFORM
      *
      *      CSV出力処理
             PERFORM  230-TOUKEICSV-SEC
      *
           END-PERFORM
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","           DELIMITED  BY  SIZE
                                   INTO  WRK-CSVDATA
                                   WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"         DELIMITED  BY  SIZE
                                   INTO  WRK-CSVDATA
                                   WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX-LEN  FROM  1  BY  1
                    UNTIL  IDX-LEN  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX-LEN:)  =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX-LEN:1)  NOT =  SPACE
                 MOVE    IDX-LEN      TO  IDX-ST
                 MOVE    1            TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    C100LNK-PRTKANRI-TBL-KEY  TO  STOUKEICSV-TBL-KEY
           MOVE    C100LNK-PRTKANRI-RENNUM   TO  STOUKEICSV-RENNUM
           MOVE    C100LNK-PRTKANRI-TBL-GROUP  TO  STOUKEICSV-TBL-GROUP
           MOVE    C100LNK-PRTKANRI-SHELLID  TO  STOUKEICSV-SHELLID
           MOVE    C100LNK-PRTKANRI-SRYYM    TO  STOUKEICSV-SRYYM
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    C100LNK-NYUGAIKBN         TO  STOUKEICSV-NYUGAIKBN
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
           MOVE    C100LNK-PRTKANRI-SHORI-RENNUM
                                             TO  STOUKEICSV-SHORI-RENNUM
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "kanjyasyukei_n_chiku_"   DELIMITED  BY  SIZE
                   C100LNK-YMD1              DELIMITED  BY  SIZE
                   "_"                       DELIMITED  BY  SIZE
                   C100LNK-YMD2              DELIMITED  BY  SIZE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "地区別入院患者数集計表"  TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"           USING
                                             ORCSTOUKEICSVAREA
                                             SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  WRK-CSVDATA
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"              TO  SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
           MOVE    C100LNK-JOBID      TO  JOB-JOBID
           MOVE    C100LNK-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-PAGE           TO  JOB-UPDCNT
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                 STS-AREA-DAY
           INITIALIZE                 LNK-DAY2-AREA
           MOVE    "21"               TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD           TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"          USING
                                      STS-AREA-DAY
                                      LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3   TO  WRK-HENYMDG
           INSPECT  WRK-HENYMDG  REPLACING  ALL  "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                 STS-AREA-DAY
           INITIALIZE                 LNK-DAY7-AREA
           MOVE    "71"               TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD           TO  LNK-DAY7-YM
           CALL    "ORCSDAY"          USING
                                      STS-AREA-DAY
                                      LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI    TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索
      *****************************************************************
       900-NYURRK-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM 900-NYURRK-FETCH-SEC
           ELSE
             MOVE    1                 TO  FLG-PTNYUINRRK
             MOVE    1                 TO  FLG-END
           END-IF
           .
       900-NYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索
      *****************************************************************
       900-NYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO              TO  FLG-PTNYUINRRK
             MOVE    MCPDATA-REC       TO  PTNYUINRRK-REC
      *    患者情報の取得
             INITIALIZE                PTINF-REC
             MOVE     PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
             MOVE     PTNYUINRRK-PTID  TO  PTINF-PTID
             PERFORM  900-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
                 AND  PTINF-TSTPTNUMKBN  =  "1"
               GO    TO   900-NYURRK-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1                 TO  FLG-PTNYUINRRK
             MOVE    1                 TO  FLG-END
           END-IF
           .
       900-NYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-NYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key26"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           .
       900-NYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
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
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
             ELSE
               MOVE    1               TO  FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-SYSKANRI3
             MOVE    MCPDATA-REC        TO  SYS-1005-REC
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE  "tbl_syskanri"         TO  MCP-TABLE
           MOVE  "key2"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
             INITIALIZE                PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
