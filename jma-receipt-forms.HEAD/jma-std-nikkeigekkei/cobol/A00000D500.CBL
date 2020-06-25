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
       PROGRAM-ID.             A00000D500.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表（ＨＣ１７）
      *  管理者            :
      *  作成日付    作業者       記述
      *  03/09/29    @@@@-有村    新規作成(ORCBD500のプログラムID変更Ver)
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/03/17  患者番号の編集処理を修正
      *  01.00.03    @@@@-田中    11/04/04  患者番号桁数を最大20桁に変更
      *  01.00.04    @@@@-門間    11/04/18  処理速度の改善、コメント履歴削除
      *  01.00.05    @@@@-田中    11/10/04  CSVファイル作成機能追加
      *  01.00.06    @@@@-田中    11/11/21  CSVファイル作成領域を修正
      *  01.00.07    @@@@-田中    12/01/10  CSV合計計算処理を修正
      *  01.00.08    @@@@-田中    12/02/02  公務災害（健保準拠）時の点数単価不具合を修正
      *  01.00.09    @@@@-門間    14/06/23  自費診療の端数処理を修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.01    @@@@-門間    14/11/13  CSV出力時、改頁処理を修正
      *  02.00.02    @@@@-門間    15/01/14  自費診療に小計を追加
      *  02.00.03    @@@@-門間    15/06/09  桁数判別方法の変更
      *                                     自費小計欄の印字桁数修正
      *  02.00.04    門間         16/02/01  一時ディレクトリ対応(エラーファイル)
      *  02.00.05    門間         18/05/30  CSVファイル作成時の処理を修正
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000D500V02.INC".
           COPY    "A00000D500V03.INC".
           COPY    "A00000D5002.INC".
           COPY    "A00000D5002V03.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-CSV             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(04).
           03  IDZ                   PIC 9(04).
           03  IDX-S                 PIC 9(05).
           03  IDX-N                 PIC 9(05).
           03  IDX-MAX               PIC 9(05).
           03  JIHIIDX               PIC 9(04).
           03  IDX-SUM               PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDX4                  PIC 9(03).
           03  IDX5                  PIC 9(03).
           03  IDX-NG                PIC 9(01).
           03  IDX-ST                PIC 9(03).
           03  IDX-ED                PIC 9(03).
           03  IDXP                  PIC 9(05).
           03  IDX-JIHI              PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-DENPPRTYMD   PIC X(08).
           03  WRK-PARA-SORTKBN      PIC X(01).
           03  WRK-PARA-GOKEIKBN     PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
      *
           03  WRK-MONEY             PIC S9(07).
           03  WRK-JIHI              PIC S9(07).
           03  WRK-SYUEKI            PIC S9(07).
      *
           03  WRK-JIHI2             PIC 9(07).
           03  WRK-JIHI3             PIC 9(07).
      *
           03  WRK-JIHI-TTL          PIC 9(07).
           03  WRK-HASU-KBN          PIC X(01).
           03  WRK-HASU-KBNJ         PIC X(01).
      *
           03  WRK-TJIHI-G           OCCURS 10.
               05  WRK-TJIHI         PIC X(08).
           03  WRK-TJIHIN-G          OCCURS 10.
               05  WRK-TJIHIN        PIC X(08).
      *
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-4001-KOMUFLG      PIC X(01).
           03  WRK-4001-KOMUTENTANKA PIC 9(02).
           03  WRK-TENTNK            PIC 9(02)V9(01).
      *
           03  WRK-RENNUM-X         PIC ZZ9.
           03  WRK-PAGE             PIC ZZ9.
           03  WRK-Z9               PIC ZZZZZZ9.
           03  WRK-ZZ               PIC ZZZZZZZ.
           03  WRK-ZZ9              PIC ZZZZZZZ9.
           03  WRK-ZZZ              PIC ZZZZZZZZ.
           03  WRK-S9               PIC -------9.
           03  WRK-SZ               PIC -------Z.
           03  WRK-SZ9              PIC --------9.
           03  WRK-SZZ              PIC --------Z.
           03  WRK-S9-2             PIC ------9.
           03  WRK-Z9-2             PIC ------Z.
           03  WRK-COMPFTN          PIC S9(10).
           03  WRK-SZ10             PIC ---------Z.
      *
       01  SUM-AREA                 VALUE ZERO.
           03  SUM-HKNTEN           PIC 9(08).
           03  SUM-HKNKEN           PIC 9(08).
           03  SUM-OCC              OCCURS 20.
               05  SUM-MONEY        PIC S9(08).
               05  SUM-KEN          PIC 9(08).
           03  SUM-SYUEKI           PIC 9(08).
           03  SUM-SEIKYU           PIC S9(08).
           03  SUM-RYOSYU           PIC S9(08).
           03  SUM-SEIKYUKEN        PIC 9(08).
           03  SUM-RYOSYUKEN        PIC 9(08).
      *
       01  SUM-G-AREA               VALUE ZERO.
           03  SUM-G-HKNTEN         PIC 9(09).
           03  SUM-G-OCC            OCCURS 20.
               05  SUM-G-MONEY      PIC S9(09).
           03  SUM-G-SYUEKI         PIC S9(09).
           03  SUM-G-SEIKYU         PIC S9(09).
           03  SUM-G-RYOSYU         PIC S9(09).
           03  SUM-G-JIHIKEI        PIC S9(09).
      *
       01  SUM-GG-AREA              VALUE   ZERO.
           03  SUM-GG-HKNTEN        PIC 9(09).
           03  SUM-GG-SYUEKI        PIC S9(10).
           03  SUM-GG-OCC           OCCURS  20.
               05  SUM-GG-MONEY     PIC S9(08).
           03  SUM-GG-SEIKYU        PIC S9(10).
           03  SUM-GG-RYOSYU        PIC S9(10).
           03  SUM-GG-JIHIKEI       PIC S9(10).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL         OCCURS  10000.
               05  WRK-SMEI-HOSPNUM         PIC 9(02).
               05  WRK-SMEI-PTID            PIC 9(10).
               05  WRK-SMEI-SORT-KEY.
                 07  WRK-SMEI-NYUGAIKBN     PIC X(01).
                 07  WRK-SMEI-SRYKA         PIC X(02).
                 07  WRK-SMEI-DENPNUM       PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM    PIC 9(02).
               05  WRK-SMEI-SORT-KEY2.
                 07  WRK-SMEI-NYUGAIKBN2    PIC X(01).
                 07  WRK-SMEI-SRYKA2        PIC X(02).
                 07  WRK-SMEI-PTNUM         PIC X(20).
               05  WRK-SMEI-SKYMONEY        PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
           03  WRK-SV-SMEI-TBL              PIC X(69).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)  VALUE "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)  VALUE "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)  VALUE "973".
      *    公害保険番号
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)  VALUE "975".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-DENPNUM       PIC X(50) VALUE "伝票番号".
               07  CSV-HEAD-PTNUM         PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-PTNAME        PIC X(50) VALUE "患者氏名".
               07  CSV-HEAD-HKNTEN        PIC X(50)
                                          VALUE "保険点数(点)".
               07  CSV-HEAD-SYUEKI        PIC X(50)
                                          VALUE "医療収益(総計額)".
               07  CSV-HEAD-HKN1          PIC X(50).
               07  CSV-HEAD-HKN2          PIC X(50).
               07  CSV-HEAD-HKN3          PIC X(50).
               07  CSV-HEAD-HKN4          PIC X(50).
               07  CSV-HEAD-HKN5          PIC X(50).
               07  CSV-HEAD-HKN6          PIC X(50).
               07  CSV-HEAD-JIHI-G.
                 09  CSV-HEAD-SKY-JIHI1   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI2   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI3   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI4   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI5   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI6   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI7   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI8   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI9   PIC X(50)  VALUE SPACE.
                 09  CSV-HEAD-SKY-JIHI10  PIC X(50)  VALUE SPACE.
               07  CSV-HEAD-JIHI-R        REDEFINES CSV-HEAD-JIHI-G.
                 09  CSV-HEAD-JIHI-OCC    OCCURS  10.
                   11  CSV-HEAD-JIHI-NAME PIC X(50).
               07  CSV-HEAD-SKY-JIHIKEI   PIC X(50)
                                          VALUE "自費小計".
               07  CSV-HEAD-SKY-JIHI      PIC X(50)
                                          VALUE "自費".
               07  CSV-HEAD-SKY-ROSAI     PIC X(50)
                                          VALUE "労災".
               07  CSV-HEAD-SKY-JIBAI     PIC X(50)
                                          VALUE "自賠責".
               07  CSV-HEAD-SKY-KOUGAI    PIC X(50)
                                          VALUE "公害".
               07  CSV-HEAD-SKYMONEY      PIC X(50)
                                          VALUE "窓口請求金額".
               07  CSV-HEAD-RSYMONEY      PIC X(50)
                                          VALUE "窓口領収金額".
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 28.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 28.
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
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY  "CPSK4001.INC".
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
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    労災保険番号マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
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
                   UNTIL  FLG-END  =  1
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  SUM-G-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  SUM-GG-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
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
                                               WRK-PARA-DENPPRTYMD
                                               WRK-PARA-SORTKBN
                                               WRK-PARA-GOKEIKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO   JOB-SHELLID
           MOVE    "ORCBD002"         TO   JOB-PGID
           MOVE    "日計表明細（窓口領収金）"   TO  JOB-SHELLMSG
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
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD   TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-SYSYMDWH
      *
      *    対象年月日編集
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-PARA-DENPPRTYMDWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE  WRK-PARA-DENPPRTYMD     TO  SYS-1001-EDYUKYMD
                                             SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    自費情報取得
      *    区分11
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "11"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1013-REC
             MOVE    SYS-1013-JIHINAME   TO  WRK-TJIHIN(1)
           ELSE
             MOVE    SPACE               TO  WRK-TJIHIN(1)
           END-IF
      *
      *    区分12
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "12"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(2)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(2)
           END-IF
      *
      *    区分13
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE   "1013"                 TO  SYS-1013-KANRICD
           MOVE   "13"                   TO  SYS-1013-KBNCD
           MOVE   SPA-HOSPNUM            TO  SYS-1013-HOSPNUM
           MOVE   WRK-PARA-DENPPRTYMD    TO  WRK-SYMD
           MOVE   WRK-PARA-DENPPRTYMD    TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE   SYS-1013-REC           TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(3)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(3)
           END-IF
      *
      *    区分14
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "14"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE  WRK-PARA-DENPPRTYMD     TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(4)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "15"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(5)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(5)
           END-IF
      *
      *    区分16
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "16"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(6)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "17"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(7)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "18"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(8)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "19"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(9)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "10"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(10)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(10)
           END-IF
      *
      *    自費情報取得
      *    区分21
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "21"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(1)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(1)
           END-IF
      *
      *    区分22
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "22"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2  =  ZERO
             MOVE  MCPDATA-REC           TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME     TO  WRK-TJIHI(2)
           ELSE
             MOVE  SPACE                 TO  WRK-TJIHI(2)
           END-IF
      *
      *    区分23
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "23"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE  WRK-PARA-DENPPRTYMD     TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(3)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(3)
           END-IF
      *
      *    区分24
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "24"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(4)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "25"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2  =  ZERO
               MOVE  MCPDATA-REC         TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME   TO  WRK-TJIHI(5)
           ELSE
               MOVE  SPACE               TO  WRK-TJIHI(5)
           END-IF
      *
      *    区分26
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "26"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(6)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "27"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(7)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "28"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(8)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "29"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(9)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE                 TO  SYS-1013-REC
           INITIALIZE                    SYS-1013-REC
           MOVE    "1013"                TO  SYS-1013-KANRICD
           MOVE    "20"                  TO  SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO  MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHI(10)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHI(10)
           END-IF
      *
      *    労災医療機関情報
           MOVE    SPACE                 TO  SYS-4001-REC
           INITIALIZE                    SYS-4001-REC
           MOVE    "4001"                TO  SYS-4001-KANRICD
           MOVE    "*"                   TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD   TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO  SYS-4001-EDYUKYMD
                                             SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC          TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5            TO  WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12              TO  WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10              TO  WRK-4001-TENTANKA
             END-EVALUATE
      *
             IF  SYS-4001-KOMUKBN  =  "2"
      *    公務災害(健保基準)時の点数単価設定
               MOVE    SYS-4001-KOMUTENTANKA-X TO  WRK-4001-KOMUTENTANKA
      *    公務災害区分が健保基準の場合、１を格納
               MOVE    "1"        TO  WRK-4001-KOMUFLG
             END-IF
           ELSE
             MOVE    10                  TO  WRK-4001-TENTANKA
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM  UNTIL FLG-END   =   1
      *    帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
             IF   FLG-GET  =  1
               EVALUATE  WRK-PARA-GOKEIKBN
                 WHEN  "1"
                   PERFORM  390-PRINT-OUT-SEC
      *    ＣＳＶ出力処理
                   PERFORM  230-TOUKEICSV-SEC
                   IF   WRK-SMEI-NYUGAIKBN(IDX-S) NOT =
                                      WRK-SMEI-NYUGAIKBN(IDX-S - 1)
                     OR WRK-SMEI-SRYKA(IDX-S)     NOT =
                                      WRK-SMEI-SRYKA(IDX-S - 1)
                     PERFORM  400-PRINT-OUT-SEC
      *
      *    ＣＳＶ出力処理
                     PERFORM  230-TOUKEICSV-SEC
      *
                     INITIALIZE       SUM-GG-AREA
                   END-IF
                 WHEN  "2"
                   IF   WRK-SMEI-NYUGAIKBN(IDX-S) NOT =
                                      WRK-SMEI-NYUGAIKBN(IDX-S - 1)
                     OR WRK-SMEI-SRYKA(IDX-S)     NOT =
                                      WRK-SMEI-SRYKA(IDX-S - 1)
                     PERFORM  400-PRINT-OUT-SEC
      *    ＣＳＶ出力処理
                     PERFORM  230-TOUKEICSV-SEC
      *
                     INITIALIZE       SUM-GG-AREA
                   END-IF
                 WHEN  OTHER
                   PERFORM  390-PRINT-OUT-SEC
      *    ＣＳＶ出力処理
                   PERFORM  230-TOUKEICSV-SEC
               END-EVALUATE
      *
      *        CSVヘッダ編集処理
               IF (    WRK-SMEI-NYUGAIKBN(IDX-S)  NOT =  WRK-NYUGAIKBN
                    OR WRK-SMEI-SRYKA(IDX-S)      NOT =  WRK-SRYKA     )
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
                 PERFORM  800-CSV-HEAD-SEC
               END-IF
      *
             END-IF
           END-PERFORM
      *
      *    帳票印刷処理
           IF  FLG-GET = ZERO
             PERFORM 310-HEAD-HEN-SEC
             MOVE  "＊＊＊  対象データが１件も存在しません。  ＊＊＊"
                                               TO   HC17-JOBMSG
                                                    HC17V03-JOBMSG
             PERFORM 390-PRINT-OUT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選定処理
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE     ZERO        TO   IDX-S
      *
      *    外来・入院患者を取得
           PERFORM  VARYING  IDX-NG   FROM 1  BY 1
                    UNTIL    IDX-NG   >    2
      *
             PERFORM   910-DBSELECT-SEC
             PERFORM   UNTIL   FLG-SYUMEI  =  1
               PERFORM   900-SYUNOU-READ-SEC
      *
               IF      FLG-SYUNOU               =  ZERO
                 AND   SYU-DENPJTIKBN       NOT =  "3"
                 AND ((SMEI-JOUTAIKBN       NOT =  "8"
                 AND   SMEI-JOUTAIKBN       NOT =  "3")
                 OR    SMEI-SKYMONEY        NOT =  0
                 OR    SMEI-NYUHEN-MONEY    NOT =  0)
      *
                 PERFORM   900-PTNUM-READ-SEC
      *
                 ADD   1                 TO  IDX-S
                 MOVE  SMEI-HOSPNUM      TO  WRK-SMEI-HOSPNUM(IDX-S)
                 MOVE  SMEI-NYUGAIKBN    TO  WRK-SMEI-NYUGAIKBN(IDX-S)
                 MOVE  SMEI-SRYKA        TO  WRK-SMEI-SRYKA(IDX-S)
                 MOVE  SMEI-PTID         TO  WRK-SMEI-PTID(IDX-S)
                 MOVE  SMEI-DENPNUM      TO  WRK-SMEI-DENPNUM(IDX-S)
                 MOVE  SMEI-DENPEDANUM   TO  WRK-SMEI-DENPEDANUM(IDX-S)
                 MOVE  SMEI-SKYMONEY     TO  WRK-SMEI-SKYMONEY(IDX-S)
                 MOVE  SMEI-NYUHEN-MONEY
                                       TO  WRK-SMEI-NYUHEN-MONEY(IDX-S)
                 MOVE  SMEI-NYUHEN-YMD   TO  WRK-SMEI-NYUHEN-YMD(IDX-S)
                 MOVE  PTNUM-PTNUM       TO  WRK-SMEI-PTNUM(IDX-S)
               END-IF
               PERFORM   910-DBFETCH-SEC
             END-PERFORM
             PERFORM   910-DBCLOSE-SEC
           END-PERFORM
      *
      *    ソート処理
           MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX     >    1
             MOVE   1         TO   FLG-GET
             MOVE   ZERO      TO   FLG-END
             MOVE    1        TO   IDX-S
             MOVE    2        TO   IDX-N
             PERFORM  UNTIL IDX-S  NOT <  IDX-MAX
               PERFORM  UNTIL IDX-N  >  IDX-MAX
                 IF  WRK-PARA-SORTKBN   =   "1"
                   IF  WRK-SMEI-SORT-KEY2(IDX-S)  >
                       WRK-SMEI-SORT-KEY2(IDX-N)
                     MOVE WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                     MOVE WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                     MOVE WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                   END-IF
                 ELSE
                   IF  WRK-SMEI-SORT-KEY(IDX-S)  >
                       WRK-SMEI-SORT-KEY(IDX-N)
                     MOVE WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                     MOVE WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                     MOVE WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                   END-IF
                 END-IF
                 ADD   1      TO   IDX-N
               END-PERFORM
               ADD    1       TO   IDX-S
               COMPUTE  IDX-N  =  IDX-S  +  1
             END-PERFORM
           ELSE
             IF   IDX-MAX  =  ZERO
               MOVE   ZERO    TO   FLG-GET
               MOVE   1       TO   FLG-END
             ELSE
               MOVE   1       TO   FLG-GET
               MOVE   ZERO    TO   FLG-END
             END-IF
           END-IF
      *
           MOVE     1         TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計帳票編集<ヘッダー部>処理
      *****************************************************************
       320-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           INITIALIZE                  D5002
                                       D5002V03
           MOVE    WRK-PARA-DENPPRTYMDWH   TO   D5002-SYSYMD
                                                D5002V03-SYSYMD
      *
      *    入外区分名称設定
           EVALUATE WRK-NYUGAIKBN
             WHEN  "1"
               MOVE   "入院"         TO   D5002-NYUGAIKBN
                                          D5002V03-NYUGAIKBN
      *    入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL  IDX2   >  10
                 MOVE   WRK-TJIHIN(IDX2)   TO   D5002-TJIHI(IDX2)
                                                D5002V03-TJIHI(IDX2)
               END-PERFORM
             WHEN  "2"
               MOVE   "外来"         TO   D5002-NYUGAIKBN
                                          D5002V03-NYUGAIKBN
      *    自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHI(IDX2)   TO   D5002-TJIHI(IDX2)
                                               D5002V03-TJIHI(IDX2)
               END-PERFORM
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                        SYS-1005-REC
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO   SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1005-EDYUKYMD
                                             SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME  TO   D5002-SRYKA
                                             D5002V03-SRYKA
           ELSE
             MOVE   SPACE               TO   D5002-SRYKA
                                             D5002V03-SRYKA
           END-IF
      *
           .
       320-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           INITIALIZE                  HC17
                                       HC17V03
      *
           MOVE    WRK-PARA-DENPPRTYMDWH   TO   HC17-SYSYMD
                                                HC17V03-SYSYMD
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN  "1"
               MOVE   "入院"         TO   HC17-NYUGAIKBN
                                          HC17V03-NYUGAIKBN
      *    入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL  IDX2   >  10
                 MOVE   WRK-TJIHIN(IDX2)   TO   HC17-TJIHI(IDX2)
                                                HC17V03-TJIHI(IDX2)
               END-PERFORM
             WHEN  "2"
               MOVE   "外来"         TO   HC17-NYUGAIKBN
                                          HC17V03-NYUGAIKBN
      *    外来自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHI(IDX2)   TO   HC17-TJIHI(IDX2)
                                               HC17V03-TJIHI(IDX2)
               END-PERFORM
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                        SYS-1005-REC
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO   SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1005-EDYUKYMD
                                             SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME   TO   HC17-SRYKA
                                              HC17V03-SRYKA
           ELSE
             MOVE   SPACE                TO   HC17-SRYKA
                                              HC17V03-SRYKA
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO        TO   CNT-LINE
      *
           INITIALIZE                          SUM-G-AREA
      *
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *
      *    CSVヘッダ編集処理
           IF  FLG-FIRST  =  ZERO
             PERFORM  800-CSV-HEAD-SEC
             MOVE    1         TO  FLG-FIRST
           END-IF
      *
      *    帳票編集<見出し>処理
           EVALUATE  WRK-PARA-GOKEIKBN
             WHEN  "1"
               PERFORM  310-HEAD-HEN-SEC
               PERFORM  320-HEAD-HEN-SEC
             WHEN  "2"
               PERFORM  320-HEAD-HEN-SEC
             WHEN  OTHER
               PERFORM  310-HEAD-HEN-SEC
           END-EVALUATE
      *
           PERFORM   UNTIL   FLG-END   =   1
                     OR  CNT-LINE   =   21
      *    収納テーブル再読み込み
             PERFORM 901-SYUNOU-READ-SEC
             IF   FLG-SYUNOU   =   ZERO
      *
      *    入外区分ブレイクチェック処理
               IF   WRK-SMEI-NYUGAIKBN(IDX-S)  NOT =  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO  WRK-SRYKA
      **
                 EVALUATE   WRK-PARA-GOKEIKBN
                   WHEN  "1"
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
      *    帳票編集<見出し>処理
                     PERFORM   310-HEAD-HEN-SEC
      *    合計印字
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
      *    CSV合計編集処理
                     IF  WRK-PARA-GOKEIKBN  NOT =  SPACE
                       PERFORM  400-CSV-GOUKEI-SEC
                     END-IF
                     INITIALIZE       SUM-GG-AREA
                     PERFORM   320-HEAD-HEN-SEC
                   WHEN  "2"
      *    合計印字
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
      *    CSV合計編集処理
                     IF  WRK-PARA-GOKEIKBN  NOT =  SPACE
                       PERFORM  400-CSV-GOUKEI-SEC
                     END-IF
                     INITIALIZE      SUM-GG-AREA
                     PERFORM   320-HEAD-HEN-SEC
                   WHEN  OTHER
      *    小計印字
                     PERFORM  3001-SYOUKEI-SET-SEC
                     PERFORM  390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
      *    帳票編集<見出し>処理
                     PERFORM  310-HEAD-HEN-SEC
                 END-EVALUATE
                 MOVE   ZERO                       TO  CNT-LINE
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO  WRK-SRYKA
      *
      *    ＣＳＶ出力処理
                 PERFORM  230-TOUKEICSV-SEC
      *
      *    CSVヘッダ編集処理
                 PERFORM  800-CSV-HEAD-SEC
               END-IF
      *
      *    診療科ブレイクチェック処理
               IF  WRK-SMEI-SRYKA(IDX-S)  NOT =  WRK-SRYKA
      *
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO  WRK-SRYKA
      *
                 EVALUATE   WRK-PARA-GOKEIKBN
                   WHEN  "1"
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
      *    帳票編集<見出し>処理
                     PERFORM   310-HEAD-HEN-SEC
      *    合計印字
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
      *    CSV合計編集処理
                     IF  WRK-PARA-GOKEIKBN  NOT =  SPACE
                       PERFORM  400-CSV-GOUKEI-SEC
                     END-IF
                     INITIALIZE       SUM-GG-AREA
      *
                     PERFORM   320-HEAD-HEN-SEC
      *
                   WHEN  "2"
      *    合計印字
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
      *    CSV合計編集処理
                     IF  WRK-PARA-GOKEIKBN  NOT =  SPACE
                       PERFORM  400-CSV-GOUKEI-SEC
                     END-IF
                     INITIALIZE       SUM-GG-AREA
                     PERFORM   320-HEAD-HEN-SEC
                   WHEN  OTHER
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
                     PERFORM  310-HEAD-HEN-SEC
                 END-EVALUATE
      *
                 INITIALIZE                    SUM-G-AREA
      *
                 MOVE    ZERO                      TO   CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO   WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO   WRK-SRYKA
      *
      *    ＣＳＶ出力処理
                 PERFORM  230-TOUKEICSV-SEC
      *
      *    CSVヘッダ編集処理
                 PERFORM  800-CSV-HEAD-SEC
               END-IF
      *
      *    患者情報読み込み
               INITIALIZE           PTINF-REC
               MOVE   WRK-SMEI-HOSPNUM(IDX-S)  TO   PTINF-HOSPNUM
               MOVE   WRK-SMEI-PTID(IDX-S)     TO   PTINF-PTID
               PERFORM   900-PTINF-READ-SEC
      *
               ADD     1             TO  CNT-LINE
      *    伝票番号
               MOVE   WRK-SMEI-DENPNUM(IDX-S)  TO  HC17-RENNUM(CNT-LINE)
                                               HC17V03-RENNUM (CNT-LINE)
      *    患者番号
               INITIALIZE   ORCSPTIDAREA
               MOVE   WRK-PARA-HOSPNUM        TO   SPTID-HOSPNUM
               MOVE   WRK-SMEI-PTID(IDX-S)    TO   SPTID-PTID
               CALL   "ORCSPTID"              USING
                                              ORCSPTIDAREA
                                              SPA-AREA
               IF    SPTID-RC  =  ZERO
                 IF   SPTID-PTNUM(11:)  =  SPACE
                   MOVE    SPTID-PTNUM        TO   HC17-PTNUM(CNT-LINE)
                                                HC17V03-PTNUM(CNT-LINE)
                 ELSE
                   MOVE    SPTID-PTNUM        TO   HC17-PTNUM2(CNT-LINE)
                                                HC17V03-PTNUM2(CNT-LINE)
                 END-IF
               ELSE
                 MOVE    SPACE         TO   HC17-PTNUM(CNT-LINE)
                                            HC17V03-PTNUM(CNT-LINE)
               END-IF
      *    氏名
               MOVE    PTINF-NAME      TO   HC17-NAME(CNT-LINE)
                                            HC17V03-NAME(CNT-LINE)
      *
      *    伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
               IF  WRK-SMEI-DENPEDANUM(IDX-S)  =  1
                 IF  FLG-SYUNOU  = ZERO
                   IF WRK-PARA-DENPPRTYMD < "20080401"
                     PERFORM  320-BODY-HEN-SUB-SEC
                   ELSE
                     PERFORM  330-BODY-HEN-SUB-SEC
                   END-IF
                 END-IF
               END-IF
      *
      *    請求金額
               INITIALIZE  WRK-SZ
               MOVE    WRK-SMEI-SKYMONEY(IDX-S)   TO  WRK-SZ
               MOVE    WRK-SZ      TO  HC17-SEIKYU-G(CNT-LINE)
                                       HC17V03-SEIKYU-G(CNT-LINE)
               IF  WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                 ADD  WRK-SMEI-SKYMONEY(IDX-S)   TO  SUM-SEIKYU
                 ADD  1            TO  SUM-SEIKYUKEN
      *
                 ADD  WRK-SMEI-SKYMONEY(IDX-S)   TO  SUM-G-SEIKYU
                 ADD  WRK-SMEI-SKYMONEY(IDX-S)   TO  SUM-GG-SEIKYU
      *
               END-IF
      *
      *    領収金額
               INITIALIZE  WRK-SZ
               MOVE    WRK-SMEI-NYUHEN-MONEY(IDX-S)  TO   WRK-SZ
               MOVE    WRK-SZ         TO  HC17-RYOSYU-G(CNT-LINE)
                                          HC17V03-RYOSYU-G(CNT-LINE)
               IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                 ADD  WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   SUM-RYOSYU
                 ADD    1             TO  SUM-RYOSYUKEN
      *
                 ADD  WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   SUM-G-RYOSYU
                 ADD  WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   SUM-GG-RYOSYU
      *
               END-IF
             END-IF
      *
      *    CSV編集処理
             IF  WRK-PARA-GOKEIKBN  NOT =  "2"
               PERFORM  400-CSV-DATA-SEC
             END-IF
      *
             ADD   1        TO  IDX-S
             IF  IDX-S   >   IDX-MAX
               MOVE    1           TO  FLG-END
      *
      *    CSV合計編集処理
               IF  WRK-PARA-GOKEIKBN  NOT =  SPACE
                 PERFORM  400-CSV-GOUKEI-SEC
               END-IF
             END-IF
      *
           END-PERFORM
      *
      *    小計編集処理
           PERFORM  3001-SYOUKEI-SET-SEC
      *
      *    合計編集処理
           PERFORM  3000-GOUKEI-SET-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>サブ処理(H20年4月以前)
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
             MOVE    10        TO   HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE     ZERO       TO  SYU-TOTAL-HKNTEN
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ   >  16
             ADD    SYU-HKNTEN (IDZ)   TO   SYU-TOTAL-HKNTEN
           END-PERFORM
      *
           MOVE    SYU-TOTAL-HKNTEN    TO   WRK-Z9
           MOVE    WRK-Z9              TO   HC17-HKNTEN (CNT-LINE)
                                            HC17V03-HKNTEN (CNT-LINE)
           ADD     SYU-TOTAL-HKNTEN    TO   SUM-HKNTEN
           ADD     1                   TO   SUM-HKNKEN
      *
           ADD   SYU-TOTAL-HKNTEN      TO   SUM-G-HKNTEN
           ADD   SYU-TOTAL-HKNTEN      TO   SUM-GG-HKNTEN
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                  TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                 TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                TO   IDX
      *
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5               TO   IDX
               ELSE
                 MOVE    4               TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3               TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1           TO   IDX
                   WHEN    "2"
                     MOVE    2           TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2)   =   "98"
             MOVE     17        TO   IDX
               END-IF
      *
           IF   IDX   >   ZERO
      *
      *    自費以外のとき自費分だけ集計
             INITIALIZE     WRK-JIHI
             INITIALIZE     WRK-JIHI3
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE   WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TAX
      *
                 COMPUTE   WRK-JIHI3   =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TAX
               ELSE
                 COMPUTE   WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE   WRK-JIHI3   =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
               END-IF
             COMPUTE   WRK-MONEY     =   SYU-SKYMONEY
                                     -   WRK-JIHI3
      *
             MOVE   WRK-MONEY     TO   WRK-S9-2
             MOVE   WRK-S9-2      TO   HC17-MONEY(IDX CNT-LINE)
                                       HC17V03-MONEY(IDX CNT-LINE)
             ADD   WRK-MONEY     TO   SUM-MONEY(IDX)
             ADD   1             TO   SUM-KEN(IDX)
      *
             ADD   WRK-MONEY     TO   SUM-G-MONEY (IDX)
             ADD   WRK-MONEY     TO   SUM-GG-MONEY(IDX)
      *
               IF   WRK-JIHI  >  ZERO
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL   IDX2  >  10
                   INITIALIZE WRK-JIHI2
                   COMPUTE WRK-JIHI2   =   SYU-JIHI-TAXNASI(IDX2)
                                       +   SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2  >  ZERO
                     MOVE   WRK-JIHI2      TO   WRK-ZZ
                     COMPUTE JIHIIDX = IDX2 + 6
                     MOVE   WRK-ZZ    TO   HC17-MONEY(JIHIIDX CNT-LINE)
                                        HC17V03-MONEY(JIHIIDX CNT-LINE)
                     ADD   WRK-JIHI2  TO   SUM-MONEY(JIHIIDX)
                     ADD   1          TO   SUM-KEN(JIHIIDX)
      *
                     ADD   WRK-JIHI2  TO   SUM-G-MONEY(JIHIIDX)
                     ADD   WRK-JIHI2  TO   SUM-GG-MONEY(JIHIIDX)
      *
                   END-IF
                 END-PERFORM
               END-IF
           END-IF
      *
           EVALUATE IDX
             WHEN 17
      *    自費の場合
               MOVE   SYU-SKYMONEY         TO   WRK-SYUEKI
             WHEN 18
      *    労災の場合
      *    患者労災保険情報取得
               PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
               IF  PTRSI-HKNKBN  =  "5"
                   AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                 MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
               ELSE
                 MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
               END-IF
      *
               COMPUTE  WRK-SYUEKI ROUNDED   =
                        (SYU-TOTAL-HKNTEN  *  WRK-TENTNK)
                               +  WRK-JIHI
                               +  SYU-RSISHOSHIN-MONEY
                               +  SYU-RSISAISHIN-MONEY
                               +  SYU-RSISDO-MONEY
                               +  SYU-RSIETC-MONEY
                               +  SYU-CHOSEI
                               +  SYU-YKZ-FTN
                               +  SYU-RYOYOHI-SKJ
                               +  SYU-RYOYOHI-SKJ-JIHI
                               +  SYU-RYOYOHI-LIFE
                               +  SYU-RYOYOHI-LIFE-JIHI
                               +  SYU-RMSAGAKU
             WHEN 19
      *    自賠責の場合
               MOVE   SYU-RSIJIBAI-SKYMONEY   TO   WRK-SYUEKI
             WHEN 20
               COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    労災、自賠責、自費以外の場合
               COMPUTE   WRK-SYUEKI   =  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                                      +  WRK-JIHI
                                      +  SYU-CHOSEI
                                      +  SYU-YKZ-FTN
                                      +  SYU-RYOYOHI-SKJ
                                      +  SYU-RYOYOHI-SKJ-JIHI
                                      +  SYU-RYOYOHI-LIFE
                                      +  SYU-RYOYOHI-LIFE-JIHI
                                      +  SYU-RMSAGAKU
               IF  SYU-DISCOUNT-MONEY  >  0
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  3
                     IF   WRK-SYUEKI(7:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
           MOVE   WRK-SYUEKI   TO   WRK-SZ
           MOVE   WRK-SZ       TO   HC17-SYUEKI (CNT-LINE)
                                    HC17V03-SYUEKI (CNT-LINE)
      *
           IF   WRK-SYUEKI  NOT =  0
             ADD  WRK-SYUEKI   TO   SUM-G-SYUEKI
             ADD  WRK-SYUEKI   TO   SUM-GG-SYUEKI
           END-IF
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細行>サブ処理(H20年4月以降)
      *****************************************************************
       330-BODY-HEN-SUB-SEC      SECTION.
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
             MOVE 10     TO HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO          TO   SYU-TOTAL-HKNTEN
      *
           PERFORM  VARYING  IDZ   FROM  1   BY  1
                    UNTIL   IDZ  >  16
             ADD   SYU-HKNTEN(IDZ)     TO   SYU-TOTAL-HKNTEN
           END-PERFORM
      *
           MOVE   SYU-TOTAL-HKNTEN      TO   WRK-Z9
           MOVE   WRK-Z9                TO   HC17-HKNTEN (CNT-LINE)
                                             HC17V03-HKNTEN (CNT-LINE)
           ADD    SYU-TOTAL-HKNTEN      TO   SUM-HKNTEN
           ADD    1                     TO   SUM-HKNKEN
      *
           ADD    SYU-TOTAL-HKNTEN      TO   SUM-G-HKNTEN
           ADD    SYU-TOTAL-HKNTEN      TO   SUM-GG-HKNTEN
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                     TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE   6                     TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE   20                    TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE   19                    TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE   18                    TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN   "060"
             WHEN   "067"
             WHEN   "068"
             WHEN   "069"
               IF   SYU-KOH1HKNNUM   =  "027"
                 MOVE    5                  TO   IDX
               ELSE
                 MOVE    4                  TO   IDX
               END-IF
             WHEN   "039"
             WHEN   "040"
               MOVE        5                TO   IDX
             WHEN   OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5                  TO   IDX
               ELSE
                 PERFORM   3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1              TO   IDX
                   WHEN    "2"
                     MOVE    2              TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17    TO      IDX
           END-IF
      *
           IF   IDX   >   ZERO
      *
      *      自費診療  端数処理
             PERFORM  340-JIHIHASU-HEN-SEC
      *
      *    自費以外のとき自費分だけ集計
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   WRK-JIHI-TTL
               END-IF
             COMPUTE    WRK-MONEY    =   SYU-SKYMONEY  -  WRK-JIHI3
      *
             MOVE    WRK-MONEY     TO   WRK-S9-2
             MOVE    WRK-S9-2      TO   HC17-MONEY(IDX CNT-LINE)
                                        HC17V03-MONEY(IDX CNT-LINE)
             ADD     WRK-MONEY     TO   SUM-MONEY(IDX)
             ADD     1             TO   SUM-KEN(IDX)
      *
             ADD     WRK-MONEY     TO   SUM-G-MONEY(IDX)
             ADD     WRK-MONEY     TO   SUM-GG-MONEY(IDX)
      *
             IF   WRK-JIHI    >   ZERO
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2   >   10
                 INITIALIZE WRK-JIHI2
                 COMPUTE   WRK-JIHI2  =   SYU-JIHI-TAXNASI(IDX2)
                                      +   SYU-JIHI-TAXARI(IDX2)
                 IF   WRK-JIHI2  >  ZERO
                   MOVE WRK-JIHI2  TO  WRK-ZZ
                   COMPUTE  JIHIIDX  =  IDX2  +  6
                   MOVE   WRK-ZZ     TO   HC17-MONEY(JIHIIDX CNT-LINE)
                                        HC17V03-MONEY(JIHIIDX CNT-LINE)
                   ADD    WRK-JIHI2  TO   SUM-MONEY(JIHIIDX)
                   ADD    1          TO   SUM-KEN(JIHIIDX)
      *
                   ADD    WRK-JIHI2  TO   SUM-G-MONEY(JIHIIDX)
                   ADD    WRK-JIHI2  TO   SUM-GG-MONEY(JIHIIDX)
      *
                 END-IF
               END-PERFORM
               MOVE   WRK-JIHI-TTL   TO  WRK-ZZ
               MOVE   WRK-ZZ         TO  HC17V03-JIHI-KEI(CNT-LINE)
               ADD    WRK-JIHI-TTL   TO  SUM-G-JIHIKEI
                                         SUM-GG-JIHIKEI
             END-IF
           END-IF
      *
           EVALUATE IDX
             WHEN 17
      *    自費の場合
               MOVE  SYU-SKYMONEY  TO   WRK-SYUEKI
             WHEN 18
      *    労災の場合
      *    患者労災保険情報取得
               PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
               IF  PTRSI-HKNKBN  =  "5"
                   AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                 MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
               ELSE
                 MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
               END-IF
      *
               COMPUTE  WRK-SYUEKI ROUNDED   =
                             (SYU-TOTAL-HKNTEN * WRK-TENTNK)
                                +  WRK-JIHI
                                +  SYU-RSISHOSHIN-MONEY
                                +  SYU-RSISAISHIN-MONEY
                                +  SYU-RSISDO-MONEY
                                +  SYU-RSIETC-MONEY
                                +  SYU-CHOSEI
                                +  SYU-YKZ-FTN
                                +  SYU-RYOYOHI-SKJ
                                +  SYU-RYOYOHI-SKJ-JIHI
                                +  SYU-RYOYOHI-LIFE
                                +  SYU-RYOYOHI-LIFE-JIHI
                                +  SYU-RMSAGAKU
             WHEN 19
      *    自賠責の場合
               MOVE  SYU-RSIJIBAI-SKYMONEY  TO  WRK-SYUEKI
      *    公害の場合
             WHEN 20
               COMPUTE   WRK-SYUEKI  =  SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    労災、自賠責、自費、公害以外の場合
               COMPUTE  WRK-SYUEKI  =
                          (SYU-TOTAL-HKNTEN  *  HKN-TENTNK)
                             +  WRK-JIHI
                             +  SYU-CHOSEI
                             +  SYU-YKZ-FTN
                             +  SYU-RYOYOHI-SKJ
                             +  SYU-RYOYOHI-SKJ-JIHI
                             +  SYU-RYOYOHI-LIFE
                             +  SYU-RYOYOHI-LIFE-JIHI
                             +  SYU-RMSAGAKU
               IF   SYU-DISCOUNT-MONEY  >  0
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  3
                     IF   WRK-SYUEKI(7:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
           MOVE  WRK-SYUEKI   TO   WRK-SZ
           MOVE  WRK-SZ       TO   HC17-SYUEKI(CNT-LINE)
                                   HC17V03-SYUEKI(CNT-LINE)
      *
           IF   WRK-SYUEKI  NOT =  0
               ADD  WRK-SYUEKI   TO   SUM-G-SYUEKI
               ADD  WRK-SYUEKI   TO   SUM-GG-SYUEKI
           END-IF
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費診療  端数処理
      *****************************************************************
       340-JIHIHASU-HEN-SEC          SECTION.
      *
           COMPUTE  WRK-JIHI-TTL  =  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
      *
           EVALUATE  SYU-SYUHKNNUM
      *    労災
           WHEN   "971"
             MOVE    SYS-1001-DISCOUNT-HASU-KBN3   TO  WRK-HASU-KBN
             MOVE    SYS-1001-DISCOUNT-HASU-KBN3J  TO  WRK-HASU-KBNJ
      *    自賠責
           WHEN   "973"
             MOVE    SYS-1001-DISCOUNT-HASU-KBN4   TO  WRK-HASU-KBN
             MOVE    SYS-1001-DISCOUNT-HASU-KBN4J  TO  WRK-HASU-KBNJ
      *    公害
           WHEN   "975"
             MOVE    SYS-1001-DISCOUNT-HASU-KBN5J  TO  WRK-HASU-KBNJ
      *    医保
           WHEN   OTHER
             MOVE    SYS-1001-DISCOUNT-HASU-KBN2   TO  WRK-HASU-KBN
             MOVE    SYS-1001-DISCOUNT-HASU-KBN2J  TO  WRK-HASU-KBNJ
      *      第三者行為
             IF  SYU-KOH1HKNNUM  =  "970"
               MOVE    SYS-1001-DISCOUNT-HASU-KBN6   TO  WRK-HASU-KBN
               MOVE    SYS-1001-DISCOUNT-HASU-KBN6J  TO  WRK-HASU-KBNJ
             END-IF
           END-EVALUATE
      *
      *    自費診療  端数サブ処理
           PERFORM  3401-JIHIHASU-SUB-SEC
      *
           .
       340-JIHIHASU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費診療  端数サブ処理
      *****************************************************************
       3401-JIHIHASU-SUB-SEC          SECTION.
      *
      *    請求額端数設定（減免無・自費分）
           EVALUATE  WRK-HASU-KBNJ
      *    １０円未満四捨五入
           WHEN  "1"
             COMPUTE  WRK-JIHI-TTL ROUNDED = WRK-JIHI-TTL / 10
             COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *    １０円未満切り捨て
           WHEN  "2"
             COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL / 10
             COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *    １０円未満切り上げ
           WHEN  "3"
             IF  WRK-JIHI-TTL(7:1)  NOT =  ZERO
               COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL / 10
               COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL * 10 + 10
             END-IF
      *    保険分に準ずる
           WHEN  "0"
      *      請求額端数設定（減免無・保険分）
             EVALUATE  WRK-HASU-KBN
      *      １０円未満四捨五入
             WHEN  "1"
               COMPUTE  WRK-JIHI-TTL ROUNDED = WRK-JIHI-TTL / 10
               COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *      １０円未満切り捨て
             WHEN  "2"
               COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL / 10
               COMPUTE  WRK-JIHI-TTL         = WRK-JIHI-TTL * 10
      *      １０円未満切り上げ
             WHEN  "3"
               IF  WRK-JIHI-TTL(7:1)  NOT =  ZERO
                 COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL / 10
                 COMPUTE  WRK-JIHI-TTL       = WRK-JIHI-TTL * 10 + 10
               END-IF
             WHEN  OTHER
               CONTINUE
             END-EVALUATE
      *    １０円未満端数処理なし
           WHEN  OTHER
             CONTINUE
           END-EVALUATE
      *
           .
       3401-JIHIHASU-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    小計印字処理
      *****************************************************************
       3001-SYOUKEI-SET-SEC          SECTION.
      *
      *    保険点数
           IF  (      SUM-G-HKNTEN  >   999999
                  OR  SUM-G-HKNTEN  <  -999999  )
      *    8桁の場合
             INITIALIZE  WRK-SZ
             MOVE    SUM-G-HKNTEN       TO   WRK-SZ
             MOVE    WRK-SZ             TO   HC17-GG-HKNTEN
                                             HC17V03-GG-HKNTEN
           ELSE
      *    7桁以下の場合
             INITIALIZE  WRK-Z9-2
             MOVE    SUM-G-HKNTEN       TO   WRK-Z9-2
             MOVE    WRK-Z9-2           TO   HC17-G-HKNTEN
                                             HC17V03-G-HKNTEN
           END-IF
      *    保険別金額（社保本人〜自賠責）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
                    UNTIL    IDX-SUM   >    20
             IF  (      SUM-G-MONEY (IDX-SUM)  >   999999
                    OR  SUM-G-MONEY (IDX-SUM)  <  -999999  )
      *    8桁の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-MONEY(IDX-SUM)   TO   WRK-SZ
               MOVE     WRK-SZ   TO   HC17-GG-MONEY(IDX-SUM)
                                      HC17V03-GG-MONEY(IDX-SUM)
             ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-Z9-2
               MOVE     SUM-G-MONEY (IDX-SUM)   TO   WRK-Z9-2
               MOVE     WRK-Z9-2   TO   HC17-G-MONEY(IDX-SUM)
                                        HC17V03-G-MONEY(IDX-SUM)
             END-IF
           END-PERFORM
      *    医療収益
           IF  (      SUM-G-SYUEKI  >   9999999
                  OR  SUM-G-SYUEKI  <  -9999999  )
      *    8桁の場合
               INITIALIZE  WRK-SZZ
             MOVE    SUM-G-SYUEKI     TO   WRK-SZZ
             MOVE    WRK-SZZ          TO   HC17-GG-SYUEKI
                                           HC17V03-GG-SYUEKI
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
             MOVE    SUM-G-SYUEKI     TO   WRK-SZ
             MOVE    WRK-SZ           TO   HC17-G-SYUEKI
                                           HC17V03-G-SYUEKI
           END-IF
      *    請求金額
           IF  (      SUM-G-SEIKYU  >   9999999
                  OR  SUM-G-SEIKYU  <  -9999999  )
      *    8桁の場合
             INITIALIZE               WRK-SZZ
             MOVE    SUM-G-SEIKYU     TO   WRK-SZZ
             MOVE    WRK-SZZ          TO   HC17-GG-SEIKYU
                                           HC17V03-GG-SEIKYU
           ELSE
      *    7桁以下の場合
             INITIALIZE               WRK-SZ
             MOVE    SUM-G-SEIKYU     TO   WRK-SZ
             MOVE    WRK-SZ           TO   HC17-G-SEIKYU
                                           HC17V03-G-SEIKYU
           END-IF
      *    領収金額
           IF  (      SUM-G-RYOSYU  >   9999999
                  OR  SUM-G-RYOSYU  <  -9999999  )
      *    8桁の場合
             INITIALIZE               WRK-SZZ
             MOVE    SUM-G-RYOSYU     TO   WRK-SZZ
             MOVE    WRK-SZZ          TO   HC17-GG-RYOSYU
                                           HC17V03-GG-RYOSYU
           ELSE
      *    7桁以下の場合
             INITIALIZE               WRK-SZ
             MOVE    SUM-G-RYOSYU     TO   WRK-SZ
             MOVE    WRK-SZ           TO   HC17-G-RYOSYU
                                           HC17V03-G-RYOSYU
           END-IF
      *    自費小計
           IF  (      SUM-G-JIHIKEI  >   999999
                  OR  SUM-G-JIHIKEI  <  -999999  )
      *    8桁の場合
             INITIALIZE               WRK-SZ
             MOVE    SUM-G-JIHIKEI    TO   WRK-SZ
             MOVE    WRK-SZ           TO   HC17V03-GG-JIHIKEI
           ELSE
      *    7桁以下の場合
             INITIALIZE               WRK-Z9-2
             MOVE    SUM-G-JIHIKEI    TO   WRK-Z9-2
             MOVE    WRK-Z9-2         TO   HC17V03-G-JIHIKEI
           END-IF
           .
       3001-SYOUKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計印字処理
      *****************************************************************
       3000-GOUKEI-SET-SEC          SECTION.
      *
      *    保険点数
           INITIALIZE   WRK-SZZ
           MOVE    SUM-GG-HKNTEN    TO   WRK-SZZ
           MOVE    WRK-SZZ          TO   D5002-HKNTEN
                                         D5002V03-HKNTEN
      *
      *    保険別金額（社保本人〜自賠責）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
                    UNTIL    IDX-SUM   >   20
             INITIALIZE  WRK-SZ
             MOVE    SUM-GG-MONEY(IDX-SUM)   TO   WRK-SZ
             MOVE    WRK-SZ         TO   D5002-MONEY(IDX-SUM)
                                         D5002V03-MONEY(IDX-SUM)
           END-PERFORM
      *
      *    医療収益
           INITIALIZE   WRK-SZ10
           MOVE    SUM-GG-SYUEKI    TO   WRK-SZ10
           MOVE    WRK-SZ10         TO   D5002-SYUEKI
                                         D5002V03-SYUEKI
      *
      *    請求金額
           INITIALIZE   WRK-SZ10
           MOVE    SUM-GG-SEIKYU    TO   WRK-SZ10
           MOVE    WRK-SZ10         TO   D5002-SEIKYU
                                         D5002V03-SEIKYU
      *
      *    領収金額
           INITIALIZE   WRK-SZ10
           MOVE    SUM-GG-RYOSYU    TO   WRK-SZ10
           MOVE    WRK-SZ10         TO   D5002-RYOSYU
                                         D5002V03-RYOSYU
      *
      *    自費小計
           INITIALIZE   WRK-SZ
           MOVE    SUM-GG-JIHIKEI   TO   WRK-SZ
           MOVE    WRK-SZ           TO   D5002V03-JIHIKEI
      *
           .
       3000-GOUKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM       TO   COMB-HOSPNUM
           MOVE    SYU-PTID          TO   COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI   =   ZERO
      *    患者保険情報読み込み
             INITIALIZE              PTHKNINF-REC
             MOVE    SYU-HOSPNUM     TO  PTHKN-HOSPNUM
             MOVE    SYU-PTID        TO  PTHKN-PTID
             MOVE    COMB-HKNID      TO  PTHKN-HKNID
             PERFORM 900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                       TO   CNT-PAGE
           MOVE    CNT-PAGE                TO   WRK-PAGE
           MOVE    WRK-PAGE                TO   HC17-PAGE
                                                HC17V03-PAGE
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *
           IF   WRK-PARA-DENPPRTYMD  >=  "20150201"
             MOVE   "A00000D500V03.red"          TO   SPRT-PRTID
             MOVE    "日計表明細（窓口領収金）"  TO   SPRT-TITLE
             MOVE    HC17V03                     TO   SPRT-PRTDATA
           ELSE
             IF   WRK-PARA-DENPPRTYMD  <  "20080401"
               MOVE   "A00000D500O.red"      TO   SPRT-PRTID
             ELSE
               MOVE   "A00000D500V02.red"    TO   SPRT-PRTID
             END-IF
             MOVE    "日計表明細（窓口領収金）"   TO   SPRT-TITLE
             MOVE    HC17                  TO   SPRT-PRTDATA
           END-IF
      *
           MOVE    LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID     TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
           CALL    "ORCSPRT"             USING
                                         ORCSPRTAREA
                                         SPA-AREA
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    総合計帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                           TO   CNT-PAGE
           MOVE    CNT-PAGE                    TO   WRK-PAGE
           MOVE    WRK-PAGE                    TO   D5002-PAGE
                                                    D5002V03-PAGE
      *
           INITIALIZE                          ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM         TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY        TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM          TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD         TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID        TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY       TO   SPRT-PRIORITY
      *
           IF   WRK-PARA-DENPPRTYMD  >=  "20150201"
             MOVE   "A00000D500NV03.red"         TO   SPRT-PRTID
             MOVE    "日計表明細（窓口領収金）"  TO   SPRT-TITLE
             MOVE    D5002V03                    TO   SPRT-PRTDATA
           ELSE
             IF   WRK-PARA-DENPPRTYMD  <  "20080401"
               MOVE   "A00000D500O2.red"         TO   SPRT-PRTID
             ELSE
               MOVE   "A00000D500N2.red"         TO   SPRT-PRTID
             END-IF
             MOVE    "日計表明細（窓口領収金）"  TO   SPRT-TITLE
             MOVE    D5002                       TO   SPRT-PRTDATA
           END-IF
      *
           MOVE    LNK-PRTKANRI-TERMID         TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID           TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM          TO   SPRT-PRTNM
           CALL    "ORCSPRT"                   USING
                                               ORCSPRTAREA
                                               SPA-AREA
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                TO  IDXP
      *    入外区分セット
           IF  WRK-NYUGAIKBN  =  1
             MOVE    "入外区分：入院"           TO  WRK-ITEM
           ELSE
             MOVE    "入外区分：外来"           TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *    診療科セット
      *    診療科名称取得
           INITIALIZE                   SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO  SYS-1005-EDYUKYMD
                                            SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM  820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE    MCPDATA-REC        TO  SYS-1005-REC
             INITIALIZE                 WRK-ITEM
             STRING    "診療科："           DELIMITED  BY  SIZE
                       SYS-1005-SRYKANAME   DELIMITED  BY  SIZE
                                            INTO  WRK-ITEM
             END-STRING
           ELSE
             MOVE    SPACE              TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付セット
           MOVE    WRK-SYSYMDWH     TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    保険名称セット
           IF   WRK-PARA-DENPPRTYMD  <  "20080401"
             MOVE    "社保本人"     TO  CSV-HEAD-HKN1
             MOVE    "社保家族"     TO  CSV-HEAD-HKN2
             MOVE    "社保老人"     TO  CSV-HEAD-HKN3
             MOVE    "国保一般"     TO  CSV-HEAD-HKN4
             MOVE    "国保老人"     TO  CSV-HEAD-HKN5
             MOVE    "公費単独"     TO  CSV-HEAD-HKN6
           ELSE
             MOVE    "社保本人"     TO  CSV-HEAD-HKN1
             MOVE    "社保家族"     TO  CSV-HEAD-HKN2
             MOVE    "国保"         TO  CSV-HEAD-HKN3
             MOVE    "後期高齢"     TO  CSV-HEAD-HKN4
             MOVE    SPACE          TO  CSV-HEAD-HKN5
             MOVE    "公費単独"     TO  CSV-HEAD-HKN6
           END-IF
      *
      *    自費診療行為名称セット
           PERFORM  VARYING  IDX-JIHI  FROM  1  BY  1
                    UNTIL   IDX-JIHI  >  10
             IF  WRK-NYUGAIKBN  =  1
               MOVE    WRK-TJIHIN(IDX-JIHI)
                                    TO  CSV-HEAD-JIHI-OCC(IDX-JIHI)
             ELSE
               MOVE    WRK-TJIHI(IDX-JIHI)
                                    TO  CSV-HEAD-JIHI-OCC(IDX-JIHI)
             END-IF
           END-PERFORM
      *
      *    項目名称セット
           IF   WRK-PARA-DENPPRTYMD  >=  "20150201"
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL  IDX4  >  CSV-HEAD-MAX
               IF  IDX4  NOT =  10
                 MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
                 IF   IDX4  >=  CSV-HEAD-MAX
                   PERFORM  810-CSVDATA-EDIT-SEC
                 ELSE
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
           ELSE
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL  IDX4  >  CSV-HEAD-MAX
               IF  IDX4  NOT =  22
                 IF  IDX4  NOT =  10
                   MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
                   IF   IDX4  >=  CSV-HEAD-MAX
                     PERFORM  810-CSVDATA-EDIT-SEC
                   ELSE
                     PERFORM  800-CSVDATA-EDIT-SEC
                   END-IF
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           IF   WRK-PARA-DENPPRTYMD  >=  "20150201"
             PERFORM  400-CSV-DATA-V03-SEC
           ELSE
             PERFORM  400-CSV-DATA-V02-SEC
           END-IF
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理(201502以前)
      *****************************************************************
       400-CSV-DATA-V02-SEC            SECTION.
      *
           MOVE    HC17-RENNUM(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    SPTID-PTNUM              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名
           MOVE    HC17-NAME(CNT-LINE)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険点数
           MOVE    HC17-HKNTEN(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    医療収益
           MOVE    HC17-SYUEKI(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    窓口請求額（集計区分別）
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                    UNTIL  IDX5  >  20
             IF  IDX5  NOT =  3
               MOVE    HC17-MONEY(IDX5 CNT-LINE)    TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *    窓口請求額
           MOVE    HC17-SEIKYU(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    窓口領収額
           MOVE    HC17-RYOSYU(CNT-LINE)    TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-DATA-V02-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理(201502以降)
      *****************************************************************
       400-CSV-DATA-V03-SEC            SECTION.
      *
      *    伝票番号
           MOVE    HC17V03-RENNUM(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    SPTID-PTNUM              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名
           MOVE    HC17V03-NAME(CNT-LINE)      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険点数
           MOVE    HC17V03-HKNTEN(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    医療収益
           MOVE    HC17V03-SYUEKI(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    窓口請求額（集計区分別）
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                    UNTIL  IDX5  >  20
             IF  IDX5  NOT =  3
               MOVE    HC17V03-MONEY(IDX5 CNT-LINE)    TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
             IF  IDX5  =  16
               MOVE    HC17V03-JIHI-KEI(CNT-LINE)      TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *    窓口請求額
           MOVE    HC17V03-SEIKYU(CNT-LINE)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    窓口領収額
           MOVE    HC17V03-RYOSYU(CNT-LINE)    TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-DATA-V03-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計編集処理
      *****************************************************************
       400-CSV-GOUKEI-SEC            SECTION.
      *
           IF   WRK-PARA-DENPPRTYMD  >=  "20150201"
             PERFORM  400-CSV-GOUKEI-V03-SEC
           ELSE
             PERFORM  400-CSV-GOUKEI-V02-SEC
           END-IF
      *
           .
       400-CSV-GOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計編集処理(201502以前)
      *****************************************************************
       400-CSV-GOUKEI-V02-SEC            SECTION.
      *
      *    伝票番号
           MOVE    SPACE            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    SPACE            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名（総合計）
           MOVE    "総合計"         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険点数
           INITIALIZE               WRK-SZZ
           MOVE    SUM-GG-HKNTEN    TO  WRK-SZZ
           MOVE    WRK-SZZ          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    医療収益
           MOVE    SUM-GG-SYUEKI    TO  WRK-SZ10
           MOVE    WRK-SZ10         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険別金額（社保本人〜自賠責）
           PERFORM  VARYING  IDX-SUM  FROM  1  BY  1
                    UNTIL  IDX-SUM  >  20
             IF  IDX-SUM  NOT =  3
               INITIALIZE           WRK-SZ
               MOVE    SUM-GG-MONEY(IDX-SUM)   TO  WRK-SZ
               MOVE    WRK-SZ       TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    請求金額
           INITIALIZE               WRK-SZ10
           MOVE    SUM-GG-SEIKYU    TO  WRK-SZ10
           MOVE    WRK-SZ10         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    領収金額
           INITIALIZE               WRK-SZ10
           MOVE    SUM-GG-RYOSYU    TO  WRK-SZ10
           MOVE    WRK-SZ10         TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-GOUKEI-V02-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV合計編集処理(201502以降)
      *****************************************************************
       400-CSV-GOUKEI-V03-SEC            SECTION.
      *
      *    伝票番号
           MOVE    SPACE            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    SPACE            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名（総合計）
           MOVE    "総合計"         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険点数
           INITIALIZE               WRK-SZZ
           MOVE    SUM-GG-HKNTEN    TO  WRK-SZZ
           MOVE    WRK-SZZ          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    医療収益
           MOVE    SUM-GG-SYUEKI    TO  WRK-SZ10
           MOVE    WRK-SZ10         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険別金額（社保本人〜自賠責）
           PERFORM  VARYING  IDX-SUM  FROM  1  BY  1
                    UNTIL  IDX-SUM  >  20
             IF  IDX-SUM  NOT =  3
               INITIALIZE           WRK-SZ
               MOVE    SUM-GG-MONEY(IDX-SUM)   TO  WRK-SZ
               MOVE    WRK-SZ       TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
             IF  IDX-SUM  =  16
               INITIALIZE           WRK-SZ
               MOVE    SUM-GG-JIHIKEI  TO  WRK-SZ
               MOVE    WRK-SZ          TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
      *    請求金額
           INITIALIZE               WRK-SZ10
           MOVE    SUM-GG-SEIKYU    TO  WRK-SZ10
           MOVE    WRK-SZ10         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    領収金額
           INITIALIZE               WRK-SZ10
           MOVE    SUM-GG-RYOSYU    TO  WRK-SZ10
           MOVE    WRK-SZ10         TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-GOUKEI-V03-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
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
           STRING    ","       DELIMITED  BY  SIZE
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
           STRING    X"0D"     DELIMITED  BY  SIZE
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
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
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
           IF   WRK-PARA-DENPPRTYMD  >=  "20080401"
             INITIALIZE                     ORCSTOUKEICSVAREA
             MOVE    "INS"                  TO  STOUKEICSV-MODE
             MOVE    LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
             MOVE    LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
             MOVE    LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
             MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
             MOVE    LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
             MOVE    LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
             MOVE    ZERO                   TO  STOUKEICSV-PTID
             MOVE    "/var/tmp/"            TO  STOUKEICSV-TO-FOLDER
      *
             STRING  SPA-HOSPNUM            DELIMITED  BY  SIZE
                     "nikkeimeisai_madoguchi_"  DELIMITED  BY  SIZE
                     WRK-PARA-DENPPRTYMD    DELIMITED  BY  SIZE
                     ".csv"                 DELIMITED  BY  SIZE
                                            INTO  STOUKEICSV-TO-DATA
             END-STRING
             MOVE    "2"                    TO  STOUKEICSV-CODE-TYPE
             MOVE    WRK-CSVDATA            TO  STOUKEICSV-CSVDATA
             MOVE    "日計表明細（窓口領収金）"    TO  STOUKEICSV-TITLE
      *
             CALL    "ORCSTOUKEICSV"     USING
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
           END-IF
      *
           MOVE    1                     TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF    STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"             TO   SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID    TO   JOB-JOBID
             MOVE    WRK-PARA-SHELLID  TO   JOB-SHELLID
             MOVE    WRK-RECEERR       TO   JOB-YOBI
             MOVE    "9999"            TO   JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO   FLG-END
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
      *    ステップ管理終了処理
           MOVE    "STE"               TO   SJOBKANRI-MODE
           INITIALIZE                       JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO   JOB-SHELLID
           MOVE    CNT-PAGE            TO   JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM   900-DBDISCONNECT-SEC
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
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO   LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO   WRK-HENYMDG
           INSPECT WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索(入院患者)
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO   SMEI-HOSPNUM
           MOVE    IDX-NG              TO   SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO   SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key4"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM 910-DBFETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理(入院患者)
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key4"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-SYUMEI
             MOVE    MCPDATA-REC       TO   SYUMEI-REC
           ELSE
               MOVE    1               TO   FLG-SYUMEI
           END-IF
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syumei"         TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                 SYUNOU-REC
           MOVE    SMEI-HOSPNUM    TO   SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN  TO   SYU-NYUGAIKBN
           MOVE    SMEI-PTID       TO   SYU-PTID
           MOVE    SMEI-DENPNUM    TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC      TO   MCPDATA-REC
           MOVE    "DBSELECT"      TO   MCP-FUNC
           MOVE    "tbl_syunou"    TO   MCP-TABLE
           MOVE    "key"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syunou"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   SYUNOU-REC
               MOVE    ZERO              TO   FLG-SYUNOU
             ELSE
               MOVE    1                 TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)     TO   SYU-HOSPNUM
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)        TO   SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)     TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC                  TO   MCPDATA-REC
           MOVE    "DBSELECT"                  TO   MCP-FUNC
           MOVE    "tbl_syunou"                TO   MCP-TABLE
           MOVE    "key"                       TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO   MCP-FUNC
             MOVE    "tbl_syunou"         TO   MCP-TABLE
             MOVE    "key"                TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF    MCP-RC  =   ZERO
               MOVE    MCPDATA-REC        TO   SYUNOU-REC
               MOVE    ZERO               TO   FLG-SYUNOU
             ELSE
               MOVE    1                  TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                    TO   FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"        TO   MCP-FUNC
           MOVE    "tbl_syunou"           TO   MCP-TABLE
           MOVE    "key"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                        HKNNUM-REC
           MOVE    SYU-HOSPNUM          TO   HKN-HOSPNUM
           MOVE    SYU-SYUHKNNUM        TO   HKN-HKNNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO   HKN-TEKSTYMD
           MOVE    ALL "9"              TO   HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_hknnum"       TO   MCP-TABLE
             MOVE    "key5"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =  ZERO
               MOVE    MCPDATA-REC      TO   HKNNUM-REC
               MOVE    ZERO             TO   FLG-HKNNUM
             ELSE
               MOVE    1                TO   FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI
             ELSE
               MOVE    1                TO   FLG-SYSKANRI
             END-IF
           ELSE
               MOVE    1                TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_syskanri"      TO   MCP-TABLE
             MOVE    "key10"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO              TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                 TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       820-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptinf"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTINF-REC
               MOVE    ZERO              TO   FLG-PTINF
             ELSE
               MOVE    1                 TO   FLG-PTINF
               INITIALIZE                PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスタ読込処理
      *****************************************************************
       900-PTNUM-READ-SEC         SECTION.
      *
           INITIALIZE                    PTNUM-REC
           MOVE    SMEI-PTID             TO   PTNUM-PTID
           MOVE    PTNUM-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptnum"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTNUM-REC
             ELSE
               INITIALIZE                PTNUM-REC
             END-IF
           ELSE
             INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_pthkninf"        TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_pthkninf"      TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTHKNINF-REC
               MOVE    ZERO              TO   FLG-PTHKNINF
             ELSE
               MOVE    1                 TO   FLG-PTHKNINF
               INITIALIZE                PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTHKNINF
             INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_pthkninf"        TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_hkncombi"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   HKNCOMBI-REC
               MOVE    ZERO             TO   FLG-HKNCOMBI
             ELSE
               MOVE    1                TO   FLG-HKNCOMBI
               INITIALIZE               HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNCOMBI
             INITIALIZE                 HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
      *    保険組合せ読み出し
           INITIALIZE                    HKNCOMBI-REC
           MOVE    SYU-HOSPNUM           TO  COMB-HOSPNUM
           MOVE    SYU-PTID              TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM       TO  COMB-HKNCOMBINUM
           PERFORM  900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI  =  ZERO
      *    患者保険情報読み込み
             INITIALIZE                  PTRSIINF-REC
             MOVE    SYU-HOSPNUM         TO  PTRSI-HOSPNUM
             MOVE    SYU-PTID            TO  PTRSI-PTID
             MOVE    COMB-HKNID          TO  PTRSI-HKNID
             MOVE    PTRSIINF-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    "DBFETCH"         TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF   MCP-RC  =  ZERO
                 MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               ELSE
                 INITIALIZE              PTRSIINF-REC
               END-IF
             END-IF
      *
             MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           END-IF
           .
       900-PTRSIINF-READ-EXT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO   MCP-FUNC.
           MOVE    LOW-VALUE           TO   MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                        .
      *
           MOVE    "DBSTART"           TO   MCP-FUNC.
           MOVE    LOW-VALUE           TO   MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCOMMIT"           TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
