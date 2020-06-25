      ******************************************************************
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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000TA02.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療行為別集計表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/02/14  新規作成
      *   01.00.01    吉川    2013/09/02  発行区分を病棟別に指定した場合
      *                                   病棟別に集計するように修正
      *                                   病棟名が印字されないのを修正
      *   01.00.02    門間    2013/09/13  金額欄の桁数変更
      *   01.00.03    門間    2013/10/03  食堂加算など加算手技料の修正
      *   01.00.04    門間    2013/11/07  食事療養を金額として集計するように修正
      *   01.00.05    門間    2013/11/20  病棟名、件数の修正
      *   01.00.06    門間    2014/03/12  該当医師なしと集計される不具合
      *                                   実際の回数より、多く集計される不具合
      *                                   自費診療の０円設定に対応
      *                                   包括区分「2」が処理対象となっていない不具合
      *   01.00.07    門間    2014/04/16  加算減算等は点数金額等は印字しない
      *   01.00.08    門間    2014/04/30  月を跨いだ場合、正しく集計されない不具合を修正
      *   01.00.09    門間    2014/05/22  自費の診療行為コードの場合、薬価／点数が０になる不具合を修正
      *                                   薬価／点数、件数、使用量を小数点第2位まで対応
      *   01.00.10    門間    2014/07/17  きざみ値処理を追加
      *   01.00.11    門間    2014/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *   01.00.12    門間    2015/04/16  在院時の病棟に集計されるよう修正
      *   01.00.13    門間    2015/07/24  CSV出力時、発行区分の印字不具合修正
      *   01.00.14    門間    2015/09/11  ＣＳＶ出力が正常に出力されない不具合を修正
      *   01.00.15    門間    2016/05/17  流動食対応
      *   01.00.16    門間    2016/08/22  次発行区分へ合計が合算される不具合を修正
      *   01.00.17    門間    2016/08/30  食事療養及び生活療養の点数（薬価）の算出方法、表示名称を修正
      *   01.00.18    門間    2016/09/16  入院分を対象とした場合の処理速度を改善
      *   01.00.19    萩野    2017/02/28  比率計算用区分別集計領域を[100]から[300]へ修正
      *   01.00.20    門間    2017/06/30  外来金額、入院金額欄の桁数を変更
      *                                   印字順序の改行時（ＣＳＶ出力）、
      *                                   グループ比率が印字されない不具合を修正
      *   01.00.21    門間    2017/09/19  区分別集計領域の上限を変更
      *   01.00.22    門間    2017/11/17  外泊時、回数減算処理を修正
      *   01.00.23    門間    2019/06/21  診療区分が「95」及び「96」の場合、自費金額を参照するように修正
      *   01.00.24    門間    2019/07/18  労災・自賠責の診断書料等の点数が印字されるよう修正
      *                                   自費項目の集計処理を修正
      *   01.00.25    門間    2020/02/20  包括判定処理を修正
      *******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    診療行為受診対象患者一時ファイル
           SELECT  TA02-FILE       ASSIGN          TA02P
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TA02TMP-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為受診対象患者一時ファイル
       FD  TA02-FILE.
           COPY    "A00000TA02TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE                 SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000TA02.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA02P//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                    PIC X(02).
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-JYURRK                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-INVALID                 PIC 9(01).
           03  FLG-PRINT                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-KENSU                   PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                    PIC 9(02).
           03  CNT-GPAGE                   PIC 9(02).
           03  CNT-LEN                     PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(02).
           03  IDX4                        PIC 9(02).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(03).
           03  IDX7                        PIC 9(03).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-POINT                   PIC 9(05).
           03  IDX-ST                      PIC 9(03).
           03  IDX-ED                      PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID              PIC 9(07).
           03  WRK-PARA-SHELLID            PIC X(08).
           03  WRK-PARA-HOSPNUM            PIC 9(02).
           03  WRK-PARA-PRTSTYMD           PIC X(08).
           03  WRK-PARA-PRTEDYMD           PIC X(08).
           03  WRK-PARA-NYUGAIKBN          PIC X(01).
           03  WRK-PARA-PRTKBN             PIC X(01).
           03  WRK-PARA-SRYKBN             PIC X(02).
           03  WRK-PARA-HOKATUKBN          PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-Z2                      PIC Z9.
           03  WRK-Z5                      PIC Z,ZZ9.
           03  WRK-V5                      PIC Z9.99.
           03  WRK-V5-2                    PIC ZZ9.9.
           03  WRK-Z6                      PIC ZZ,ZZ9.
           03  WRK-Z7                      PIC ZZZ,ZZ9.
           03  WRK-Z9                      PIC Z,ZZZ,ZZ9.
           03  WRK-Z10                     PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11                     PIC ZZZ,ZZZ,ZZ9.
           03  WRK-Z12                     PIC ZZZZ,ZZZ,ZZ9.
           03  WRK-V9                      PIC Z,ZZZ,ZZ9.
           03  WRK-V9-1                    PIC ZZZ,ZZ9.9.
           03  WRK-V9-2                    PIC ZZ,ZZ9.99.
           03  WRK-V10                     PIC ZZ,ZZZ,ZZ9.
           03  WRK-V10-1                   PIC ZZZZ,ZZ9.9.
           03  WRK-V10-2                   PIC ZZZ,ZZ9.99.
           03  WRK-V11                     PIC ZZZ,ZZZ,ZZ9.
           03  WRK-V11-1                   PIC Z,ZZZ,ZZ9.9.
           03  WRK-V11-2                   PIC ZZZZ,ZZ9.99.
           03  WRK-V12                     PIC Z,ZZZ,ZZ9.99.
           03  WRK-CSV-Z4                  PIC ZZZ9.
           03  WRK-CSV-V4                  PIC Z9.99.
           03  WRK-CSV-V4-2                PIC ZZ9.9.
           03  WRK-CSV-Z5                  PIC ZZZZ9.
           03  WRK-CSV-Z6                  PIC ZZZZZ9.
           03  WRK-CSV-Z7                  PIC ZZZZZZ9.
           03  WRK-CSV-Z8                  PIC ZZZZZZZ9.
           03  WRK-CSV-Z9                  PIC ZZZZZZZZ9.
           03  WRK-CSV-Z10                 PIC ZZZZZZZZZ9.
           03  WRK-CSV-Z11                 PIC ZZZZZZZZZZ9.
           03  WRK-CSV-V10                 PIC ZZZZZZ9.99.
           03  WRK-CSV-V11                 PIC ZZZZZZZ9.99.
           03  WRK-KINGAKU                 PIC S9(09).
           03  WRK-TENSU                   PIC 9(09)V9(02).
           03  WRK-SYMD                    PIC 9(08).
           03  WRK-HENYMDG                 PIC X(22).
           03  WRK-AMARI                   PIC 9(02).
           03  WRK-GPAGE                   PIC 9(02).
           03  WRK-NISSU                   PIC 9(02).
           03  WRK-GAIRAI-KEN              PIC 9(07)V9(02).
           03  WRK-GAIRAI-SIYO             PIC 9(07)V9(02).
           03  WRK-GAIRAI-KIN              PIC 9(09).
           03  WRK-NYUIN-KEN               PIC 9(07)V9(02).
           03  WRK-NYUIN-SIYO              PIC 9(07)V9(02).
           03  WRK-NYUIN-KIN               PIC 9(09).
           03  WRK-KAISU                   PIC 9(08)V9(02).
           03  WRK-HIRITU                  PIC 9(03)V9(02).
           03  WRK-ZENSORTKBN              PIC 9(02).
           03  WRK-ZENSRYKBN               PIC 9(02).
           03  WRK-ZENPRTKBN               PIC 9(05).
           03  WRK-MAX                     PIC 9(05).
           03  WRK-TENTNK                  PIC 9(03)V9(02).
           03  WRK-STARTYMD.
               05  WRK-STARTY              PIC 9(04).
               05  WRK-STARTM              PIC 9(02).
               05  WRK-STARTD              PIC 9(02).
           03  WRK-ENDYMD.
               05  WRK-ENDY                PIC 9(04).
               05  WRK-ENDM                PIC 9(02).
               05  WRK-ENDD                PIC 9(02).
           03  WRK-YM.
               05  WRK-Y                   PIC 9(04).
               05  WRK-M                   PIC 9(02).
           03  WRK-STARTDAY                PIC 9(02).
           03  WRK-ENDDAY                  PIC 9(02).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SYOKUJITEN.
               05  WRK-SYOKUJITEN1         PIC 9(08)V9(02).
               05  WRK-SYOKUJITEN2         PIC 9(08)V9(02).
               05  WRK-SYOKUJITEN3         PIC 9(08)V9(02).
               05  WRK-SYOKUJITEN4         PIC 9(08)V9(02).
           03  WRK-SEIKATUTEN.
               05  WRK-SEIKATUTEN1         PIC 9(08)V9(02).
               05  WRK-SEIKATUTEN2         PIC 9(08)V9(02).
               05  WRK-SEIKATUTEN3         PIC 9(08)V9(02).
               05  WRK-SEIKATUTEN4         PIC 9(08)V9(02).
           03  WRK-OBJYMD                  PIC 9(08).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-KIZAMI                  PIC S9(09).
      *
      *    診療科、医師、病棟別集計領域
       01  WRK-SYUKEI-AREA.
           03  WRK-MEISAITBL               OCCURS 10000.
             05  WRK-NAME                  PIC X(20).
             05  WRK-NAMEKBN               PIC 9(05).
             05  WRK-STYUKYMD              PIC 9(08).
             05  WRK-EDYUKYMD              PIC 9(08).
      *
      *    比率計算用区分別集計領域
       01  WRK-HIRITU-AREA.
           03  WRK-HIRITU-PRTKBN-TBL       OCCURS 10000.
             05  WRK-HIRITU-SORTKBN-TBL    OCCURS 14.
               07  WRK-HIRITU-GOUKEI       PIC 9(10).
               07  WRK-HIRITU-SRYKBN-TBL   OCCURS 99.
                 09  WRK-HIRITU-SYOKEI     PIC 9(10).
      *
      *    小計集計領域
       01  SUM-S-AREA.
           03  WRK-SUM-SYAKKA              PIC 9(09).
           03  WRK-SUM-STANKA              PIC 9(09).
           03  WRK-SUM-SGAIRAI-KEN         PIC 9(07)V9(02).
           03  WRK-SUM-SNYUIN-KEN          PIC 9(07)V9(02).
           03  WRK-SUM-STOTAL-KEN          PIC 9(08)V9(02).
           03  WRK-SUM-SGAIRAI-SIYO        PIC 9(07)V9(02).
           03  WRK-SUM-SNYUIN-SIYO         PIC 9(07)V9(02).
           03  WRK-SUM-STOTAL-SIYO         PIC 9(08)V9(02).
           03  WRK-SUM-SGAIRAI-KIN         PIC 9(09).
           03  WRK-SUM-SNYUIN-KIN          PIC 9(09).
           03  WRK-SUM-STOTAL-KIN          PIC 9(09).
           03  WRK-SUM-SZENHI              PIC 9(03)V9(02).
           03  WRK-SUM-SGRPHI              PIC 9(03)V9(02).
      *
      *    合計集計領域
       01  SUM-G-AREA.
           03  WRK-SUM-GYAKKA              PIC 9(10).
           03  WRK-SUM-GTANKA              PIC 9(10).
           03  WRK-SUM-GGAIRAI-KEN         PIC 9(08)V9(02).
           03  WRK-SUM-GNYUIN-KEN          PIC 9(08)V9(02).
           03  WRK-SUM-GTOTAL-KEN          PIC 9(09)V9(02).
           03  WRK-SUM-GGAIRAI-SIYO        PIC 9(08)V9(02).
           03  WRK-SUM-GNYUIN-SIYO         PIC 9(08)V9(02).
           03  WRK-SUM-GTOTAL-SIYO         PIC 9(09)V9(02).
           03  WRK-SUM-GGAIRAI-KIN         PIC 9(09).
           03  WRK-SUM-GNYUIN-KIN          PIC 9(09).
           03  WRK-SUM-GTOTAL-KIN          PIC 9(10).
           03  WRK-SUM-GZENHI              PIC 9(03)V9(02).
           03  WRK-SUM-GGRPHI              PIC 9(03)V9(02).
      *
      *    ＣＳＶデータ領域
       01  CSV-DATA-AREA.
           03  CSV-DATA-TBL.
             05  CSV-DATA                  PIC X(100).
           03  WRK-OUTPUT-DATA             PIC X(20000).
           03  CSV-DATA-SIZE               PIC 9(03)  VALUE 100.
           03  CSV-DIR                     PIC X(200).
           03  CSV-NAME                    PIC X(200).
      *
      *    ＣＳＶヘッダー領域１
       01  CSV-HEAD01-AREA.
           03  CSV-HEAD01-MAX           PIC 9(03)  VALUE 12.
           03  CSV-HEAD01-TBL.
             05  CSV-HEAD01-SRYKBN      PIC X(30)  VALUE "診区".
             05  CSV-HEAD01-SRYCD       PIC X(30)  VALUE "診療コード".
             05  CSV-HEAD01-SRYNAME     PIC X(30)  VALUE "診療名称".
             05  CSV-HEAD01-YAKKA       PIC X(30)  VALUE "薬価/点数".
             05  CSV-HEAD01-G-KENSU     PIC X(30)  VALUE "外来件数".
             05  CSV-HEAD01-N-KENSU     PIC X(30)  VALUE "入院件数".
             05  CSV-HEAD01-T-KENSU     PIC X(30)  VALUE "合計件数".
             05  CSV-HEAD01-G-KIN       PIC X(30)  VALUE "外来金額".
             05  CSV-HEAD01-N-KIN       PIC X(30)  VALUE "入院金額".
             05  CSV-HEAD01-T-KIN       PIC X(30)  VALUE "合計金額".
             05  CSV-HEAD01-ZENHI       PIC X(30)  VALUE "全体比率".
             05  CSV-HEAD01-GRPHI       PIC X(30)  VALUE "グループ比率".
           03  CSV-HEAD01-TBL2          REDEFINES    CSV-HEAD01-TBL.
             05  CSV-HEAD01-DATA        PIC X(30) OCCURS 12.
      *
      *    ＣＳＶヘッダー領域２
       01  CSV-HEAD02-AREA.
           03  CSV-HEAD02-MAX           PIC 9(03)  VALUE 11.
           03  CSV-HEAD02-TBL.
             05  CSV-HEAD02-SRYKBN      PIC X(30)  VALUE "診区".
             05  CSV-HEAD02-SRYCD       PIC X(30)  VALUE "診療コード".
             05  CSV-HEAD02-SRYNAME     PIC X(30)  VALUE "診療名称".
             05  CSV-HEAD02-TANKA       PIC X(30)  VALUE "単価".
             05  CSV-HEAD02-G-SIYO      PIC X(30)  VALUE "外来使用量".
             05  CSV-HEAD02-N-SIYO      PIC X(30)  VALUE "入院使用量".
             05  CSV-HEAD02-T-SIYO      PIC X(30)  VALUE "合計使用量".
             05  CSV-HEAD02-G-KIN       PIC X(30)  VALUE "外来金額".
             05  CSV-HEAD02-N-KIN       PIC X(30)  VALUE "入院金額".
             05  CSV-HEAD02-T-KIN       PIC X(30)  VALUE "合計金額".
             05  CSV-HEAD02-ZENHI       PIC X(30)  VALUE "全体比率".
           03  CSV-HEAD02-TBL2          REDEFINES    CSV-HEAD02-TBL.
             05  CSV-HEAD02-DATA        PIC X(30) OCCURS 11.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    医師情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災自賠責医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    食事療養費情報
           COPY    "CPSK5000.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    室料差額情報
           COPY    "CPSK5113.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療会計マスタ２
       01  NYUINACCT2-REC.
           COPY    "CPNYUINACCT.INC"    REPLACING  //NACCT//
                                        BY         //NACCT2//.
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    受信履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
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
      *****************************************************************
      *    サブプロ用領域
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
      *
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    入院時生活療養費負担額取得サブ２
           COPY    "CPORCSNYULIFE2.INC".
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
           02  FILLER            PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "*** START ***"
           PERFORM  100-INIT-SEC
      *
           IF  FLG-END  NOT =  1
             PERFORM  300-MAIN-SEC
           END-IF
      *
           PERFORM  600-END-SEC
           DISPLAY "*** END ***"
      *
           STOP  RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-S-AREA
           INITIALIZE                  SUM-G-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-HIRITU-AREA
           INITIALIZE                  CSV-DATA
           INITIALIZE                  WRK-OUTPUT-DATA
           INITIALIZE                  RECEERR
           INITIALIZE                  ORCSNYULIFE2-AREA
      *
           PERFORM  100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO   LNK-PRTKANRI-RENNUM
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
                                              WRK-PARA-PRTSTYMD
                                              WRK-PARA-PRTEDYMD
                                              WRK-PARA-NYUGAIKBN
                                              WRK-PARA-PRTKBN
                                              WRK-PARA-SRYKBN
                                              WRK-PARA-HOKATUKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM     TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"                TO  SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO  JOB-SHELLID
           MOVE    "A00000TA02"         TO  JOB-PGID
           MOVE    "診療行為別集計表"   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           MOVE    "ATA02TP"                TO  TA02P-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA02P-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA02P-HOSPNUM
      *
           MOVE    TA02P-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA02P-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM  200-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       200-PARA-HENSYU-SEC            SECTION.
      *
           MOVE    ZERO        TO  FLG-END
      *
           IF  WRK-PARA-PRTEDYMD  =  SPACE
             MOVE    WRK-PARA-PRTSTYMD     TO  WRK-PARA-PRTEDYMD
           END-IF
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTSTYMD     TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-PRTEDYMD     TO  SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM  900-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメータ＿発行年月日チェック
           IF  WRK-PARA-PRTSTYMD  >  WRK-PARA-PRTEDYMD
             MOVE    "集計範囲が不正です"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメータ＿入外区分チェック
           IF  WRK-PARA-NYUGAIKBN  NOT =  "1" AND "2" AND SPACE
             MOVE    "入外区分が不正です"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメータ＿発行区分チェック
           IF  WRK-PARA-PRTKBN  NOT =  "1" AND "2" AND "3" AND SPACE
             MOVE    "集計区分が不正です"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメータ＿発行区分チェック
           IF  WRK-PARA-PRTKBN  =  "3"
               AND  WRK-PARA-NYUGAIKBN  NOT =  "1"
             MOVE    "病棟別の場合は入外区分を1(入院)に設定して下さい"
                 TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    入力パラメータ＿包括区分チェック
           IF  WRK-PARA-HOKATUKBN  NOT =  "1"  AND  "2"  AND  SPACE
             MOVE    "包括区分が不正です"      TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    労災自賠責医療機関情報
           MOVE    SPACE                 TO  SYS-4001-REC
           INITIALIZE                    SYS-4001-REC
           MOVE    "4001"                TO  SYS-4001-KANRICD
           MOVE    "*"                   TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTSTYMD     TO  SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-PRTEDYMD     TO  SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC          TO  MCPDATA-REC
           PERFORM   900-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-4001-REC
             MOVE    SYS-4001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           END-IF
      *
      *    集計名称取得
           MOVE    1           TO  IDX1
           EVALUATE  WRK-PARA-PRTKBN
             WHEN  "1"
      *    診療科
               INITIALIZE                    SYS-1005-REC
               MOVE    "1005"                TO  SYS-1005-KANRICD
               MOVE    "%"                   TO  SYS-1005-KBNCD
               MOVE    SPA-HOSPNUM           TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC          TO  MCPDATA-REC
               PERFORM  910-SYSKANRI-SELECT-SEC
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               PERFORM  UNTIL  FLG-SYSKANRI  =  1
                 MOVE    SYS-1005-SRYKANAME  TO  WRK-NAME(IDX1)
                 MOVE    SYS-1005-KBNCD      TO  WRK-NAMEKBN(IDX1)
                 MOVE    SYS-1005-STYUKYMD   TO  WRK-STYUKYMD(IDX1)
                 MOVE    SYS-1005-EDYUKYMD   TO  WRK-EDYUKYMD(IDX1)
                 ADD   1                     TO  IDX1
                 MOVE    IDX1                TO  WRK-MAX
                 PERFORM   910-SYSKANRI-FETCH-SEC
                 MOVE    MCPDATA-REC         TO  SYS-1005-REC
               END-PERFORM
               MOVE    "該当診療科なし"      TO  WRK-NAME(WRK-MAX)
               PERFORM   910-SYSKANRI-CLOSE-SEC
             WHEN  "2"
      *    医師
               INITIALIZE                    SYS-1010-REC
               MOVE    "1010"                TO  SYS-1010-KANRICD
               MOVE    "1%"                  TO  SYS-1010-KBNCD
               MOVE    SPA-HOSPNUM           TO  SYS-1010-HOSPNUM
               MOVE    WRK-PARA-PRTSTYMD     TO  SYS-1010-EDYUKYMD
               MOVE    WRK-PARA-PRTEDYMD     TO  SYS-1010-STYUKYMD
               MOVE    SYS-1010-REC          TO  MCPDATA-REC
               PERFORM  910-SYSKANRI-SELECT-SEC
               MOVE    MCPDATA-REC           TO  SYS-1010-REC
               PERFORM  UNTIL  FLG-SYSKANRI  =  1
                 MOVE    SYS-1010-NAME       TO  WRK-NAME(IDX1)
                 MOVE    SYS-1010-KBNCD      TO  WRK-NAMEKBN(IDX1)
                 MOVE    SYS-1010-STYUKYMD   TO  WRK-STYUKYMD(IDX1)
                 MOVE    SYS-1010-EDYUKYMD   TO  WRK-EDYUKYMD(IDX1)
                 ADD   1                     TO  IDX1
                 MOVE    IDX1                TO  WRK-MAX
                 PERFORM  910-SYSKANRI-FETCH-SEC
                 MOVE    MCPDATA-REC         TO  SYS-1010-REC
               END-PERFORM
               MOVE    "該当医師なし"        TO  WRK-NAME(WRK-MAX)
               PERFORM  910-SYSKANRI-CLOSE-SEC
             WHEN  "3"
      *    病棟情報
               INITIALIZE                    SYS-5001-REC
               MOVE    "5001"                TO  SYS-5001-KANRICD
               MOVE    "%"                   TO  SYS-5001-KBNCD
               MOVE    SPA-HOSPNUM           TO  SYS-5001-HOSPNUM
               MOVE    WRK-PARA-PRTSTYMD     TO  SYS-5001-EDYUKYMD
               MOVE    WRK-PARA-PRTEDYMD     TO  SYS-5001-STYUKYMD
               MOVE    SYS-5001-REC          TO  MCPDATA-REC
               PERFORM  910-SYSKANRI-SELECT-SEC
               MOVE    MCPDATA-REC           TO  SYS-5001-REC
               PERFORM  UNTIL  FLG-SYSKANRI  =  1
                 MOVE    SYS-5001-BTU-NAME   TO  WRK-NAME(IDX1)
                 MOVE    SYS-5001-BTU-NUM    TO  WRK-NAMEKBN(IDX1)
                 MOVE    SYS-5001-STYUKYMD   TO  WRK-STYUKYMD(IDX1)
                 MOVE    SYS-5001-EDYUKYMD   TO  WRK-EDYUKYMD(IDX1)
                 ADD  1                      TO  IDX1
                 MOVE    IDX1                TO  WRK-MAX
                 PERFORM  910-SYSKANRI-FETCH-SEC
                 MOVE    MCPDATA-REC         TO  SYS-5001-REC
               END-PERFORM
      *
               MOVE    "該当病棟なし"        TO  WRK-NAME(WRK-MAX)
               PERFORM  910-SYSKANRI-CLOSE-SEC
           END-EVALUATE
      *
      *    入院基本情報
           INITIALIZE                   SYS-5000-REC
           MOVE    SPA-HOSPNUM          TO  SYS-5000-HOSPNUM
           MOVE    "5000"               TO  SYS-5000-KANRICD
           MOVE    WRK-PARA-PRTEDYMD    TO  SYS-5000-STYUKYMD
           MOVE    WRK-PARA-PRTSTYMD    TO  SYS-5000-EDYUKYMD
           PERFORM  930-SYSKANRI-DBSELECT-SEC
      *
           IF  FLG-SYSKANRI  =  ZERO
      *
      *      食事療養費
      *      (WRK-SYOKUJITEN1 : 食事療養費のみ)
      *      (WRK-SYOKUJITEN2 : 食事療養費＋特別食加算)
      *      (WRK-SYOKUJITEN3 : 入院時食事療養（流動食）)
             EVALUATE  SYS-5000-SHOKUJIRYOYOCD
             WHEN  "197000110"
      *        入院時食事療養（１）
               MOVE    "197000110"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SYOKUJITEN1
                                                   WRK-SYOKUJITEN2
      *        特別食加算
               MOVE    "197000470"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               ADD     TNS-TEN                 TO  WRK-SYOKUJITEN2
      *        入院時食事療養（１）（流動食）
               MOVE    "197003110"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SYOKUJITEN3
             WHEN  "197000710"
      *        入院時食事療養（２）
               MOVE    "197000710"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SYOKUJITEN1
      *        入院時食事療養（２）（流動食）
               MOVE    "197003210"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SYOKUJITEN3
             END-EVALUATE
      *
      *      特別管理加算
             IF   SYS-5000-SHOKUKBN(1)  =  "2"
               MOVE    SYS-5000-SHOKUCD(1)     TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               ADD     TNS-TEN                 TO  WRK-SYOKUJITEN1
                                                   WRK-SYOKUJITEN2
             END-IF
      *
      *      生活療養費
      *      (WRK-SEIKATUTEN1 : 生活療養費のみ)
      *      (WRK-SEIKATUTEN2 : 生活療養費＋特別食加算)
      *      (WRK-SEIKATUTEN3 : 入院時生活療養（流動食）)
             EVALUATE  SYS-5000-SHOKUJIRYOYOCD
             WHEN  "197000110"
      *        入院時生活療養（１）
               MOVE   "197001310"              TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SEIKATUTEN1
                                                   WRK-SEIKATUTEN2
      *        特別食加算（生活療養）
               MOVE    "197001570"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               ADD     TNS-TEN                 TO  WRK-SEIKATUTEN2
      *        入院時生活療養（１）（流動食）
               MOVE    "197003510"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SEIKATUTEN3
      *
             WHEN  "197000710"
      *        入院時生活療養（２）
               MOVE    "197001710"             TO  WRK-SRYCD
               PERFORM  910-TENSU-READ-SEC
               MOVE    TNS-TEN                 TO  WRK-SEIKATUTEN1
      *
             END-EVALUATE
      *
           END-IF
      *
           .
       200-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       300-MAIN-SEC         SECTION.
      *
           OPEN    OUTPUT   TA02-FILE
           CLOSE   TA02-FILE
           OPEN    I-O      TA02-FILE
      *
      *    対象月設定
           MOVE    WRK-PARA-PRTSTYMD       TO  WRK-STARTYMD
           MOVE    WRK-PARA-PRTSTYMD(1:6)  TO  WRK-YM
           IF   WRK-PARA-PRTEDYMD  =  SPACE
             MOVE    WRK-PARA-PRTSTYMD     TO  WRK-ENDYMD
           ELSE
             MOVE    WRK-PARA-PRTEDYMD     TO  WRK-ENDYMD
           END-IF
      *
      *    集計範囲編集
           PERFORM  UNTIL  WRK-YM  >  WRK-ENDYMD(1:6)
      *
      *    診療行為情報読み込み
             PERFORM  900-SRYACT-SELECT-SEC
             PERFORM  UNTIL  FLG-SRYACT  =  1
      *
      *    帳票編集
      *    診療区分・入外区分判定(コメントは印字対象外)
               IF  ( SRY-SRYKBN  =  WRK-PARA-SRYKBN
                   OR   WRK-PARA-SRYKBN  =  SPACE )
                   AND  ( WRK-PARA-NYUGAIKBN  =  SRY-NYUGAIKBN
                   OR  WRK-PARA-NYUGAIKBN  =  SPACE )
                   AND  SRY-SRYKBN  NOT =  "98"
                   AND  SRY-SRYKBN  NOT =  "99"
      *
      *    診療会計情報読込み
                 PERFORM  900-SRYACCT-READ-SEC
      *
      *    包括判定(包括区分=" ":全て、括区分=1:包括以外、包括区分=2:包括のみ)
                 IF    ( ( ACCT-JIHOKBN  NOT =  1  AND
                           ACCT-HKNCOMBI NOT = "9999"  )  AND
                           WRK-PARA-HOKATUKBN  =  "1" )
                   OR  ( ( ACCT-JIHOKBN      =  1  OR
                           ACCT-HKNCOMBI     = "9999"  )  AND
                           WRK-PARA-HOKATUKBN  =  "2" )
                   OR  WRK-PARA-HOKATUKBN  =  SPACE
      *
                   PERFORM  400-SRYACT-HEN-SEC
                 END-IF
               END-IF
      *
               PERFORM  900-SRYACT-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYACT-CLOSE-SEC
      *
      *    入院診療行為情報読み込み
             PERFORM  900-NYUINACT-SELECT-SEC
             PERFORM  UNTIL  FLG-NYUINACT  =  1
      *
      *    診療区分・入外区分判定
               IF  ( WRK-PARA-SRYKBN  =  NSRY-SRYKBN
                   OR   WRK-PARA-SRYKBN  =  SPACE )
                   AND  ( WRK-PARA-NYUGAIKBN  =  NSRY-NYUGAIKBN
                   OR  WRK-PARA-NYUGAIKBN  =  SPACE )
      *
      *    入院会計情報読込み
                 PERFORM  900-NYUINACCT-READ-SEC
      *
      *    入院時生活療養費負担額取得サブ２
                 INITIALIZE                  ORCSNYULIFE2-AREA
                 MOVE    NACCT-HOSPNUM   TO  LNK-NYULIFE2-HOSPNUM
                 MOVE    NACCT-PTID      TO  LNK-NYULIFE2-PTID
                 MOVE    NACCT-SRYYM     TO  LNK-NYULIFE2-SANTEIYM
                 CALL    "ORCSNYULIFE2"      USING
                                             ORCSNYULIFE2-AREA
                                             SPA-AREA
      *
      *    帳票編集
                 PERFORM  400-NYUINACT-HEN-SEC
      *
               END-IF
      *
               PERFORM  900-NYUINACT-FETCH-SEC
             END-PERFORM
             PERFORM  900-NYUINACT-CLOSE-SEC
      *
      *    集計対象月カウント
             IF   WRK-M  =  12
               ADD   1            TO  WRK-Y
               MOVE    1          TO  WRK-M
             ELSE
               ADD   1            TO  WRK-M
             END-IF
      *
           END-PERFORM
      *
           CLOSE  TA02-FILE
      *
           IF   FLG-KENSU  =  ZERO
             MOVE    "データがありません"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           ELSE
      *    合計頁数計算処理
             PERFORM  400-GPAGE-SEC
      *    印刷処理
             PERFORM  400-PRINT-SEC
           END-IF
      *
           .
       300-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為編集<明細行>
      *****************************************************************
       400-SRYACT-HEN-SEC          SECTION.
      *
      *    保険組合せ情報取得
           PERFORM  900-HKNCOMBI-READ-SEC
      *
      *    点数単価取得
           IF  COMB-HKNNUM  =  "971"  OR  "973"
             IF  SYS-4001-TENKBN  =  "1"
               MOVE    11.5          TO  WRK-TENTNK
             ELSE
               MOVE    12            TO  WRK-TENTNK
             END-IF
           ELSE
             MOVE    10              TO  WRK-TENTNK
           END-IF
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  5
                    OR  SRY-SRYCD(IDX1)  =  SPACE
      *
             IF  SRY-SRYCD(IDX1)(1:3)  NOT =  "001"
                 AND  SRY-SRYCD(IDX1)(1:3)  NOT =  "002"
                 AND  SRY-SRYCD(IDX1)(1:4)  NOT =  "0082"
                 AND  SRY-SRYCD(IDX1)(1:4)  NOT =  "0083"
                 AND  SRY-SRYCD(IDX1)(1:4)  NOT =  "0084"
                 AND  SRY-SRYCD(IDX1)(1:4)  NOT =  "0085"
      *
      *    対象日設定
               IF   WRK-PARA-PRTSTYMD(1:6)  =  WRK-YM
                 MOVE    WRK-STARTD    TO  IDX-DAY
               ELSE
                 MOVE    1             TO  IDX-DAY
               END-IF
               IF   WRK-PARA-PRTEDYMD(1:6)  =  WRK-YM
                 MOVE    WRK-ENDD      TO  WRK-ENDDAY
               ELSE
                 MOVE    31            TO  WRK-ENDDAY
               END-IF
      *
               MOVE   IDX-DAY          TO   WRK-STARTDAY
               PERFORM  VARYING  IDX-DAY  FROM  WRK-STARTDAY  BY  1
                        UNTIL  IDX-DAY  >   WRK-ENDDAY
      *
                 INITIALIZE                TA02TMP-REC
      *
      *    処理を行う年月日を格納
                 MOVE    WRK-YM        TO  WRK-OBJYMD(1:6)
                 MOVE    IDX-DAY       TO  WRK-OBJYMD(7:2)
      *
                 IF  ACCT-DAY(1 IDX-DAY)  >  ZERO
      *
      *    点数マスタ読込み
                   MOVE    SRY-SRYCD(IDX1)     TO  WRK-SRYCD
                   PERFORM  900-TENSU-READ-SEC
      *
      *    キー設定
      *    順序設定
                   PERFORM  500-SORTKBN-SEC
      *
      *    科別、医師別区分
                   EVALUATE  WRK-PARA-PRTKBN
                     WHEN  SPACE
                       MOVE    1               TO  TA02TMP-PRTKBN
                     WHEN  "1"
                       PERFORM  VARYING  IDX2  FROM  1  BY  1
                                UNTIL  IDX2  >  WRK-MAX
                         MOVE    IDX2          TO  TA02TMP-PRTKBN
                         IF  ACCT-SRYKA  =  WRK-NAMEKBN(IDX2)(4:2)
                             AND  WRK-OBJYMD  >=  WRK-STYUKYMD(IDX2)
                             AND  WRK-OBJYMD  <=  WRK-EDYUKYMD(IDX2)
                           MOVE    999         TO  IDX2
                         END-IF
                       END-PERFORM
                     WHEN  "2"
      *    受診履歴情報取得
                       PERFORM  900-JYURRK-SELECT-SEC
                       MOVE    WRK-MAX         TO  TA02TMP-PRTKBN
                       PERFORM  UNTIL  FLG-JYURRK  =  1
                         IF  JYURRK-SRYYMD  =  WRK-OBJYMD
                           PERFORM  VARYING  IDX2  FROM  1  BY  1
                                    UNTIL  IDX2  >  WRK-MAX
                             MOVE    IDX2      TO  TA02TMP-PRTKBN
                             IF  JYURRK-DRCD  =  WRK-NAMEKBN(IDX2)
                                 AND  WRK-OBJYMD  >=  WRK-STYUKYMD(IDX2)
                                 AND  WRK-OBJYMD  <=  WRK-EDYUKYMD(IDX2)
                               MOVE    999     TO  IDX2
                             END-IF
                           END-PERFORM
                         END-IF
                         PERFORM  900-JYURRK-FETCH-SEC
                       END-PERFORM
                       PERFORM  900-JYURRK-CLOSE-SEC
                     WHEN  "3"
                       MOVE    SRY-PTID      TO  WRK-PTID
                       PERFORM  900-PTNYUINRRK-READ-SEC
                       PERFORM  VARYING  IDX2  FROM  1  BY  1
                                UNTIL  IDX2  >  WRK-MAX
                         MOVE    IDX2          TO  TA02TMP-PRTKBN
                         IF  PTNYUINRRK-BTUNUM =  WRK-NAMEKBN(IDX2)(4:2)
                             AND  WRK-OBJYMD  >=  WRK-STYUKYMD(IDX2)
                             AND  WRK-OBJYMD  <=  WRK-EDYUKYMD(IDX2)
                           MOVE    999         TO  IDX2
                         END-IF
                       END-PERFORM
                   END-EVALUATE
      *
      *    診療区分
                   MOVE    SRY-SRYKBN          TO  TA02TMP-SRYKBN
      *
      *    診療名称
                   MOVE    TNS-NAME            TO  TA02TMP-SRYNAME
      *
      *    診療コード
                   MOVE    SRY-SRYCD(IDX1)     TO  TA02TMP-SRYCD
      *
      *    点数・薬価
                   IF  TNS-TENSIKIBETU  =  7
                     COMPUTE  TNS-TEN  =  TNS-TEN  *  -1
                   END-IF
      *
      *    単価、薬価が０になるのを回避するため
                   IF  ( SRY-SRYCD(IDX1)(1:5) = "09591" OR "09592" OR
                                                "09593" OR "09594"   )
                     MOVE    1                   TO   TNS-TENSIKIBETU
                   END-IF
      *
      *    きざみ値処理
                   IF   TNS-KZMCOMPSIKIBETU   =  1
                     IF  SRY-SRYSURYO(IDX1)  >=  TNS-KZMJGN
                       COMPUTE  WRK-KIZAMI  =  TNS-KZMJGN
                                            -  TNS-KZMKGN
                     ELSE
                       COMPUTE  WRK-KIZAMI  =  SRY-SRYSURYO(IDX1)
                                            -  TNS-KZMKGN
                     END-IF
      *
                     PERFORM    UNTIL  WRK-KIZAMI  <=  0
                       COMPUTE  WRK-KIZAMI  =  WRK-KIZAMI
                                            -  TNS-KZM
                       COMPUTE  TNS-TEN     =  TNS-TEN
                                            +  TNS-KZMTEN
                     END-PERFORM
                   END-IF
      *
                   IF  ( SRY-SRYKBN  =  "95"  OR  "96" )
      *
                     MOVE    SRY-JIHIMONEY(IDX1)  TO  TA02TMP-YAKKA
                                                      TA02TMP-TANKA
                                                      WRK-TENSU
      *
                   ELSE
                     EVALUATE  TNS-TENSIKIBETU
                       WHEN  0
                       WHEN  4
                       WHEN  5
                       WHEN  6
                       WHEN  7
                         MOVE    ZERO            TO  TA02TMP-YAKKA
                                                     TA02TMP-TANKA
                                                     WRK-TENSU
                       WHEN  OTHER
                         MOVE    TNS-TEN         TO  TA02TMP-YAKKA
                                                     TA02TMP-TANKA
                                                     WRK-TENSU
                     END-EVALUATE
                   END-IF
      *
      *    TMPファイル読み込み
                   PERFORM  700-TMP-READ-SEC
      *
                   INITIALIZE    WRK-GAIRAI-SIYO
                                 WRK-NYUIN-SIYO
                                 WRK-GAIRAI-KEN
                                 WRK-NYUIN-KEN
      *
                   IF  SRY-NYUGAIKBN  =  "2"
      *    外来使用量
                     IF  TA02TMP-SORTKBN  =  12  OR  13  OR  14
                       COMPUTE  WRK-GAIRAI-SIYO  =  ACCT-DAY(1 IDX-DAY)
                                                 *  SRY-SRYSURYO(IDX1)
                       ADD  WRK-GAIRAI-SIYO    TO  TA02TMP-GAIRAI-SIYO
                     END-IF
                   ELSE
      *    入院使用量
                     IF  TA02TMP-SORTKBN  =  12  OR  13  OR  14
                       COMPUTE  WRK-NYUIN-SIYO  =  ACCT-DAY(1 IDX-DAY)
                                                *  SRY-SRYSURYO(IDX1)
                       ADD  WRK-NYUIN-SIYO     TO  TA02TMP-NYUIN-SIYO
                     END-IF
                   END-IF
                   IF   TA02TMP-SORTKBN  NOT =  12  AND  13  AND  14
                     IF   ACCT-NYUGAIKBN  =  "2"
      *    外来件数
                       EVALUATE  TNS-TENSIKIBETU
                         WHEN  1
                           COMPUTE  WRK-GAIRAI-KEN = SRY-SRYSURYO(IDX1)
                                                   * ACCT-DAY(1 IDX-DAY)
                         WHEN  0
                         WHEN  3
                         WHEN  4
                         WHEN  5
                         WHEN  6
                         WHEN  7
                         WHEN  8
                           MOVE   ACCT-DAY(1 IDX-DAY) TO WRK-GAIRAI-KEN
                       END-EVALUATE
                       ADD  WRK-GAIRAI-KEN     TO  TA02TMP-GAIRAI-KEN
                     ELSE
      *    入院件数
                       EVALUATE  TNS-TENSIKIBETU
                         WHEN  1
                           COMPUTE  WRK-NYUIN-KEN = SRY-SRYSURYO(IDX1)
                                                  * ACCT-DAY(1 IDX-DAY)
                         WHEN  0
                         WHEN  3
                         WHEN  4
                         WHEN  5
                         WHEN  6
                         WHEN  7
                         WHEN  8
                           MOVE   ACCT-DAY(1 IDX-DAY) TO WRK-NYUIN-KEN
                       END-EVALUATE
                       ADD  WRK-NYUIN-KEN      TO  TA02TMP-NYUIN-KEN
                     END-IF
                   END-IF
      *
                   INITIALIZE                 WRK-GAIRAI-KIN
                                              WRK-NYUIN-KIN
      *
                   IF  SRY-NYUGAIKBN  =  "2"
      *    外来金額
                     IF ( SRY-SRYCD(IDX1)(1:1)  =  "6"  OR  "7" )
                        OR ( ( SRY-SRYCD(IDX1)(1:3)  =  "095"  OR
                                                        "096"  OR
                                                        "059" )
                         AND ( SRY-SRYCD(IDX1)(1:5)  NOT =  "09591"  AND
                                                            "09592"  AND
                                                            "09593"  AND
                                                            "09594"  ) )
                       COMPUTE  WRK-GAIRAI-KIN  =  WRK-TENSU
                                                *  WRK-GAIRAI-SIYO
                       ADD  WRK-GAIRAI-KIN     TO  TA02TMP-GAIRAI-KIN
                     ELSE
                       IF  TNS-TENSIKIBETU  =  1
                         COMPUTE  WRK-GAIRAI-KIN  =  WRK-TENSU
                                                  *  WRK-GAIRAI-KEN
                       ELSE
                         COMPUTE  WRK-GAIRAI-KIN  =  WRK-TENSU
                                                  *  WRK-GAIRAI-KEN
                                                  *  WRK-TENTNK
                       END-IF
                       ADD  WRK-GAIRAI-KIN     TO  TA02TMP-GAIRAI-KIN
                     END-IF
                   ELSE
      *    入院金額
                     IF ( SRY-SRYCD(IDX1)(1:1)  =  "6"  OR  "7" )
                        OR ( ( SRY-SRYCD(IDX1)(1:3)  =  "095"  OR
                                                        "096"  OR
                                                        "059" )
                         AND ( SRY-SRYCD(IDX1)(1:5)  NOT =  "09591"  AND
                                                            "09592"  AND
                                                            "09593"  AND
                                                            "09594"  ) )
                       COMPUTE  WRK-NYUIN-KIN  =  WRK-TENSU
                                               *  WRK-NYUIN-SIYO
                       ADD  WRK-NYUIN-KIN      TO  TA02TMP-NYUIN-KIN
                     ELSE
                       IF  TNS-TENSIKIBETU  =  1
                         COMPUTE  WRK-NYUIN-KIN  =  WRK-TENSU
                                                 *  WRK-NYUIN-KEN
                       ELSE
                         COMPUTE  WRK-NYUIN-KIN  =  WRK-TENSU
                                                 *  WRK-NYUIN-KEN
                                                 *  WRK-TENTNK
                       END-IF
                       ADD  WRK-NYUIN-KIN      TO  TA02TMP-NYUIN-KIN
                     END-IF
                   END-IF
      *
      *    診療区分別小計集計
                   MOVE    TA02TMP-PRTKBN      TO  IDX2
                   MOVE    TA02TMP-SORTKBN     TO  IDX3
                   MOVE    SRY-SRYKBN          TO  IDX4
                   ADD  WRK-NYUIN-KIN
                                  TO  WRK-HIRITU-SYOKEI(IDX2 IDX3 IDX4)
                   ADD  WRK-GAIRAI-KIN
                                  TO  WRK-HIRITU-SYOKEI(IDX2 IDX3 IDX4)
      *
      *    総合計金額集計
                   ADD  WRK-NYUIN-KIN   TO  WRK-HIRITU-GOUKEI(IDX2 IDX3)
                   ADD  WRK-GAIRAI-KIN  TO  WRK-HIRITU-GOUKEI(IDX2 IDX3)
      *
                   IF  FLG-INVALID  =  1
                     MOVE    1                 TO  FLG-KENSU
                     WRITE   TA02TMP-REC
                   ELSE
                     REWRITE TA02TMP-REC
                   END-IF
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM
      *
           .
       400-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為編集<明細行>
      *****************************************************************
       400-NYUINACT-HEN-SEC          SECTION.
      *
      *    保険組合せ情報取得
           PERFORM  910-HKNCOMBI-READ-SEC
      *
      *    点数単価取得
           IF  COMB-HKNNUM  =  "971"  OR  "973"
             IF  SYS-4001-TENKBN  =  "1"
               MOVE    11.5        TO  WRK-TENTNK
             ELSE
               MOVE    12          TO  WRK-TENTNK
             END-IF
           ELSE
             MOVE    10            TO  WRK-TENTNK
           END-IF
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  5
                    OR  NSRY-SRYCD(IDX1)  =  SPACE
      *
      *    対象日設定
             IF   WRK-PARA-PRTSTYMD(1:6)  =  WRK-YM
               MOVE    WRK-STARTD    TO  IDX-DAY
             ELSE
               MOVE    1             TO  IDX-DAY
             END-IF
             IF   WRK-PARA-PRTEDYMD(1:6)  =  WRK-YM
               MOVE    WRK-ENDD      TO  WRK-ENDDAY
             ELSE
               MOVE    31            TO  WRK-ENDDAY
             END-IF
      *
             MOVE   IDX-DAY          TO   WRK-STARTDAY
             PERFORM  VARYING  IDX-DAY  FROM  WRK-STARTDAY  BY  1
                      UNTIL  IDX-DAY  >   WRK-ENDDAY
      *
               INITIALIZE                TA02TMP-REC
      *
      *    処理を行う年月日を格納
               MOVE    WRK-YM        TO  WRK-OBJYMD(1:6)
               MOVE    IDX-DAY       TO  WRK-OBJYMD(7:2)
      *
      *    点数マスタ読込み
               MOVE    NSRY-SRYCD(IDX1)     TO  WRK-SRYCD
               PERFORM  900-TENSU-READ-SEC
      *
      *    外泊日に食事などのフラグを消す
               PERFORM  920-NYUINACCT-READ-SEC
      *
               IF  NACCT-DAY(IDX-DAY)  >  ZERO
      *
      *    キー設定
      *    順序設定(入院料固定)
                 MOVE    11          TO  TA02TMP-SORTKBN
      *
                 MOVE    NSRY-PTID   TO  WRK-PTID
      *
      *    科別、医師別区分
                 EVALUATE  WRK-PARA-PRTKBN
                   WHEN  SPACE
                     MOVE    1                TO  TA02TMP-PRTKBN
                   WHEN  "1"
      *    入院履歴情報取得
                     PERFORM  900-PTNYUINRRK-READ-SEC
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                              UNTIL  IDX2  >  WRK-MAX
                       MOVE    IDX2           TO  TA02TMP-PRTKBN
                       IF  PTNYUINRRK-NYUINKA  =  WRK-NAMEKBN(IDX2)(4:2)
                           AND  WRK-OBJYMD  >=  WRK-STYUKYMD(IDX2)
                           AND  WRK-OBJYMD  <=  WRK-EDYUKYMD(IDX2)
                         MOVE    999          TO  IDX2
                       END-IF
                     END-PERFORM
                   WHEN  "2"
      *    入院履歴情報取得
                     PERFORM  900-PTNYUINRRK-READ-SEC
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                              UNTIL  IDX2  >  WRK-MAX
                       MOVE    IDX2           TO  TA02TMP-PRTKBN
                       IF  PTNYUINRRK-DRCD1  =  WRK-NAMEKBN(IDX2)
                           AND  WRK-OBJYMD   >=  WRK-STYUKYMD(IDX2)
                           AND  WRK-OBJYMD   <=  WRK-EDYUKYMD(IDX2)
                         MOVE    999          TO  IDX2
                       END-IF
                     END-PERFORM
      *    入院履歴情報取得
                   WHEN  "3"
                     PERFORM  900-PTNYUINRRK-READ-SEC
                     PERFORM  VARYING  IDX2  FROM  1  BY  1
                              UNTIL  IDX2  >  WRK-MAX
                       MOVE    IDX2           TO  TA02TMP-PRTKBN
                       IF  PTNYUINRRK-BTUNUM  =  WRK-NAMEKBN(IDX2)(4:2)
                           AND  WRK-OBJYMD  >=  WRK-STYUKYMD(IDX2)
                           AND  WRK-OBJYMD  <=  WRK-EDYUKYMD(IDX2)
                         MOVE    999          TO  IDX2
                       END-IF
                     END-PERFORM
                 END-EVALUATE
      *
      *        診療区分
                 MOVE    NSRY-SRYKBN          TO  TA02TMP-SRYKBN
      *
      *        診療名称
                 MOVE    TNS-NAME             TO  TA02TMP-SRYNAME
      *
      *        診療コード
                 MOVE    NSRY-SRYCD(IDX1)     TO  TA02TMP-SRYCD
      *
      *        点数・薬価

                 IF  TNS-TENSIKIBETU  =  7
                   COMPUTE  TNS-TEN  =  TNS-TEN  *  -1
                 END-IF
      *
      *    室料差額
                 EVALUATE  NACCT-ZAISKBKBN
                   WHEN  "3"
                     INITIALIZE               SYS-5113-REC
                     MOVE    SPA-HOSPNUM      TO  SYS-5113-HOSPNUM
                     MOVE    "5113"           TO  SYS-5113-KANRICD
                     STRING  NACCT-DAY(IDX-DAY)(2:2)
                                              DELIMITED  BY  SIZE
                             "%"              DELIMITED  BY  SIZE
                                              INTO  SYS-5113-KBNCD
                     END-STRING
                     MOVE    WRK-OBJYMD       TO  SYS-5113-STYUKYMD
                     MOVE    WRK-OBJYMD       TO  SYS-5113-EDYUKYMD
                     PERFORM  920-SYSKANRI-DBSELECT-SEC
                     MOVE    SYS-5113-BRM-SAGAKU-NM  TO  TA02TMP-YAKKA
                                                         TA02TMP-TANKA
                                                         WRK-TENSU
      *    室料差額は自費の為点数単価＝1
                     MOVE    1                TO  WRK-TENTNK
                   WHEN  "4"
      *    食事療養費
                     IF  TNS-DATAKBN  =  "1"
      *
                       IF  LNK-NYULIFE2-KBN (IDX-DAY)  NOT =  "1"
      *                食事療養
                         EVALUATE  NACCT-DAY(IDX-DAY)
                         WHEN  1
      *                    診療名称
                           EVALUATE  NSRY-SRYCD(IDX1)
                           WHEN  "099999918"
                             MOVE  "食事療養のみ（朝）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999919"
                             MOVE  "食事療養のみ（昼）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999920"
                             MOVE  "食事療養のみ（夕）"
                                                  TO  TA02TMP-SRYNAME
                           END-EVALUATE
      *                    金額
                           MOVE    WRK-SYOKUJITEN1  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         WHEN  2
      *                    診療名称
                           EVALUATE  NSRY-SRYCD(IDX1)
                           WHEN  "099999918"
                             MOVE  "食事療養＋特食（朝）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999919"
                             MOVE  "食事療養＋特食（昼）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999920"
                             MOVE  "食事療養＋特食（夕）"
                                                  TO  TA02TMP-SRYNAME
                           END-EVALUATE
      *                    金額
                           MOVE    WRK-SYOKUJITEN2  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         WHEN  3
      *                    診療名称
                           EVALUATE  NSRY-SRYCD(IDX1)
                           WHEN  "099999918"
                             MOVE  "流動食（朝）"  TO  TA02TMP-SRYNAME
                           WHEN  "099999919"
                             MOVE  "流動食（昼）"  TO  TA02TMP-SRYNAME
                           WHEN  "099999920"
                             MOVE  "流動食（夕）"  TO  TA02TMP-SRYNAME
                           END-EVALUATE
      *                    金額
                           MOVE    WRK-SYOKUJITEN3  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         WHEN  4
                           MOVE    WRK-SYOKUJITEN4  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         END-EVALUATE
                       ELSE
      *                生活療養
                         EVALUATE  NACCT-DAY(IDX-DAY)
                         WHEN  1
      *                    診療名称
                           EVALUATE  NSRY-SRYCD(IDX1)
                           WHEN  "099999918"
                             MOVE  "食事療養のみ（朝）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999919"
                             MOVE  "食事療養のみ（昼）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999920"
                             MOVE  "食事療養のみ（夕）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           END-EVALUATE
      *                    金額
                           MOVE    WRK-SEIKATUTEN1  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         WHEN  2
      *                    診療名称
                           EVALUATE  NSRY-SRYCD(IDX1)
                           WHEN  "099999918"
                             MOVE  "食事療養＋特食（朝）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999919"
                             MOVE  "食事療養＋特食（昼）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999920"
                             MOVE  "食事療養＋特食（夕）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           END-EVALUATE
      *                    金額
                           MOVE    WRK-SEIKATUTEN2  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         WHEN  3
      *                    診療名称
                           EVALUATE  NSRY-SRYCD(IDX1)
                           WHEN  "099999918"
                             MOVE  "流動食（朝）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999919"
                             MOVE  "流動食（昼）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           WHEN  "099999920"
                             MOVE  "流動食（夕）（生活療養）"
                                                  TO  TA02TMP-SRYNAME
                           END-EVALUATE
      *                    金額
                           MOVE    WRK-SEIKATUTEN3  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         WHEN  4
                           MOVE    WRK-SEIKATUTEN4  TO  TA02TMP-YAKKA
                                                        TA02TMP-TANKA
                                                        WRK-TENSU
                         END-EVALUATE
                       END-IF
      *
                     ELSE
                       EVALUATE  TNS-TENSIKIBETU
                         WHEN  0
                         WHEN  4
                         WHEN  5
                         WHEN  6
                         WHEN  7
                           MOVE    ZERO            TO  TA02TMP-YAKKA
                                                       TA02TMP-TANKA
                                                       WRK-TENSU
                         WHEN  OTHER
                           MOVE    TNS-TEN         TO  TA02TMP-YAKKA
                                                       TA02TMP-TANKA
                                                       WRK-TENSU
                       END-EVALUATE
                     END-IF
      *    食事療養は自費の為点数単価＝1
                     MOVE    1                TO  WRK-TENTNK
                   WHEN  OTHER
                     EVALUATE  TNS-TENSIKIBETU
                       WHEN  0
                       WHEN  4
                       WHEN  5
                       WHEN  6
                       WHEN  7
                         MOVE    ZERO            TO  TA02TMP-YAKKA
                                                     TA02TMP-TANKA
                                                     WRK-TENSU
                       WHEN  OTHER
                         MOVE    TNS-TEN         TO  TA02TMP-YAKKA
                                                     TA02TMP-TANKA
                                                     WRK-TENSU
                       END-EVALUATE
                 END-EVALUATE
      *
      *      TMPファイル読み込み
                 PERFORM  700-TMP-READ-SEC
      *
      *
      *    使用量カウント
      *    入院使用量
                 IF  TA02TMP-SORTKBN  =  12  OR  13  OR  14
                   COMPUTE  WRK-NYUIN-SIYO  =  NACCT-DAY(IDX-DAY)
                                            *  NSRY-SRYSURYO(IDX1)
                   ADD  WRK-NYUIN-SIYO    TO  TA02TMP-NYUIN-SIYO
                 END-IF
      *
      *    件数カウント
                 IF   TA02TMP-SORTKBN  NOT =  12  AND  13  AND  14
      *    入院件数
                   MOVE    1       TO  WRK-NYUIN-KEN
                   ADD  1          TO  TA02TMP-NYUIN-KEN
                 END-IF
      *
                 INITIALIZE         WRK-GAIRAI-KIN
                                    WRK-NYUIN-KIN
      *
                 COMPUTE  WRK-NYUIN-KIN  =  WRK-TENSU
                                         *  WRK-NYUIN-KEN
                                         *  WRK-TENTNK
                 ADD  WRK-NYUIN-KIN       TO  TA02TMP-NYUIN-KIN
      *
      *        診療区分別小計集計
                 MOVE    TA02TMP-PRTKBN     TO  IDX2
                 MOVE    TA02TMP-SORTKBN    TO  IDX3
                 MOVE    NSRY-SRYKBN        TO  IDX4
                 ADD  WRK-NYUIN-KIN
                                 TO  WRK-HIRITU-SYOKEI(IDX2 IDX3 IDX4)
                 ADD  WRK-GAIRAI-KIN
                                 TO  WRK-HIRITU-SYOKEI(IDX2 IDX3 IDX4)
      *
      *        総合計金額集計
                 ADD  WRK-NYUIN-KIN    TO  WRK-HIRITU-GOUKEI(IDX2 IDX3)
                 ADD  WRK-GAIRAI-KIN   TO  WRK-HIRITU-GOUKEI(IDX2 IDX3)
      *
                 IF  FLG-INVALID  =  1
                   MOVE    1                TO  FLG-KENSU
                   WRITE   TA02TMP-REC
                 ELSE
                   REWRITE TA02TMP-REC
                 END-IF
               END-IF
             END-PERFORM
           END-PERFORM
      *
           .
       400-NYUINACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       400-GPAGE-SEC            SECTION.
      *
           OPEN    INPUT       TA02-FILE
           MOVE    ZERO        TO  FLG-PRINT
                                   CNT-GPAGE
           MOVE    1           TO  IDX1
      *
           INITIALIZE          TA02TMP-REC
           INITIALIZE          TA02
      *
                               WRK-ZENSORTKBN
                               WRK-ZENSRYKBN
                               WRK-ZENPRTKBN
      *
      *    一時ファイル読込
           PERFORM  710-TMP-READ-SEC
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
      *    前回情報を保持
             MOVE    TA02TMP-SORTKBN    TO  WRK-ZENSORTKBN
             MOVE    TA02TMP-SRYKBN     TO  WRK-ZENSRYKBN
             MOVE    TA02TMP-PRTKBN     TO  WRK-ZENPRTKBN
      *    一時ファイル読込
             PERFORM  710-TMP-READ-SEC
             ADD  1            TO  IDX1
      *
             IF  IDX1  >  34
      *    合計頁数編集
               ADD  1          TO  CNT-GPAGE
               MOVE    1       TO  IDX1
             END-IF
      *
             IF  (     TA02TMP-SRYKBN  NOT =  WRK-ZENSRYKBN
                   OR  TA02TMP-PRTKBN  NOT =  WRK-ZENPRTKBN )
      *    小計計算
               ADD  1          TO  IDX1
      *
               IF  IDX1  >  34
      *    合計頁数編集
                 ADD  1        TO  CNT-GPAGE
                 MOVE    1     TO  IDX1
               END-IF
             END-IF
      *
             IF  (     TA02TMP-SORTKBN  NOT =  WRK-ZENSORTKBN
                   OR  TA02TMP-PRTKBN   NOT =  WRK-ZENPRTKBN  )
      *    合計頁数編集
               ADD  1          TO  CNT-GPAGE
               MOVE    1       TO  IDX1
             END-IF
      *
           END-PERFORM
      *
      *    合計頁数編集
           ADD  1              TO  CNT-GPAGE
           MOVE    1           TO  IDX1
      *
           CLOSE  TA02-FILE
      *
           .
       400-GPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       400-PRINT-SEC            SECTION.
      *
           OPEN    INPUT       TA02-FILE
           MOVE    ZERO        TO  FLG-PRINT
           MOVE    1           TO  IDX1
      *
           INITIALIZE          TA02TMP-REC
           INITIALIZE          TA02
      *
                               WRK-ZENSORTKBN
                               WRK-ZENSRYKBN
                               WRK-ZENPRTKBN
      *
      *    一時ファイル読込
           PERFORM  710-TMP-READ-SEC
      *
      *    CSVヘッダー処理
           IF   TA02TMP-SORTKBN  NOT =  12  AND  13  AND  14
             PERFORM  500-CSV-HEAD-SEC
           ELSE
             PERFORM  510-CSV-HEAD-SEC
           END-IF
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
      *    診療区分
             MOVE    TA02TMP-SRYKBN       TO  TA02-SRYKBN(IDX1)
      *
      *    診療コード
             MOVE    TA02TMP-SRYCD        TO  TA02-SRYCD(IDX1)
      *
      *    診療名称
             MOVE    TA02TMP-SRYNAME      TO  TA02-SRYNAME(IDX1)
      *
      *    薬価
             MOVE    TA02TMP-YAKKA        TO  WRK-V12
             MOVE    WRK-V12              TO  TA02-YAKKA(IDX1)
      *
      *    単価
             MOVE    TA02TMP-TANKA        TO  WRK-V12
             MOVE    WRK-V12              TO  TA02-TANKA(IDX1)
      *
      *    外来件数
             IF    TA02TMP-GAIRAI-KEN(8:2)    = "00"
               MOVE    TA02TMP-GAIRAI-KEN   TO  WRK-V9
               MOVE    WRK-V9               TO  TA02-GAIRAI-KEN(IDX1)
             ELSE
               IF    TA02TMP-GAIRAI-KEN(9:1)    = "0"
                 MOVE    TA02TMP-GAIRAI-KEN   TO  WRK-V9-1
                 MOVE    WRK-V9-1             TO  TA02-GAIRAI-KEN(IDX1)
               ELSE
                 MOVE    TA02TMP-GAIRAI-KEN   TO  WRK-V9-2
                 MOVE    WRK-V9-2             TO  TA02-GAIRAI-KEN(IDX1)
               END-IF
             END-IF
             ADD  TA02TMP-GAIRAI-KEN      TO  WRK-SUM-SGAIRAI-KEN
                                              WRK-SUM-GGAIRAI-KEN
      *
      *    入院件数
             IF    TA02TMP-NYUIN-KEN(8:2)    = "00"
               MOVE    TA02TMP-NYUIN-KEN    TO  WRK-V9
               MOVE    WRK-V9               TO  TA02-NYUIN-KEN(IDX1)
             ELSE
               IF    TA02TMP-NYUIN-KEN(9:1)    = "0"
                 MOVE    TA02TMP-NYUIN-KEN    TO  WRK-V9-1
                 MOVE    WRK-V9-1             TO  TA02-NYUIN-KEN(IDX1)
               ELSE
                 MOVE    TA02TMP-NYUIN-KEN    TO  WRK-V9-2
                 MOVE    WRK-V9-2             TO  TA02-NYUIN-KEN(IDX1)
               END-IF
             END-IF
             ADD  TA02TMP-NYUIN-KEN       TO  WRK-SUM-SNYUIN-KEN
                                              WRK-SUM-GNYUIN-KEN
      *
      *    合計件数
             COMPUTE  WRK-KAISU  =  TA02TMP-GAIRAI-KEN
                                 +  TA02TMP-NYUIN-KEN
             IF    WRK-KAISU(9:2)    = "00"
               MOVE    WRK-KAISU            TO  WRK-V10
               MOVE    WRK-V10              TO  TA02-TOTAL-KEN(IDX1)
             ELSE
               IF    WRK-KAISU(10:1)    = "0"
                 MOVE    WRK-KAISU            TO  WRK-V10-1
                 MOVE    WRK-V10-1            TO  TA02-TOTAL-KEN(IDX1)
               ELSE
                 MOVE    WRK-KAISU            TO  WRK-V10-2
                 MOVE    WRK-V10-2            TO  TA02-TOTAL-KEN(IDX1)
               END-IF
             END-IF
             ADD  WRK-KAISU               TO  WRK-SUM-STOTAL-KEN
                                              WRK-SUM-GTOTAL-KEN
      *
      *    外来使用量
             IF    TA02TMP-GAIRAI-SIYO(8:2)    = "00"
               MOVE    TA02TMP-GAIRAI-SIYO  TO  WRK-V9
               MOVE    WRK-V9               TO  TA02-GAIRAI-SIYO(IDX1)
             ELSE
               IF    TA02TMP-GAIRAI-SIYO(9:1)    = "0"
                 MOVE    TA02TMP-GAIRAI-SIYO  TO  WRK-V9-1
                 MOVE    WRK-V9-1             TO  TA02-GAIRAI-SIYO(IDX1)
               ELSE
                 MOVE    TA02TMP-GAIRAI-SIYO  TO  WRK-V9-2
                 MOVE    WRK-V9-2             TO  TA02-GAIRAI-SIYO(IDX1)
               END-IF
             END-IF
             ADD  TA02TMP-GAIRAI-SIYO     TO  WRK-SUM-SGAIRAI-SIYO
                                              WRK-SUM-GGAIRAI-SIYO
      *
      *    入院使用量
             IF    TA02TMP-NYUIN-SIYO(8:2)    = "00"
               MOVE    TA02TMP-NYUIN-SIYO   TO  WRK-V9
               MOVE    WRK-V9               TO  TA02-NYUIN-SIYO(IDX1)
             ELSE
               IF    TA02TMP-NYUIN-SIYO(9:1)    = "0"
                 MOVE    TA02TMP-NYUIN-SIYO   TO  WRK-V9-1
                 MOVE    WRK-V9-1             TO  TA02-NYUIN-SIYO(IDX1)
               ELSE
                 MOVE    TA02TMP-NYUIN-SIYO   TO  WRK-V9-2
                 MOVE    WRK-V9-2             TO  TA02-NYUIN-SIYO(IDX1)
               END-IF
             END-IF
             ADD  TA02TMP-NYUIN-SIYO      TO  WRK-SUM-SNYUIN-SIYO
                                              WRK-SUM-GNYUIN-SIYO
      *
      *    合計使用量
             COMPUTE  WRK-KAISU  =  TA02TMP-GAIRAI-SIYO
                                 +  TA02TMP-NYUIN-SIYO
             IF    WRK-KAISU(9:2)    = "00"
               MOVE    WRK-KAISU            TO  WRK-V10
               MOVE    WRK-V10              TO  TA02-TOTAL-SIYO(IDX1)
             ELSE
               IF    WRK-KAISU(10:1)    = "0"
                 MOVE    WRK-KAISU            TO  WRK-V10-1
                 MOVE    WRK-V10-1            TO  TA02-TOTAL-SIYO(IDX1)
               ELSE
                 MOVE    WRK-KAISU            TO  WRK-V10-2
                 MOVE    WRK-V10-2            TO  TA02-TOTAL-SIYO(IDX1)
               END-IF
             END-IF
             ADD  WRK-KAISU               TO  WRK-SUM-STOTAL-SIYO
                                              WRK-SUM-GTOTAL-SIYO
      *
      *    外来金額
             MOVE    TA02TMP-GAIRAI-KIN   TO  WRK-Z11
             MOVE    WRK-Z11              TO  TA02-GAIRAI-KIN(IDX1)
             ADD  TA02TMP-GAIRAI-KIN      TO  WRK-SUM-SGAIRAI-KIN
                                              WRK-SUM-GGAIRAI-KIN
      *
      *    入院金額
             MOVE    TA02TMP-NYUIN-KIN    TO  WRK-Z11
             MOVE    WRK-Z11              TO  TA02-NYUIN-KIN(IDX1)
             ADD  TA02TMP-NYUIN-KIN       TO  WRK-SUM-SNYUIN-KIN
                                              WRK-SUM-GNYUIN-KIN
      *
      *    合計金額
             COMPUTE  WRK-KINGAKU  =  TA02TMP-GAIRAI-KIN
                                   +  TA02TMP-NYUIN-KIN
             MOVE    WRK-KINGAKU          TO  WRK-Z11
             MOVE    WRK-Z11              TO  TA02-TOTAL-KIN(IDX1)
             ADD  WRK-KINGAKU             TO  WRK-SUM-STOTAL-KIN
                                              WRK-SUM-GTOTAL-KIN
      *
      *    全体比率
             MOVE    TA02TMP-PRTKBN       TO  IDX2
             MOVE    TA02TMP-SORTKBN      TO  IDX3
             COMPUTE  WRK-HIRITU  ROUNDED  =  WRK-KINGAKU
                            /  WRK-HIRITU-GOUKEI(IDX2 IDX3)  *  100
             IF  WRK-HIRITU  =  100
               MOVE    WRK-HIRITU         TO  WRK-V5-2
               MOVE    WRK-V5-2           TO  TA02-ZENHI(IDX1)
             ELSE
               MOVE    WRK-HIRITU         TO  WRK-V5
               MOVE    WRK-V5             TO  TA02-ZENHI(IDX1)
             END-IF
             ADD  WRK-HIRITU              TO  WRK-SUM-SZENHI
                                              WRK-SUM-GZENHI
      *
      *    グループ比率
             MOVE    TA02TMP-SRYKBN       TO  IDX4
             COMPUTE  WRK-HIRITU  ROUNDED  =  WRK-KINGAKU
                            /  WRK-HIRITU-SYOKEI(IDX2 IDX3 IDX4) * 100
             IF  WRK-HIRITU  =  100
               MOVE    WRK-HIRITU         TO  WRK-V5-2
               MOVE    WRK-V5-2           TO  TA02-GRPHI(IDX1)
             ELSE
               MOVE    WRK-HIRITU         TO  WRK-V5
               MOVE    WRK-V5             TO  TA02-GRPHI(IDX1)
             END-IF
      *
      *    CSV明細処理
             PERFORM  500-CSV-MEI-SEC
      *
      *    前回情報を保持
             MOVE    TA02TMP-SORTKBN      TO  WRK-ZENSORTKBN
             MOVE    TA02TMP-SRYKBN       TO  WRK-ZENSRYKBN
             MOVE    TA02TMP-PRTKBN       TO  WRK-ZENPRTKBN
      *    一時ファイル読込
             PERFORM  710-TMP-READ-SEC
             ADD  1          TO  IDX1
      *
             IF   IDX1  >  34
      *    印刷処理
               IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
                 PERFORM  510-PRINT-OUT-SEC
               ELSE
                 PERFORM  500-PRINT-OUT-SEC
               END-IF
             END-IF
      *
             IF  (     TA02TMP-SRYKBN  NOT =  WRK-ZENSRYKBN
                   OR  TA02TMP-PRTKBN  NOT =  WRK-ZENPRTKBN )
      *    小計計算
               PERFORM  500-SYOKEI-HEN-SEC
               IF   IDX1  >  34
      *    印刷処理
                 IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
                   PERFORM  510-PRINT-OUT-SEC
                 ELSE
                   PERFORM  500-PRINT-OUT-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF  (     TA02TMP-SORTKBN  NOT =  WRK-ZENSORTKBN
                   OR  TA02TMP-PRTKBN   NOT =  WRK-ZENPRTKBN  )
      *    合計集計
               PERFORM  500-GOUKEI-HEN-SEC
      *    印刷処理
               IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
                 PERFORM  510-PRINT-OUT-SEC
               ELSE
                 PERFORM  500-PRINT-OUT-SEC
               END-IF
      *
               IF  TA02TMP-SORTKBN  =  12  OR  13  OR  14
      *    CSVヘッダー処理
                 PERFORM  510-CSV-HEAD-SEC
               ELSE
      *    CSVヘッダー処理
                 PERFORM  500-CSV-HEAD-SEC
               END-IF
             END-IF
      *
           END-PERFORM
      *
      *    小計計算
           PERFORM  500-SYOKEI-HEN-SEC
      *
      *    合計集計
           PERFORM  500-GOUKEI-HEN-SEC
      *
      *    印刷処理
           IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
             PERFORM  510-PRINT-OUT-SEC
           ELSE
             PERFORM  500-PRINT-OUT-SEC
           END-IF
      *
           MOVE    1          TO  FLG-END
      *
           CLOSE  TA02-FILE
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分印刷順序設定(診療行為)
      *****************************************************************
       500-SORTKBN-SEC            SECTION.
      *
           EVALUATE  SRY-SRYKBN
      *    診察料
             WHEN  "11"  THRU  "14"
               MOVE    1           TO  TA02TMP-SORTKBN
      *    調剤・処方
             WHEN  "21"  THRU  "27"
               MOVE    2           TO  TA02TMP-SORTKBN
      *    注射
             WHEN  "31"  THRU  "33"
               MOVE    3           TO  TA02TMP-SORTKBN
      *    処置
             WHEN  "40"
               MOVE    4           TO  TA02TMP-SORTKBN
      *    手術・麻酔
             WHEN  "50"
             WHEN  "54"
               MOVE    5           TO  TA02TMP-SORTKBN
      *    検査
             WHEN  "60"
               MOVE    6           TO  TA02TMP-SORTKBN
      *    病理診断
      *       WHEN  "64"
      *         MOVE    7           TO  TA02TMP-SORTKBN
             WHEN  "70"
               IF  TNS-DATAKBN  =  3
      *    フィルム
                 MOVE    8         TO  TA02TMP-SORTKBN
               ELSE
      *    画像診断
                 MOVE    9         TO  TA02TMP-SORTKBN
               END-IF
             WHEN  "90"
             WHEN  "92"
             WHEN  "97"
      *    入院料
               MOVE    11          TO  TA02TMP-SORTKBN
             WHEN  OTHER
      *    病理・その他
               MOVE    10          TO  TA02TMP-SORTKBN
           END-EVALUATE
      *
      *    薬剤
           IF  SRY-SRYCD(IDX1)(1:1)  =  "6"
             MOVE    12            TO  TA02TMP-SORTKBN
           END-IF
      *
      *    材料・器材
           IF  SRY-SRYCD(IDX1)(1:1)  =  "7"
               OR  ( SRY-SRYCD(IDX1)(1:3)  =  "059"
               AND  TNS-DATAKBN  NOT =  3 )
             MOVE    13            TO  TA02TMP-SORTKBN
           END-IF
      *
      *    自費項目
           IF  SRY-SRYCD(IDX1)(1:3)  =  "095"
               OR  SRY-SRYCD(IDX1)(1:3)  =  "096"
             MOVE    14            TO  TA02TMP-SORTKBN
           END-IF
      *
      *    その他（労災・自賠責入力）
           IF   ( SRY-SRYCD(IDX1)(1:5)  =  "09591" OR "09592" OR
                                           "09593" OR "09594"    )
             MOVE    10            TO  TA02TMP-SORTKBN
           END-IF
      *
           .
       500-SORTKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    小計集計
      *****************************************************************
       500-SYOKEI-HEN-SEC            SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *
      *    名称
           MOVE    "　　　　　　　診区別　小計"
                                         TO  TA02-SRYNAME(IDX1)
      *
      *    外来件数
           IF    WRK-SUM-SGAIRAI-KEN(8:2)    = "00"
             MOVE    WRK-SUM-SGAIRAI-KEN   TO  WRK-V9
             MOVE    WRK-V9                TO  TA02-GAIRAI-KEN(IDX1)
           ELSE
             IF    WRK-SUM-SGAIRAI-KEN(9:1)    = "0"
               MOVE    WRK-SUM-SGAIRAI-KEN   TO  WRK-V9-1
               MOVE    WRK-V9-1              TO  TA02-GAIRAI-KEN(IDX1)
             ELSE
               MOVE    WRK-SUM-SGAIRAI-KEN   TO  WRK-V9-2
               MOVE    WRK-V9-2              TO  TA02-GAIRAI-KEN(IDX1)
             END-IF
           END-IF
      *
      *    入院件数
           IF    WRK-SUM-SNYUIN-KEN(8:2)    = "00"
             MOVE    WRK-SUM-SNYUIN-KEN    TO  WRK-V9
             MOVE    WRK-V9                TO  TA02-NYUIN-KEN(IDX1)
           ELSE
             IF    WRK-SUM-SNYUIN-KEN(9:1)    = "0"
               MOVE    WRK-SUM-SNYUIN-KEN    TO  WRK-V9-1
               MOVE    WRK-V9-1              TO  TA02-NYUIN-KEN(IDX1)
             ELSE
               MOVE    WRK-SUM-SNYUIN-KEN    TO  WRK-V9-2
               MOVE    WRK-V9-2              TO  TA02-NYUIN-KEN(IDX1)
             END-IF
           END-IF
      *
      *    合計件数
           IF    WRK-SUM-STOTAL-KEN(9:2)    = "00"
             MOVE    WRK-SUM-STOTAL-KEN    TO  WRK-V10
             MOVE    WRK-V10               TO  TA02-TOTAL-KEN(IDX1)
           ELSE
             IF    WRK-SUM-STOTAL-KEN(10:1)    = "0"
               MOVE    WRK-SUM-STOTAL-KEN    TO  WRK-V10-1
               MOVE    WRK-V10-1             TO  TA02-TOTAL-KEN(IDX1)
             ELSE
               MOVE    WRK-SUM-STOTAL-KEN    TO  WRK-V10-2
               MOVE    WRK-V10-2             TO  TA02-TOTAL-KEN(IDX1)
             END-IF
           END-IF
      *
      *    外来使用量
           IF    WRK-SUM-SGAIRAI-SIYO(8:2)    = "00"
             MOVE    WRK-SUM-SGAIRAI-SIYO  TO  WRK-V9
             MOVE    WRK-V9                TO  TA02-GAIRAI-SIYO(IDX1)
           ELSE
             IF    WRK-SUM-SGAIRAI-SIYO(9:1)    = "0"
               MOVE    WRK-SUM-SGAIRAI-SIYO  TO  WRK-V9-1
               MOVE    WRK-V9-1              TO  TA02-GAIRAI-SIYO(IDX1)
             ELSE
               MOVE    WRK-SUM-SGAIRAI-SIYO  TO  WRK-V9-2
               MOVE    WRK-V9-2              TO  TA02-GAIRAI-SIYO(IDX1)
             END-IF
           END-IF
      *
      *    入院使用量
           IF    WRK-SUM-SNYUIN-SIYO(8:2)    = "00"
             MOVE    WRK-SUM-SNYUIN-SIYO   TO  WRK-V9
             MOVE    WRK-V9                TO  TA02-NYUIN-SIYO(IDX1)
           ELSE
             IF    WRK-SUM-SNYUIN-SIYO(9:1)    = "0"
               MOVE    WRK-SUM-SNYUIN-SIYO   TO  WRK-V9-1
               MOVE    WRK-V9-1              TO  TA02-NYUIN-SIYO(IDX1)
             ELSE
               MOVE    WRK-SUM-SNYUIN-SIYO   TO  WRK-V9-2
               MOVE    WRK-V9-2              TO  TA02-NYUIN-SIYO(IDX1)
             END-IF
           END-IF
      *
      *    合計使用量
           IF    WRK-SUM-STOTAL-SIYO(9:2)    = "00"
             MOVE    WRK-SUM-STOTAL-SIYO   TO  WRK-V10
             MOVE    WRK-V10               TO  TA02-TOTAL-SIYO(IDX1)
           ELSE
             IF    WRK-SUM-STOTAL-SIYO(10:1)    = "0"
               MOVE    WRK-SUM-STOTAL-SIYO   TO  WRK-V10-1
               MOVE    WRK-V10-1             TO  TA02-TOTAL-SIYO(IDX1)
             ELSE
               MOVE    WRK-SUM-STOTAL-SIYO   TO  WRK-V10-2
               MOVE    WRK-V10-2             TO  TA02-TOTAL-SIYO(IDX1)
             END-IF
           END-IF
      *
      *    外来金額
           MOVE    WRK-SUM-SGAIRAI-KIN   TO  WRK-Z11
           MOVE    WRK-Z11               TO  TA02-GAIRAI-KIN(IDX1)
      *
      *    入院金額
           MOVE    WRK-SUM-SNYUIN-KIN    TO  WRK-Z11
           MOVE    WRK-Z11               TO  TA02-NYUIN-KIN(IDX1)
      *
      *    合計金額
           MOVE    WRK-SUM-STOTAL-KIN    TO  WRK-Z11
           MOVE    WRK-Z11               TO  TA02-TOTAL-KIN(IDX1)
      *
      *    全体比率
           IF  WRK-SUM-SZENHI  >=  100
             MOVE    WRK-SUM-SZENHI      TO  WRK-V5-2
             MOVE    WRK-V5-2            TO  TA02-ZENHI(IDX1)
           ELSE
             MOVE    WRK-SUM-SZENHI      TO  WRK-V5
             MOVE    WRK-V5              TO  TA02-ZENHI(IDX1)
           END-IF
      *
      *    グループ比率
           MOVE    "100.0"               TO  TA02-GRPHI(IDX1)
      *
      *    CSV小計集計
           PERFORM  600-CSV-SYOKEI-HEN-SEC
      *
      *    CSV出力処理
           PERFORM  500-CSV-OUTPUT-SEC
      *
           INITIALIZE         SUM-S-AREA
           ADD  1             TO   IDX1
      *
            .
       500-SYOKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    合計集計
      *****************************************************************
       500-GOUKEI-HEN-SEC            SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *
      *    外来件数
           IF    WRK-SUM-GGAIRAI-KEN(9:2)    = "00"
             MOVE    WRK-SUM-GGAIRAI-KEN   TO  WRK-V10
             MOVE    WRK-V10               TO  TA02-GGAIRAI-KEN
           ELSE
             IF    WRK-SUM-GGAIRAI-KEN(10:1)    = "0"
               MOVE    WRK-SUM-GGAIRAI-KEN   TO  WRK-V10-1
               MOVE    WRK-V10-1             TO  TA02-GGAIRAI-KEN
             ELSE
               MOVE    WRK-SUM-GGAIRAI-KEN   TO  WRK-V10-2
               MOVE    WRK-V10-2             TO  TA02-GGAIRAI-KEN
             END-IF
           END-IF
      *
      *    入院件数
           IF    WRK-SUM-GNYUIN-KEN(9:2)    = "00"
             MOVE    WRK-SUM-GNYUIN-KEN    TO  WRK-V10
             MOVE    WRK-V10               TO  TA02-GNYUIN-KEN
           ELSE
             IF    WRK-SUM-GNYUIN-KEN(10:1)    = "0"
               MOVE    WRK-SUM-GNYUIN-KEN    TO  WRK-V10-1
               MOVE    WRK-V10-1             TO  TA02-GNYUIN-KEN
             ELSE
               MOVE    WRK-SUM-GNYUIN-KEN    TO  WRK-V10-2
               MOVE    WRK-V10-2             TO  TA02-GNYUIN-KEN
             END-IF
           END-IF
      *
      *    合計件数
           IF    WRK-SUM-GTOTAL-KEN(10:2)    = "00"
             MOVE    WRK-SUM-GTOTAL-KEN    TO  WRK-V11
             MOVE    WRK-V11               TO  TA02-GTOTAL-KEN
           ELSE
             IF    WRK-SUM-GTOTAL-KEN(11:1)    = "0"
               MOVE    WRK-SUM-GTOTAL-KEN    TO  WRK-V11-1
               MOVE    WRK-V11-1             TO  TA02-GTOTAL-KEN
             ELSE
               MOVE    WRK-SUM-GTOTAL-KEN    TO  WRK-V11-2
               MOVE    WRK-V11-2             TO  TA02-GTOTAL-KEN
             END-IF
           END-IF
      *
      *    外来使用量
           IF    WRK-SUM-GGAIRAI-SIYO(9:2)    = "00"
             MOVE    WRK-SUM-GGAIRAI-SIYO   TO  WRK-V10
             MOVE    WRK-V10                TO  TA02-GGAIRAI-SIYO
           ELSE
             IF    WRK-SUM-GGAIRAI-SIYO(10:1)    = "0"
               MOVE    WRK-SUM-GGAIRAI-SIYO   TO  WRK-V10-1
               MOVE    WRK-V10-1              TO  TA02-GGAIRAI-SIYO
             ELSE
               MOVE    WRK-SUM-GGAIRAI-SIYO   TO  WRK-V10-2
               MOVE    WRK-V10-2              TO  TA02-GGAIRAI-SIYO
             END-IF
           END-IF
      *
      *    入院使用量
           IF    WRK-SUM-GNYUIN-SIYO(9:2)    = "00"
             MOVE    WRK-SUM-GNYUIN-SIYO   TO  WRK-V10
             MOVE    WRK-V10               TO  TA02-GNYUIN-SIYO
           ELSE
             IF    WRK-SUM-GNYUIN-SIYO(10:1)    = "0"
               MOVE    WRK-SUM-GNYUIN-SIYO   TO  WRK-V10-1
               MOVE    WRK-V10-1             TO  TA02-GNYUIN-SIYO
             ELSE
               MOVE    WRK-SUM-GNYUIN-SIYO   TO  WRK-V10-2
               MOVE    WRK-V10-2             TO  TA02-GNYUIN-SIYO
             END-IF
           END-IF
      *
      *    合計使用量
           IF    WRK-SUM-GTOTAL-SIYO(10:2)    = "00"
             MOVE    WRK-SUM-GTOTAL-SIYO   TO  WRK-V11
             MOVE    WRK-V11               TO  TA02-GTOTAL-SIYO
           ELSE
             IF    WRK-SUM-GTOTAL-SIYO(11:1)    = "0"
               MOVE    WRK-SUM-GTOTAL-SIYO   TO  WRK-V11-1
               MOVE    WRK-V11-1             TO  TA02-GTOTAL-SIYO
             ELSE
               MOVE    WRK-SUM-GTOTAL-SIYO   TO  WRK-V11-2
               MOVE    WRK-V11-2             TO  TA02-GTOTAL-SIYO
             END-IF
           END-IF
      *
      *    外来金額
           MOVE    WRK-SUM-GGAIRAI-KIN   TO  WRK-Z11
           MOVE    WRK-Z11               TO  TA02-GGAIRAI-KIN
      *
      *    入院金額
           MOVE    WRK-SUM-GNYUIN-KIN    TO  WRK-Z11
           MOVE    WRK-Z11               TO  TA02-GNYUIN-KIN
      *
      *    合計金額
           MOVE    WRK-SUM-GTOTAL-KIN    TO  WRK-Z12
           MOVE    WRK-Z12               TO  TA02-GTOTAL-KIN
      *
      *    全体比率
           MOVE    "100.0"               TO  TA02-GZENHI
      *
      *    グループ比率
           MOVE    "100.0"               TO  TA02-GGRPHI
      *
      *    CSV合計集計
           PERFORM  600-CSV-GOUKEI-HEN-SEC
      *
      *    CSV出力処理
           PERFORM  500-CSV-OUTPUT-SEC
      *
           INITIALIZE                    SUM-G-AREA
      *
            .
       500-GOUKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       500-PRINT-OUT-SEC                SECTION.
      *
      *    ヘッダー編集
           PERFORM  500-HEAD-HEN-SEC
      *
           INITIALIZE                        ORCSPRTAREA
           MOVE    "INS"                     TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM       TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM        TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD       TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID      TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY     TO  SPRT-PRIORITY
           MOVE    "A00000TA021.red"           TO  SPRT-PRTID
           MOVE    "診療行為別集計表"        TO  SPRT-TITLE
           MOVE    TA02                      TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID       TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID         TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM        TO  SPRT-PRTNM
           CALL    "ORCSPRT"                 USING
                                             ORCSPRTAREA
                                             SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
           INITIALIZE          TA02
           MOVE    1           TO  IDX1
      *
           .
       500-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       510-PRINT-OUT-SEC                SECTION.
      *
      *    ヘッダー編集
           PERFORM  500-HEAD-HEN-SEC
      *
           INITIALIZE                         ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           MOVE    "A00000TA022.red"            TO  SPRT-PRTID
           MOVE    "診療行為別集計表"         TO  SPRT-TITLE
           MOVE    TA02                    TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
                                              SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
           INITIALIZE          TA02
           MOVE    1           TO  IDX1
      *
           .
       510-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       500-HEAD-HEN-SEC          SECTION.
      *
      *    頁数編集
           ADD  1                 TO  CNT-PAGE
           MOVE    CNT-PAGE       TO  WRK-Z2
           MOVE    WRK-Z2         TO  TA02-PAGE
      *
           MOVE    CNT-GPAGE      TO  WRK-Z2
           MOVE    WRK-Z2         TO  TA02-GPAGE
      *
      *    集計対象年月日
           MOVE    WRK-PARA-PRTSTYMD     TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  TA02-PRTYMD(1:22)
           MOVE    "　〜　"              TO  TA02-PRTYMD(23:6)
           MOVE    WRK-PARA-PRTEDYMD     TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  TA02-PRTYMD(29:22)
      *    作成年月日
           MOVE    LNK-PRTKANRI-SKYYMD   TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  TA02-CREATEYMD
      *
      *    診療区分名称
           EVALUATE  WRK-ZENSORTKBN
             WHEN  1
               MOVE    "（診察料）"      TO  TA02-SRYKBNNAME
             WHEN  2
               MOVE    "（調剤・処方）"  TO  TA02-SRYKBNNAME
             WHEN  3
               MOVE    "（注射）"        TO  TA02-SRYKBNNAME
             WHEN  4
               MOVE    "（処置）"        TO  TA02-SRYKBNNAME
             WHEN  5
               MOVE    "（手術・麻酔）"  TO  TA02-SRYKBNNAME
             WHEN  6
               MOVE    "（検査）"        TO  TA02-SRYKBNNAME
             WHEN  7
               MOVE    "（病理診断）"    TO  TA02-SRYKBNNAME
             WHEN  8
               MOVE    "（フィルム）"    TO  TA02-SRYKBNNAME
             WHEN  9
               MOVE    "（画像診断）"    TO  TA02-SRYKBNNAME
             WHEN  10
               MOVE    "（病理・その他）"    TO  TA02-SRYKBNNAME
             WHEN  11
               MOVE    "（入院料）"      TO  TA02-SRYKBNNAME
             WHEN  12
               MOVE    "（薬剤）"        TO  TA02-SRYKBNNAME
             WHEN  13
               MOVE    "（材料・器材）"  TO  TA02-SRYKBNNAME
             WHEN  14
               MOVE    "（自費項目）"    TO  TA02-SRYKBNNAME
           END-EVALUATE
      *
      *    印刷種別
           EVALUATE  WRK-PARA-PRTKBN
             WHEN  SPACE
               MOVE    SPACE             TO  TA02-GRPTITLE
             WHEN  "1"
               MOVE    "［科別］"        TO  TA02-GRPTITLE
             WHEN  "2"
               MOVE    "［医師別］"      TO  TA02-GRPTITLE
             WHEN  "3"
               MOVE    "［病棟別］"      TO  TA02-GRPTITLE
           END-EVALUATE
      *
      *    科名、医師名
           IF  WRK-PARA-PRTKBN  =  "1"  OR  "2"  OR  "3"
             IF  WRK-NAME(WRK-ZENPRTKBN)  =  SPACE
               MOVE    "該当なし"        TO  TA02-KANAME
             ELSE
               MOVE    WRK-NAME(WRK-ZENPRTKBN)    TO  TA02-KANAME
             END-IF
           END-IF
           .
       500-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV明細編集
      *****************************************************************
       500-CSV-MEI-SEC               SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *
      *    診療区分
           MOVE    TA02TMP-SRYKBN           TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    診療コード
           MOVE    TA02TMP-SRYCD            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    診療名称
           MOVE    TA02TMP-SRYNAME          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
           IF  TA02TMP-SORTKBN  =  12  OR  13  OR  14
      *    単価
             MOVE    TA02TMP-TANKA          TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    外来使用量
             MOVE    TA02TMP-GAIRAI-SIYO    TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院使用量
             MOVE    TA02TMP-NYUIN-SIYO     TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計使用量
             COMPUTE  WRK-KAISU  =  TA02TMP-GAIRAI-SIYO
                                 +  TA02TMP-NYUIN-SIYO
             MOVE    WRK-KAISU              TO  WRK-CSV-V11
             MOVE    WRK-CSV-V11            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
           ELSE
      *    薬価
             MOVE    TA02TMP-YAKKA          TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    外来件数
             MOVE    TA02TMP-GAIRAI-KEN     TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院件数
             MOVE    TA02TMP-NYUIN-KEN      TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計件数
             COMPUTE  WRK-KAISU  =  TA02TMP-GAIRAI-KEN
                                 +  TA02TMP-NYUIN-KEN
             MOVE    WRK-KAISU              TO  WRK-CSV-V11
             MOVE    WRK-CSV-V11            TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
           END-IF
      *
      *    外来金額
           MOVE    TA02TMP-GAIRAI-KIN       TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11              TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院金額
           MOVE    TA02TMP-NYUIN-KIN        TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11              TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計金額
           COMPUTE  WRK-KINGAKU  =  TA02TMP-GAIRAI-KIN
                                 +  TA02TMP-NYUIN-KIN
           MOVE    WRK-KINGAKU              TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11              TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    全体比率
           IF  WRK-SUM-SZENHI  >=  100
             MOVE    TA02-ZENHI(IDX1)    TO  WRK-CSV-V4-2
             MOVE    WRK-CSV-V4-2           TO  CSV-DATA
           ELSE
             MOVE    TA02-ZENHI(IDX1)    TO  WRK-CSV-V4
             MOVE    WRK-CSV-V4             TO  CSV-DATA
           END-IF
           IF   TA02TMP-SORTKBN  =  12  OR  13  OR  14
             PERFORM  510-CSV-DATAHEN-SEC
           ELSE
             PERFORM  500-CSV-DATAHEN-SEC
           END-IF
      *
           IF   TA02TMP-SORTKBN  NOT =  12  AND  13  AND  14
      *    グループ比率
             IF  WRK-SUM-SZENHI  >=  100
               MOVE    TA02-GRPHI(IDX1)  TO  WRK-CSV-V4-2
               MOVE    WRK-CSV-V4-2         TO  CSV-DATA
             ELSE
               MOVE    TA02-GRPHI(IDX1)  TO  WRK-CSV-V4
               MOVE    WRK-CSV-V4           TO  CSV-DATA
             END-IF
             PERFORM  510-CSV-DATAHEN-SEC
           END-IF
      *
      *    CSV出力処理
           PERFORM  500-CSV-OUTPUT-SEC
      *
           .
       500-CSV-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV小計集計
      *****************************************************************
       600-CSV-SYOKEI-HEN-SEC           SECTION.
      *
      *    診療区分（空白）
           MOVE    SPACE         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    診療行為コード（空白）
           MOVE    SPACE         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    名称
           MOVE    "診区別小計"            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    薬価・単価（空白）
           PERFORM  500-CSV-DATAHEN-SEC
      *
           IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
      *
      *    外来使用量
             MOVE    WRK-SUM-SGAIRAI-SIYO  TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院使用量
             MOVE    WRK-SUM-SNYUIN-SIYO   TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計使用量
             MOVE    WRK-SUM-STOTAL-SIYO   TO  WRK-CSV-V11
             MOVE    WRK-CSV-V11           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
           ELSE
      *    外来件数
             MOVE    WRK-SUM-SGAIRAI-KEN   TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院件数
             MOVE    WRK-SUM-SNYUIN-KEN    TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計件数
             MOVE    WRK-SUM-STOTAL-KEN    TO  WRK-CSV-V11
             MOVE    WRK-CSV-V11           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
           END-IF
      *
      *    外来金額
           MOVE    WRK-SUM-SGAIRAI-KIN     TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11             TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院金額
           MOVE    WRK-SUM-SNYUIN-KIN      TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11             TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計金額
           MOVE    WRK-SUM-STOTAL-KIN      TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11             TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    全体比率
           IF  WRK-SUM-SZENHI  >=  100
             MOVE    WRK-SUM-SZENHI        TO  WRK-CSV-V4-2
             MOVE    WRK-CSV-V4-2          TO  CSV-DATA
           ELSE
             MOVE    WRK-SUM-SZENHI        TO  WRK-CSV-V4
             MOVE    WRK-CSV-V4            TO  CSV-DATA
           END-IF
           IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
             PERFORM  510-CSV-DATAHEN-SEC
           ELSE
             PERFORM  500-CSV-DATAHEN-SEC
           END-IF
      *
      *    グループ比率
           IF  WRK-ZENSORTKBN  NOT =  12  AND  13  AND  14
             MOVE    "100.0"             TO  CSV-DATA
             PERFORM  510-CSV-DATAHEN-SEC
           END-IF
      *
           .
       600-CSV-SYOKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計集計
      *****************************************************************
       600-CSV-GOUKEI-HEN-SEC           SECTION.
      *
      *    診療区分（空白）
           MOVE    SPACE            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    診療行為コード（空白）
           MOVE    SPACE            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    名称
           MOVE    "総合計"         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    薬価・単価（空白）
           PERFORM  500-CSV-DATAHEN-SEC
      *
           IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
      *    外来使用量
             MOVE    WRK-SUM-GGAIRAI-SIYO  TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院使用量
             MOVE    WRK-SUM-GNYUIN-SIYO   TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計使用量
             MOVE    WRK-SUM-GTOTAL-SIYO   TO  WRK-CSV-V11
             MOVE    WRK-CSV-V11           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
           ELSE
      *    外来件数
             MOVE    WRK-SUM-GGAIRAI-KEN   TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院件数
             MOVE    WRK-SUM-GNYUIN-KEN    TO  WRK-CSV-V10
             MOVE    WRK-CSV-V10           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計件数
             MOVE    WRK-SUM-GTOTAL-KEN    TO  WRK-CSV-V11
             MOVE    WRK-CSV-V11           TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
           END-IF
      *
      *    外来金額
           MOVE    WRK-SUM-GGAIRAI-KIN     TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11             TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    入院金額
           MOVE    WRK-SUM-GNYUIN-KIN      TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11             TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    合計金額
           MOVE    WRK-SUM-GTOTAL-KIN      TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11             TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    全体比率
           MOVE    "100.0"                 TO  CSV-DATA
           IF  WRK-ZENSORTKBN  =  12  OR  13  OR  14
             PERFORM  510-CSV-DATAHEN-SEC
           ELSE
             PERFORM  500-CSV-DATAHEN-SEC
           END-IF
      *
      *    グループ比率
           IF  WRK-ZENSORTKBN  NOT =  12  AND  13  AND  14
             MOVE    "100.0"               TO  CSV-DATA
             PERFORM  510-CSV-DATAHEN-SEC
           END-IF
      *
           .
       600-CSV-GOUKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダー１ファイル編集
      *****************************************************************
       500-CSV-HEAD-SEC              SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *    ヘッダー共通部分
           PERFORM  520-CSV-HEAD-SEC
      *
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                    UNTIL  IDX5  >  CSV-HEAD01-MAX
      *
             MOVE    CSV-HEAD01-DATA(IDX5)    TO  CSV-DATA
      *
             IF  IDX5  >=  CSV-HEAD01-MAX
               PERFORM  510-CSV-DATAHEN-SEC
             ELSE
               PERFORM  500-CSV-DATAHEN-SEC
             END-IF
      *
           END-PERFORM
      *
      *    CSV出力処理
           PERFORM  500-CSV-OUTPUT-SEC
      *
           .
       500-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダー２編集
      *****************************************************************
       510-CSV-HEAD-SEC              SECTION.
      *
           MOVE    1       TO  IDX-POINT
      *    ヘッダー共通部分
           PERFORM  520-CSV-HEAD-SEC
      *
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                    UNTIL  IDX5  >  CSV-HEAD02-MAX
      *
             MOVE    CSV-HEAD02-DATA(IDX5)    TO  CSV-DATA
      *
             IF  IDX5  >=  CSV-HEAD02-MAX
               PERFORM  510-CSV-DATAHEN-SEC
             ELSE
               PERFORM  500-CSV-DATAHEN-SEC
             END-IF
      *
           END-PERFORM
      *
      *    CSV出力処理
           PERFORM  500-CSV-OUTPUT-SEC
      *
           .
       510-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダー共通部分編集
      *****************************************************************
       520-CSV-HEAD-SEC              SECTION.
      *
      *    集計対象年月日
           MOVE    WRK-PARA-PRTSTYMD     TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  CSV-DATA(1:22)
           MOVE    "　〜　"              TO  CSV-DATA(23:6)
           MOVE    WRK-PARA-PRTEDYMD     TO  WRK-SYMD
           PERFORM  700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  CSV-DATA(29:22)
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    診療区分名称
           EVALUATE  TA02TMP-SORTKBN
             WHEN  1
               MOVE    "（診察料）"         TO  CSV-DATA
             WHEN  2
               MOVE    "（調剤・処方）"     TO  CSV-DATA
             WHEN  3
               MOVE    "（注射）"           TO  CSV-DATA
             WHEN  4
               MOVE    "（処置）"           TO  CSV-DATA
             WHEN  5
               MOVE    "（手術・麻酔）"     TO  CSV-DATA
             WHEN  6
               MOVE    "（検査）"           TO  CSV-DATA
             WHEN  7
               MOVE    "（病理診断）"       TO  CSV-DATA
             WHEN  8
               MOVE    "（フィルム）"       TO  CSV-DATA
             WHEN  9
               MOVE    "（画像診断）"       TO  CSV-DATA
             WHEN  10
               MOVE    "（病理・その他）"   TO  CSV-DATA
             WHEN  11
               MOVE    "（入院料）"         TO  CSV-DATA
             WHEN  12
               MOVE    "（薬剤）"           TO  CSV-DATA
             WHEN  13
               MOVE    "（材料・器材）"     TO  CSV-DATA
             WHEN  14
               MOVE    "（自費項目）"       TO  CSV-DATA
           END-EVALUATE
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    印刷種別・印字区分
           EVALUATE  WRK-PARA-PRTKBN
             WHEN  SPACE
               MOVE    SPACE                TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
               MOVE    SPACE                TO  CSV-DATA
               PERFORM  510-CSV-DATAHEN-SEC
             WHEN  "1"
               MOVE    "［科別］"           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
               MOVE    WRK-NAME(TA02TMP-PRTKBN)   TO  CSV-DATA
               PERFORM  510-CSV-DATAHEN-SEC
             WHEN  "2"
               MOVE    "［医師別］"         TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
               MOVE    WRK-NAME(TA02TMP-PRTKBN)   TO  CSV-DATA
               PERFORM  510-CSV-DATAHEN-SEC
             WHEN  "3"
               MOVE    "［病棟別］"         TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
               MOVE    WRK-NAME(TA02TMP-PRTKBN)   TO  CSV-DATA
               PERFORM  510-CSV-DATAHEN-SEC
           END-EVALUATE
      *
           .
       520-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       500-CSV-DATAHEN-SEC            SECTION.
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
       510-CSV-DATAHEN-SEC            SECTION.
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
           IF  CSV-DATA  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  CSV-DATA(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-OUTPUT-DATA
                                              WITH  POINTER  IDX-POINT
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
                               INTO  WRK-OUTPUT-DATA
                               WITH  POINTER  IDX-POINT
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
                               INTO  WRK-OUTPUT-DATA
                               WITH  POINTER  IDX-POINT
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
           PERFORM  VARYING  IDX7  FROM  1  BY  1
                    UNTIL  IDX7  >  CSV-DATA-SIZE
                    OR  CSV-DATA(IDX7:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  CSV-DATA(IDX7:1)  NOT =  SPACE
                 MOVE    IDX7      TO  IDX-ST
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
       500-CSV-OUTPUT-SEC              SECTION.
      *
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID      TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM        TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN        TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "sryactlist"              DELIMITED  BY  SIZE
                   WRK-PARA-PRTSTYMD         DELIMITED  BY  SIZE
                   "-"                       DELIMITED  BY  SIZE
                   WRK-PARA-PRTEDYMD         DELIMITED  BY  SIZE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-OUTPUT-DATA           TO  STOUKEICSV-CSVDATA
           MOVE    "診療行為別集計表ＣＳＶ作成"  TO  STOUKEICSV-TITLE
           CALL    "ORCSTOUKEICSV"           USING
                                             ORCSTOUKEICSVAREA
                                             SPA-AREA
      *
           IF   STOUKEICSV-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE    "ＣＳＶＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  WRK-OUTPUT-DATA
      *
           .
       500-CSV-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(KEY READ)
      *****************************************************************
       700-TMP-READ-SEC                SECTION.
      *
           INITIALIZE                FLG-INVALID
           READ   TA02-FILE
               INVALID    KEY
                 MOVE    1           TO  FLG-INVALID
               NOT  INVALID  KEY
                 MOVE    ZERO        TO  FLG-INVALID
           END-READ
           .
       700-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(NEXT READ)
      *****************************************************************
       710-TMP-READ-SEC                SECTION.
      *
           READ    TA02-FILE  NEXT
               AT  END
                 MOVE    1           TO  FLG-PRINT
               NOT  AT END
                 MOVE    ZERO        TO  FLG-PRINT
           END-READ
           .
       710-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       800-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT             RECEERR-FILE
      *
             MOVE    WRK-RECEERR        TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                 JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           MOVE    1           TO  FLG-END
      *
           .
       800-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       700-SEIWA-HEN-SEC               SECTION.
      *
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY2-AREA
           MOVE    "21"                 TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"            USING
                                        STS-AREA-DAY
                                        LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3     TO  WRK-HENYMDG
           INSPECT  WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY2-AREA
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       600-END-SEC                      SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"                TO  SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE             TO  JOB-UPDCNT
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       600-END-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"     TO  MCP-TABLE
             MOVE    "key10"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    ZERO             TO  FLG-SYSKANRI
             ELSE
               MOVE    1                TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-SELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM  910-SYSKANRI-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    ZERO               TO  FLG-SYSKANRI
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索
      *****************************************************************
       920-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-5113-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_syskanri"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO             TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC      TO  SYS-5113-REC
             ELSE
               MOVE    1                TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       920-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索
      *****************************************************************
       930-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-5000-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"            TO  MCP-FUNC
             MOVE    "tbl_syskanri"       TO  MCP-TABLE
             MOVE    "key2"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    ZERO               TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC        TO  SYS-5000-REC
             ELSE
               MOVE    1                  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       930-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC            SECTION.
      *
           INITIALIZE                   SRYACT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    WRK-YM               TO  SRY-SRYYM
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "st_key4"            TO  MCP-PATHNAME
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-SRYACT-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-FETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "st_key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  SRYACT-REC
             MOVE    ZERO               TO  FLG-SRYACT
           ELSE
             MOVE    1                  TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-CLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "st_key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報検索
      *****************************************************************
       900-SRYACCT-READ-SEC             SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    SRY-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID             TO  ACCT-PTID
           MOVE    SRY-SRYKA            TO  ACCT-SRYKA
           MOVE    SRY-SRYYM            TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN           TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM           TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_sryacct"      TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  SRYACCT-REC
               MOVE    ZERO             TO  FLG-SRYACCT
             ELSE
               MOVE    1                TO  FLG-SRYACCT
             END-IF
           ELSE
             MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-SELECT-SEC          SECTION.
      *
           INITIALIZE                   NYUINACT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  NSRY-HOSPNUM
           MOVE    WRK-YM               TO  NSRY-SRYYM
           MOVE    NYUINACT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "st_key1"            TO  MCP-PATHNAME
           MOVE    "tbl_nyuinact"       TO  MCP-TABLE
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-NYUINACT-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-NYUINACT
           END-IF
           .
       900-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-FETCH-SEC           SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_nyuinact"       TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  NYUINACT-REC
             MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
             MOVE    1                  TO  FLG-NYUINACT
           END-IF
           .
       900-NYUINACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_nyuinact"       TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYUINACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計情報検索
      *****************************************************************
       900-NYUINACCT-READ-SEC           SECTION.
      *
           INITIALIZE                   NYUINACCT-REC
           MOVE    NSRY-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    NSRY-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    NSRY-PTID            TO  NACCT-PTID
           MOVE    NSRY-SRYKA           TO  NACCT-SRYKA
           MOVE    NSRY-SRYYM           TO  NACCT-SRYYM
           MOVE    NSRY-SRYKBN          TO  NACCT-SRYKBN
           MOVE    NSRY-ZAINUM          TO  NACCT-ZAINUM
           MOVE    NYUINACCT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  NYUINACCT-REC
               MOVE    ZERO             TO  FLG-NYUINACCT
             ELSE
               MOVE    1                TO  FLG-NYUINACCT
             END-IF
           ELSE
             MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-READ-SEC           SECTION.
      *
           INITIALIZE                   NYUINACCT2-REC
           MOVE    NACCT-HOSPNUM        TO  NACCT2-HOSPNUM
           MOVE    NACCT-PTID           TO  NACCT2-PTID
           MOVE    NACCT-SRYYM          TO  NACCT2-SRYYM
           MOVE    "2"                  TO  NACCT2-ZAISKBKBN
           MOVE    NYUINACCT2-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  920-NYUINACCT-FETCH-SEC
             EVALUATE  NACCT2-DAY(IDX-DAY)
               WHEN  1
               WHEN  2
               WHEN  3
      *          食事療養費または入院料加算
                 IF   NACCT-ZAISKBKBN  =  "4"  OR  "6"
                   MOVE    0            TO  NACCT-DAY(IDX-DAY)
                 END-IF
      *          入院基本料かつ加算手技料
                 IF  ( NACCT-ZAISKBKBN  =  "1"  AND
                       TNS-DATAKBN      =  "2"      )
                   MOVE    0            TO  NACCT-DAY(IDX-DAY)
                 END-IF
             END-EVALUATE
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  NYUINACCT2-REC
           ELSE
             INITIALIZE                 NYUINACCT2-REC
           END-IF
           .
       920-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報検索(診療行為)
      *****************************************************************
       900-HKNCOMBI-READ-SEC            SECTION.
      *
           INITIALIZE                   HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ACCT-PTID            TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI        TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_hkncombi"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  HKNCOMBI-REC
               MOVE    ZERO             TO  FLG-HKNCOMBI
             ELSE
               MOVE    1                TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ情報検索(入院診療行為)
      *****************************************************************
       910-HKNCOMBI-READ-SEC            SECTION.
      *
           INITIALIZE                   HKNCOMBI-REC
           MOVE    NACCT-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    NACCT-PTID           TO  COMB-PTID
           MOVE    NACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_hkncombi"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  HKNCOMBI-REC
               MOVE    ZERO             TO  FLG-HKNCOMBI
             ELSE
               MOVE    1                TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       900-JYURRK-SELECT-SEC            SECTION.
      *
           INITIALIZE                   JYURRK-REC
           MOVE    ACCT-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID            TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA           TO  JYURRK-SRYKA
           MOVE    ACCT-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
           MOVE    ACCT-ZAINUM          TO  JYURRK-ZAINUM1
                                            JYURRK-ZAINUM2
                                            JYURRK-ZAINUM3
                                            JYURRK-ZAINUM4
                                            JYURRK-ZAINUM5
                                            JYURRK-ZAINUM6
                                            JYURRK-ZAINUM7
                                            JYURRK-ZAINUM8
                                            JYURRK-ZAINUM9
                                            JYURRK-ZAINUM10
                                            JYURRK-ZAINUM11
                                            JYURRK-ZAINUM12
                                            JYURRK-ZAINUM13
                                            JYURRK-ZAINUM14
                                            JYURRK-ZAINUM15
           MOVE    JYURRK-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key20"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報検索
      *****************************************************************
       900-JYURRK-FETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key20"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  JYURRK-REC
             MOVE    ZERO               TO  FLG-JYURRK
           ELSE
             MOVE    1                  TO  FLG-JYURRK
           END-IF
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報検索
      *****************************************************************
       900-JYURRK-CLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key20"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴SELECT
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           INITIALIZE                   PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID             TO  PTNYUINRRK-PTID
           MOVE    WRK-OBJYMD           TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-OBJYMD           TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key42"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
             MOVE    "key42"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  PTNYUINRRK-REC
               MOVE    ZERO             TO  FLG-PTNYUINRRK
             ELSE
               MOVE    1                TO  FLG-PTNYUINRRK
             END-IF
           ELSE
             MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key42"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数情報検索
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           INITIALIZE                   TNS-TENSU-REC
           MOVE    SPA-HOSPNUM          TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD            TO  TNS-SRYCD
           MOVE    WRK-OBJYMD           TO  TNS-YUKOSTYMD
           MOVE    WRK-OBJYMD           TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_tensu"        TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  TNS-TENSU-REC
               MOVE    ZERO             TO  FLG-TENSU
             ELSE
               MOVE    1                TO  FLG-TENSU
             END-IF
           ELSE
             MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報検索
      *****************************************************************
       910-TENSU-READ-SEC               SECTION.
      *
           INITIALIZE                   TNS-TENSU-REC
           MOVE    SPA-HOSPNUM          TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD            TO  TNS-SRYCD
           MOVE    LNK-PRTKANRI-SKYYMD  TO  TNS-YUKOSTYMD
           MOVE    LNK-PRTKANRI-SKYYMD  TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_tensu"        TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  TNS-TENSU-REC
               MOVE    ZERO             TO  FLG-TENSU
             ELSE
               MOVE    1                TO  FLG-TENSU
             END-IF
           ELSE
             MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                   SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
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
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC             SECTION.
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
