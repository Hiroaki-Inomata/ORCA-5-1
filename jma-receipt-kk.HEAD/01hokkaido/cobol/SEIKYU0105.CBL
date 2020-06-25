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
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU0105.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 北海道・ひとり親家庭等医療費請求書
      *                      (144,244,344,544,644,744,844)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/19    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            月途中の公費切替え対応
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01     有村     05/05/06    医療機関情報取得条件修正
      *  01.00.02     山口     05/09/13    初診データの集計方法修正
      *  01.00.03     内田     07/02/14    印字対象番号の追加（644）
      *  01.00.04     内田     07/04/25    帳票様式に伴う修正
      *  01.00.05     内田     07/05/23    認定証を提示時のみ71限度額を印字
      *                                    長期上位対応
      *                                    印字対象番号の追加（744,844）
      *                                    旭川の備考欄修正（初再診の判定）
      *  01.00.06     内田     07/06/22    道内広域連合の場合、市町村名のみ印字
      *  01.00.07     内田     07/09/24    保険番号744で入院の場合、
      *                                    患者負担があるなら初に○付加
      *  01.00.08     内田     07/09/26    日レセver4.0対応
      *                                    システム管理マスタ「1901」の
      *                                    医療機関情報を参照
      *  01.00.09     内田     07/11/13    保険番号644,744,844にて公費の助成が
      *                                    ないなら集計しない
      *                                    保険番号744で患者負担がある場合、
      *                                    初に○付加
      *  01.00.10     内田     08/07/01    札幌市代表保険者番号変更
      *  01.00.11     吉川     08/09/11    SPA-AREA初期化
      *  01.00.12     内田     09/05/22    上限達した時の旭川市の備考欄印字修正
      *  01.00.13     吉川     09/07/08    公費の助成がなくても印字するのは
      *                                    函館市等８市のみにする
      *  01.00.14     吉川     09/09/29    全市町村で長期及び全国公費併用時は
      *                                    公費の助成がなくても印字する
      *  01.00.15     吉川     10/11/16    室蘭市、釧路市、江別市、根室市は
      *                                    公費の助成が無い場合は印字対象外
      *  01.00.16     吉川     11/02/04    小樽市、登別市、北広島市は
      *                                    公費の助成が無い場合は印字対象外
      *  01.00.17     吉川     11/02/16    公費請求テーブルclose処理位置変更
      *  01.00.18     吉川     12/04/09    帳票様式の変更に伴う修正
      *                                    外来も高額限度額を印字する
      *  01.00.19     吉川     12/07/12    特定疾患と957等を持っている場合
      *                                    限度額欄に値が記載されるのを修正
      *  01.00.20     吉川     12/12/13    644で初診の場合、一部負担金欄は
      *                                    初診時一部負担金580円との差額を記載
      *                                    644で上限額に達すると、備考欄の
      *                                    印字がされなかったのを修正
      *                                    644で患者負担額が580円未満の場合、
      *                                    備考欄の数字は患者負担額を記載
      *                                    医療機関情報編集修正
      *  01.00.21     吉川     13/03/18    旭川市助成分が580円未満の場合の
      *                                    備考欄の金額を修正
      *  01.00.22     吉川     14/03/14    消費税改正対応
      *  01.00.23     吉川     14/10/02    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.24     門間     16/05/10    医療機関編集情報への開設者名追加対応
      *  01.00.25     吉川     16/06/01    対象公費はPGオプションから取得する
      *  01.00.26     吉川     16/08/15    一部負担金が四捨五入で580円に
      *                                    達する場合は580円と記載する
      *  01.00.27     吉川     16/11/16    一部負担金が四捨五入で580円に
      *                                    達する場合の一部負担金欄の修正
      *  01.00.28     吉川     17/02/10    他保険で初診を算定している場合
      *                                    一部負担金の記載はしない
      *                                    医療機関コード変更時、
      *                                    医療機関コード別の作成処理対応
      *  01.00.29     吉川     18/06/11    SITEKBN変更（動作に変更なし）
      *  01.00.30     萩野     18/09/11    修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI0105.INC".
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
           COPY  "SC0105.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-SYOSIN     PIC 9(01).
           03  FLG-JYURRK     PIC 9(01).
           03  FLG-SRYACCT    PIC 9(01).
           03  FLG-SYOKEI     PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-HKNNUM     PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-SRYACT     PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDX-STG.
               05  IDX-ST     PIC 9(02).
           03  IDX-EDG.
               05  IDX-ED     PIC 9(02).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
           03  WRK-PARA-STSRYYM      PIC 9(06).
           03  WRK-PARA-EDSRYYM      PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY           PIC 9(04).
               05  WRK-NMM           PIC 9(02).
               05  WRK-NDD           PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY           PIC 9(04).
               05  WRK-TMM           PIC 9(02).
               05  WRK-TDD           PIC 9(02).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOBETUCITYNUM     PIC X(03).
           03  WRK-SRYACCT-PATH      PIC X(64).
           03  WRK-PRTID             PIC X(20).
           03  WRK-TAIHI-TBL.
               05  WRK-T-FTNJANUM    PIC X(20).
               05  WRK-T-TOTALTEN    PIC 9(07).
               05  WRK-T-KOGAKU      PIC 9(07).
               05  WRK-T-TYOKI       PIC 9(07).
               05  WRK-T-KESE        PIC 9(07).
               05  WRK-T-ITBFTN      PIC 9(07).
               05  WRK-T-KASAN-FLG   PIC 9(01).
           03  WRK-SYUKEI-TBL.
               05  WRK-SEIKYUGK      PIC 9(07)V9(01).
               05  WRK-SEIKYUGK-G    PIC 9(07).
               05  WRK-SEIKYUGK-G2   PIC 9(07)V9(01).
               05  WRK-KOHTENSU      PIC 9(07).
               05  WRK-KESE          PIC 9(07)V9(01).
               05  WRK-ETC-ITBFTN    PIC 9(07)V9(01).
               05  WRK-ETC-ITBFTN-G  PIC 9(07).
               05  WRK-ETC-SYOSINFTN PIC 9(07)V9(01).
               05  WRK-ASAHIFTN      PIC 9(07).
           03  WRK-COMPFTN           PIC 9(07).
           03  WRK-N-JOUGENGK        PIC 9(07).
           03  WRK-G-JOUGENGK        PIC 9(07).
           03  WRK-SHOSAI            PIC 9(07).
           03  WRK-PATH              PIC X(50).
      *    医療機関情報
           03  WRK-HOSPNAME          PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1     PIC X(50).
               05  WRK-HOSPNAME2     PIC X(50).
           03  WRK-HOSPADRS          PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1     PIC X(50).
               05  WRK-HOSPADRS2     PIC X(50).
           03  WRK-HOSPTEL           PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
           03  WRK-HENKAN            PIC X(20).
      *
      *    プログラムオプション
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
      *
       01  SUM-AREA.
           03  SUM-KENSU          PIC 9(04) OCCURS 5.
           03  SUM-TENSU7         PIC 9(07).
           03  SUM-TENSU8         PIC 9(07).
           03  SUM-TENSU9         PIC 9(07).
           03  SUM-TYOKI          PIC 9(07).
           03  SUM-KESE           PIC 9(07).
           03  SUM-ITBFTN         PIC 9(07).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
      *    キー用エリア
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-FTNJANUM   PIC X(08).
           03  KEY-OLD.
               05  KEY-O-FTNJANUM   PIC X(08).
      *
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-201403         PIC X(06) VALUE "201403".
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
               USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  SUM-AREA
           INITIALIZE  KEY-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
      *
           DISPLAY "START"
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
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU0105"    TO  JOB-PGID
           MOVE    "ひとり親家庭等医療費請求書" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
      *****
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU0105 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
      *****
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU0105 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    ３４４の上限額取得
           PERFORM 940-HKNNUM-SET-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU0105"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  WRK-TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  WRK-TAISYO-KOHNUM(IDX)
           END-PERFORM
      *    
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1    >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF 
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
      *
           IF       CNT-RECE02   >       0
                    PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理１
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
             INITIALIZE                            WRK-CHK-KOHNUM-TBL
             MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
               PERFORM  20011-SYUKEI-SEC
             END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-KYURATE      TO  MF100-FTNRATE
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE  MF100-KOHFTNJANUM         TO  MF100-FTNJANUM
           MOVE  MF100-KOHJKYSNUM          TO  MF100-JKYSNUM
           COMPUTE IDY     =         IDZ   +   1
           ADD   RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           ADD   RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
      *
           DISPLAY "患者番号=" MF100-PTNUM
                   "変換前=" MF100-FTNJANUM " " MF100-KOHFTNJANUM
           MOVE     SPACE    TO    WRK-HENKAN
           MOVE     ZERO     TO    IDX2
           PERFORM  VARYING  IDX1  FROM  1   BY  2
                    UNTIL    IDX1  >     20
                    OR       MF100-FTNJANUM(IDX1:2) = SPACE
             EVALUATE  MF100-FTNJANUM(IDX1:2)
             WHEN "１"
               ADD      1    TO    IDX2
               MOVE    "1"   TO    WRK-HENKAN(IDX2:1)
             WHEN "２"
               ADD      1    TO    IDX2
               MOVE    "2"   TO    WRK-HENKAN(IDX2:1)
             WHEN "３"
               ADD      1    TO    IDX2
               MOVE    "3"   TO    WRK-HENKAN(IDX2:1)
             WHEN "４"
               ADD      1    TO    IDX2
               MOVE    "4"   TO    WRK-HENKAN(IDX2:1)
             WHEN "５"
               ADD      1    TO    IDX2
               MOVE    "5"   TO    WRK-HENKAN(IDX2:1)
             WHEN "６"
               ADD      1    TO    IDX2
               MOVE    "6"   TO    WRK-HENKAN(IDX2:1)
             WHEN "７"
               ADD      1    TO    IDX2
               MOVE    "7"   TO    WRK-HENKAN(IDX2:1)
             WHEN "８"
               ADD      1    TO    IDX2
               MOVE    "8"   TO    WRK-HENKAN(IDX2:1)
             WHEN "９"
               ADD      1    TO    IDX2
               MOVE    "9"   TO    WRK-HENKAN(IDX2:1)
             WHEN "０"
               ADD      1    TO    IDX2
               MOVE    "0"   TO    WRK-HENKAN(IDX2:1)
             END-EVALUATE
           END-PERFORM
           IF  WRK-HENKAN  NOT  =  SPACE
             MOVE    WRK-HENKAN    TO    MF100-FTNJANUM
                                         MF100-KOHFTNJANUM 
           END-IF
           DISPLAY "変換後=" MF100-FTNJANUM " " MF100-KOHFTNJANUM
      *
      *    個別発行用市町村番号
           MOVE  MF100-KOHFTNJANUM(1:3)     TO  WRK-KOBETUCITYNUM
      *
      *    区内は札幌市で請求する
           IF  MF100-FTNJANUM(1:1)  =  "4"
             MOVE  "0017"   TO    MF100-FTNJANUM
                                  MF100-KOHFTNJANUM
           END-IF
      *
      *    一部負担金がある公費を使用している場合は助成対象判定を行う
           MOVE     ZERO                       TO  FLG-ERR
             MOVE      1                       TO  FLG-ERR
      *
      *    長期・結核・精神編集
           MOVE     ZERO                       TO  FLG-TYOKI
           MOVE    MF100-KOHID                 TO  WRK-KOHID
           MOVE    MF100-KOHNUM                TO  WRK-KOHNUM
           INITIALIZE                              WRK-SYUKEI-TBL
                                                   WRK-COMPFTN
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"                 TO  WRK-PATH
           ELSE
               MOVE    "key25"                 TO  WRK-PATH
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *    四捨五入
           COMPUTE  MF100-KESE ROUNDED      =  WRK-KESE
           COMPUTE  WRK-SEIKYUGK-G ROUNDED  =  WRK-SEIKYUGK-G2
      *
      *    ５４４・１割相当金額編集
           IF  MF100-KOHNUM   =  "544"
             COMPUTE WRK-ETC-ITBFTN-G ROUNDED = WRK-ETC-ITBFTN
             IF  MF100-NYUGAIKBN        =  "1"
               IF  WRK-ETC-ITBFTN-G   >  WRK-N-JOUGENGK
                 MOVE  WRK-N-JOUGENGK TO  WRK-ETC-ITBFTN-G
               END-IF
             ELSE
               IF  WRK-ETC-ITBFTN-G   >   WRK-G-JOUGENGK
                 MOVE  WRK-G-JOUGENGK TO  WRK-ETC-ITBFTN-G
               END-IF
             END-IF
             MOVE     WRK-ETC-ITBFTN-G TO  MF100-ETCFTNMONEY
           END-IF
      *
      *   ２４４・初診時一部負担金相当額編集
           IF     MF100-KOHNUM   =   "244"
             COMPUTE MF100-ETCFTNMONEY ROUNDED = WRK-ETC-SYOSINFTN
           END-IF
      *
      *    旭川の場合、初診再診判定
           IF  MF100-KOHNUM  =  "644"
             COMPUTE  WRK-SHOSAI  =  RECE02-TOTALTEN (IDY) / 10 * 10
             COMPUTE WRK-ETC-ITBFTN-G ROUNDED = WRK-ETC-ITBFTN
      *      患者負担額と計算負担金の値が一致しなかったら
      *      初診時一部負担金が引かれたということなので、初診と判定
             IF  WRK-COMPFTN  NOT =  WRK-ETC-ITBFTN-G
      *        市助成分が580円に満たない場合、備考欄に患者負担額を記載
               IF  (WRK-ASAHIFTN  NOT =  ZERO)
               AND (WRK-ASAHIFTN  <  580)
                 MOVE  WRK-ASAHIFTN        TO  MF100-ETCFTNMONEY
               ELSE
                 MOVE  580                 TO  MF100-ETCFTNMONEY
               END-IF
               MOVE  WRK-ETC-ITBFTN-G      TO  MF100-FTNMONEY
               MOVE  1                     TO  MF100-SHOSAISIN
             END-IF
           END-IF
      *
      *    長期編集
           IF     FLG-TYOKI             =      1   AND
                  WRK-SEIKYUGK-G        >  10000
             MOVE    10000             TO  WRK-SEIKYUGK-G
                                           MF100-TYOKI
             MOVE    ZERO              TO  MF100-TOTALTEN
             IF      MF100-FTNMONEY    >   10000
               MOVE  10000             TO  MF100-FTNMONEY
             END-IF
           END-IF
      *    長期上位
           IF     FLG-TYOKI             =      2   AND
                  WRK-SEIKYUGK-G        >  20000
             MOVE    20000             TO  WRK-SEIKYUGK-G
                                           MF100-TYOKI
             MOVE    ZERO              TO  MF100-TOTALTEN
             IF      MF100-FTNMONEY    >   20000
               MOVE  20000             TO  MF100-FTNMONEY
             END-IF
           END-IF
      *
           IF      RECE02-FTNMONEY(1)  NOT =   ZERO    AND
                   RECE02-FTNMONEY(1)      <   WRK-SEIKYUGK-G
               MOVE    RECE02-FTNMONEY(1)  TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    結核・精神編集
           IF     MF100-KESE-FLG    NOT =  SPACE
             MOVE    WRK-SEIKYUGK-G     TO  MF100-KESE
             MOVE    ZERO               TO  MF100-TOTALTEN
           END-IF
      *
      *    高額療養費チェック
           IF  RECE02-FLG(4)  =  ZERO
             IF   RECE02-ZAITAKU  NOT =  SPACE
             AND  RECE02-ZAITAKU  NOT =  "1"
               IF  RECE02-FTNMONEY (1)  NOT =  ZERO
                 MOVE  RECE02-FTNMONEY (1)     TO  MF100-KOGAKU
               ELSE
                 COMPUTE  MF100-KOGAKU  =  RECE02-TOTALTEN (1)
                                        *  RECE02-KYURATE  /  10
               END-IF
             END-IF
           END-IF
      *
      *    保険にかかる点数が580円相当分を超えており、
      *    一部負担金が四捨五入で580円となる場合は580円とする
           IF  MF100-KOHNUM  =  "144"
               EVALUATE  RECE02-KYURATE
               WHEN  30
                   IF  (MF100-FTNMONEY  >  574  AND 
                        MF100-FTNMONEY  <  580)
                   AND (RECE02-TOTALTEN(1)  >  193)
                       MOVE    580     TO  MF100-FTNMONEY
                   END-IF
               WHEN  20
                   IF  (MF100-FTNMONEY  >  574  AND 
                        MF100-FTNMONEY  <  580)
                   AND (RECE02-TOTALTEN(1)  >  290)
                       MOVE    580     TO  MF100-FTNMONEY
                   END-IF
               WHEN  10
                   IF  (MF100-FTNMONEY  >  574  AND 
                        MF100-FTNMONEY  <  580)
                   AND (RECE02-TOTALTEN(1)  >  580)
                       MOVE    580     TO  MF100-FTNMONEY
                   END-IF
               END-EVALUATE
           END-IF
      *
      *    件数加算場所を判定
           IF    MF100-TOTALTEN         =  ZERO
             IF     MF100-KESE-FLG  NOT =  SPACE
               MOVE     5              TO  MF100-KASAN-FLG
             ELSE
               MOVE     4              TO  MF100-KASAN-FLG
             END-IF
           ELSE
             IF  MF100-KOGAKU  NOT =  ZERO
               MOVE     3              TO  MF100-KASAN-FLG
             ELSE
               EVALUATE  MF100-KYURATE
               WHEN 20
                 MOVE     2            TO  MF100-KASAN-FLG
               WHEN OTHER
                 MOVE     1            TO  MF100-KASAN-FLG
               END-EVALUATE
             END-IF
           END-IF
      *    一般患者用区分設定
           MOVE         2              TO  MF100-KBN
      *
      *    合計ファイル用にデータ退避
           INITIALIZE   WRK-TAIHI-TBL
           MOVE  MF100-FTNJANUM        TO  WRK-T-FTNJANUM
           MOVE  MF100-TOTALTEN        TO  WRK-T-TOTALTEN
           MOVE  MF100-KOGAKU          TO  WRK-T-KOGAKU
           MOVE  MF100-KESE            TO  WRK-T-KESE
           MOVE  MF100-TYOKI           TO  WRK-T-TYOKI
           MOVE  MF100-FTNMONEY        TO  WRK-T-ITBFTN
           MOVE  MF100-KASAN-FLG       TO  WRK-T-KASAN-FLG
      *
           IF       FLG-ERR                    =   ZERO
             WRITE    MF100-REC 
             ADD      1                 TO    CNT-MF100
      *
      *    合計用ファイル編集
             PERFORM  2002-GOKEI-HEN-SEC
      *
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                     "点数="                RECE02-TOTALTEN(1)
                     "一部負担金"           MF100-FTNMONEY
                     "負担割合="            MF100-KYURATE
                     "公費番号="            MF100-KOHNUM
                     "診療年月="            MF100-SRYYM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    合計ファイル編集処理
      *****************************************************************
       2002-GOKEI-HEN-SEC          SECTION.
      *
      *    合計ファイル編集
           INITIALIZE  MF100-REC
           MOVE     SPA-HOSPNUM       TO    MF100-HOSPNUM
           MOVE     WRK-T-FTNJANUM    TO    MF100-FTNJANUM
           MOVE     1                 TO    MF100-KBN
      *    一時ファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
           MOVE     WRK-T-KASAN-FLG   TO    IDX1
           ADD      1                 TO    MF100-GKENSU(IDX1)
                                            MF100-TOTAL-KENSU
           ADD      WRK-T-ITBFTN      TO    MF100-GITBFTN(IDX1)
      *    請求事務手数料
           IF  LNK-PRTKANRI-SRYYM  >=  WRK-CONS-201403
             ADD    216               TO    MF100-GTESURYO(IDX1)
           ELSE
             ADD    210               TO    MF100-GTESURYO(IDX1)
           END-IF
           ADD      WRK-T-KESE        TO    MF100-GTENSU(5)
           ADD      WRK-T-TYOKI       TO    MF100-GTENSU(4)
      *    高額療養費
           IF  WRK-T-KOGAKU  NOT =  ZERO
             ADD  WRK-T-KOGAKU        TO    MF100-GTENSU(3)
           ELSE
             EVALUATE  RECE02-KYURATE
             WHEN 20
               ADD    WRK-T-TOTALTEN  TO    MF100-GTENSU(2)
             WHEN OTHER
               ADD    WRK-T-TOTALTEN  TO    MF100-GTENSU(1)
             END-EVALUATE
           END-IF
           IF      FLG-MF100  =  1
                   WRITE    MF100-REC
           ELSE
                   REWRITE  MF100-REC
           END-IF
      *
           .
       2002-GOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２４４初診一部負担金相当額集計処理
      *****************************************************************
       2002-SYOSIN-CHK-SEC  SECTION.
      *
      *    初診チェック
      *    入院は診療会計より判定
           MOVE   ZERO                  TO  FLG-SYOSIN
           IF     SYU-NYUGAIKBN   =   "1"
             INITIALIZE        SRYACCT-REC
             MOVE     "key14"                  TO  WRK-SRYACCT-PATH
      *
             MOVE     SPA-HOSPNUM              TO  ACCT-HOSPNUM
             MOVE     SYU-NYUGAIKBN            TO  ACCT-NYUGAIKBN
             MOVE     SYU-PTID                 TO  ACCT-PTID
             MOVE     SYU-SRYYMD(1:6)          TO  ACCT-SRYYM
             MOVE     "11"                     TO  ACCT-SRYKBN
             PERFORM  900-SRYACCT-SELECT-SEC
             MOVE     SYU-SKYSTYMD(7:2) TO  IDX-STG(1:2)
             MOVE     SYU-SKYEDYMD(7:2) TO  IDX-EDG(1:2)
             DISPLAY  "請求開始日="  SYU-SKYSTYMD
             DISPLAY  "請求終了日="  SYU-SKYEDYMD
             DISPLAY  "開始IDX="  IDX-ST
             DISPLAY  "終了IDX="  IDX-ED
             PERFORM  UNTIL  FLG-SRYACCT  =   1  OR
                      FLG-SYOSIN          =   1
               PERFORM  UNTIL  IDX-ST  >  IDX-ED OR
                               IDX-ST  >  31     OR
                               FLG-SYOSIN        =   1
                 IF  ACCT-DAY(1 IDX-ST)   >   0
                   MOVE  1               TO   FLG-SYOSIN
                 END-IF
                 ADD    1                TO   IDX-ST
               END-PERFORM
               PERFORM  900-SRYACCT-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYACCT-CLOSE-SEC
      *    外来は受診履歴と診療会計より判定
           ELSE
             MOVE    ZERO    TO    FLG-JYURRK
             PERFORM  950-JYUSIN-SELECT-SEC
             PERFORM  UNTIL  FLG-JYURRK   =   1  OR
                      FLG-SYOSIN          =   1
               PERFORM  VARYING  IDX  FROM  1  BY  1
                        UNTIL    IDX  >    15  OR
                        JYURRK-ZAINUM(IDX)  =  ZERO OR SPACE
                 INITIALIZE        SRYACCT-REC
                 MOVE     "key5"                   TO  WRK-SRYACCT-PATH
                 MOVE     SPA-HOSPNUM              TO  ACCT-HOSPNUM
                 MOVE     JYURRK-NYUGAIKBN         TO  ACCT-NYUGAIKBN
                 MOVE     JYURRK-PTID              TO  ACCT-PTID
                 MOVE     JYURRK-SRYKA             TO  ACCT-SRYKA
                 MOVE     JYURRK-SRYYMD(1:6)       TO  ACCT-SRYYM
                 MOVE     JYURRK-ZAINUM(IDX)       TO  ACCT-ZAINUM
                 PERFORM  900-SRYACCT-SELECT-SEC
                 PERFORM  900-SRYACCT-CLOSE-SEC
      *    小児科外来診療料 初診時も初診として数える
                 IF    ACCT-SRYKBN  =  "11"
                 AND   ACCT-ZAITEN  NOT =  ZERO
                   MOVE    1    TO    FLG-SYOSIN
                   MOVE    15   TO    IDX
                 ELSE
      *
                   DISPLAY  "小児科外来診療料"
                   DISPLAY  "患者番号 = " RECE02-PTNUM
                            "ZAINUM = " ACCT-ZAINUM
      *
                   INITIALIZE  SRYACT-REC
                   MOVE  SPA-HOSPNUM              TO  SRY-HOSPNUM
                   MOVE  ACCT-NYUGAIKBN           TO  SRY-NYUGAIKBN
                   MOVE  ACCT-PTID                TO  SRY-PTID
                   MOVE  ACCT-SRYKA               TO  SRY-SRYKA
                   MOVE  ACCT-SRYYM               TO  SRY-SRYYM
                   MOVE  ACCT-ZAINUM              TO  SRY-ZAINUM
                   MOVE  1                        TO  SRY-RENNUM
                   PERFORM  920-SRYACT-SET-SEC
      *
                   IF  SRY-SRYCD(1)  =  "113003510" OR "113003710"
                   OR  SRY-SRYCD(2)  =  "113003510" OR "113003710"
                   OR  SRY-SRYCD(3)  =  "113003510" OR "113003710"
                   OR  SRY-SRYCD(4)  =  "113003510" OR "113003710"
                   OR  SRY-SRYCD(5)  =  "113003510" OR "113003710"
      *
                     DISPLAY  "SRY-SRYCD(1) = " SRY-SRYCD(1)
      *
                     MOVE  1                      TO  FLG-SYOSIN
                     MOVE  15                     TO  IDX
                   END-IF
                 END-IF
      ****
               END-PERFORM
               PERFORM  950-JYUSIN-FETCH-SEC
             END-PERFORM
             PERFORM  950-JYUSIN-CLOSE-SEC
           END-IF
      *
      *    初診のデータの時のみ、初診時一部負担金相当額を算出
           IF    FLG-SYOSIN   =   1
             MOVE    ZERO    TO    WRK-SEIKYUGK
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >    4
               IF    SYU-KOH-HKNNUM(IDX) = "244"
                 IF    IDX  =  1
                   IF  MF100-RJNHKNNUM  =  SPACE 
                     MOVE    SYU-SYUCOMPFTN-ENTANI TO    WRK-SEIKYUGK
                   ELSE
                     MOVE    SYU-KOH-COMPFTN-ENTANI(1)
                                                   TO    WRK-SEIKYUGK
                   END-IF
                 ELSE
                   MOVE  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                                   TO  WRK-SEIKYUGK
                 END-IF
               END-IF
             END-PERFORM
      *
             DISPLAY  "WRK-ETC-SYOSINFTN MAE = " WRK-ETC-SYOSINFTN
             DISPLAY  "WRK-SEIKYUGK = " WRK-SEIKYUGK
      *
             IF    WRK-SEIKYUGK    >    580
               ADD    580               TO    WRK-ETC-SYOSINFTN
             ELSE
               ADD    WRK-SEIKYUGK      TO    WRK-ETC-SYOSINFTN
             END-IF
           END-IF
      *
           DISPLAY  "WRK-ETC-SYOSINFTN ATO = " WRK-ETC-SYOSINFTN
      *
           .
       2002-SYOSIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "01"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)  OR
                         ( WRK-HKNJANUM(1:3)  =  "014"   AND
                           TBL-PARA-HKNJANUM(IDXX)(1:5)  =  "01400" )
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM 
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
      *
           ADD     1    TO    CNT-PAGE
      *
           INITIALIZE                             SC05
      *
      *    対象年月
           MOVE    WRK-SRYYMWH               TO   SC05-PRTYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH              TO   SC05-SEIYMD
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD          TO   SC05-HOSPCD
      *    医療機関住所
           MOVE    WRK-HOSPADRS              TO  SC05-ADRS
           INSPECT SC05-ADRS                 REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME              TO  SC05-HOSPNAME
           INSPECT SC05-HOSPNAME             REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME           TO   SC05-KAISETU
      *    電話番号
           MOVE     WRK-HOSPTEL              TO   SC05-TEL
      *
      *    市町村名
           INITIALIZE                             WRK-HKNJANUM
           MOVE  "01"                        TO   WRK-HKNJANUM(1:2)
           MOVE  MF100-FTNJANUM(1:3)         TO   WRK-HKNJANUM(3:3)
           PERFORM  920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME           =    SPACE
             MOVE    "長  様"                TO   SC05-CITYNAME(13:6)
           ELSE
             IF  HKNJA-HKNJANAME(1:16)       =    "空知中部広域連合"
                 OR  HKNJA-HKNJANAME(1:16)   =    "大雪地区広域連合"
               STRING  HKNJA-HKNJANAME(19:)         DELIMITED  BY "〕"
                       "長  様"          DELIMITED  BY SIZE
                       INTO                    SC05-CITYNAME
               END-STRING
             ELSE
               STRING  HKNJA-HKNJANAME         DELIMITED  BY SPACE
                       "長  様"          DELIMITED  BY SIZE
                       INTO                    SC05-CITYNAME
               END-STRING
             END-IF
           END-IF
      *
      *    合計用ファイル時の編集
           IF  MF100-KBN                     =    1
             MOVE     "SC0105.red"           TO   WRK-PRTID
             IF  MF100-TOTAL-KENSU           >    10
               MOVE   1                      TO   FLG-SYOKEI
             ELSE
               MOVE   ZERO                   TO   FLG-SYOKEI
             END-IF
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     5
               MOVE   MF100-GKENSU(IDX1)     TO   WRK-Z5
               MOVE   WRK-Z5                 TO   SC05-GKENSU(IDX1)
               MOVE   MF100-GTENSU(IDX1)     TO   WRK-Z9
               MOVE   WRK-Z9                 TO   SC05-GTENSU(IDX1)
               MOVE   MF100-GITBFTN(IDX1)    TO   WRK-Z7
               MOVE   WRK-Z7                 TO   SC05-GITBFTN(IDX1)
               MOVE   MF100-GTESURYO(IDX1)   TO   WRK-Z6
               MOVE   WRK-Z6                 TO   SC05-GTESURYO(IDX1)
             END-PERFORM
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           ELSE
             MOVE     "SC0106.red"           TO   WRK-PRTID
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
           INITIALIZE          SUM-AREA
           MOVE    KEY-NEW     TO   KEY-OLD 
      *
           PERFORM  VARYING    IDX1  FROM  1  BY  1
                    UNTIL      IDX1  >    10
                    OR         KEY-NEW  NOT    =   KEY-OLD
                    OR         FLG-PRINT       =   1
      * 
      *    診療年月
             MOVE    MF100-SRYYM         TO  WRK-SYMD (1:6)
             MOVE    "01"                TO  WRK-SYMD (7:2)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMDG(5:4)    TO  SC05-SRYYM(IDX1)(1:4)
             MOVE    WRK-HENYMDG(11:4)   TO  SC05-SRYYM(IDX1)(5:4)
      *    受給者番号
             MOVE    MF100-JKYSNUM       TO  WRK-MAE-INPUT
             DISPLAY "受給者番号変換前="     WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             DISPLAY "受給者番号変換後="     WRK-OUT-INPUT
      *
             MOVE     ZERO     TO    IDX3
             PERFORM  VARYING  IDX2  FROM   1  BY  2
                      UNTIL    IDX2  >      14 OR
                      WRK-OUT-INPUT(IDX2:2) =  SPACE
               ADD    2        TO            IDX3
             END-PERFORM
             IF    IDX3    >    ZERO
               COMPUTE  IDX2  =  15 - IDX3
               MOVE  WRK-OUT-INPUT(1:IDX3)
                               TO  SC05-JKYSNUM(IDX1)(IDX2:IDX3)
             END-IF
      *
      *    受給者番号の空白を０に置き換える
           INSPECT SC05-JKYSNUM(IDX1)  REPLACING  ALL "  "  BY  "０"
      *    氏名
             MOVE    MF100-NAME          TO  SC05-NAME(IDX1)
      *    入外区分
             IF      MF100-NYUGAIKBN     =   "1"
               MOVE  "○"                TO  SC05-NYUGAIKBN(IDX1 1)
             ELSE
               MOVE  "○"                TO  SC05-NYUGAIKBN(IDX1 2)
             END-IF
      *
      *    点数・請求金額・一部負担金
      *      高額療養費
             IF  MF100-KOGAKU  NOT =  ZERO
               MOVE  MF100-KOGAKU      TO  WRK-Z7
               MOVE  WRK-Z7            TO  SC05-TENSU9(IDX1)
             ELSE
               MOVE    MF100-TOTALTEN  TO  WRK-Z7
               EVALUATE  MF100-KYURATE
               WHEN 20
                 MOVE    WRK-Z7        TO  SC05-TENSU8(IDX1)
               WHEN OTHER
                 MOVE    WRK-Z7        TO  SC05-TENSU7(IDX1)
               END-EVALUATE
             END-IF
      *
             MOVE    MF100-TYOKI       TO  WRK-Z6
             MOVE    WRK-Z6            TO  SC05-TYOKI(IDX1)
      * 
             MOVE    MF100-KESE        TO  WRK-Z7
             MOVE    WRK-Z7            TO  SC05-KESE(IDX1)
             EVALUATE  MF100-KESE-FLG
             WHEN  "010"
             WHEN  "011"
               MOVE  "○"              TO  SC05-KESEMARU(IDX1 1)
             WHEN  "020"
             WHEN  "021"
               MOVE  "○"              TO  SC05-KESEMARU(IDX1 2)
             END-EVALUATE
      *
             IF    (MF100-KOHNUM   =  "244"  AND
                    MF100-ETCFTNMONEY   >  0) OR
                   (MF100-KOHNUM   =  "544")
               MOVE  MF100-ETCFTNMONEY TO  WRK-Z6
               MOVE  "○"              TO  SC05-MARU(IDX1)
             ELSE
               MOVE  MF100-FTNMONEY    TO  WRK-Z6
             END-IF
             MOVE  WRK-Z6              TO  SC05-ITBFTN(IDX1)
             IF     MF100-KOHNUM           =  "144"  OR
                    MF100-KOHNUM           =  "244"
             OR     (MF100-KOHNUM = "744")
      *
      * 一部負担金欄に金額を記載する時のみ◯をする
               IF  MF100-FTNMONEY      >   ZERO  OR
                   MF100-ETCFTNMONEY   >   ZERO
                 MOVE    "◯"          TO  SC05-FTNMARU(IDX1 1)
               END-IF
             ELSE
               MOVE  "○"              TO  SC05-FTNMARU(IDX1 2)
             END-IF
      *    旭川市の場合は備考欄印字
             IF  MF100-SHOSAISIN  =  1
                 MOVE  MF100-ETCFTNMONEY  TO  WRK-Z3
                 MOVE  WRK-Z3          TO  SC05-BIKO1(IDX1)
                 MOVE  "○"            TO  SC05-BIKO2(IDX1)
             END-IF
      *    小計加算
             MOVE    MF100-KASAN-FLG   TO  IDX2
             ADD     1                 TO  SUM-KENSU(IDX2)
             IF  MF100-KOGAKU  NOT =  ZERO
               ADD  MF100-KOGAKU        TO  SUM-TENSU9
             ELSE
               EVALUATE  MF100-KYURATE
               WHEN 20
                 ADD     MF100-TOTALTEN  TO  SUM-TENSU8
               WHEN OTHER
                 ADD     MF100-TOTALTEN  TO  SUM-TENSU7
               END-EVALUATE
             END-IF
             ADD    MF100-TYOKI        TO  SUM-TYOKI
             ADD    MF100-KESE         TO  SUM-KESE
             ADD    MF100-FTNMONEY     TO  SUM-ITBFTN
      *    一時ファイル読込
             PERFORM   900-TMP-READ-SEC
           END-PERFORM
      *
      *    １１件以上明細が有る場合のみ小計編集
           IF     FLG-SYOKEI     =     1
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     5
               MOVE  SUM-KENSU(IDX1)   TO  WRK-Z5
               MOVE  WRK-Z5            TO  SC05-SKENSU(IDX1)
             END-PERFORM
             MOVE  SUM-TENSU7          TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC05-STENSU7
             MOVE  SUM-TENSU8          TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC05-STENSU8
             MOVE  SUM-TENSU9          TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC05-STENSU9
             MOVE  SUM-TYOKI           TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC05-STYOKI
             MOVE  SUM-KESE            TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC05-SKESE
             MOVE  SUM-ITBFTN          TO  WRK-Z7
             MOVE  WRK-Z7              TO  SC05-SITBFTN
           END-IF
      *
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
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
           MOVE   WRK-PRTID            TO  SPRT-PRTID
           MOVE    "ひとり親家庭等医療費請求書"
                                    TO  SPRT-TITLE
           MOVE    SC05             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
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
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR  TO  RECEERR-REC
             WRITE RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU0105 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU0105 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU0105 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
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
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       WRK-TAISYO-KOHNUM(IDXX)  =  SPACE
               IF  WRK-CHK-KOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
                   MOVE     1                 TO  FLG-TAISYO
                   MOVE     100               TO  IDXX
               END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT  AT  END
                   MOVE  MF100-FTNJANUM       TO  KEY-N-FTNJANUM
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(1)        TO  WRK-CHK-KOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  WRK-CHK-KOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  WRK-CHK-KOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  WRK-CHK-KOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
      *        個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO        TO  FLG-ERR
                         END-IF
                     END-PERFORM 
                   END-IF
                   DISPLAY "患者番号=" RECE02-PTNUM
                           "負担者番号=" RECE02-KOHFTNJANUM(1)
                           "保険者番号=" RECE02-HKNJANUM
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタクローズ
      *****************************************************************
       900-KOHSKY-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           IF  RECE02-FTNMONEY(1)  =   ZERO
               IF  RECE02-RJNHKNNUM    NOT =   SPACE
                   MOVE    SYU-KOH-COMPFTN-ENTANI(1)
                                               TO  WRK-SEIKYUGK
               ELSE
                   MOVE    SYU-SYUCOMPFTN-ENTANI
                                               TO  WRK-SEIKYUGK
               END-IF
           ELSE
               IF  RECE02-RJNHKNNUM    NOT =   SPACE
                   MOVE    SYU-KOH-COMPFTN(1)  TO  WRK-SEIKYUGK
               ELSE
                   MOVE    SYU-SYUCOMPFTN      TO  WRK-SEIKYUGK
               END-IF
           END-IF
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     4  OR
                    SYU-KOH-HKNNUM(IDX) =  SPACE
      *
      *    公費併用判定
             EVALUATE  SYU-KOH-HKNNUM(IDX)
             WHEN "010"
             WHEN "011"
             WHEN "020"
             WHEN "021"
               IF      RECE02-FTNMONEY(1)    =   ZERO
                   MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)  TO  WRK-SEIKYUGK
                   ADD     SYU-KOH-COMPFTN-ENTANI(IDX)  TO  WRK-KESE
               ELSE
                   MOVE    SYU-KOH-COMPFTN(IDX)  TO  WRK-SEIKYUGK
                   ADD     SYU-KOH-COMPFTN(IDX)  TO  WRK-KESE
               END-IF
               MOVE    ZERO                  TO  FLG-ERR
      *
               ADD     SYU-TOTAL-HKNTEN      TO  WRK-KOHTENSU
               MOVE    SYU-KOH-HKNNUM(IDX)   TO  MF100-KESE-FLG
             WHEN "972"
               MOVE    1                     TO  FLG-TYOKI
               MOVE    ZERO                  TO  FLG-ERR
             WHEN "974"
               MOVE    2                     TO  FLG-TYOKI
               MOVE    ZERO                  TO  FLG-ERR
             END-EVALUATE
      *
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
             IF     FLG-ERR                 =  1
               IF   SYU-KOH-HKNNUM(IDX)(1:1) NOT = "0" AND "9"
                 IF     IDX                 =  1
                   IF  SYU-SYUCOMPFTN-ENTANI
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR
                   END-IF
                 END-IF
               END-IF
             END-IF
      *
      *    ５４４を使用している場合、点数の１割相当額を集計する
             IF  SYU-KOH-HKNNUM(IDX)  =  "544"
               PERFORM  8001-SEIKYU-SEC
             END-IF
      *
      *    旭川親の場合、初診時一部負担金との差額を記載
      *    （本体側でSYU-KOH-COMPFTN-ENTANIに計算済み）
             IF  SYU-KOH-HKNNUM(IDX)  =  "644"
               IF  SYU-KOH-COMPFTN-ENTANI(IDX)  =  ZERO
      *          市助成分を集計
                 ADD  SYU-KOH-COMPFTN(IDX)         TO  WRK-ASAHIFTN
               END-IF
      *        初診判定用にCOMPFTN集計
               ADD    SYU-KOH-COMPFTN(IDX)         TO  WRK-COMPFTN
               ADD    SYU-KOH-COMPFTN-ENTANI(IDX)  TO  WRK-ETC-ITBFTN
             END-IF
      *
      *    長期及び全国公費を併用している場合は公費の助成がなくても記載
             IF  SYU-KOH-HKNNUM(IDX)(1:1)  =  "0"
             OR  SYU-KOH-HKNNUM(IDX)       =  "972"
             OR  SYU-KOH-HKNNUM(IDX)       =  "974"
               MOVE  ZERO                 TO  FLG-ERR
             END-IF
      *
           END-PERFORM
      *
           ADD     WRK-SEIKYUGK            TO  WRK-SEIKYUGK-G2
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    １割相当額計算
      *****************************************************************
       8001-SEIKYU-SEC  SECTION.
      *
      *
           IF  IDX     =     1
             IF  SYU-SYUCOMPFTN-ENTANI  <  SYU-TOTAL-HKNTEN
               ADD  SYU-SYUCOMPFTN-ENTANI  TO WRK-ETC-ITBFTN
             ELSE
               ADD  SYU-TOTAL-HKNTEN       TO WRK-ETC-ITBFTN
             END-IF
           ELSE
             IF SYU-KOH-COMPFTN-ENTANI(IDX - 1) < SYU-TOTAL-HKNTEN
               ADD  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                           TO WRK-ETC-ITBFTN
             ELSE
               ADD  SYU-TOTAL-HKNTEN       TO WRK-ETC-ITBFTN
             END-IF
           END-IF
      *
           .
       8001-SEIKYU-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10)
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF     COMB-KOHID(1)    =  WRK-KOHID   OR
                            COMB-KOHID(2)    =  WRK-KOHID   OR
                            COMB-KOHID(3)    =  WRK-KOHID   OR
                            COMB-KOHID(4)    =  WRK-KOHID
      *
                       PERFORM  800-SEIKYU-SEC
      *
      *              244のみ使用している場合、一部負担金相当額を集計する
                       IF     MF100-KOHNUM   =  "244"
                         PERFORM  2002-SYOSIN-CHK-SEC
                       END-IF
      *
                     END-IF
      *
                   END-IF
                 END-IF
      *
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  WRK-PATH             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報   2005/09/13 yamaguchi INS
      *****************************************************************
       920-SRYACT-SET-SEC         SECTION.
      *
           MOVE    SRYACT-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC               =   ZERO
                   MOVE    "DBFETCH"               TO  MCP-FUNC
                   MOVE    "tbl_sryact"       TO  MCP-TABLE
                   MOVE    "key"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"        USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
                   IF      MCP-RC              =   ZERO
                           MOVE    MCPDATA-REC     TO  SRYACT-REC
                           MOVE    ZERO            TO  FLG-SRYACT
                   ELSE
                           MOVE   1               TO  FLG-SRYACT
                           INITIALIZE             SRYACT-REC
                   END-IF
           ELSE
                   MOVE   1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       920-SRYACT-SET-EXT.
           EXIT.
      *****************************************************************
      *   保険番号情報セット(上限額取得)
      *****************************************************************
       940-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE  SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE  "344"                TO  HKN-HKNNUM
           MOVE  HKNNUM-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  945-HKNNUM-READ-SEC
      *
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       940-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ情報読込
      *****************************************************************
       945-HKNNUM-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
      *
             IF  SYS-1001-SYOHOKBN   =   "0"
               MOVE  HKN-HON-GAIMNAIJGNGAK  TO  WRK-G-JOUGENGK
             ELSE
               MOVE  HKN-HON-GAIMGAIJGNGAK  TO  WRK-G-JOUGENGK
             END-IF
             MOVE  HKN-HON-NYUMJGNGAK       TO  WRK-N-JOUGENGK
             DISPLAY "外来上限額=" WRK-G-JOUGENGK
             DISPLAY "入院上限額=" WRK-N-JOUGENGK
      *
             IF    HKN-HKNNUM  =  "051"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "特定疾患"   TO   HKN-TANSEIDONAME
             END-IF
      *
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1            TO  FLG-HKNNUM
           END-IF
      *
           .
       945-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報検索   
      *****************************************************************
       950-JYUSIN-SELECT-SEC   SECTION.
      *
           INITIALIZE                       JYURRK-REC
           MOVE    SPA-HOSPNUM                  TO  JYURRK-HOSPNUM
           MOVE    SYU-PTID                     TO  JYURRK-PTID
           MOVE    SYU-NYUGAIKBN                TO  JYURRK-NYUGAIKBN
           MOVE    SYU-DENPNUM                  TO  JYURRK-DENPNUM
           MOVE    JYURRK-REC                   TO  MCPDATA-REC
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_jyurrk"       TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JYURRK
               PERFORM  950-JYUSIN-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       950-JYUSIN-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報FETCH    
      *****************************************************************
       950-JYUSIN-FETCH-SEC  SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_jyurrk"       TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       950-JYUSIN-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報CLOSE    
      *****************************************************************
       950-JYUSIN-CLOSE-SEC  SECTION.
      *
      *
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_jyurrk"       TO  MCP-TABLE
           MOVE    "key6"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       950-JYUSIN-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報SELECT  
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    WRK-SRYACCT-PATH   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC           =   ZERO
                   MOVE   ZERO            TO  FLG-SRYACCT
                   PERFORM  900-SRYACCT-FETCH-SEC
           ELSE
                   MOVE   1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       900-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    WRK-SRYACCT-PATH   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
                   MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
                   MOVE    1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報クローズ  
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    WRK-SRYACCT-PATH   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                  TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                 TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                 TO  MCPDATA-REC
           MOVE  "DBSELECT"                   TO  MCP-FUNC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"          TO  MCP-FUNC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    国保でない時は、読み直す
             IF    HKNJA-HKNNUM  NOT  =  "060" AND "067"
               GO               TO    925-HKNJAINF-READ-SEC
             END-IF
      *
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM                TO  COMB-HOSPNUM
           MOVE    SYU-PTID                   TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM            TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
             MOVE  "tbl_syskanri"        TO  MCP-TABLE
             MOVE  "key10"               TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key10"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE                   TO  SYS-1900-REC
           INITIALIZE                          SYS-1900-REC
           MOVE    "1900"                  TO  SYS-1900-KANRICD
           MOVE    "*"                     TO  SYS-1900-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-1900-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1901-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1902-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
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
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
