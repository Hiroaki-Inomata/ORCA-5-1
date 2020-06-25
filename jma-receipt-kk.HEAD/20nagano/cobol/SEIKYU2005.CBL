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
       PROGRAM-ID.     SEIKYU2005.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 長野・複写式レセプト（ＦＤ請求）
      *                      (199,299,399)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/28    内田          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    2008/02/26    ＣＳＶフォーマットの変更
      *                                    事業コードの取得の変更
      *  01.00.02    内田    2008/05/28    ４月改正対応
      *  01.00.03    内田    2008/06/13    後期高齢は「保険証・記号」には
      *                                    保険者番号を印字する
      *  01.00.04    内田    2008/07/15    「保険証・記号」は
      *                                    後期高齢者も社保国保と同様とする
      *  01.00.05    吉川    2008/09/10    公費の点数・診療実日数が
      *                                    出力されない不具合を修正
      *  01.00.06    内田    2009/02/24    食事療養費等は０円なら０と記録する
      *  01.00.07    吉川    2010/09/27    市町村番号が「000」の場合、
      *                                    件数が正しくならないのを修正
      *  01.00.08    吉川    2011/03/07    主保険＋全国公費の組合せのデータが
      *                                    集計されないのを修正
      *                                    生保は集計対象外とするように修正
      *                                    open-cobol1.0対応
      *  01.00.09    吉川    2011/05/19    生年月日は８桁で記録する
      *  01.00.10    萩野    2012/08/10    負担者番号の三桁未満エラー対応
      *  01.00.11    萩野    2014/02/13    実日数、点数ともに福祉使用分のみの
      *                                    記載に修正
      *  01.00.12    吉川    2014/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.13    吉川    2015/05/12    保険変更時、それぞれの点数と日数を
      *                                    印字するように修正
      *  01.00.14    吉川    2015/09/14    患者負担がない場合は印字しない
      *  01.00.15    吉川    2015/10/01    食事は保険分を印字する
      *  01.00.16    吉川    2016/01/08    データがない場合はファイルを作成しない
      *                                    処理結果画面の件数の不具合修正
      *  01.00.17    門間    2016/09/05    対象公費はPGオプションから取得する
      *  01.00.18    萩野    2017/12/11    医療機関コード変更時、
      *                                    医療機関コード別の作成処理対応
      *  01.00.19    萩野    2018/12/12    修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF101-FILE   ASSIGN    MF101PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF101-KEY
                               FILE    STATUS  IS  STS-MF101.
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF101-FILE.
       01  MF101-REC.
           COPY  "SEI2003.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *    シェル用領域
           COPY  "CPSHELLTBL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF101//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101       PIC X(02).
           03  STS-RECEERR     PIC X(02).
           03  STS-RECE        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF       PIC 9(01).
           03  FLG-END         PIC 9(01).
           03  FLG-MF101       PIC 9(01).
           03  FLG-PRINT       PIC 9(01).
           03  FLG-SYSKANRI    PIC 9(01).
           03  FLG-ERR         PIC 9(01).
           03  FLG-SYUNOU      PIC 9(01).
           03  FLG-RECE10      PIC 9(01).
           03  FLG-PTKOHINF    PIC 9(01).
           03  FLG-HKNCOMBI    PIC 9(01).
           03  FLG-BTPARA      PIC 9(01).
           03  FLG-SYORI       PIC 9(01).
           03  FLG-MOJI        PIC 9(01).
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
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
           03  IDW            PIC 9(04).
           03  IDX-TOWN       PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z1                PIC Z.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z4                PIC ZZZ9.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z8                PIC ZZZZZZZZ.
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
           03  WRK-PRTID             PIC X(20).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOBETUCITYNUM     PIC X(03).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
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
           03  WRK-KOHID               PIC 9(10).
           03  WRK-FTNMONEY-G          PIC 9(08).
           03  WRK-ETC-TBL.
               05  WRK-ETC-KOHNUM          PIC X(03) OCCURS 2.
               05  WRK-ETC-TENSU           PIC 9(07) OCCURS 2.
               05  WRK-ETC-ITBFTN          PIC 9(07) OCCURS 2.
               05  WRK-ETC-FTNJANUM        PIC X(20).
               05  WRK-ETC-JKYSNUM         PIC X(20).
               05  WRK-ETC-SKJ-NISSU       PIC 9(02) OCCURS 2.
               05  WRK-ETC-SKJ-RYOYOHI     PIC 9(07) OCCURS 2.
               05  WRK-ETC-SKJ-FTN         PIC 9(07) OCCURS 2.
      *
           03  WRK-PATH                   PIC  X(50).
      *    データ編集
           03  WRK-HENSHU.
               05  WRK-GOKEI-TBL           OCCURS  10.
                   07  WRK-GOKEI-YM        PIC 9(06).
                   07  WRK-GOKEI-DATA      OCCURS  151.
                       09  WRK-GOKEIKENSU  PIC  9(03).
               05  WRK-SOUGOKEIKENSU       PIC 9(07).
      *
               05  WRK-RECENO            PIC 9(05).
               05  WRK-RECEHKNSHU        PIC 9(01).
               05  WRK-TOKKI             PIC X(02).
               05  WRK-TOWN              PIC 9(03).
               05  WRK-CHK-SRYYM         PIC 9(06).
               05  WRK-HKNJACVS          PIC X(08).
               05  WRK-MOJIST            PIC 9(03).
               05  WRK-MOJIED            PIC 9(03).
               05  WRK-RATE              PIC 9(02).
               05  WRK-MOJIHEN           PIC 9(10).
      *
               05  WRK-JNISSU            PIC X(03).
               05  WRK-EJNISSU1          PIC X(03).
               05  WRK-EJNISSU2          PIC X(03).
               05  WRK-TOTALTEN          PIC X(08).
               05  WRK-ETOTALTEN1        PIC X(08).
               05  WRK-ETOTALTEN2        PIC X(08).
               05  WRK-FTNMONEY          PIC X(08).
               05  WRK-EITBFTN1          PIC X(08).
               05  WRK-EITBFTN2          PIC X(08).
               05  WRK-SHOKUJINISSU      PIC X(03).
               05  WRK-SHOKUJIRYOYOHI    PIC X(08).
               05  WRK-SHOKUJIFTN        PIC X(08).
      *
           03  WRK-REC             PIC X(500).
           03  WRK-REC-LENGTH      PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(100).
               05  WRK-NUM         PIC ZZZZZZZZZZ.999.
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-ST          PIC 9(02).
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-END         PIC 9(01).
      *
           03  WRK-SCSVINFO-SEQ-NO       PIC 9(06).
           03  WRK-SCSVINFO-PTID         PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN    PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF101   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-OUT     PIC 9(06).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *    レセプト印刷情報
           COPY    "CPRCF008.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
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
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
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
           MOVE    "SEIKYU2005"    TO  JOB-PGID
           MOVE    "福祉ＣＳＶ処理" 
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
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF101PARA-HOSPNUM
           MOVE  "MF101ZZ"            TO  MF101PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF101PARA-TERMID
      *
           MOVE  MF101PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF101PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"         TO  SYS-1001-KANRICD
           MOVE  "*"            TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"           TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC   TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU2005 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"         TO  SYS-1002-KANRICD
           MOVE  "*"            TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"           TO  SYS-1002-STYUKYMD(7:2)
                                    SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC   TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU2005 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
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
           MOVE    "SEIKYU2005"              TO  WRK-PRGID
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
           OPEN     OUTPUT  MF101-FILE
           CLOSE            MF101-FILE
           OPEN     I-O     MF101-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           MOVE   ZERO      TO   WRK-TOWN
           MOVE   ZERO      TO   IDX-TOWN
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF101-FILE
      *
           IF       CNT-MF101   >       0
      *       ＣＳＶファイル出力
              PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
      *    月途中の公費切替え対応
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
             INITIALIZE                            WRK-CHK-KOHNUM-TBL
             MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
               PERFORM 20011-SYUKEI-SEC
             END-IF
           END-PERFORM
      *
           IF  FLG-END  =  ZERO
               PERFORM  900-KOHSKY-READ-SEC
           END-IF
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
           INITIALIZE       MF101-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF101-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF101-PTNUM
           MOVE  RECE02-RECEKA       TO  MF101-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF101-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF101-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF101-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF101-SRYYM
           MOVE  RECE02-TEISYUTUSAKI TO  MF101-TEISYUTUSAKI
           MOVE  RECE02-RECESYUBETU  TO  MF101-RECESYUBETU
           MOVE  RECE02-HOJOKBN-KEY  TO  MF101-HOJOKBN-KEY
      *
           MOVE  RECE02-PTID         TO  MF101-PTID
           MOVE  RECE02-NAME         TO  MF101-NAME
           MOVE  RECE02-KANANAME     TO  MF101-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF101-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF101-SEX
           MOVE  RECE02-AGE          TO  MF101-AGE
           MOVE  RECE02-HKN          TO  MF101-HKN
           MOVE  RECE02-RJNHKN       TO  MF101-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF101-ZAI
      *
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF101-FTNJANUM
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF101-KOHINF
           COMPUTE  IDY  =  IDZ  +  1
      *    保険のデータ取得
           MOVE  RECE02-JNISSU(IDY)      TO  MF101-JNISSU
           MOVE  RECE02-FTNMONEY(IDY)    TO  MF101-FTNMONEY2
           MOVE  RECE02-TOTALTEN(IDY)    TO  MF101-TOTALTEN
           MOVE  RECE02-SHOKUJIINF(1)    TO  MF101-SHOKUJIINF
      *
      *    正しい市町村番号か
           IF  RECE02-KOHFTNJANUM(IDZ)(1:3)  >  150
           OR  RECE02-KOHFTNJANUM(IDZ)  =  SPACE
               MOVE  ZERO  TO  CNT-RECE02
               MOVE    "不明な負担者番号です。患者番号："
                                         TO  WRK-RECEERR
               MOVE  RECE02-PTNUM        TO  WRK-RECEERR(33:)
               MOVE    1                 TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
               PERFORM  500-ERR-HENSYU-SEC
               GO  TO  20011-SYUKEI-EXT
           END-IF
      *
           IF  RECE02-KOHFTNJANUM(IDZ)(3:1)  =  SPACE
               MOVE  ZERO  TO  CNT-RECE02
               MOVE    "負担者番号が３桁未満です。患者番号："
                                         TO  WRK-RECEERR
               MOVE  RECE02-PTNUM        TO  WRK-RECEERR(37:)
               MOVE    1                 TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
               PERFORM  500-ERR-HENSYU-SEC
               GO  TO  20011-SYUKEI-EXT
           END-IF
      *
      *    個別発行用市町村番号
           MOVE  RECE02-KOHFTNJANUM(IDZ)(1:3)    TO  WRK-KOBETUCITYNUM
      *
      *    請求管理情報より、他公費情報取得
           IF       IDZ           =     1
           AND      RECE02-KOHNUM(IDZ + 1)  =  SPACE
             PERFORM  900-SEIKYU-READ-SEC
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     2  OR
                      RECE10-KOHNUM(IDX)  =  SPACE
               IF  RECE10-KOHNUM(IDX)  NOT =  "012"
                 MOVE    RECE10-KOHNUM(IDX)       TO  MF101-EKOHNUM(IDX)
                 MOVE    RECE10-JNISSU(IDX + 1)   TO  MF101-ENISSU(IDX)
                 MOVE    RECE10-TOTALTEN(IDX + 1) TO  MF101-ETENSU(IDX)
                 MOVE    RECE10-FTNMONEY(IDX + 1) TO  MF101-EITBFTN(IDX)
      *食事日数
                 MOVE    RECE10-SHOKUJINISSU(IDX + 1)
                                     TO  MF101-E-SHOKUJINISSU(IDX)
      *食事療養日
                 MOVE    RECE10-SHOKUJIRYOYOHI(IDX + 1)
                                     TO  MF101-E-SHOKUJIRYOYOHI(IDX)
      *食事標準負担額
                 MOVE    RECE10-SHOKUJIFTN(IDX + 1)
                                     TO  MF101-E-SHOKUJIFTN(IDX)
                 MOVE    RECE10-KOHNUM(IDX)         TO  WRK-KOHNUM
                 PERFORM 930-PTKOHINF-SET-SEC
                 MOVE    WRK-ETC-FTNJANUM    TO  MF101-EKOHFTNJANUM(IDX)
                 MOVE    WRK-ETC-JKYSNUM     TO  MF101-EKOHJKYSNUM(IDX)
               END-IF
             END-PERFORM
      *    特記事項
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     3
                 IF   RECE10-TOKKI(IDX)  NOT =  SPACE
                   DISPLAY "RECE10-TOKKI(IDX) =" RECE10-TOKKI(IDX)
                   MOVE  RECE10-TOKKI(IDX)  TO  MF101-TOKKI(IDX)
                 END-IF
             END-PERFORM
           END-IF
      *
      *    助成判定及び、月途中で公費を切替えた時の全国公費分のデータ集計
           MOVE     MF101-KOHNUM           TO  WRK-KOHNUM
           MOVE     MF101-KOHID            TO  WRK-KOHID
           MOVE     ZERO                   TO  WRK-FTNMONEY-G
           INITIALIZE                          WRK-ETC-TBL
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"        TO  WRK-PATH
           ELSE
               MOVE    "key25"        TO  WRK-PATH
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
           IF       IDZ           =     1
           AND      RECE02-KOHNUM(IDZ + 1)  =  SPACE
             DISPLAY "請求管理から公費情報取得済み" RECE02-PTNUM
           ELSE
             DISPLAY  "患者番号=" RECE02-PTNUM
                      "その他公費=" WRK-ETC-KOHNUM(1)
             IF  WRK-ETC-KOHNUM(1)     NOT  =  SPACE
               DISPLAY "その他公費番号=" WRK-ETC-KOHNUM(1)
                       ","               WRK-ETC-KOHNUM(2)
               IF      WRK-ETC-KOHNUM(2)    =  SPACE  OR
                       WRK-ETC-KOHNUM(2)    >  WRK-ETC-KOHNUM(1)
                 MOVE  1                   TO  IDX
                 MOVE  2                   TO  IDY
               ELSE
                 MOVE  2                   TO  IDX
                 MOVE  1                   TO  IDY
               END-IF
               MOVE    WRK-ETC-KOHNUM(1)   TO  WRK-KOHNUM
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    WRK-ETC-TENSU(1)    TO  MF101-ETENSU(IDX)
               MOVE    WRK-ETC-ITBFTN(1)   TO  MF101-EITBFTN(IDX)
               MOVE    WRK-ETC-FTNJANUM    TO  MF101-EKOHFTNJANUM(IDX)
               MOVE    WRK-ETC-JKYSNUM     TO  MF101-EKOHJKYSNUM(IDX)
      *食事
               MOVE    WRK-ETC-SKJ-NISSU(1)
                               TO  MF101-E-SHOKUJINISSU(IDX)
               MOVE    WRK-ETC-SKJ-RYOYOHI(1)
                               TO  MF101-E-SHOKUJIRYOYOHI(IDX)
               MOVE    WRK-ETC-SKJ-FTN(1)
                               TO  MF101-E-SHOKUJIFTN(IDX)
               IF      WRK-ETC-KOHNUM(2)  NOT  =  SPACE
                 MOVE    WRK-ETC-KOHNUM(2)   TO  WRK-KOHNUM
                 PERFORM 930-PTKOHINF-SET-SEC
                 MOVE    WRK-ETC-TENSU(2)  TO  MF101-ETENSU(IDY)
                 MOVE    WRK-ETC-ITBFTN(2) TO  MF101-EITBFTN(IDY)
                 MOVE    WRK-ETC-FTNJANUM  TO  MF101-EKOHFTNJANUM(IDY)
                 MOVE    WRK-ETC-JKYSNUM   TO  MF101-EKOHJKYSNUM(IDY)
      *食事
                 MOVE    WRK-ETC-SKJ-NISSU(2)
                               TO  MF101-E-SHOKUJINISSU(IDY)
                 MOVE    WRK-ETC-SKJ-RYOYOHI(2)
                               TO  MF101-E-SHOKUJIRYOYOHI(IDY)
                 MOVE    WRK-ETC-SKJ-FTN(2)
                               TO  MF101-E-SHOKUJIFTN(IDY)
               END-IF
             END-IF
           END-IF
      *
           IF  RECE02-FTNMONEY(1)          >   ZERO  AND
               RECE02-FTNMONEY(1)          <   WRK-FTNMONEY-G
             MOVE  RECE02-FTNMONEY(1)      TO  WRK-FTNMONEY-G
           END-IF
           MOVE  WRK-FTNMONEY-G            TO  MF101-FTNMONEY
      *
      *    保険者番号チェック処理
           MOVE    ZERO        TO  FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF      FLG-ERR     =   1
           OR      MF101-FTNMONEY2  =  ZERO
             DISPLAY "助成対象外="  RECE02-PTNUM
             CONTINUE
           ELSE
             WRITE   MF101-REC
             ADD  1  TO  CNT-MF101
      *      CSV総括用件数カウント
             MOVE  RECE02-KOHFTNJANUM(IDZ)(1:3)   TO   WRK-TOWN
             COMPUTE  IDX-TOWN  =  WRK-TOWN  +  1
      *      診療年月でデータを分ける
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     10
                      OR       WRK-GOKEI-YM (IDX1)  =  ZERO
                 IF  MF101-SRYYM  =  WRK-GOKEI-YM (IDX1)
                     ADD   1    TO  WRK-GOKEIKENSU(IDX1 IDX-TOWN)
                     MOVE  10   TO  IDX1
                 END-IF
             END-PERFORM
      *      その診療年月で初データの場合
             IF  IDX1  <  10
                 MOVE  MF101-SRYYM  TO  WRK-GOKEI-YM (IDX1)
                 ADD   1    TO  WRK-GOKEIKENSU(IDX1 IDX-TOWN)
             END-IF
             ADD   1   TO   WRK-SOUGOKEIKENSU
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "20"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)
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
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF101-FILE
      *
           MOVE     ZERO        TO       WRK-RECENO
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     ZERO        TO       WRK-TOWN
           MOVE     ZERO        TO       IDX-TOWN
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
      *    ＣＳＶヘッダー処理
           PERFORM  330-CSVH-HEN-SEC
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    FLG-PRINT   =  1
      *       ＣＳＶ<総括>処理
              PERFORM  331-CSVSOKATU-HEN-SEC
      *
              PERFORM  UNTIL  MF101-KOHFTNJANUM(1:3) NOT  =  WRK-TOWN
                       OR     MF101-SRYYM    NOT  =  WRK-CHK-SRYYM
                       OR     FLG-PRINT   =  1
      *           ＣＳＶ<レセ>処理
                  PERFORM  332-CSVRECE-HEN-SEC
              END-PERFORM
      *
           END-PERFORM
      *    ＣＳＶ＜合計＞処理
           PERFORM  333-CSVGOKEI-HEN-SEC
      *    ＣＳＶ<トレーラ>処理
           PERFORM  334-CSVTRAILER-HEN-SEC
      *
           CLOSE    MF101-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<ヘッダー>処理
      *****************************************************************
       330-CSVH-HEN-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *    ヘッダー
      *
      *    レコード区分
           MOVE    "1"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    作成年月日
           MOVE    LNK-PRTKANRI-SKYYMD     TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    医療機関コード
           MOVE    "201"                   TO  WRK-NUM(1:)
           MOVE    SYS-1001-HOSPCD         TO  WRK-NUM(4:)
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    マルチボリューム識別番号
           MOVE    "00"                    TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       330-CSVH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<内容>処理
      *****************************************************************
       331-CSVSOKATU-HEN-SEC      SECTION.
      *
           INITIALIZE                     WRK-REC
                                          WRK-REC-LENGTH
                                          WRK-MOJIHEN
      *    総括
      *
           MOVE  MF101-KOHFTNJANUM(1:3)   TO   WRK-TOWN
           COMPUTE  IDX-TOWN  =  WRK-TOWN  +  1
           MOVE  MF101-SRYYM              TO   WRK-CHK-SRYYM
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     10
               IF  MF101-SRYYM  =  WRK-GOKEI-YM (IDX1)
                   MOVE  WRK-GOKEIKENSU(IDX1 IDX-TOWN)
                         TO  WRK-MOJIHEN
                   MOVE  10    TO  IDX1
               END-IF
           END-PERFORM
      *
      *    レコード区分
           MOVE    "2"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療年月
           MOVE    MF101-SRYYM             TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    医療機関コード
           MOVE    "201"                   TO  WRK-NUM(1:)
           MOVE    SYS-1001-HOSPCD         TO  WRK-NUM(4:)
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    旧総合病院診療科
           IF  SYS-1001-HOSPSBT1  =  1
               MOVE  MF101-SRYKA           TO  WRK-NUM
           ELSE
               MOVE  00                    TO  WRK-NUM
           END-IF
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    福祉市町村コード
           MOVE    MF101-KOHFTNJANUM(1:3)  TO  WRK-DATA
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    件数
           MOVE    WRK-MOJIHEN             TO  WRK-NUM
           MOVE    5                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       331-CSVSOKATU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<内容>処理
      *****************************************************************
       332-CSVRECE-HEN-SEC      SECTION.
      *
           INITIALIZE  WRK-REC
                       WRK-REC-LENGTH
           INITIALIZE  FLG-MOJI
           INITIALIZE  WRK-MOJIST
           INITIALIZE  WRK-MOJIED
      *
      *    レセプト
      *
      *    レコード区分
           MOVE    3                       TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １．データNo
           ADD   1     TO    WRK-RECENO
           MOVE    WRK-RECENO              TO  WRK-NUM
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２．福祉市町村コード
           MOVE    MF101-KOHFTNJANUM(1:3)  TO  WRK-DATA
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．事業コード
           MOVE    MF101-KOHJKYSNUM(1:2)        TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．福祉受給者番号
           MOVE    MF101-KOHJKYSNUM(3:)    TO  WRK-DATA
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．保険者番号
      *    保険者の先頭スペース詰め
           PERFORM   VARYING  IDX2    FROM    1   BY  1
                UNTIL     FLG-MOJI   =   1
                   OR     WRK-MOJIED =   8
              IF  MF101-HKNJANUM(IDX2:1)    =    SPACE
                 MOVE   1             TO     FLG-MOJI
              ELSE
                 ADD    1             TO     WRK-MOJIED
              END-IF
           END-PERFORM
      *
           COMPUTE  WRK-MOJIST  =  9  -  WRK-MOJIED
           MOVE  SPACE                TO     WRK-HKNJACVS
           MOVE  MF101-HKNJANUM       TO
                                  WRK-HKNJACVS(WRK-MOJIST:WRK-MOJIED)
           IF  MF101-HKNNUM = "067"
              MOVE   SPACE            TO     WRK-HKNJACVS(1:2)
           END-IF
           MOVE    WRK-HKNJACVS            TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ６．診療年月
           MOVE    MF101-SRYYM             TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ７．保険種別
           EVALUATE  TRUE
             WHEN  MF101-RJNHKNNUM = "027"
             WHEN  MF101-HKNNUM  =  "039"  OR  "040"
               MOVE  3   TO  WRK-RECEHKNSHU
             WHEN  MF101-HKNNUM = "063" OR "072" OR "073" OR
                                  "067" OR "074" OR "075"
               MOVE  4   TO  WRK-RECEHKNSHU
             WHEN OTHER
               MOVE  1   TO  WRK-RECEHKNSHU
           END-EVALUATE
           MOVE    WRK-RECEHKNSHU          TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ８．本人家族入外区分
           MOVE    MF101-RECESYUBETU(4:1)  TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ９．老人保健市町村番号
           MOVE    MF101-RJNFTNJANUM       TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １０．公費１・負担者番号
           MOVE    MF101-EKOHFTNJANUM(1)   TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １１．公費２・負担者番号
           MOVE    MF101-EKOHFTNJANUM(2)   TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １２．給付割合
           EVALUATE  MF101-KYURATE
             WHEN  30
               MOVE  70        TO  WRK-RATE
             WHEN  20
               MOVE  80        TO  WRK-RATE
             WHEN  10
               MOVE  90        TO  WRK-RATE
           END-EVALUATE
           MOVE    WRK-RATE                TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １３．記号
           MOVE    MF101-KIGO              TO  WRK-DATA
           MOVE    40                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １４．番号
           MOVE    MF101-NUM               TO  WRK-DATA
           MOVE    40                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １５．受給者氏名
           MOVE    MF101-NAME              TO  WRK-DATA
           MOVE    60                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １６．性別
           MOVE    MF101-SEX               TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １７．生年月日
           MOVE    MF101-BIRTHDAY          TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １８．特記事項１
           MOVE    MF101-TOKKI(1)          TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １９．特記事項２
           MOVE    MF101-TOKKI(2)          TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２０．特記事項３
           MOVE    MF101-TOKKI(3)          TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２１．保険・診療実日数
           MOVE    MF101-JNISSU            TO  WRK-NUM
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２２．公費１・診療実日数
           MOVE    MF101-ENISSU(1)         TO  WRK-NUM
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２３．公費２・診療実日数
           MOVE    MF101-ENISSU(2)         TO  WRK-NUM
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２４．保険・点数
           MOVE    MF101-TOTALTEN          TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２５．公費１・点数
           MOVE    MF101-ETENSU(1)         TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２６．公費２・点数
           MOVE    MF101-ETENSU(2)         TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２７．保険・一部負担金
           MOVE    MF101-FTNMONEY          TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２８．公費１・一部負担金
           MOVE    MF101-EITBFTN(1)        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２９．公費２・一部負担金
           MOVE    MF101-EITBFTN(2)        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３０．保険・食事日数
           MOVE    MF101-SHOKUJINISSU      TO  WRK-NUM
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３１．保険・食事基準額
           MOVE    MF101-SHOKUJIRYOYOHI    TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３２．保険・食事標準負担額
           MOVE    MF101-SHOKUJIFTN        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３３．公費１・食事日数
           IF   MF101-EKOHNUM (1)  NOT =  SPACE
           AND  MF101-E-SHOKUJINISSU(1)  =  0
             MOVE    "0"                   TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    MF101-E-SHOKUJINISSU(1) TO  WRK-NUM
             MOVE    3                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３４．公費１・食事基準額
           IF   MF101-EKOHNUM (1)  NOT =  SPACE
           AND  MF101-E-SHOKUJIRYOYOHI(1)  =  0
             MOVE    "0"                   TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    MF101-E-SHOKUJIRYOYOHI(1)   TO  WRK-NUM
             MOVE    8                           TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３５．公費１・食事標準負担額
           IF   MF101-EKOHNUM (1)  NOT =  SPACE
           AND  MF101-E-SHOKUJIFTN(1)  =  0
             MOVE    "0"                   TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    MF101-E-SHOKUJIFTN(1)   TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３６．公費２・食事日数
           IF   MF101-EKOHNUM (2)  NOT =  SPACE
           AND  MF101-E-SHOKUJINISSU(2)  =  0
             MOVE    "0"                   TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    MF101-E-SHOKUJINISSU(2) TO  WRK-NUM
             MOVE    3                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３７．公費２・食事基準額
           IF   MF101-EKOHNUM (2)  NOT =  SPACE
           AND  MF101-E-SHOKUJIRYOYOHI(2)  =  0
             MOVE    "0"                   TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    MF101-E-SHOKUJIRYOYOHI(2)   TO  WRK-NUM
             MOVE    8                           TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３８．公費２・食事標準負担額
           IF   MF101-EKOHNUM (2)  NOT =  SPACE
           AND  MF101-E-SHOKUJIFTN(2)  =  0
             MOVE    "0"                   TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    MF101-E-SHOKUJIFTN(2)   TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    ３９．公費１・対象一部負担金
           MOVE    ZERO                    TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ４０．公費２・対象一部負担金
           MOVE    ZERO                    TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC

           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
           .
       332-CSVRECE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<合計>処理
      *****************************************************************
       333-CSVGOKEI-HEN-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *    合計
      *
      *    レコード区分
           MOVE    8                       TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    作成年月日
           MOVE    LNK-PRTKANRI-SKYYMD     TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    医療機関コード
           MOVE    "201"                   TO  WRK-NUM(1:)
           MOVE    SYS-1001-HOSPCD         TO  WRK-NUM(4:)
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    明細レコード数
           MOVE    WRK-SOUGOKEIKENSU       TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       333-CSVGOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<トレーラ>処理
      *****************************************************************
       334-CSVTRAILER-HEN-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *　　トレーラ
      *
      *    レコード区分
           MOVE    9                       TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    作成年月日
           MOVE    LNK-PRTKANRI-SKYYMD     TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    医療機関コード
           MOVE    "201"                   TO  WRK-NUM(1:)
           MOVE    SYS-1001-HOSPCD         TO  WRK-NUM(4:)
           MOVE    10                      TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       334-CSVTRAILER-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯ出力処理
      *****************************************************************
       500-CSVINFO-SEC                SECTION.
      *
           ADD     1                   TO  WRK-SCSVINFO-SEQ-NO
      *
           INITIALIZE                  ORCSCSVINFOAREA
           MOVE    "INS"               TO  SCSVINFO-MODE
           MOVE    WRK-PARA-SHELLID    TO  SCSVINFO-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SCSVINFO-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SCSVINFO-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-RENNUM TO  SCSVINFO-RENNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SCSVINFO-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SCSVINFO-SKYYMD
           MOVE    WRK-SCSVINFO-SEQ-NO TO  SCSVINFO-SEQ-NO
           MOVE    WRK-SCSVINFO-NYUGAIKBN
                                       TO  SCSVINFO-NYUGAIKBN
           MOVE    WRK-SCSVINFO-PTID   TO  SCSVINFO-PTID
           MOVE    WRK-REC             TO  SCSVINFO-CSVDATA
           CALL    "ORCSCSVINFO"       USING
                                       ORCSCSVINFOAREA
                                       SPA-AREA
           IF      SCSVINFO-RETURN     =   ZERO
               ADD     1                   TO  CNT-OUT
           ELSE
               MOVE    "ＣＳＶ用ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           .
       500-CSVINFO-EXT.
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
      *      ジョブ終了処理
             MOVE    "JBE"           TO  SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
             MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID
                                     TO  JOB-SHELLID
             MOVE    WRK-RECEERR     TO  JOB-YOBI
             MOVE    "9999"          TO  JOB-ERRCD
             CALL    "ORCSJOB"       USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
           END-IF
      *
           MOVE   1                  TO  FLG-END
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
           CLOSE  MF101-FILE
           MOVE   WRK-SOUGOKEIKENSU  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2005 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2005 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU2005 END ***"
      *
           IF  CNT-MF101  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ファイル保存情報更新
           PERFORM 3001-FILE-INFO-INSERT-SEC
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
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
           MOVE    "FUKUSHI.DAT"   TO  SFILESV-TO-DATA  (1)
           MOVE    WRK-SOUGOKEIKENSU   TO  SFILESV-CNT-ALL  (1)
           MOVE    "福祉ＣＳＶ"    TO  SFILESV-TITLE    (1)
           IF      WRK-SOUGOKEIKENSU   >   ZERO
               MOVE    "1"         TO  SFILESV-DATA-TYPE(1)
           END-IF 
           MOVE    WRK-RECEERR     TO  SFILESV-ERR-MSG  (1)
           IF      WRK-RECEERR NOT =   SPACE
               MOVE    "1"         TO  SFILESV-ERR-CODE (1)
           END-IF
           MOVE    "1"             TO  SFILESV-CODE-TYPE (1)
      *
           CALL    "ORCSFILESV"        USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF101-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
               PERFORM  900-KOHSKY-CLOSE-SEC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
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
                           MOVE    ZERO                TO  FLG-ERR
                           MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                         END-IF
                     END-PERFORM 
                   END-IF
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
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
           MOVE    SYU-PTID                TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
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
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     4  OR
                    SYU-KOH-HKNNUM(IDX) =  SPACE
      *
               IF   SYU-KOH-HKNNUM(IDX)  NOT =  "027"  AND
                    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"
                 IF  WRK-ETC-KOHNUM(1)       =  SPACE
                 OR                             SYU-KOH-HKNNUM(IDX)
                   MOVE  1                    TO  IDY
                 ELSE
                   MOVE  2                    TO  IDY
                 END-IF
                 DISPLAY "IDY=" IDY
                 MOVE  SYU-KOH-HKNNUM(IDX)  TO  WRK-ETC-KOHNUM(IDY)
                 ADD   SYU-TOTAL-HKNTEN     TO  WRK-ETC-TENSU(IDY)
                 IF   (RECE02-FTNMONEY(1)    >  ZERO)
                 OR   (SYU-KOH-HKNNUM(IDX)  NOT =  "010" AND "011"
                                            AND    "021" AND "020")
                   ADD     SYU-KOH-COMPFTN(IDX)
                                            TO  WRK-ETC-ITBFTN(IDY)
                 END-IF
      * 主保険の食事療養費情報から食事療養費、標準負担額を取得
      * ※全国公費分がある場合に限り取得しているため、主保険で代用
                 ADD  SYU-SYUSKJ-NISSU
                           TO  WRK-ETC-SKJ-NISSU(IDY)
                 ADD  SYU-SYUSKJ-RYOYOHI
                           TO  WRK-ETC-SKJ-RYOYOHI(IDY)
                 ADD  SYU-SYUSKJ-FTN
                           TO  WRK-ETC-SKJ-FTN(IDY)
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX) = "972"
                 MOVE    1     TO      MF101-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX) = "974"
                 MOVE    2     TO      MF101-TYOKI
               END-IF
      *
           END-PERFORM
      *
           IF  RECE02-FTNMONEY(1)       >  ZERO
             IF  RECE02-RJNHKNNUM  NOT  =  SPACE
               ADD  SYU-KOH-COMPFTN(1) TO WRK-FTNMONEY-G
             ELSE
               ADD  SYU-SYUCOMPFTN     TO WRK-FTNMONEY-G
             END-IF
           END-IF
      *
           .
       800-SEIKYU-SEC-EXT.
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
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
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
           CALL  "ORCDBMAIN"        USING
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
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY              TO  RECE10-KEY
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_seikyu"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE10
                   INITIALIZE                  RECE10-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
               INITIALIZE                  RECE10-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE  RECE02-PTID        TO  PTKOH-PTID
           MOVE  WRK-KOHNUM         TO  PTKOH-KOHNUM
      *
           MOVE  PTKOHINF-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
             MOVE    "key4"                  TO  MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
               IF  PTKOH-SAKJOKBN  NOT = "1"
                 IF  PTKOH-TEKSTYMD(1:6) <= RECE02-SRYYM(1:6)  AND 
                     PTKOH-TEKEDYMD(1:6) >= RECE02-SRYYM(1:6)
                     MOVE  PTKOH-FTNJANUM  TO  WRK-ETC-FTNJANUM
                     MOVE  PTKOH-JKYSNUM   TO  WRK-ETC-JKYSNUM
                 END-IF
               END-IF
                 MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
                 MOVE    "key4"                  TO  MCP-PATHNAME
                 PERFORM  935-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       930-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1  TO  FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW    <       1
                   IF      WRK-DATA (IDW:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW)           DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW    FROM    WRK-ST    BY  1
                       UNTIL   IDW    >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                      IDZ
                        MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
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
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"               TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
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
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
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
           CALL    "ORCDBMAIN"         USING
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
      *
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
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
