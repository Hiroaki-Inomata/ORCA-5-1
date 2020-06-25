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
       PROGRAM-ID.     SEIKYU2113.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 岐阜・福祉医療費ＣＳＶ(145,146,143)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/03/14    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    13/04/11    前期高齢者で高額療養費に
      *                                  達した場合の患者負担額の計算修正
      *  01.00.02    吉川    14/05/12    前期高齢者２割対応
      *  01.00.03    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.04    萩野    15/01/15    平成２７年１月改正対応（長期欄の難病対応）
      *                                  前期高齢者欄にて負担割合３割の場合
      *                                  「１」と印字するように修正
      *  01.00.05    吉川    15/11/17    受給者番号のハイフンを除去する
      *                                  カナ氏名は半角カナにする
      *  01.00.06    吉川    16/06/07    対象公費はPGオプションから取得する
      *  01.00.07    門間    17/04/10    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.08    萩野    17/08/10    平成２９年８月から高額療養費の限度額を変更。
      *  01.00.09    萩野    18/03/05    項目「処理年月」は、診療年月ではなく
      *                                  請求年月を出力するように変更
      *  01.00.10    萩野    18/10/11    修正履歴コメント削除
      *  01.00.11    井上    19/05/08    改元対応
      *  01.00.12    萩野    19/08/08    患者カナ氏名が２０文字を超えた場合の
      *                                  改行の不具合を修正
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
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI2103.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                    REPLACING  //RECE01//
                                    BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100             PIC X(02).
           03  STS-RECEERR           PIC X(02).
           03  STS-RECE02            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF             PIC 9(01).
           03  FLG-END               PIC 9(01).
           03  FLG-MF100             PIC 9(01).
           03  FLG-PRINT             PIC 9(01).
           03  FLG-SYSKANRI          PIC 9(01).
           03  FLG-ERR               PIC 9(01).
           03  FLG-HEIYOU            PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-BTPARA            PIC 9(01).
           03  FLG-SYORI             PIC 9(01).
           03  FLG-TYOKI             PIC 9(01).
           03  FLG-JOUGEN            PIC 9(01).
           03  FLG-TSYRRK            PIC 9(01).
           03  FLG-TNKRRK            PIC 9(01).
           03  FLG-TEISHO            PIC 9(01).
           03  FLG-PRGOPTION         PIC 9(01).
           03  FLG-OK                PIC 9(01).
           03  FLG-TAISYO            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(05).
           03  IDXX                  PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDX4                  PIC 9(04).
           03  IDY                   PIC 9(04).
           03  IDZ                   PIC 9(04).
           03  IDW                   PIC 9(04).
           03  IDW2                  PIC 9(04).
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
           03  WRK-Z1                PIC Z.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z4                PIC ZZZ9.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z8                PIC ZZZZZZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-9                 PIC 9(10).
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
           03  WRK-GYYMM             PIC X(05).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-KASAN-TBL.
               05   WRK-FTNJANUM     PIC X(08).
               05   WRK-SEIKYUGK-G   PIC 9(08)V9(01).
               05   WRK-SEIKYUGK     PIC 9(08)V9(01).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-GPAGE             PIC 9(03).
           03  WRK-GKENSU            PIC 9(04).
           03  WRK-AMARI             PIC 9(02).
           03  WRK-CALC              PIC 9(05).
           03  WRK-HOSPSYOGO         PIC 9(08).
           03  WRK-KANANAME          PIC X(100).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
           03  WRK-ETC-INPUT         PIC X(200).
      *    ファイル名称用エリア
           03  FILE-NAME             PIC X(512).
      *
           03  WRK-PATH              PIC  X(50).
           03  WRK-FILENAME          PIC  X(40).
      *    データ編集
           03  WRK-HENSHU.
               05  WRK-RATE          PIC 9(02).
               05  WRK-MOJIHEN       PIC 9(10).
      *
           03  WRK-REC               PIC X(500).
           03  WRK-REC-LENGTH        PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA          PIC X(200).
               05  WRK-NUM           PIC 9999999999.
               05  WRK-NAGASA        PIC 9(04).
               05  WRK-NAGASA-SPACE  PIC 9(04).
               05  WRK-ST            PIC 9(02).
               05  WRK-SYOSUU        PIC 9(01).
               05  WRK-END           PIC 9(01).
      *
           03  WRK-SCSVINFO-SEQ-NO   PIC 9(06).
           03  WRK-SCSVINFO-PTID     PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN    PIC X(01).
      *
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
      *    明細領域
       01  SUM-MEISAI.
           03  SUM-MEI  OCCURS 10.
               05  SUM-RENNUM        PIC 9(02).
               05  SUM-JKYSNUM       PIC X(15).
               05  SUM-NYUGAIKBN     PIC X(01).
               05  SUM-HKNKBN        PIC X(01).
               05  SUM-KOREI         PIC X(01).
               05  SUM-TYOKI         PIC X(01).
               05  SUM-GKENTEN  OCCURS  3.
                   07  SUM-TENSU     PIC 9(08).
                   07  SUM-KOHTENSU  PIC 9(08).
               05  SUM-ITBFTN        PIC 9(06).
               05  SUM-SRYYM         PIC X(04).
               05  SUM-KANANAME      PIC X(100).
               05  SUM-NAGASA        PIC 9(02).
       
      *    集計領域
       01  SUM-AREA.
           03  SUM-GKENTEN  OCCURS  3.
               05  SUM-GKENSU        PIC 9(05).
               05  SUM-GTENSU        PIC 9(10).
               05  SUM-GKOHTENSU     PIC 9(10).
           03  SUM-GITBFTN           PIC 9(10).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02            PIC 9(06).
           03  CNT-MF100             PIC 9(06).
           03  CNT-PAGE              PIC 9(03).
           03  CNT-OUT               PIC 9(06).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM PIC X(08).
               05  TBL-PARA-PTID     PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX         PIC 9(03)   VALUE   100.
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
           03  CONS-SRYYM-201405      PIC X(06)   VALUE  "201405".
           03  CONS-BIRTHDAY-19440402 PIC 9(08)   VALUE  19440402.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
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
      *    レセプト印刷情報
           COPY    "CPRCF008.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    低所得情報
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    低年金情報
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
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
           COPY    "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
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
           INITIALIZE  SUM-AREA
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
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
           MOVE    "SEIKYU2113"        TO  JOB-PGID
           MOVE    "福祉医療費ＣＳＶ処理" 
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO  WRK-LASTYMD
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
           IF  STS-DAY-RC1  =  ZERO 
             MOVE    LNK-DAY2-EDTYMD3   TO  WRK-SEIYMDWH
           END-IF
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
           MOVE    "1001"               TO  SYS-1001-KANRICD
           MOVE    "*"                  TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                 TO  SYS-1001-STYUKYMD(7:2)
                                            SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM          TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC         TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC        TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU2113 SYS 1001 ERR ***"
             MOVE    1                  TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE    "1002"               TO  SYS-1002-KANRICD
           MOVE    "*"                  TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                 TO  SYS-1002-STYUKYMD(7:2)
                                            SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM          TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC         TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC        TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU2113 SYS 1002 ERR ***"
             MOVE    1                  TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME    TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS        TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL         TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
      *    公費請求テーブル呼び出し
           PERFORM 900-KOHSKY-START-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU2113"              TO  WRK-PRGID
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
           MOVE    ZERO                 TO  IDXX
                                            FLG-SYORI
           INITIALIZE                       TABLE-AREA
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
      *
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
           MOVE     ZERO    TO  FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
      *
           IF  CNT-RECE02  >  0
      *       ＣＳＶファイル出力
              PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1         TO   FLG-END
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
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >  4
             INITIALIZE                            WRK-CHK-KOHNUM-TBL
             MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
                 PERFORM 20011-SYUKEI-SEC
             END-IF
           END-PERFORM
      *
           PERFORM 900-KOHSKY-READ-SEC
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
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
      *    氏名はカナ名のほう
           MOVE  RECE02-KANANAME     TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  RECE02-ZAITAKU      TO  MF100-ZAITAKU
           IF    RECE02-HKNNUM       =   "060" OR "067"
               MOVE    "2"           TO  MF100-HKNKBN
           ELSE
               MOVE    "1"           TO  MF100-HKNKBN
           END-IF
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
      *    公費番号キーセット
           MOVE  MF100-KOHNUM              TO  MF100-KOHNUM-KEY
           MOVE  MF100-KOHJKYSNUM          TO  MF100-JKYSNUM-KEY
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSU      (IDY)  TO  MF100-JNISSU
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
           MOVE  RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
      *
      *    請求金額集計集計
           MOVE    MF100-KOHNUM            TO  WRK-KOHNUM
           MOVE    MF100-KOHID             TO  WRK-KOHID
           MOVE    ZERO                    TO  WRK-SEIKYUGK-G
                                               FLG-TYOKI
           MOVE    1                       TO  FLG-ERR
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
           DISPLAY "集計終了 請求額=" WRK-SEIKYUGK-G
      *
           MOVE  ZERO                      TO  FLG-JOUGEN
      *
      *    長期併用か、特定疾患併用かレセプトに
      *    一部負担金を記載する場合のみ患者負担額を記載する
           IF  MF100-TYOKI         NOT =   ZERO    OR
               RECE02-FTNMONEY(1)  NOT =   ZERO
      *
      *    保険限度額設定
               IF  RECE02-FTNMONEY(1)  >   ZERO    AND
                   RECE02-FTNMONEY(1)  <   WRK-SEIKYUGK-G
                   MOVE  RECE02-FTNMONEY(1)
                                           TO  WRK-SEIKYUGK-G
                   MOVE  1                 TO  FLG-JOUGEN
               END-IF
      *    長期限度額設定
               IF  WRK-SEIKYUGK-G      >   10000   AND
                   FLG-TYOKI         =   1
                   MOVE    10000       TO  WRK-SEIKYUGK-G
               END-IF
      *    長期上位
               IF  WRK-SEIKYUGK-G      >   20000   AND
                   FLG-TYOKI           =   2
                   MOVE    20000       TO  WRK-SEIKYUGK-G
               END-IF
               MOVE    WRK-SEIKYUGK-G  TO  MF100-ITBFTN
      *
      *    特定疾患併用判定
               IF  MF100-TYOKI         =   2  OR  3  OR  4  OR  5
                   MOVE    MF100-TOKUTEIFTN
                                       TO  MF100-ITBFTN
                   IF  MF100-KOHTENSU  =   MF100-TOTALTEN
                       CONTINUE
                   ELSE
      *              高額療養費の場合は請求額を印字
                     IF RECE02-ZAITAKU  =  "2" OR "3" OR "4"
                        MOVE    WRK-SEIKYUGK-G   TO  MF100-ITBFTN
                     ELSE
                       MOVE    ZERO              TO  FLG-TEISHO
      *                低所得チェック
                       PERFORM 200113-TEISHO-CHK-SEC
      *                低年金チェック
                       IF  FLG-TEISHO  =  ZERO
                           PERFORM 200114-TEINEN-CHK-SEC
                       END-IF
      *
      *                70歳以上で、高額限度額に達成していたときの
      *                患者負担額の計算
                       IF  RECE02-AGE  >=  70
      *
      *                  入院
                         IF  RECE02-NYUGAIKBN  =  "1"
      *                    現役並み所得者
                           IF  RECE02-KYURATE  =  30
                             MOVE    WRK-SEIKYUGK-G    TO  MF100-ITBFTN
                           ELSE
                             EVALUATE  FLG-TEISHO
      *                        低所得
                               WHEN  1
                                 IF  RECE02-FTNMONEY(1)  >=  24600
                                   MOVE    24600       TO  MF100-ITBFTN
                                 END-IF
      *                        低年金
                               WHEN  2
                                 IF  RECE02-FTNMONEY(1)  >=  15000
                                   MOVE    15000       TO  MF100-ITBFTN
                                 END-IF
      *                        一般
                               WHEN  OTHER
                                 IF  RECE02-FTNMONEY(1)  >=  57600
                                   MOVE    57600       TO  MF100-ITBFTN
                                 END-IF
                             END-EVALUATE
                           END-IF
      *
      *                  外来
                         ELSE
      *                    現役並み所得者
                           IF  RECE02-KYURATE  =  30
                             IF  RECE02-FTNMONEY(1)  >=  57600
                               MOVE    57600           TO  MF100-ITBFTN
                             END-IF
                           ELSE
      *                      低所得・低年金
                             IF  FLG-TEISHO  NOT =  ZERO
                               IF  RECE02-FTNMONEY(1)  >=  8000
                                 MOVE    8000          TO  MF100-ITBFTN
                               END-IF
      *                      一般
                             ELSE
                               IF  RECE02-FTNMONEY(1)  >=  14000
                                 MOVE    14000         TO  MF100-ITBFTN
                               END-IF
                             END-IF
                           END-IF
                         END-IF
                       END-IF
                     END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-ERR  =  ZERO
               WRITE  MF100-REC
               ADD  1  TO  CNT-MF100
           ELSE
             DISPLAY "助成対象外=" RECE02-PTNUM
                     "負担者番号=" MF100-FTNJANUM
                     "点数="       RECE02-TOTALTEN(IDZ)
                     "診療年月="   RECE02-SRYYM
                     "入外区分="   RECE02-NYUGAIKBN
           END-IF
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
           MOVE    MF100-FTNJANUM(3:5)   TO  WRK-HKNJANUM(1:5)
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
      *    低所得チェック処理
      *****************************************************************
       200113-TEISHO-CHK-SEC                SECTION.
      *
           INITIALIZE                      TSYRRK-REC
                                           FLG-TSYRRK
           MOVE    SPA-HOSPNUM             TO  TSYRRK-HOSPNUM
           MOVE    RECE02-PTID             TO  TSYRRK-PTID
           MOVE    TSYRRK-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      FLG-TSYRRK          =   ZERO
               PERFORM  800-TSYRRK-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-TSYRRK  =  1
                 IF  RECE02-SRYYM  >=  TSYRRK-NINSTYMD(1:6)  AND
                     RECE02-SRYYM  <=  TSYRRK-NINEDYMD(1:6)
                   MOVE  1  TO  FLG-TEISHO
                   MOVE  1  TO  FLG-TSYRRK
                 END-IF
                 PERFORM  800-TSYRRK-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       200113-TEISHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低年金チェック処理
      *****************************************************************
       200114-TEINEN-CHK-SEC                SECTION.
      *
           INITIALIZE                      TNKRRK-REC
                                           FLG-TNKRRK
           MOVE    SPA-HOSPNUM             TO  TNKRRK-HOSPNUM
           MOVE    RECE02-PTID             TO  TNKRRK-PTID
           MOVE    TNKRRK-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      FLG-TNKRRK          =   ZERO
               PERFORM  800-TNKRRK-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-TNKRRK  =  1
                 IF  RECE02-SRYYM  >=  TNKRRK-STYMD(1:6)  AND
                     RECE02-SRYYM  <=  TNKRRK-EDYMD(1:6)
                   MOVE  2  TO  FLG-TEISHO
                   MOVE  1  TO  FLG-TNKRRK
                 END-IF
                 PERFORM  800-TNKRRK-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       200114-TEINEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
      *    ＣＳＶ<バッチ表>処理
           PERFORM  310-CSV-HEAD-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *    ＣＳＶ用データ編集処理
             PERFORM  3001-CSVDATA-HEN-SEC
           
      *    ＣＳＶ<合計行>処理
             PERFORM  320-CSV-GOKEI-SEC
      *    ＣＳＶ<明細行>処理
             PERFORM  330-CSV-MEI-SEC
           END-PERFORM
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ用データ編集処理
      *****************************************************************
       3001-CSVDATA-HEN-SEC      SECTION.
      *
           INITIALIZE                        SUM-AREA
                                             SUM-MEISAI
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  10
                    OR       FLG-PRINT  =  1
      *
      *      連番（ＣＳＶに使わない）
             MOVE  IDX1                      TO  SUM-RENNUM(IDX1)
      *      受給者番号
             MOVE  MF100-KOHJKYSNUM          TO  WRK-MAE-INPUT
             PERFORM 30012-HYPHEN-ERASE-SEC
             MOVE  WRK-OUT-INPUT             TO  SUM-JKYSNUM(IDX1)(2:)
      *      入外区分
             MOVE  MF100-NYUGAIKBN           TO  SUM-NYUGAIKBN(IDX1)
      *      社国退後区分
             EVALUATE  MF100-HKNNUM
               WHEN "060"
                 MOVE    "2"                 TO  SUM-HKNKBN(IDX1)
               WHEN "067"
                 MOVE    "3"                 TO  SUM-HKNKBN(IDX1)
               WHEN "039"
                 MOVE    "4"                 TO  SUM-HKNKBN(IDX1)
               WHEN OTHER
                 MOVE    "1"                 TO  SUM-HKNKBN(IDX1)
             END-EVALUATE
      *      前期高齢者
             IF  LNK-PRTKANRI-SRYYM  >=  CONS-SRYYM-201405
               IF  MF100-AGE        >=  70
               AND MF100-HKNNUM  NOT =  "039"
      *          平成26年4月2日以降に70歳を迎える人は「2」とする
                 IF  MF100-BIRTHDAY  >=  CONS-BIRTHDAY-19440402
                       EVALUATE    MF100-KYURATE
                           WHEN    30
                               MOVE    "1"         TO  SUM-KOREI(IDX1)
                           WHEN    20
                               MOVE    "2"         TO  SUM-KOREI(IDX1)
                       END-EVALUATE
                 ELSE
                     MOVE    "1"             TO  SUM-KOREI(IDX1)
                 END-IF
               END-IF
             ELSE
               IF  MF100-AGE           >=  70
               AND MF100-HKNNUM     NOT =  "039"
                   MOVE    "1"               TO  SUM-KOREI(IDX1)
               END-IF
             END-IF
      *      長期
             IF  MF100-TYOKI  NOT =  ZERO
               MOVE    MF100-TYOKI           TO  SUM-TYOKI(IDX1)
             END-IF
      *      点数、公費負担点数
             EVALUATE  MF100-KYURATE
               WHEN  30
                 MOVE    MF100-TOTALTEN      TO  SUM-TENSU(IDX1 1)
                 MOVE    MF100-KOHTENSU      TO  SUM-KOHTENSU(IDX1 1)
               WHEN  20
                 MOVE    MF100-TOTALTEN      TO  SUM-TENSU(IDX1 2)
                 MOVE    MF100-KOHTENSU      TO  SUM-KOHTENSU(IDX1 2)
               WHEN  10
                 MOVE    MF100-TOTALTEN      TO  SUM-TENSU(IDX1 3)
                 MOVE    MF100-KOHTENSU      TO  SUM-KOHTENSU(IDX1 3)
             END-EVALUATE
      *      患者負担額
             MOVE  MF100-ITBFTN              TO  SUM-ITBFTN(IDX1)
      *      診療年月（月遅れ）
             IF  MF100-SRYYM  NOT  =  LNK-PRTKANRI-SRYYM
               MOVE    MF100-SRYYM           TO  WRK-SYMD (1:6)
               MOVE    "01"                  TO  WRK-SYMD (7:2)
               PERFORM 31011-SEIWA-HEN-SEC
               MOVE    LNK-DAY1-WYMD(2:4)    TO  SUM-SRYYM(IDX1)
             END-IF
      *      カナ氏名
             MOVE    MF100-NAME              TO  WRK-MAE-INPUT
      *      性と名の間のスペース除去
             PERFORM 30011-SPACE-ERASE-SEC
             MOVE    WRK-OUT-INPUT           TO  WRK-KANANAME
             INITIALIZE                          WRK-MAE-INPUT
                                                 WRK-OUT-INPUT
             MOVE    WRK-KANANAME(1:42)      TO  WRK-MAE-INPUT
             PERFORM 2008-HENKAN-SEC
             MOVE    WRK-OUT-INPUT           TO  SUM-KANANAME(IDX1)
             COMPUTE WRK-NAGASA-SPACE = ( ( IDX3 - IDX4 ) - 1 ) / 2 
             IF  WRK-NAGASA-SPACE  >   21
                 MOVE    21                  TO  WRK-NAGASA-SPACE
             END-IF
             MOVE    WRK-NAGASA-SPACE        TO  SUM-NAGASA(IDX1)
      *      合計・小計加算
             EVALUATE  MF100-KYURATE
               WHEN  30
                 ADD    1                    TO  SUM-GKENSU(1)
                 ADD    MF100-TOTALTEN       TO  SUM-GTENSU(1)
                 ADD    MF100-KOHTENSU       TO  SUM-GKOHTENSU(1)
               WHEN  20
                 ADD    1                    TO  SUM-GKENSU(2)
                 ADD    MF100-TOTALTEN       TO  SUM-GTENSU(2)
                 ADD    MF100-KOHTENSU       TO  SUM-GKOHTENSU(2)
               WHEN  10
                 ADD    1                    TO  SUM-GKENSU(3)
                 ADD    MF100-TOTALTEN       TO  SUM-GTENSU(3)
                 ADD    MF100-KOHTENSU       TO  SUM-GKOHTENSU(3)
             END-EVALUATE
             ADD    MF100-ITBFTN             TO  SUM-GITBFTN
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
      *
           END-PERFORM
      *
           .
       3001-CSVDATA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スペース除去処理
      *****************************************************************
       30011-SPACE-ERASE-SEC      SECTION.
      *
           INITIALIZE                               WRK-ETC-INPUT
      *
           IF  WRK-MAE-INPUT  =  SPACE
             CONTINUE
           ELSE
             PERFORM  VARYING  IDW  FROM  1  BY  2
                      UNTIL    IDW  >     100
      *        N番目にスペースがある場合、スペースを消して詰める
               IF  WRK-MAE-INPUT(IDW:2)  =  "　"
                 COMPUTE  IDY  =  WRK-NAGASA  -  IDW
                 STRING  WRK-MAE-INPUT(1:IDW - 1)  DELIMITED BY SIZE
                         WRK-MAE-INPUT(IDW + 2:)   DELIMITED BY SIZE
                                                   INTO  WRK-ETC-INPUT
                 END-STRING
                 MOVE  WRK-ETC-INPUT  TO  WRK-MAE-INPUT
               END-IF
             END-PERFORM
           END-IF
      *
           MOVE  WRK-MAE-INPUT                  TO  WRK-OUT-INPUT
      *
           .
       30011-SPACE-ERASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ハイフン除去処理
      *****************************************************************
       30012-HYPHEN-ERASE-SEC      SECTION.
      *
           INITIALIZE                             WRK-OUT-INPUT
      *
           MOVE    1                          TO  IDW2
      *
           IF  WRK-MAE-INPUT  =  SPACE
               CONTINUE
           ELSE
             PERFORM  VARYING  IDW  FROM  1  BY  1
                      UNTIL    IDW  >     15
      *        N番目にハイフンがある場合、ハイフンを消して詰める
               IF  WRK-MAE-INPUT(IDW:1)  =  "-"
                 CONTINUE
               ELSE
                 MOVE  WRK-MAE-INPUT(IDW:1)   TO  WRK-OUT-INPUT(IDW2:1)
                 ADD    1                     TO  IDW2
               END-IF
             END-PERFORM
           END-IF
      *
           .
       30011-HYPHEN-ERASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<バッチ表>処理
      *****************************************************************
       310-CSV-HEAD-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *    １．区分
           MOVE    "9"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．請求区分
           MOVE    "3"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．医療機関番号
           STRING  "21"                    DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN      DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   INTO                    WRK-DATA
           END-STRING
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．請求書枚数
      *    請求書のページ数を求める
           DIVIDE  CNT-MF100  BY  10
                   GIVING                      WRK-GPAGE
                   REMAINDER                   WRK-AMARI
           IF  WRK-AMARI  NOT  =  ZERO
             ADD    1                      TO  WRK-GPAGE
           END-IF
      *    ページ数
           MOVE    WRK-GPAGE               TO  WRK-NUM
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ５．医療機関照合値（医療機関番号下５桁×請求書枚数）
           MOVE    SYS-1001-HOSPCD(3:5)    TO  WRK-CALC
           COMPUTE WRK-HOSPSYOGO  =  WRK-CALC  *  WRK-GPAGE
           MOVE    WRK-HOSPSYOGO           TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    バッチ番号エリア（全てブランク）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ６．法制区分
           MOVE    "00"                    TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    119                     TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ７．処理年月
           MOVE    LNK-PRTKANRI-SKYYMD     TO  WRK-SYMD
           PERFORM  31011-SEIWA-HEN-SEC
           MOVE    LNK-DAY1-WYMD(1:5)      TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
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
       310-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<合計行>処理
      *****************************************************************
       320-CSV-GOKEI-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *    １．区分
           MOVE    "5"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．請求区分
           MOVE    "3"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．医療機関番号
           STRING  "21"                    DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN      DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   INTO                    WRK-DATA
           END-STRING
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．診療科（平成21年4月以前の総合病院のみ）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．７割−件数
           MOVE    SUM-GKENSU(1)           TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ６．７割−点数
           MOVE    SUM-GTENSU(1)           TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ７．７割−公費負担点数
           MOVE    SUM-GKOHTENSU(1)        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ８．８割−件数
           MOVE    SUM-GKENSU(2)           TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ９．８割−点数
           MOVE    SUM-GTENSU(2)           TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １０．８割−公費負担点数
           MOVE    SUM-GKOHTENSU(2)        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １１．９割−件数
           MOVE    SUM-GKENSU(3)           TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １２．９割−点数
           MOVE    SUM-GTENSU(3)           TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １３．９割−公費負担点数
           MOVE    SUM-GKOHTENSU(3)        TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １４．患者負担額
           MOVE    SUM-GITBFTN             TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １５．食事療養費−件数（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      * 
      *    １６．食事療養費−基準額（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １７．食事療養費−標準負担額（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １９．１０割−入院日数（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    3                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２０．１０割−入院件数（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２１．１０割−入院一部負担金（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２２．１０割−入院標準負担額（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２３．１０割−外来件数（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２４．１０割−外来一部負担額（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １８．薬剤一部負担額（未使用）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    23                      TO  WRK-NAGASA
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
       320-CSV-GOKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<明細行>処理
      *****************************************************************
       330-CSV-MEI-SEC      SECTION.
      *
           ADD     1                         TO  CNT-PAGE
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >  10
                    OR       SUM-RENNUM(IDX2)  =  ZERO
      *
             INITIALIZE                      WRK-REC
                                             WRK-REC-LENGTH
      *
      *    １．区分
             MOVE    "1"                     TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．請求区分
             MOVE    "3"                     TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．受給者番号
             MOVE    SUM-JKYSNUM(IDX2)       TO  WRK-DATA
             MOVE    15                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．入外区分
             MOVE    SUM-NYUGAIKBN(IDX2)     TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．社国退後区分
             MOVE    SUM-HKNKBN(IDX2)        TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ６．前期高齢者
             MOVE    SUM-KOREI(IDX2)         TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ７．長期
             MOVE    SUM-TYOKI(IDX2)         TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ８．７割−点数
             MOVE    SUM-TENSU(IDX2 1)       TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ９．７割−公費負担点数
             MOVE    SUM-KOHTENSU(IDX2 1)    TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １０．８割−点数
             MOVE    SUM-TENSU(IDX2 2)       TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １１．８割−公費負担点数
             MOVE    SUM-KOHTENSU(IDX2 2)    TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １２．９割−点数
             MOVE    SUM-TENSU(IDX2 3)       TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １３．９割−公費負担点数
             MOVE    SUM-KOHTENSU(IDX2 3)    TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １４．患者負担額
             MOVE    SUM-ITBFTN(IDX2)        TO  WRK-NUM
             MOVE    6                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    4                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １５．食事療養費−基準額（未使用）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １６．食事療養費−標準負担額（未使用）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    4                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２０．１０割−入院日数（未使用）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    2                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２１．１０割−入院一部負担金（未使用）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２２．１０割−入院標準負担額（未使用）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２３．１０割−外来一部負担額（未使用）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    Ｒ／Ｖエリア（全てブランク）
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    4                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １７．診療年月
             MOVE    SUM-SRYYM(IDX2)         TO  WRK-DATA
             MOVE    4                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １８．薬剤一部負担額
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    5                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １９．カナ氏名
             MOVE    SUM-KANANAME(IDX2)      TO  WRK-DATA
             COMPUTE WRK-NAGASA  =  21  +  SUM-NAGASA(IDX2)
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
             ADD    1                        TO  WRK-GKENSU
      *
           END-PERFORM
      *
           .
       330-CSV-MEI-EXT.
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
      *    西暦・和暦変換処理
      *****************************************************************
       31011-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           .
       31011-SEIWA-HEN-EXT.
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
           MOVE   CNT-PAGE        TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2113 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2113 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU2113 END ***"
      *
           IF  CNT-MF100  =  0
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
      *    ファイル名は「県番号＋点数表＋医療機関コード＋診療年月.txt」
           MOVE    LNK-PRTKANRI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                    TO  WRK-SYMD(7:2)
           PERFORM  31011-SEIWA-HEN-SEC
           STRING  "21"                    DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN      DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   LNK-DAY1-WYMD(2:4)      DELIMITED  BY  SIZE
                   ".txt"                  DELIMITED  BY  SIZE
                   INTO                    WRK-FILENAME
           END-STRING
           MOVE    WRK-FILENAME    TO  SFILESV-TO-DATA  (1)
           MOVE    WRK-GKENSU      TO  SFILESV-CNT-ALL  (1)
           MOVE    "福祉医療費ＣＳＶ"    TO  SFILESV-TITLE    (1)
           IF      WRK-GKENSU   >   ZERO
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
                   MOVE  1                 TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
      *           個別発行で患者番号指定の場合、対象判定を行う
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   MOVE    1                   TO  FLG-HEIYOU
                   IF  RECE02-FTNMONEY(1)      =   ZERO
                       MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                               TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-KOH-COMPFTN(IDX)
                                               TO  WRK-SEIKYUGK
                   END-IF
      *
                   EVALUATE  SYU-KOH-HKNNUM(IDX)
                       WHEN "010"
                           ADD SYU-TOTAL-HKNTEN
                                               TO  MF100-KOHTENSU
                       WHEN "051"
                       WHEN "091"
                           ADD      SYU-TOTAL-HKNTEN
                                               TO  MF100-KOHTENSU
                           MOVE   3            TO  MF100-TYOKI
                           ADD SYU-KOH-COMPFTN(IDX)
                                               TO  MF100-TOKUTEIFTN
                       WHEN "052"
                           ADD      SYU-TOTAL-HKNTEN
                                               TO  MF100-KOHTENSU
                           MOVE   2            TO  MF100-TYOKI
                           ADD SYU-KOH-COMPFTN(IDX)
                                               TO  MF100-TOKUTEIFTN
                       WHEN "015"
                       WHEN "016"
                       WHEN "021"
                       WHEN "079"
                           ADD      SYU-TOTAL-HKNTEN
                                               TO  MF100-KOHTENSU
                           MOVE   4            TO  MF100-TYOKI
                           ADD SYU-KOH-COMPFTN(IDX)
                                               TO  MF100-TOKUTEIFTN
                       WHEN "054"
                           ADD      SYU-TOTAL-HKNTEN
                                               TO  MF100-KOHTENSU
                           MOVE   5            TO  MF100-TYOKI
                           ADD SYU-KOH-COMPFTN(IDX)
                                               TO  MF100-TOKUTEIFTN
                   END-EVALUATE
      *
               END-IF
               IF  SYU-KOH-HKNNUM(IDX) =   "972"
                   MOVE    1           TO  MF100-TYOKI
                                           FLG-TYOKI
               END-IF
      *
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX) =   "974"
                   MOVE    1           TO  MF100-TYOKI
                   MOVE    2           TO  FLG-TYOKI
               END-IF
      *
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF   SYU-KOH-HKNNUM(IDX)     =  WRK-KOHNUM
                 IF     IDX                 =  1
                   IF  SYU-SYUCOMPFTN-ENTANI
                                         > SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO          TO  FLG-ERR
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO          TO  FLG-ERR
                   END-IF
                 END-IF
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-HEIYOU      =   ZERO
               IF  RECE02-FTNMONEY(1)  =   ZERO
                   IF  RECE02-RJNHKNNUM    =   SPACE
                       MOVE    SYU-SYUCOMPFTN-ENTANI
                                               TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-KOH-COMPFTN-ENTANI(1)
                                               TO  WRK-SEIKYUGK
                   END-IF
               ELSE
                   IF  RECE02-RJNHKNNUM    =   SPACE
                       MOVE  SYU-SYUCOMPFTN    TO  WRK-SEIKYUGK
                   ELSE
                       MOVE  SYU-KOH-COMPFTN(1)
                                               TO  WRK-SEIKYUGK
                   END-IF
               END-IF
           END-IF
      *
           ADD WRK-SEIKYUGK                    TO  WRK-SEIKYUGK-G
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
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    WRK-PATH       TO  MCP-PATHNAME
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
                 MOVE    "tbl_syunou"   TO  MCP-TABLE
                 MOVE    WRK-PATH       TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
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
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
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
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
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
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
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
      *    出力レコード処理（文字・固定長）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-DATA(1:WRK-NAGASA)    DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
           ADD     WRK-NAGASA               TO  WRK-REC-LENGTH
      *     
           INITIALIZE                      WRK-HENSYU-AREA
      *
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）9999999999.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NUM          =   ZERO
               MOVE    SPACE                    TO  WRK-DATA
               PERFORM 5004-MOJI-HENSYU-SEC
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
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
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
      *    低所得マスタ読込
      *****************************************************************
       800-TSYRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC   =   ZERO
               MOVE    ZERO                TO  FLG-TSYRRK
               MOVE    MCPDATA-REC         TO  TSYRRK-REC
           ELSE
               MOVE    1                   TO  FLG-TSYRRK
               INITIALIZE                  TSYRRK-REC
           END-IF
      *
           .
       800-TSYRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    低年金マスタ読込
      *****************************************************************
       800-TNKRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC   =   ZERO
               MOVE    ZERO                TO  FLG-TNKRRK
               MOVE    MCPDATA-REC         TO  TNKRRK-REC
           ELSE
               MOVE    1                   TO  FLG-TNKRRK
               INITIALIZE                  TNKRRK-REC
           END-IF
      *
           .
       800-TNKRRK-FETCH-EXT.
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
      *****************************************************************
      *    全角カナ→半角カナ変換処理
      *****************************************************************
       2008-HENKAN-SEC                 SECTION.
      *
           MOVE  1          TO  IDX3
           MOVE  ZERO       TO  IDX4
           PERFORM VARYING  IDX2  FROM  1  BY  2
                   UNTIL    IDX2  >   42
             EVALUATE   WRK-MAE-INPUT(IDX2:2)
               WHEN  "ア"
                 MOVE  "ｱ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "イ"
                 MOVE  "ｲ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ウ"
                 MOVE  "ｳ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "エ"
                 MOVE  "ｴ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "オ"
                 MOVE  "ｵ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "カ"
                 MOVE  "ｶ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "キ"
                 MOVE  "ｷ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ク"
                 MOVE  "ｸ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ケ"
                 MOVE  "ｹ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "コ"
                 MOVE  "ｺ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "サ"
                 MOVE  "ｻ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "シ"
                 MOVE  "ｼ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ス"
                 MOVE  "ｽ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "セ"
                 MOVE  "ｾ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ソ"
                 MOVE  "ｿ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "タ"
                 MOVE  "ﾀ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "チ"
                 MOVE  "ﾁ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ツ"
                 MOVE  "ﾂ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "テ"
                 MOVE  "ﾃ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ト"
                 MOVE  "ﾄ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ナ"
                 MOVE  "ﾅ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ニ"
                 MOVE  "ﾆ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヌ"
                 MOVE  "ﾇ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ネ"
                 MOVE  "ﾈ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ノ"
                 MOVE  "ﾉ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ハ"
                 MOVE  "ﾊ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヒ"
                 MOVE  "ﾋ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "フ"
                 MOVE  "ﾌ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヘ"
                 MOVE  "ﾍ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ホ"
                 MOVE  "ﾎ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "マ"
                 MOVE  "ﾏ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ミ"
                 MOVE  "ﾐ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ム"
                 MOVE  "ﾑ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "メ"
                 MOVE  "ﾒ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "モ"
                 MOVE  "ﾓ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヤ"
                 MOVE  "ﾔ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ユ"
                 MOVE  "ﾕ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヨ"
                 MOVE  "ﾖ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ラ"
                 MOVE  "ﾗ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "リ"
                 MOVE  "ﾘ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ル"
                 MOVE  "ﾙ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "レ"
                 MOVE  "ﾚ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ロ"
                 MOVE  "ﾛ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ワ"
                 MOVE  "ﾜ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヲ"
                 MOVE  "ｦ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ン"
                 MOVE  "ﾝ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ァ"
                 MOVE  "ｧ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ィ"
                 MOVE  "ｨ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ゥ"
                 MOVE  "ｩ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ェ"
                 MOVE  "ｪ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ォ"
                 MOVE  "ｫ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ャ"
                 MOVE  "ｬ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ュ"
                 MOVE  "ｭ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ョ"
                 MOVE  "ｮ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ッ"
                 MOVE  "ｯ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "１"
                 MOVE  "1"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "２"
                 MOVE  "2"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "３"
                 MOVE  "3"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "４"
                 MOVE  "4"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "５"
                 MOVE  "5"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "６"
                 MOVE  "6"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "７"
                 MOVE  "7"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "８"
                 MOVE  "8"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "９"
                 MOVE  "9"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "０"
                 MOVE  "0"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "　"
                 MOVE  " "  TO  WRK-OUT-INPUT(IDX3:1)
                 ADD   1    TO  IDX3
                 IF  WRK-MAE-INPUT(IDX2 - 2:2)   NOT  =   SPACE
                   IF  WRK-MAE-INPUT(IDX2 + 2:2)   NOT  =   SPACE
                       ADD   1    TO  IDX4
                   END-IF
                 END-IF
               WHEN  "ガ"
                 MOVE  "ｶﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ギ"
                 MOVE  "ｷﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "グ"
                 MOVE  "ｸﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゲ"
                 MOVE  "ｹﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゴ"
                 MOVE  "ｺﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ザ"
                 MOVE  "ｻﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ジ"
                 MOVE  "ｼﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ズ"
                 MOVE  "ｽﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゼ"
                 MOVE  "ｾﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゾ"
                 MOVE  "ｿﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ダ"
                 MOVE  "ﾀﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ヂ"
                 MOVE  "ﾁﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ヅ"
                 MOVE  "ﾂﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "デ"
                 MOVE  "ﾃﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ド"
                 MOVE  "ﾄﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "バ"
                 MOVE  "ﾊﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ビ"
                 MOVE  "ﾋﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ブ"
                 MOVE  "ﾌﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ベ"
                 MOVE  "ﾍﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ボ"
                 MOVE  "ﾎﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "パ"
                 MOVE  "ﾊﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ピ"
                 MOVE  "ﾋﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "プ"
                 MOVE  "ﾌﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ペ"
                 MOVE  "ﾍﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ポ"
                 MOVE  "ﾎﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ヴ"
                 MOVE  "ｳﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
             END-EVALUATE
           END-PERFORM
      *
           .
       2008-HENKAN-EXT.
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
