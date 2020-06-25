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
       PROGRAM-ID.     SEIKYU2603.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 京都・福祉及び子育て支援医療費請求書
      *                      (143,144,145,245)
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/05/18    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    07/08/24    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  長期上位（974）は長期と同様の
      *                                  処理をする
      *  01.00.02    内田    07/11/11    最終ページの合計欄は総合計とする
      *                                  全国公費との併用で複数行印字の場合、
      *                                  件数も複数カウントする
      *  01.00.03    内田    07/12/11    合計件数のバイト数増加（２→３）
      *  01.00.04    内田    08/01/25    「表示の確認」欄へ７９等の表示追加
      *  01.00.05    内田    08/05/28    合計件数の印字修正
      *  01.00.06    内田    08/06/18    前期高齢者の場合、患者負担額印字
      *  01.00.07    門間    08/09/25    SPA-AREA初期化
      *  01.00.08    吉川    08/10/17    患者負担額が上限額を超えた場合、
      *                                  負担額はその上限額までとする
      *  01.00.09    吉川    08/12/10    協会けんぽ対応
      *  01.00.10    内田    09/02/10    公費分患者負担額欄にて前期高齢者
      *                                  軽減特例措置で長期の場合の上限を考慮
      *  01.00.11    内田    09/03/25    ７５歳到達月の自己負担限度額の対応
      *                                  （該当者は公費分患者負担額欄印字）
      *  01.00.12    吉川    09/08/17    公費分患者負担額欄にて償還払いでも
      *                                  記載する負担額は上限額までとする
      *  01.00.13    吉川    09/09/03    帳票様式の変更に伴う修正
      *  01.00.14    吉川    09/09/15    様式統一に伴い、帳票名称変更
      *  01.00.15    吉川    10/08/13    対象公費番号をINIファイルから
      *                                  取得するようにする
      *  01.00.16    吉川    10/11/24    open-cobol1.0対応
      *  01.00.17    吉川    11/03/18    75歳到達月で公費分患者負担額が
      *                                  印字されない不具合を修正
      *  01.00.18    吉川    12/06/14    自己負担が上限額未満の場合
      *                                  １円単位で記載するように修正
      *  01.00.19    吉川    13/01/10    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.20    吉川    13/01/16    テーブルのprimary key変更対応
      *  01.00.21    吉川    13/08/08    プログラムオプション対応
      *  01.00.22    萩野    14/05/14    帳票様式の変更に伴う修正
      *  01.00.23    吉川    14/09/03    月途中で同じ保険種別の保険に
      *                                  変更した場合はまとめて印字する
      *  01.00.24    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.25    萩野    14/12/09    「公費分患者負担額」欄を1円単位で
      *                                  記載するように修正
      *  01.00.26    吉川    15/03/12    公費分患者負担額は入院の上限額で
      *                                  止まるように修正
      *  01.00.27    吉川    15/07/07    全国公費との分点は一行にまとめる
      *  01.00.28    吉川    15/12/07    医療機関編集情報への開設者名追加対応
      *  01.00.29    吉川    16/05/12    全国公費との分点は別行に分ける（元の仕様に戻す）
      *  01.00.30    吉川    16/06/14    全国公費との分点時、自己負担欄は
      *                                  各々の負担額を記載するように修正
      *  01.00.31    吉川    17/01/11    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.32    萩野    17/05/15    医療機関情報の取得の修正
      *  01.00.33    吉川    17/06/12    公費の助成がない場合は印字しない
      *                                  （全国公費併用時のみ）
      *  01.00.34    吉川    17/06/30    患者負担無しの全国公費を
      *                                  併用した場合のみ印字対象外とする
      *  01.00.35    萩野    17/08/10    平成２９年８月から高額療養費の限度額を変更。
      *  01.00.36    門間    17/10/05    SITEKBN変更（動作に変更なし）
      *  01.00.37    門間    18/12/04    修正履歴コメント削除
      *                                  合計件数を4桁へ増加
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
           COPY  "SEI2603.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    対象公費番号データ
           COPY  "TAISYO2601.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
           COPY  "SC2603.INC".
      *    一時退避用
       01  MF101-REC.
           COPY  "SEI2603.INC"
                   REPLACING  //MF100//
                   BY         //MF101//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100   PIC X(02).
           03  STS-RECE02  PIC X(02).
           03  STS-RECEERR  PIC X(02).
           03  STS-TAISYOKOHI PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-TSYRRK     PIC 9(01).
           03  FLG-TNKRRK     PIC 9(01).
           03  FLG-TEISYO     PIC 9(01).
           03  FLG-TAISYOKOHI PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z4                PIC ZZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z69               PIC ZZ,ZZ9.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z79               PIC ZZZ,ZZ9.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z99               PIC Z,ZZZ,ZZ9.
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-G       PIC X(01).
               05  WRK-BIRTH-YY      PIC Z9.
               05  WRK-BIRTH-P1      PIC X(01).
               05  WRK-BIRTH-MM      PIC Z9.
               05  WRK-BIRTH-P2      PIC X(01).
               05  WRK-BIRTH-DD      PIC Z9.
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
           03  WRK-SYUKEI-TBL.
               05   WRK-KOHTENSU-G   PIC 9(08).
               05   WRK-KOHTENSU     PIC 9(08).
               05   WRK-ETCKOHFTN-G1 PIC 9(08)V9(01).
               05   WRK-ETCKOHFTN-G2 PIC 9(08).
               05   WRK-ETCKOHFTN    PIC 9(08)V9(01).
           03  WRK-HKNJANUM          PIC X(06).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOBETUCITYNUM     PIC X(03).
           03  WRK-KEISAN            PIC 9(08).
           03  WRK-AMARI             PIC 9(08).
           03  WRK-SEIKYUGK-G        PIC 9(08)V9(01).
           03  WRK-SEIKYUGK          PIC 9(08)V9(01).
           03  WRK-SEIKYUGK2-G       PIC 9(08)V9(01).
           03  WRK-SEIKYUGK3-G       PIC 9(08)V9(01).
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH            PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *    集計エリア
       01  SUM-AREA.
           03  SUM-KENSU             PIC 9(04).
           03  SUM-TENSU             PIC 9(09).
           03  SUM-ETCKOHFTN         PIC 9(09).
           03  SUM-ITBFTN            PIC 9(09).
      *
      *    総合計エリア
       01  SUM-G-AREA.
           03  SUM-GKENSU            PIC 9(04).
           03  SUM-GTENSU            PIC 9(09).
           03  SUM-GETCKOHFTN        PIC 9(09).
           03  SUM-GITBFTN           PIC 9(09).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
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
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
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
      *    請求
           COPY    "CPRCF010.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE  SUM-G-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  TAISYO-DAT
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
           MOVE    "SEIKYU2603"    TO  JOB-PGID
      *     MOVE    "福祉医療費請求書"
           MOVE    "福祉及び子育て支援医療費請求書"
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
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU2603 SYS 1001 ERR ***"
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
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU2603 SYS 1002 ERR ***"
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
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
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
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU2603"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  TAISYO-KOHNUM(IDX)
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
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
      *
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
             INITIALIZE                            TBL-HIKAKU-KOHI
             MOVE    RECE02-KOHNUM(IDZ)        TO  TBL-HKOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  200112-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
               PERFORM 20011-SYUKEI-SEC
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
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  RECE02-ZAITAKU      TO  MF100-ZAITAKU
           MOVE  RECE02-RECESYUBETU  TO  MF100-RECESYUBETU
      *
      *    負担者番号
           MOVE  RECE02-KOHFTNJANUM(IDZ)   TO  MF100-FTNJANUM
      *    受給者番号
           MOVE  RECE02-KOHJKYSNUM(IDZ)    TO  MF100-JKYSNUM
      *    保険種別
           EVALUATE    RECE02-HKNNUM
               WHEN    "001"
               WHEN    "009"
                   MOVE    "1"       TO  MF100-HKNKBN
               WHEN    "002"
                   MOVE    "2"       TO  MF100-HKNKBN
               WHEN    "003"
               WHEN    "004"
                   MOVE    "3"       TO  MF100-HKNKBN
               WHEN    "006"
               WHEN    "063"
                   MOVE    "5"       TO  MF100-HKNKBN
               WHEN    "007"
                   MOVE    "6"       TO  MF100-HKNKBN
               WHEN    OTHER
                   MOVE    "4"       TO  MF100-HKNKBN
           END-EVALUATE
           MOVE    1                 TO  MF100-IPANCD
      *
      *    集計ファイル読込
           PERFORM  900-SKTTMP-READ-SEC
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           COMPUTE IDY     =       IDZ     +   1
           ADD  RECE02-JNISSU       (IDY)  TO  MF100-JNISSU
           ADD  RECE02-TOTALTEN     (IDY)  TO  MF100-TOTALTEN
           ADD  RECE02-SHOKUJINISSU (IDY)  TO  MF100-SHOKUJINISSU
           ADD  RECE02-SHOKUJIRYOYOHI(IDY) TO  MF100-SHOKUJIRYOYOHI
           ADD  RECE02-SHOKUJIFTN   (IDY)  TO  MF100-SHOKUJIFTN
      *
      *    個別発行用市町村番号
           MOVE  MF100-KOHFTNJANUM(5:3)     TO  WRK-KOBETUCITYNUM
      *
      *    保険者番号チェック処理
           MOVE    ZERO               TO   FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *    全国公費の点数、一部負担金集計
           INITIALIZE                    WRK-SYUKEI-TBL
                                         WRK-SEIKYUGK-G
                                         WRK-SEIKYUGK2-G
                                         WRK-SEIKYUGK3-G
           MOVE    MF100-KOHNUM    TO    WRK-KOHNUM
           MOVE    MF100-KOHID     TO    WRK-KOHID
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"                 TO  WRK-PATH
           ELSE
               MOVE    "key25"                 TO  WRK-PATH
           END-IF
           MOVE     1                      TO   FLG-ERR
           PERFORM  920-SYUNOU-SET-SEC
      *
           COMPUTE  WRK-ETCKOHFTN-G2  =  WRK-ETCKOHFTN-G1 + 0.5
      *    結核を併用している場合は１０円単位に変換する
           IF      MF100-KEKKAKUFLG    =   1   AND
                   WRK-ETCKOHFTN-G2    >   ZERO
               DIVIDE  WRK-ETCKOHFTN-G2    BY  10
                       GIVING              WRK-KEISAN
                       REMAINDER           WRK-AMARI
               COMPUTE WRK-ETCKOHFTN-G2    =   WRK-ETCKOHFTN-G2
                                           -   WRK-AMARI
               IF      WRK-AMARI       >=  5
                   ADD 10              TO  WRK-ETCKOHFTN-G2
               END-IF
           END-IF
      *
      *    保険に限度額が有る場合の対応
           IF  RECE02-FTNMONEY(1)  >   ZERO  AND
               RECE02-FTNMONEY(1)  <   WRK-ETCKOHFTN-G2
             MOVE  RECE02-FTNMONEY(1)      TO  WRK-ETCKOHFTN-G2
           END-IF
      *
      *    前期高齢者軽減特例措置は患者負担額印字
           IF  WRK-ETCKOHFTN-G2  =  ZERO
             IF  RECE02-FLG (2)  =  1
               MOVE  MF100-TOTALTEN    TO  MF100-ETCKOHFTN
      *      長期上限考慮
               PERFORM  900-SEIKYU-SET-SEC
               IF  RECE10-ETC-KOHNUM (1)  =  "972"
               OR  RECE10-ETC-KOHNUM (2)  =  "972"
               OR  RECE10-ETC-KOHNUM (3)  =  "972"
               OR  RECE10-ETC-KOHNUM (4)  =  "972"
                 IF  MF100-ETCKOHFTN  >  10000
                     MOVE  10000          TO  MF100-ETCKOHFTN
                 END-IF
               END-IF
               IF  RECE10-ETC-KOHNUM (1)  =  "974"
               OR  RECE10-ETC-KOHNUM (2)  =  "974"
               OR  RECE10-ETC-KOHNUM (3)  =  "974"
               OR  RECE10-ETC-KOHNUM (4)  =  "974"
                 IF  MF100-ETCKOHFTN  >  20000
                     MOVE  20000          TO  MF100-ETCKOHFTN
                 END-IF
               END-IF
               IF  RECE02-FTNMONEY(1)  >   ZERO
               AND RECE02-FTNMONEY(1)  <  MF100-ETCKOHFTN
                   MOVE    RECE02-FTNMONEY(1)  TO  MF100-ETCKOHFTN
               END-IF
             END-IF
           END-IF
      *    自己負担額
           ADD    WRK-SEIKYUGK-G           TO  MF100-FTNMONEY
      *
      *    公費を併用している場合、公費用ファイルを書き出す
           IF      WRK-KOHTENSU-G          >   ZERO
      *
      *    元データ一時退避
               MOVE    MF100-REC           TO  MF101-REC
      *    公費併用点数、患者負担額を設定
               MOVE   WRK-KOHTENSU-G       TO  MF100-TOTALTEN
               MOVE   WRK-ETCKOHFTN-G2     TO  MF100-ETCKOHFTN
      *
      *    全国公費の点数と地方公費の点数が同じ場合はレコードを分けない
               IF      WRK-KOHTENSU-G      =   MF101-TOTALTEN
                   MOVE    1               TO  MF100-IPANCD
               ELSE
      *
                   MOVE    ZERO            TO  MF100-IPANCD
                   MOVE    ZERO            TO  MF100-FTNMONEY
      *
                   MOVE    WRK-SEIKYUGK2-G TO  MF100-FTNMONEY
      *
                   IF  FLG-ERR    =    ZERO
                       WRITE   MF100-REC
                       ADD     1   TO  CNT-MF100
                   ELSE
                       DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                               "公費番号="            MF100-KOHNUM
                               "診療年月="            MF100-SRYYM
                   END-IF
      *    退避していた地方公費の元データを再設定
                   INITIALIZE      MF100-REC
                   MOVE    MF101-REC       TO  MF100-REC
                   MOVE    1               TO  MF100-IPANCD
                   MOVE    ZERO            TO  MF100-KEKKAKUFLG
                                               MF100-SEISHINFLG
                   MOVE    WRK-SEIKYUGK3-G TO  MF100-FTNMONEY
      *
      *    地方公費の点数から全国公費の分点数を差し引く
                   COMPUTE MF100-TOTALTEN  =   MF100-TOTALTEN
                                           -   WRK-KOHTENSU-G
      *    前期高齢者軽減特例措置は患者負担額印字
                   IF  RECE02-FLG (2)  =  1
                     MOVE  MF100-TOTALTEN  TO  MF100-ETCKOHFTN
      *      長期上限考慮
                     PERFORM  900-SEIKYU-SET-SEC
                     IF  RECE10-ETC-KOHNUM (1)  =  "972"
                     OR  RECE10-ETC-KOHNUM (2)  =  "972"
                     OR  RECE10-ETC-KOHNUM (3)  =  "972"
                     OR  RECE10-ETC-KOHNUM (4)  =  "972"
                       IF  MF100-ETCKOHFTN  >  10000
                           MOVE  10000          TO  MF100-ETCKOHFTN
                       END-IF
                     END-IF
                     IF  RECE10-ETC-KOHNUM (1)  =  "974"
                     OR  RECE10-ETC-KOHNUM (2)  =  "974"
                     OR  RECE10-ETC-KOHNUM (3)  =  "974"
                     OR  RECE10-ETC-KOHNUM (4)  =  "974"
                       IF  MF100-ETCKOHFTN  >  20000
                           MOVE  20000          TO  MF100-ETCKOHFTN
                       END-IF
                     END-IF
                     IF  RECE02-FTNMONEY(1)  >   ZERO
                     AND RECE02-FTNMONEY(1)  <  MF100-ETCKOHFTN
                         MOVE    RECE02-FTNMONEY(1)  TO  MF100-ETCKOHFTN
                     END-IF
                   END-IF
                   MOVE    SPACE           TO  MF100-ETCKOHNUM
               END-IF
           ELSE
               MOVE    1                   TO  MF100-IPANCD
           END-IF
      *
      *    ７５歳到達月の自己負担限度額の対応
           IF   RECE02-FLG (3)  =  "1" OR "2" OR "3"
             IF  MF100-ETCKOHFTN  =  ZERO
                 MOVE  RECE02-FTNMONEY(1)  TO  MF100-ETCKOHFTN
             ELSE
               IF   RECE02-FTNMONEY(1)  <  MF100-ETCKOHFTN
               AND  RECE02-FTNMONEY(1)  >  ZERO
               AND  MF100-ETCKOHFTN     NOT = ZERO
                 MOVE  RECE02-FTNMONEY(1)  TO  MF100-ETCKOHFTN
               END-IF
             END-IF
           END-IF
      *
      *    70歳以上外来（在宅無し）で償還払いとなる場合でも上限額を考慮
           IF   RECE02-AGE        >=  70
           AND  RECE02-NYUGAIKBN  =   "2"
           AND  RECE02-ZAITAKU    =   SPACE
      *
             IF  RECE02-FLG (2)  =  1
               IF  MF100-ETCKOHFTN  >  14000
                 MOVE  14000              TO  MF100-ETCKOHFTN
               END-IF
      *
      *        低所得・低年金チェック
               MOVE  ZERO                   TO  FLG-TEISYO
               PERFORM  200112-TEISYO-CHK-SEC
               PERFORM  200113-TEINEN-CHK-SEC
               IF  FLG-TEISYO  =  1
                 IF  MF100-ETCKOHFTN  >  8000
                   MOVE  8000               TO  MF100-ETCKOHFTN
                 END-IF
               END-IF
             END-IF
           END-IF
      *    特記事項チェック
           PERFORM  900-SEIKYU-SET-SEC
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     5
               EVALUATE  RECE10-TOKKI(IDX1)
                 WHEN  "01"
                   MOVE  1                 TO  MF100-TOKKI
                 WHEN  "02"
                   MOVE  1                 TO  MF100-TYOKI
                 WHEN  "16"
                   MOVE  2                 TO  MF100-TYOKI
               END-EVALUATE
           END-PERFORM
      *
           IF  FLG-ERR    =    ZERO
             IF  FLG-MF100  =  1
                 WRITE   MF100-REC
                 ADD     1       TO  CNT-MF100
             ELSE
                 DISPLAY "上書き"
                 REWRITE  MF100-REC
             END-IF
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                     "公費番号="            MF100-KOHNUM
                     "診療年月="            MF100-SRYYM
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
           MOVE    "26"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF100-PTNUM
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
      *
                   DISPLAY  "TBL-PARA-HKNJANUM = "
                                               TBL-PARA-HKNJANUM(IDXX)
      *
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
       200112-TEISYO-CHK-SEC                SECTION.
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
               PERFORM  UNTIL  FLG-TSYRRK  =  1
                 IF  RECE02-SRYYM  >=  TSYRRK-NINSTYMD(1:6)  AND
                     RECE02-SRYYM  <=  TSYRRK-NINEDYMD(1:6)
                   MOVE  1    TO  FLG-TEISYO
                   MOVE  1    TO  FLG-TSYRRK
                 END-IF
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
       200112-TEISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低年金チェック処理
      *****************************************************************
       200113-TEINEN-CHK-SEC                SECTION.
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
               PERFORM  UNTIL  FLG-TNKRRK  =  1
                 IF  RECE02-SRYYM  >=  TNKRRK-STYMD(1:6)  AND
                     RECE02-SRYYM  <=  TNKRRK-EDYMD(1:6)
                   MOVE  1    TO  FLG-TEISYO
                   MOVE  1    TO  FLG-TNKRRK
                 END-IF
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
       200113-TEINEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       200112-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       TAISYO-KOHNUM(IDXX)  =  SPACE
             IF  TAISYO-KOHNUM(IDXX)  =  TBL-HKOHNUM(1)
                                     OR  TBL-HKOHNUM(2)
                                     OR  TBL-HKOHNUM(3)
                                     OR  TBL-HKOHNUM(4)
               MOVE     1                   TO  FLG-TAISYO
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       200112-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
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
           INITIALIZE                             SC03
           ADD     1       TO      CNT-PAGE
      *
      *    請求年月
           MOVE    WRK-SRYYMWH              TO  SC03-PRTYM
      *
      *    印刷日付
           MOVE    WRK-SEIYMDWH             TO   SC03-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD          TO  SC03-HOSPCD
      *    住所
           MOVE    WRK-HOSPADRS             TO  SC03-ADRS
           INSPECT SC03-ADRS                REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME             TO  SC03-HOSPNAME
           INSPECT SC03-HOSPNAME            REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME          TO  SC03-KAISETUNAME
      *    点数表区分
           MOVE    "◯"                     TO  SC03-TENSUKBN(1)
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
           INITIALIZE  SUM-AREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   FLG-PRINT       =   1   OR
                   IDX1                    >   25
      *
      *    氏名・カナ氏名
               MOVE    MF100-NAME          TO  SC03-NAME(IDX1)
               MOVE    MF100-KANANAME      TO  SC03-KANANAME(IDX1)
      *
               IF  ( MF100-RECESYUBETU(4:1)  =  7  OR  8 )
      *          生年月日
                 MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
                 PERFORM 31012-SEIWA-HEN-SEC
                 MOVE    WRK-BIRTH-YY        TO  SC03-BIRTHDAY-Y(IDX1)
                 MOVE    WRK-BIRTH-MM        TO  SC03-BIRTHDAY-M(IDX1)
                 MOVE    WRK-BIRTH-DD        TO  SC03-BIRTHDAY-D(IDX1)
               END-IF
      *
      *    保険種別
               EVALUATE    MF100-HKNNUM
                   WHEN    "001"
                   WHEN    "009"
                       MOVE    "◯"        TO  SC03-HKNKBN(IDX1 1)
                   WHEN    "002"
                       MOVE    "◯"        TO  SC03-HKNKBN(IDX1 2)
                   WHEN    "003"
                   WHEN    "004"
                       MOVE    "◯"        TO  SC03-HKNKBN(IDX1 3)
                   WHEN    "006"
                   WHEN    "063"
                       MOVE    "◯"        TO  SC03-HKNKBN(IDX1 5)
                   WHEN    "007"
                       MOVE    "◯"        TO  SC03-HKNKBN(IDX1 6)
                   WHEN    OTHER
                       MOVE    "◯"        TO  SC03-HKNKBN(IDX1 4)
               END-EVALUATE
      *
      *    本人家族区分
               MOVE    MF100-RECESYUBETU(4:1)
                                           TO  SC03-HONKZKKBN(IDX1)
      *
      *    負担者番号
               MOVE    MF100-FTNJANUM      TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:4)  TO  SC03-FTNJANUM1(IDX1)
               MOVE    WRK-OUT-INPUT(9:8)  TO  SC03-FTNJANUM2(IDX1)
      *
      *    受給者番号
               MOVE    MF100-JKYSNUM       TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC03-JKYSNUM(IDX1)
      *
      *    診療年月
               MOVE    MF100-SRYYM         TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3(5:4)
                                           TO  SC03-SRYYM(IDX1)(1:4)
               MOVE    LNK-DAY2-EDTYMD3(11:4)
                                           TO  SC03-SRYYM(IDX1)(5:4)
      *
      *    点数
               MOVE    MF100-TOTALTEN      TO  WRK-Z99
               MOVE    WRK-Z99             TO  SC03-TENSU(IDX1)
      *
      *    公費分負担額
               MOVE    MF100-ETCKOHFTN     TO  WRK-Z6
               MOVE    WRK-Z6              TO  SC03-KSFTN(IDX1)
      *
      *    自己負担
               MOVE    MF100-FTNMONEY      TO  WRK-Z69
               MOVE    WRK-Z69             TO  SC03-ITBFTN(IDX1)
      *    公長表示の確認
               IF  MF100-ETCKOHNUM  NOT =  SPACE
                   MOVE  MF100-ETCKOHNUM(2:2)  TO  SC03-SMOJI(IDX1)
                   MOVE  "○"                  TO  SC03-SMARU(IDX1)
               END-IF
      *        高額療養費
               IF  MF100-TOKKI  =  1
                   IF  MF100-ETCKOHNUM  =  SPACE
                       MOVE  "公"      TO  SC03-SMOJI(IDX1)
                       MOVE  "◯"      TO  SC03-SMARU(IDX1)
                   ELSE
                       MOVE  "公"      TO  SC03-KMOJI(IDX1)
                       MOVE  "◯"      TO  SC03-KMARU(IDX1)
                   END-IF
               END-IF
      *        長期
               IF      MF100-TYOKI         =   1
                   MOVE    "長"        TO  SC03-KMOJI(IDX1)
                   MOVE    "◯"        TO  SC03-KMARU(IDX1)
               END-IF
               IF      MF100-TYOKI         =   2
                   MOVE    "長２"      TO  SC03-KMOJI(IDX1)
                   MOVE    "◯"        TO  SC03-KMARU(IDX1)
               END-IF
      *
      *    合計加算
      *    全国公費分のデータは件数としてカウントしない
               ADD     1                   TO  SUM-KENSU
                                               SUM-GKENSU
      *    点数加算
               ADD     MF100-TOTALTEN      TO  SUM-TENSU
                                               SUM-GTENSU
      *    公費分患者負担額加算
               ADD     MF100-ETCKOHFTN     TO  SUM-ETCKOHFTN
                                               SUM-GETCKOHFTN
      *    自己負担額加算
               ADD     MF100-FTNMONEY      TO  SUM-ITBFTN
                                               SUM-GITBFTN
      *    TMPファイル読み込み
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
      *    合計編集（最終ページは総合計を印字）
           IF  FLG-PRINT  =  1
      *    合計件数
             MOVE    SUM-GKENSU              TO  WRK-Z4
             MOVE    WRK-Z4                  TO  SC03-GKENSU
      *
      *    合計点数
             MOVE    SUM-GTENSU              TO  WRK-Z99
             MOVE    WRK-Z99                 TO  SC03-GTENSU
      *
      *    合計公費分患者負担額
             MOVE    SUM-GETCKOHFTN          TO  WRK-Z7
             MOVE    WRK-Z7                  TO  SC03-GKSFTN
      *
      *    合計自己負担
             MOVE    SUM-GITBFTN             TO  WRK-Z79
             MOVE    WRK-Z79                 TO  SC03-GITBFTN
      *
           ELSE
      *    合計件数
             MOVE    SUM-KENSU               TO  WRK-Z4
             MOVE    WRK-Z4                  TO  SC03-GKENSU
      *
      *    合計点数
             MOVE    SUM-TENSU               TO  WRK-Z99
             MOVE    WRK-Z99                 TO  SC03-GTENSU
      *
      *    合計公費分患者負担額
             MOVE    SUM-ETCKOHFTN           TO  WRK-Z7
             MOVE    WRK-Z7                  TO  SC03-GKSFTN
      *
      *    合計自己負担
             MOVE    SUM-ITBFTN              TO  WRK-Z79
             MOVE    WRK-Z79                 TO  SC03-GITBFTN
      *
           END-IF
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
           MOVE   "SC2603.red"         TO  SPRT-PRTID
           MOVE    "福祉及び子育て支援医療費請求書"
                                    TO  SPRT-TITLE
           MOVE    SC03             TO  SPRT-PRTDATA
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2603 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2603 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU2603 END ***"
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
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-BIRTHDAY
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO    WRK-ETCKOHFTN
                                 WRK-KOHTENSU
                                 FLG-HEIYOU
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
      *
               IF      SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                       SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   EVALUATE  SYU-KOH-HKNNUM(IDX)
                       WHEN    "010"
                       WHEN    "015"
                       WHEN    "016"
                       WHEN    "021"
                       WHEN    "024"
                       WHEN    "079"
                           MOVE  SYU-KOH-HKNNUM(IDX)
                                           TO  MF100-ETCKOHNUM
                           IF  SYU-KOH-HKNNUM(IDX)  =  "010"
                               MOVE    1   TO  MF100-KEKKAKUFLG
                           END-IF
                   END-EVALUATE
      *
                   IF      RECE02-FTNMONEY(1)  NOT =   ZERO
                       MOVE    SYU-KOH-COMPFTN(IDX)
                                               TO  WRK-ETCKOHFTN
                   ELSE
                       MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                               TO  WRK-ETCKOHFTN
                   END-IF
                   MOVE    SYU-TOTAL-HKNTEN    TO  WRK-KOHTENSU
                   MOVE    1                   TO  FLG-HEIYOU
      *
      *            患者負担無しの全国公費は印字対象外とする
                   IF  (FLG-ERR                  =  1)
                   AND (SYU-KOH-COMPFTN-ENTANI(IDX)  NOT =  ZERO)
                       MOVE     ZERO           TO  FLG-ERR
                   END-IF
               END-IF
      *
               IF   SYU-KOH-HKNNUM(IDX)  =  WRK-KOHNUM
                   MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                               TO  WRK-SEIKYUGK
               END-IF
      *
           END-PERFORM
      *
      *    全国公費を助成していない場合は助成がなくても記載する
           IF  FLG-HEIYOU  =  ZERO
               MOVE     ZERO                   TO  FLG-ERR
           END-IF
      *
           ADD     WRK-ETCKOHFTN   TO  WRK-ETCKOHFTN-G1
           ADD     WRK-KOHTENSU    TO  WRK-KOHTENSU-G
           ADD     WRK-SEIKYUGK    TO  WRK-SEIKYUGK-G
      *
           IF   SYU-KOH-HKNNUM(1)(1:1)  =  "0"
           OR   SYU-KOH-HKNNUM(2)(1:1)  =  "0"
           OR   SYU-KOH-HKNNUM(3)(1:1)  =  "0"
           OR   SYU-KOH-HKNNUM(4)(1:1)  =  "0"
                ADD     WRK-SEIKYUGK    TO  WRK-SEIKYUGK2-G
           ELSE
                ADD     WRK-SEIKYUGK    TO  WRK-SEIKYUGK3-G
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
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       900-SKTTMP-READ-EXT.
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
               INITIALIZE                          TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)        TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  TBL-HKOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  200112-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
      *    テスト患者のときは読み飛ばす
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
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC        SECTION.
      *
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SET-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                     RECE10-REC
           MOVE  RECE02-KEY           TO  RECE10-KEY
      *
           MOVE  RECE10-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE  "tbl_seikyu"         TO  MCP-TABLE
           MOVE  "key"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC          =  ZERO
               MOVE  "DBFETCH"        TO  MCP-FUNC
               MOVE  "tbl_seikyu"     TO  MCP-TABLE
               MOVE  "key"            TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC             =   ZERO
                   MOVE    MCPDATA-REC    TO  RECE10-REC
               END-IF
           END-IF
      *
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE  "tbl_seikyu"         TO  MCP-TABLE
           MOVE  "key"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SEIKYU-SET-EXT.
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
      *
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"         TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"        TO  MCP-FUNC.
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
