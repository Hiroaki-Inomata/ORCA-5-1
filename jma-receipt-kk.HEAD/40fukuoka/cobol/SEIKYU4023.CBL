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
       PROGRAM-ID.     SEIKYU4023.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 福岡・特定疾病療養受領証保持者の
      *                      自己負担分相当額申請書
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/02/12    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/03/05  口座名義人追加
      *  01.00.02    吉川    2009/08/20  2度目の入院期間を印字する
      *                                  同一患者で入院と外来がある場合
      *                                  1行にまとめて印字する
      *                                  印字対象番号の追加（680）
      *  01.00.03    吉川    2010/12/16  open-cobol1.0対応
      *  01.00.04    吉川    2011/02/08  負担者番号毎に改頁するように修正
      *                                  請求金額合計は頁毎に計算する
      *  01.00.05    吉川    2011/05/17  月遅れ分は別帳票とする
      *                                  公費の助成がない場合は印字しない
      *  01.00.06    吉川    2011/12/07  印字対象番号の追加（250）
      *  01.00.07    吉川    2012/03/12  対象公費番号をINIファイルから
      *                                  取得するようにする
      *  01.00.08    吉川    2013/01/22  医療機関情報編集対応
      *  01.00.09    吉川    2013/02/07  プラグイン対応
      *  01.00.10    吉川    2013/09/04  プログラムオプション対応
      *  01.00.11    吉川    2014/04/07  口座名義人を３０文字まで印字対応
      *  01.00.12    吉川    2014/10/27  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.13    萩野    2015/11/16  医療機関編集情報への開設者名追加対応
      *  01.00.14    門間    2017/03/14  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.15    吉川    2017/10/16  SITEKBN変更
      *  01.00.16    門間    2019/01/10  修正履歴コメント削除
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
           COPY  "SEI4023.INC".
      *
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
           COPY    "CPRECEERR.INC".
      *
      *    対象公費番号データ
           COPY  "TAISYO4001.INC".
      *
           COPY  "SC4023.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100             PIC X(02).
           03  STS-RECE02            PIC X(02).
           03  STS-RECEERR           PIC X(02).
           03  STS-KINYU             PIC X(02).
           03  STS-TAISYOKOHI        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF             PIC 9(01).
           03  FLG-END               PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
           03  FLG-HEIYOU            PIC 9(01).
           03  FLG-MF100             PIC 9(01).
           03  FLG-TYOKI             PIC 9(01).
           03  FLG-PRINT             PIC 9(01).
           03  FLG-SYSKANRI          PIC 9(01).
           03  FLG-ERR               PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-HKNJAINF          PIC 9(01).
           03  FLG-PTNYUINRRK        PIC 9(01).
           03  FLG-BTPARA            PIC 9(01).
           03  FLG-SYORI             PIC 9(01).
           03  FLG-TAISYOKOHI        PIC 9(01).
           03  FLG-TAISYO            PIC 9(01).
           03  FLG-PRGOPTION         PIC 9(01).
           03  FLG-OK                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(05).
           03  IDXX                  PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDY                   PIC 9(04).
           03  IDZ                   PIC 9(04).
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
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZZ.
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
           03  TBL-MINNYMD.
               05  WRK-MINNYMD  OCCURS 2.
                   07  WRK-MINNYY    PIC 9(04).
                   07  WRK-MINNMM    PIC 9(02).
                   07  WRK-MINNDD    PIC 9(02).
           03  TBL-MINTYMD.
               05  WRK-MINTYMD  OCCURS 2.
                   07  WRK-MINTYY    PIC 9(04).
                   07  WRK-MINTMM    PIC 9(02).
                   07  WRK-MINTDD    PIC 9(02).
           03  WRK-NYUINKIKAN.
               05  WRK-NYUMM         PIC X(04).
               05  WRK-NYUDD         PIC X(02).
               05  WRK-TAIDD         PIC X(02).
           03  WRK-KASAN-TBL.
               05  WRK-FTNJANUM      PIC X(08).
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-KOHID         PIC 9(10).
               05  WRK-SEIKYUGK      PIC 9(09)V9(01).
               05  WRK-SEIKYUGK-G    PIC 9(09)V9(01).
               05  WRK-SEIKYUGK-G2   PIC 9(09).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-FTNJANUM2         PIC X(05).
           03  WRK-HKNCOMBINUM       PIC X(04).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
      *
      *    ファイル名称用エリア
           03  WRK-KINYUDATA         PIC X(60) OCCURS 5.
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
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
      *    キー領域
       01  KEY-AREA  VALUE  ZERO.
           03  KEY-NEW.
               05  KEY-N-SRYYM       PIC X(06).
               05  KEY-N-KBN         PIC X(01).
               05  KEY-N-CITYNUM     PIC X(03).
           03  KEY-OLD.
               05  KEY-O-SRYYM       PIC X(06).
               05  KEY-O-KBN         PIC X(01).
               05  KEY-O-CITYNUM     PIC X(03).
      *
      *    合計領域
       01  SUM-AREA.
           03  SUM-SEIKYUGK          PIC  9(10).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02            PIC 9(06).
           03  CNT-MF100             PIC 9(06).
           03  CNT-PAGE              PIC 9(03).
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
       01  WRK-CONS-PARA-MAX         PIC 9(03)   VALUE   100.
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
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
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
           MOVE    "SEIKYU4023"    TO  JOB-PGID
           MOVE    "特定疾病自己負担分相当額申請書"
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
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1001-STYUKYMD(7:2)
                                       SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =  ZERO
               MOVE    MCPDATA-REC TO  SYS-1001-REC
           ELSE
               DISPLAY "*** SEIKYU4023 SYS 1001 ERR ***"
               MOVE  1             TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1002-STYUKYMD(7:2)
                                       SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1002-REC
           ELSE
               DISPLAY "*** SEIKYU4023 SYS 1002 ERR ***"
               MOVE    1           TO  FLG-END
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
           MOVE    "SEIKYU4023"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    金融機関情報
           MOVE    "KINYU"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  5
             IF  WRK-OPTION(IDX)(1:2)  =  "※"
               MOVE    SPACE                 TO  WRK-KINYUDATA(IDX)
             ELSE
               MOVE    WRK-OPTION(IDX)(1:60) TO  WRK-KINYUDATA(IDX)
             END-IF
           END-PERFORM
           DISPLAY "銀行名 ="  WRK-KINYUDATA(1)
           DISPLAY "支店名 ="  WRK-KINYUDATA(2)
           DISPLAY "普・当 ="  WRK-KINYUDATA(3)
           DISPLAY "口座番号 ="  WRK-KINYUDATA(4)
           DISPLAY "口座名義人 =" WRK-KINYUDATA(5)
      *
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
                         OR   (IDX1  >  100)
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
           MOVE    ZERO                TO  IDXX
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
           IF       CNT-MF100   >       0
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
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(IDZ)        TO  TBL-HKOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
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
      *
           MOVE    ZERO    TO  FLG-ERR
           INITIALIZE       MF100-REC
      *
           MOVE    SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE    RECE02-PTNUM        TO  MF100-PTNUM
           MOVE    RECE02-RECEKA       TO  MF100-SRYKA
           MOVE    RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE    RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE    RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
           MOVE    RECE02-PTID         TO  MF100-PTID
           MOVE    RECE02-NAME         TO  MF100-NAME
           MOVE    RECE02-HKN          TO  MF100-HKN
           MOVE    RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE    RECE02-KOHINF   (IDZ)   TO  MF100-KOHINF
           COMPUTE IDY     =       IDZ     +   1
      *
      *    ソートKEY設定
           MOVE    MF100-KOHFTNJANUM(5:3)  TO  MF100-CITYNUM
           MOVE    MF100-KOHJKYSNUM        TO  MF100-JKYSNUM
      *
      *    福岡市と北九州市は代表番号セット
      *    北九州市
           IF  MF100-KOHFTNJANUM(5:2)  =  "40"
               MOVE  "400"                 TO  MF100-CITYNUM
           END-IF
      *    福岡市
           IF  MF100-KOHFTNJANUM(5:2)  =  "50"
               MOVE  "500"                 TO  MF100-CITYNUM
           END-IF
      *
      *    区分（1=一般、2=後期高齢）
           IF  MF100-HKNNUM  =  "039" OR "040"
               MOVE  "2"                   TO  MF100-KBN
           ELSE
               MOVE  "1"                   TO  MF100-KBN
           END-IF
      *
      *    一時ファイル読み込み
           PERFORM  900-SKTTMP-READ-SEC
      *
           MOVE    RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
      *    請求金額集計
           MOVE    MF100-KOHNUM        TO    WRK-KOHNUM
           MOVE    MF100-KOHID         TO    WRK-KOHID
           MOVE    ZERO                TO    WRK-SEIKYUGK-G
                                             FLG-TYOKI
           MOVE    1                   TO    FLG-ERR
      *
           IF  RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"         TO  WRK-PATH
               MOVE    1               TO  IDX2
           ELSE
               MOVE    "key25"         TO  WRK-PATH
               MOVE    2               TO  IDX2
           END-IF
      *
      *    収納テーブル取得
           PERFORM 920-SYUNOU-SET-SEC
      *
      *    入院患者の場合、入退院日をセット
           IF  RECE02-NYUGAIKBN = "1"
               PERFORM 2003-YMDSET-SEC
           END-IF
      *
           IF      RECE02-FTNMONEY(1)  >   ZERO
           AND     RECE02-FTNMONEY(1)  <   WRK-SEIKYUGK-G
               MOVE    RECE02-FTNMONEY(1)  TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    長期
           IF      FLG-TYOKI       =   1
           AND     WRK-SEIKYUGK-G  >   10000
               MOVE    10000       TO  WRK-SEIKYUGK-G
           END-IF
      *    長期上位
           IF      FLG-TYOKI       =   2
           AND     WRK-SEIKYUGK-G  >   20000
               MOVE    20000       TO  WRK-SEIKYUGK-G
           END-IF
      *
           COMPUTE WRK-SEIKYUGK-G2 =   WRK-SEIKYUGK-G  +   0.5
      *
      *    請求額
           COMPUTE WRK-SEIKYUGK-G2 =   WRK-SEIKYUGK-G2  -
                                       RECE02-FTNMONEY(IDY)
           MOVE    WRK-SEIKYUGK-G2     TO  MF100-SEIKYUGK(IDX2)
      *
      *    日数
           MOVE    RECE02-JNISSU(IDY)   TO  MF100-JNISSU(IDX2)
      *
      *    給付外金額
           MOVE    RECE02-FTNMONEY(IDY) TO  MF100-FTNMONEY(IDX2)
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF      FLG-ERR         =   ZERO
               IF      FLG-MF100  =  1
                   WRITE   MF100-REC
                   ADD     1       TO  CNT-MF100
               ELSE
                   REWRITE  MF100-REC
               END-IF
           ELSE
               DISPLAY "市町村番号対象外 " RECE02-PTNUM
                       "負担者番号="       RECE02-KOHFTNJANUM(IDZ)
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
           MOVE    "40"                 TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-CITYNUM        TO  WRK-HKNJANUM(3:3)
      *
           IF      WRK-PARA-SYORIFLG   =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
      *
      *    ４０４００、４０５００の代表番号が指定されている場合、
      *    福岡市内、北九州市内の全ての区を対象とする
      *    (実際に印字する市町村番号は東区、門司区の番号)
                   IF      (WRK-HKNJANUM(1:5)  =
                                       TBL-PARA-HKNJANUM(IDXX)(1:5))
                       OR  ((WRK-HKNJANUM(1:3) =   "404")  AND
                            (TBL-PARA-HKNJANUM(IDXX)(1:5) = "40400"))
                       OR  ((WRK-HKNJANUM(1:3) =   "405")  AND
                            (TBL-PARA-HKNJANUM(IDXX)(1:5) = "40500"))
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
      *    入退院日セット処理
      *****************************************************************
       2003-YMDSET-SEC  SECTION.
      *
      *    患者入院履歴情報
           INITIALIZE                  PTNYUINRRK-REC
                                       TBL-MINNYMD
                                       TBL-MINTYMD
      *
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE02-PTID          TO  PTNYUINRRK-PTID
           MOVE    WRK-LASTYMD          TO  WRK-NYMD
                                            WRK-TYMD
           MOVE    1                    TO  WRK-TDD
           MOVE    WRK-NYMD             TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-TYMD             TO  PTNYUINRRK-TAIINYMD
           MOVE    WRK-HKNCOMBINUM      TO  PTNYUINRRK-HKNCOMBINUM
      *
      *    入院履歴テーブル取得
           PERFORM  930-PTNYUINRRK-SELECT-SEC
           PERFORM  930-PTNYUINRRK-CLOSE-SEC
      *
      *    入院日セット
           IF    WRK-TYMD             >   WRK-MINNYMD(1)
             MOVE  WRK-TYMD             TO  MF100-NYUINYMD(1)
           ELSE
             MOVE  WRK-MINNYMD(1)       TO  MF100-NYUINYMD(1)
             MOVE  WRK-MINNYMD(2)       TO  MF100-NYUINYMD(2)
           END-IF
      *
      *    退院日セット
           IF  WRK-NYMD               <   WRK-MINTYMD(1)
             MOVE  WRK-NYMD             TO  MF100-TAIINYMD(1)
           ELSE
             MOVE  WRK-MINTYMD(1)       TO  MF100-TAIINYMD(1)
             MOVE  WRK-MINTYMD(2)       TO  MF100-TAIINYMD(2)
           END-IF
      *
           .
       2003-YMDSET-EXT.
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
               MOVE  KEY-NEW    TO  KEY-OLD
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
           INITIALIZE                      SC23
      *
      *    印刷日付
           MOVE    WRK-SEIYMDWH            TO  SC23-SEIYMD
      *
      *    市町村番号（公費負担者番号）
           INITIALIZE  WRK-MAE-INPUT
           MOVE    MF100-KOHFTNJANUM       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC23-CITYNUM
      *
      *    市町村名
           MOVE  "40"                      TO  WRK-HKNJANUM(1:2)
           MOVE  MF100-CITYNUM             TO  WRK-HKNJANUM(3:3)
           PERFORM  920-HKNJAINF-SET-SEC
           IF       FLG-HKNJAINF        =   ZERO
                    MOVE    HKNJA-HKNJANAME
                                           TO  SC23-CITYNAME
           ELSE
                    MOVE    SPACE          TO  SC23-CITYNAME
           END-IF
      *
      *    診療年月
           MOVE  MF100-SRYYM               TO  WRK-SYMD (1:6)
           MOVE  "01"                      TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG               TO  SC23-SRYYM
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCDN       TO  SC23-HOSPCDN
      *
      *    住所
           MOVE    WRK-HOSPADRS            TO  SC23-ADRS
           INSPECT SC23-ADRS               REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SC23-HOSPNAME
           INSPECT SC23-HOSPNAME           REPLACING ALL "  " BY "　"
      *
      *    代表者名（口座名義人）
           IF  WRK-KINYUDATA(5)  =  SPACE
             MOVE  WRK-KAISETUNAME         TO  SC23-KAISETU
           ELSE
             STRING  WRK-KAISETUNAME       DELIMITED  BY  SPACE
                     "（" WRK-KINYUDATA(5) DELIMITED  BY  SPACE
                     "）"                  DELIMITED  BY  SIZE
                                           INTO  SC23-KAISETU
             END-STRING
           END-IF
      *
      *    銀行名
           MOVE  WRK-KINYUDATA(1)          TO  SC23-GINKOU
      *
      *    支店名
           MOVE  WRK-KINYUDATA(2)          TO  SC23-SITEN
      *
      *    口座区分
           IF  WRK-KINYUDATA(3)(1:4)  =  "普通"
             MOVE  "◯"                    TO  SC23-KOUZAKBN(1)
           ELSE
             IF  WRK-KINYUDATA(3)(1:4)  =  "当座"
               MOVE  "◯"                  TO  SC23-KOUZAKBN(2)
             END-IF
           END-IF
      *
      *    口座番号
           MOVE  WRK-KINYUDATA(4)          TO  SC23-KOUZANUM
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >   4
                   OR  KEY-NEW     NOT =   KEY-OLD
                   OR  FLG-PRINT       =   1
      *
      *    保険者番号
               INITIALIZE  WRK-MAE-INPUT
               MOVE    MF100-HKNJANUM-KEY  TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC23-HKNJANUM(IDX1)
      *
      *    記号・番号
               STRING  MF100-KIGO  DELIMITED  BY  SPACE
                       MF100-NUM   DELIMITED  BY  SPACE
                       INTO       SC23-KIGONUM (IDX1)
               END-STRING
      *
      *    受給者番号
               INITIALIZE  WRK-MAE-INPUT
               MOVE    MF100-JKYSNUM       TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC23-JKYSNUM(IDX1)
      *
      *    氏名
               MOVE    MF100-NAME          TO  SC23-NAME(IDX1)
      *
      *    入院期間
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >  2
      *          2度目の入院はデータがある時のみ印字
                 IF   IDX2  =  2
                 AND  MF100-NYUINYMD(IDX2)  =  ZERO
                   CONTINUE
                 ELSE
      *            日付の編集
                   MOVE  MF100-NYUINMM(IDX2)  TO  WRK-Z2
                   MOVE  WRK-Z2               TO  WRK-NYUMM
                   MOVE  MF100-NYUINDD(IDX2)  TO  WRK-Z2
                   MOVE  WRK-Z2               TO  WRK-NYUDD
                   MOVE  MF100-TAIINDD(IDX2)  TO  WRK-Z2
                   MOVE  WRK-Z2               TO  WRK-TAIDD
      *
      *            結合(STRING命令はスペースが全角で入るので使用しない)
                   MOVE  WRK-NYUMM  TO  SC23-NYUINKIKAN(IDX1 IDX2)(1:2)
                   MOVE  "月"       TO  SC23-NYUINKIKAN(IDX1 IDX2)(3:2)
                   MOVE  WRK-NYUDD  TO  SC23-NYUINKIKAN(IDX1 IDX2)(5:2)
                   MOVE  "日〜"     TO  SC23-NYUINKIKAN(IDX1 IDX2)(7:4)
                   MOVE  WRK-TAIDD  TO  SC23-NYUINKIKAN(IDX1 IDX2)(11:2)
                   MOVE  "日"       TO  SC23-NYUINKIKAN(IDX1 IDX2)(13:2)
                 END-IF
               END-PERFORM
      *
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >  2
      *    日数
                 MOVE  MF100-JNISSU(IDX2)    TO  WRK-Z2
                 MOVE  WRK-Z2            TO  SC23-NISSU(IDX1 IDX2)
      *
      *    給付外金額
                 MOVE  MF100-FTNMONEY(IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9            TO  SC23-KYUFUGAIGK(IDX1 IDX2)
      *
      *    請求額
                 MOVE  MF100-SEIKYUGK(IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9            TO  SC23-SEIKYUGK(IDX1 IDX2)
               END-PERFORM
      *
      *    合計加算
               ADD  MF100-SEIKYUGK(1)    TO  SUM-SEIKYUGK
               ADD  MF100-SEIKYUGK(2)    TO  SUM-SEIKYUGK
      *
               PERFORM 900-TMP-READ-SEC
      *
           END-PERFORM
      *
      *    合計印字
             MOVE  SUM-SEIKYUGK          TO  WRK-Z10
             MOVE  WRK-Z10               TO  SC23-GSEIKYUGK
      *
             INITIALIZE                      SUM-AREA
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
           ADD      1         TO        CNT-PAGE
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
           MOVE   "SC4023.red"        TO  SPRT-PRTID
           MOVE    "特定疾病自己負担分相当額申請書"
                                    TO  SPRT-TITLE
           MOVE    SC23             TO  SPRT-PRTDATA
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
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
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
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
      *
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
           MOVE    SYU-PTID                TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
      *      計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF   SYU-KOH-HKNNUM(IDX)     =  WRK-KOHNUM
                 IF     IDX                 =  1
                   IF  SYU-SYUCOMPFTN-ENTANI
                                         > SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE  ZERO          TO  FLG-ERR
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE  ZERO          TO  FLG-ERR
                   END-IF
                 END-IF
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   MOVE    1       TO      FLG-HEIYOU
                   IF  RECE02-FTNMONEY(1)  =   ZERO
                       MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                           TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-KOH-COMPFTN(IDX)
                                           TO  WRK-SEIKYUGK
                   END-IF
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      FLG-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      FLG-TYOKI
               END-IF
      *
           END-PERFORM
      *
      *    上限が有る場合は計算負担金、ない場合は総点数×給付割合
      *    を加算
           IF  FLG-HEIYOU      =   ZERO
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
           END-IF
      *
           ADD    WRK-SEIKYUGK               TO    WRK-SEIKYUGK-G
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
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    RECE02-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    RECE02-PTID         TO  SYU-PTID
           MOVE    RECE02-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
           MOVE    "__"                TO  SYU-SRYYMD(7:2)
           MOVE    WRK-KOHNUM          TO  SYU-KOH-HKNNUM(1)
                                           SYU-KOH-HKNNUM(2)
                                           SYU-KOH-HKNNUM(3)
                                           SYU-KOH-HKNNUM(4)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
                       MOVE  SYU-HKNCOMBINUM      TO  WRK-HKNCOMBINUM
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
           MOVE  "DBFETCH"             TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF      MCP-RC      =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE      SYUNOU-REC
               MOVE    1   TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU4023 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU4023 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU4023 END ***"
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT AT  END
                   MOVE  MF100-SRYYM    TO  KEY-N-SRYYM
                   MOVE  MF100-KBN      TO  KEY-N-KBN
                   MOVE  MF100-CITYNUM  TO  KEY-N-CITYNUM
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
           MOVE    "DBFETCH"               TO  MCP-FUNC
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
                       PERFORM  900-KOHSKY-READ-SEC
                   END-IF
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >  4
                 INITIALIZE                            TBL-HIKAKU-KOHI
                 MOVE    RECE02-KOHNUM(IDX1)       TO  TBL-HKOHNUM(1)
      *          対象公費番号チェック処理
                 PERFORM  800-TAISYOKOHI-CHK-SEC
                 IF  FLG-TAISYO  =  1
                     MOVE  RECE02-KOHFTNJANUM (IDX1)(1:5)
                                     TO  WRK-FTNJANUM2
                     MOVE  4         TO  IDX1
                 END-IF
             END-PERFORM
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
                     PERFORM  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                 PERFORM  900-KOHSKY-READ-SEC
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
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
      *
           MOVE  SPA-HOSPNUM              TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM             TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC             TO  MCPDATA-REC
           MOVE  "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"       TO  MCP-TABLE
             MOVE    "key2"               TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
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
      *    入院履歴情報SELECT
      *****************************************************************
       930-PTNYUINRRK-SELECT-SEC         SECTION.
      *
           MOVE    PTNYUINRRK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key21"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           IF      MCP-RC           =   ZERO
                   PERFORM 930-PTNYUINRRK-FETCH-SEC
      *
      *その月の最小入退院日を取得（2件まで）
                   MOVE  PTNYUINRRK-NYUINYMD      TO  WRK-MINNYMD(1)
                   MOVE  PTNYUINRRK-TAIINYMD      TO  WRK-MINTYMD(1)
      *
                   PERFORM  UNTIL  FLG-PTNYUINRRK  =  1
      *              最小入院日が見つかったら並べ替え
                     IF  PTNYUINRRK-NYUINYMD  <  WRK-MINNYMD(1)
                       MOVE  WRK-MINNYMD(1)       TO  WRK-MINNYMD(2)
                       MOVE  WRK-MINTYMD(1)       TO  WRK-MINTYMD(2)
                       MOVE  PTNYUINRRK-NYUINYMD  TO  WRK-MINNYMD(1)
                       MOVE  PTNYUINRRK-TAIINYMD  TO  WRK-MINTYMD(1)
                     END-IF
                     PERFORM 930-PTNYUINRRK-FETCH-SEC
                   END-PERFORM
           ELSE
                   MOVE   1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報１件読み込み
      *****************************************************************
       930-PTNYUINRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key21"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報クローズ
      *****************************************************************
       930-PTNYUINRRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"              TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key21"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       930-PTNYUINRRK-CLOSE-EXT.
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
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD (1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
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
