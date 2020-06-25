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
       PROGRAM-ID.     SEIKYU0503.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 秋田・複写式レセプト
      *                      (274,474,273,275,276,272,277,278)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/08/05    有村          制度変更による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    07/05/08    プレビュー時の患者番号を印字
      *                                  返戻時の診療年月の修正
      *  01.00.02    内田    07/08/19    高齢者の場合、本人家族欄へ２箇所
      *                                  印字していたのを修正
      *  01.00.03    内田    07/09/28    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  長期上位（974）は長期と同様の
      *                                  処理をする
      *  01.00.04    内田    08/05/16    ４月改正対応
      *  01.00.05    吉川    08/10/22    SPA-AREA初期化
      *  01.00.06    井上    19/12/09    修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF101-FILE  ASSIGN    MF101PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF101-KEY
                               FILE    STATUS  IS  STS-MF101.
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
       FD  MF101-FILE.
       01  MF101-REC.
           COPY  "SEI0503.INC".
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
                   BY         //MF101//.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
           COPY  "SC0503.INC".
           COPY  "SC0504.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
           03  STS-RECE       PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-MF101      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-SYSKANRI3  PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
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
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z1                PIC Z.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z79               PIC ZZZ,ZZ9.
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
           03  WRK-HKNJANUM          PIC X(06).
           03  WRK-PRTID             PIC X(20).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOBETUCITYNUM     PIC X(03).
           03  WRK-FTNMONEY-G        PIC 9(08).
           03  WRK-ETC-TBL.
               05  WRK-ETC-KOHNUM          PIC X(03).
               05  WRK-ETC-KOHID           PIC 9(10).
               05  WRK-ETC-TENSU           PIC 9(07).
               05  WRK-ETC-ITBFTN          PIC 9(07).
               05  WRK-ETC-S-KINGK         PIC 9(08).
               05  WRK-ETC-S-FTNGK         PIC 9(08).
               05  WRK-ETC-FTNJANUM        PIC X(20).
               05  WRK-ETC-JKYSNUM         PIC X(20).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-PATH              PIC  X(50).
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF101   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
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
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
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
           MOVE    "SEIKYU0503"    TO  JOB-PGID
           MOVE    "複写式レセプト" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
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
           IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *
           MOVE  "MF101ZZ"        TO  MF101PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)
                                  TO  MF101PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM     TO  MF101PARA-HOSPNUM
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"           TO  SYS-1001-KANRICD
           MOVE  "*"              TO  SYS-1001-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                                  TO  SYS-1001-STYUKYMD(1:6)
                                      SYS-1001-EDYUKYMD(1:6)
           MOVE  "01"             TO  SYS-1001-STYUKYMD(7:2)
                                      SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM      TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC     TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI        =  ZERO
             MOVE  MCPDATA-REC    TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU0503 SYS 1001 ERR ***"
             MOVE  1              TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1002-STYUKYMD(1:6)
                                   SYS-1002-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU0503 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
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
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF101-FILE
      *
           IF  CNT-RECE02   >       0
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
               IF  RECE02-KOHNUM (IDZ) = "274" OR "474" OR "273" OR
                                         "275" OR "276" OR "272" OR
                                         "277" OR "278"
                   PERFORM 20011-SYUKEI-SEC
               END-IF
           END-PERFORM
      *
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
           INITIALIZE       MF101-REC
      *
           MOVE    SPA-HOSPNUM         TO  MF101-HOSPNUM
           MOVE    RECE02-PTNUM        TO  MF101-PTNUM
           MOVE    RECE02-RECEKA       TO  MF101-SRYKA
           MOVE    RECE02-HKNJANUM-KEY TO  MF101-HKNJANUM-KEY
           MOVE    RECE02-TEKSTYMD     TO  MF101-TEKSTYMD
           MOVE    RECE02-NYUGAIKBN    TO  MF101-NYUGAIKBN
           MOVE    RECE02-SRYYM        TO  MF101-SRYYM
           MOVE    RECE02-TEISYUTUSAKI TO  MF101-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  MF101-RECESYUBETU
           MOVE    RECE02-HOJOKBN-KEY  TO  MF101-HOJOKBN-KEY
      *
           MOVE    RECE02-PTID         TO  MF101-PTID
           MOVE    RECE02-NAME         TO  MF101-NAME
           MOVE    RECE02-KANANAME     TO  MF101-KANANAME
           MOVE    RECE02-BIRTHDAY     TO  MF101-BIRTHDAY
           MOVE    RECE02-SEX          TO  MF101-SEX
           MOVE    RECE02-AGE          TO  MF101-AGE
           MOVE    RECE02-HKN          TO  MF101-HKN
           MOVE    RECE02-RJNHKN       TO  MF101-RJNHKN
           MOVE    RECE02-ZAITAKU      TO  MF101-ZAI
      *
           MOVE    RECE02-JNISSU      (1)      TO  MF101-G-JNISSU
           MOVE    RECE02-TOTALTEN    (1)      TO  MF101-G-TOTALTEN
           MOVE    RECE02-FTNMONEY    (1)      TO  MF101-G-FTNMONEY
           MOVE    RECE02-SHOKUJIINF  (1)      TO  MF101-G-SHOKUJIINF
      *
           MOVE    RECE02-KOHFTNJANUM (IDZ)    TO  MF101-FTNJANUM
           MOVE    RECE02-KOHJKYSNUM  (IDZ)    TO  MF101-JKYSNUM
           MOVE    MF101-FTNJANUM(3:6)         TO  MF101-CITYNUM
           MOVE    RECE02-KOHINF      (IDZ)    TO  MF101-KOHINF
      *
      *    個別発行用市町村番号
           MOVE    MF101-KOHFTNJANUM(5:3)      TO  WRK-KOBETUCITYNUM
      *
      *    長期併用判定と月途中で公費を切替えた時の全国公費分のデータ集計
           MOVE     MF101-KOHNUM           TO  WRK-KOHNUM
           MOVE     MF101-KOHID            TO  WRK-KOHID
           MOVE     ZERO                   TO  WRK-FTNMONEY-G
           INITIALIZE                          WRK-ETC-TBL
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
      *
      *    月途中で公費を切替えていない場合請求管理情報より他公費情報取得
           IF  IDZ           =     1   AND
               RECE02-KOHNUM(IDZ + 1)  =  SPACE
               PERFORM  900-SEIKYU-READ-SEC
               DISPLAY "請求管理から公費情報取得" RECE02-PTNUM
                       "取得公費番号１=" RECE10-KOHNUM(1)
               IF      RECE10-KOHNUM(1)  NOT =  SPACE
                   MOVE    RECE10-KOHNUM(1)  TO  MF101-E-KOHNUM
      *
      *            実日数
                   MOVE    RECE10-JNISSU(2)
                                           TO  MF101-E-JNISSU
      *
      *            点数
                   MOVE    RECE10-TOTALTEN(2)
                                           TO  MF101-E-TOTALTEN
      *
      *            一部負担金
                   MOVE    RECE10-FTNMONEY(2)
                                           TO  MF101-E-FTNMONEY
      *            食事日数
                   MOVE    RECE10-SHOKUJINISSU(2)
                                           TO  MF101-E-SHOKUJINISSU
      *
      *            食事療養費
                   MOVE    RECE10-SHOKUJIRYOYOHI(2)
                                           TO  MF101-E-SHOKUJIRYOYOHI
      *
      *            食事負担額
                   MOVE    RECE10-SHOKUJIFTN(2)
                                           TO  MF101-E-SHOKUJIFTN
      *
                   MOVE    WRK-ETC-KOHID       TO  WRK-KOHID
                   PERFORM 930-PTKOHINF-SET-SEC
                   DISPLAY "負担者番号=" WRK-ETC-FTNJANUM
                           "公費番号="   MF101-E-KOHNUM
                           "公費ID=" WRK-ETC-KOHID
                   MOVE    WRK-ETC-FTNJANUM    TO  MF101-E-KOHFTNJANUM
                   MOVE    WRK-ETC-JKYSNUM     TO  MF101-E-KOHJKYSNUM
               END-IF
           ELSE
      *        月途中で公費を切替えている場合は収納テーブルから
      *        他公費情報取得(この場合日数は表示しない)
               DISPLAY "収納情報から公費情報取得 " RECE02-PTNUM
               IF      WRK-ETC-KOHNUM      NOT =   SPACE
                   DISPLAY "その他公費番号=" WRK-ETC-KOHNUM
                   MOVE    WRK-ETC-KOHID   TO  WRK-KOHID
                   PERFORM 930-PTKOHINF-SET-SEC
      *
      *            点数
                   MOVE    WRK-ETC-TENSU   TO  MF101-E-JNISSU
      *
      *            一部負担金
                   MOVE    WRK-ETC-ITBFTN  TO  MF101-E-FTNMONEY
      *
      *            食事療養費
                   MOVE    WRK-ETC-S-KINGK TO  MF101-E-SHOKUJIRYOYOHI
      *
      *            食事標準負担額
                   MOVE    WRK-ETC-S-FTNGK TO  MF101-E-SHOKUJIFTN
      *
                   MOVE    WRK-ETC-FTNJANUM
                                           TO  MF101-E-KOHFTNJANUM
                   MOVE    WRK-ETC-JKYSNUM
                                           TO  MF101-E-KOHJKYSNUM
               END-IF
           END-IF
      *
           COMPUTE IDY     =   IDZ +   1
      *
      *    ３併用している場合や、主保険分と異る場合は省略しない
      *    実日数
           IF      RECE02-JNISSU(IDY)  NOT =   MF101-G-JNISSU
           OR      MF101-E-KOHFTNJANUM NOT =   SPACE
               MOVE    RECE02-JNISSU(IDY)      TO  MF101-JNISSU
           END-IF
      *
      *    点数
           IF      RECE02-TOTALTEN(IDY)
                                       NOT =   MF101-G-TOTALTEN
           OR      MF101-E-KOHFTNJANUM NOT =   SPACE
               MOVE    RECE02-TOTALTEN(IDY)    TO  MF101-TOTALTEN
           END-IF
      *
      *    食事日数
           IF      RECE02-SHOKUJINISSU(IDY)
                                       NOT =   MF101-G-SHOKUJINISSU
           OR      MF101-E-KOHFTNJANUM NOT =   SPACE
               MOVE    RECE02-SHOKUJINISSU(IDY)
                                               TO  MF101-SHOKUJINISSU
           END-IF
      *
      *    食事療養費
           IF      RECE02-SHOKUJIRYOYOHI(IDY)
                                       NOT =   MF101-G-SHOKUJIRYOYOHI
           OR      MF101-E-KOHFTNJANUM NOT =   SPACE
               MOVE    RECE02-SHOKUJIRYOYOHI(IDY)
                                               TO  MF101-SHOKUJIRYOYOHI
           END-IF
      *
      *    食事負担
           IF      RECE02-SHOKUJIFTN(IDY)
                                       NOT =   MF101-G-SHOKUJIFTN
           OR      MF101-E-KOHFTNJANUM NOT =   SPACE
               MOVE    RECE02-SHOKUJIFTN(IDY)  TO  MF101-SHOKUJIFTN
           END-IF
      *
      *    一部負担金
           MOVE    RECE02-FTNMONEY    (IDY)    TO  MF101-FTNMONEY
      *
      *    フラグ（未就学児、特例措置）
           MOVE    RECE02-FLG (1)     TO  MF101-FLG (1)
           MOVE    RECE02-FLG (2)     TO  MF101-FLG (2)
      *
      *    保険者番号チェック処理
           MOVE    ZERO               TO   FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR    =    ZERO
             WRITE  MF101-REC
             ADD  1  TO  CNT-MF101
           ELSE
               DISPLAY "助成対象外患者番号="  RECE02-PTNUM
           END-IF
      *
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
           MOVE    "05"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF101-PTNUM
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
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF101-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *       外来
               IF  MF101-NYUGAIKBN  =  "2"
      *        帳票編集<見出し>処理
                   PERFORM  310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                   PERFORM  320-BODY-HEN-SEC
      *        帳票印刷処理
                   PERFORM  330-PRINT-OUT-SEC
      *       入院
               ELSE
      *        帳票編集<見出し>処理
                   PERFORM  311-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                   PERFORM  321-BODY-HEN-SEC
      *        帳票印刷処理
                   PERFORM  331-PRINT-OUT-SEC
               END-IF
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    MF101-FILE
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
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC03
      *
      *    診療年月（診療年月日として退避）
           MOVE    MF101-SRYYM         TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)     TO  SC03-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN      TO  SC03-HOSPCDN
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SC03-ADRS
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SC03-HOSPNAME
      *
      *    開設者名
           MOVE  SYS-1001-KAISETUNAME  TO  SC03-KAISETUNAME
      *
      *    保険種別区分
           EVALUATE  TRUE
               WHEN  MF101-HKNNUM = "063" OR "072" OR "073" OR
                                    "067" OR "074" OR "075"
                   MOVE    "○"        TO  SC03-HKNKBN (4)
               WHEN  MF101-RJNHKNNUM   =   "027"
               WHEN  MF101-HKNNUM  =  "039" OR "040"
                   MOVE    "○"        TO  SC03-HKNKBN (3)
               WHEN OTHER
                   MOVE    "○"        TO  SC03-HKNKBN (1)
           END-EVALUATE
      *
      *    併用区分
           IF  MF101-RECESYUBETU(3:1)  = "1"
               MOVE    "○"    TO  SC03-HEIKBN(2)
           ELSE
               MOVE    "○"    TO  SC03-HEIKBN(3)
           END-IF
      *
      *    本人家族区分
           IF  MF101-AGE         >   69  OR
               MF101-RJNHKNNUM   NOT =   SPACE
           OR  MF101-HKNNUM  =  "039"  OR  "040"
               IF  MF101-KYURATE   =   10
                   MOVE    "○"    TO  SC03-KOREIKBN (1)
               ELSE
                   MOVE    "○"    TO  SC03-KOREIKBN (2)
               END-IF
           ELSE
             IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
             AND  MF101-FLG (1)  =  1
               MOVE    "○"            TO  SC03-HONKZKKBN (2)
             ELSE
               IF  MF101-AGE > 2
                   IF  MF101-HONKZKKBN = "1"
                       MOVE    "○"    TO  SC03-HONKZKKBN (1)
                   ELSE
                       MOVE    "○"    TO  SC03-HONKZKKBN (3)
                   END-IF
               ELSE
                   MOVE    "○"      TO  SC03-HONKZKKBN (2)
               END-IF
             END-IF
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
      *    老人市町村番号
           MOVE  MF101-RJNFTNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT         TO  SC03-RJNFTNJANUM
      *    老人受給者番号
           MOVE  MF101-RJNJKYSNUM      TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT         TO  SC03-RJNJKYSNUM
      *
      *    負担者番号・受給者番号
      *    全国公費が有る場合は公費１の欄に全国公費の番号を印字し、
      *    公費２の欄に地方公費の番号を印字する
           IF    MF101-E-KOHFTNJANUM       =   SPACE
               MOVE  MF101-KOHFTNJANUM     TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC03-FTNJANUM(1)
      *
               MOVE  MF101-KOHJKYSNUM      TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC03-JKYSNUM(1)
           ELSE
               MOVE  MF101-E-KOHFTNJANUM   TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC03-FTNJANUM(1)
      *
               MOVE  MF101-E-KOHJKYSNUM    TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC03-JKYSNUM(1)
      *
               MOVE  MF101-KOHFTNJANUM     TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC03-FTNJANUM(2)
      *
               MOVE  MF101-KOHJKYSNUM      TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC03-JKYSNUM(2)
           END-IF
      *
      *    保険者番号
           MOVE  MF101-HKNJANUM            TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           DISPLAY "保険者番号=" MF101-HKNJANUM
                   " WRK-OUT-INPUT" WRK-OUT-INPUT
                   "リターンCD=" KANACHK-RC
           EVALUATE  MF101-HKNNUM
               WHEN "001"
                   MOVE  WRK-OUT-INPUT(1:8)    TO  SC03-HKNJANUM(9:8)
               WHEN "060"
                   MOVE  WRK-OUT-INPUT(1:12)   TO  SC03-HKNJANUM(5:12)
               WHEN OTHER
                   MOVE  WRK-OUT-INPUT         TO  SC03-HKNJANUM
           END-EVALUATE
           DISPLAY "保険者番号 結果=" SC03-HKNJANUM
      *
      *    給付割合
           IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF101-FLG (2)  =  1
               MOVE    "○"                TO  SC03-KYURATE(3)
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN  10
                   MOVE    "○"            TO  SC03-KYURATE(2)
               WHEN  20
                   MOVE    "○"            TO  SC03-KYURATE(3)
               WHEN  30
                   MOVE    "○"            TO  SC03-KYURATE(4)
               WHEN  OTHER
                   MOVE    "○"            TO  SC03-KYURATE(1)
             END-EVALUATE
           END-IF
      *
      *    被保険者証記号・番号
           MOVE  MF101-KIGO            TO  SC03-KIGO
           MOVE  MF101-NUM             TO  SC03-BANGO
      *
      *    患者番号
           MOVE  MF101-PTNUM           TO  SC03-PTNUM
      *
      *    氏名
           MOVE  MF101-NAME            TO  SC03-NAME
      *    性別
           IF    MF101-SEX             =   "1"
             MOVE  "○"                TO  SC03-SEX(1)
           ELSE
             MOVE  "○"                TO  SC03-SEX(2)
           END-IF
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *
           DISPLAY  "BIRTHDAY = " WRK-SYMD
           DISPLAY  "年号 = " LNK-DAY2-EDTYMD2
      *
           EVALUATE  LNK-DAY2-EDTYMD2 (1:4)
               WHEN  "明治"
                   MOVE    "○"        TO  SC03-NENGO (1)
               WHEN  "大正"
                   MOVE    "○"        TO  SC03-NENGO (2)
               WHEN  "昭和"
                   MOVE    "○"        TO  SC03-NENGO (3)
               WHEN  "平成"
                   MOVE    "○"        TO  SC03-NENGO (4)
           END-EVALUATE
           MOVE  LNK-DAY2-EDTYMD1 (2:8)    TO  SC03-BIRTHDAY
      *
      *    特記事項
           IF  MF101-TYOKI  =  1
               MOVE    "０２長"        TO  SC03-TOKKI
           END-IF
           IF  MF101-TYOKI  =  2
               MOVE    "１６長２"      TO  SC03-TOKKI
           END-IF
      *
      *    診療実日数(保険)
           MOVE  MF101-G-JNISSU        TO  WRK-Z2
           MOVE  WRK-Z2                TO  SC03-NISSU(1)
      *
      *    点数(保険)
           MOVE  MF101-G-TOTALTEN      TO  WRK-Z9
           MOVE  WRK-Z9                TO  SC03-TENSU(1)
      *
      *    一部負担金額(保険)
           MOVE  MF101-G-FTNMONEY      TO  WRK-Z7
           MOVE  WRK-Z7                TO  SC03-FTNMONEY(1)
      *
      *    
           DISPLAY "公費番号=" MF101-E-KOHNUM
           IF      MF101-E-KOHNUM      =   SPACE
      *
      *        診療実日数(公費１)
               MOVE    MF101-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC03-NISSU(2)
      *
      *        点数(公費１)
               MOVE    MF101-TOTALTEN      TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC03-TENSU(2)
      *
      *        一部負担金(公費１) 
               IF      MF101-KOHNUM    =   "274" OR "474"
                   MOVE    MF101-FTNMONEY      TO  WRK-Z79
                   MOVE    WRK-Z79             TO  SC03-FTNMONEY(2)
               ELSE
                   MOVE    MF101-FTNMONEY      TO  WRK-Z7
                   MOVE    WRK-Z7              TO  SC03-FTNMONEY(2)
               END-IF
           ELSE
      *
      *
      *        診療実日数(公費１)
               MOVE    MF101-E-JNISSU      TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC03-NISSU(2)
      *
      *        点数(公費１)
               MOVE    MF101-E-TOTALTEN    TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC03-TENSU(2)
      *
      *        一部負担金(公費１)
               MOVE    MF101-E-FTNMONEY    TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC03-FTNMONEY(2)
      *
      *        診療実日数(公費２)
               MOVE    MF101-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC03-NISSU(3)
      *
      *        点数(公費２)
               MOVE    MF101-TOTALTEN      TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC03-TENSU(3)
      *
      *        一部負担金(公費２)
               IF      MF101-KOHNUM    =   "274" OR "474"
                   MOVE    MF101-FTNMONEY      TO  WRK-Z79
                   MOVE    WRK-Z79             TO  SC03-FTNMONEY(3)
               ELSE
                   MOVE    MF101-FTNMONEY      TO  WRK-Z7
                   MOVE    WRK-Z7              TO  SC03-FTNMONEY(3)
               END-IF
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
           MOVE    "SC0503.red"        TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                       TO  SPRT-TITLE
           MOVE    SC03                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    MF101-PTID          TO  SPRT-PTID
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
      *    帳票編集<ヘッダー部>処理 −入院
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC04
      *
      *    診療年月（診療年月日として退避）
           MOVE    MF101-SRYYM         TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)     TO  SC04-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN      TO  SC04-HOSPCDN
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SC04-ADRS
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SC04-HOSPNAME
      *
      *    開設者名
           MOVE  SYS-1001-KAISETUNAME  TO  SC04-KAISETUNAME
      *
      *    保険種別区分
           EVALUATE  TRUE
               WHEN  MF101-HKNNUM = "063" OR "072" OR "073" OR
                                    "067" OR "074" OR "075"
                   MOVE    "○"        TO  SC04-HKNKBN (4)
               WHEN  MF101-RJNHKNNUM   =   "027"
               WHEN  MF101-HKNNUM  =  "039" OR "040"
                   MOVE    "○"        TO  SC04-HKNKBN (3)
               WHEN OTHER
                   MOVE    "○"        TO  SC04-HKNKBN (1)
           END-EVALUATE
      *
      *    併用区分
           IF  MF101-RECESYUBETU(3:1)  = "1"
               MOVE    "○"    TO  SC04-HEIKBN(2)
           ELSE
               MOVE    "○"    TO  SC04-HEIKBN(3)
           END-IF
      *
      *    本人家族区分
           IF  MF101-AGE         >   69  OR
               MF101-RJNHKNNUM   NOT =   SPACE
           OR  MF101-HKNNUM  =  "039"  OR  "040"
               IF  MF101-KYURATE   =   10
                   MOVE    "○"    TO  SC04-KOREIKBN (1)
               ELSE
                   MOVE    "○"    TO  SC04-KOREIKBN (2)
               END-IF
           ELSE
             IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
             AND  MF101-FLG (1)  =  1
               MOVE    "○"            TO  SC04-HONKZKKBN (2)
             ELSE
               IF  MF101-AGE > 2
                   IF  MF101-HONKZKKBN = "1"
                       MOVE    "○"    TO  SC04-HONKZKKBN (1)
                   ELSE
                       MOVE    "○"    TO  SC04-HONKZKKBN (3)
                   END-IF
               ELSE
                   MOVE    "○"      TO  SC04-HONKZKKBN (2)
               END-IF
             END-IF
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理−入院
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    老人市町村番号
           MOVE  MF101-RJNFTNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT         TO  SC04-RJNFTNJANUM
      *    老人受給者番号
           MOVE  MF101-RJNJKYSNUM      TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT         TO  SC04-RJNJKYSNUM
      *
      *    負担者番号・受給者番号
      *    全国公費が有る場合は公費１の欄に全国公費の番号を印字し、
      *    公費２の欄に地方公費の番号を印字する
           IF    MF101-E-KOHFTNJANUM       =   SPACE
               MOVE  MF101-KOHFTNJANUM     TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC04-FTNJANUM(1)
      *
               MOVE  MF101-KOHJKYSNUM      TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC04-JKYSNUM(1)
           ELSE
               MOVE  MF101-E-KOHFTNJANUM   TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC04-FTNJANUM(1)
      *
               MOVE  MF101-E-KOHJKYSNUM    TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC04-JKYSNUM(1)
      *
               MOVE  MF101-KOHFTNJANUM     TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC04-FTNJANUM(2)
      *
               MOVE  MF101-KOHJKYSNUM      TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE  WRK-OUT-INPUT         TO  SC04-JKYSNUM(2)
           END-IF
      *
      *    保険者番号
           MOVE  MF101-HKNJANUM            TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           DISPLAY "保険者番号=" MF101-HKNJANUM
                   " WRK-OUT-INPUT" WRK-OUT-INPUT
                   "リターンCD=" KANACHK-RC
           EVALUATE  MF101-HKNNUM
               WHEN "001"
                   MOVE  WRK-OUT-INPUT(1:8)    TO  SC04-HKNJANUM(9:8)
               WHEN "060"
                   MOVE  WRK-OUT-INPUT(1:12)   TO  SC04-HKNJANUM(5:12)
               WHEN OTHER
                   MOVE  WRK-OUT-INPUT         TO  SC04-HKNJANUM
           END-EVALUATE
           DISPLAY "保険者番号 結果=" SC04-HKNJANUM
      *
      *    給付割合
           IF   MF101-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF101-FLG (2)  =  1
               MOVE    "○"                TO  SC04-KYURATE(3)
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN  10
                   MOVE    "○"            TO  SC04-KYURATE(2)
               WHEN  20
                   MOVE    "○"            TO  SC04-KYURATE(3)
               WHEN  30
                   MOVE    "○"            TO  SC04-KYURATE(4)
               WHEN  OTHER
                   MOVE    "○"            TO  SC04-KYURATE(1)
             END-EVALUATE
           END-IF
      *
      *    被保険者証記号・番号
           MOVE  MF101-KIGO            TO  SC04-KIGO
           MOVE  MF101-NUM             TO  SC04-BANGO
      *
      *    患者番号
           MOVE  MF101-PTNUM           TO  SC04-PTNUM
      *
      *    氏名
           MOVE  MF101-NAME            TO  SC04-NAME
      *    性別
           IF    MF101-SEX             =   "1"
             MOVE  "○"                TO  SC04-SEX(1)
           ELSE
             MOVE  "○"                TO  SC04-SEX(2)
           END-IF
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY2-EDTYMD2 (1:4)
               WHEN  "明治"
                   MOVE    "○"        TO  SC04-NENGO (1)
               WHEN  "大正"
                   MOVE    "○"        TO  SC04-NENGO (2)
               WHEN  "昭和"
                   MOVE    "○"        TO  SC04-NENGO (3)
               WHEN  "平成"
                   MOVE    "○"        TO  SC04-NENGO (4)
           END-EVALUATE
           MOVE  LNK-DAY2-EDTYMD1 (2:8)    TO  SC04-BIRTHDAY
      *
      *    特記事項
           IF  MF101-TYOKI  =  1
               MOVE    "０２長"        TO  SC04-TOKKI
           END-IF
           IF  MF101-TYOKI  =  2
               MOVE    "１６長２"      TO  SC04-TOKKI
           END-IF
      *
      *    診療実日数(保険)
           MOVE  MF101-G-JNISSU        TO  WRK-Z2
           MOVE  WRK-Z2                TO  SC04-NISSU(1)
      *
      *    点数(保険)
           MOVE  MF101-G-TOTALTEN      TO  WRK-Z9
           MOVE  WRK-Z9                TO  SC04-TENSU(1)
      *
      *    一部負担金額(保険)
           MOVE  MF101-G-FTNMONEY      TO  WRK-Z7
           MOVE  WRK-Z7                TO  SC04-FTNMONEY(1)
      *
      *    食事日数(保険)
           MOVE  MF101-G-SHOKUJINISSU  TO  WRK-Z2
           MOVE  WRK-Z2                TO  SC04-SNISSU(1)
      *
      *    食事療養費(保険)
           MOVE  MF101-G-SHOKUJIRYOYOHI
                                       TO  WRK-Z9
           MOVE  WRK-Z9                TO  SC04-SKINGK(1)
      *
      *    標準負担額(保険)
           MOVE  MF101-G-SHOKUJIFTN    TO  WRK-Z9
           MOVE  WRK-Z9                TO  SC04-SFTNGK(1)
      *
      *    
           DISPLAY "併用公費チェック：入院" MF101-E-KOHNUM
           IF      MF101-E-KOHNUM      =   SPACE
      *
      *        診療実日数(公費１)
               MOVE    MF101-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-NISSU(2)
      *
      *        点数(公費１)
               MOVE    MF101-TOTALTEN      TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC04-TENSU(2)
      *
      *        一部負担金(公費１)
               IF      MF101-KOHNUM    =   "274" OR "474"
                   MOVE    MF101-FTNMONEY      TO  WRK-Z79
                   MOVE    WRK-Z79             TO  SC04-FTNMONEY(2)
               ELSE
                   MOVE    MF101-FTNMONEY      TO  WRK-Z7
                   MOVE    WRK-Z7              TO  SC04-FTNMONEY(2)
               END-IF
      *
      *        食事日数(公費１)
               MOVE    MF101-SHOKUJINISSU  TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-SNISSU(2)
      *
      *        食事療養費(公費１)
               MOVE    MF101-SHOKUJIRYOYOHI
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC04-SKINGK(2)
      *
      *        標準負担額(公費１)
               MOVE    MF101-SHOKUJIFTN   TO  WRK-Z9
               MOVE    WRK-Z9             TO  SC04-SFTNGK(2)
           ELSE
      *
      *
      *        診療実日数(公費１)
               MOVE    MF101-E-JNISSU      TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-NISSU(2)
      *
      *        点数(公費１)
               MOVE    MF101-E-TOTALTEN    TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC04-TENSU(2)
      *
      *        一部負担金(公費１)
               MOVE    MF101-E-FTNMONEY    TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC04-FTNMONEY(2)
               DISPLAY "全国公費：一部負担金=" MF101-E-FTNMONEY
      *
      *        食事日数(公費１)
               MOVE    MF101-E-SHOKUJINISSU
                                           TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-SNISSU(2)
      *
      *        食事療養費(公費１)
               MOVE    MF101-E-SHOKUJIRYOYOHI
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC04-SKINGK(2)
      *
      *        標準負担額(公費１)
               MOVE    MF101-E-SHOKUJIFTN TO  WRK-Z9
               MOVE    WRK-Z9             TO  SC04-SFTNGK(2)
      *
      *        診療実日数(公費２)
               MOVE    MF101-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-NISSU(3)
      *
      *        点数(公費２)
               MOVE    MF101-TOTALTEN      TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC04-TENSU(3)
      *
      *        一部負担金(公費２)
               IF      MF101-KOHNUM    =   "274" OR "474"
                   MOVE    MF101-FTNMONEY      TO  WRK-Z79
                   MOVE    WRK-Z79             TO  SC04-FTNMONEY(3)
               ELSE
                   MOVE    MF101-FTNMONEY      TO  WRK-Z7
                   MOVE    WRK-Z7              TO  SC04-FTNMONEY(3)
               END-IF
      *
      *        食事日数(公費２)
               MOVE    MF101-SHOKUJINISSU  TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-SNISSU(3)
      *
      *        食事療養費(公費２)
               MOVE    MF101-SHOKUJIRYOYOHI
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  SC04-SKINGK(3)
      *
      *        標準負担額(公費２)
               MOVE    MF101-SHOKUJIFTN   TO  WRK-Z9
               MOVE    WRK-Z9             TO  SC04-SFTNGK(3)
           END-IF
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理−入院
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           MOVE    "SC0504.red"        TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                       TO  SPRT-TITLE
           MOVE    SC04                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    MF101-PTID          TO  SPRT-PTID
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
       331-PRINT-OUT-EXT.
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
      *
           CLOSE  MF101-FILE
           MOVE   CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU0503 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU0503 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU0503 END ***"
      *
           IF  CNT-MF101  =  0
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
           MOVE    "DBSELECT"            TO  MCP-FUNC
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
           MOVE    "DBFETCH"              TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF  RECE02-KOHNUM(1) = "274" OR "474" OR "273" OR "275"
                                   OR "276" OR "272" OR "277" OR "278"
               OR  RECE02-KOHNUM(2) = "274" OR "474" OR "273" OR "275"
                                   OR "276" OR "272" OR "277" OR "278"
               OR  RECE02-KOHNUM(3) = "274" OR "474" OR "273" OR "275"
                                   OR "276" OR "272" OR "277" OR "278"
               OR  RECE02-KOHNUM(4) = "274" OR "474" OR "273" OR "275"
                                   OR "276" OR "272" OR "277" OR "278"
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
               MOVE     1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
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
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
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
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY             TO  RECE10-KEY
           MOVE    RECE10-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE  "tbl_seikyu"                 TO  MCP-TABLE
           MOVE  "key"                        TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"                  USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE  "tbl_seikyu"                 TO  MCP-TABLE
               MOVE  "key"                        TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"                  USING
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE  "tbl_seikyu"                 TO  MCP-TABLE
           MOVE  "key"                        TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"                  USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM              TO  COMB-HOSPNUM
           MOVE    SYU-PTID                 TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
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
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
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
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =  SPACE
      *
               IF  SYU-KOH-HKNNUM(IDX)     NOT =   "027"   AND
                   SYU-KOH-HKNNUM(IDX)(1:1)    =   "0"
                   MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-ETC-KOHNUM
                   MOVE  COMB-KOHID    (IDX)   TO  WRK-ETC-KOHID
                   ADD   SYU-TOTAL-HKNTEN      TO  WRK-ETC-TENSU
      *            限度額が無い場合は一部負担金は表示しない
                   IF  (RECE02-FTNMONEY(1)     >   ZERO)   OR
                       (SYU-KOH-HKNNUM(IDX)    NOT =   "010" AND "011"
                                                   AND "021" AND "020")
                       ADD     SYU-KOH-COMPFTN(IDX)
                                               TO  WRK-ETC-ITBFTN
                   END-IF
      *           主保険の食事療養費情報から食事療養費、標準負担額を取得
                   ADD  SYU-SYUSKJ-RYOYOHI
                                           TO  WRK-ETC-S-KINGK
                   ADD  SYU-SYUSKJ-FTN
                                           TO  WRK-ETC-S-FTNGK
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      MF101-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      MF101-TYOKI
               END-IF
      *
           END-PERFORM
      *
           IF  RECE02-FTNMONEY(1)          >   ZERO
               IF  RECE02-RJNHKNNUM    NOT =   SPACE
                   ADD  SYU-KOH-COMPFTN(1) TO WRK-FTNMONEY-G
               ELSE
                   ADD  SYU-SYUCOMPFTN     TO WRK-FTNMONEY-G
               END-IF
           END-IF
      *
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
             MOVE  "DBCLOSECURSOR"   TO  MCP-FUNC
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
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
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE  RECE02-PTID        TO  PTKOH-PTID
           MOVE  WRK-KOHID          TO  PTKOH-KOHID
      *
           MOVE  PTKOHINF-REC            TO  MCPDATA-REC
           MOVE  "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
                 IF  PTKOH-SAKJOKBN  NOT = "1"
                     IF  PTKOH-TEKSTYMD(1:6) <=  RECE02-SRYYM(1:6)  AND 
                         PTKOH-TEKEDYMD(1:6) >=  RECE02-SRYYM(1:6)
                         MOVE  PTKOH-FTNJANUM    TO  WRK-ETC-FTNJANUM
                         MOVE  PTKOH-JKYSNUM     TO  WRK-ETC-JKYSNUM
                     END-IF
                 END-IF
                 MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                 MOVE    "key"               TO  MCP-PATHNAME
                 PERFORM  935-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
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
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
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
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-STYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-EDYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-EDYUKYMD(7:2)
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-EDYUKYMD(7:2)
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
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
