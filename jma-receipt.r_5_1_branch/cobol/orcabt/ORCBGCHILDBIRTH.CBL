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
       PROGRAM-ID.             ORCBGCHILDBIRTH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 出産育児一時金等代理申請・受取請求書
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/07/28    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    09/10/13  集計票、送付書の作成対応
      *  04.05.02    NACL-藤原    10/07/07  ２５日請求対応
      *                                     ２５日請求時はＣＳＶの診療年
      *                                     月は請求年月と同じにする
      *                                     記録条件仕様の変更に伴う修正
      *  04.05.03    NACL-藤原    10/08/10  支払基金支部の名称の変更に伴
      *                                     う修正
      *
      *  04.06.01    NACL-藤原    10/06/17  被保険者番号の桁数拡張対応
      *  04.06.02    NACL-藤原    11/03/15  平成２３年４月以降提出分は
      *                                     様式変更（１枚１名記載） 
      *                                     HCN61V02.red
      *  04.06.03    NACL-藤原    13/06/05  退職国保の保険者番号を請求書の
      *                                     保険者番号欄へ記載する
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.02    NACL-藤原    14/09/26  システム管理対象月設定
      *  04.07.03    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    17/02/15  直接支払制度の一部改正に伴う
      *                                     対応
      *
      *  05.00.01    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *  05.00.02    NACL-藤原    19/06/05  医療機関名称の編集方法の変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    分娩管理ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  BUNBENUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-BUNBENUPD.
      *
      *    エラーファイル
           SELECT  BUNBENERR-FILE  ASSIGN  BUNBENERR
                                   FILE    STATUS  IS  STS-BUNBENERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    分娩管理ＤＢ更新用ファイル
       FD  UPD-FILE.
       01  BUNBENX-REC.
           COPY    "CPBUNBEN.INC"  REPLACING  //BUNBEN-//
                                   BY         //BUNBENX-//.
      *
      *    エラーファイル
       FD  BUNBENERR-FILE.
       01  BUNBENERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    分娩管理ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BUNBENUPD//.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //BUNBENERR//.
      *
      *    請求書
           COPY    "HCN61V01.INC"  REPLACING  //HCN61V01//
                                   BY         //HCN61//.
      *    平成２３年４月以降提出分
           COPY    "HCN61V03.INC"  REPLACING  //HCN61V03//
                                   BY         //HCN61V02//.
      *    集計票
           COPY    "HCN65.INC".
      *    送付書
           COPY    "HCN64.INC".
      *
       01  INDEX-AREA.
           03  IDW                 PIC 9(03).
           03  IDX                 PIC 9(04). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
           03  IDY1                PIC 9(03).
           03  IDY2                PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDXX                PIC 9(03).
           03  IDW2                PIC 9(04).
           03  IDZ1                PIC 9(03).
           03  IDZ2                PIC 9(03).
           03  IDZ3                PIC 9(03).
           03  IDZ4                PIC 9(03).
           03  IDZ5                PIC 9(03).
           03  IDZ6                PIC 9(03).
           03  IDYY                PIC 9(03).
           03  IDZZ                PIC 9(03).
           03  IDWW                PIC 9(03).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BUNBENERR       PIC X(02).
           03  STS-BUNBENUPD       PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CSV-INFO        PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
      *
           03  FLG-UPDFILE         PIC 9(01). 
           03  FLG-ERR             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-UPDFILE         PIC 9(05).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM  PIC X(08).
               05  KEY-N-BUNBENKBN PIC 9(01).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM  PIC X(08).
               05  KEY-O-BUNBENKBN PIC 9(01).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-TBL-OCC             OCCURS  2.
               05  SUM-CNT         PIC 9(03).
               05  SUM-BIRTHCNT    PIC 9(03).
               05  SUM-MONEY       PIC 9(08).
               05  SUM-PAGE        PIC 9(03).
      *
           03  SUM-RECORD-CNT      PIC 9(09).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SKYYM      PIC X(06).
           03  WRK-PARA-TEISYUTUBIKBN
                                   PIC X(01).
           03  WRK-PARA-TEISYUTUSAKI
                                   PIC X(01).
           03  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-SYSYM      PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-BUNBENERR       PIC X(200).
      *
           03  WRK-TEISYUTUSAKI    PIC X(01).
      *
           03  WRK-PRTID           PIC X(20).
      *
           03  WRK-LNK-PRTKANRI-SHORI-RENNUM
                                   PIC 9(04).
      *
           03  WRK-SRYYM           PIC X(06).
           03  WRK-SKYYM           PIC X(16).
           03  WRK-SKYYMH          PIC X(05).
           03  WRK-BIRTHDAYH       PIC X(07).
           03  WRK-CHILD-BIRTHDAYH PIC X(07).
           03  WRK-BIKOU.
               05  WRK-BIKOU-O     PIC X(40)   OCCURS  2.
      *
           03  WRK-LINE            PIC 9(05).
           03  WRK-FORMID          PIC X(20).
      *
           03  WRK-9               PIC 9.
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZZZZZZ9        PIC ZZ,ZZZ,ZZ9.
      *
           03  WRK-CONTROL-SEQ-NO  PIC 9(06).
           03  WRK-DATA-SEQ-NO     PIC 9(06).
           03  WRK-SEQ-NO          PIC 9(06).
           03  WRK-PAGE            PIC 9(05).
           03  WRK-OUT             PIC 9(03).
      *
           03  WRK-SCSVINFO-SEQ-NO PIC 9(06).
           03  WRK-SCSVINFO-PTID   PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN
                                   PIC X(01).
      *
      *    医療機関情報
           03  WRK-HOSPNAME        PIC X(80).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(40).
               05  WRK-HOSPNAME2   PIC X(40).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-KAISETUNAME     PIC X(80).
      *
      *     右づめ編集用     
           03  WRK-HENKAN          PIC X(20).
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-HENSYU          PIC X(20).
      *    出力データ編集用
           03  WRK-REC             PIC X(2500).
           03  WRK-REC-LENGTH      PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(2400).
               05  WRK-NUM         PIC ZZZZZZZZZ9.99999. 
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-ST          PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-END         PIC 9(01).
      *
      *    請求年月
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ERRMSG          PIC X(300).
      *
      *    ワーク点数表区分
           03  WRK-TENHYOKBN       PIC  9(01).
      *
      *   ワーク日付変換サブ返却値
           COPY    "CPORCSDAY.INC" REPLACING   //LNK-DAY//
                                   BY          //WRK-LNK-DAY//.
      *
      *    複数枚用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC ZZ9.
               05  WRK-PAGE-HEN    PIC X(01).
               05  WRK-PAGE2       PIC ZZ9.
      *
      *    請求書退避用
       01  TABLE-AREA.
           03  TBL-LIST-MAX        PIC 9(02).
           03  TBL-LIST-OCC        OCCURS  99.
               05  TBL-LIST-PRTDATA
                                   PIC X(10000).
               05  TBL-LIST-FORMID PIC X(20).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
      *
           03  WRK-CONS-SRYYM      PIC X(06)   VALUE   "201104".      
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    医療機関情報（基本）
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報（住所）
           COPY    "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
           COPY    "CPSK1903.INC".
      *
       01  CSV-INFO-REC.
           COPY    "CPCSV-INFO.INC".
      *
      *    クライアント保存
       01  FILE-INFO-REC.
           COPY    "CPFILE-INFO.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
      *
      *    県名取得パラメタ
           COPY    "CPORCSPREF.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(400).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  BUNBENERR
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
                                       BUNBENERR
                                       WRK-PARA-SKYYM
                                       WRK-PARA-TEISYUTUBIKBN
                                       WRK-PARA-TEISYUTUSAKI
                                       WRK-PARA-PRTKBN
                                       WRK-PARA-SYSYM
           END-UNSTRING
      *
           DISPLAY "SKYYM        =" WRK-PARA-SKYYM
           DISPLAY "TEISYUTUBIKBN=" WRK-PARA-TEISYUTUBIKBN
           DISPLAY "TEISYUTUSAKI =" WRK-PARA-TEISYUTUSAKI
           DISPLAY "PRTKBN       =" WRK-PARA-PRTKBN
           DISPLAY "SYSYM        =" WRK-PARA-SYSYM
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBGCHILDBIRTH"
                                   TO  JOB-PGID
           MOVE    "出産育児一時金等代理申請・受取請求書"
                                   TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    3               TO  JOB-STOPFLG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    請求年月編集
           MOVE    WRK-PARA-SKYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-LNK-DAY2-EDTYMD3(1:16)
                                   TO  WRK-SKYYM
           MOVE    WRK-LNK-DAY1-WYMD   (1:5)
                                   TO  WRK-SKYYMH
      *
      *    診療年月（１０日請求時は請求年月の前月）取得
           IF      WRK-PARA-TEISYUTUBIKBN
                                   =   "2"
               MOVE    WRK-PARA-SKYYM  TO  WRK-SRYYM
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE  "61"              TO  LNK-DAY6-IRAI
               MOVE  WRK-SYMD          TO  LNK-DAY6-KIJUN
               MOVE  "2"               TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  WRK-SRYYM
           END-IF    
      *
      *    更新用ファイル名設定
           MOVE    "BUNBUPD"       TO  BUNBENUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BUNBENUPD-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  BUNBENUPD-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BUNBENUPD-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    BUNBENERR       TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BUNBENUPD-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  BUNBENERR
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    システム管理情報取得 
           PERFORM 1001-SYSKANRI-HENSYU-SEC
      *
           IF      WRK-PARA-TEISYUTUBIKBN
                                   =   "1" OR  "2"
               CONTINUE
           ELSE
               MOVE    "提出日区分の設定が誤っています" 
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-TEISYUTUSAKI
                                   =   "0" OR  "1" OR  "2"
               CONTINUE
           ELSE
               MOVE    "提出先の設定が誤っています" 
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-PRTKBN =   "0" OR  "1" OR  "2" OR  "3"
               CONTINUE
           ELSE
               MOVE    "集計票／送付書作成の設定が誤っています" 
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( WRK-PARA-TEISYUTUBIKBN
                                   =   "2"      )
           AND   ( WRK-PARA-TEISYUTUSAKI
                                   =   "1"      )
           AND   ( WRK-PARA-SKYYM  <   "201704" )
               MOVE    "２５日請求時は社保のみは設定できません" 
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    
           IF      WRK-PARA-SKYYM  >=  WRK-CONS-SRYYM
               MOVE    1               TO  WRK-LINE
               MOVE    "HCN61V03"      TO  WRK-FORMID
           ELSE
               MOVE    3               TO  WRK-LINE
               MOVE    "HCN61V01"      TO  WRK-FORMID
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理情報取得 
      *****************************************************************
       1001-SYSKANRI-HENSYU-SEC      SECTION.
      *
           IF      WRK-PARA-SYSYM NOT =   SPACE
               MOVE     WRK-PARA-SYSYM    TO  WRK-SYMD (1:6)
               DISPLAY "SYSYM --> SYMD=" WRK-SYMD
           END-IF
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-SANKANUM
                                           NOT NUMERIC
                   MOVE    ZERO                TO  SYS-1001-SANKANUM
               END-IF
           ELSE
               MOVE    "医療機関情報（基本）が取得できません" 
                                           TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           INITIALIZE                  SYS-1002-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1002-STYUKYMD
                                           SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               MOVE    "医療機関情報（住所）が取得できません" 
                                           TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
               MOVE    SYS-1001-KAISETUNAME
                                           TO  WRK-KAISETUNAME
               MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
               MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *        医療機関編集情報取得
               PERFORM 900-1900-READ-SEC
           END-IF    
      *    
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           OPEN    OUTPUT              UPD-FILE
      *
      *    国保分作成
           IF      WRK-PARA-TEISYUTUSAKI
                                   =   "0" OR  "2"
               MOVE    "2"             TO  WRK-TEISYUTUSAKI
               PERFORM 2001-TEISYUTUSAKI-HENSYU-SEC
           END-IF
      *
      *    社保分作成
      *   （請求年月が２０１７年３月以前は２５日請求時は作成しない）
           IF      WRK-PARA-TEISYUTUSAKI
                                   =   "0" OR  "1"
               IF    ( WRK-PARA-TEISYUTUBIKBN
                                       =   "2"      )
               AND   ( WRK-PARA-SKYYM  <   "201704" )
                   CONTINUE
               ELSE 
                   MOVE    "1"             TO  WRK-TEISYUTUSAKI
                   PERFORM 2001-TEISYUTUSAKI-HENSYU-SEC
               END-IF
           END-IF
      *
           CLOSE   UPD-FILE
      *
      *   分娩管理ＤＢ更新処理
          IF       FLG-ERR         =   ZERO
              IF      CNT-UPDFILE      >   ZERO
                  PERFORM 310-BUNBEN-UPD-SEC
              END-IF    
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    支払機関別編集処理
      *****************************************************************
       2001-TEISYUTUSAKI-HENSYU-SEC    SECTION.
      *
           INITIALIZE                  TABLE-AREA
           INITIALIZE                  SUM-AREA
           MOVE    LOW-VALUE       TO  KEY-AREA
      *
           MOVE    WRK-LNK-PRTKANRI-SHORI-RENNUM
                                   TO  LNK-PRTKANRI-SHORI-RENNUM
           EVALUATE    WRK-PARA-PRTKBN
               WHEN    "0"
                   ADD     3           TO  LNK-PRTKANRI-SHORI-RENNUM
               WHEN    "1"
               WHEN    "2"
                   ADD     2           TO  LNK-PRTKANRI-SHORI-RENNUM
               WHEN    "3"
                   ADD     1           TO  LNK-PRTKANRI-SHORI-RENNUM
           END-EVALUATE
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  WRK-LNK-PRTKANRI-SHORI-RENNUM
      *
           MOVE    ZERO            TO  WRK-PAGE
                                       WRK-OUT
      *
           MOVE    1               TO  WRK-SEQ-NO
           MOVE    WRK-SEQ-NO      TO  WRK-CONTROL-SEQ-NO
      *
           ADD     1               TO  WRK-SEQ-NO
           MOVE    WRK-SEQ-NO      TO  WRK-DATA-SEQ-NO
      *
      *    削除処理
           PERFORM 1001-CSVINFO-CLEAR-SEC
      *
      *    分娩管理読込
           PERFORM 900-BUNBEN-READ-SEC
      *
           PERFORM          UNTIL   FLG-END        =   1
               INITIALIZE                  SUM-TBL-OCC (1)
               MOVE    ZERO            TO  WRK-PAGE
               INITIALIZE                  TABLE-AREA
      *
      *        分娩区分・保険者別編集処理
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM          UNTIL   FLG-END        =   1
                                OR      KEY-NEW    NOT =   KEY-OLD
      *            明細編集処理
                   PERFORM 2002-BODY-PRINT-SEC
               END-PERFORM
      *
      *        合計編集処理
               PERFORM 2003-TERM-PRINT-SEC
      *
      *        帳票格納処理
               PERFORM 2006-TABLE-SET-SEC
      *
      *        帳票印刷処理
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       TBL-LIST-MAX
                       OR      FLG-ERR >       ZERO
                   MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
                   MOVE    IDYY                TO  WRK-PAGE1
                   MOVE    "/"                 TO  WRK-PAGE-HEN
                   MOVE    TBL-LIST-MAX        TO  WRK-PAGE2
                   EVALUATE    WRK-FORMID
                       WHEN    "HCN61V01"
                           MOVE    TBL-LIST-PRTDATA (IDYY)
                                                       TO  HCN61
                           MOVE    WRK-HEN-PAGE-AREA   TO  HCN61-PAGE
                       WHEN    "HCN61V03"
                           MOVE    TBL-LIST-PRTDATA (IDYY)
                                                       TO  HCN61V02
                           MOVE    WRK-HEN-PAGE-AREA   TO  HCN61V02-PAGE
                   END-EVALUATE
                   MOVE    WRK-FORMID          TO  WRK-PRTID
                   PERFORM 2004-PRINT-OUT-SEC
               END-PERFORM
      *
               IF      FLG-END         =   1
      *            ＣＳＶ（データレコード）編集処理
                   PERFORM 2001-CSV-DATA-7111-RECORD-SEC
      *
      *            ＣＳＶ（コントロールレコード）編集処理
                   PERFORM 2001-CSV-CONTROL-RECORD-SEC
      *
      *            ＣＳＶ（エンドレコード）編集処理
                   PERFORM 2001-CSV-END-RECORD-SEC
      *
      *            集計票、送付書編集処理
                   PERFORM 2005-HKNJANUM-PRINT-SEC
               END-IF 
           END-PERFORM
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ファイル保存情報更新
           PERFORM 3001-FILE-INFO-INSERT-SEC
           .
       2001-TEISYUTUSAKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       1001-CSVINFO-CLEAR-SEC          SECTION.
      *
      *    クライアント保存削除処理
           MOVE    SPACE           TO  FILE-INFO-REC
           INITIALIZE                  FILE-INFO-REC
           MOVE    SPA-HOSPNUM     TO  FILE-INFO-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  FILE-INFO-TBL-KEY
           MOVE    WRK-SRYYM       TO  FILE-INFO-SRYYM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  FILE-INFO-SHELLID
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  FILE-INFO-SHORI-RENNUM
               WHEN    "2"
                   MOVE    2               TO  FILE-INFO-SHORI-RENNUM
           END-EVALUATE
      *
           MOVE    FILE-INFO-REC   TO  MCPDATA-REC
           MOVE    "tbl_file_info" TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           MOVE    "DBDELETE"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＣＳＶＤＢ削除処理
           INITIALIZE                  CSV-INFO-REC
           MOVE    SPA-HOSPNUM     TO  CSV-INFO-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  CSV-INFO-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  CSV-INFO-SHELLID
           MOVE    WRK-SRYYM       TO  CSV-INFO-SRYYM
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  CSV-INFO-SHORI-RENNUM
               WHEN    "2"
                   MOVE    2               TO  CSV-INFO-SHORI-RENNUM
           END-EVALUATE
           MOVE    CSV-INFO-REC    TO  MCPDATA-REC
           MOVE    "tbl_csv_info"  TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           MOVE    "DBDELETE"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
     *    
           .
       1001-CSVINFO-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       2001-HEAD-PRINT-SEC                SECTION.
      *     
           ADD     1               TO  WRK-PAGE
                                       CNT-PAGE
                                       SUM-PAGE (1)
                                       SUM-PAGE (2)
      *
           EVALUATE    WRK-FORMID
               WHEN    "HCN61V01"
                   MOVE    SPACE           TO  HCN61
      *
                   MOVE    KEY-N-HKNJANUM  TO  WRK-HENKAN
                   MOVE    8               TO  WRK-LENGTH 
                   PERFORM 5003-DATA-RIGHT-HENSYU-SEC
                   MOVE    WRK-HENKAN      TO  HCN61-HKNJANUM
      *
                   MOVE    WRK-SKYYM       TO  HCN61-SKYYM
                   EVALUATE    KEY-N-BUNBENKBN
                       WHEN    1
                           MOVE    "○"        TO  HCN61-BUNBENKBN (1)
                       WHEN    2
                           MOVE    "○"        TO  HCN61-BUNBENKBN (2)
                   END-EVALUATE
                   MOVE    SYS-1001-PREFNUM
                                           TO  HCN61-HOSPCD (1:2)
                   MOVE    "1"             TO  HCN61-HOSPCD (3:1) 
                   MOVE    SYS-1001-HOSPCD TO  HCN61-HOSPCD (4:7)
                   IF      SYS-1001-SANKANUM
                                           =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    SYS-1001-SANKANUM
                                               TO  HCN61-SANKANUM
                   END-IF
                   MOVE    WRK-HOSPADRS1   TO  HCN61-HOSPADRS (1)
                   MOVE    WRK-HOSPADRS2   TO  HCN61-HOSPADRS (2)
                   MOVE    WRK-HOSPNAME1   TO  HCN61-HOSPNAME (1)
                   MOVE    WRK-HOSPNAME2   TO  HCN61-HOSPNAME (2)
      *
                   MOVE    WRK-PAGE        TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HCN61-PAGE
               WHEN    "HCN61V03"
                   MOVE    SPACE           TO  HCN61V02
      *
                   MOVE    KEY-N-HKNJANUM  TO  WRK-HENKAN
                   MOVE    8               TO  WRK-LENGTH 
                   PERFORM 5003-DATA-RIGHT-HENSYU-SEC
                   MOVE    WRK-HENKAN      TO  HCN61V02-HKNJANUM
      *
                   MOVE    WRK-SKYYM       TO  HCN61V02-SKYYM
                   EVALUATE    KEY-N-BUNBENKBN
                       WHEN    1
                           MOVE    "○"        TO  HCN61V02-BUNBENKBN(1)
                       WHEN    2
                           MOVE    "○"        TO  HCN61V02-BUNBENKBN(2)
                   END-EVALUATE
                   MOVE    SYS-1001-PREFNUM
                                           TO  HCN61V02-HOSPCD (1:2)
                   MOVE    "1"             TO  HCN61V02-HOSPCD (3:1) 
                   MOVE    SYS-1001-HOSPCD TO  HCN61V02-HOSPCD (4:7)
                   IF      SYS-1001-SANKANUM
                                           =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    SYS-1001-SANKANUM
                                               TO  HCN61V02-SANKANUM
                   END-IF
                   MOVE    WRK-HOSPADRS1   TO  HCN61V02-HOSPADRS (1)
                   MOVE    WRK-HOSPADRS2   TO  HCN61V02-HOSPADRS (2)
                   MOVE    WRK-HOSPNAME1   TO  HCN61V02-HOSPNAME (1)
                   MOVE    WRK-HOSPNAME2   TO  HCN61V02-HOSPNAME (2)
      *
                   MOVE    WRK-PAGE        TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HCN61V02-PAGE
           END-EVALUATE
      *
           MOVE    ZERO            TO  CNT-LINE
           .
       2001-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       2002-BODY-PRINT-SEC          SECTION.
      *
      *    見出し編集処理
           PERFORM 2001-HEAD-PRINT-SEC
      *
           PERFORM          UNTIL   FLG-END        =   1
                            OR      KEY-NEW    NOT =   KEY-OLD
                            OR      CNT-LINE       >=  WRK-LINE
      *
               ADD     1               TO  CNT-LINE
      *
               INITIALIZE                  WRK-BIRTHDAYH
                                           WRK-CHILD-BIRTHDAYH
      *
      *        正常分娩時の一部負担金等はその他とする
               IF    ( BUNBEN-BUNBENKBN    =   "1"  )
               AND   ( BUNBEN-MONEY (10)   >   ZERO )
                   ADD     BUNBEN-MONEY (10)   TO  BUNBEN-MONEY (9)
                   MOVE    ZERO                TO  BUNBEN-MONEY (10)
               END-IF
      *
      ******** INITIALIZE                      PTINF-REC
      ******** MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
      ******** MOVE    BUNBEN-PTID         TO  PTINF-PTID
      ******** PERFORM 900-PTINF-READ-SEC
      *
      *        明細様式別編集処理
               PERFORM 20021-BODY-HENSYU-SEC
      *
               ADD     1                   TO  SUM-CNT      (1)
                                               SUM-CNT      (2)
               ADD     BUNBEN-BIRTHCNT     TO  SUM-BIRTHCNT (1)
                                               SUM-BIRTHCNT (2)
               ADD     BUNBEN-MONEY (12)   TO  SUM-MONEY    (1)
                                               SUM-MONEY    (2)
      *
      *        ＣＳＶ（データレコード）編集処理
               PERFORM 2001-CSV-DATA-7121-RECORD-SEC
      *
      *        更新用ファイル出力処理
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *        分娩管理読込   
               PERFORM 900-BUNBEN-READ-N-SEC
           END-PERFORM
      *
      *    帳票印刷処理
           IF      KEY-NEW         =   KEY-OLD
      *        帳票格納処理
               PERFORM 2006-TABLE-SET-SEC
           END-IF
 
           .
       2002-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *        明細様式別編集処理
      *****************************************************************
       20021-BODY-HENSYU-SEC           SECTION.
      *
           EVALUATE    WRK-FORMID
             WHEN    "HCN61V01"
               EVALUATE    BUNBEN-KANYUKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61-TEISYUTUSAKI
                                                           (CNT-LINE 1)
                   WHEN    "2"
                       MOVE    "○"            TO  HCN61-TEISYUTUSAKI
                                                           (CNT-LINE 2)
               END-EVALUATE
               EVALUATE    BUNBEN-HONKZKKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61-HONKZKKBN
                                                           (CNT-LINE 1)
                   WHEN    "5"
                       MOVE    "○"            TO  HCN61-HONKZKKBN
                                                           (CNT-LINE 2)
               END-EVALUATE
               IF      BUNBEN-KIGO (23:)   =   SPACE
                   MOVE    BUNBEN-KIGO         TO  HCN61-KIGO (CNT-LINE)
               ELSE
                   MOVE    BUNBEN-KIGO (1:20)  TO  HCN61-KIGO2
                                                           (CNT-LINE 1)
                   MOVE    BUNBEN-KIGO (21:)   TO  HCN61-KIGO2
                                                           (CNT-LINE 2)
               END-IF
               IF      BUNBEN-NUM  (23:)   =   SPACE
                   MOVE    BUNBEN-NUM          TO  HCN61-NUM  (CNT-LINE)
               ELSE
                   MOVE    BUNBEN-NUM (1:20)   TO  HCN61-NUM2
                                                           (CNT-LINE 1)
                   MOVE    BUNBEN-NUM (21:)    TO  HCN61-NUM2
                                                           (CNT-LINE 2)
               END-IF
               IF      BUNBEN-NAME (35:)   =   SPACE
                   MOVE    BUNBEN-NAME         TO  HCN61-NAME (CNT-LINE)
               ELSE
                   MOVE    BUNBEN-NAME         TO  HCN61-NAME2(CNT-LINE)
               END-IF
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-LNK-DAY1-WYMD   TO  WRK-BIRTHDAYH
               EVALUATE    WRK-LNK-DAY2-EDTYMD1-G
                   WHEN    "S"
                       MOVE    "○"            TO  HCN61-BIRTHG
                                                           (CNT-LINE 1)
                   WHEN    "H"
                       MOVE    "○"            TO  HCN61-BIRTHG
                                                           (CNT-LINE 2)
               END-EVALUATE
               MOVE    WRK-LNK-DAY2-EDTYMD1-Y
                                           TO  HCN61-BIRTHY(CNT-LINE)
               MOVE    WRK-LNK-DAY2-EDTYMD1-M
                                           TO  HCN61-BIRTHM(CNT-LINE)
               MOVE    WRK-LNK-DAY2-EDTYMD1-D
                                           TO  HCN61-BIRTHD(CNT-LINE)
               MOVE    BUNBEN-ZAIWEEK      TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCN61-ZAIWEEK
                                                           (CNT-LINE)
               MOVE    BUNBEN-BIRTHDAY     TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-LNK-DAY1-WYMD   TO  WRK-CHILD-BIRTHDAYH
               MOVE    WRK-LNK-DAY2-EDTYMD1-Y
                                           TO  HCN61-CBIRTHY (CNT-LINE)
               MOVE    WRK-LNK-DAY2-EDTYMD1-M
                                           TO  HCN61-CBIRTHM (CNT-LINE)
               MOVE    WRK-LNK-DAY2-EDTYMD1-D
                                           TO  HCN61-CBIRTHD (CNT-LINE)
               EVALUATE    BUNBEN-SIZANKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61-SIZANKBN
                                                           (CNT-LINE 1)
                   WHEN    "2"
                       MOVE    "○"            TO  HCN61-SIZANKBN
                                                           (CNT-LINE 2)
                   WHEN    "3"
                       MOVE    "○"            TO  HCN61-SIZANKBN
                                                           (CNT-LINE 3)
               END-EVALUATE
               MOVE    BUNBEN-BIRTHCNT     TO  WRK-9
               MOVE    WRK-9               TO  HCN61-BIRTHCNT (CNT-LINE)
               MOVE    BUNBEN-NYUIN-DAY    TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  HCN61-NYUINDAY (CNT-LINE)
               EVALUATE    BUNBEN-SANKAKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61-SANKAKBN
                                                           (CNT-LINE 1)
                   WHEN    "2"
                       MOVE    "○"            TO  HCN61-SANKAKBN
                                                           (CNT-LINE 2)
                   WHEN    "3"
                       MOVE    "○"            TO  HCN61-SANKAKBN
                                                           (CNT-LINE 3)
               END-EVALUATE
               MOVE    BUNBEN-MONEY (1)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-NYU-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (2)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-RMSGK-MONEY
                                                           (CNT-LINE)
               IF      BUNBEN-BUNBENKBN    =   "2"
                   MOVE    BUNBEN-MONEY (3)    TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9        TO  HCN61-KAIJO-MONEY
                                                           (CNT-LINE)
               ELSE
                   MOVE    "　−　　　"        TO  HCN61-KAIJO-MONEY
                                                           (CNT-LINE)
               END-IF
               IF      BUNBEN-BUNBENKBN    =   "1"
                   MOVE    BUNBEN-MONEY (4)    TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9        TO  HCN61-BUNBEN-MONEY
                                                           (CNT-LINE)
               ELSE
                   MOVE    "　−　　　"        TO  HCN61-BUNBEN-MONEY
                                                           (CNT-LINE)
               END-IF
               MOVE    BUNBEN-MONEY (5)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-SHIN-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (6)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-KNS-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (7)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-SYC-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (8)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-SNK-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (9)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-ETC-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (10)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-ICBFTN-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (11)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-NINPUFTN-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-MONEY (12)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61-DAIRI-MONEY
                                                           (CNT-LINE)
               MOVE    BUNBEN-BIKOU        TO  WRK-BIKOU
               IF      WRK-BIKOU-O (2)     =   SPACE
                   MOVE    WRK-BIKOU       TO  HCN61-BIKOU (CNT-LINE)
               ELSE
                   MOVE    WRK-BIKOU-O (1) TO  HCN61-BIKOU2(CNT-LINE 1)
                   MOVE    WRK-BIKOU-O (2) TO  HCN61-BIKOU2(CNT-LINE 2)
               END-IF
             WHEN    "HCN61V03"
               EVALUATE    BUNBEN-KANYUKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61V02-TEISYUTUSAKI
                                                           (1)
                   WHEN    "2"
                       MOVE    "○"            TO  HCN61V02-TEISYUTUSAKI
                                                           (2)
               END-EVALUATE
               EVALUATE    BUNBEN-HONKZKKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61V02-HONKZKKBN
                                                           (1)
                   WHEN    "5"
                       MOVE    "○"            TO  HCN61V02-HONKZKKBN
                                                           (2)
               END-EVALUATE
               IF      BUNBEN-KIGO (23:)   =   SPACE
                   MOVE    BUNBEN-KIGO         TO  HCN61V02-KIGO
               ELSE
                   MOVE    BUNBEN-KIGO (1:20)  TO  HCN61V02-KIGO2  (1)
                   MOVE    BUNBEN-KIGO (21:)   TO  HCN61V02-KIGO2  (2)
               END-IF
               IF      BUNBEN-NUM  (23:)   =   SPACE
                   MOVE    BUNBEN-NUM          TO  HCN61V02-NUM
               ELSE
                   MOVE    BUNBEN-NUM (1:20)   TO  HCN61V02-NUM2   (1)
                   MOVE    BUNBEN-NUM (21:)    TO  HCN61V02-NUM2   (2)
               END-IF
               IF      BUNBEN-NAME (35:)   =   SPACE
                   MOVE    BUNBEN-NAME         TO  HCN61V02-NAME
               ELSE
                   MOVE    BUNBEN-NAME         TO  HCN61V02-NAME2
               END-IF
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-LNK-DAY1-WYMD   TO  WRK-BIRTHDAYH
               EVALUATE    WRK-LNK-DAY2-EDTYMD1-G
                   WHEN    "S"
                       MOVE    "○"            TO  HCN61V02-BIRTHG (1)
                   WHEN    "H"
                       MOVE    "○"            TO  HCN61V02-BIRTHG (2)
               END-EVALUATE
               MOVE    WRK-LNK-DAY2-EDTYMD1-Y
                                           TO  HCN61V02-BIRTHY
               MOVE    WRK-LNK-DAY2-EDTYMD1-M
                                           TO  HCN61V02-BIRTHM
               MOVE    WRK-LNK-DAY2-EDTYMD1-D
                                           TO  HCN61V02-BIRTHD
               MOVE    BUNBEN-ZAIWEEK      TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCN61V02-ZAIWEEK
               MOVE    BUNBEN-BIRTHDAY     TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-LNK-DAY1-WYMD   TO  WRK-CHILD-BIRTHDAYH
               MOVE    WRK-LNK-DAY2-EDTYMD1-Y
                                           TO  HCN61V02-CBIRTHY
               MOVE    WRK-LNK-DAY2-EDTYMD1-M
                                           TO  HCN61V02-CBIRTHM
               MOVE    WRK-LNK-DAY2-EDTYMD1-D
                                           TO  HCN61V02-CBIRTHD
               MOVE    WRK-LNK-DAY1-GENGO  TO  HCN61V02-CBIRTHGG1 (1:1)
               MOVE    ":"                 TO  HCN61V02-CBIRTHGG1 (2:1)
               MOVE    WRK-LNK-DAY2-EDTYMD2-G
                                           TO  HCN61V02-CBIRTHGG2
               EVALUATE    BUNBEN-SIZANKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61V02-SIZANKBN
                                                           (1)
                   WHEN    "2"
                       MOVE    "○"            TO  HCN61V02-SIZANKBN
                                                           (2)
                   WHEN    "3"
                       MOVE    "○"            TO  HCN61V02-SIZANKBN
                                                           (3)
               END-EVALUATE
               MOVE    BUNBEN-BIRTHCNT     TO  WRK-9
               MOVE    WRK-9               TO  HCN61V02-BIRTHCNT
               MOVE    BUNBEN-NYUIN-DAY    TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  HCN61V02-NYUINDAY 
               EVALUATE    BUNBEN-SANKAKBN
                   WHEN    "1"
                       MOVE    "○"            TO  HCN61V02-SANKAKBN
                                                           (1)
                   WHEN    "2"
                       MOVE    "○"            TO  HCN61V02-SANKAKBN
                                                           (2)
                   WHEN    "3"
                       MOVE    "○"            TO  HCN61V02-SANKAKBN
                                                           (3)
               END-EVALUATE
               MOVE    BUNBEN-MONEY (1)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-NYU-MONEY
               MOVE    BUNBEN-MONEY (2)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-RMSGK-MONEY
               IF      BUNBEN-BUNBENKBN    =   "2"
                   MOVE    BUNBEN-MONEY (3)    TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-KAIJO-MONEY
               ELSE
                   MOVE    "　−　　　"        TO  HCN61V02-KAIJO-MONEY
               END-IF
               IF      BUNBEN-BUNBENKBN    =   "1"
                   MOVE    BUNBEN-MONEY (4)    TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-BUNBEN-MONEY
               ELSE
                   MOVE    "　−　　　"        TO  HCN61V02-BUNBEN-MONEY
               END-IF
               MOVE    BUNBEN-MONEY (5)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-SHIN-MONEY
               MOVE    BUNBEN-MONEY (6)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-KNS-MONEY
               MOVE    BUNBEN-MONEY (7)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-SYC-MONEY
               MOVE    BUNBEN-MONEY (8)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-SNK-MONEY
               MOVE    BUNBEN-MONEY (9)    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-ETC-MONEY
               MOVE    BUNBEN-MONEY (10)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-ICBFTN-MONEY
               MOVE    BUNBEN-MONEY (11)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-NINPUFTN-MONEY
               MOVE    BUNBEN-MONEY (12)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCN61V02-DAIRI-MONEY
               MOVE    BUNBEN-BIKOU        TO  WRK-BIKOU
               IF      WRK-BIKOU-O (2)     =   SPACE
                   MOVE    WRK-BIKOU           TO  HCN61V02-BIKOU
               ELSE
                   MOVE    WRK-BIKOU-O (1)     TO  HCN61V02-BIKOU2 (1)
                   MOVE    WRK-BIKOU-O (2)     TO  HCN61V02-BIKOU2 (2)
               END-IF
           END-EVALUATE
      *
           .
       20021-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    BUNBEN-REC          TO  BUNBENX-REC
      *****INITIALIZE                      BUNBENX-XX
           WRITE   BUNBENX-REC
      *
           IF      STS-BUNBENUPD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-BUNBENERR
               STRING "更新用ファイル 書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-BUNBENUPD           DELIMITED  BY  SIZE
                                               INTO    WRK-BUNBENERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF 
      *
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       2003-TERM-PRINT-SEC                SECTION.
      *
           EVALUATE    WRK-FORMID
               WHEN    "HCN61V01"
                   MOVE    SUM-CNT     (1) TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HCN61-GCNT
                   MOVE    SUM-BIRTHCNT(1) TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HCN61-GBIRTHCNT
                   MOVE    SUM-MONEY   (1) TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9    TO  HCN61-GDAIRI-MONEY
               WHEN    "HCN61V03"
                   MOVE    SUM-CNT     (1) TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HCN61V02-GCNT
                   MOVE    SUM-BIRTHCNT(1) TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HCN61V02-GBIRTHCNT
                   MOVE    SUM-MONEY   (1) TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9    TO  HCN61V02-GDAIRI-MONEY
           END-EVALUATE
           .
      *
       2003-TERM-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計票、送付書編集処理
      *****************************************************************
       2005-HKNJANUM-PRINT-SEC     SECTION.
      *
      *    送付書作成
           IF      WRK-PARA-PRTKBN =   "0" OR  "2"
               COMPUTE LNK-PRTKANRI-SHORI-RENNUM
                                   =   LNK-PRTKANRI-SHORI-RENNUM
                                   -   1
      *
               MOVE    SPACE           TO  HCN64
      *
               INITIALIZE                  SPREF-AREA
               CALL    "ORCSPREF"      USING   SPREF-AREA
                                               SPA-AREA
      *
               MOVE    SYS-1001-PREFNUM    TO  IDX
               EVALUATE   WRK-TEISYUTUSAKI
                   WHEN    "1"
      *                東京、大阪、京都は「府、都」を消去
                       IF      IDX         =   13  OR  26  OR  27
                           MOVE    SPACE   TO  SPREF-PREFNAME2 (IDX)(5:)
                       END-IF
                       STRING  "社会保険診療報酬支払基金"
                                           DELIMITED    BY  SIZE
                               SPREF-PREFNAME2 (IDX)
                                           DELIMITED    BY  SPACE
                               "支部　御中"
                                           DELIMITED    BY  SIZE
                                           INTO    HCN64-TEISYUTUSAKI
                       END-STRING
                   WHEN    "2"
                       STRING  SPREF-PREFNAME1 (IDX)
                                           DELIMITED    BY  SPACE
                               "国民健康保険団体連合会　御中"
                                           DELIMITED    BY  SIZE
                                           INTO    HCN64-TEISYUTUSAKI
                       END-STRING
               END-EVALUATE
      *
      *        所在地
               MOVE    WRK-HOSPADRS    TO  HCN64-HOSPADRS
               INSPECT HCN64-HOSPADRS  REPLACING   ALL "  "    BY  "　"
      *
      *        開設者
               MOVE    WRK-KAISETUNAME TO  HCN64-KAISETUNAME
      *
      *        電話番号
               MOVE    WRK-HOSPTEL     TO  HCN64-HOSPTEL
      *
      *        医療機関等コード
               MOVE    SYS-1001-PREFNUM
                                       TO  HCN64-HOSPCD (1:2)
               MOVE    "1"             TO  HCN64-HOSPCD (3:1) 
               MOVE    SYS-1001-HOSPCD TO  HCN64-HOSPCD (4:7)
      *
      *        医療機関名称
               MOVE    WRK-HOSPNAME1   TO  HCN64-HOSPNAME (1:40)
               MOVE    WRK-HOSPNAME2   TO  HCN64-HOSPNAME (51:40)
               INSPECT HCN64-HOSPNAME  REPLACING   ALL "  "    BY  "　"
      *
      *        分娩機関管理番号・施設等区分
               EVALUATE    SYS-1001-HOSPSBT
                   WHEN    "1"
                       MOVE    "○"        TO  HCN64-HOSPSBT (1)
                   WHEN    "2"
                       MOVE    "○"        TO  HCN64-HOSPSBT (2)
               END-EVALUATE
               IF      SYS-1001-SANKANUM
                                       =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SYS-1001-SANKANUM
                                          TO  HCN64-SANKANUM
      *************EVALUATE    SYS-1001-SANKANUM (3:1)
      *************    WHEN    "1"
      *************        MOVE    "○"        TO  HCN64-HOSPSBT (1)
      *************    WHEN    "2"
      *************        MOVE    "○"        TO  HCN64-HOSPSBT (2)
      *************    WHEN    "3"
      *************        MOVE    "○"        TO  HCN64-HOSPSBT (3)
      *************END-EVALUATE
               END-IF
      *
      *        請求月分
               MOVE    WRK-SKYYM       TO  HCN64-SKYYM
      *
      *        提出年月日
               IF      WRK-PARA-SKYYM  <=  "201904"
                   MOVE    "平成"          TO  HCN64-TEIG
               ELSE
                   MOVE    "令和"          TO  HCN64-TEIG
               END-IF
      *
               ADD     1               TO  CNT-PAGE
      *
      *        帳票印刷処理
               MOVE   "HCN64"          TO  WRK-PRTID
               PERFORM 2004-PRINT-OUT-SEC
           END-IF
      *
      *    集計票作成
           IF      WRK-PARA-PRTKBN =   "0" OR  "1"
               COMPUTE LNK-PRTKANRI-SHORI-RENNUM
                                   =   LNK-PRTKANRI-SHORI-RENNUM
                                   -   1
      *
               MOVE    SPACE           TO  HCN65
      *
               INITIALIZE                  SPREF-AREA
               CALL    "ORCSPREF"      USING   SPREF-AREA
                                               SPA-AREA
      *
               MOVE    SYS-1001-PREFNUM    TO  IDX
               EVALUATE   WRK-TEISYUTUSAKI
                   WHEN    "1"
      *                東京、大阪、京都は「府、都」を消去
                       IF      IDX         =   13  OR  26  OR  27
                           MOVE    SPACE   TO  SPREF-PREFNAME2 (IDX)(5:)
                       END-IF
                       STRING  "社会保険診療報酬支払基金"
                                           DELIMITED    BY  SIZE
                               SPREF-PREFNAME2 (IDX)
                                           DELIMITED    BY  SPACE
                               "支部　御中"
                                           DELIMITED    BY  SIZE
                                           INTO    HCN65-TEISYUTUSAKI
                       END-STRING
                   WHEN    "2"
                       STRING  SPREF-PREFNAME1 (IDX)
                                           DELIMITED    BY  SPACE
                               "国民健康保険団体連合会　御中"
                                           DELIMITED    BY  SIZE
                                           INTO    HCN65-TEISYUTUSAKI
                       END-STRING
               END-EVALUATE
      *
      *        所在地
               MOVE    WRK-HOSPADRS    TO  HCN65-HOSPADRS
               INSPECT HCN65-HOSPADRS  REPLACING   ALL "  "    BY  "　"
      *
      *        開設者
               MOVE    WRK-KAISETUNAME TO  HCN65-KAISETUNAME
      *
      *        電話番号
               MOVE    WRK-HOSPTEL     TO  HCN65-HOSPTEL
      *
      *        医療機関等コード
               MOVE    SYS-1001-PREFNUM
                                       TO  HCN65-HOSPCD (1:2)
               MOVE    "1"             TO  HCN65-HOSPCD (3:1) 
               MOVE    SYS-1001-HOSPCD TO  HCN65-HOSPCD (4:7)
      *
      *        医療機関名称
               MOVE    WRK-HOSPNAME1   TO  HCN65-HOSPNAME (1:40)
               MOVE    WRK-HOSPNAME2   TO  HCN65-HOSPNAME (51:40)
               INSPECT HCN65-HOSPNAME  REPLACING   ALL "  "    BY  "　"
      *
      *        分娩機関管理番号・施設等区分
               EVALUATE    SYS-1001-HOSPSBT
                   WHEN    "1"
                       MOVE    "○"        TO  HCN65-HOSPSBT (1)
                   WHEN    "2"
                       MOVE    "○"        TO  HCN65-HOSPSBT (2)
               END-EVALUATE
               IF      SYS-1001-SANKANUM
                                       =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SYS-1001-SANKANUM
                                          TO  HCN65-SANKANUM
      *************EVALUATE    SYS-1001-SANKANUM (3:1)
      *************    WHEN    "1"
      *************        MOVE    "○"        TO  HCN65-HOSPSBT (1)
      *************    WHEN    "2"
      *************        MOVE    "○"        TO  HCN65-HOSPSBT (2)
      *************    WHEN    "3"
      *************        MOVE    "○"        TO  HCN65-HOSPSBT (3)
      *************END-EVALUATE
               END-IF
      *
      *        請求月分
               MOVE    WRK-SKYYM       TO  HCN65-SKYYM
      *
      *        請求書枚数
               MOVE    SUM-PAGE    (2) TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9    TO  HCN65-PAGE
      *
      *        取扱件数
               MOVE    SUM-CNT     (2) TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9    TO  HCN65-KENSU
      *
      *        出産数
               MOVE    SUM-BIRTHCNT(2) TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9    TO  HCN65-BIRTHCNT
      *
      *        代理受取額合計
               MOVE    SUM-MONEY   (2) TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9    TO  HCN65-DAIRI-MONEY
      *
      *        提出年月日
               IF      WRK-PARA-SKYYM  <=  "201904"
                   MOVE    "平成"          TO  HCN65-TEIG
               ELSE
                   MOVE    "令和"          TO  HCN65-TEIG
               END-IF
      *
               ADD     1               TO  CNT-PAGE
      *
      *        帳票印刷処理
               MOVE   "HCN65"          TO  WRK-PRTID
               PERFORM 2004-PRINT-OUT-SEC
           END-IF
           .
       2005-HKNJANUM-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2004-PRINT-OUT-SEC              SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           EVALUATE    WRK-PRTID
               WHEN    "HCN61V01"
                   MOVE   "HCN61V01.red"   TO  SPRT-PRTID
                   MOVE    HCN61           TO  SPRT-PRTDATA
                   EVALUATE    WRK-TEISYUTUSAKI
                       WHEN    "1"
                           MOVE
                          "（社保）出産育児一時金等代理申請・受取請求書"
                                                   TO  SPRT-TITLE
                       WHEN    "2"
                           MOVE
                          "（国保）出産育児一時金等代理申請・受取請求書"
                                                    TO  SPRT-TITLE
                   END-EVALUATE
               WHEN    "HCN61V03"
                   MOVE   "HCN61V03.red"   TO  SPRT-PRTID
                   MOVE    HCN61V02        TO  SPRT-PRTDATA
                   EVALUATE    WRK-TEISYUTUSAKI
                       WHEN    "1"
                           MOVE
                          "（社保）出産育児一時金等代理申請・受取請求書"
                                                   TO  SPRT-TITLE
                       WHEN    "2"
                           MOVE
                          "（国保）出産育児一時金等代理申請・受取請求書"
                                                    TO  SPRT-TITLE
                   END-EVALUATE
               WHEN    "HCN64"
                   MOVE   "HCN64.red"      TO  SPRT-PRTID
                   MOVE    HCN64           TO  SPRT-PRTDATA
                   EVALUATE    WRK-TEISYUTUSAKI
                       WHEN    "1"
                           MOVE
                               "（社保）送付書"    TO  SPRT-TITLE
                       WHEN    "2"
                           MOVE
                               "（国保）送付書"    TO  SPRT-TITLE
                   END-EVALUATE
               WHEN    "HCN65"
                   MOVE   "HCN65.red"      TO  SPRT-PRTID
                   MOVE    HCN65           TO  SPRT-PRTDATA
                   EVALUATE    WRK-TEISYUTUSAKI
                       WHEN    "1"
                           MOVE
                               "（社保）集計票"    TO  SPRT-TITLE
                       WHEN    "2"
                           MOVE
                               "（国保）集計票"    TO  SPRT-TITLE
                   END-EVALUATE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"       USING
                                   ORCSPRTAREA
                                   SPA-AREA
           IF      SPRT-RETURN     =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                      TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       2004-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       2006-TABLE-SET-SEC          SECTION.
      *
           ADD     1               TO  TBL-LIST-MAX
      *
           EVALUATE    WRK-FORMID
               WHEN    "HCN61V01"
                   MOVE    "HCN61V01.red"  TO
                                       TBL-LIST-FORMID  (TBL-LIST-MAX)
                   MOVE    HCN61           TO
                                       TBL-LIST-PRTDATA (TBL-LIST-MAX)
               WHEN    "HCN61V03"
                   MOVE    "HCN61V03.red"  TO
                                       TBL-LIST-FORMID  (TBL-LIST-MAX)
                   MOVE    HCN61V02        TO
                                       TBL-LIST-PRTDATA (TBL-LIST-MAX)
           END-EVALUATE
      *
           .
       2006-TABLE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（コントロールレコード）編集処理
      *****************************************************************
       2001-CSV-CONTROL-RECORD-SEC                SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    レコード識別          
           MOVE    "1,"            TO  WRK-REC
           MOVE    2               TO  WRK-REC-LENGTH
      *    提出先
           IF      WRK-PARA-TEISYUTUBIKBN
                                   =   "1"
               MOVE    WRK-TEISYUTUSAKI
                                       TO  WRK-DATA
           ELSE
               IF      WRK-TEISYUTUSAKI    =   "2"
                   MOVE    "3"             TO  WRK-DATA
               ELSE
                   MOVE    WRK-TEISYUTUSAKI
                                           TO  WRK-DATA
               END-IF
           END-IF 
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レコード番号
           MOVE    WRK-CONTROL-SEQ-NO
                                   TO  WRK-NUM
           MOVE    9               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    ボリューム番号
           MOVE    "0"             TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レコード件数
           MOVE    SUM-RECORD-CNT  TO  WRK-NUM
           MOVE    9               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    医療機関等番号
           MOVE    SYS-1001-PREFNUM
                                   TO  WRK-DATA (1:2)
           MOVE    SYS-1001-TENHYOKBN
                                   TO  WRK-DATA (3:1) 
           MOVE    SYS-1001-HOSPCD TO  WRK-DATA (4:7)
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    分娩機関管理番号
           IF      SYS-1001-SANKANUM
                                   =   ZERO
               CONTINUE
           ELSE
               MOVE    SYS-1001-SANKANUM
                                       TO  WRK-DATA
               MOVE    10              TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    請求年月
           MOVE    WRK-SKYYMH      TO  WRK-DATA
           MOVE    5               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ファイル管理番号
      **** MOVE    0               TO  WRK-NUM
      **** MOVE    6               TO  WRK-NAGASA
           MOVE    1               TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    ZERO            TO  WRK-SCSVINFO-PTID
           MOVE    SPACE           TO  WRK-SCSVINFO-NYUGAIKBN
           MOVE    1               TO  WRK-SCSVINFO-SEQ-NO
           PERFORM 500-CSVINFO-SEC
           .
       2001-CSV-CONTROL-RECORD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード・請求書情報）編集処理
      *****************************************************************
       2001-CSV-DATA-7111-RECORD-SEC SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    レコード識別          
           MOVE    "2,"            TO  WRK-REC
           MOVE    2               TO  WRK-REC-LENGTH
      *    レコード番号
           MOVE    WRK-DATA-SEQ-NO TO  WRK-NUM
           MOVE    9               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    交換情報識別番号
           IF      WRK-PARA-TEISYUTUBIKBN
                                   =   "1"
               MOVE    "7111"          TO  WRK-DATA
           ELSE
               MOVE    "7112"          TO  WRK-DATA
           END-IF
           MOVE    4               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    請求年月
           MOVE    WRK-SKYYMH      TO  WRK-DATA
           MOVE    5               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    医療機関等番号
           MOVE    SYS-1001-PREFNUM
                                   TO  WRK-DATA (1:2)
           MOVE    SYS-1001-TENHYOKBN
                                   TO  WRK-DATA (3:1) 
           MOVE    SYS-1001-HOSPCD TO  WRK-DATA (4:7)
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    分娩機関管理番号
           IF      SYS-1001-SANKANUM
                                   =   ZERO
               CONTINUE
           ELSE
               MOVE    SYS-1001-SANKANUM
                                       TO  WRK-DATA
               MOVE    10              TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    件数
           MOVE    SUM-CNT     (2) TO  WRK-NUM
           MOVE    6               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    出産数
           MOVE    SUM-BIRTHCNT(2) TO  WRK-NUM
           MOVE    6               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    出産育児一時金等代理受取額合計
           MOVE    SUM-MONEY   (2) TO  WRK-NUM
           MOVE    12              TO  WRK-NAGASA
           MOVE    1               TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    BUNBEN-PTID     TO  WRK-SCSVINFO-PTID
           MOVE    SPACE           TO  WRK-SCSVINFO-NYUGAIKBN
           MOVE    WRK-DATA-SEQ-NO TO  WRK-SCSVINFO-SEQ-NO
           PERFORM 500-CSVINFO-SEC
      *
           ADD     1               TO  SUM-RECORD-CNT
           .
       2001-CSV-DATA-7111-RECORD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード・申請受取請求書情報）編集処理
      *****************************************************************
       2001-CSV-DATA-7121-RECORD-SEC SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    レコード識別          
           MOVE    "2,"            TO  WRK-REC
           MOVE    2               TO  WRK-REC-LENGTH
      *    レコード番号
           ADD     1               TO  WRK-SEQ-NO
           MOVE    WRK-SEQ-NO      TO  WRK-NUM
           MOVE    9               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    交換情報識別番号
           IF      WRK-PARA-TEISYUTUBIKBN
                                   =   "1"
               MOVE    "7121"          TO  WRK-DATA
           ELSE
               MOVE    "7122"          TO  WRK-DATA
           END-IF
           MOVE    4               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険者番号
           MOVE    BUNBEN-HKNJANUM TO  WRK-HENKAN
           MOVE    8               TO  WRK-LENGTH 
           PERFORM 5003-DATA-RIGHT-HENSYU-SEC
           MOVE    WRK-HENKAN      TO  WRK-DATA
           MOVE    8               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    請求年月
           MOVE    WRK-SKYYMH      TO  WRK-DATA
           MOVE    5               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    分娩区分
           MOVE    BUNBEN-BUNBENKBN
                                   TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    医療機関等番号
           MOVE    SYS-1001-PREFNUM
                                   TO  WRK-DATA (1:2)
           MOVE    SYS-1001-TENHYOKBN
                                   TO  WRK-DATA (3:1) 
           MOVE    SYS-1001-HOSPCD TO  WRK-DATA (4:7)
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    分娩機関管理番号
           IF      SYS-1001-SANKANUM
                                   =   ZERO
               CONTINUE
           ELSE
               MOVE    SYS-1001-SANKANUM
                                       TO  WRK-DATA
               MOVE    10              TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    加入制度区分
           MOVE    BUNBEN-KANYUKBN TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    本人家族区分
           MOVE    BUNBEN-HONKZKKBN
                                   TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号
           MOVE    BUNBEN-KIGO     TO  WRK-DATA
           MOVE    38              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    番号
           MOVE    BUNBEN-NUM      TO  WRK-DATA
           MOVE    38              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    妊婦氏名
           MOVE    BUNBEN-NAME     TO  WRK-DATA
           MOVE    40              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
           MOVE    WRK-BIRTHDAYH   TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    在胎週数
           MOVE    BUNBEN-ZAIWEEK  TO  WRK-NUM
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    出産年月日
           MOVE    WRK-CHILD-BIRTHDAYH
                                   TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    死産有無
           MOVE    BUNBEN-SIZANKBN TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    出産数
           MOVE    BUNBEN-BIRTHCNT TO  WRK-NUM
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    入院日数
           MOVE    BUNBEN-NYUIN-DAY
                                   TO  WRK-NUM
           MOVE    3               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    産科医療保証制度区分
           MOVE    BUNBEN-SANKAKBN TO  WRK-NUM
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    入院料〜代理受取額
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       12
               EVALUATE    IDXX
                   WHEN    3
      *                分娩介助料
                       IF      BUNBEN-BUNBENKBN    =   "2"
                           MOVE    BUNBEN-MONEY (IDXX)
                                                   TO  WRK-NUM
                            MOVE    8               TO  WRK-NAGASA
                       END-IF
                   WHEN    4
      *                分娩料
                       IF      BUNBEN-BUNBENKBN    =   "1"
                           MOVE    BUNBEN-MONEY (IDXX)
                                                   TO  WRK-NUM
                            MOVE    8              TO  WRK-NAGASA
                       END-IF
                   WHEN    OTHER
                       MOVE    BUNBEN-MONEY (IDXX)
                                               TO  WRK-NUM
                       MOVE    8               TO  WRK-NAGASA
               END-EVALUATE
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM    
      *    備考
           MOVE    BUNBEN-BIKOU    TO  WRK-DATA
           MOVE    60              TO  WRK-NAGASA
           MOVE    1               TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    BUNBEN-PTID     TO  WRK-SCSVINFO-PTID
           MOVE    SPACE           TO  WRK-SCSVINFO-NYUGAIKBN
           MOVE    WRK-SEQ-NO      TO  WRK-SCSVINFO-SEQ-NO
           PERFORM 500-CSVINFO-SEC
      *
           ADD     1               TO  SUM-RECORD-CNT
           .
       2001-CSV-DATA-7121-RECORD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（エンドレコード）編集処理
      *****************************************************************
       2001-CSV-END-RECORD-SEC                SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    レコード識別          
           MOVE    "3,"            TO  WRK-REC
           MOVE    2               TO  WRK-REC-LENGTH
      *    レコード番号
           ADD     1               TO  WRK-SEQ-NO
           MOVE    WRK-SEQ-NO      TO  WRK-NUM
           MOVE    9               TO  WRK-NAGASA
           MOVE    1               TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    ZERO            TO  WRK-SCSVINFO-PTID
           MOVE    SPACE           TO  WRK-SCSVINFO-NYUGAIKBN
           MOVE    WRK-SEQ-NO      TO  WRK-SCSVINFO-SEQ-NO
           PERFORM 500-CSVINFO-SEC
           .
       2001-CSV-END-RECORD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯ出力処理
      *****************************************************************
       500-CSVINFO-SEC                SECTION.
      *
           INITIALIZE                  ORCSCSVINFOAREA
           MOVE    "INS"           TO  SCSVINFO-MODE
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SCSVINFO-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SCSVINFO-SHELLID
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SCSVINFO-RENNUM
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  SCSVINFO-SHORI-RENNUM
               WHEN    "2"
                   MOVE    2               TO  SCSVINFO-SHORI-RENNUM
           END-EVALUATE
           MOVE    WRK-SRYYM       TO  SCSVINFO-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SCSVINFO-SKYYMD
           MOVE    WRK-SCSVINFO-SEQ-NO
                                   TO  SCSVINFO-SEQ-NO
           MOVE    WRK-SCSVINFO-NYUGAIKBN
                                   TO  SCSVINFO-NYUGAIKBN
           MOVE    WRK-TEISYUTUSAKI
                                   TO  SCSVINFO-TEISYUTUSAKI
           MOVE    WRK-SCSVINFO-PTID
                                   TO  SCSVINFO-PTID
           MOVE    WRK-REC         TO  SCSVINFO-CSVDATA
           CALL    "ORCSCSVINFO"       USING
                                       ORCSCSVINFOAREA
                                       SPA-AREA
           IF      SCSVINFO-RETURN =   ZERO
               ADD     1               TO  WRK-OUT
                                           CNT-OUT
           ELSE
               MOVE    "ＣＳＶ用ＤＢに更新できませんでした"
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-CSVINFO-EXT.
           EXIT.
      *
      *****************************************************************
      *   分娩管理ＤＢ更新処理
      *****************************************************************
       310-BUNBEN-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               INITIALIZE                      BUNBEN-REC
               MOVE    BUNBENX-KEY         TO  BUNBEN-KEY
               MOVE    BUNBEN-REC          TO  MCPDATA-REC
               PERFORM 900-BUNBEN-INV-SEC
               IF      FLG-BUNBEN          =   ZERO
                   MOVE    "2"             TO  BUNBEN-SKYKBN
                   MOVE    SMCNDATE-YMD    TO  BUNBEN-UPYMD
                   MOVE    SMCNDATE-HMS    TO  BUNBEN-UPHMS
                   MOVE    BUNBEN-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_bunben"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       MOVE    SPACE               TO  WRK-BUNBENERR
                       STRING "分娩管理ＤＢ　更新エラー　"
                                               DELIMITED  BY  SIZE
                               BUNBEN-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-BUNBENERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
               ELSE
                   STRING "分娩管理ＤＢ　読込み更新エラー　"
                                               DELIMITED  BY  SIZE
                           BUNBENX-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-BUNBENERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-BUNBEN-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               BUNBENERR-FILE
           IF        ( STS-BUNBENERR   =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              BUNBENERR-FILE
      *
               MOVE    WRK-BUNBENERR   TO  BUNBENERR-REC
               WRITE   BUNBENERR-REC
               CLOSE   BUNBENERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-BUNBENERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBGCHILDBIRTH "
                                   DELIMITED  BY  SIZE
                   WRK-BUNBENERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBGCHILDBIRTH IN   "  CNT-IN
           DISPLAY "*** ORCBGCHILDBIRTH OUT  "  CNT-OUT
           DISPLAY "*** ORCBGCHILDBIRTH PAGE "  CNT-PAGE
           DISPLAY "*** ORCBGCHILDBIRTH END ***"
      *
           IF      CNT-IN          =   ZERO
               MOVE    "対象患者がありませんでした"
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
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
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  SFILESV-SHORI-RENNUM (1)
               WHEN    "2"
                   MOVE    2               TO  SFILESV-SHORI-RENNUM (1)
           END-EVALUATE
           MOVE    WRK-SRYYM       TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
      *****MOVE    "/home/orca/"   TO  SFILESV-TO-FOLDER(1)
           MOVE    SYS-1001-TENHYOKBN
                                   TO  WRK-TENHYOKBN
           IF      WRK-PARA-TEISYUTUBIKBN
                                   =   "2"
               EVALUATE    SYS-1001-TENHYOKBN
                   WHEN    "1"
                       MOVE    3               TO  WRK-TENHYOKBN
                   WHEN    "0"
                       MOVE    4               TO  WRK-TENHYOKBN
               END-EVALUATE
           END-IF
           STRING  WRK-TENHYOKBN       DELIMITED   BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED   BY  SIZE
                   ".UKE"              DELIMITED   BY  SIZE
                                       INTO    SFILESV-TO-DATA  (1)
           END-STRING
           MOVE    WRK-OUT         TO  SFILESV-CNT-ALL  (1)
           MOVE    WRK-TEISYUTUSAKI
                                   TO  SFILESV-TEISYUTUSAKI  (1)
           EVALUATE    WRK-TEISYUTUSAKI
                                   ALSO    WRK-PARA-TEISYUTUBIKBN
               WHEN    "1"         ALSO    "1"
                   MOVE
               "（社保）１０日請求　出産育児一時金等代理申請・受取請求"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "1"         ALSO    "2"
                   MOVE
               "（社保）２５日請求　出産育児一時金等代理申請・受取請求"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "2"         ALSO    "1"
                   MOVE
               "（国保）１０日請求　出産育児一時金等代理申請・受取請求"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "2"         ALSO    "2"
                   MOVE
               "（国保）２５日請求　出産育児一時金等代理申請・受取請求"
                                           TO  SFILESV-TITLE    (1)
           END-EVALUATE
           IF      WRK-OUT         >   ZERO
               MOVE    "1"             TO  SFILESV-DATA-TYPE(1)
           END-IF 
           MOVE    WRK-BUNBENERR   TO  SFILESV-ERR-MSG  (1)
           IF      WRK-BUNBENERR   NOT =   SPACE
               MOVE    "1"             TO  SFILESV-ERR-CODE (1)
           END-IF
           MOVE    "1"             TO  SFILESV-CODE-TYPE (1)
      *
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付変換処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC         SECTION.
      *
           INITIALIZE                  WRK-LNK-DAY1-AREA
                                       WRK-LNK-DAY2-AREA
                                       WRK-LNK-DAY3-AREA
                                       WRK-LNK-DAY4-AREA
                                       WRK-LNK-DAY5-AREA
                                       WRK-LNK-DAY6-AREA
                                       WRK-LNK-DAY7-AREA
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-AREA   TO  WRK-LNK-DAY2-AREA
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY1-AREA
           MOVE    LNK-DAY1-AREA   TO  WRK-LNK-DAY1-AREA
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    右づめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN                            
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
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
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW2                TO  WRK-REC-LENGTH        
                       MOVE    1                   TO  IDW2
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
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
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
               PERFORM VARYING IDW2    FROM    WRK-ST    BY  1
                       UNTIL   IDW2    >       10
                   IF      WRK-NUM (IDW2:1)  NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW2       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW2
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
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
                   DISPLAY "UPD =" BUNBENX-KEY
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩管理読込
      *****************************************************************
       900-BUNBEN-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-END
      * 
           INITIALIZE                  BUNBEN-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  BUNBEN-HOSPNUM
           MOVE    WRK-PARA-SKYYM  TO  BUNBEN-SKYYM
           MOVE    WRK-TEISYUTUSAKI
                                   TO  BUNBEN-TEISYUTUSAKI
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-BUNBEN-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
       900-BUNBEN-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    分娩管理読込
      *****************************************************************
       900-BUNBEN-READ-N-SEC           SECTION.
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  BUNBEN-REC
      *
               IF      WRK-PARA-TEISYUTUBIKBN
                                   NOT =   BUNBEN-TEISYUTUBIKBN
                   GO TO  900-BUNBEN-READ-N-SEC
               END-IF
      *
               INITIALIZE                      PTINF-REC
               MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
               MOVE    BUNBEN-PTID         TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      PTINF-TSTPTNUMKBN   =   "1"
                   GO TO  900-BUNBEN-READ-N-SEC
               END-IF
      *
               MOVE    BUNBEN-BUNBENKBN
                                       TO  KEY-N-BUNBENKBN
      *********MOVE    BUNBEN-HKNJANUM-S
               MOVE    BUNBEN-HKNJANUM TO  KEY-N-HKNJANUM    
               ADD     1               TO  CNT-IN
      *  
               DISPLAY "PTNUM="         BUNBEN-PTNUM
                       " TEISYUTUSAKI=" BUNBEN-TEISYUTUSAKI
                       " BUNBENKBN="    BUNBEN-BUNBENKBN
                       " HKNJANUM="     BUNBEN-HKNJANUM
           ELSE
               MOVE    1               TO  FLG-END
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-BUNBEN-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       900-BUNBEN-INV-SEC         SECTION.
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-BUNBEN
                   MOVE    MCPDATA-REC     TO  BUNBEN-REC
               ELSE
                   MOVE    1               TO  FLG-BUNBEN
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BUNBEN-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE              PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読込
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    WRK-SYMD        TO  SYS-1900-STYUKYMD
                                       SYS-1900-EDYUKYMD
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(4)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関名称編集情報（２）
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
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(4)
                                   TO  SYS-1901-KBNCD
           MOVE    WRK-SYMD        TO  SYS-1901-STYUKYMD
                                       SYS-1901-EDYUKYMD
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                      SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読込
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(4)
                                   TO  SYS-1902-KBNCD
           MOVE    WRK-SYMD        TO  SYS-1902-STYUKYMD
                                       SYS-1902-EDYUKYMD
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1      TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2      TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL        TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                      SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報（２）読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1903-REC
           INITIALIZE                  SYS-1903-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1903-HOSPNUM
           MOVE    "1903"          TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(4)
                                   TO  SYS-1903-KBNCD
           MOVE    WRK-SYMD        TO  SYS-1903-STYUKYMD
                                       SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                           TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                      SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
