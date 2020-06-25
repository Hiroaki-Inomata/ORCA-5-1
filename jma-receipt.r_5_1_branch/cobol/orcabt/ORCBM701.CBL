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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM701.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト電算ファイル作成（労災）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/02/07    NACL−藤原    新規作成
      *  17/12/22    NACL−伊藤    新規作成（ORCBM700複写）
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    13/08/20  ０９５９３で始まるコードがあ
      *                                     るレセプトは対象外とする
      *  04.07.02    NACL-藤原    13/08/30  改行コードの編集追加
      *  04.07.03    NACL-藤原    13/10/07  返戻対応
      *  04.07.04    NACL-藤原    14/02/13  労災指定医療機関番号の記録
      *                                     方法の修正
      *  04.07.05    NACL-藤原    14/05/28  請求情報の編集の修正
      *  04.07.06    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    15/08/24  レセ電データ作成時の請求区分
      *                                     更新対応
      *  04.08.03    NACL-藤原    18/05/17  平成３０年４月改正対応
      *                                     カタカナ（氏名）、患者状態の追加
      *
      *  05.00.02    NACL-伊藤    17/12/22  診療情報システム対応
      *                                     ORCBM700を複写して作成
      *                                     請求管理なしでデータ作成
      *                                     tbl_receden_tmp使用
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.      
       FILE-CONTROL.
      *    ワークファイル  
           SELECT  RECE3W-FILE     ASSIGN  RECE3WPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECE3W.
      *    レセプト電算ファイル  
           SELECT  RECE30-FILE     ASSIGN  RECE30PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECE30.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ワークファイル  
       FD  RECE3W-FILE.
       01  RECE3W-R                    PIC X(2500).
      *
      *    レセプト電算ファイル  
       FD  RECE30-FILE.
       01  RECE30-R                    PIC X(2500).
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    ワークファイル名 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE3WPARA//.
      *01  RECE3WPARA.
      *****03  FILLER                  PIC X(05)   VALUE   "/tmp/".
       01  RECE3WPARA-BASENAME.
           03  RECE3WPARA-HOSPNUM      PIC 9(02).
           03  RECE3WPARA-FILENM       PIC X(06)   VALUE   "ROUSAI".
      *****03  RECE3WPARA-SEQNUM       PIC 9(02).
      *****03  RECE3WPARA-RENNUM       PIC 9(02).
           03  FILLER                  PIC X(04)   VALUE   ".txt".
      *
      *    レセプト電算ファイル名 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE30PARA//.
      *01  RECE30PARA.
      *****03  FILLER                  PIC X(05)   VALUE   "/tmp/".
       01  RECE30PARA-BASENAME.
           03  RECE30PARA-HOSPNUM      PIC 9(02).
           03  RECE30PARA-FILENM       PIC X(07).
           03  RECE30PARA-SEQNUM1      PIC 9(02).
           03  RECE30PARA-SEQNUM2      PIC 9(02).
           03  RECE30PARA-RENNUM       PIC 9(02).
           03  FILLER                  PIC X(04)   VALUE   ".txt".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE30              PIC X(02).
           03  STS-RECE3W              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-BTPARA              PIC 9(01).
      *
           03  FLG-DATA                PIC 9(01).
           03  FLG-RECE3W              PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECEDEN             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(05).
           03  IDX                     PIC 9(05). 
           03  IDY                     PIC 9(05). 
           03  IDZ                     PIC 9(05).
           03  IDY1                    PIC 9(05).
           03  IDY2                    PIC 9(05).
           03  IDY3                    PIC 9(05).
           03  IDWW                    PIC 9(05).
           03  IDXX                    PIC 9(05).
           03  IDYY                    PIC 9(05).
           03  IDZZ                    PIC 9(05).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               07  KEY-N-SYOKAIFLG     PIC X(01).
               07  KEY-N-SYOCD         PIC X(05).
           03  KEY-OLD.
               07  KEY-O-SYOKAIFLG     PIC X(01).
               07  KEY-O-SYOCD         PIC X(05).
      *
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKENSU          PIC 9(06).
           03  SUM-TOTALTEN            PIC 9(10).
      *
      *    一時領域
      *    パラメタエリア
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-FOLDER         PIC X(150).
           05  WRK-PARA-NYUGAIKBN      PIC X(01).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-DATAKBN        PIC X(01).
           05  WRK-PARA-FIXEDSIZE      PIC 9(07).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
           05  WRK-PARA-PRTKBN         PIC X(01).
           05  WRK-PARA-RSIIRYOKBN     PIC X(01).
      *
      *    マルチボリューム識別情報     
           03  WRK-VOLNUM              PIC 9(02).
      *    レセプト番号
           03  WRK-RECENUM             PIC 9(06).
           03  WRK-RECENUM-VOL         PIC 9(06).
           03  WRK-RECENUM-ALL         PIC 9(06).
      *    請求年月日（和暦）
           03  WRK-SKYYMD              PIC 9(07).
           03  WRK-SRYYMW              PIC X(05).
      *    請求年月（医療機関レコード用）
           03  WRK-SRYYM               PIC 9(06).
           03  WRK-SRYYM-W             PIC X(05).
      *
           03  WRK-SKYYM-W             PIC X(05).
      *
           03  WRK-MOJISU              PIC 9(02).
      *
           03  WRK-RSIIRYOCD           PIC X(07).
           03  WRK-PREFNAME            PIC X(08).
      *
           03  WRK-INFO1-INDEX         PIC 9(04).        
           03  WRK-INFO2-INDEX         PIC 9(04).        
      *
      *    データ長
           03  WRK-DATA-LENGTH         PIC 9(07).        
           03  WRK-PTNUM-LENGTH        PIC 9(07).   
           03  WRK-LEN                 PIC 9(07). 
      *       
           03  WRK-STR                 PIC X(2500).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-KETA1               PIC 9(04).
           03  WRK-KETA2               PIC 9(04).
      *
      *     右づめ編集用
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-COM-SRYKBN          PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC1                PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-FILE-RENNUM         PIC 9(02).
           03  WRK-FILE-SEQNUM         PIC 9(02).
           03  WRK-FILE-SEQNUM1        PIC 9(02).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999. 
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *    数字変換用
           03  WRK-9                   PIC 9(04).
           03  WRK-9-R                 REDEFINES   WRK-9.
               05  FILLER              PIC X(03).
               05  WRK-9-X             PIC X(01).
      *    日付変換用
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
      *
      *    パラメタＤＢ編集用
           03  WRK-BTPARA-INFO-PARA.
               05  WRK-BTPARA-DATA     PIC X(10).
               05  WRK-BTPARA-FILENM   PIC X(20).
               05  WRK-BTPARA-DATAKBN  PIC X(01).
               05  WRK-BTPARA-STNO     PIC 9(02).
               05  WRK-BTPARA-EDNO     PIC 9(02).
               05  WRK-BTPARA-WRNO     PIC 9(02).
      * 
      *    タイトル
           03  WRK-SFILESV-TITLE       PIC X(100).
      *
      *    数字半角全角変換用
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-IDX        PIC 9(02).
               05  WRK-CHAR-FLG        PIC 9(01).
      *
               05  WRK-CHAR-X          PIC X(10).
               05  WRK-CHAR-N          PIC X(20).
               05  WRK-CHAR-CNT        PIC 9(02).
               05  WRK-CHAR-N-CNT      PIC 9(02).
      *
      *    医療機関情報ワーク
           COPY    "CPSK1001.INC"      REPLACING   //SYS-1001//
                                       BY          //WRK-S-1001//.
           COPY    "CPSK1002.INC"      REPLACING   //SYS-1002//
                                       BY          //WRK-S-1002//.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  WRK-HENYMD-FIL      PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *
       01  TABLE-AREA.
           03  TABLE-OCC               OCCURS  10000.
               05  TBL-RECEDATA        PIC X(2500).
               05  TBL-LENGTH          PIC 9(04).
           03  TABLE-MAX               PIC 9(05).
      *
       01  TABLE-SRYYM-AREA.
           03  TABLE-SRYYM-OCC         OCCURS  100.
               05  TBL-SRYYM           PIC 9(06).
               05  TBL-SRYYM-SEQNUM    PIC 9(02).
      *
      *    労災自賠医療機関情報退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC         OCCURS  100.
               05  TBL-SYS4001         PIC X(610).
      *     
           03  SYSKAN-IDX              PIC 9(03).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *
           03  WRK-CONS-LENGTH         PIC 9(07)   VALUE   1441500.
      *
           03  WRK-CONS-SRYYM-201307   PIC 9(06)   VALUE   201307.
      *
      *    全角数字
       01  TBL-SUJI-AREA.
           03  FILLER                  PIC X(20)   VALUE
               "１２３４５６７８９０".
       01  TBL-SUJI-R                  REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI                PIC X(02)   OCCURS  10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      **** COPY    "CPSK2005.INC".
      *
      **** COPY    "CPSK200501.INC".
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
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
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
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              CNT-AREA
                                   WRK-AREA
                                   FLG-AREA
                                   SPA-AREA
                                   KEY-AREA
                                   RECEERR
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
                                               WRK-PARA-FOLDER
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-FIXEDSIZE
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-RSIIRYOKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-HOSPNUM     =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-PRTKBN      =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-DATAKBN     =" WRK-PARA-DATAKBN
           DISPLAY "LNK-PRTKANRI-SRYYM   =" LNK-PRTKANRI-SRYYM
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           MOVE   SGETTEMP-TEMPDIR TO  WRK-SGETTEMP-TEMPDIR
           MOVE   SGETTEMP-ST      TO  WRK-SGETTEMP-ST
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM701"      TO  JOB-PGID
           EVALUATE    WRK-PARA-NYUGAIKBN   
               WHEN    ZERO
                   MOVE    "レセ電ファイル作成（労災）"
                                           TO  JOB-SHELLMSG
               WHEN    "1"
                   MOVE    "レセ電ファイル作成（労災・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "2"
                   MOVE    "レセ電ファイル作成（労災・入院外）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           IF    ( WRK-PARA-FIXEDSIZE  NUMERIC         )
           AND   ( WRK-PARA-FIXEDSIZE
                               NOT =   WRK-CONS-LENGTH )
               MOVE    WRK-PARA-FIXEDSIZE      TO  WRK-CONS-LENGTH
           END-IF
           DISPLAY "LENGTH=" WRK-CONS-LENGTH
      *
      *    請求年月日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-SKYYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD (1:5)    TO  WRK-SKYYM-W
      *
      *    システム管理情報取得
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 900-RECEDEN-SELECT-SEC
           IF      FLG-RECEDEN     =   1
               DISPLAY "!!!!!! RECEDEN NOT FOUND !!!!!"
               MOVE    1                   TO  FLG-END
           END-IF
           MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理情報取得
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
      *    医療機関情報取得
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報（基本）が取得できません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD   TO  SYS-1002-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               MOVE    "医療機関情報（住所）が取得できません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  WRK-S-1001-HOSPNAME
               MOVE    SYS-1001-KAISETUNAME
                                           TO  WRK-S-1001-KAISETUNAME
               MOVE    SYS-1002-POST       TO  WRK-S-1002-POST
               MOVE    SYS-1002-ADRS       TO  WRK-S-1002-ADRS
               MOVE    SYS-1002-TEL        TO  WRK-S-1002-TEL
      *        医療機関編集情報
               PERFORM 900-1900-READ-SEC
           END-IF    
      *
      *    労災医療機関情報取得
           MOVE     ZERO               TO  SYSKAN-IDX
           MOVE     SPACE              TO  TBL-SYS4001-T
           INITIALIZE                      TBL-SYS4001-T
           MOVE    WRK-CONS-SRYYM-201307
                                       TO  WRK-SYS-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SYS-SRYYMD (7:2)
      *
           INITIALIZE                      SYS-4001-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-4001-HOSPNUM
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-N-SEC
               PERFORM UNTIL ( FLG-SYSKANRI    =   1   )
                       OR    ( SYSKAN-IDX      >=  100 )
                   MOVE    MCPDATA-REC             TO  SYS-4001-REC
                   IF      SYS-4001-EDYUKYMD       <   WRK-SYS-SRYYMD
                       CONTINUE
                   ELSE
                       ADD     1                   TO  SYSKAN-IDX
                       MOVE    SYS-4001-REC        TO  TBL-SYS4001
                                                          (SYSKAN-IDX)
      *
           DISPLAY "4001=" SYSKAN-IDX "#" SYS-4001-STYUKYMD " "
                                          SYS-4001-EDYUKYMD " "
                                          SYS-4001-RSIIRYOKBN
      *
                   END-IF
      *
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key8"            TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-N-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      SYSKAN-IDX          =   ZERO
               MOVE    "労災指定医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *    
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    一時請求分
           MOVE    1                   TO  FLG-SYORI
      *
           MOVE    ZERO                TO  WRK-FILE-SEQNUM1
                                           WRK-FILE-RENNUM
      *
           PERFORM 2001-RECEDEN-HENSYU-SEC
                                       UNTIL   FLG-END =   1
      *
      *    再請求分
      **   MOVE    2                   TO  FLG-SYORI
      *
      **   MOVE    ZERO                TO  WRK-FILE-SEQNUM1
      **                                   WRK-FILE-RENNUM
      *
      **   PERFORM 2001-RECEDEN-HENSYU-SEC
      **                               UNTIL   FLG-END =   1
      *
           IF      CNT-RECEDEN         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ファイル保存情報更新
               IF      WRK-PARA-DATAKBN    =   "5" OR  "6"
                   PERFORM 3001-FILE-INFO-INSERT-SEC
               END-IF
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    レセ電データ作成処理
      *****************************************************************
       2001-RECEDEN-HENSYU-SEC              SECTION.
      *
      *
           MOVE    ZERO            TO  WRK-RECENUM
                                       SUM-AREA
                                       WRK-LENGTH 
                                       FLG-DATA
                                       WRK-VOLNUM
           COMPUTE WRK-FILE-SEQNUM1    =   WRK-FILE-SEQNUM1  +   1
      *??
           DISPLAY "KEY-NEW=" KEY-NEW
      *??
      *     
      *    ワークファイルＯＰＥＮ処理
           PERFORM     2001-HENSYU-SEC
      *
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *
      *        レセプト件数が９９７件を超えたとき
               IF      WRK-RECENUM-VOL >=  997
      *            労災診療費請求書レコード
                   PERFORM 2008-HENSYU-SEC
      *
                   DISPLAY "CNT-OUT=" CNT-OUT
      *
      *            ワークファイルＯＰＥＮ処理
                   PERFORM 2001-HENSYU-SEC
               END-IF
      *
               INITIALIZE                  TABLE-AREA
                                           WRK-PTNUM-LENGTH
      *
               PERFORM 900-RECEDEN-SELECT-SEC
               IF      FLG-RECEDEN     =   1
                   DISPLAY "!!!! RECEDEN NOT FOUND !!!!"
               END-IF
      *
      *        レコード出力処理
               PERFORM     UNTIL   FLG-RECEDEN =   1
                   PERFORM 2002-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
               MOVE    "all2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *        データ出力処理
               PERFORM 2003-TABLE-WRITE-SEC
               IF      FLG-RECEDEN     =   1
                   MOVE    1                   TO  FLG-END
               END-IF
           END-PERFORM
      * 
      *    労災診療費請求書レコード
           DISPLAY "2008 WRITE VOL="  WRK-VOLNUM 
      *
           MOVE    9               TO  FLG-DATA 
           PERFORM 2008-HENSYU-SEC
           .
       2001-RECEDEN-HENSYU-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    ワークファイルＯＰＥＮ処理
      *****************************************************************
       2001-HENSYU-SEC              SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  RECE3WPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECE3WPARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECE3WPARA
      *
           OPEN    OUTPUT              RECE3W-FILE
           IF      STS-RECE3W          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE3WPARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE3W          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           MOVE    ZERO            TO  WRK-SRYYM
                                       WRK-RECENUM-VOL
           .
       2001-HENSYU-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    レコード出力処理
      *****************************************************************
       2002-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
      *
           IF      RECEDEN-RECEDATA (1:2)  =   "RE"
      *
               MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYM
      *
      *        総件数・請求金額の集計
               ADD     1                   TO  SUM-TOTALKENSU
               ADD     RECEDEN-TOTALTEN    TO  SUM-TOTALTEN
      *  
      *        レセプト番号の編集
               MOVE    "RE,"               TO  WRK-REC
               MOVE    3                   TO  WRK-REC-LENGTH
               ADD     1                   TO  WRK-RECENUM
                                               WRK-RECENUM-ALL
                                               WRK-RECENUM-VOL
               MOVE    WRK-RECENUM         TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       RECEDEN-RECEDATA (5:)     DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
      *
               MOVE    WRK-REC         TO  RECEDEN-RECEDATA
      *
      *        請求情報の編集（診療年月、請求年月）
               MOVE    ZERO            TO  IDYY
                                           WRK-KETA1
                                           WRK-KETA2
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       2500
                       OR      WRK-REC (IDXX:1)
                                       =   WRK-KAIGYO
                   IF      WRK-REC (IDXX:1) =   ","
                       ADD     1               TO  IDYY
                       IF      IDYY            =   20
                           MOVE    IDXX            TO  WRK-KETA1
                           COMPUTE WRK-KETA2   =   WRK-KETA1 +   1
                           MOVE    2500            TO  IDXX
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-KETA1           >   ZERO
                   MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2)
                   PERFORM 5002-HIZUKE-HEN-SEC
      *
                   STRING    WRK-REC (1:WRK-KETA1) DELIMITED  BY  SIZE
                             WRK-SKYYM-W           DELIMITED  BY  SIZE
                             WRK-HENYMD (1:5)      DELIMITED  BY  SIZE
                             WRK-REC (WRK-KETA2:)  DELIMITED  BY  SIZE
                             INTO                  RECEDEN-RECEDATA
                   END-STRING
               ELSE
                   DISPLAY "ERROR WRK-KETA1=0!!"
               END-IF
      *
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO             TO  WRK-MOJISU
                                            WRK-NAGASA
               INSPECT RECEDEN-RECEDATA   TALLYING    WRK-MOJISU
                                           FOR ALL "," 
               IF      WRK-MOJISU          =   35
                   PERFORM VARYING IDX FROM    2500 BY  -1
                           UNTIL   IDX <       1
                       IF      RECEDEN-RECEDATA (IDX:1)
                                               =   WRK-KAIGYO
                           COMPUTE WRK-NAGASA  =   IDX     -   1
                           MOVE    1               TO  IDX
                       END-IF    
                   END-PERFORM   
                   STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                           DELIMITED  BY  SIZE
                             ",,"          DELIMITED  BY  SIZE
                             WRK-KAIGYO    DELIMITED  BY  SIZE
                             INTO          WRK-REC
                   END-STRING
                   MOVE    WRK-REC         TO  RECEDEN-RECEDATA
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    1                   TO  WRK-LEN
           STRING  RECEDEN-RECEDATA    DELIMITED   BY  X"0D"
                                       INTO        WRK-REC
                                       WITH        POINTER WRK-LEN
           END-STRING
      *
           ADD     1                   TO  TABLE-MAX
           MOVE    WRK-REC             TO  TBL-RECEDATA (TABLE-MAX)
      *
           COMPUTE TBL-LENGTH (TABLE-MAX)
                                       =   WRK-REC-LENGTH  +   1
      *
           COMPUTE WRK-PTNUM-LENGTH    =   WRK-PTNUM-LENGTH
                                       +   WRK-REC-LENGTH
                                       +   1
      *
           PERFORM 900-RECEDEN-READ-SEC     
           .
       2002-HENSYU-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2003-TABLE-WRITE-SEC         SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       TABLE-MAX
               MOVE    TBL-RECEDATA (IDX)  TO  RECE3W-R
               WRITE   RECE3W-R
      *
               IF      STS-RECE3W          =   "00"
                   CONTINUE
               ELSE
                   CALL "coblog" USING   "file write err " RECE3WPARA
      *
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE3WPARA          TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE3W          TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-DATA-LENGTH  =   WRK-DATA-LENGTH
                                    +   WRK-PTNUM-LENGTH
      *
           .
       2003-TABLE-WRITE-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    労災診療費請求書レコード編集処理
      *****************************************************************
       2008-HENSYU-SEC              SECTION.
      *
           CLOSE   RECE3W-FILE
      *
      *    医療機関レコード編集処理
           PERFORM 20082-FILE-WRITE-SEC
      *
      *    パラメタＤＢ更新処理
      **   IF      FLG-DATA            =   ZERO    OR  9
      **       PERFORM 20081-BTPARA-UPDATE-SEC
      **   END-IF
      *
           IF      FLG-DATA            =   ZERO 
               MOVE    1                   TO  FLG-DATA
           END-IF
           .
       2008-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    パラメタＤＢ更新処理
      *****************************************************************
       20081-BTPARA-UPDATE-SEC     SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
           MOVE    "recept4.sh"        TO  BTPARA-SCRIPTID
           MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           PERFORM 900-BTPARA-READ-SEC
           IF      FLG-BTPARA          =   ZERO
               MOVE    BTPARA-INFO-PARA    TO  WRK-BTPARA-INFO-PARA
               EVALUATE    FLG-DATA
                   WHEN    ZERO
                       MOVE    WRK-FILE-RENNUM TO  WRK-BTPARA-STNO
                   WHEN    9
                       MOVE    WRK-FILE-RENNUM TO  WRK-BTPARA-EDNO
               END-EVALUATE
               MOVE    WRK-BTPARA-INFO-PARA
                                           TO  BTPARA-INFO-PARA
               MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
               MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "パラメタＤＢの更新処理でエラーとなりました"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           ELSE
               MOVE    "対象のパラメタＤＢが存在しません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           .
       20081-BTPARA-UPDATE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    医療機関レコード編集処理
      *****************************************************************
       20082-FILE-WRITE-SEC             SECTION.
      *
      *    最新の診療年月の労災医療機関情報取得
           MOVE    WRK-SRYYM           TO  WRK-SYS-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SYS-SRYYMD (7:2)
      *
           MOVE    ZERO                TO  FLG-SET
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS4001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS4001 (SYSKAN-IDX)    TO  SYS-4001-REC
               IF    ( SYS-4001-STYUKYMD   <=  WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD      <=  SYS-4001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING
                   "最新の診療年月の労災医療機関情報取得ができません　"
                                               DELIMITED  BY  SIZE
                       WRK-SRYYM               DELIMITED  BY  SIZE 
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    労災指定医療機関番号を数字変換
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SYS-4001-RSIIRYOCDN TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-SYUBETU     =   "2"  )
               INITIALIZE                      WRK-CHAR-HENKAN-AREA
               MOVE    SYS-4001-RSIIRYOCDN TO  WRK-CHAR-N
               MOVE    13                  TO  WRK-CHAR-N-CNT
               IF      KANACHK-MAX         <   14
                   COMPUTE WRK-CHAR-N-CNT  =   KANACHK-MAX   -   1
               END-IF
               PERFORM 800-NUM-HANKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-X          TO  WRK-RSIIRYOCD
           END-IF
      *
      *    最新の診療年月を和暦変換
           MOVE    SPACE               TO  WRK-SRYYM-W
      *
           IF      WRK-SRYYM           >   ZERO         
               MOVE    WRK-SRYYM           TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               IF    ( STS-DAY-RC1         =   0 )
               AND   ( STS-DAY-RC2         =   0 )
                   MOVE    WRK-HENYMD (1:5)    TO  WRK-SRYYM-W
               END-IF
           END-IF
           IF      WRK-SRYYM-W         =   SPACE
               STRING  "医療機関レコードの診療年月が設定できません　"
                                               DELIMITED  BY  SIZE
                       WRK-SRYYM               DELIMITED  BY  SIZE 
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    請求年月単位の連番取得
           MOVE    ZERO                TO  WRK-FILE-SEQNUM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       100
               IF      TBL-SRYYM (IDX) =   ZERO
                   MOVE    WRK-SRYYM       TO  TBL-SRYYM (IDX)
               END-IF
               IF      WRK-SRYYM       =   TBL-SRYYM (IDX)
                   ADD     1               TO  TBL-SRYYM-SEQNUM (IDX)
                   MOVE    TBL-SRYYM-SEQNUM (IDX)
                                           TO  WRK-FILE-SEQNUM
                   MOVE    100             TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-FILE-SEQNUM     =   ZERO
               STRING  "請求年月単位の連番が取得できませんでした　"
                                               DELIMITED  BY  SIZE
                       WRK-SRYYM               DELIMITED  BY  SIZE 
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           MOVE    WRK-PARA-HOSPNUM    TO  RECE30PARA-HOSPNUM
      *****MOVE    WRK-FILE-SEQNUM     TO  RECE30PARA-SEQNUM1
           MOVE    WRK-VOLNUM          TO  RECE30PARA-SEQNUM2
           COMPUTE WRK-FILE-RENNUM =   WRK-FILE-RENNUM  +   1
           MOVE    WRK-FILE-RENNUM     TO  RECE30PARA-RENNUM
           MOVE    WRK-FILE-SEQNUM1    TO  RECE30PARA-SEQNUM1
           EVALUATE    FLG-SYORI
               WHEN    1
      *            一時請求分
                   IF      WRK-PARA-PRTKBN     =   "1"
                       MOVE    "RECEDEN"           TO  RECE30PARA-FILENM
                   ELSE
                       MOVE    "TENKENT"           TO  RECE30PARA-FILENM
                   END-IF
               WHEN    2
      *            再請求分
                   IF      WRK-PARA-PRTKBN     =   "1"
                       MOVE    "UKSRECE"           TO  RECE30PARA-FILENM
                   ELSE
                       MOVE    "UKSTENK"           TO  RECE30PARA-FILENM
                   END-IF
           END-EVALUATE
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECE30PARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECE30PARA
      *??
           DISPLAY "RECE30PARA=" RECE30PARA
      *??
      *
      *    ファイル作成処理
           OPEN    INPUT               RECE3W-FILE
           IF      STS-RECE3W          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE3WPARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE3W          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE30-FILE
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE30PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE30          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
      *    医療機関レコード編集
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    レコード識別情報      
           MOVE    "IR,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    予備１ 
           PERFORM 5004-MOJI-HENSYU-SEC
      *    都道府県 
           MOVE    SYS-1001-PREFNUM    TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    点数表 
           MOVE    "1"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    医療機関コード 
           MOVE    SYS-1001-HOSPCD     TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    予備２ 
           PERFORM 5004-MOJI-HENSYU-SEC
      *    医療機関名称        
           MOVE    WRK-S-1001-HOSPNAME
                                       TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    請求年月
           MOVE    WRK-SRYYM-W         TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    マルチボリューム番号
           MOVE    WRK-VOLNUM          TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    電話番号
           MOVE    WRK-S-1002-TEL    TO  WRK-DATA
           MOVE    15                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  RECE30-R
           WRITE   RECE30-R
      *
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file write err " RECE30PARA
      *
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE30PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE30          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
      *
      *    ワークファイルから編集
           MOVE    ZERO                TO  FLG-RECE3W
           PERFORM 900-RECE3W-READ-SEC
           PERFORM             UNTIL   FLG-RECE3W  =   1
      *        改行コード編集 
               PERFORM VARYING IDX FROM    2500    BY  -1
                       UNTIL   IDX <       1               
                   IF      RECE3W-R (IDX:1)
                                       NOT =   SPACE
                       MOVE    IDX             TO  WRK-NAGASA
                       MOVE    1               TO  IDX
                   END-IF
               END-PERFORM
               MOVE    SPACE                   TO  WRK-REC
               STRING  RECE3W-R (1:WRK-NAGASA) DELIMITED  BY  SIZE
                       WRK-KAIGYO              DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
      * 
               MOVE    WRK-REC             TO  RECE30-R
               WRITE   RECE30-R
      *
               IF      STS-RECE30          =   "00"
                   CONTINUE
               ELSE
                   CALL "coblog" USING   "file write err " RECE30PARA
      *
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE30PARA          TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE30          TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
      *
               ADD     1                   TO  CNT-OUT
      *
               PERFORM 900-RECE3W-READ-SEC
           END-PERFORM
      *      
      *    労災診療費請求書レコード編集
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RS,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    病院・診療所の区分
           EVALUATE    SYS-1001-HOSPSBT
               WHEN    "1"
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    1               TO  WRK-NUM
                   ELSE
                       MOVE    2               TO  WRK-NUM
                   END-IF
               WHEN    "2"
                   MOVE    3               TO  WRK-NUM
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    請求書提出日
           MOVE    WRK-SKYYMD          TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    都道府県労働局コード
           IF      KEY-O-SYOKAIFLG     =   "1"
               MOVE    KEY-O-SYOCD (1:2)   TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    労働基準監督署コード
           IF      KEY-O-SYOKAIFLG     =   "1"
               MOVE    KEY-O-SYOCD (4:2)   TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    指定病院等の番号
           MOVE    WRK-RSIIRYOCD       TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    郵便番号
           MOVE    WRK-S-1002-POST   TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    医療機関所在地
           MOVE    WRK-S-1002-ADRS   TO  WRK-DATA
           MOVE    80                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    医療機関責任者氏名
           MOVE    WRK-S-1001-KAISETUNAME
                                       TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    労災診療費単価
           EVALUATE    SYS-4001-TENKBN
               WHEN    "1"
                   MOVE    1150        TO  WRK-NUM
               WHEN    "2"
                   MOVE    1200        TO  WRK-NUM
           END-EVALUATE 
           MOVE    4               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    請求金額
           IF      FLG-DATA            =   9
               MOVE    SUM-TOTALTEN        TO  WRK-NUM
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    内訳書添付枚数
           IF      FLG-DATA            =   9
               MOVE    SUM-TOTALKENSU      TO  WRK-NUM
               MOVE    3                   TO  WRK-NAGASA
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *
           IF      FLG-DATA            =   9
               MOVE    "99"                TO  WRK-DATA
           ELSE
               COMPUTE WRK-VOLNUM  =   WRK-VOLNUM  +   1
               MOVE    WRK-VOLNUM          TO  WRK-DATA
           END-IF    
           MOVE    2                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  RECE30-R
           WRITE   RECE30-R
      *
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file write err " RECE30PARA
      *
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE30PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE30          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           COMPUTE WRK-DATA-LENGTH  =   WRK-DATA-LENGTH
                                    +   WRK-REC-LENGTH
                                    +   2
      *
           CLOSE   RECE30-FILE
           CLOSE   RECE3W-FILE
      *
           .
       20082-FILE-WRITE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      WRK-RECENUM-ALL =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECENUM-ALL TO  JOB-UPDCNT2
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE300  OUT  " CNT-OUT
           DISPLAY "RECE300  RE   " WRK-RECENUM-ALL
           DISPLAY "RECE300  FILE " WRK-FILE-RENNUM  
           DISPLAY "*** ORCBM701 END ***"
           .
       300-TERM-EXT.
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
           MOVE    "recept4.sh"    TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
      *****MOVE    WRK-PARA-FOLDER TO  SFILESV-FOR-FOLDER
      *****                                             (1)
           MOVE    WRK-SGETTEMP-TEMPDIR
                                   TO  SFILESV-FOR-FOLDER
                                                        (1)
           IF      WRK-PARA-DATAKBN    =   "5"
               IF      WRK-PARA-PRTKBN     =   "1"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_Rousai_"          DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".zip"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_Rousai_"          DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".zip"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
               ELSE
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_TRousai_"         DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".zip"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_TRousai_"         DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".zip"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
               END-IF
           ELSE
               IF      WRK-PARA-PRTKBN     =   "1"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_Rousai_"          DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".iso"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  "Rousai_"           DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".iso"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
               ELSE
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_TRousai_"         DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".iso"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  "TRousai_"          DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".iso"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
               END-IF
           END-IF
           MOVE    CNT-OUT         TO  SFILESV-CNT-ALL  (1)
           EVALUATE    WRK-PARA-NYUGAIKBN   
               WHEN    ZERO
                   MOVE    "レセ電ファイル作成（労災）"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "1"
                   MOVE    "レセ電データ（労災・入院）"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "2"
                   MOVE    "レセ電データ（労災・入院外）"
                                           TO  SFILESV-TITLE    (1)
           END-EVALUATE
           IF      WRK-PARA-PRTKBN =   "2"
               MOVE    SFILESV-TITLE (1)   TO  WRK-SFILESV-TITLE
               MOVE    SPACE               TO  SFILESV-TITLE (1)
               STRING  "【点検用】"        DELIMITED  BY  SIZE
                       WRK-SFILESV-TITLE   DELIMITED  BY  SPACE
                                           INTO    SFILESV-TITLE (1)
               END-STRING
           END-IF       
           MOVE    "1"             TO  SFILESV-DATA-TYPE(1)
      *
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM701 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCBM701 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    ZERO                TO  WRK-SYMD
           END-IF    
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-HENYMDG
           END-IF
      *
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
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
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
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
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
      *    数値全角→半角変換処理
      *****************************************************************
       800-NUM-HANKAKU-HENKAN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-CHAR-CNT
                                           WRK-CHAR-IDX
      *
           PERFORM VARYING     IDZ     FROM    1   BY  2
                   UNTIL       IDZ     >       WRK-CHAR-N-CNT
               IF      WRK-CHAR-N (IDZ:2)  =   SPACE
                   MOVE    WRK-CHAR-N-CNT      TO  IDZ
               ELSE
                   MOVE    ZERO                TO  WRK-CHAR-FLG
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY             >   10
                           OR      WRK-CHAR-FLG    =   1
                       IF       WRK-CHAR-N (IDZ:2) =   TBL-SUJI (IDY)
                           ADD     1           TO  WRK-CHAR-IDX
                           IF      IDY         =   10
                               MOVE    "0"     TO   
                                           WRK-CHAR-X (WRK-CHAR-IDX:1)
                           ELSE
                               MOVE    IDY         TO  WRK-9
                               MOVE    WRK-9-X     TO
                                           WRK-CHAR-X (WRK-CHAR-IDX:1)
                           END-IF
                           MOVE    1           TO  WRK-CHAR-FLG
                       END-IF
                   END-PERFORM
      *            数字以外のとき変換終了
                   IF      WRK-CHAR-FLG        =   ZERO
                       MOVE    WRK-CHAR-N-CNT      TO  IDZ
                       MOVE    SPACE               TO  WRK-CHAR-X
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       800-NUM-HANKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワークファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE3W-READ-SEC         SECTION.
      *
           READ    RECE3W-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE3W
               NOT  AT  END
                   IF      STS-RECE3W          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE3WPARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE3W          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
           END-READ
           .
       900-RECE3W-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電読込
      *****************************************************************
       900-RECEDEN-SELECT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-RECEDEN
      *     
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                      RECEDEN-REC
           MOVE    SPA-HOSPNUM         TO  RECEDEN-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECEDEN-SRYYM
      **   MOVE    WRK-PARA-TEISYUTUSAKI
      **                               TO  RECEDEN-TEISYUTUSAKI
           MOVE    3                   TO  RECEDEN-TEISYUTUSAKI
           IF      WRK-PARA-NYUGAIKBN  =   "0"
               MOVE    "%"                 TO  RECEDEN-NYUGAIKBN
           ELSE
               MOVE    WRK-PARA-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
           END-IF
      **   MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RECEDEN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
           .
       900-RECEDEN-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC           SECTION.
      *
           MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECEDEN
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
               ADD     1                   TO  CNT-RECEDEN
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           .
       900-RECEDEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
               PERFORM 900-DBFETCH-SEC
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
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1900-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(19) NOT =   SPACE
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
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-S-1001-HOSPNAME
           ELSE
               INITIALIZE                      SYS-1901-REC
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
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1902-REC
               IF      SYS-1902-TEL    NOT =   SPACE
                   MOVE    SYS-1902-TEL        TO  WRK-S-1002-TEL
               END-IF
               IF      SYS-1902-POST   NOT =   SPACE
                   MOVE    SYS-1902-POST       TO  WRK-S-1002-POST
               END-IF
               IF      SYS-1902-ADRS1  NOT =   SPACE
                   MOVE    SYS-1902-ADRS1      TO  WRK-S-1002-ADRS
               END-IF
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
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1903-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1903-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD
                                   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                           TO  WRK-S-1001-KAISETUNAME
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
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0700 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
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
