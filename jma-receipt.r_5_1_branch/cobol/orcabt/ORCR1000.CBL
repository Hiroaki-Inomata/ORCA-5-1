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
       PROGRAM-ID.             ORCR1000.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 公害レセプト（入院外）
      *                      基本情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE100-FILE    ASSIGN  RECE100PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE100-KEY
                                   FILE    STATUS  IS  STS-RECE01.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE100-FILE.
       01  RECE100-REC. 
           COPY    "CPRCF100.INC".
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    レセプト明細ファイル名称
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE100//.
      *
      *    エラーファイル名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名称領域
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE01          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-RECE01          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
      *
           03  FLG-SET             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-RECE100         PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDX1                PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03). 
           03  IDY                 PIC 9(03).
           03  IDZ                 PIC 9(03).
           03  IDXX                PIC 9(03).
           03  TBL-MAX             PIC 9(03).
           03  SYSKAN-IDX          PIC 9(03).
      *
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID      PIC 9(10).
               05  KEY-N-HKNCOMBI  PIC 9(04).
               05  KEY-N-SRYKA     PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID      PIC 9(10).
               05  KEY-O-HKNCOMBI  PIC 9(04).
               05  KEY-O-SRYKA     PIC X(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-SYOKBN     PIC X(01).
               05  WRK-PARA-RECEKBN    PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-PTID       PIC X(10).
               05  WRK-PARA-SRYYM-O    PIC X(06).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-PRTKBN         PIC X(01).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-SRYACCT-PATHNAME    PIC X(64).
      *
           03  WRK-PRTSEQ              PIC 9(03).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
               05  WRK-ROUSAIKBN       PIC X(01).
               05  WRK-ERR-DAY         PIC 9(02).
               05  WRK-ERR-HKNCOMBINUM PIC 9(04).
               05  WRK-ERR-ERRKBN      PIC 9(01).
               05  WRK-ERR-TEKSTYMD    PIC 9(08).
               05  WRK-ERR-TEKEDYMD    PIC 9(08).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      * 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-SYUKA-SRYYM         PIC X(06).
           03  WRK-SYUKA-PTID          PIC 9(10).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
       01  WRK-ERRMSG-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
       01  WRK-RECE100-REC.
           COPY    "CPRCF100.INC"  REPLACING   //RECE100//
                                   BY          //WRK-RECE100//.
      *
       01  TABLE-RECE100-AREA.
           02  TBL-RECE100-OCC     OCCURS  100.
           COPY    "CPRCF100.INC"  REPLACING   //RECE100//
                                   BY          //TBL-RECE100//.
       01  TBL-RECE100-MAX         PIC 9(03)   VALUE   100.
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  100.
               05  TBL-SYS1001     PIC X(610).
      *
       01  WRK-CONS-AREA.
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM
                                   PIC X(03)   VALUE   "975". 
      *
           COPY    "MCPAREA".
      *     
      ***  COPY    "ORCA-DBMETA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
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
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       TABLE-RECE100-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM=" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM  =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN =" WRK-PARA-PRTKBN
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE900"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE100PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1000"      TO  JOB-PGID
               MOVE    "対象患者抽出"  TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-END           =   ZERO
      *        医療機関情報取得
               IF      WRK-PARA-SRYYM    NOT =  ZERO
                   INITIALIZE                  SYS-1001-REC
                   MOVE    "1001"              TO  SYS-1001-KANRICD
                   MOVE    "*"                 TO  SYS-1001-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO 
                                               SYS-1001-STYUKYMD (1:6)
                   MOVE    "01"                TO
                                               SYS-1001-STYUKYMD (7:2)
                   MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC        TO  MCPDATA-REC
                   PERFORM 910-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1001-REC
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               ELSE
                   MOVE     ZERO               TO  SYSKAN-IDX
                   MOVE     SPACE              TO  TBL-SYS1001-T
                   INITIALIZE                      TBL-SYS1001-T
      *
                   INITIALIZE                      SYS-1001-REC
                   MOVE    "1001"              TO  SYS-1001-KANRICD
                   MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM  UNTIL ( FLG-SYSKANRI   =   1   )
                                OR    ( SYSKAN-IDX     >=  100 )
      *
                           MOVE    MCPDATA-REC   TO  SYS-1001-REC
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1001-REC  TO
                                                 TBL-SYS1001(SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
      *
                       END-PERFORM
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
      *
      *        患者番号構成管理情報編集
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD  (1:6)
               MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
               MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN  
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               IF        WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            パラメタＤＢ読み込み
                   INITIALIZE                  BTPARA-REC
                   MOVE    WRK-PARA-SHELLID
                                           TO  BTPARA-SHELLID
                   MOVE    WRK-PARA-JOBID  TO  BTPARA-JOBID
                   PERFORM 900-BTPARA-READ-SEC
                   IF      FLG-BTPARA      NOT =   ZERO
                       MOVE    "パラメタ情報が取得できませんでした。"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    ファイルＯＰＥＮ処理
           OPEN    OUTPUT              RECE100-FILE
           CLOSE                       RECE100-FILE
      *
           OPEN    I-O                 RECE100-FILE  
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   PERFORM 2001-IKKATU-SYORI-SEC
               WHEN    "2"
                   PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
           IF      FLG-END             =   ZERO 
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "2"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key60"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key36"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1
                   PERFORM 20011-IKKATU-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    一括編集処理
      *****************************************************************
       20011-IKKATU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  TABLE-RECE100-AREA
      *
      *    患者情報取得
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *****MOVE    SYS-2001-SRYYM  TO  WRK-SYUKA-SRYYM
      *****MOVE    KEY-N-PTID      TO  WRK-SYUKA-PTID
      *****PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *****        " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    テスト患者は読み飛ばす
           IF   ( PTINF-TSTPTNUMKBN    =   "1"  )
      *****OR   ( SYUKA-MISEIKYU-FLG   =   "01" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-HKNNUM  =   WRK-CONS-KOUGAI-HKNNUM
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                           ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       ELSE
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       END-IF    
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    1               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO  WRK-ERR-HKNCOMBINUM
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       IF      WRK-ROUSAIKBN       =   SPACE
                           PERFORM 200111-FILE-SYORI-SEC
                       END-IF 
      *
      *                公害以外か日付変更がないとき
      *                同じ患者ＩＤ・保険組合せ・診療科は読み飛ばす
                       IF    ( WRK-ROUSAIKBN       =   "1"   )
                       OR    ( WRK-UPDKBN          =   SPACE )
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-END     =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
      *
      *        ファイル出力処理
               PERFORM   200112-KIHON-WRITE-SEC
           END-IF    
           .
       20011-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       200111-FILE-SYORI-SEC              SECTION.
      *                  
           IF      WRK-UPDKBN          =   "1"
      *        保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    2               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    3               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    4               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    5               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD
                   END-IF    
               END-IF    
           END-IF
      *
           INITIALIZE                      WRK-RECE100-REC
           MOVE    SPA-HOSPNUM         TO  WRK-RECE100-HOSPNUM
           MOVE    WRK-STYMD (1:6)     TO  WRK-RECE100-SRYYM
           MOVE    ACCT-NYUGAIKBN      TO  WRK-RECE100-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE100-PTID
           MOVE    SPACE               TO WRK-RECE100-RECEKA
           MOVE    COMB-HKNID          TO  WRK-RECE100-HKNID
           MOVE    COMB-TEKSTYMD       TO  WRK-RECE100-TEKSTYMD
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       TBL-RECE100-MAX
               IF      TBL-RECE100-HOSPNUM (IDXX) =   ZERO
                   PERFORM 2001111-RECE100-WRITE-SEC
                   MOVE    TBL-RECE100-MAX     TO  IDXX
               ELSE
                   IF      TBL-RECE100-KEY (IDXX) =
                                               WRK-RECE100-KEY
                       PERFORM 2001112-RECE100-REWRITE-SEC
                       MOVE    TBL-RECE100-MAX     TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
           .
       200111-FILE-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       2001111-RECE100-WRITE-SEC           SECTION.
      *
           MOVE    COMB-HKNNUM         TO  WRK-RECE100-HKNNUM
           MOVE    COMB-HKNCOMBINUM    TO  WRK-RECE100-HKNCOMBI
           MOVE    ACCT-SRYKA          TO  WRK-RECE100-SRYKA (1)
      *    
      *    主保険情報
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                           PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM 910-PTHKNINF-INV-SEC
           MOVE    PTHKN-KIGO          TO  WRK-RECE100-KIGO
           MOVE    PTHKN-NUM           TO  WRK-RECE100-NUM
      *
      *    患者情報
           MOVE   PTINF-KANANAME       TO  WRK-RECE100-KANANAME
           MOVE   PTINF-NAME           TO  WRK-RECE100-NAME
           MOVE   PTINF-SEX            TO  WRK-RECE100-SEX
           MOVE   PTINF-BIRTHDAY       TO  WRK-RECE100-BIRTHDAY
           MOVE   PTINF-DEATHKBN       TO  WRK-RECE100-DEATHKBN
           MOVE   PTINF-TSTPTNUMKBN    TO  WRK-RECE100-TSTPTNUMKBN
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
grpsys     MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-RECE100-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
grpsys                                         SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-RECE100-PTNUM
      *    年齢
           INITIALIZE                      ORCSAGECHKAREA
grpsys     MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    WRK-RECE100-PTID     TO  AGECHK-PTID
           MOVE    WRK-STYMD           TO  AGECHK-SRYYMD
           MOVE    WRK-RECE100-BIRTHDAY TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA 
           IF      AGECHK-RC           =   ZERO
               MOVE  AGECHK-NENREI         TO  WRK-RECE100-AGE
           END-IF
      *    旧姓履歴
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    WRK-RECE100-PTID     TO  KYUSEI-PTID
           MOVE    WRK-EDYMD           TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK      =   ZERO
               MOVE    KYUSEI-NAME         TO  WRK-RECE100-NAME
               MOVE    KYUSEI-KANANAME     TO  WRK-RECE100-KANANAME
           END-IF
      *
      *    印刷順（個別作成時の個別指定のみ）
           IF    ( WRK-PARA-SYOKBN     =   "2"              )
           AND   ( BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                   OR  "05" )
               ADD     1                       TO  WRK-PRTSEQ
               MOVE    WRK-PRTSEQ              TO  WRK-RECE100-PRTSEQ
           END-IF
      *    未請求フラグ
      *****MOVE    SYUKA-MISEIKYU-FLG  TO  WRK-RECE100-MISEIKYU-FLG
      *    レセプト対象年月日情報
           MOVE    WRK-RECE100-TEKSTYMD
                                       TO  WRK-RECE100-TEKSTYMD-X
           MOVE    COMB-TEKEDYMD       TO  WRK-RECE100-TEKEDYMD-X
      *
      *    レセプト種別不明エラー情報
           MOVE    WRK-TEISEIKBN       TO  WRK-RECE100-TEISEIKBN
           IF      WRK-TEISEIKBN       =   "1"
               MOVE    WRK-ERR-ERRKBN      TO  WRK-RECE100-ERR-ERRKBN
               MOVE    WRK-ERR-DAY         TO  WRK-RECE100-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM 
                                           TO
                                           WRK-RECE100-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD    TO  WRK-RECE100-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD    TO  WRK-RECE100-ERR-TEKEDYMD
           END-IF 
      *
      *    レセプト種別
           IF      WRK-TEISEIKBN       =   "1"
               MOVE    ZERO                TO  WRK-RECE100-RECESYUBETU
           ELSE
               MOVE    7200                TO  WRK-RECE100-RECESYUBETU
           END-IF
      *
           MOVE    WRK-RECE100-REC     TO  TBL-RECE100-OCC (IDXX)
      *
           .
       2001111-RECE100-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       2001112-RECE100-REWRITE-SEC            SECTION.
      *
           MOVE    TBL-RECE100-OCC (IDXX)  TO  WRK-RECE100-REC
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
               IF      WRK-RECE100-SRYKA (IDX) =   SPACE
                   MOVE    ACCT-SRYKA      TO  WRK-RECE100-SRYKA (IDX)
                   MOVE    20              TO  IDX
               ELSE
                   IF      WRK-RECE100-SRYKA (IDX) =   ACCT-SRYKA
                       MOVE    20                  TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-TEISEIKBN           =   "1"
               IF      WRK-RECE100-TEISEIKBN    =   SPACE
                   MOVE    WRK-ERR-ERRKBN  TO  WRK-RECE100-ERR-ERRKBN
                   MOVE    WRK-ERR-DAY     TO  WRK-RECE100-ERR-DAY
                   MOVE    WRK-ERR-HKNCOMBINUM TO 
                                           WRK-RECE100-ERR-HKNCOMBINUM
                   MOVE    WRK-ERR-TEKSTYMD
                                           TO  WRK-RECE100-ERR-TEKSTYMD
                   MOVE    WRK-ERR-TEKEDYMD
                                           TO  WRK-RECE100-ERR-TEKEDYMD
               END-IF            
               MOVE    "1"                 TO  WRK-RECE100-TEISEIKBN     
           END-IF          
      *
           MOVE    WRK-RECE100-REC     TO  TBL-RECE100-OCC (IDXX)
           .
       2001112-RECE100-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本ファイル出力処理
      *****************************************************************
       200112-KIHON-WRITE-SEC              SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       TBL-RECE100-MAX
                   OR      TBL-RECE100-HOSPNUM (IDXX)  =   ZERO
               MOVE    TBL-RECE100-OCC (IDXX)      TO  RECE100-REC
      *
      *        レセプト種別
               PERFORM 2001121-RECESYUBETU-SET-SEC
      *
               DISPLAY "RECE100 PTNUM=" RECE100-PTNUM
                       " RECESYUBETU="  RECE100-RECESYUBETU 
      *
               WRITE   RECE100-REC
      *
               IF      STS-RECE01          =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-ERRMSG-AREA
                   INITIALIZE                      WRK-ERRMSG-AREA
      *
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE100PARA         TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE01          TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
      *
               ADD     1                    TO  CNT-RECE100
           END-PERFORM
           .
       200112-KIHON-WRITE-EXT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       2001121-RECESYUBETU-SET-SEC         SECTION.
      *
           IF      RECE100-TEISEIKBN   =   "1"
               MOVE    ZERO                TO  RECE100-RECESYUBETU
           ELSE
               MOVE    7200                TO  RECE100-RECESYUBETU
           END-IF
           .
        2001121-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
                   PERFORM 20021-KOBETU-REAL-SEC
                   MOVE    "1"   TO  PTINF-TSTPTNUMKBN
                   GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
           EVALUATE    BTPARA-INFO-KBN
               WHEN    "01"
               WHEN    "04"
               WHEN    "05"
      *            レセプト指示（個別指示）コード読み込み
                   INITIALIZE                      SYS-2002-REC
                   MOVE    "2002"              TO  SYS-2002-KANRICD
                   MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                                   SYS-2002-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
                   MOVE    SYS-2002-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
      *
                   PERFORM     UNTIL       FLG-SYSKANRI    =   1
                               OR          FLG-END         =   1
                       PERFORM 20021-KOBETU-HENSYU-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               WHEN    "02"
      *            該当診療年月の医療機関情報取得
                   MOVE  ZERO                  TO  FLG-SET
                   MOVE  BTPARA-INFO-PARA(17:6)
                                               TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                            UNTIL ( SYSKAN-IDX              >  100  )
                            OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
                       MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                       IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                           MOVE     1       TO    FLG-SET
                       END-IF
                   END-PERFORM
                   IF     FLG-SET              =   ZERO
                       STRING
                       "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                            BTPARA-INFO-PARA(1:6)
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                   ELSE
      *                診療年月編集
                       MOVE     BTPARA-INFO-PARA(17:6)
                                               TO  WRK-STYMD (1:6)
                       MOVE    "01"            TO  WRK-STYMD (7:2)
                       INITIALIZE                      STS-AREA-DAY
                       INITIALIZE                      LNK-DAY7-AREA
                       MOVE    "71"            TO  LNK-DAY7-IRAI
                       MOVE    WRK-STYMD (1:6) TO  LNK-DAY7-YM
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                       MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *                対象患者のよる抽出処理
                       INITIALIZE                      PTINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD    
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       MOVE    "6"                 TO  PTINF-HKNKBN
                       MOVE    BTPARA-INFO-PARA(17:6) 
                                                   TO  PTINF-SRYYM
                       MOVE    BTPARA-INFO-PARA(17:6) 
                                                   TO  PTINF-SRYYMD(1:6)
                       MOVE    "__"                TO  PTINF-SRYYMD(7:2)
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       MOVE    "tbl_ptinf"         TO  WRK-MCP-TABLE
                       MOVE    "key14"             TO  WRK-MCP-PATHNAME
                       PERFORM 20022-JYUSINYMD-HENSYU-SEC
                   END-IF
               WHEN    "03"
      *            該当診療年月の医療機関情報取得
                   MOVE  ZERO                  TO  FLG-SET
                   MOVE  BTPARA-INFO-PARA(1:6) TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                            UNTIL ( SYSKAN-IDX              >  100  )
                            OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                            OR    ( FLG-SET                 =   1   )
                       MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                       IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                       AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                           MOVE     1       TO    FLG-SET
                       END-IF
                   END-PERFORM
                   IF     FLG-SET              =   ZERO
                       STRING
                       "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                            BTPARA-INFO-PARA(1:6)
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                   ELSE
      *                診療年月編集
                       MOVE     BTPARA-INFO-PARA(1:6)
                                               TO  WRK-STYMD (1:6)
                       MOVE    "01"            TO  WRK-STYMD (7:2)
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY7-AREA
                       MOVE    "71"            TO  LNK-DAY7-IRAI
                       MOVE    WRK-STYMD (1:6) TO  LNK-DAY7-YM
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                       MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *                対象患者のよる抽出処理
                       INITIALIZE                      PTINF-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD    
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       MOVE    "6"                 TO  PTINF-HKNKBN
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "tbl_ptinf"         TO  WRK-MCP-TABLE
                       MOVE    "key12"             TO  WRK-MCP-PATHNAME
                       PERFORM 20022-JYUSINYMD-HENSYU-SEC
                   END-IF
           END-EVALUATE
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           INITIALIZE                  TABLE-RECE100-AREA
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "2"              TO  SYS-2002-NYUGAIKBN
      *
      *****MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
      *****MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
      *****PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      *****        " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    該当診療年月の医療機関情報取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
           END-PERFORM
           IF     FLG-SET              =   ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    患者情報取得
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
      *
           IF      FLG-END             =   ZERO
      *        診療会計読み込み
               INITIALIZE                  SRYACCT-REC
grpsys         MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SYS-2002-PTID       TO  ACCT-PTID
               MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
               MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                   MOVE    "key61"             TO  WRK-SRYACCT-PATHNAME
               ELSE
                   MOVE    "key37"             TO  WRK-SRYACCT-PATHNAME
               END-IF
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1
                   PERFORM 20011-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *        ファイル出力処理
               PERFORM   200112-KIHON-WRITE-SEC
           END-IF
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  TABLE-RECE100-AREA
      *
           MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
      *****MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
      *****MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
      *****PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      *****        " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    未請求設定時は読み飛ばす
      *****IF    SYUKA-MISEIKYU-FLG  =   "01"
      *****    CONTINUE
      *****ELSE
      *
      *        該当診療年月の医療機関情報取得
               MOVE  ZERO           TO  FLG-SET
               MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
               PERFORM VARYING  SYSKAN-IDX  FROM  1  BY  1
                       UNTIL ( SYSKAN-IDX              >  100  )
                       OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                       OR    ( FLG-SET                 =   1   )
                   MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                   IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                   AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                       MOVE     1       TO    FLG-SET
                   END-IF
               END-PERFORM
               IF     FLG-SET              =   ZERO
                   STRING
                       "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE 
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO       TO      20021-KOBETU-HENSYU-EXT
               END-IF
      *
      *        患者情報取得
grpsys         MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SYS-2002-PTID       TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
      *        診療年月編集
               MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
               MOVE    "01"                TO  WRK-STYMD (7:2)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
      *
               IF      FLG-END             =   ZERO
      *            診療会計読み込み
                   INITIALIZE                  SRYACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  ACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                       MOVE    "key61"         TO  WRK-SRYACCT-PATHNAME
                   ELSE
                       MOVE    "key37"         TO  WRK-SRYACCT-PATHNAME
                   END-IF
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
      *
                   PERFORM UNTIL       FLG-SRYACCT     =   1
                           OR          FLG-END         =   1
                       PERFORM 20011-HENSYU-SEC
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    WRK-SRYACCT-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
      *            ファイル出力処理
                   PERFORM   200112-KIHON-WRITE-SEC
               END-IF
      *****END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
      *
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日指定編集処理
      *****************************************************************
       20022-JYUSINYMD-HENSYU-SEC              SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                  TO  FLG-PTINF
           END-IF
      *
           PERFORM     UNTIL       FLG-PTINF       =   1
                       OR          FLG-END         =   1
      *
               INITIALIZE                  TABLE-RECE100-AREA
      *
               EVALUATE    BTPARA-INFO-KBN
                   WHEN    "02"
                       MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  WRK-SYUKA-SRYYM
                   WHEN    "03"
                       MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  WRK-SYUKA-SRYYM
               END-EVALUATE
               MOVE    PTINF-PTID      TO  WRK-SYUKA-PTID
      *********PERFORM 800-MISEIKYU-CHECK-SEC
      *
               DISPLAY "JYUSIN PTID=" PTINF-PTID 
                       " SRYYM="   WRK-SYUKA-SRYYM
      *********        " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *        未請求設定時は読み飛ばす
      *********IF    SYUKA-MISEIKYU-FLG  =   "01"
      *********    CONTINUE
      *********ELSE
      *
      *            処理中止設定処理
                   PERFORM 500-CANCEL-HENSYU-SEC
      *
                   IF      FLG-END             =   ZERO
      *                診療会計読み込み
                       INITIALIZE                  SRYACCT-REC
grpsys                 MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                       MOVE    PTINF-PTID          TO  ACCT-PTID
                       EVALUATE    BTPARA-INFO-KBN
                           WHEN    "02"
                               MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  ACCT-SRYYM
                           WHEN    "03"
                               MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  ACCT-SRYYM
                       END-EVALUATE
                       MOVE    "2"                 TO  ACCT-NYUGAIKBN
                       MOVE    SRYACCT-REC         TO  MCPDATA-REC
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                           MOVE    "key61"     TO  WRK-SRYACCT-PATHNAME
                       ELSE
                           MOVE    "key37"     TO  WRK-SRYACCT-PATHNAME
                       END-IF
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
grpsys                  PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       ELSE
                           INITIALIZE                  SRYACCT-REC
                           MOVE    1                   TO  FLG-SRYACCT
                       END-IF
      *
                       PERFORM UNTIL       FLG-SRYACCT     =   1
                               OR          FLG-END         =   1
                           PERFORM 20011-HENSYU-SEC
                       END-PERFORM
      *                カーソルクロース
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
      *                ファイル出力処理
                       PERFORM   200112-KIHON-WRITE-SEC
                   END-IF
      *********END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-CLOSE-SEC
      *
           .
       20022-JYUSINYMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-SRYACCT     =   1 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   IF      COMB-HKNNUM     =   WRK-CONS-KOUGAI-HKNNUM
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                       ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   ELSE                                         
                       MOVE    "1"             TO  WRK-ROUSAIKBN
                   END-IF    
               ELSE
                   MOVE    "1"                 TO  WRK-TEISEIKBN
                   MOVE    1                   TO  WRK-ERR-ERRKBN
                   MOVE    ACCT-HKNCOMBI       TO  WRK-ERR-HKNCOMBINUM        
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1
                               OR      KEY-NEW     NOT =   KEY-OLD
                   IF      WRK-ROUSAIKBN       =   SPACE
                       PERFORM 200111-FILE-SYORI-SEC
                   END-IF 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       20011-HENSYU-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
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
           STRING  "ORCR1000 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE100-FILE
      *
           IF      CNT-RECE100            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *     
           DISPLAY "RECE100  CNT-OUT :" CNT-RECE100
           DISPLAY "*** ORCR1000 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求フラグチェック処理
      *****************************************************************
       800-MISEIKYU-CHECK-SEC          SECTION.
      *
           INITIALIZE                      SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYUKA-HOSPNUM
           MOVE    WRK-SYUKA-PTID      TO  SYUKA-PTID
           MOVE    WRK-SYUKA-SRYYM     TO  SYUKA-SRYYM
           MOVE    "2"                 TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"              TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           PERFORM 900-SYUKA-READ-SEC
           .
       800-MISEIKYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
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
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-SYUKA
                   MOVE    MCPDATA-REC          TO  SYUKA-REC
               ELSE
                   MOVE    1                    TO  FLG-SYUKA
                   INITIALIZE                       SYUKA-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
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
               STRING  "ORCR1000 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
