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
       PROGRAM-ID.             SOKATU0605.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 山形・広域連合・診療報酬総括表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/05/16    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/06/10  県外分の集計の不具合を修正
      *  01.00.02    吉川    2009/03/12  点数表区分追加
      *  01.00.03    吉川    2010/02/26  広域連合の保険者番号指定対応
      *  01.00.04    吉川    2010/07/02  オンライン返戻対応
      *  01.00.05    吉川    2012/05/10  帳票様式の変更に伴う修正
      *                                  PERFORM→GOTOに修正
      *  01.00.06    吉川    2012/12/21  医療機関情報編集対応
      *  01.00.07    吉川    2014/10/03  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.08    萩野    2015/10/08  医療機関編集情報への開設者名追加対応
      *  01.00.09    吉川    2017/07/14  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.10    萩野    2018/05/08  SITEKBN変更（動作に変更なし）
      *  01.00.11    井上    2018/10/11  修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *     
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT0605.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOH-END         PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *     
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PRINT1          PIC 9(01).
           03  FLG-PRINT2          PIC 9(01).
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
      *     
           03  IDZ                 PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-SYOKBN1    PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-CNT             PIC 9(02).
           03  WRK-GPAGE           PIC 9(02).
           03  WRK-CNTPAGE         PIC 9(03).
           03  WRK-AMARI           PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-HKNJANAME       PIC X(20).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z14             PIC ZZ,ZZZ,ZZZ,ZZZ.
           03  WRK-KOHNUM9         PIC Z9.
           03  WRK-PAGE            PIC ZZZ9.
      *
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
      *
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-ERRMSG          PIC X(300). 
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SYOKBN          PIC X(01).
           03  WRK-INDEX           PIC 9(02).
      *
      *    公費名称編集
           03  WRK-KOHKBN.
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-HKNNAME       PIC X(20). 
           03  WRK-MEISYO          PIC X(44).
      *
      *    医療機関情報
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-KAISETUNAME     PIC X(80).
      *
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  10.
           03  WRK-2007-MAX        PIC 9(03).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KOH             PIC 9(02).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-TBL   OCCURS  2.
               05  SUM-HKNJA-TBL   OCCURS  100.
                   07  SUM-HKNJANAME           PIC  X(20).
                   07  SUM-HENREIYM            PIC  X(06).
                   07  SUM-KAMIBUN             PIC  X(12).
                   07  SUM-NYUGAI-TBL  OCCURS  2.
                       09  SUM-KENSU           PIC  9(07).
                       09  SUM-NISSU           PIC  9(07).
                       09  SUM-TENSU           PIC  9(14).
               05  SUM-GOKEI-TBL       OCCURS  2.
                   07  SUM-GKENSU              PIC 9(07).
                   07  SUM-GNISSU              PIC 9(07).
                   07  SUM-GTENSU              PIC 9(14).
      *
      *    キー領域
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM      PIC X(08).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM      PIC X(08). 
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *    診療科目情報
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
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    レセプト・総括印刷情報（広域連合）
           COPY    "CPSK200501.INC".
      *
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      * 
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   半角チェックサブ
           COPY  "CPORCSKANACHK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU0605"    TO  JOB-PGID
           MOVE    "広域連合・診療報酬総括表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
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
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF    
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
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
      *    公費番号まとめ情報
           INITIALIZE                      WRK-2007-AREA
      *
           INITIALIZE                      SYS-2007-REC
           MOVE    "2007"              TO  SYS-2007-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2007-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2007-STYUKYMD (7:2)
           MOVE    SYS-2007-STYUKYMD   TO  SYS-2007-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2007-HOSPNUM
           MOVE    SYS-2007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX            >=  100
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX
               MOVE    SYS-2007-MEISYO(1) TO  WRK-2007-MEISYO(IDX)
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      SYS-2007-KOHNUM (IDY)   =   SPACE
                   MOVE    SYS-2007-KOHNUM (IDY)
                                           TO WRK-2007-KOHNUM(IDX IDY)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    IDX                     TO  WRK-2007-MAX
      *
      *    レセプト・総括編集情報
      *    区分コード０１のデータ読み込み
           MOVE    SPACE               TO  SYS-200501-REC
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-200501-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200501-STYUKYMD (7:2)
           MOVE    SYS-200501-STYUKYMD TO  SYS-200501-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200501-REC
           ELSE
               INITIALIZE                      SYS-200501-REC
           END-IF
           IF      SYS-200501-KOUIKIPRTKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-200501-KOUIKIPRTKBN
           END-IF
      *
           IF      SYS-200501-KOUIKIPRTKBN
                                       =   "1"
               CONTINUE
           ELSE
               MOVE    "00000000"          TO  WRK-PARA-HKNJANUM
           END-IF
      *
           DISPLAY "SYS-200501-KOUIKIPRTKBN=" SYS-200501-KOUIKIPRTKBN
           DISPLAY "WRK-PARA-HKNJANUM      =" WRK-PARA-HKNJANUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "2"
                   MOVE    "2"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "1"
                   MOVE    "1"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
      *
           CLOSE   UPD-FILE
      *
      *    請求ＤＢ更新
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO 
               IF      CNT-UPDFILE    >   ZERO
                   PERFORM 310-SEIKYU-UPD-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分集計処理
      *****************************************************************
       2001-SYOKBN-SYUKEI-SEC         SECTION.
      *
      *    入院外（その他のレセ）・入院集計
      *    
           MOVE    LOW-VALUE           TO  KEY-AREA
           MOVE    1                   TO  WRK-INDEX
      *
      *    処理区分によってキーを分ける
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "key66"         TO  WRK-MCP-PATHNAME
               WHEN    "2"
                   MOVE    "key65"         TO  WRK-MCP-PATHNAME
           END-EVALUATE  
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM 200-HENSYU-SEC
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2001-SYOKBN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  SUM-AREA
                                       FLG-UPD
                                       FLG-PRINT1
                                       FLG-PRINT2
      *
           MOVE    KEY-NEW             TO  KEY-OLD
      *    保険者別集計処理
           PERFORM 2001-KOUKI-SYUKEI-SEC
      *
      *    印刷処理
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO
                PERFORM 2002-PRINT-SEC
           END-IF
      *
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者別集計処理
      *****************************************************************
       2001-KOUKI-SYUKEI-SEC                SECTION.
      *
           PERFORM  VARYING  WRK-CNT     FROM  1  BY  1
                    UNTIL    WRK-CNT  >  100
                    OR       FLG-RECE10  =  1
                    OR       FLG-ERR     =  1
      *
             INITIALIZE                         FLG-UPD
                                                IDX-AREA
                                                WRK-HKNJANUM
                                                WRK-HKNJANAME
      *
      *      県名取得
             MOVE  RECE10-HKNJANUM     TO   WRK-HKNJANUM
             PERFORM  200101-HKNJANUM-SET-SEC
      *      県内県外区分
             EVALUATE    RECE10-PREFKBN
               WHEN    "1"
      *          県名
                 MOVE  WRK-HKNJANAME       TO  SUM-HKNJANAME(1 1)
                 MOVE  1                   TO  FLG-PRINT1
                 COMPUTE  WRK-CNT  =  WRK-CNT  -  1
               WHEN    "2"
      *          県名
                 MOVE  WRK-HKNJANAME       TO  SUM-HKNJANAME(2 WRK-CNT)
                 MOVE  1                   TO  FLG-PRINT2
             END-EVALUATE
      *
      *      キーが変わるかデータが終わるまでループ
             PERFORM  UNTIL  KEY-NEW   NOT =  KEY-OLD
                      OR     FLG-RECE10    =  1
                      OR     FLG-ERR       =  1
      *
      *        入外区分
               EVALUATE    RECE10-NYUGAIKBN
                   WHEN    "1"
                       MOVE    1            TO  IDX1
                   WHEN    "2"
                       MOVE    2            TO  IDX1
               END-EVALUATE
      *
               EVALUATE    RECE10-PREFKBN
                   WHEN    "1"
                      PERFORM 20011-HKN-SYUKEI-SEC
                   WHEN    "2"
                      PERFORM 20012-HKN-SYUKEI-SEC
               END-EVALUATE
      *
      *        集計の対象にならなかったとき
               IF      FLG-UPD             =   1
                 PERFORM 20013-UPD-FILE-HENSYU-SEC
      *
                 MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                 MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                 PERFORM 900-SEIKYU-READ-SEC
               ELSE
                 MOVE    SPACE               TO  WRK-RECEERR
                 STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                        RECE10-PTNUM                DELIMITED  BY  SPACE
                        " 診療年月＝"               DELIMITED  BY  SIZE
                        RECE10-SRYYM                DELIMITED  BY  SIZE
                        " レセ種別＝"               DELIMITED  BY  SIZE
                        RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                            INTO    WRK-RECEERR
                 END-STRING
                 PERFORM 500-ERR-HENSYU-SEC
                 PERFORM 500-COBABORT-SEC
               END-IF
      *
             END-PERFORM
      *
      *      次のキーを保存
             MOVE    KEY-NEW                 TO  KEY-OLD
      *
           END-PERFORM
      *
           .
       2001-KOUKI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県名セット
      *****************************************************************
       200101-HKNJANUM-SET-SEC                SECTION.
      *
           EVALUATE  WRK-HKNJANUM(3:2)
      *      北海道
             WHEN  "01"  MOVE  "北海道"    TO  WRK-HKNJANAME
      *      青森県
             WHEN  "02"  MOVE  "青森県"    TO  WRK-HKNJANAME
      *      岩手県
             WHEN  "03"  MOVE  "岩手県"    TO  WRK-HKNJANAME
      *      宮城県
             WHEN  "04"  MOVE  "宮城県"    TO  WRK-HKNJANAME
      *      秋田県
             WHEN  "05"  MOVE  "秋田県"    TO  WRK-HKNJANAME
      *      山形県
             WHEN  "06"  MOVE  "山形県"    TO  WRK-HKNJANAME
      *      福島県
             WHEN  "07"  MOVE  "福島県"    TO  WRK-HKNJANAME
      *      茨城県
             WHEN  "08"  MOVE  "茨城県"    TO  WRK-HKNJANAME
      *      栃木県
             WHEN  "09"  MOVE  "栃木県"    TO  WRK-HKNJANAME
      *      群馬県
             WHEN  "10"  MOVE  "群馬県"    TO  WRK-HKNJANAME
      *      埼玉県
             WHEN  "11"  MOVE  "埼玉県"    TO  WRK-HKNJANAME
      *      千葉県
             WHEN  "12"  MOVE  "千葉県"    TO  WRK-HKNJANAME
      *      東京都
             WHEN  "13"  MOVE  "東京都"    TO  WRK-HKNJANAME
      *      神奈川県
             WHEN  "14"  MOVE  "神奈川県"  TO  WRK-HKNJANAME
      *      新潟県
             WHEN  "15"  MOVE  "新潟県"    TO  WRK-HKNJANAME
      *      富山県
             WHEN  "16"  MOVE  "富山県"    TO  WRK-HKNJANAME
      *      石川県
             WHEN  "17"  MOVE  "石川県"    TO  WRK-HKNJANAME
      *      福井県
             WHEN  "18"  MOVE  "福井県"    TO  WRK-HKNJANAME
      *      山梨県
             WHEN  "19"  MOVE  "山梨県"    TO  WRK-HKNJANAME
      *      長野県
             WHEN  "20"  MOVE  "長野県"    TO  WRK-HKNJANAME
      *      岐阜県
             WHEN  "21"  MOVE  "岐阜県"    TO  WRK-HKNJANAME
      *      静岡県
             WHEN  "22"  MOVE  "静岡県"    TO  WRK-HKNJANAME
      *      愛知県
             WHEN  "23"  MOVE  "愛知県"    TO  WRK-HKNJANAME
      *      三重県
             WHEN  "24"  MOVE  "三重県"    TO  WRK-HKNJANAME
      *      滋賀県
             WHEN  "25"  MOVE  "滋賀県"    TO  WRK-HKNJANAME
      *      京都府
             WHEN  "26"  MOVE  "京都府"    TO  WRK-HKNJANAME
      *      大阪府
             WHEN  "27"  MOVE  "大阪府"    TO  WRK-HKNJANAME
      *      兵庫県
             WHEN  "28"  MOVE  "兵庫県"    TO  WRK-HKNJANAME
      *      奈良県
             WHEN  "29"  MOVE  "奈良県"    TO  WRK-HKNJANAME
      *      和歌山県
             WHEN  "30"  MOVE  "和歌山県"  TO  WRK-HKNJANAME
      *      鳥取県
             WHEN  "31"  MOVE  "鳥取県"    TO  WRK-HKNJANAME
      *      島根県
             WHEN  "32"  MOVE  "島根県"    TO  WRK-HKNJANAME
      *      岡山県
             WHEN  "33"  MOVE  "岡山県"    TO  WRK-HKNJANAME
      *      広島県
             WHEN  "34"  MOVE  "広島県"    TO  WRK-HKNJANAME
      *      山口県
             WHEN  "35"  MOVE  "山口県"    TO  WRK-HKNJANAME
      *      徳島県
             WHEN  "36"  MOVE  "徳島県"    TO  WRK-HKNJANAME
      *      香川県
             WHEN  "37"  MOVE  "香川県"    TO  WRK-HKNJANAME
      *      愛媛県
             WHEN  "38"  MOVE  "愛媛県"    TO  WRK-HKNJANAME
      *      高知県
             WHEN  "39"  MOVE  "高知県"    TO  WRK-HKNJANAME
      *      福岡県
             WHEN  "40"  MOVE  "福岡県"    TO  WRK-HKNJANAME
      *      佐賀県
             WHEN  "41"  MOVE  "佐賀県"    TO  WRK-HKNJANAME
      *      長崎県
             WHEN  "42"  MOVE  "長崎県"    TO  WRK-HKNJANAME
      *      熊本県
             WHEN  "43"  MOVE  "熊本県"    TO  WRK-HKNJANAME
      *      大分県
             WHEN  "44"  MOVE  "大分県"    TO  WRK-HKNJANAME
      *      宮崎県
             WHEN  "45"  MOVE  "宮崎県"    TO  WRK-HKNJANAME
      *      鹿児島県
             WHEN  "46"  MOVE  "鹿児島県"  TO  WRK-HKNJANAME
      *      沖縄県
             WHEN  "47"  MOVE  "沖縄県"    TO  WRK-HKNJANAME
      *      未取得
             WHEN  OTHER MOVE  SPACE       TO  WRK-HKNJANAME
           END-EVALUATE
      *
           .
       200101-HKNJANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理（県内）
      *****************************************************************
       20011-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX1             =   ZERO
               PERFORM  20011-HKN-SYUKEI-EXT
           END-IF
      *
      *    療養の給付：件数
           ADD  1                  TO  SUM-KENSU(1 1 IDX1)
                                       SUM-GKENSU(1 IDX1)
      *    療養の給付：日数
           ADD  RECE10-JNISSU(1)   TO  SUM-NISSU(1 1 IDX1)
                                       SUM-GNISSU(1 IDX1)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1) TO  SUM-TENSU(1 1 IDX1)
                                       SUM-GTENSU(1 IDX1)
      *
           MOVE    1               TO  FLG-UPD
      *
           .
       20011-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理（県外）
      *****************************************************************
       20012-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX1             =   ZERO
               PERFORM  20012-HKN-SYUKEI-EXT
           END-IF
      *
      *    療養の給付：件数
           ADD  1                  TO  SUM-KENSU(2 WRK-CNT IDX1)
                                       SUM-GKENSU(2 IDX1)
      *    療養の給付：日数
           ADD  RECE10-JNISSU(1)   TO  SUM-NISSU(2 WRK-CNT IDX1)
                                       SUM-GNISSU(2 IDX1)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1) TO  SUM-TENSU(2 WRK-CNT IDX1)
                                       SUM-GTENSU(2 IDX1)
      *
           MOVE    1               TO  FLG-UPD
      *
           .
       20012-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       20013-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           INITIALIZE                      RECE10X-UPHKNJANUM
                                           RECE10X-XXPREFNUM
                                           RECE10X-XXSRYYM
                                           RECE10X-XXSTHKNJANUM
                                           RECE10X-XXEDHKNJANUM
                                           RECE10X-XXTEISYUTUSAKI
           WRITE   RECE10X-REC
      *
           IF      STS-RECEUPD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "更新用ファイル 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                       STS-RECEUPD                 DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF 
           ADD     1                   TO  CNT-UPDFILE
           .
       20013-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *
      *    県内
           IF  FLG-PRINT1  =  1
             MOVE   ZERO        TO       FLG-PRINT
             MOVE   1           TO       IDX
      *
      *      集計テーブル用添字初期化
             MOVE   1           TO       IDX1
      *      帳票編集<見出し>処理
             PERFORM 20021-HEAD-HEN-SEC
      *      帳票編集<明細>処理
             PERFORM 20023-BODY-HEN-SEC
      *      帳票編集<合計>処理
             PERFORM 20024-GOKEI-HEN-SEC
      *      帳票印刷処理
             PERFORM 20025-PRINT-OUT-SEC
           END-IF
      *
      *    県外
           IF  FLG-PRINT2  =  1
             MOVE   ZERO        TO       FLG-PRINT
             MOVE   2           TO       IDX
      *
      *      集計テーブル用添字初期化
             MOVE   1           TO       IDX1
             PERFORM  UNTIL  FLG-PRINT  =  1
      *
      *        帳票編集<見出し>処理
               PERFORM 20021-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 20023-BODY-HEN-SEC
      *        帳票編集<合計>処理
               PERFORM 20024-GOKEI-HEN-SEC
      *        帳票印刷処理
               PERFORM 20025-PRINT-OUT-SEC
      *
             END-PERFORM
           END-IF
      *    
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      SKT0605
      *    診療年月
           MOVE    WRK-SRYYMWH         TO  SKT0605-SRYYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SKT0605-ADRS
           INSPECT SKT0605-ADRS        REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SKT0605-HOSPNAME
           INSPECT SKT0605-HOSPNAME    REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME     TO  SKT0605-KAISETUNAME
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD    TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT      TO  SKT0605-HOSPCD
           EVALUATE SYS-1001-TENHYOKBN
             WHEN    1
               MOVE    "○"            TO  SKT0605-TENHYOKBN(1)
             WHEN    3
               MOVE    "○"            TO  SKT0605-TENHYOKBN(2)
             WHEN    4
               MOVE    "○"            TO  SKT0605-TENHYOKBN(3)
             WHEN    6
               MOVE    "○"            TO  SKT0605-TENHYOKBN(4)
           END-EVALUATE
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20023-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX2  FROM    1   BY  1
                    UNTIL    IDX2  >       26
                    OR       FLG-PRINT   =  1
      *
      *
             IF  SUM-HKNJANAME(IDX IDX1)  =  SPACE
               MOVE    1                   TO   FLG-PRINT
             ELSE
      *    保険名(県名)
               MOVE    SUM-HKNJANAME(IDX IDX1)  
                                           TO  SKT0605-HKNJANAME(IDX2)
      *
               PERFORM VARYING IDX3 FROM    1   BY  1
                       UNTIL   IDX3 >       2
      *    療養の給付：件数
                 MOVE  SUM-KENSU(IDX IDX1 IDX3) TO  WRK-Z7
                 MOVE  WRK-Z7              TO  SKT0605-KENSU(IDX2 IDX3)
      *    療養の給付：日数
                 MOVE  SUM-NISSU(IDX IDX1 IDX3) TO  WRK-Z7
                 MOVE  WRK-Z7              TO  SKT0605-NISSU(IDX2 IDX3)
      *    療養の給付：点数
                 MOVE  SUM-TENSU(IDX IDX1 IDX3) TO  WRK-Z14
                 MOVE  WRK-Z14             TO  SKT0605-TENSU(IDX2 IDX3)
      *
               END-PERFORM
      *
               ADD   1                     TO  IDX1
      *
             END-IF
           END-PERFORM
      *
           IF  SUM-HKNJANAME(IDX IDX1)  =  SPACE
               MOVE    1                   TO   FLG-PRINT
           END-IF
      *
           .
       20023-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計>処理
      *****************************************************************
       20024-GOKEI-HEN-SEC          SECTION.
      *
           PERFORM  VARYING  IDY  FROM  1  BY  1
                    UNTIL    IDY  >     2
      *        合計：件数
               MOVE  SUM-GKENSU(IDX IDY)  TO  WRK-Z7
               MOVE  WRK-Z7  TO  SKT0605-KENSU(27 IDY)
      *        合計：日数
               MOVE  SUM-GNISSU(IDX IDY)  TO  WRK-Z7
               MOVE  WRK-Z7  TO  SKT0605-NISSU(27 IDY)
      *        合計：点数
               MOVE  SUM-GTENSU(IDX IDY)  TO  WRK-Z14
               MOVE  WRK-Z14 TO  SKT0605-TENSU(27 IDY)
           END-PERFORM
      *
           .
       20024-GOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20025-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
           ADD     1                   TO  CNT-PAGE
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
           MOVE    "SKT0605.red"         TO  SPRT-PRTID
           MOVE    SKT0605               TO  SPRT-PRTDATA
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "後期高齢者総括表"    TO  SPRT-TITLE
               WHEN    "2"
                   MOVE    "後期高齢者総括表［返戻分］" 
                                               TO  SPRT-TITLE
           END-EVALUATE
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
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       20025-PRINT-OUT-EXT.
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
      *
           MOVE    1                   TO  FLG-END     
                                           FLG-ERR
      *
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
           STRING  "SOKATU0605 "         DELIMITED  BY  SIZE
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
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** SOKATU0605 IN   "  CNT-RECE10 
           DISPLAY "***          UPD  "  CNT-UPDFILE 
           DISPLAY "***          PAGE "  CNT-PAGE 
           DISPLAY "*** SOKATU0605 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *                              
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求管理ＤＢ更新処理
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
                   MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD TO  RECE10-UPDYMD  
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       STRING "請求管理ＤＢ　更新エラー　"
                                               DELIMITED  BY  SIZE
                               RECE10-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
               ELSE
                   STRING "請求管理ＤＢ　読込み更新エラー　"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
      *     
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           MOVE    WRK-CONS-SRYYM-200804
                                       TO  RECE10-SRYYM
           MOVE    999999              TO  RECE10-XXSRYYM
           IF      WRK-PARA-HKNJANUM   NOT =   ZERO
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
                                               RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           MOVE    "039"               TO  RECE10-HKNNUM-S
           MOVE    "040"               TO  RECE10-HKNNUM
      *
      *###
           DISPLAY "SYOKBN=" WRK-SYOKBN
                   " SRYYM=" RECE10-SRYYM "-" RECE10-XXSRYYM 
                   " HKNNUM= " RECE10-HKNNUM
                   " PATHNAME=" WRK-MCP-PATHNAME
                   " HKNJANUM=" RECE10-XXSTHKNJANUM "-" 
                                RECE10-XXEDHKNJANUM
      *###
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECE10
               MOVE    MCPDATA-REC         TO  RECE10-REC
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *        キーに県番号を格納
               MOVE    RECE10-HKNJANUM(1:4)  TO  KEY-N-HKNJANUM
               ADD     1                     TO  CNT-RECE10   
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM 
      *         
           ELSE
               MOVE    HIGH-VALUE      TO  KEY-NEW
               MOVE    1               TO  FLG-RECE10
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
       800-PTINF-READ-EXT.
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
           END-READ
           .
       900-UPD-FILE-READ-EXT.
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
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1900-STYUKYMD(7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(17)  NOT =   SPACE
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
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(17)
                                   TO  SYS-1901-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1901-STYUKYMD(7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
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
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(17)
                                   TO  SYS-1902-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1902-STYUKYMD(7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
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
           MOVE    SYS-1900-PRTKBN(17) TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-KEY10-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-INV-EXT.
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
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
