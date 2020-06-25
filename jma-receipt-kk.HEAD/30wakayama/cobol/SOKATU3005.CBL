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
       PROGRAM-ID.             SOKATU3005.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 広域連合・診療報酬請求書（医科・歯科）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/03/11    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者     日付      内容
      *  04.02.01    NACL-藤原  08/04/21  広域連合診療報酬請求書区分
      *                                   による作成
      *  04.03.01    吉川       10/05/19  WRK-MEISYO初期化
      *                                   シス管「2007」のまとめ公費で
      *                                   6つ目以降がまとまらない不具合を修正
      *                                   オンライン返戻対応
      *                                   広域連合の保険者番号指定対応
      *  04.03.02    吉川       10/12/08  open-cobol1.0対応
      *  04.07.01    吉川       12/11/07  PERFORMをGOTOに修正
      *  04.07.02    吉川       13/01/11  医療機関情報編集対応
      *  04.08.01    吉川       14/10/24  ver4.8.0対応（一時ディレクトリ変更）
      *  04.08.02    萩野       16/02/12  医療機関編集情報への開設者名追加対応
      *  04.08.03    萩野       17/02/14  公費番号まとめ情報の公費番号の追加
      *                                   医療機関コード変更時、
      *                                   医療機関コード別の作成処理対応
      *  04.08.04    萩野       18/04/09  SITEKBN変更（動作に変更なし）
      *  04.08.05    萩野       18/12/12  修正履歴コメント削除
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
       COPY    "COMMON-SPA".
      *
      *    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
           COPY    "SKT3005.INC".
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
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
           03  FLG-RED             PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDY4                PIC 9(04).
           03  IDY5                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
      *     
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-SYOKBN1    PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
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
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-ERRMSG          PIC X(300). 
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-INDEX           PIC 9(02).
           03  WRK-SYOKBN          PIC X(01).
      *
      *    数字項目編集
           03  WRK-KOHNUM9         PIC ZZ9.
           03  WRK-NISSUZ          PIC ZZZZZZ.
           03  WRK-KENSUZ          PIC ZZZZZZ.
           03  WRK-TENSUZ          PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-FTNMONEYZ       PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-KINGAKZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-FTNGAKZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-PAGE            PIC ZZZ9.
      *
      *    公費名称編集
           03  WRK-KOHKBN.
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-HKNNAME       PIC X(20). 
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-TEL               PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
           03  WRK-MEISYO          PIC X(44).
      *
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX        PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE              PIC 9(04).
           03  CNT-RECE10            PIC 9(06).
           03  CNT-UPDFILE           PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
         02  SUM-TABLE-OCC           OCCURS  2.
           03  SUM-TABLE-OCC1            OCCURS  100.
               05  SUM-TBL-SRYKA         PIC X(02).
      *        後期高齢者
               05  SUM-TBL               OCCURS  2.
                   07  SUM-TBL-G         OCCURS  2.
                       09  SUM-KENSU           PIC  9(06).
                       09  SUM-NISSU           PIC  9(06).
                       09  SUM-TOTALTEN        PIC  9(10).
                       09  SUM-FTNMONEY        PIC  9(10).
                   07  SUM-SHOKUJIKENSU        PIC  9(06).
                   07  SUM-SHOKUJINISSU        PIC  9(06).
                   07  SUM-SHOKUJIRYOYOHI      PIC  9(10).
                   07  SUM-SHOKUJIFTN          PIC  9(10).
      *             
      *        公費負担 
               05  SUM-KOH-TABLE. 
                   07  SUM-KOH-TBL     OCCURS  100.
                       09  SUM-KOH-KOHNUM      PIC  X(03).
                       09  SUM-KOH-KOHHBTNUM   PIC  X(02).
                       09  SUM-TBL-G   OCCURS  2.
                           11  SUM-KOH-KENSU   PIC  9(06).
                           11  SUM-KOH-NISSU   PIC  9(06).
                           11  SUM-KOH-TOTALTEN
                                               PIC  9(10).
                           11  SUM-KOH-FTNMONEY
                                               PIC  9(10).
                       09  SUM-KOH-SHOKUJIKENSU
                                               PIC  9(06).
                       09  SUM-KOH-SHOKUJINISSU
                                               PIC  9(06).
                       09  SUM-KOH-SHOKUJIRYOYOHI
                                               PIC  9(10).
                       09  SUM-KOH-SHOKUJIFTN  PIC  9(10).
                       09  SUM-KOH-MEISYO      PIC  X(44).
      *
      *    キー領域
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM.
                   07  KEY-N-HKNJANUM1 PIC X(04).
                   07  KEY-N-HKNJANUM2 PIC X(04).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM.
                   07  KEY-O-HKNJANUM1 PIC X(04).
                   07  KEY-O-HKNJANUM2 PIC X(04).
      *
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    広域連合番号テーブル
           COPY    "CMKOUIKITBL.INC".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *    
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK2007.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      * 
      ******************************************************************
      *    サブプロ用領域
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
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           MOVE    ZERO     TO  FLG-RED
           MOVE    ZERO     TO  FLG-NYUIN
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
      *---- kokokara nyuinn ----------
      *
           PERFORM 100-INIT-SEC
      *
           MOVE    1     TO  FLG-RED
           MOVE    1     TO  FLG-NYUIN
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
                                       SPA-AREA
           INITIALIZE                  RECEERR
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
                                               WRK-PARA-SYOKBN1
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "SOKATU3005"      TO  JOB-PGID
           MOVE    "広域連合・診療報酬請求書"
                                   TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECEUPD"       TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  RECEUPD-HOSPNUM
      *
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  WRK-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *     
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)
                                   TO  WRK-SRYYMWH 
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1     =   ZERO 
               MOVE    LNK-DAY2-EDTYMD3
                                       TO  WRK-SEIYMDWH
           END-IF
      *
      *    医療機関ＩＤ編集
           INITIALIZE              SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD
                                   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME
                                           =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    医療機関情報−所在地
           INITIALIZE              SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD
                                   TO  SYS-1002-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM 
                                   TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
               IF      SYS-1002-ADRS   =   LOW-VALUE
                   MOVE    SPACE           TO  SYS-1002-ADRS
               END-IF    
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF 
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
      *    電話番号
           MOVE    SYS-1002-TEL        TO  WRK-TEL
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
           PERFORM 900-ORCDBMAIN-SEC
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
               MOVE    SYS-2007-MEISYO (1) TO  WRK-2007-MEISYO   (IDX)
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      SYS-2007-KOHNUM (IDY)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM (IDY)
                                           TO WRK-2007-KOHNUM(IDX IDZ)
               END-PERFORM
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       20
                       OR      SYS-2007-KOHNUM1(IDY)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM1(IDY)
                                           TO  WRK-2007-KOHNUM(IDX IDZ)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  WRK-2007-MAX    
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
           DISPLAY "WRK-PARA-HKNJANUM      =" WRK-PARA-HKNJANUM
           DISPLAY "SYS-200501-KOUIKIPRTKBN=" SYS-200501-KOUIKIPRTKBN
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
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
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO 
               IF      CNT-UPDFILE    >   ZERO
                   PERFORM 310-SEIKYU-UPD-SEC
               END-IF
           END-IF    
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYOKBN-SYUKEI-SEC         SECTION.
      *
      *    入院外（その他のレセ）・入院集計
      *    
           MOVE    LOW-VALUE           TO  KEY-AREA
           MOVE    1                   TO  WRK-INDEX
      *     
           EVALUATE    WRK-SYOKBN      ALSO    WRK-PARA-SYOKBN1
            WHEN    "1"             ALSO    "1"
                MOVE    "key62"         TO  WRK-MCP-PATHNAME
            WHEN    "2"             ALSO    "1"
                MOVE    "key61"         TO  WRK-MCP-PATHNAME
            WHEN    "1"             ALSO    "2"
                MOVE    "key66"         TO  WRK-MCP-PATHNAME
            WHEN    "2"             ALSO    "2"
                MOVE    "key65"         TO  WRK-MCP-PATHNAME
           END-EVALUATE  
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM 200-HENSYU-SEC  UNTIL   FLG-RECE10   =   1 
                                   OR      FLG-ERR      =   1
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院外集計（在総診または在医総を算定したレセ）
      *    特別療養費の場合は在総診はなし
           IF    ( FLG-ERR             =   ZERO )
           AND   ( WRK-PARA-SYOKBN1    =   "1"  )
               MOVE    LOW-VALUE           TO  KEY-AREA
               MOVE    2                   TO  WRK-INDEX
      *         
               EVALUATE    WRK-SYOKBN
                   WHEN    "1"
                       MOVE    "key64"     TO  WRK-MCP-PATHNAME
                   WHEN    "2"
                       MOVE    "key63"     TO  WRK-MCP-PATHNAME
               END-EVALUATE  
               MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
               PERFORM 900-SEIKYU-SELECT-SEC
      *
               PERFORM 200-HENSYU-SEC  UNTIL   FLG-RECE10   =  1
                                       OR      FLG-ERR      =  1
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *     
           MOVE    1                   TO  FLG-END
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
      *
      *    広域連合別集計 
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-END     =   1
                               OR      KEY-N-HKNJANUM1
                                               NOT =   KEY-O-HKNJANUM1
               INITIALIZE                  SUM-TABLE-OCC (2)
      *
      *        保険者番号別集計 
               MOVE    KEY-NEW             TO  KEY-OLD      
               PERFORM             UNTIL   FLG-END     =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *         
                   MOVE    1             TO  IDY5
                   PERFORM 2001-SYUKEI-SEC
      *
                   IF      KEY-O-HKNJANUM2   =   "0000"
                       CONTINUE
                   ELSE
                       MOVE    2             TO  IDY5
                       PERFORM 2001-SYUKEI-SEC
                   END-IF
      *
                   PERFORM 20013-UPD-FILE-HENSYU-SEC
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-SEIKYU-READ-SEC
               END-PERFORM
      *
      *        保険者番号別印刷
               IF      KEY-O-HKNJANUM2   =   "0000"
                   CONTINUE
               ELSE
                   IF    ( FLG-UPD        =   1    )
                   AND   ( FLG-ERR        =   ZERO )
                       MOVE    2             TO  IDY5
                       PERFORM 2002-PRINT-HENSYU-SEC
                  END-IF
              END-IF
           END-PERFORM
      *
      *    広域連合別印刷 
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO 
               MOVE    1             TO  IDY5
               PERFORM 2002-PRINT-HENSYU-SEC
           END-IF
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者別集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      * 
           MOVE    ZERO                TO  IDX3
                                           IDX
                                           IDXX
                                           FLG-UPD
      *
      *    主科別
           IF      RECE10-RECEKA       =   "00"
               MOVE    RECE10-RECEKA   TO  SUM-TBL-SRYKA (IDY5 1)
               MOVE    1                   TO  IDXX
           ELSE
               PERFORM VARYING IDXY    FROM    2   BY  1
                       UNTIL   IDXY    >       99
                   IF      SUM-TBL-SRYKA (IDY5 IDXY)    =   SPACE
                       MOVE    RECE10-RECEKA   TO
                                               SUM-TBL-SRYKA (IDY5 IDXY)
                   END-IF 
                   IF      SUM-TBL-SRYKA (IDY5 IDXY)
                                               =   RECE10-RECEKA
                       MOVE    IDXY            TO  IDXX
                       MOVE    99              TO  IDXY
                   END-IF
               END-PERFORM
           END-IF        
      *
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    1                   TO  IDX3
               WHEN    "2"
                   MOVE    2                   TO  IDX3
           END-EVALUATE           
      *
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    1                   TO  IDX
               WHEN    7
                   MOVE    2                   TO  IDX
           END-EVALUATE        
      *
           IF      IDXX            >   ZERO
               MOVE    IDXX            TO  IDY4
               PERFORM 20011-HKN-SYUKEI-SEC
               MOVE    100             TO  IDY4
               PERFORM 20011-HKN-SYUKEI-SEC
           END-IF
      *
      *    公費負担分     
           IF      RECE10-KOHNUM (1)   NOT =   SPACE
               IF      IDXX            >   ZERO
                   MOVE    IDXX            TO  IDY4
                   PERFORM 20012-KOH-SYUKEI-SEC
                   MOVE    100             TO  IDY4
                   PERFORM 20012-KOH-SYUKEI-SEC
               END-IF
           END-IF
      *
      *    集計の対象にならなかったとき                
           IF      FLG-UPD             =   1
               CONTINUE
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
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       20011-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX             =   ZERO
           OR      IDX3            =   ZERO 
               GO  TO  20011-HKN-SYUKEI-EXT
           END-IF
      *
           MOVE    1                         TO  IDX1
           MOVE    IDX                       TO  IDX2
      *    
      *    件数
           ADD     1               TO  SUM-KENSU (IDY5 IDY4 IDX2 IDX3)
      *
      *    療養の給付 
           ADD     RECE10-JNISSU   (IDX1) TO 
                                   SUM-NISSU      (IDY5 IDY4 IDX2 IDX3)
           ADD     RECE10-TOTALTEN (IDX1) TO
                                   SUM-TOTALTEN   (IDY5 IDY4 IDX2 IDX3)
           ADD     RECE10-FTNMONEY (IDX1) TO
                                   SUM-FTNMONEY   (IDY5 IDY4 IDX2 IDX3)
      *    食事療養
           IF      IDX3                   =   1 
              IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
                  ADD     1              TO
                                  SUM-SHOKUJIKENSU    (IDY5 IDY4 IDX2)
                  ADD     RECE10-SHOKUJINISSU   (IDX1)
                                         TO
                                  SUM-SHOKUJINISSU    (IDY5 IDY4 IDX2)
               END-IF
               ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                                         TO
                                  SUM-SHOKUJIRYOYOHI  (IDY5 IDY4 IDX2)
               ADD     RECE10-SHOKUJIFTN     (IDX1)
                                         TO
                                  SUM-SHOKUJIFTN      (IDY5 IDY4 IDX2)
           END-IF    
      *
           MOVE    1               TO  FLG-UPD 
           .
       20011-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       20012-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
                   OR  RECE10-KOHNUM (IDZ) =   SPACE
      *
      *        特定疾患無しのとき 
               IF      RECE10-KOHNUM (IDZ) =   "091"
                   MOVE    "051"               TO  RECE10-KOHNUM (IDZ)
               END-IF
      *
      *        公費番号まとめ情報があるとき
               IF      WRK-2007-MAX        >   ZERO
                   MOVE    SPACE               TO  WRK-MEISYO
                   PERFORM VARYING IDZ1    FROM    1   BY  1
                           UNTIL   IDZ1    >       WRK-2007-MAX
                       PERFORM VARYING IDZ2    FROM    1   BY  1
                               UNTIL   IDZ2    >       20
                               OR      WRK-2007-KOHNUM (IDZ1 IDZ2)
                                               =   SPACE
                           IF      RECE10-KOHNUM (IDZ)
                                       =   WRK-2007-KOHNUM (IDZ1 IDZ2)
                               MOVE    WRK-2007-KOHNUM (IDZ1 1)
                                               TO  RECE10-KOHNUM (IDZ)
                               MOVE    WRK-2007-MEISYO (IDZ1)
                                               TO  WRK-MEISYO
                               MOVE    20      TO  IDZ2
                               MOVE    WRK-2007-MAX
                                               TO  IDZ1
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2 >          100
                   IF      SUM-KOH-KOHNUM (IDY5 IDY4 IDX2) =   SPACE
                       MOVE    RECE10-KOHNUM (IDZ)     TO 
                                       SUM-KOH-KOHNUM (IDY5 IDY4 IDX2)
                       MOVE    WRK-MEISYO              TO 
                                       SUM-KOH-MEISYO (IDY5 IDY4 IDX2)
                   END-IF                                
                   IF      RECE10-KOHNUM (IDZ) 
                                   =   SUM-KOH-KOHNUM (IDY5 IDY4 IDX2)
                       PERFORM 200121-KOH-SYUKEI-01-SEC
                       MOVE    100             TO  IDX2
                   END-IF
               END-PERFORM
           END-PERFORM                 
           .
       20012-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理
      *****************************************************************
       200121-KOH-SYUKEI-01-SEC        SECTION.
      *
           COMPUTE IDY     =   IDZ     +   1
      *    
      *    件数
           ADD     1           TO  SUM-KOH-KENSU (IDY5 IDY4 IDX2 IDX3)
      *
      *    療養の給付 
           ADD     RECE10-JNISSU   (IDY) TO 
                                   SUM-KOH-NISSU   (IDY5 IDY4 IDX2 IDX3)
           ADD     RECE10-TOTALTEN (IDY) TO
                                   SUM-KOH-TOTALTEN(IDY5 IDY4 IDX2 IDX3)
           ADD     RECE10-FTNMONEY (IDY) TO
                                   SUM-KOH-FTNMONEY(IDY5 IDY4 IDX2 IDX3)
      *    食事療養
           IF      IDX3                   =   1 
              IF      RECE10-SHOKUJINISSU(IDY)   >   ZERO
                  ADD     1              TO
                                  SUM-KOH-SHOKUJIKENSU  (IDY5 IDY4 IDX2)
                  ADD     RECE10-SHOKUJINISSU   (IDY)
                                         TO
                                  SUM-KOH-SHOKUJINISSU  (IDY5 IDY4 IDX2)
               END-IF
               ADD     RECE10-SHOKUJIRYOYOHI (IDY)
                                         TO
                                  SUM-KOH-SHOKUJIRYOYOHI(IDY5 IDY4 IDX2)
               ADD     RECE10-SHOKUJIFTN     (IDY)
                                         TO
                                  SUM-KOH-SHOKUJIFTN    (IDY5 IDY4 IDX2)
           END-IF    
      *
           MOVE    1               TO  FLG-UPD 
           .
       200121-KOH-SYUKEI-01-EXT.
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
       2002-PRINT-HENSYU-SEC                SECTION.
      *
           IF      SUM-TBL-SRYKA (IDY5 2)  =   SPACE
               MOVE    100                 TO  IDXX
           ELSE
               MOVE    1                   TO  IDXX
           END-IF
      *       
           PERFORM VARYING IDX3    FROM   IDXX    BY  1
                   UNTIL   IDX3    >      100
               IF    ( SUM-TBL-SRYKA (IDY5 IDX3)
                                              =   SPACE )
               AND   ( IDX3                   <   100   )
                   CONTINUE
               ELSE
      *            主科名称
                   IF      SUM-TBL-SRYKA (IDY5 IDX3)   =   "00"
                       MOVE    "主科未設定"       TO  SYS-1005-SRYKANAME
                   ELSE
                       INITIALIZE                  SYS-1005-REC
                       MOVE    "1005"          TO  SYS-1005-KANRICD
                       MOVE    SUM-TBL-SRYKA (IDY5 IDX3)
                                               TO  SYS-1005-KBNCD
                       MOVE    LNK-PRTKANRI-SRYYM
                                               TO SYS-1005-STYUKYMD(1:6)
                       MOVE    "01"            TO SYS-1005-STYUKYMD(7:2)
                       MOVE    SYS-1005-STYUKYMD
                                               TO  SYS-1005-EDYUKYMD 
                       MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
                       MOVE    SYS-1005-REC    TO  MCPDATA-REC
                       PERFORM 910-SYSKANRI-KEY10-SEC
                       IF      FLG-SYSKANRI    =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1005-REC
                       ELSE
                           INITIALIZE                  SYS-1005-REC
                       END-IF
                   END-IF
      *
      *            帳票編集<見出し>処理
                   PERFORM 20021-HEAD-HEN-SEC
      *            帳票編集<明細>処理
                   PERFORM 20023-BODY-HEN-SEC
      *            帳票印刷処理
                   PERFORM 20024-PRINT-OUT-SEC
               END-IF   
           END-PERFORM    
      *
           .
       2002-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      SKT3005      
      *
           MOVE    WRK-SEIYMDWH        TO  SKT3005-SEIYMD
      *
           MOVE    WRK-SRYYMWH         TO  SKT3005-PRTYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SKT3005-ADRS
           INSPECT SKT3005-ADRS        REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SKT3005-HOSPNAME
           INSPECT SKT3005-HOSPNAME    REPLACING ALL "  " BY "　"
      *    電話番号
           MOVE    WRK-TEL             TO  SKT3005-TEL
      *    
           MOVE    WRK-KAISETUNAME     TO  SKT3005-KAISETUNAME
      *    
           MOVE    SYS-1001-HOSPCD    TO  SKT3005-HOSPCD
      *
           EVALUATE    IDY5    
               WHEN    1
                   IF      KEY-O-HKNJANUM      =   "00000000"
                       MOVE    KEY-O-HKNJANUM      TO  SKT3005-HKNJANUM
                       MOVE    "保険者番号入力なし"
                                                   TO  SKT3005-HKNJANAME
                   ELSE
                       MOVE    KEY-O-HKNJANUM1     TO  SKT3005-HKNJANUM
                       PERFORM VARYING IDZZ    FROM    1   BY  1
                               UNTIL   IDZZ    >       CONST-KOUIKI-MAX
                           IF      KEY-O-HKNJANUM1     =
                                           CONST-KOUIKI-NUM (IDZZ) (1:4)
                               MOVE    CONST-KOUIKI-NUM  (IDZZ)
                                                   TO  SKT3005-HKNJANUM
                               STRING  CONST-KOUIKI-NAME (IDZZ)
                                                   DELIMITED  BY  SPACE
                                   "　殿"          DELIMITED  BY  SIZE
                                              INTO  SKT3005-HKNJANAME
                               END-STRING
                               MOVE    CONST-KOUIKI-MAX
                                                   TO  IDZZ
                           END-IF    
                       END-PERFORM
                   END-IF    
               WHEN    2
                   MOVE    SPACE               TO  HKNJAINF-REC
                   INITIALIZE                      HKNJAINF-REC
                   MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                   MOVE    KEY-O-HKNJANUM      TO  HKNJA-HKNJANUM
                   MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                   PERFORM 900-HKNJAINF-INV-SEC
                   IF      FLG-HKNJAINF        =   ZERO
                       STRING  HKNJA-HKNJANAME DELIMITED  BY  SPACE
                           "　殿"              DELIMITED  BY  SIZE
                                           INTO  SKT3005-HKNJANAME
                       END-STRING
                   END-IF                                
                   MOVE    KEY-O-HKNJANUM      TO  SKT3005-HKNJANUM
           END-EVALUATE
      *
           EVALUATE    WRK-INDEX
               WHEN    1
                   MOVE    "その他のレセ"      TO  SKT3005-TITLE  
               WHEN    2
                   MOVE    "在医総管または在医総を算定したレセ"
                                               TO  SKT3005-TITLE
           END-EVALUATE           
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"  TO  SKT3005-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-SYOKBN1
               WHEN    "2"
                   MOVE    "特別療養費"  TO  SKT3005-MOJI
           END-EVALUATE           
      *
           MOVE    SYS-1005-SRYKANAME    TO  SKT3005-SRYKA
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
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               ADD     1                   TO  IDZ    
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       2
                   MOVE    SUM-KENSU   (IDY5 IDX3 IDX IDY)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  
                                           SKT3005-HKN-KENSU (IDZ IDY)
                   MOVE    SUM-NISSU   (IDY5 IDX3 IDX IDY)
                                           TO  WRK-NISSUZ 
                   MOVE    WRK-NISSUZ      TO  
                                           SKT3005-HKN-NISSU (IDZ IDY) 
                   MOVE    SUM-TOTALTEN(IDY5 IDX3 IDX IDY)
                                           TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ      TO  
                                           SKT3005-HKN-TENSU (IDZ IDY)
                   MOVE    SUM-FTNMONEY    (IDY5 IDX3 IDX IDY)
                                           TO WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ   TO 
                                           SKT3005-HKN-ITBFTN(IDZ IDY)
               END-PERFORM     
               MOVE    SUM-SHOKUJIKENSU(IDY5 IDX3 IDX)
                                           TO WRK-KENSUZ
               MOVE    WRK-KENSUZ          TO SKT3005-HKN-SYOKENSU (IDZ)
               MOVE    SUM-SHOKUJINISSU(IDY5 IDX3 IDX)
                                           TO WRK-NISSUZ
               MOVE    WRK-NISSUZ          TO SKT3005-HKN-SYONISSU (IDZ)
               MOVE    SUM-SHOKUJIRYOYOHI(IDY5 IDX3 IDX)
                                           TO WRK-KINGAKZ
               MOVE    WRK-KINGAKZ         TO SKT3005-HKN-SYOKINGAK(IDZ)
               MOVE    SUM-SHOKUJIFTN   (IDY5 IDX3 IDX)
                                           TO WRK-FTNGAKZ
               MOVE    WRK-FTNGAKZ         TO SKT3005-HKN-SYOFTNGAK(IDZ)
           END-PERFORM
      *     
      *    公費負担分編集処理
           MOVE    ZERO                TO  FLG-KOH-END
           MOVE    1                   TO  IDX2
      * 
           PERFORM 200231-KOH-HENSYU-SEC
                                   UNTIL   FLG-KOH-END   =   1
      *
           .
       20023-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<公費負担分>処理
      *****************************************************************
       200231-KOH-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       12         
                   OR      FLG-KOH-END     =   1
               IF      SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)   =   SPACE
                   MOVE    1                   TO  FLG-KOH-END
               ELSE    
                   PERFORM 2002311-HKNNUM-HENSYU-SEC     
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       2
                       MOVE    SUM-KOH-KENSU   (IDY5 IDX3 IDX2 IDZ)
                                           TO  WRK-KENSUZ
                       MOVE    WRK-KENSUZ  TO  
                                           SKT3005-KOH-KENSU (IDY IDZ)
                       MOVE    SUM-KOH-NISSU   (IDY5 IDX3 IDX2 IDZ)
                                           TO  WRK-NISSUZ
                       MOVE    WRK-NISSUZ  TO  
                                           SKT3005-KOH-NISSU (IDY IDZ) 
                       MOVE    SUM-KOH-TOTALTEN(IDY5 IDX3 IDX2 IDZ)
                                           TO  WRK-TENSUZ
                       MOVE    WRK-TENSUZ  TO  
                                           SKT3005-KOH-TENSU (IDY IDZ)
                       MOVE    SUM-KOH-FTNMONEY(IDY5 IDX3 IDX2 IDZ)
                                           TO  WRK-FTNMONEYZ
                       MOVE    WRK-FTNMONEYZ
                                           TO  
                                           SKT3005-KOH-ITBFTN(IDY IDZ)
                   END-PERFORM     
                   MOVE    SUM-KOH-SHOKUJIKENSU (IDY5 IDX3 IDX2)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  
                                           SKT3005-KOH-SYOKENSU (IDY)
                   MOVE    SUM-KOH-SHOKUJINISSU (IDY5 IDX3 IDX2)
                                           TO  WRK-NISSUZ
                   MOVE    WRK-NISSUZ      TO  
                                           SKT3005-KOH-SYONISSU (IDY)
                   MOVE    SUM-KOH-SHOKUJIRYOYOHI (IDY5 IDX3 IDX2)
                                           TO  WRK-KINGAKZ
                   MOVE    WRK-KINGAKZ     TO  
                                           SKT3005-KOH-SYOKINGAK(IDY)
                   MOVE    SUM-KOH-SHOKUJIFTN    (IDY5 IDX3 IDX2)
                                           TO  WRK-FTNGAKZ
                   MOVE    WRK-FTNGAKZ     TO  
                                           SKT3005-KOH-SYOFTNGAK(IDY)
      *             
                   ADD     1               TO  IDX2   
               END-IF        
           END-PERFORM
      *
           IF      SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)  =   SPACE
               MOVE    1                   TO  FLG-KOH-END
           ELSE    
      *        帳票印刷処理（２頁以降）
               PERFORM 20024-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 20021-HEAD-HEN-SEC
           END-IF     
      *
           .
       200231-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       2002311-HKNNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHKBN
      *
           IF      SUM-KOH-KOHNUM (IDY5 IDX3 IDX2) (1:1)
                                       =   "0"
               MOVE    SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)
                                           TO  WRK-KOHNUM9
           ELSE
      *        法別番号取得（地方公費のみ） 
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)
                                           TO  HKN-HKNNUM
               MOVE    WRK-SRYYMD          TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               PERFORM 900-HKNNUM-INV-SEC
               IF      FLG-HKNNUM          =   ZERO
                   MOVE   HKN-HBTNUM           TO  WRK-KOHNUM9
               ELSE
                   MOVE    SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)
                                               TO  WRK-KOHNUM9
               END-IF
           END-IF
           MOVE    WRK-KOHNUM9         TO  SKT3005-KOH-NUM (IDY)
      *
      *    特定疾患のとき 
           IF      SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)  =   "051"
               MOVE    "特定疾患"          TO  SKT3005-KOH-NAME(IDY)
           ELSE
      *        公費番号まとめ情報からの集計か
               IF      SUM-KOH-MEISYO  (IDY5 IDX3 IDX2)
                                           =   SPACE
      *            保険番号読込み  
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE   SUM-KOH-KOHNUM (IDY5 IDX3 IDX2)
                                               TO    HKN-HKNNUM
                   MOVE   HKNNUM-REC           TO    MCPDATA-REC
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-HKNNUM-READ-SEC
                   ELSE
                       MOVE    1                  TO  FLG-HKNNUM
                   END-IF
      *
                   PERFORM UNTIL       FLG-HKNNUM      =   1
                     IF      HKN-TEKSTYMD            <=  WRK-SRYYMD
                     AND     HKN-TEKEDYMD            >=  WRK-SRYYMD
                         MOVE    HKN-TANSEIDONAME    TO  WRK-HKNNAME
                         MOVE    1                   TO  FLG-HKNNUM   
                     END-IF
      *
                     IF      FLG-HKNNUM           =   ZERO
                         MOVE    "tbl_hknnum"        TO  MCP-TABLE
                         MOVE    "key2"              TO  MCP-PATHNAME
                         PERFORM 900-HKNNUM-READ-SEC
                     END-IF
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   INSPECT WRK-HKNNAME  REPLACING   ALL "（" BY  "・"
                                                    ALL "）" BY  "　"  
                   MOVE    WRK-HKNNAME         TO  SKT3005-KOH-NAME(IDY)
               ELSE
                   MOVE    SUM-KOH-MEISYO (IDY5 IDX3 IDX2)
                                               TO  SKT3005-KOH-NAME(IDY)
               END-IF
           END-IF
           . 
       2002311-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20024-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
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
           MOVE    WRK-PAGE            TO  SKT3005-PAGE
           IF  FLG-RED                 =  1 
               MOVE    "SKT3006.red"   TO  SPRT-PRTID
           ELSE
               MOVE    "SKT3005.red"   TO  SPRT-PRTID
           END-IF
           MOVE    SKT3005               TO  SPRT-PRTDATA
           EVALUATE    WRK-INDEX    ALSO    WRK-SYOKBN
                                            ALSO    WRK-PARA-SYOKBN1
               WHEN    1            ALSO    "1"     
                                            ALSO   "1"
                   MOVE    "広域連合請求書"        TO  SPRT-TITLE
               WHEN    2            ALSO    "1"
                                            ALSO   "1"
                   MOVE    "広域連合請求書（在医総等）"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "広域連合請求書［返戻分］" 
                                               TO  SPRT-TITLE
               WHEN    2            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "広域連合請求書（在医総等）［返戻分］"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "1"     
                                            ALSO   "2"
                   MOVE    "広域連合請求書［特別療養費］"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "2"
                   MOVE    "広域連合請求書［返戻分］［特別療養費］" 
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
       20024-PRINT-OUT-EXT.
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
           STRING  "SOKATU3005 "         DELIMITED  BY  SIZE
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
           DISPLAY "*** SOKATU3005 IN   "  CNT-RECE10 
           DISPLAY "***          UPD  "  CNT-UPDFILE 
           DISPLAY "***          PAGE "  CNT-PAGE 
           DISPLAY "*** SOKATU3005 END ***"
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
           IF      WRK-PARA-SYOKBN1    =   "1"
               MOVE    "039"               TO  RECE10-HKNNUM-S
                                               RECE10-HKNNUM
           ELSE 
               MOVE    "040"               TO  RECE10-HKNNUM-S
                                               RECE10-HKNNUM
           END-IF
      *
      *###
           DISPLAY "SYOKBN=" WRK-SYOKBN " SYOKBN1=" WRK-PARA-SYOKBN1
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
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
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
      *---- KUSUMOTO START -----
               IF  (FLG-NYUIN          =   ZERO   AND
                    RECE10-NYUGAIKBN   =   2)     OR 
                   (FLG-NYUIN          =   1      AND
                    RECE10-NYUGAIKBN   =   1) 
                      CONTINUE
               ELSE
                      GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *---- KUSUMOTO END ----
      *         
               MOVE    RECE10-HKNJANUM-S   TO  KEY-N-HKNJANUM
               EVALUATE    SYS-200501-KOUIKIPRTKBN
                   WHEN    "0"
                       MOVE    "0000"          TO  KEY-N-HKNJANUM2
                   WHEN    "2"
                       IF      RECE10-PREFKBN  =   "2"
                           MOVE    "0000"          TO  KEY-N-HKNJANUM2
                       END-IF
                   WHEN    "3"
                       IF      RECE10-PREFKBN  =   "1"
                           MOVE    "0000"          TO  KEY-N-HKNJANUM2
                       END-IF
               END-EVALUATE
               ADD     1                   TO  CNT-RECE10   
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM
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
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
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
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
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
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-TEL
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
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
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
      *    保険番号マスタ読込
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNNUM
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNNUM-INV-EXT.
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
