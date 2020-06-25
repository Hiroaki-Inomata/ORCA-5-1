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
       PROGRAM-ID.             SOKATU3600.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・総括票集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/08/23    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    06/11/02    公費欄"145"と"245"は１つにまとめる
      *  01.00.02    内田    06/11/05    結核精神の指定位置の削除
      *  01.00.03    内田    06/11/29    県内県外とも同一帳票を使用する
      *  01.00.04    内田    07/06/17    在総診は合算して集計
      *  01.00.05    内田    07/09/24    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  返戻対応
      *  01.00.06    内田K   08/04/22    帳票様式の変更に伴う修正
      *                                  SPA-AREA初期化
      *  01.00.07    内田    08/05/11    県外県内の印字追加
      *  01.00.08    内田    08/07/04    後期高齢のみの場合でも印刷するよう修正
      *                                  シス管「2007」の公費まとめ対応
      *                                  保険番号748は公費再掲欄に印字しない
      *  01.00.09    吉川    09/06/08    点数、金額、食事回数のバイト数増加
      *  01.00.10    吉川    09/12/25    シス管「2007」のまとめ公費で
      *                                  6つ目以降がまとまらない不具合を修正
      *  01.00.11    吉川    10/05/13    オンライン返戻対応
      *  01.00.12    吉川    11/01/11    特別療養費国保対応
      *  01.00.13    吉川    13/02/06    医療機関情報編集対応
      *  01.00.14    吉川    14/05/14    前期高齢者２割対応
      *  01.00.15    吉川    14/10/27    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.16    吉川    15/02/12    返戻時、再印刷画面のタイトル修正
      *  01.00.17    萩野    16/03/11    医療機関編集情報への開設者名追加対応
      *  01.00.18    萩野    18/01/11    公費番号まとめ情報の公費番号の追加
      *                                  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.19    萩野    19/01/11    修正履歴コメント削除
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求書一時ファイル  
           SELECT  SKT3600TMP-FILE ASSIGN    SKT3600TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT3600TMP-KEY
                                   FILE    STATUS  IS  STS-SKT3600.

      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル  
       FD  SKT3600TMP-FILE.
           COPY    "CPSKT3600TMP.INC".
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求書一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT3600TP//.
006900*    請求ＤＢ更新用ファイル 名称領域 
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
           COPY    "SKT3600.INC".
           COPY    "SKT3605.INC".
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT3600         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-SKT3600TMP      PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-TOKUBETU   PIC X(01).
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
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-KOHNAME         PIC X(16).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-MAE-INPUT              PIC X(200).
           03  WRK-OUT-INPUT              PIC X(200).
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KOH             PIC 9(02).
      *
       01  SORT-AREA.
           03  SORT-TBL.
               05  SORT-KOH-TBL  OCCURS  20.
                   07  SORT-KOHNUM              PIC  X(03).
                   07  SORT-KOHNAME             PIC  X(16).
                   07  SORT-KOH-NYUGAI-TBL OCCURS 2.
                       09  SORT-K-KENSU         PIC  9(05).
                       09  SORT-K-NISSU         PIC  9(05).
                       09  SORT-K-TENSU         PIC  9(09).
                       09  SORT-K-ITBFTN        PIC  9(09).
                   07  SORT-K-SKENSU            PIC  9(05).
                   07  SORT-K-SNISSU            PIC  9(05).
                   07  SORT-K-SKINGK            PIC  9(09).
                   07  SORT-K-SFTNGK            PIC  9(09).
      *
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX        PIC 9(03).
      *
           COPY    "COMMON-SPA".
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
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
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
      *    請求管理情報
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
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
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
                                               WRK-PARA-TOKUBETU
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
           MOVE    "SOKATU3600"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT3600"           TO  SKT3600TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT3600TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT3600TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT3600TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT3600TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
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
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
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
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE   TO  SYS-1001-HOSPNAME
               END-IF
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE   TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1           TO  FLG-END
           END-IF

      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
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
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
               IF      SYS-1002-ADRS   =   LOW-VALUE
                       MOVE    SPACE   TO    SYS-1002-ADRS
               END-IF
           ELSE
               MOVE "医療機関情報（所在地）が取得できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1           TO  FLG-END
           END-IF 
      *
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
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX1
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX1           >=  100
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX1
               MOVE    SYS-2007-MEISYO(1) TO  WRK-2007-MEISYO(IDX1)
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       10
                       OR      SYS-2007-KOHNUM (IDX2)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM (IDX2)
                                           TO WRK-2007-KOHNUM(IDX1 IDZ)
               END-PERFORM
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       20
                       OR      SYS-2007-KOHNUM1(IDX2)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM1(IDX2)
                                           TO  WRK-2007-KOHNUM(IDX1 IDZ)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    IDX1                    TO  WRK-2007-MAX
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
           OPEN    OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
           END-EVALUATE
      *
           PERFORM 900-SEIKYU-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT  SKT3600TMP-FILE
           CLOSE           SKT3600TMP-FILE
           OPEN    I-O     SKT3600TMP-FILE
      *
           MOVE    ZERO    TO      FLG-UPD
      *
           PERFORM 2001-KOKUHO-SYUKEI-SEC
                   UNTIL   FLG-END =   1
           PERFORM  900-SEIKYU-CLOSE-SEC
           IF  WRK-PARA-TOKUBETU  =  "1"
             MOVE     ZERO    TO        FLG-END
             MOVE     1       TO        FLG-KOUKI
             MOVE    "039"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           ELSE
             MOVE     1       TO        FLG-KOUKI
             MOVE     ZERO    TO        FLG-END
             MOVE    "040"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           END-IF
      *
           CLOSE   UPD-FILE
           CLOSE   SKT3600TMP-FILE
      *
           IF      FLG-UPD     =   1
               PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了    処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      CNT-RECE10          =   ZERO 
               CONTINUE
           ELSE
               IF      FLG-UPD     =   1
                   PERFORM     310-SEIKYU-UPD-SEC
               END-IF
           END-IF
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU3600 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU3600 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA

           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC              SECTION.
      * 
           INITIALIZE  SKT3600TMP-REC
      *
      *    県内・県外
           MOVE    RECE10-PREFKBN      TO  SKT3600TMP-PREFKBN
      *
           PERFORM 900-SKTTMP-READ-SEC
      *
           IF  FLG-KOUKI  =  ZERO
               DISPLAY "210-HKN-SYUKEI-SEC"
               PERFORM  210-HKN-SYUKEI-SEC
           ELSE
               DISPLAY "220-KOUKI-SYUKEI-SEC"
               PERFORM  220-KOUKI-SYUKEI-SEC
           END-IF
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               IF  FLG-KOUKI  =  ZERO
                  PERFORM   900-SEIKYU-READ-SEC
               ELSE
                  PERFORM   950-SEIKYU-READ-SEC
               END-IF
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
               MOVE    1                   TO  FLG-END
           END-IF 
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    老人かどうか
           IF  RECE10-RJNHKNNUM  NOT =  SPACE
               IF      RECE10-KYURATE  =   9
                   MOVE    1279      TO  IDX2
               ELSE
                   MOVE    1277      TO  IDX2
               END-IF
           ELSE
      *    退職者かどうか
               IF      RECE10-HKNNUM   =   "060" OR "068"
                   IF      RECE10-AGE  >=  70
                     EVALUATE  RECE10-KYURATE
                       WHEN  9
                         MOVE    1     TO    IDX2
                       WHEN  8
                         IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                             MOVE    1    TO    IDX2
                         ELSE
                             MOVE    2    TO    IDX2
                         END-IF
                       WHEN  7
                         MOVE    2     TO    IDX2
                     END-EVALUATE
                   ELSE
                     IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                         AND  (RECE10-FLG (1)   =  1)
                           MOVE  4     TO  IDX2
                     ELSE
                         IF      RECE10-AGE  >=  3
                             MOVE    3       TO  IDX2
                         ELSE
                             MOVE    4       TO  IDX2
                         END-IF
                     END-IF
                   END-IF
      *
               ELSE
                   IF  RECE10-AGE  >=  70
                       AND  RECE10-SRYYM  <  WRK-CONS-SRYYM-200804  
                       IF      RECE10-KYURATE  =   9
                            MOVE   1699     TO  IDX2
                       ELSE
                            MOVE   1677     TO  IDX2
                       END-IF
                   ELSE
                       IF      RECE10-HONKZKKBN    =   "1"
                           MOVE    5       TO  IDX2
                       ELSE
                         IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                         AND  (RECE10-FLG (1)   =  1)
                               MOVE    7       TO  IDX2
                         ELSE
                           IF      RECE10-AGE  <   3
                               MOVE    7       TO  IDX2
                           ELSE
                               MOVE    6       TO  IDX2
                           END-IF
                         END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    入院・外来
           IF      RECE10-NYUGAIKBN    =   "1"
               MOVE  1  TO  IDX3
           ELSE
               MOVE  2  TO  IDX3
           END-IF
      *
           IF  IDX2  <  10
             PERFORM   2101-SYUKEI-SEC
           ELSE
             IF  IDX2  >  10 
               PERFORM   221-KOH-SEC
             END-IF
           END-IF
      *
      *    公費併用か
           IF      RECE10-KOHNUM (1) NOT =  SPACE  OR
                   RECE10-KOHNUM (2) NOT =  SPACE  OR
                   RECE10-KOHNUM (3) NOT =  SPACE  OR
                   RECE10-KOHNUM (4) NOT =  SPACE
                PERFORM  220-KOH-SYUKEI-SEC
            END-IF
      *
           IF      FLG-SKT3600TMP  =   1
               WRITE   SKT3600TMP-REC
           ELSE
               REWRITE SKT3600TMP-REC
           END-IF
      *
           MOVE    1       TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢者分集計処理
      *****************************************************************
       220-KOUKI-SYUKEI-SEC               SECTION.
      *
      *    入院・外来
           IF      RECE10-NYUGAIKBN    =   "1"
               MOVE  1  TO  IDX3
           ELSE
               MOVE  2  TO  IDX3
           END-IF
      *
      *    給付割合
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    8             TO  IDX2
               WHEN    7
                   MOVE    9             TO  IDX2
           END-EVALUATE
      *
           PERFORM 2101-SYUKEI-SEC
      *
      *    公費併用か
           IF      RECE10-KOHNUM (1) NOT =  SPACE  OR
                   RECE10-KOHNUM (2) NOT =  SPACE  OR
                   RECE10-KOHNUM (3) NOT =  SPACE  OR
                   RECE10-KOHNUM (4) NOT =  SPACE
                PERFORM  220-KOH-SYUKEI-SEC
           END-IF
      *
           IF      FLG-SKT3600TMP  =   1
               WRITE   SKT3600TMP-REC
           ELSE
               REWRITE SKT3600TMP-REC
           END-IF
      *
           MOVE    1       TO  FLG-UPD 
      *
           .
       220-KOUKI-SYUKEI-EXT.
           EXIT.       
      *
      *****************************************************************
      *    医療保険・国保分集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD     1                   TO  SKT3600TMP-KENSU(IDX2 IDX3)
      *
      *    療養の給付:診療実日数
           ADD     RECE10-JNISSU(1)    TO  SKT3600TMP-NISSU (IDX2 IDX3)
      *
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT3600TMP-TENSU (IDX2 IDX3)
      *
      *    療養の給付：一部負担金
           ADD     RECE10-FTNMONEY(1)  TO  SKT3600TMP-ITBFTN (IDX2 IDX3)
      *
      *    食事療養：件数
           IF      RECE10-SHOKUJINISSU(1)  >   0
               ADD     1               TO  SKT3600TMP-SKENSU (IDX2)
           END-IF
      *
      *    食事療養:日数
           ADD     RECE10-SHOKUJINISSU(1)
                                       TO  SKT3600TMP-SNISSU(IDX2)
      *
      *    食事療養：金額
           ADD     RECE10-SHOKUJIRYOYOHI(1)
                                       TO  SKT3600TMP-SKINGK(IDX2)
      *
      *    食事療養：標準負担額
           ADD     RECE10-SHOKUJIFTN(1)
                                       TO  SKT3600TMP-SFTNGK(IDX2)
      *
           MOVE    1       TO  FLG-UPD
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL   IDX4    >       4
                   OR      RECE10-KOHNUM(IDX4) =   SPACE
      *
      *        ０５１と０９１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX4) =   "091"
                   MOVE    "051"   TO  RECE10-KOHNUM(IDX4)
               END-IF
      *        "145"と"245"は同じ場所に集計する
               IF  RECE10-KOHNUM(IDX4)  =  "245"
                   MOVE  "145"     TO  RECE10-KOHNUM(IDX4)
               END-IF
      *
      *        公費判別
      *        保険番号７４８は集計しない
               IF  RECE10-KOHNUM (IDX4)  NOT =  "748"
      *            公費番号まとめ情報があるとき
                   IF  WRK-2007-MAX  >  ZERO
                     MOVE  SPACE     TO  WRK-KOHNAME
                     PERFORM VARYING IDZ1    FROM    1   BY  1
                             UNTIL   IDZ1    >       WRK-2007-MAX
                       PERFORM VARYING IDZ2  FROM    1   BY  1
                               UNTIL   IDZ2  >       20
                               OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                         IF  RECE10-KOHNUM(IDX4)  
                                       =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                           MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                       TO  RECE10-KOHNUM (IDX4)
                           MOVE  WRK-2007-MEISYO (IDZ1)
                                       TO  WRK-KOHNAME
                           MOVE  20            TO  IDZ2
                           MOVE  WRK-2007-MAX  TO  IDZ1
                         END-IF
                       END-PERFORM
                     END-PERFORM
                   END-IF
                   PERFORM VARYING IDX5    FROM    1   BY  1
                           UNTIL   IDX5    >       20
                           OR      SKT3600TMP-KOHNUM(IDX5) =   SPACE
                       IF      RECE10-KOHNUM (IDX4)
                                           =   SKT3600TMP-KOHNUM(IDX5)
                           MOVE    IDX5    TO  CNT-KOH
                           MOVE    20      TO  IDX5
                       END-IF
                   END-PERFORM
      *
      *            該当公費がなかった場合
                   IF      IDX5    <   20
                       MOVE    IDX5    TO  CNT-KOH
      *
      *              公費名
                       MOVE    RECE10-KOHNUM (IDX4)
                                       TO  SKT3600TMP-KOHNUM(CNT-KOH)
      *              公費名称
                       MOVE    WRK-KOHNAME
                                       TO  SKT3600TMP-KOHNAME(CNT-KOH)
                   END-IF
      *
      *            療養の給付：件数
                   ADD     1       TO  SKT3600TMP-K-KENSU (CNT-KOH IDX3)
      *
      *            療養の給付：診療実日数
                   ADD     RECE10-JNISSU(IDX4 + 1)    
                                   TO  SKT3600TMP-K-NISSU (CNT-KOH IDX3)
      *
      *            療養の給付：点数
                   ADD     RECE10-TOTALTEN(IDX4 + 1)
                                   TO  SKT3600TMP-K-TENSU (CNT-KOH IDX3)
      *
      *            療養の給付：一部負担金
                   ADD     RECE10-FTNMONEY(IDX4 + 1)
                                   TO  SKT3600TMP-K-ITBFTN(CNT-KOH IDX3)
      *
      *            食事療養：件数
                   IF      RECE10-SHOKUJINISSU(1)  >   0
                       ADD     1   TO  SKT3600TMP-K-SKENSU (CNT-KOH)
                   END-IF
      *
      *            食事療養:日数
                   ADD     RECE10-SHOKUJINISSU(IDX4 + 1)
                                   TO  SKT3600TMP-K-SNISSU(CNT-KOH)
      *
      *            食事療養：金額
                   ADD     RECE10-SHOKUJIRYOYOHI(IDX4 + 1)
                                   TO  SKT3600TMP-K-SKINGK(CNT-KOH)
      *
      *            食事療養：標準負担額
                   ADD     RECE10-SHOKUJIFTN(IDX4 + 1)
                                   TO  SKT3600TMP-K-SFTNGK(CNT-KOH)
               END-IF
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    月遅れ編集処理
      *****************************************************************
       221-KOH-SEC              SECTION.
      *
           DISPLAY "月遅れ221-KOH-SEC"IDX2
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL   IDX5    >       20
                   OR      SKT3600TMP-KOHNUM(IDX5) =   SPACE
                IF     IDX2(2:3)  =   SKT3600TMP-KOHNUM(IDX5)
                    MOVE    IDX5    TO  CNT-KOH
                    MOVE    20      TO  IDX5
                END-IF
           END-PERFORM
      *
           DISPLAY "IDX5"IDX5
      *    該当保険がなかった場合
           IF      IDX5    <   20
             MOVE    IDX5    TO  CNT-KOH
      *
      *    保険名
             DISPLAY "保険名"IDX2(2:3)
             MOVE    IDX2(2:3)
                             TO  SKT3600TMP-KOHNUM(CNT-KOH)
           END-IF
      *
      *    療養の給付：件数
           ADD     1       TO  SKT3600TMP-K-KENSU (CNT-KOH IDX3)
      *
      *    療養の給付：診療実日数
           ADD     RECE10-JNISSU(1)    
                           TO  SKT3600TMP-K-NISSU (CNT-KOH IDX3)
      *
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(1)
                           TO  SKT3600TMP-K-TENSU (CNT-KOH IDX3)
      *
      *    療養の給付：一部負担金
           ADD     RECE10-FTNMONEY(1)
                           TO  SKT3600TMP-K-ITBFTN(CNT-KOH IDX3)
      *
      *    食事療養：件数
           IF      RECE10-SHOKUJINISSU(1)  >   0
                ADD     1   TO  SKT3600TMP-K-SKENSU (CNT-KOH)
           END-IF
      *
      *    食事療養:日数
           ADD     RECE10-SHOKUJINISSU(1)
                            TO  SKT3600TMP-K-SNISSU(CNT-KOH)
      *
      *    食事療養：金額
           ADD     RECE10-SHOKUJIRYOYOHI(1)
                            TO  SKT3600TMP-K-SKINGK(CNT-KOH)
      *
      *    食事療養：標準負担額
           ADD     RECE10-SHOKUJIFTN(1)
                            TO  SKT3600TMP-K-SFTNGK(CNT-KOH)
      *
           .
       221-KOH-EXT.
           EXIT.
      *
      *****************************************************************
      *    結核精神負担分集計処理
      *****************************************************************
       220-KESE-SYUKEI-SEC        SECTION.
      *
           EVALUATE    RECE10-KOHNUM(IDX4)
               WHEN    "010"
                   MOVE    1       TO  IDX6
                   MOVE    IDX3    TO  IDX7
               WHEN    "011"
                   MOVE    2       TO  IDX6
                   MOVE    1       TO  IDX7
               WHEN    "021"
                   MOVE    2       TO  IDX6
                   MOVE    2       TO  IDX7
           END-EVALUATE
      *
      *    療養の給付：件数
           ADD     1           TO  SKT3600TMP-KESE-KENSU (IDX6 IDX7)
      *
      *    療養の給付：診療実日数
           ADD     RECE10-JNISSU(IDX4 + 1)    
                               TO  SKT3600TMP-KESE-NISSU (IDX6 IDX7)
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(IDX4 + 1)
                               TO  SKT3600TMP-KESE-TENSU (IDX6 IDX7)
      *
      *    療養の給付：一部負担金
           ADD     RECE10-FTNMONEY(IDX4 + 1)
                               TO  SKT3600TMP-KESE-ITBFTN (IDX6 IDX7)
      *    食事療養：件数
           IF      RECE10-SHOKUJINISSU(1)  >   0
               ADD     1       TO  SKT3600TMP-KESE-SKENSU (IDX6)
           END-IF
      *
      *    食事療養:日数
           ADD     RECE10-SHOKUJINISSU(IDX4 + 1)
                               TO  SKT3600TMP-KESE-SNISSU(IDX6)
      *
      *    食事療養：金額
           ADD     RECE10-SHOKUJIRYOYOHI(IDX4 + 1)
                               TO  SKT3600TMP-KESE-SKINGK(IDX6)
      *
      *    食事療養：標準負担額
           ADD     RECE10-SHOKUJIFTN(IDX4 + 1)
                               TO  SKT3600TMP-KESE-SFTNGK(IDX6)
      *
           .
       220-KESE-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
006900*    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN    INPUT   SKT3600TMP-FILE
      *
           MOVE    ZERO    TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
      *
           PERFORM UNTIL   FLG-PRINT   =   1
      *
      *        公費印字順編集処理
               PERFORM  KOH-SORT-SEC
      *
      *        公費が残っているか
               MOVE    ZERO        TO  FLG-PASS
               MOVE    1           TO  CNT-KOH
               PERFORM UNTIL   SKT3600TMP-KOHNUM(CNT-KOH)  =   SPACE
                       AND     FLG-PASS    =   1
      *
      *                帳票編集<見出し>処理
                       PERFORM 311-HEAD-HEN-SEC
      *
                       IF      FLG-PASS    =   ZERO
      *                    帳票編集<明細：保険>処理
                           PERFORM 321-BODY-HEN-SEC
                       END-IF
      *
      *                帳票編集<明細：公費>処理
                       PERFORM 3211-BODY-HEN-SEC
      *
      *                帳票印刷処理
                       PERFORM 331-PRINT-OUT-SEC
                   MOVE    1       TO  FLG-PASS
               END-PERFORM
               PERFORM 900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE   SKT3600TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理  (県内用)
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT3605
      *
           IF  SKT3600TMP-PREFKBN  =  1
               MOVE    "県内専用"          TO  SKT3605-PREF
           ELSE
               MOVE    "県外専用"          TO  SKT3605-PREF
           END-IF
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT3605-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT3605-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN        TO  SKT3605-HOSPCD
      *
      *    郵便番号
           MOVE    SYS-1002-POST (1:3)     TO  SKT3605-POST (1:)
           MOVE    "-"                     TO  SKT3605-POST (4:)
           MOVE    SYS-1002-POST (4:)      TO  SKT3605-POST (5:)
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT3605-ADRS
           INSPECT SKT3605-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT3605-HOSPNAME
           INSPECT SKT3605-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SKT3605-KAISETU
      *
      *    県内・県外
           IF      SKT3600TMP-PREFKBN      =   "1"
               MOVE    "県内専用"          TO  SKT3605-PREFKBN
           ELSE
               MOVE    "県外専用"          TO  SKT3605-PREFKBN
           END-IF
      *
      *    在宅
           IF      SKT3600TMP-ZAITAKU      =   "1"
               MOVE    "在"                TO  SKT3605-ZAI1
               MOVE    "◯"                TO  SKT3605-ZAI2
           END-IF
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"            TO  SKT3605-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理  (県内用)
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       11
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3600TMP-KENSU(IDX2 IDX3)     TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3605-KENSU(IDX2 IDX3)
      *
      *            療養の給付：日数
                   MOVE    SKT3600TMP-NISSU (IDX2 IDX3)    TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3605-NISSU (IDX2 IDX3)
      *
      *            療養の給付：点数
                   MOVE    SKT3600TMP-TENSU (IDX2 IDX3)    TO  WRK-Z10
                   MOVE    WRK-Z10     TO  SKT3605-TENSU (IDX2 IDX3)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3600TMP-ITBFTN (IDX2 IDX3)   TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3605-ITBFTN2 (IDX2 IDX3)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3600TMP-SKENSU (IDX2)    TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3605-SKENSU (IDX2)
      *
      *        食事療養：日数
               MOVE    SKT3600TMP-SNISSU (IDX2)    TO  WRK-Z6
               MOVE    WRK-Z6     TO  SKT3605-SNISSU (IDX2)
      *
      *        食事療養：金額
               MOVE    SKT3600TMP-SKINGK (IDX2)    TO  WRK-Z10
               MOVE    WRK-Z10     TO  SKT3605-SKINGK (IDX2)
      *
      *        食事療養：標準負担額
               MOVE    SKT3600TMP-SFTNGK (IDX2)    TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3605-SFTNGK (IDX2)
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理  (県内用)
      *****************************************************************
       3211-BODY-HEN-SEC      SECTION.
      *
      *    公費処理
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       7
                   OR      SKT3600TMP-KOHNUM (CNT-KOH)  = SPACE
      *
      *        公費名
               DISPLAY "公費名"SKT3600TMP-KOHNUM(CNT-KOH)
               IF  SKT3600TMP-KOHNUM(CNT-KOH)  =  "279"  OR  "277"
                                              OR  "699"  OR  "677"
                 EVALUATE  SKT3600TMP-KOHNUM(CNT-KOH)
                   WHEN  "279"
                       MOVE  "老人９割"  TO   SKT3605-KOHNAME(IDX2)
                   WHEN  "277"
                       MOVE  "老人７割"  TO   SKT3605-KOHNAME(IDX2)
                   WHEN  "699"
                       MOVE  "退職者７０歳以上９割"
                                         TO   SKT3605-KOHNAME2(IDX2)
                   WHEN  "677"
                       MOVE  "退職者７０歳以上７割"
                                         TO   SKT3605-KOHNAME2(IDX2)
                 END-EVALUATE
               ELSE
                 MOVE    SKT3600TMP-KOHNUM(CNT-KOH)  TO  WRK-KOHNUM
                 PERFORM 950-HKNNUM-SET-SEC
                 MOVE    HKN-HBTNUM          TO  WRK-MAE-INPUT
                 PERFORM 2009-HENKAN-SEC
                 IF  SKT3600TMP-KOHNAME(CNT-KOH)  NOT =  SPACE
                   STRING  WRK-OUT-INPUT(1:4)  DELIMITED BY SIZE
                           SKT3600TMP-KOHNAME(CNT-KOH)
                                               DELIMITED BY SPACE
                           INTO                SKT3605-KOHNAME(IDX2)
                   END-STRING
                 ELSE
                   STRING  WRK-OUT-INPUT(1:4)  DELIMITED BY SIZE
                           HKN-TANSEIDONAME    DELIMITED BY SPACE
                           INTO                SKT3605-KOHNAME(IDX2)
                   END-STRING
                 END-IF
               END-IF
      *
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3600TMP-K-KENSU(CNT-KOH IDX3)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3605-K-KENSU (IDX2 IDX3)
      *
      *            療養の給付：日数
                   MOVE    SKT3600TMP-K-NISSU(CNT-KOH IDX3)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3605-K-NISSU (IDX2 IDX3)
      *
      *            療養の給付：点数
                   MOVE    SKT3600TMP-K-TENSU(CNT-KOH IDX3)
                                                       TO  WRK-Z10
                   MOVE    WRK-Z10     TO  SKT3605-K-TENSU (IDX2 IDX3)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3600TMP-K-ITBFTN(CNT-KOH IDX3)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3605-K-ITBFTN (IDX2 IDX3)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3600TMP-K-SKENSU(CNT-KOH)    TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3605-K-SKENSU (IDX2)
      *
      *        食事療養：日数
               MOVE    SKT3600TMP-K-SNISSU(CNT-KOH)    TO  WRK-Z6
               MOVE    WRK-Z6      TO  SKT3605-K-SNISSU  (IDX2)
      *
      *        食事療養：金額
               MOVE    SKT3600TMP-K-SKINGK(CNT-KOH)    TO  WRK-Z10
               MOVE    WRK-Z10     TO  SKT3605-K-SKINGK  (IDX2)
      *
      *        食事療養：標準負担額
               MOVE    SKT3600TMP-K-SFTNGK(CNT-KOH)    TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3605-K-SFTNGK  (IDX2)
      *
               ADD     1       TO  CNT-KOH
           END-PERFORM
      *
      *
           .
       3211-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理  (県内用)
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           MOVE   "SKT3605.red"        TO  SPRT-PRTID
      *
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保総括表"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保総括表［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保総括表［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保総括表［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3605             TO  SPRT-PRTDATA
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
       331-PRINT-OUT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票編集<ヘッダー部>処理  (県外用)
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT3600
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT3600-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT3600-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN        TO  SKT3600-HOSPCD
      *
      *    郵便番号
           MOVE    SYS-1002-POST (1:3)     TO  SKT3600-POST (1:)
           MOVE    "-"                     TO  SKT3600-POST (4:)
           MOVE    SYS-1002-POST (4:)      TO  SKT3600-POST (5:)
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3600-ADRS
           INSPECT SKT3600-ADRS          REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3600-HOSPNAME
           INSPECT SKT3600-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SKT3600-KAISETU
      *
      *    県内・県外
           IF      SKT3600TMP-PREFKBN  =   "1"
               MOVE    "県内専用"          TO  SKT3600-PREFKBN
           ELSE
               MOVE    "県外専用"          TO  SKT3600-PREFKBN
           END-IF
      *
      *    在宅
           IF      SKT3600TMP-ZAITAKU      =   "1"
               MOVE    "在"                TO  SKT3600-ZAI1
               MOVE    "◯"                TO  SKT3600-ZAI2
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理  (県外用)
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       11
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3600TMP-KENSU (IDX2 IDX3)    TO  WRK-Z5
                   MOVE    WRK-Z5   TO  SKT3600-KENSU(IDX2 IDX3)
      *
      *            療養の給付：日数
                   MOVE    SKT3600TMP-NISSU (IDX2 IDX3)    TO  WRK-Z5
                   MOVE    WRK-Z5   TO  SKT3600-NISSU (IDX2 IDX3)
      *
      *            療養の給付：点数
                   MOVE    SKT3600TMP-TENSU (IDX2 IDX3)    TO  WRK-Z10
                   MOVE    WRK-Z10  TO  SKT3600-TENSU (IDX2 IDX3)
      *            療養の給付：一部負担金
                   MOVE    SKT3600TMP-ITBFTN (IDX2 IDX3)  TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3600-ITBFTN2 (IDX2 IDX3)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3600TMP-SKENSU (IDX2)    TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3600-SKENSU (IDX2)
      *
      *        食事療養：日数
               MOVE    SKT3600TMP-SNISSU (IDX2)    TO  WRK-Z6
               MOVE    WRK-Z6      TO  SKT3600-SNISSU (IDX2)
      *
      *        食事療養：金額
               MOVE    SKT3600TMP-SKINGK (IDX2)    TO  WRK-Z10
               MOVE    WRK-Z10     TO  SKT3600-SKINGK (IDX2)
      *
      *        食事療養：標準負担額
               MOVE    SKT3600TMP-SFTNGK (IDX2)    TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3600-SFTNGK (IDX2)
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理  (県外用)
      *****************************************************************
       3201-BODY-HEN-SEC      SECTION.
      *
      *    公費処理
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       7
                   OR      SKT3600TMP-KOHNUM (CNT-KOH) =   SPACE
      *
      *        公費名
               DISPLAY "公費名"SKT3600TMP-KOHNUM(CNT-KOH) 
               IF  SKT3600TMP-KOHNUM(CNT-KOH)  =  "279"  OR  "277"
                                              OR  "699"  OR  "677"
                 EVALUATE  SKT3600TMP-KOHNUM(CNT-KOH)
                   WHEN  "279"
                       MOVE  "老人９割"  TO   SKT3600-KOHNAME(IDX2)
                   WHEN  "277"
                       MOVE  "老人７割"  TO   SKT3600-KOHNAME(IDX2)
                   WHEN  "699"
                       MOVE  "退職者７０歳以上９割"
                                         TO   SKT3600-KOHNAME2(IDX2)
                   WHEN  "677"
                       MOVE  "退職者７０歳以上７割"
                                         TO   SKT3600-KOHNAME2(IDX2)
                 END-EVALUATE
               ELSE
                 MOVE    SKT3600TMP-KOHNUM(CNT-KOH)  TO  WRK-KOHNUM
                 PERFORM 950-HKNNUM-SET-SEC
                 MOVE    HKN-HBTNUM      TO  WRK-MAE-INPUT
                 PERFORM 2009-HENKAN-SEC
                 IF  SKT3600TMP-KOHNAME(CNT-KOH)  NOT =  SPACE
                   STRING  WRK-OUT-INPUT(1:4)  DELIMITED BY SIZE
                           SKT3600TMP-KOHNAME(CNT-KOH)
                                               DELIMITED BY SPACE
                           INTO                SKT3600-KOHNAME(IDX2)
                   END-STRING
                 ELSE
                   STRING  WRK-OUT-INPUT(1:4)  DELIMITED BY SIZE
                           HKN-TANSEIDONAME    DELIMITED BY SPACE
                           INTO                SKT3600-KOHNAME(IDX2)
                   END-STRING
                 END-IF
               END-IF
      *
              PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3600TMP-K-KENSU(CNT-KOH IDX3)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3600-K-KENSU (IDX2 IDX3)
      * 
      *            療養の給付：日数
                   MOVE    SKT3600TMP-K-NISSU(CNT-KOH IDX3)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3600-K-NISSU (IDX2 IDX3)
      *
      *            療養の給付：点数
                   MOVE    SKT3600TMP-K-TENSU(CNT-KOH IDX3)
                                                       TO  WRK-Z10
                   MOVE    WRK-Z10     TO  SKT3600-K-TENSU (IDX2 IDX3)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3600TMP-K-ITBFTN(CNT-KOH IDX3)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3600-K-ITBFTN (IDX2 IDX3)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3600TMP-K-SKENSU(CNT-KOH)    TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3600-K-SKENSU (IDX2)
      *
      *        食事療養：日数
               MOVE    SKT3600TMP-K-SNISSU(CNT-KOH)    TO  WRK-Z6
               MOVE    WRK-Z6      TO  SKT3600-K-SNISSU  (IDX2)
      *
      *        食事療養：金額
               MOVE    SKT3600TMP-K-SKINGK(CNT-KOH)    TO  WRK-Z10
               MOVE    WRK-Z10     TO  SKT3600-K-SKINGK  (IDX2)
      *
      *        食事療養：標準負担額
               MOVE    SKT3600TMP-K-SFTNGK(CNT-KOH)    TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3600-K-SFTNGK  (IDX2)
      *
               ADD     1       TO  CNT-KOH
           END-PERFORM
      *
           .
       3201-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理  (県外用)
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           MOVE   "SKT3600.red"        TO  SPRT-PRTID
           MOVE    "国保総括表(県外)"  TO  SPRT-TITLE
           MOVE    SKT3600             TO  SPRT-PRTDATA
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
057400*
      *****************************************************************
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
      *
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO  IDX-MAX
                               IDX2 
           MOVE    SPACE   TO  WRK-KOHNUM
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       20
               IF      SKT3600TMP-KOHNUM(IDX1) =   SPACE
                   MOVE    20      TO  IDX1
               ELSE
                   IF      WRK-KOHNUM          =   SPACE
                   OR      SKT3600TMP-KOHNUM(IDX1) <   WRK-KOHNUM
                       MOVE    IDX1            TO  IDX2
                       MOVE    SKT3600TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
                   END-IF
                   ADD     1       TO  IDX-MAX
               END-IF
           END-PERFORM
           IF      IDX2    >   ZERO
               MOVE    SKT3600TMP-KOH-TBL(IDX2)    TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "一般ソート前１" SKT3600TMP-KOHNUM(1)
           DISPLAY "一般ソート前２" SKT3600TMP-KOHNUM(2)
           DISPLAY "一般ソート前３" SKT3600TMP-KOHNUM(3)
      *
           IF      IDX-MAX     >   ZERO
               MOVE    2       TO  IDX1
               MOVE    1       TO  IDX2
               MOVE    1       TO  IDX3
               MOVE    "999"   TO  WRK-KOHNUM
               PERFORM UNTIL   IDX1    >   IDX-MAX
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       IDX-MAX
                       IF      (SKT3600TMP-KOHNUM(IDX2)
                                           >   SORT-KOHNUM(IDX1 - 1))
                       AND     (SKT3600TMP-KOHNUM(IDX2)
                                           <   WRK-KOHNUM)
                           MOVE    SKT3600TMP-KOHNUM(IDX2)
                                           TO  WRK-KOHNUM
                           MOVE    IDX2    TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    SKT3600TMP-KOH-TBL(IDX3)
                                       TO  SORT-KOH-TBL(IDX1)
                   MOVE    "999"       TO  WRK-KOHNUM
                   ADD     1           TO  IDX1
               END-PERFORM
           END-IF
      *
           INITIALIZE  SKT3600TMP-KOH-AREA
           MOVE    SORT-AREA   TO  SKT3600TMP-KOH-AREA
      *
           DISPLAY "一般ソート後１" SKT3600TMP-KOHNUM(1)
           DISPLAY "一般ソート後２" SKT3600TMP-KOHNUM(2)
           DISPLAY "一般ソート後３" SKT3600TMP-KOHNUM(3)
      *
      *
           .
       KOH-SORT-EXT.
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
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
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
      *
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU3600 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU3600 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                   MOVE    RECE10-KEY      TO  WRK-RECEERR(33:)
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
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
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE    "00"                 TO  HKN-PAYKBN
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                        TO  HKN-TEKSTYMD(1:6)
                                            HKN-TEKEDYMD(1:6)
           MOVE    "01"                 TO  HKN-TEKSTYMD(7:2)
                                            HKN-TEKEDYMD(7:2)
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
           ELSE
               DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
      *
             IF    HKN-HKNNUM  =  "051"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "特定疾患"   TO   HKN-TANSEIDONAME
             END-IF
      *
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
           MOVE    SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM      TO  RECE10-SKYYM.
      *
           MOVE    SPACE                   TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"              TO  RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000                  TO  RECE10-SRYYM
             MOVE  999999                  TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF     MCP-RC        =       ZERO
                  PERFORM  900-SEIKYU-READ-SEC
           ELSE
                  MOVE    1    TO      FLG-END
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
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
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
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *         
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込（後期高齢）
      *****************************************************************
       950-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
           MOVE   SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM      TO  RECE10-SKYYM
           MOVE   WRK-CONS-SRYYM-200804   TO  RECE10-SRYYM
           MOVE   999999                  TO  RECE10-XXSRYYM
           MOVE   WRK-HKNNUM              TO  RECE10-HKNNUM-S
                                              RECE10-HKNNUM
      *
           MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 950-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               DISPLAY "SELECT ERR"
           END-IF
           .
      *
       950-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（後期高齢）
      *****************************************************************
       950-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH2           TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  950-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF   
      *         
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
      *
       950-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       950-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH2            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT3600TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT3600TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT3600TMP
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
           READ  SKT3600TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC                  =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SEIKYU-INV-EXT.
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
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
