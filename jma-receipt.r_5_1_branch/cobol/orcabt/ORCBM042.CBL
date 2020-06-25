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
       PROGRAM-ID.             ORCBM042.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 労災・総括表
      *  管理者            : 
      *  作成日付    作業者        記述
      * 117/09/27    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    13/05/07  公務災害を対象外とする
      *  04.07.02    NACL-藤原    13/09/27  ￥マークの変更
      *  04.07.03    NACL-藤原    15/07/27  医療機関編集情報からの郵便番
      *                                     号の編集を追加
      *  04.07.04    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
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
           COPY    "HCM72V01.INC"  REPLACING  //HCM72V01//
                                   BY         //HCM72//.
      *
           COPY    "HCM73V01.INC"  REPLACING  //HCM73V01//
                                   BY         //HCM73//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECE10          PIC X(02). 
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
      *     
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
           03  FLG-RECE10          PIC 9(01). 
           03  FLG-PTINF           PIC 9(01).
           03  FLG-LABOR-SIO       PIC 9(01).
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *     
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMDWH        PIC X(06).
           03  WRK-SRYYMWH         PIC X(04).
           03  WRK-SRYYMD          PIC X(08).
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    数字項目編集
           03  WRK-NINZUZ          PIC ZZ9.
           03  WRK-KENSUZ          PIC ZZ9.
           03  WRK-KINGAKZ         PIC ZZZZZZZZ9.
           03  WRK-9               PIC 9(04).
           03  WRK-9-R             REDEFINES   WRK-9.
               05  FILLER          PIC X(03).
               05  WRK-9-X         PIC X(01).
      *
      *    医療機関情報編集
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-TEL             PIC X(15).
           03  WRK-POST            PIC X(07).
           03  WRK-KAISETUNAME     PIC X(80).
      *
           03  WRK-RSIIRYOCD       PIC X(07).
           03  WRK-PREFNAME        PIC X(08).
      *
      *    数字半角全角変換用
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-IDX    PIC 9(02).
               05  WRK-CHAR-FLG    PIC 9(01).
      *
               05  WRK-CHAR-X      PIC X(10).
               05  WRK-CHAR-N      PIC X(20).
               05  WRK-CHAR-CNT    PIC 9(02).
               05  WRK-CHAR-N-CNT  PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(04).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-TABLE.
               07  SUM-KENSU       PIC 9(06).
               07  SUM-KINGAK      PIC 9(13).
               07  SUM-NINZU       PIC 9(03).
      *
      *    キー領域
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-NEW1.
                   07  KEY-N-SYOKAIFLG
                                   PIC X(01).
                   07  KEY-N-SYOCD PIC X(05).
               05  KEY-N-PTNUM     PIC X(20).
           03  KEY-OLD.
               05  KEY-OLD1.
                   07  KEY-O-SYOKAIFLG
                                   PIC X(01).
                   07  KEY-O-SYOCD PIC X(05).
               05  KEY-O-PTNUM     PIC X(20).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    全角数字
       01  TBL-SUJI-AREA.
           03  FILLER              PIC X(20)   VALUE
               "１２３４５６７８９０".
       01  TBL-SUJI-R              REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI            PIC X(02)   OCCURS  10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    労働基準監督署
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
           COPY    "CPSK1903.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
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
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    県名取得パラメタ
           COPY    "CPORCSPREF.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
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
           MOVE    LOW-VALUE       TO  KEY-AREA    
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM042"      TO  JOB-PGID
           MOVE    "労災・診療費請求書"
                                   TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "RECEUPD"       TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  RECEUPD-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECEUPD-BASENAME
                                   TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEUPD
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  WRK-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *     
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SRYYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1     =   ZERO 
               MOVE    LNK-DAY2-EDTYMD1-Y
                                       TO  WRK-SRYYMWH (1:2)
               MOVE    LNK-DAY2-EDTYMD1-M
                                       TO  WRK-SRYYMWH (3:2)
           END-IF
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1     =   ZERO 
               MOVE    LNK-DAY2-EDTYMD1-Y
                                       TO  WRK-SEIYMDWH (1:2)
               MOVE    LNK-DAY2-EDTYMD1-M
                                       TO  WRK-SEIYMDWH (3:2)
               MOVE    LNK-DAY2-EDTYMD1-D
                                       TO  WRK-SEIYMDWH (5:2)
           END-IF
      *
      *    労災医療機関情報取得
           INITIALIZE                  SYS-4001-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-4001-HOSPNUM
           MOVE    "4001"          TO  SYS-4001-KANRICD
           MOVE    "*"             TO  SYS-4001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-4001-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-4001-STYUKYMD (7:2)
           MOVE    SYS-4001-STYUKYMD
                                   TO  SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-4001-REC
               IF      SYS-4001-RSIIRYOCDN =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-4001-RSIIRYOCDN
               END-IF
           ELSE
               MOVE    "労災自賠責医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
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
      *    医療機関情報取得
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD
                                   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME
                                           =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    医療機関情報取得−所在地
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD
                                   TO  SYS-1002-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
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
           MOVE    SYS-1002-TEL        TO  WRK-TEL
           MOVE    SYS-1002-POST       TO  WRK-POST
      *
      *    医療機関編集情報
           PERFORM 900-1900-HENSYU-SEC
      *
      *    都道府県名取得
           INITIALIZE                      SPREF-AREA
           CALL    "ORCSPREF"          USING   SPREF-AREA
                                               SPA-AREA
           MOVE    SYS-1001-PREFNUM    TO  IDX
      *    東京、大阪、京都は「府、都」を消去
           IF      IDX                 =   13  OR  26  OR  27
               MOVE    SPACE               TO  SPREF-PREFNAME2 (IDX)(5:)
           END-IF
           MOVE    SPREF-PREFNAME2 (IDX)
                                       TO  WRK-PREFNAME
      *
           OPEN    OUTPUT  UPD-FILE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    労働者災害補償保険診療費請求書
           MOVE    1               TO  FLG-SYORI
           PERFORM 2001-SYUKEI-SEC
      *
      *    アフターケア委託費請求書
           MOVE    2               TO  FLG-SYORI
           PERFORM 2001-SYUKEI-SEC
      *
           MOVE    1               TO  FLG-END 
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
           DISPLAY "FLG-SYORI=" FLG-SYORI
      *
      *
           MOVE    LOW-VALUE       TO  KEY-AREA    
      *
           MOVE    "key77"         TO  WRK-MCP-PATHNAME
           MOVE    "tbl_seikyu"    TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM         UNTIL   FLG-RECE10  =   1 
      *
               INITIALIZE                  SUM-AREA
               MOVE    KEY-NEW             TO  KEY-OLD      
               PERFORM             UNTIL   FLG-RECE10      =   1
                                   OR      KEY-NEW1    NOT =   KEY-OLD1
      *          患者の人数
                 ADD     1                   TO  SUM-NINZU
      * 
                 MOVE    KEY-NEW             TO  KEY-OLD      
                 PERFORM             UNTIL   FLG-RECE10   =   1
                                     OR      KEY-NEW  NOT =   KEY-OLD
      * 
                   MOVE    ZERO                TO  FLG-UPD
      *
                   ADD     1                   TO  SUM-KENSU
                   ADD     RECE10-FTNMONEY (1) TO  SUM-KINGAK
                   MOVE    1                   TO   FLG-UPD
      *
      *            集計の対象にならなかったとき                
                   IF      FLG-UPD             =   1
                       PERFORM 20011-UPD-FILE-HENSYU-SEC
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-SEIKYU-READ-SEC
                   ELSE
                       MOVE    SPACE               TO  WRK-RECEERR
                       STRING  "集計対象外　患者番号＝"
                                                   DELIMITED  BY  SIZE
                               RECE10-PTNUM        DELIMITED  BY  SPACE
                               " 診療年月＝"       DELIMITED  BY  SIZE
                               RECE10-SRYYM        DELIMITED  BY  SIZE
                               " 入外区分＝"       DELIMITED  BY  SIZE
                               RECE10-NYUGAIKBN    DELIMITED  BY  SIZE
                                                   INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING  "ORCBM042 "         DELIMITED  BY  SIZE
                               WRK-RECEERR         DELIMITED  BY  SIZE
                               LOW-VALUE           DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                       END-STRING   
                       CALL    "cobabort"          USING   WRK-ERRMSG
                   END-IF 
                 END-PERFORM
               END-PERFORM
      *
               PERFORM 2002-PRINT-HENSYU-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       20011-UPD-FILE-HENSYU-SEC       SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           IF      STS-RECEUPD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-ERR-AREA
               INITIALIZE                  WRK-ERR-AREA
               MOVE    3               TO  WRK-ERR-FLG
               MOVE    RECEUPD         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECEUPD     TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           ADD     1                   TO  CNT-UPDFILE
           .
       20011-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-HENSYU-SEC                SECTION.
      *           
      *    帳票編集処理
           PERFORM 20021-PRINT-HEN-SEC
      *    帳票印刷処理
           PERFORM 20023-PRINT-OUT-SEC
      *
           .
       2002-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       20021-PRINT-HEN-SEC             SECTION.
      *
           EVALUATE    FLG-SYORI
               WHEN    1 
                   INITIALIZE                      HCM73
      *
                   MOVE    WRK-RSIIRYOCD       TO  HCM73-RSIIRYOCD
      *
                   MOVE    SUM-KINGAK          TO  WRK-KINGAKZ
                   MOVE    WRK-KINGAKZ         TO  HCM73-GOKEI
                   PERFORM VARYING IDX FROM    8   BY  -1
                           UNTIL   IDX <       1
                       IF      WRK-KINGAKZ (IDX:1) =   SPACE
                           MOVE    "\"             TO
                                                   HCM73-MARK-S (IDX)
                           MOVE    1               TO  IDX
                       END-IF
                   END-PERFORM    
      *
                   MOVE    SUM-KENSU           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ          TO  HCM73-MAISU
      *
                   MOVE    WRK-SRYYMWH         TO  HCM73-SKYYM
      *
                   MOVE    WRK-SEIYMDWH        TO  HCM73-PRTYMD
      *
                   COMPUTE SUM-NINZU   =   SUM-NINZU   -   1
                   MOVE    SUM-NINZU           TO  WRK-NINZUZ
                   MOVE    WRK-NINZUZ          TO  HCM73-NINZU
      *
                   MOVE    WRK-POST (1:3)      TO  HCM73-HOSPPOST (1:3)
                   MOVE    "-"                 TO  HCM73-HOSPPOST (4:1)
                   MOVE    WRK-POST (4:4)      TO  HCM73-HOSPPOST (5:4)
      *
                   MOVE    WRK-HOSPADRS        TO  HCM73-HOSPADRS
                   INSPECT HCM73-HOSPADRS
                                   REPLACING   ALL "  "    BY  "　"
      *
                   MOVE    WRK-HOSPNAME        TO  HCM73-HOSPNAME
                   INSPECT HCM73-HOSPNAME
                                   REPLACING   ALL "  "    BY  "　"
      *    
                   MOVE    WRK-KAISETUNAME     TO  HCM73-KAISETSUNAME
      *    
                   MOVE    WRK-TEL             TO  HCM73-HOSPTEL
      *
                   MOVE    WRK-PREFNAME        TO  HCM73-ROUDOUNAME
      *
                   IF      KEY-O-SYOKAIFLG     =   "1"
                       MOVE    KEY-O-SYOCD         TO  HCM73-SYOCD
      *
                       INITIALIZE                  LABOR-SIO-REC
                       MOVE    KEY-O-SYOCD         TO  LABOR-SIO-SYOCD
                       MOVE    LNK-PRTKANRI-SRYYM  TO
                                               LABOR-SIO-YUKOSTYMD(1:6)
                       MOVE    "01"                TO
                                               LABOR-SIO-YUKOSTYMD(7:2)
                       MOVE    LABOR-SIO-YUKOSTYMD TO
                                               LABOR-SIO-YUKOEDYMD
                       MOVE    LABOR-SIO-REC       TO  MCPDATA-REC
                       PERFORM 900-LABOR-SIO-READ-SEC
                       IF      FLG-LABOR-SIO       =   ZERO
                           STRING  "（"            DELIMITED   BY  SIZE
                                   LABOR-SIO-NAME  DELIMITED   BY  SPACE
                                   "署分）"        DELIMITED   BY  SIZE
                                                   INTO    HCM73-SYONAME
                           END-STRING
                       END-IF
                   END-IF
               WHEN    2 
                   INITIALIZE                      HCM72
      *
                   MOVE    WRK-RSIIRYOCD       TO  HCM72-RSIIRYOCD
      *
                   MOVE    SUM-KINGAK          TO  WRK-KINGAKZ
                   MOVE    WRK-KINGAKZ         TO  HCM72-GOKEI
                   PERFORM VARYING IDX FROM    8   BY  -1
                           UNTIL   IDX <       1
                       IF      WRK-KINGAKZ (IDX:1) =   SPACE
                           MOVE    "\"            TO
                                                   HCM72-MARK-S (IDX)
                           MOVE    1               TO  IDX
                       END-IF
                   END-PERFORM    
      *
                   MOVE    SUM-KENSU           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ          TO  HCM72-MAISU
      *
                   MOVE    "7"                 TO  HCM72-SKYYM (1:1)
                   MOVE    WRK-SRYYMWH         TO  HCM72-SKYYM (2:4)
      *
                   MOVE    WRK-SEIYMDWH        TO  HCM72-PRTYMD
      *
                   COMPUTE SUM-NINZU   =   SUM-NINZU   -   1
                   MOVE    SUM-NINZU           TO  WRK-NINZUZ
                   MOVE    WRK-NINZUZ          TO  HCM72-NINZU
      *    
                   MOVE    WRK-POST (1:3)      TO  HCM72-HOSPPOST (1:3)
                   MOVE    "-"                 TO  HCM72-HOSPPOST (4:1)
                   MOVE    WRK-POST (4:4)      TO  HCM72-HOSPPOST (5:4)
      *
                   MOVE    WRK-HOSPADRS        TO  HCM72-HOSPADRS
                   INSPECT HCM72-HOSPADRS
                                   REPLACING   ALL "  "    BY  "　"
      *
                   MOVE    WRK-HOSPNAME        TO  HCM72-HOSPNAME
                   INSPECT HCM72-HOSPNAME
                                   REPLACING   ALL "  "    BY  "　"
      *    
                   MOVE    WRK-KAISETUNAME     TO  HCM72-KAISETSUNAME
      *    
                   MOVE    WRK-TEL             TO  HCM72-HOSPTEL
      *
                   MOVE    WRK-PREFNAME        TO  HCM72-ROUDOUNAME
           END-EVALUATE
           .
       20021-PRINT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20023-PRINT-OUT-SEC                 SECTION.
      *
           ADD     1                   TO  CNT-PAGE
      *
           ADD     1                   TO  LNK-PRTKANRI-SHORI-RENNUM
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
           EVALUATE    FLG-SYORI
               WHEN    1
                   MOVE   "HCM73V01.red"       TO  SPRT-PRTID
                   MOVE    HCM73               TO  SPRT-PRTDATA
      *
                   IF    ( KEY-O-SYOKAIFLG     =   "1"  )
                   AND   ( FLG-LABOR-SIO       =   ZERO )
                       STRING  LABOR-SIO-NAME      DELIMITED  BY  SPACE
                               "　労働者災害補償保険診療費請求書"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPRT-TITLE
                       END-STRING
                   ELSE
                       MOVE    "労働者災害補償保険診療費請求書" 
                                                   TO  SPRT-TITLE
                   END-IF
               WHEN    2
                   MOVE   "HCM72V01.red"       TO  SPRT-PRTID
                   MOVE    HCM72               TO  SPRT-PRTDATA
                   MOVE    "アフターケア委託費請求書" 
                                               TO  SPRT-TITLE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *     
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
               MOVE    SPACE               TO  WRK-ERRMSG
               STRING  "ORCBM042 "         DELIMITED  BY  SIZE
                       WRK-RECEERR         DELIMITED  BY  SIZE
                       LOW-VALUE           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING   
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF                                                              
           .
       20023-PRINT-OUT-EXT.
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
           STRING  "ORCBM042 "             DELIMITED   BY  SIZE
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
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE   UPD-FILE
      *
      *
           IF      CNT-RECE10      =   ZERO 
               CONTINUE
           ELSE
              IF    ( FLG-UPD          =   1    )
              AND   ( FLG-ERR          =   ZERO ) 
                   IF      CNT-UPDFILE    >   ZERO
                       PERFORM 310-SEIKYU-UPD-SEC
                   END-IF
               END-IF     
           END-IF    
      *
           DISPLAY "*** ORCBM042 IN   "  CNT-RECE10 
           DISPLAY "***          PAGE "  CNT-PAGE
           DISPLAY "*** ORCBM042 END ***"
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
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                   STRING "請求管理ＤＢ　読込み更新エラー　"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
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
           MOVE    3                   TO  RECE10-TEISYUTUSAKI
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
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
               EVALUATE    FLG-SYORI
                   WHEN    1
                       IF      RECE10-HKNKBN   =   "3" OR  "4"
                           GO  TO  900-SEIKYU-READ-SEC
                       END-IF
                   WHEN    2
                       IF      RECE10-HKNKBN   =   "3"
                           CONTINUE
                       ELSE
                           GO  TO  900-SEIKYU-READ-SEC
                       END-IF
               END-EVALUATE
      *         
               MOVE    RECE10-HKNJANUM-S   TO  KEY-N-SYOCD
               MOVE    RECE10-SYOKAIFLG    TO  KEY-N-SYOKAIFLG
               MOVE    RECE10-PTNUM        TO  KEY-N-PTNUM
      ***         MOVE SPACE TO KEY-N-PTNUM
               ADD     1                   TO  CNT-RECE10   
      *         
               DISPLAY "PTNUM="     RECE10-PTNUM
                       " SRYYM="    RECE10-SRYYM 
                       " SYOKAFLG=" RECE10-SYOKAIFLG
                       " HKNJANUM-S="    RECE10-HKNJANUM-S
                       " HKNKBN="    RECE10-HKNKBN 
                       " " RECE10-RECESYUBETU
                       " FTNMONEY=" RECE10-FTNMONEY (1)
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
       900-PTINF-READ-SEC         SECTION.
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
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-HENSYU-SEC           SECTION.
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
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN (23)
                                       NOT =   SPACE
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
       900-1900-HENSYU-EXT.
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
           MOVE    SYS-1900-PRTKBN (23)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
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
           MOVE    SYS-1900-PRTKBN (23)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-TEL
               MOVE    SYS-1902-POST   TO  WRK-POST
           ELSE
               INITIALIZE                  SYS-1902-REC
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
           MOVE    SYS-1900-PRTKBN(23)
                                   TO  SYS-1903-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1903-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD
                                   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
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
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
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
       900-SYSKANRI-KEY10-EXT.
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
      *    労働基準監督署名読込
      *****************************************************************
       900-LABOR-SIO-READ-SEC         SECTION.
      *
           MOVE    "tbl_labor_sio"    TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
           MOVE    "tbl_labor_sio"    TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-LABOR-SIO
                   MOVE    MCPDATA-REC         TO  LABOR-SIO-REC
               ELSE
                   MOVE    1                   TO  FLG-LABOR-SIO
               END-IF
           ELSE
               MOVE    1                   TO  FLG-LABOR-SIO
           END-IF
      *
           MOVE    "tbl_labor_sio"    TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-LABOR-SIO-READ-EXT.
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
               STRING  "ORCBM042 select err TABLE="
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
