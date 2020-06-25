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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGI20.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院患者検索
      *  コンポーネント名  : 入院患者検索（Ｉ２０）
      *  管理者            : 
      *  02/09/10    NACL−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02/12/05  診療科９９件対応等
      *  01.00.02    NACL-太田    02/01/14  COPY句の重複定義を修正
      *  01.00.03    NACL-太田    03/10/14  CSV出力、印刷機能追加
      *  01.00.04    NACL-太田    04/01/14  退院日入力機能追加
      *  01.00.05    NACL-太田    04/01/30  状態に当月在院を追加
      *  01.00.06    NACL-太田    04/02/09  コラムリスト編集修正
      *  02.07.00    NACL-太田    05/10/27  MONFUNC対応
      *  03.05.00    NACL-太田    07.05.29  グループ診療対応
      *  04.01.01     NACL-藤原   07/10/19  クライアント保存対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  BI201-FILE      ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *    パラメータファイル
       FD  BI201-FILE.
           COPY    "CPBI201.INC".
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入院患者照会共通ＳＰＡ
           COPY    "I2SCR-SPA".
      *
      *    入院患者照会用共通パラメタ
           COPY    "I2COMMON-SPA".
      *
           COPY    "ENUM-VALUE".     
      *
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //PRFNAME//.
      *
           COPY    "CPASGNPARA.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTSRH           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *     
           03  FLG-CHK             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-JYOKEN          PIC 9(01).
           03  FLG-JOBEND          PIC 9(01).
           03  FLG-GMN-INIT        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(05).
           03  IDXA                PIC 9(04).
           03  IDXD                PIC 9(05).
           03  IDXE                PIC 9(05).
           03  IDXF                PIC 9(05).
           03  IDX-CMB             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(03). 
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CMB-CD          PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM     PIC X(200).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(09).
               05  WRK-WIDGET2     PIC 9(01).  
           03  WRK-YMD             PIC X(10).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-PIN.
               05  PIN9            PIC 9(03).
               05  WRK-PIN-R       REDEFINES  PIN9.
                   07  PIN10       PIC 9(02).
                   07  PIN1        PIC 9(01).
           03  WRK-PED.
               05  PEDX            PIC X(02)  OCCURS  15.
           03  WRK-PKANA           PIC X(02).
           03  WRK-SQL-PNT         PIC 9(05).
           03  WRK-SQL             PIC X(15000).
           03  WRK-SYSYMD          PIC 9(08).
           03  WRK-AGE             PIC 9(03).
      *
           03  WRK-HENFLG          PIC X(01).
      *     
           03  WRK-BRMNUM-9        PIC 9(08).
           03  WRK-BRMNUM-Z        PIC ZZZZZZZ9.
      *
           03  WRK-JYOKEN-AREA.
               05  WRK-NAME        PIC X(200).
               05  WRK-STAGE       PIC 9(03).
               05  WRK-EDAGE       PIC 9(03). 
               05  WRK-ADRS        PIC X(150).
               05  WRK-STBIRTHDAY  PIC 9(08).
               05  WRK-EDBIRTHDAY  PIC 9(08).
               05  WRK-MOJISU1     PIC 9(03).
               05  WRK-MOJISU2     PIC 9(03).
           03  WRK-SELNUMST        PIC 9(05).
           03  WRK-SELNUMED        PIC 9(05).
           03  WRK-STYMD           PIC X(08).
           03  WRK-EDYMD           PIC X(08).
           03  WRK-SRT1KBN         PIC X(01).
           03  WRK-SRT2KBN         PIC X(01).
           03  WRK-SRT3KBN         PIC X(01).
           03  WRK-SRTKBN          PIC X(01).
           03  WRK-ORDER           PIC X(1).
      *
           03  WRK-FILENM-AREA.
               07  WRK-FILENM      PIC X(100).
               07  WRK-FILENM-FOLDER 
                                   PIC X(100).
           03  WRK-ORCSSTR-NUM1    PIC 9(03).
           03  WRK-LIMIT           PIC 9(05).
           03  WRK-OFFSET          PIC 9(05).
      *
       01  WRK-ROW-AREA.
           03  WRK-KANJYALST-ROW   PIC S9(9)   BINARY.
      *
       01  WRK-HEN-AREA.
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *         
           03  WRK-I2IDMSG         PIC X(80).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05  WRK-STR-LEFT-FL                 PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-SKIP-MAX        PIC 9(05).
           03  WRK-SKIP-KNS        PIC 9(05).
           03  WRK-PROC-FLG                        PIC 9(1).
           03  WRK-SHORIERR                        PIC X(200).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1                    PIC 9(04).
               05  WRK-CALC-MM1                    PIC 9(02).
               05  WRK-CALC-DD1                    PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2                    PIC 9(04).
               05  WRK-CALC-MM2                    PIC 9(02).
               05  WRK-CALC-DD2                    PIC 9(02).
           03  WRK-AGE-YY                          PIC S9(04).
           03  WRK-AGE-MM                          PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z                       PIC ---9.
               05  WRK-AGE-MEI                     PIC X(04).
           03  WRK-NAME-G.
               05  WRK-NAME1                       PIC X(100).
               05  WRK-NAME2                       PIC X(100).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-AGE-TABLE.
               05  FILLER          PIC X(42)   VALUE 
                   "000010020030040050060070080090100110120130".
               05  FILLER          PIC X(42)   VALUE
                   "009019029039049059069079089099109119129139".
           03  WRK-CONS-AGE-TABLE-R    REDEFINES   WRK-CONS-AGE-TABLE.
               05  WRK-CONS-STAGE      PIC 9(03)   OCCURS  14.
               05  WRK-CONS-EDAGE      PIC 9(03)   OCCURS  14.
      *                                      
       01  PIN-TBL-AREA.
           03  KANA-TBL.
               05 FILLER PIC X(20) VALUE "アイウエオ　　　　　".
               05 FILLER PIC X(20) VALUE "カキクケコ　　　　　".
               05 FILLER PIC X(20) VALUE "サシスセソ　　　　　".
               05 FILLER PIC X(20) VALUE "タチツテト　　　　　".
               05 FILLER PIC X(20) VALUE "ナニヌネノ　　　　　".
               05 FILLER PIC X(20) VALUE "ハヒフヘホ　　　　　".
               05 FILLER PIC X(20) VALUE "マミムメモ　　　　　".
               05 FILLER PIC X(20) VALUE "ヤ　ユ　ヨ　　　　　".
               05 FILLER PIC X(20) VALUE "ラリルレロ　　　　　".
               05 FILLER PIC X(20) VALUE "ワヲン゛゜　　　　　".
               05 FILLER PIC X(20) VALUE "ー　　　　　　　　　".
               05 FILLER PIC X(20) VALUE "ＡＧＭＳＹａｇｍｓｙ".
               05 FILLER PIC X(20) VALUE "ＢＨＮＴＺｂｈｎｔｚ".
               05 FILLER PIC X(20) VALUE "ＣＩＯＵ　ｃｉｏｕ　".
               05 FILLER PIC X(20) VALUE "ＤＪＰＶ　ｄｊｐｖ　".
               05 FILLER PIC X(20) VALUE "ＥＫＱＷ　ｅｋｑｗ　".
               05 FILLER PIC X(20) VALUE "ＦＬＲＸ　ｆｌｒｘ　".
           03  KANA-TBL-R          REDEFINES  KANA-TBL.
               05  PKANAX          OCCURS   17.
                   07  PKANA       PIC X(02)  OCCURS  10.
      *
       01  CONST-AREA.
           03  CONST-NAME-MAX          PIC 9(02)  VALUE 15.
           03  CONST-GMN-MAX           PIC 9(05)  VALUE 200.
           03  CONST-LINE              PIC 9(05)  VALUE   30.
           03  CONST-JOBID             PIC 9(07)
               VALUE   1.
           03  CONST-SHELLID           PIC X(08)   VALUE   "ORCBI201".
           03  CONST-UTF8              PIC 9(01)   VALUE 2.
      *
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WRK-SPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK5013.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
       01  PTSRH-REC. 
           COPY    "CPPTSRH.INC".
      *
       01  I001-REC. 
           COPY    "CPI001.INC".
      *
      *    シェルテーブル
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      ***     COPY    "ORCA-DBPATH".
      *
      *    入院患者照会サブ
           COPY    "CPI2SUB01.INC".
           COPY    "CPI2SUB02.INC".
           COPY    "CPI2SUB03.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    半角チェックサブ（拡張漢字対応版）
           COPY    "CPORCSKANACHK2.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    保険組合せ取得処理
           COPY    "CPORCSHKNSET.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    クライアント保存可否情報取得サブ
           COPY    "CPORCSCLIENTSV.INC".
      *     
           COPY    "ORCA-DBMETA".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01 SCRAREA.
           COPY    "ORCA36SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I2ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "I20    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    情報削除画面
           WHEN    "XB01"
               MOVE    LNK-COMMON          TO  SPA-KYOUTU
               MOVE    LNK-SCRSPA          TO  SPA-I2FREE
               MOVE    SPA-WORK            TO  SPA-I2KYOUTU
               GO  TO      300-SCREEN-EXT
           WHEN    "I2ID1"
                   PERFORM 330-I2ID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           WHEN    "I22"
                   PERFORM 3001-I22SPASET-SEC
                   GO  TO  300-SCREEN-EXT
           WHEN    "I23"
                   PERFORM 3001-I23SPASET-SEC
                   GO  TO  300-SCREEN-EXT
      *
           WHEN    "I21"
                   GO  TO  300-SCREEN-EXT
           WHEN    "I01"
      *
      *        退避しておいたＳＰＡの内容を元に戻す
               INITIALIZE                  I2SUB01-LNK
               MOVE    "I"             TO  I2SUB01-KBN
               CALL    "ORCGI2SUB01"   USING   I2SUB01-LNK
                                               SPA-AREA
      *
               MOVE    I2SUB01-SCRSPA-AREA
                                       TO  SPA-I2FREE
               MOVE    I2SUB01-COMMON  TO  SPA-I2KYOUTU
      *
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｉ２２戻りスパ編集処理
      *****************************************************************
       3001-I22SPASET-SEC             SECTION.
      *
           IF  ( SPA-I22-RTOK-FLG =   ZERO )
               GO  TO  3001-I22SPASET-EXT
           END-IF
      *
           PERFORM 900-PTSRH-INS2-SEC
      *
           MOVE    SPA-HOSPNUM         TO  PRFNAME-HOSPNUM
           MOVE    "ORC36001"          TO  PRFNAME-FILE-ID
           MOVE    1                   TO  PRFNAME-SYOKBN1
           MOVE    SPA-TERMID          TO  PRFNAME-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    PRFNAME-BASENAME    TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGNPARA
           MOVE    SGETTEMP-ST         TO  ASGNPARA-ST
      *
           PERFORM 800-MCNDATE-SEC
      *
           OPEN    OUTPUT              BI201-FILE
      *
           INITIALIZE                  BI201
      *
           MOVE    SPA-I22-PROC-FLG
                                       TO  BI201-PROC-FLG
           MOVE    SPA-I22-FILENAME
                                       TO  BI201-CSVFILENAME
           MOVE    SPA-I22-STPAGE
                                       TO  BI201-STPAGE
           MOVE    SPA-I22-EDPAGE
                                       TO  BI201-EDPAGE
           MOVE    SPA-I22-STKNS   TO  BI201-STKNS
           MOVE    SPA-I22-EDKNS   TO  BI201-EDKNS
      *
           MOVE    SPA-SYSYMD          TO  BI201-KJYNYMD
      *
           MOVE    SPA-NAI-SQL         TO  BI201-SQL
      *
           WRITE   BI201
      *
           CLOSE   BI201-FILE
      *
           MOVE    ZERO            TO  SPA-I23-FILESV-CHK-FLG
           MOVE    SPA-GMN-SELHMS  TO  SPA-I23-SELHMS
      *
           MOVE    SPACE           TO  WRK-FILENM-AREA
           PERFORM VARYING IDX4    FROM    100 BY  -1
                   UNTIL   IDX4     <       1
               IF      ( SPA-I22-FILENAME (IDX4:1)
                                               =   "/" )
                   MOVE    SPA-GMN-I22-FILENAME (1:IDX4)
                                           TO  WRK-FILENM-FOLDER
                   COMPUTE IDX5    =   IDX4    +   1
                   MOVE    SPA-GMN-I22-FILENAME (IDX5:)
                                           TO  WRK-FILENM
                   MOVE    1               TO  IDX4
               END-IF
           END-PERFORM
           IF      WRK-FILENM          =   SPACE
               MOVE    "/tmp/"             TO  WRK-FILENM-FOLDER
               MOVE    SPA-I22-FILENAME
                                           TO  WRK-FILENM
           END-IF             
      *
      *    ファイル保存情報削除
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    CONST-SHELLID   TO  SFILESV-TBL-KEY
           MOVE    SPA-GMN-SELHMS  TO  SFILESV-SRYYM(1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
      *
      *    ジョブ開始セット処理
           PERFORM 900-JOBKANRI-HENSYU-SEC
      *
      *    印刷用スクリプト処理
      *
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  SHELLTBL
      *
           MOVE    "orcbi201.sh"   TO  SHELLTBL-NAME
      *
           MOVE    SPA-I22-PROC-FLG
                                   TO  SHELLTBL-ARG1
           IF    ( SPA-I22-FILE   =   "1" )
               STRING   WRK-FILENM-FOLDER      DELIMITED  BY  SPACE
                        WRK-FILENM             DELIMITED  BY  SPACE
                                               INTO  SHELLTBL-ARG2
               END-STRING
           ELSE
               MOVE    SPA-I22-FILENAME
                                       TO  SHELLTBL-ARG2
           END-IF
           MOVE    ASGNPARA (ASGNPARA-ST:)
                                   TO  SHELLTBL-ARG3
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG4
           MOVE    SPA-SYSYMD      TO  SHELLTBL-ARG5
           MOVE    SPA-GMN-SELHMS  TO  SHELLTBL-ARG6
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG7
           MOVE    SPA-STAFFCD     TO  SHELLTBL-ARG8
           MOVE    CONST-JOBID     TO  SHELLTBL-ARG9
           MOVE    CONST-SHELLID   TO  SHELLTBL-ARG10
           MOVE    SPA-I22-FILE
                                   TO  SHELLTBL-ARG11
           MOVE    SPA-I22-CDKBN
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-FILENM-FOLDER
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-FILENM      TO  SHELLTBL-ARG14
           STRING  "I20.csv."              DELIMITED  BY  SIZE
                    SPA-TERMID             DELIMITED  BY  SIZE
                    ".wk"                  DELIMITED  BY  SIZE
                                           INTO  SHELLTBL-ARG15
           END-STRING
      *
           INITIALIZE                  ORCA-BLOB
           MOVE    ASGNPARA        TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBIMPORT-SEC
           MOVE    ORCA-BLOB-OBJECT
                                   TO  SHELLTBL-ARG16
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF    ( SPA-I22-FILE   =   "1" )
               MOVE    "SV"            TO  SPA-I2ID1-FLG
           ELSE
               MOVE    SPACE           TO  SPA-I2ID1-FLG
           END-IF
           MOVE    SPACE           TO  SPA-I2IDCD
      *
           PERFORM 440-I23-SENI-SEC
      *
           .
       3001-I22SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       3001-I23SPASET-SEC             SECTION.
      *
           IF    ( SPA-I2ID1-FLG    =   "SV" )
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "D"             TO  SFILESV-MODE
               MOVE    CONST-SHELLID   TO  SFILESV-TBL-KEY
               MOVE    SPA-GMN-SELHMS  TO  SFILESV-SRYYM(1)
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
      *
           MOVE    SPACE           TO  SPA-I2ID1-FLG
           MOVE    SPACE           TO  SPA-I2IDCD
           MOVE    SPACE           TO  SPA-I23-MOTOPG
      *
           .
       3001-I23SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE       TO  SPA-I2FREE
           INITIALIZE              SPA-I20-AREA
                                   SPA-I2KYOUTU
      *
      *    クライアント保存可否情報取得サブ
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"  USING   ORCSCLIENTSVAREA
                                           SPA-AREA
           MOVE    SCLIENTSV-CLIENT-SAVE
                                       TO  SPA-I20-CLIENT-SAVE
      *
           MOVE    "01"            TO  SPA-GMN-STAGEL        (1)
           MOVE    "02"            TO  SPA-GMN-STAGEL        (2)
           MOVE    "03"            TO  SPA-GMN-STAGEL        (3)
           MOVE    "04"            TO  SPA-GMN-STAGEL        (4)
           MOVE    "05"            TO  SPA-GMN-STAGEL        (5)
           MOVE    "06"            TO  SPA-GMN-STAGEL        (6)
           MOVE    "07"            TO  SPA-GMN-STAGEL        (7)
           MOVE    "08"            TO  SPA-GMN-STAGEL        (8)
           MOVE    "09"            TO  SPA-GMN-STAGEL        (9)
           MOVE    "10"            TO  SPA-GMN-STAGEL        (10)
           MOVE    "11"            TO  SPA-GMN-STAGEL        (11)
           MOVE    "12"            TO  SPA-GMN-STAGEL        (12)
           MOVE    "13"            TO  SPA-GMN-STAGEL        (13)
           MOVE    "14"            TO  SPA-GMN-STAGEL        (14)
           MOVE    "０歳"          TO  SPA-GMN-STAGEMEIL     (1)
           MOVE    "１０歳"        TO  SPA-GMN-STAGEMEIL     (2)
           MOVE    "２０歳"        TO  SPA-GMN-STAGEMEIL     (3)
           MOVE    "３０歳"        TO  SPA-GMN-STAGEMEIL     (4)
           MOVE    "４０歳"        TO  SPA-GMN-STAGEMEIL     (5)
           MOVE    "５０歳"        TO  SPA-GMN-STAGEMEIL     (6)
           MOVE    "６０歳"        TO  SPA-GMN-STAGEMEIL     (7)
           MOVE    "７０歳"        TO  SPA-GMN-STAGEMEIL     (8)
           MOVE    "８０歳"        TO  SPA-GMN-STAGEMEIL     (9)
           MOVE    "９０歳"        TO  SPA-GMN-STAGEMEIL     (10)
           MOVE    "１００歳"      TO  SPA-GMN-STAGEMEIL     (11)
           MOVE    "１１０歳"      TO  SPA-GMN-STAGEMEIL     (12)
           MOVE    "１２０歳"      TO  SPA-GMN-STAGEMEIL     (13)
           MOVE    "１３０歳"      TO  SPA-GMN-STAGEMEIL     (14)
           MOVE    14              TO  SPA-GMN-STAGE-MAX
      *
           MOVE    "01"            TO  SPA-GMN-EDAGEL        (1)
           MOVE    "02"            TO  SPA-GMN-EDAGEL        (2)
           MOVE    "03"            TO  SPA-GMN-EDAGEL        (3)
           MOVE    "04"            TO  SPA-GMN-EDAGEL        (4)
           MOVE    "05"            TO  SPA-GMN-EDAGEL        (5)
           MOVE    "06"            TO  SPA-GMN-EDAGEL        (6)
           MOVE    "07"            TO  SPA-GMN-EDAGEL        (7)
           MOVE    "08"            TO  SPA-GMN-EDAGEL        (8)
           MOVE    "09"            TO  SPA-GMN-EDAGEL        (9)
           MOVE    "10"            TO  SPA-GMN-EDAGEL        (10)
           MOVE    "11"            TO  SPA-GMN-EDAGEL        (11)
           MOVE    "12"            TO  SPA-GMN-EDAGEL        (12)
           MOVE    "13"            TO  SPA-GMN-EDAGEL        (13)
           MOVE    "14"            TO  SPA-GMN-EDAGEL        (14)
           MOVE    "９歳"          TO  SPA-GMN-EDAGEMEIL     (1)
           MOVE    "１９歳"        TO  SPA-GMN-EDAGEMEIL     (2)
           MOVE    "２９歳"        TO  SPA-GMN-EDAGEMEIL     (3)
           MOVE    "３９歳"        TO  SPA-GMN-EDAGEMEIL     (4)
           MOVE    "４９歳"        TO  SPA-GMN-EDAGEMEIL     (5)
           MOVE    "５９歳"        TO  SPA-GMN-EDAGEMEIL     (6)
           MOVE    "６９歳"        TO  SPA-GMN-EDAGEMEIL     (7)
           MOVE    "７９歳"        TO  SPA-GMN-EDAGEMEIL     (8)
           MOVE    "８９歳"        TO  SPA-GMN-EDAGEMEIL     (9)
           MOVE    "９９歳"        TO  SPA-GMN-EDAGEMEIL     (10)
           MOVE    "１０９歳"      TO  SPA-GMN-EDAGEMEIL     (11)
           MOVE    "１１９歳"      TO  SPA-GMN-EDAGEMEIL     (12)
           MOVE    "１２９歳"      TO  SPA-GMN-EDAGEMEIL     (13)
           MOVE    "１３９歳"      TO  SPA-GMN-EDAGEMEIL     (14)
           MOVE    14              TO  SPA-GMN-EDAGE-MAX
      *
      *
           INITIALIZE                      I2SUB03-AREA
           MOVE    SPA-SYSYMD      TO      I2SUB03-BASEYMD
           CALL    "ORCGI2SUB03"   USING   I2SUB03-AREA
                                           SPA-AREA
      *
           MOVE    I2SUB03-SRYKALST-MAX
                                   TO  SPA-GMN-SRYKA-MAX
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL ( IDX    >   SPA-GMN-SRYKA-MAX   )
                    OR   ( IDX    >   99   )
      *
               MOVE    I2SUB03-SRYKALST (IDX)
                                   TO      SPA-GMN-SRYKALST (IDX)
      *
           END-PERFORM
      *
           MOVE    I2SUB03-BTUNUMLST-MAX   TO  SPA-GMN-BTUNUM-MAX
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL ( IDX    >   SPA-GMN-BTUNUM-MAX   )
                    OR   ( IDX    >   50   )
      *
               MOVE    I2SUB03-BTUNUMLST (IDX)
                                   TO      SPA-GMN-BTUNUMLST (IDX)
      *
           END-PERFORM
      *
           MOVE    I2SUB03-JOTAILST-MAX   TO  SPA-GMN-JOTAI-MAX
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL ( IDX    >   SPA-GMN-JOTAI-MAX   )
                    OR   ( IDX    >   4   )
      *
               MOVE    I2SUB03-JOTAILST (IDX)
                                   TO      SPA-GMN-JOTAILST (IDX)
      *
           END-PERFORM
      *
           MOVE    I2SUB03-TAIINCDLST-MAX   TO  SPA-GMN-TAIINCD-MAX
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL ( IDX    >   SPA-GMN-TAIINCD-MAX   )
                    OR   ( IDX    >   50   )
      *
               MOVE    I2SUB03-TAIINCDLST (IDX)
                                   TO      SPA-GMN-TAIINCDLST (IDX)
      *
           END-PERFORM
      *
           MOVE    I2SUB03-DEATHKBNLST-MAX   TO  SPA-GMN-DEATHKBN-MAX
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL ( IDX    >   SPA-GMN-DEATHKBN-MAX   )
                    OR   ( IDX    >   2   )
      *
               MOVE    I2SUB03-DEATHKBNLST (IDX)
                                   TO      SPA-GMN-DEATHKBNLST (IDX)
      *
           END-PERFORM
      *
           MOVE    I2SUB03-SORTLST-MAX   TO  SPA-GMN-SRTKBN-MAX
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL ( IDX    >   SPA-GMN-SRTKBN-MAX   )
                    OR   ( IDX    >   7   )
      *
               MOVE    I2SUB03-SORTLST (IDX)
                                   TO      SPA-GMN-SRTKBNLST (IDX)
      *
           END-PERFORM
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR                                 
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-I2ID1-SET-SEC            SECTION.
      *
           IF      SPA-I2ID1-FLG    =   "OK"
               EVALUATE    SPA-I2IDCD
      *            WHEN    "1001"
      *                MOVE    1           TO  SPA-GMN-PAGE
      *                PERFORM 450-KENSAKU-SEC
      *                PERFORM 4504-COUNT-GET-SEC
                   WHEN    "6666"
                      MOVE    "JOIN"       TO  MCP-PUTTYPE
                      PERFORM 2101-BACK-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-I2ID1-FLG
           MOVE    SPACE           TO  SPA-I2IDCD
           .
       330-I2ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *    入退院登録画面
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-I01-SENI-SEC
      *    病棟別入院患者数
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 440-I21-SENI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-PAGE-BACK-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-PAGE-NEXT-SEC
      *    情報削除
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 440-XB01-SENI-SEC
      *    ＣＳＶ出力
               WHEN    "CLICKED"       ALSO    "B10"
                   MOVE    ZERO        TO  WRK-PROC-FLG
                   PERFORM 440-I22-SENI-SEC
      *    印刷
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    1           TO  WRK-PROC-FLG
                   PERFORM 440-I22-SENI-SEC
      *    予約登録へ
      ****     WHEN    "CLICKED"       ALSO    "B10"
      ****         PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
      ****     WHEN    "CLICKED"       ALSO    "B11"
      ****         PERFORM 490-UKEICHI-SEC
      *    検索開始
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KENSAKU-MAE-SEC
      *    処理結果画面
               WHEN    "CLICKED"       ALSO    "B12S"
                   MOVE    1           TO  WRK-PROC-FLG
                   PERFORM 440-I23-SENI-SEC
      *    テスト患者区分
               WHEN    "CLICKED"       ALSO    "CHKTESTPT"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *            入力個所のセット
                   PERFORM 400-GMN-INPUT-SEC
      *
                   MOVE    ZERO                TO  FLG-TOUROKU
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
      *    カナ入力             
               WHEN    OTHER       
                   IF      MCP-EVENT           =   "CLICKED" 
                       IF      MCP-WIDGET (1:2)    =   "BA"
                           IF      MCP-WIDGET (3:3)    NUMERIC
                               MOVE    MCP-WIDGET(3:3)     TO  PIN9
                               PERFORM 410111-PIN-SET-SEC
                           END-IF
                       END-IF
                       EVALUATE    MCP-WIDGET
                       WHEN    "BSP"
                           PERFORM 410114-SPACE-SEC
                       WHEN    "BSH"
                           PERFORM 410112-SHIFT-SEC
                       WHEN    "BAK"
                           PERFORM 410113-BACK-SEC
                       END-EVALUATE
                   END-IF
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    保険組合せ選択
               WHEN    ANY             ALSO    "KANJYALST"
                   PERFORM 400-KANJYALST-SEL-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 400-SELNUM-CHK-SEC
               WHEN    OTHER    
      *            入力個所のセット
                   PERFORM 400-GMN-INPUT-SEC
      *
                   MOVE    ZERO                TO  FLG-TOUROKU
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       400-KANJYALST-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    ZERO            TO  SPA-GMN-TBL-SEL
      *
           IF    ( SPA-GMN-MAX         <=  ZERO )
               GO  TO  400-KANJYALST-SEL-EXT
           END-IF
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               IF      I20-SELECT (IDX)    =   "T"
                   MOVE    IDX                  TO  SPA-GMN-TBL-SEL
                   MOVE    SPA-GMN-MAX          TO  IDX
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-TBL-SEL  >  ZERO )
               MOVE    SPA-GMN-TRENNUM (SPA-GMN-TBL-SEL)
                                       TO  SPA-GMN-SELNUM
           END-IF
      *
           .
       400-KANJYALST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       400-SELNUM-CHK-SEC         SECTION.
      *
           MOVE    ZERO           TO  SPA-GMN-TBL-SEL
      *
           MOVE    12             TO  SPA-GMN-CUR
      *
           MOVE    I20-SELNUM     TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM     =    ZERO )
               GO  TO  400-SELNUM-CHK-EXT
           END-IF
      *
           COMPUTE WRK-SELNUMST
               = ( SPA-GMN-PAGE  -   1 )
               *   CONST-GMN-MAX
               +   1
      *
           COMPUTE WRK-SELNUMED
               = ( SPA-GMN-PAGE  -   1 )
               *   CONST-GMN-MAX
               +   SPA-GMN-MAX
      *
           IF    ( SPA-GMN-SELNUM >=   WRK-SELNUMST )
            AND  ( SPA-GMN-SELNUM <=   WRK-SELNUMED )
               CONTINUE
           ELSE
               MOVE    "0010"          TO  SPA-ERRCD
               GO  TO  400-SELNUM-CHK-EXT
           END-IF
      *
           COMPUTE SPA-GMN-TBL-SEL
               =   SPA-GMN-SELNUM
               - ( SPA-GMN-PAGE  - 1 )
               *   CONST-GMN-MAX
      *
           .
       400-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           IF    ( SPA-ERRCD       =   SPACE  )
               PERFORM 4101-GMN-SPA-SET-SEC
           END-IF
      *
      *    基本チェック
           IF    ( SPA-ERRCD       =   SPACE  )
               PERFORM 4102-KIHON-CHK-SEC
           END-IF
      *     
      *    関連チェック
           IF    ( SPA-ERRCD       =   SPACE  )
               PERFORM 4103-KANREN-CHK-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    I20-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    I20-NAME            TO  SPA-GMN-NAME
           MOVE    I20-STAGE           TO  SPA-GMN-STAGE-G
           MOVE    I20-EDAGE           TO  SPA-GMN-EDAGE-G
           MOVE    I20-ADRS            TO  SPA-GMN-ADRS
           MOVE    I20-STNYUINYMD      TO  SPA-GMN-STNYUINYMD
           MOVE    I20-EDNYUINYMD      TO  SPA-GMN-EDNYUINYMD
           MOVE    I20-STTAIINYMD      TO  SPA-GMN-STTAIINYMD
           MOVE    I20-EDTAIINYMD      TO  SPA-GMN-EDTAIINYMD
           MOVE    I20-SRYKA           TO  SPA-GMN-SRYKA-G
           MOVE    I20-BTUNUM          TO  SPA-GMN-BTUNUM-G
           MOVE    I20-JOTAI           TO  SPA-GMN-JOTAI-G
           MOVE    I20-CHKTESTPT       TO  SPA-GMN-CHKTESTPT
           MOVE    I20-TAIINCD         TO  SPA-GMN-TAIINCD-G
           MOVE    I20-DEATHKBN        TO  SPA-GMN-DEATHKBN-G
           MOVE    I20-SRT1KBN         TO  SPA-GMN-SRT1KBN-G
           MOVE    I20-SRT2KBN         TO  SPA-GMN-SRT2KBN-G
           MOVE    I20-SRT3KBN         TO  SPA-GMN-SRT3KBN-G
      *
      *****MOVE    I20-SELNUM          TO  SPA-GMN-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      * 
      *    患者番号
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41021-PTNUM-CHK-SEC
           END-IF
      *    患者氏名         
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41022-NAME-CHK-SEC
           END-IF
      *    開始年齢
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41023-STAGE-CHK-SEC
           END-IF
      *    終了年齢
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41029-EDAGE-CHK-SEC
           END-IF
      *    住所         
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41024-ADRS-CHK-SEC
           END-IF
      *    開始入院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41025-STNYUINYMD-CHK-SEC
           END-IF
      *    終了入院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41026-EDNYUINYMD-CHK-SEC
           END-IF
      *    開始退院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41025-STTAIINYMD-CHK-SEC
           END-IF
      *    終了退院日
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41026-EDTAIINYMD-CHK-SEC
           END-IF
      *    診療科
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41027-SRYKA-CHK-SEC
           END-IF
      *    病棟         
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41028-BTUNUM-CHK-SEC
           END-IF
      *    状態
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41029-JOTAI-CHK-SEC
           END-IF
      *    テスト患者
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41030-CHKTESTPT-CHK-SEC
           END-IF
      *    退院事由
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41030-TAIINCD-CHK-SEC
           END-IF
      *    死亡区分
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41030-DEATHKBN-CHK-SEC
           END-IF
      *    並び順１
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41031-SRT1KBN-CHK-SEC
           END-IF
      *    並び順２
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41031-SRT2KBN-CHK-SEC
           END-IF
      *    並び順３
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41031-SRT3KBN-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41021-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-PTNUM       =   SPACE
      *        CONTINUE
               MOVE    ZERO            TO  SPA-NAI-PTID
           ELSE     
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                                   SPA-AREA
               MOVE    SPTNUM-PTNUM    TO  SPA-GMN-PTNUM
      *
               IF      SPTNUM-RC           =   ZERO 
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPA-NAI-PTID
      *            患者マスター存在チェック
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-PTID        TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF           NOT =   ZERO
                       MOVE    "0001"          TO  SPA-ERRCD
                   ELSE
                       MOVE    PTINF-NAME      TO  SPA-GMN-NAME
                   END-IF
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
           . 
       41021-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名チェック処理
      *****************************************************************
       41022-NAME-CHK-SEC             SECTION.
      *
           MOVE    2                    TO  SPA-GMN-CUR
      * 
           MOVE    ZERO                 TO  SPA-NAI-NAME-MAX
      *
           IF      SPA-GMN-NAME         =   SPACE
               GO  TO  41022-NAME-CHK-EXT
           END-IF
      *
      *    半角チェック
      *
           IF    ( SPA-ENCODING    =   CONST-UTF8 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"                 TO  KANACHK2-SYORI 
               MOVE    SPA-GMN-NAME        TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"      USING   ORCSKANACHK2AREA
               MOVE    KANACHK2-OUT-INPUT   TO  SPA-GMN-NAME
               IF      KANACHK2-RC          =   ZERO
               AND     KANACHK2-SYUBETU     =   "2"
                   MOVE    KANACHK2-OUT-INPUT   TO  SPA-GMN-NAME
               ELSE
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-NAME        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"      USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
               ELSE
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               PERFORM VARYING IDX FROM    15  BY  -1
                       UNTIL   IDX <       1
                   IF      SPA-GMN-NAME-X (IDX)    NOT =   SPACE
                       MOVE    IDX             TO  SPA-NAI-NAME-MAX
                       MOVE    1               TO  IDX
                   END-IF
               END-PERFORM
           END-IF            
                                       
           .
       41022-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始年齢チェック処理
      *****************************************************************
       41023-STAGE-CHK-SEC                SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-STAGE         =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-STAGE-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-STAGE 
                                  =   SPA-GMN-STAGEL (IDY)
                       MOVE    "1"        TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0003"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41023-STAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了年齢チェック処理
      *****************************************************************
       41029-EDAGE-CHK-SEC                SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-EDAGE         =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-STAGE-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-EDAGE 
                                  =   SPA-GMN-STAGEL (IDY)
                       MOVE    "1"        TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0015"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41029-EDAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       41024-ADRS-CHK-SEC             SECTION.
      *
           MOVE    5                    TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-ADRS         =   SPACE
               GO  TO  41024-ADRS-CHK-EXT
           END-IF
      *
      *    半角チェック
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    SPA-GMN-ADRS        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-ADRS
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-SYUBETU     =   "2"
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-ADRS
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
           .
       41024-ADRS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始入院日チェック処理
      *****************************************************************
       41025-STNYUINYMD-CHK-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
           IF     SPA-GMN-STNYUINYMD   =   SPACE
               MOVE    "00000000"      TO  SPA-NAI-STNYUINYMD
               GO  TO  41025-STNYUINYMD-CHK-EXT
           END-IF
      *
           IF     SPA-GMN-STNYUINYMD (1:3) =   "000"
               MOVE    "00000000"      TO  SPA-GMN-STNYUINYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-STNYUINYMD  TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-STNYUINYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-STNYUINYMD
               ELSE
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       41025-STNYUINYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了入院日チェック処理
      *****************************************************************
       41026-EDNYUINYMD-CHK-SEC         SECTION.
      *
           MOVE    7                    TO  SPA-GMN-CUR
      *
           IF     SPA-GMN-EDNYUINYMD    =   SPACE
               MOVE    "99999999"       TO  SPA-NAI-EDNYUINYMD
               GO  TO  41026-EDNYUINYMD-CHK-EXT
           END-IF
      *
           IF     SPA-GMN-EDNYUINYMD (1:3) =   "999"
               MOVE    "99999999"       TO  SPA-GMN-EDNYUINYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-EDNYUINYMD  TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-EDNYUINYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-EDNYUINYMD
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       41026-EDNYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始退院日チェック処理
      *****************************************************************
       41025-STTAIINYMD-CHK-SEC         SECTION.
      *
           MOVE    13                   TO  SPA-GMN-CUR
      *
           MOVE    "00000000"           TO  SPA-NAI-STTAIINYMD
      *
           IF     SPA-GMN-STTAIINYMD    =   SPACE
               GO  TO  41025-STTAIINYMD-CHK-EXT
           END-IF
      *
           IF     SPA-GMN-STTAIINYMD (1:3)  =   "000"
               MOVE    "00000000"       TO  SPA-GMN-STTAIINYMD
           ELSE
               IF     SPA-GMN-STTAIINYMD (1:3)  =   "999"
                   MOVE    "99999999"   TO  SPA-GMN-STTAIINYMD
                   MOVE    "99999999"   TO  SPA-NAI-STTAIINYMD
               ELSE    
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    SPA-GMN-STTAIINYMD  TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY     TO  SPA-GMN-STTAIINYMD
                       MOVE    SGDAY-SDAY      TO  SPA-NAI-STTAIINYMD
                   ELSE
                       MOVE    "0011"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF    
           .
       41025-STTAIINYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了退院日チェック処理
      *****************************************************************
       41026-EDTAIINYMD-CHK-SEC         SECTION.
      *
           MOVE    14                   TO  SPA-GMN-CUR
      *
           MOVE    "99999999"           TO  SPA-NAI-EDTAIINYMD
      *
           IF     SPA-GMN-EDTAIINYMD    =   SPACE
               GO  TO  41026-EDTAIINYMD-CHK-EXT
           END-IF
      *
           IF     SPA-GMN-EDTAIINYMD (1:3) =   "999"
               MOVE    "99999999"       TO  SPA-GMN-EDTAIINYMD
           ELSE    
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-EDTAIINYMD  TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-EDTAIINYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-EDTAIINYMD
               ELSE
                   MOVE    "0012"          TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       41026-EDTAIINYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41027-SRYKA-CHK-SEC             SECTION.
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-SRYKA       =   SPACE
               CONTINUE
           ELSE    
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-SRYKA-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-SRYKA 
                                      =   SPA-GMN-SRYKAL (IDY)
                       MOVE    SPA-GMN-SRYKALST (IDY)
                                          TO  SPA-GMN-SRYKA-G
                       MOVE    "1"        TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0007"               TO  SPA-ERRCD
               END-IF
           END-IF            
           .
       41027-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟入力処理
      *****************************************************************
       41028-BTUNUM-CHK-SEC             SECTION.
      *
           MOVE    9                   TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-BTUNUM      =   SPACE
               CONTINUE
           ELSE    
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-BTUNUM-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-BTUNUM 
                                      =   SPA-GMN-BTUNUML (IDY)
                       MOVE    SPA-GMN-BTUNUMLST (IDY)
                                         TO  SPA-GMN-BTUNUM-G
                       MOVE    "1"       TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0008"               TO  SPA-ERRCD
               END-IF
           END-IF            
           .
       41028-BTUNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態入力処理
      *****************************************************************
       41029-JOTAI-CHK-SEC             SECTION.
      *
           MOVE    10                  TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-JOTAI      =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-JOTAI-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-JOTAI 
                                      =   SPA-GMN-JOTAIL (IDY)
                       MOVE    SPA-GMN-JOTAILST (IDY)
                                         TO  SPA-GMN-JOTAI-G
                       MOVE    "1"       TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0009"               TO  SPA-ERRCD
               END-IF
           END-IF
           .
       41029-JOTAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者入力処理
      *****************************************************************
       41030-CHKTESTPT-CHK-SEC         SECTION.
      *
      *
           .
       41030-CHKTESTPT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由入力処理
      *****************************************************************
       41030-TAIINCD-CHK-SEC             SECTION.
      *
           MOVE    16                   TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-TAIINCD      =   SPACE
               CONTINUE
           ELSE    
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-TAIINCD-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-TAIINCD 
                                      =   SPA-GMN-TAIINCDL (IDY)
                       MOVE    SPA-GMN-TAIINCDLST (IDY)
                                         TO  SPA-GMN-TAIINCD-G
                       MOVE    "1"       TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0013"               TO  SPA-ERRCD
               END-IF
           END-IF            
           .
       41030-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死亡区分入力処理
      *****************************************************************
       41030-DEATHKBN-CHK-SEC          SECTION.
      *
           MOVE    19                  TO  SPA-GMN-CUR
      *
           IF      SPA-GMN-DEATHKBN    =   SPACE
               CONTINUE
           ELSE    
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-DEATHKBN-MAX
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-DEATHKBN 
                                      =   SPA-GMN-DEATHKBNL (IDY)
                       MOVE    SPA-GMN-DEATHKBNLST (IDY)
                                         TO  SPA-GMN-DEATHKBN-G
                       MOVE    "1"       TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG         =   ZERO
                   MOVE    "0014"               TO  SPA-ERRCD
               END-IF
           END-IF            
           .
       41030-DEATHKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順区分チェック処理
      *****************************************************************
       41031-SRT1KBN-CHK-SEC           SECTION.
      *
           MOVE    15                  TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-SRT1KBN     TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRT1KBN-G
      *
           .
       41031-SRT1KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順区分チェック処理
      *****************************************************************
       41031-SRT2KBN-CHK-SEC           SECTION.
      *
           MOVE    17                  TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-SRT2KBN     TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRT2KBN-G
      *
           .
       41031-SRT2KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順区分チェック処理
      *****************************************************************
       41031-SRT3KBN-CHK-SEC           SECTION.
      *
           MOVE    18                  TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-SRT3KBN     TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRT3KBN-G
      *
           .
       41031-SRT3KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4103-KANREN-CHK-SEC             SECTION.
      *
      *    入院日の関連チェック
           PERFORM 41031-KANREN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE 
               GO      TO      4103-KANREN-CHK-EXT
           END-IF
      *
      *    退院日の関連チェック
           PERFORM 41032-KANREN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE 
               GO      TO      4103-KANREN-CHK-EXT
           END-IF
           .
      *
       4103-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院日の関連チェック
      *****************************************************************
       41031-KANREN-CHK-SEC           SECTION.
      * 
           IF      SPA-NAI-STNYUINYMD    =   ZERO
           OR      SPA-NAI-EDNYUINYMD    =   ZERO
               CONTINUE
           ELSE
               IF      SPA-NAI-STNYUINYMD    <=   SPA-NAI-EDNYUINYMD
                   CONTINUE
               ELSE
                   MOVE    "0101"        TO  SPA-ERRCD
               END-IF   
           END-IF
           .
       41031-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院日の関連チェック
      *****************************************************************
       41032-KANREN-CHK-SEC           SECTION.
      * 
           IF      SPA-NAI-STTAIINYMD    =   ZERO
           OR      SPA-NAI-EDTAIINYMD    =   ZERO
               CONTINUE
           ELSE
               IF      SPA-NAI-STTAIINYMD    <=   SPA-NAI-EDTAIINYMD
                   CONTINUE
               ELSE
                   MOVE    "0102"        TO  SPA-ERRCD
               END-IF   
           END-IF
           .
       41032-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字セット　処理
      *****************************************************************
       410111-PIN-SET-SEC              SECTION.
      *
           MOVE    PIN10               TO  IDX1
           MOVE    PIN1                TO  IDX2
      *
           MOVE    PKANA(IDX1 IDX2)    TO  WRK-PKANA
           IF     (WRK-PKANA           =   "゜"  ) AND
                  (SPA-NAI-NAME-MAX         >   0     )
               EVALUATE    SPA-GMN-NAME-X (SPA-NAI-NAME-MAX)
                   WHEN    "ハ"
                       MOVE    "パ"            TO  WRK-PKANA
                   WHEN    "ヒ"
                       MOVE    "ピ"            TO  WRK-PKANA
                   WHEN    "フ"
                       MOVE    "プ"            TO  WRK-PKANA
                   WHEN    "ヘ"
                       MOVE    "ペ"            TO  WRK-PKANA
                   WHEN    "ホ"
                       MOVE    "ポ"            TO  WRK-PKANA
                   WHEN    OTHER
                       ADD    1                TO  SPA-NAI-NAME-MAX
               END-EVALUATE
           ELSE
               IF    (WRK-PKANA        =   "゛" ) AND
                     (SPA-NAI-NAME-MAX      >   0    )
                   EVALUATE    SPA-GMN-NAME-X (SPA-NAI-NAME-MAX)
                       WHEN    "ウ"
                           MOVE    "ヴ"            TO  WRK-PKANA
                       WHEN    "カ"
                           MOVE    "ガ"            TO  WRK-PKANA
                       WHEN    "キ"
                           MOVE    "ギ"            TO  WRK-PKANA
                       WHEN    "ク"
                           MOVE    "グ"            TO  WRK-PKANA
                       WHEN    "ケ"
                           MOVE    "ゲ"            TO  WRK-PKANA
                       WHEN    "コ"
                           MOVE    "ゴ"            TO  WRK-PKANA
                       WHEN    "サ"
                           MOVE    "ザ"            TO  WRK-PKANA
                       WHEN    "シ"
                           MOVE    "ジ"            TO  WRK-PKANA
                       WHEN    "ス"
                           MOVE    "ズ"            TO  WRK-PKANA
                       WHEN    "セ"
                           MOVE    "ゼ"            TO  WRK-PKANA
                       WHEN    "ソ"
                           MOVE    "ゾ"            TO  WRK-PKANA
                       WHEN    "タ"
                           MOVE    "ダ"            TO  WRK-PKANA
                       WHEN    "チ"
                           MOVE    "ヂ"            TO  WRK-PKANA
                       WHEN    "ツ"
                           MOVE    "ヅ"            TO  WRK-PKANA
                       WHEN    "テ"
                           MOVE    "デ"            TO  WRK-PKANA
                       WHEN    "ト"
                           MOVE    "ド"            TO  WRK-PKANA
                       WHEN    "ハ"
                           MOVE    "バ"            TO  WRK-PKANA
                       WHEN    "ヒ"
                           MOVE    "ビ"            TO  WRK-PKANA
                       WHEN    "フ"
                           MOVE    "ブ"            TO  WRK-PKANA
                       WHEN    "ヘ"
                           MOVE    "ベ"            TO  WRK-PKANA
                       WHEN    "ホ"
                           MOVE    "ボ"            TO  WRK-PKANA
                       WHEN    OTHER
                           ADD    1                TO  SPA-NAI-NAME-MAX
                   END-EVALUATE
               ELSE
                   ADD     1               TO  SPA-NAI-NAME-MAX
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-NAME-MAX    >   CONST-NAME-MAX )
               MOVE    CONST-NAME-MAX      TO  SPA-NAI-NAME-MAX
           ELSE
               MOVE    WRK-PKANA           TO  SPA-GMN-NAME-X
                                                (SPA-NAI-NAME-MAX)
           END-IF
           .
       410111-PIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    小文字変換　処理
      *****************************************************************
       410112-SHIFT-SEC                    SECTION.
      *
           IF      SPA-NAI-NAME-MAX         =   0
               GO      TO      410112-SHIFT-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PKANA
           EVALUATE    SPA-GMN-NAME-X (SPA-NAI-NAME-MAX)
               WHEN    "ア"
                   MOVE    "ァ"            TO  WRK-PKANA
               WHEN    "イ"
                   MOVE    "ィ"            TO  WRK-PKANA
               WHEN    "ウ"
                   MOVE    "ゥ"            TO  WRK-PKANA
               WHEN    "エ"
                   MOVE    "ェ"            TO  WRK-PKANA
               WHEN    "オ"
                   MOVE    "ォ"            TO  WRK-PKANA
               WHEN    "カ"
                   MOVE    "ヵ"            TO  WRK-PKANA
               WHEN    "ツ"
                   MOVE    "ッ"            TO  WRK-PKANA
               WHEN    "ヤ"
                   MOVE    "ャ"            TO  WRK-PKANA
               WHEN    "ユ"
                   MOVE    "ュ"            TO  WRK-PKANA
               WHEN    "ヨ"
                   MOVE    "ョ"            TO  WRK-PKANA
               WHEN    OTHER
                   PERFORM VARYING IDX1    FROM    12  BY  1
                           UNTIL   IDX1    >       17
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL   IDX2    >       5
                           IF      PKANA (IDX1 IDX2)   =   SPACE
                               CONTINUE
                           ELSE
                               IF      SPA-GMN-NAME-X (SPA-NAI-NAME-MAX)
                                           =   PKANA (IDX1 IDX2)
                                   MOVE    PKANA (IDX1 IDX2 + 5 )
                                                   TO  WRK-PKANA
                                   MOVE    5       TO  IDX2
                                   MOVE    17      TO  IDX1
                               END-IF
                           END-IF
                       END-PERFORM
                   END-PERFORM                                                       
           END-EVALUATE
      *
           IF      WRK-PKANA       NOT =   SPACE
               MOVE    WRK-PKANA           TO  SPA-GMN-NAME-X
                                                (SPA-NAI-NAME-MAX)
           END-IF
           .
       410112-SHIFT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    後退　処理
      *****************************************************************
       410113-BACK-SEC                    SECTION.
      *
      *    前の桁を削除
           IF      SPA-NAI-NAME-MAX        >   0
               MOVE    SPACE               TO
                                       SPA-GMN-NAME-X(SPA-NAI-NAME-MAX)
               COMPUTE SPA-NAI-NAME-MAX    =   SPA-NAI-NAME-MAX   -   1
           END-IF
           .
       410113-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    空白　処理
      *****************************************************************
       410114-SPACE-SEC                    SECTION.
      *
           IF    ( SPA-NAI-NAME-MAX    <   CONST-NAME-MAX )
               COMPUTE SPA-NAI-NAME-MAX
                   =   SPA-NAI-NAME-MAX    +   1
               MOVE    "　"    TO  SPA-GMN-NAME-X (SPA-NAI-NAME-MAX)
           ELSE
               MOVE    CONST-NAME-MAX      TO  SPA-NAI-NAME-MAX
           END-IF
           .
       410114-SPACE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ソート区分コンボボックスチェック
      *****************************************************************
       4200-SRTKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRTKBNL (IDX-CMB)   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SRTKBNLST (IDX-CMB)
                                           TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "6666"          TO  SPA-I2IDCD
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  210-BACK-EXT
           END-IF
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           PERFORM 2101-BACK-SYORI-SEC
      *
           .
       210-BACK-EXT.
           EXIT.
      *    
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       2101-BACK-SYORI-SEC              SECTION.
      *
           PERFORM 900-PTSRH-DEL3-SEC
      * 
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "I20"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2101-BACK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           MOVE    SPA-GMN-SRT1KBN     TO  WRK-SRT1KBN
           MOVE    SPA-GMN-SRT2KBN     TO  WRK-SRT2KBN
           MOVE    SPA-GMN-SRT3KBN     TO  WRK-SRT3KBN
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-GMN-TBL
                                       SPA-NAI-KOUMOKU
                                       SPA-NAI-TBL
                                       SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
                                           SPA-NAI-NAME-MAX                            
      *
           MOVE   "00000000"           TO  SPA-NAI-STNYUINYMD
           MOVE   "99999999"           TO  SPA-NAI-EDNYUINYMD
      *
           MOVE   "00000000"           TO  SPA-NAI-STTAIINYMD
           MOVE   "99999999"           TO  SPA-NAI-EDTAIINYMD
      *
           MOVE   "99999999"           TO  SPA-GMN-EDTAIINYMD
      *
           MOVE    "F"                 TO  SPA-GMN-CHKTESTPT
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    WRK-SRT1KBN         TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRT1KBN-G
      *
           MOVE    WRK-SRT2KBN         TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRT2KBN-G
      *
           MOVE    WRK-SRT3KBN         TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRT3KBN-G
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *    
      *****************************************************************
      *    入退院登録画面へ
      *****************************************************************
       440-I01-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    選択番号チェック
           PERFORM 400-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  440-I01-SENI-EXT
           END-IF
      *
      *    選択されていないとき
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    "0104"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
               GO  TO  440-I01-SENI-EXT
           END-IF
      *
           MOVE    SPA-GMN-TBL-SEL         TO  IDX4
      *
           INITIALIZE                      SPA-KYOTUKEY
                                           SPA-KYOTU-SET
      *
           MOVE    SPA-GMN-TPTNUM  (IDX4)
                                           TO  SPA-PTNUM
           MOVE    SPA-NAI-TPTID   (IDX4)
                                           TO  SPA-PTID
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I2SUB01-LNK
           MOVE    "O"                 TO  I2SUB01-KBN
           MOVE    SPA-I2FREE          TO  I2SUB01-SCRSPA-AREA
           MOVE    SPA-I2KYOUTU        TO  I2SUB01-COMMON
      *
           CALL    "ORCGI2SUB01"       USING   I2SUB01-LNK
                                               SPA-AREA
      *
      *    入退院登録画面へ遷移する
           MOVE    "I20"               TO  SPA-MOTOPG
           MOVE    "I01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I2KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I2FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "I01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       440-I01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    入退院登録画面へ
      *****************************************************************
       440-I21-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPA-I2FREE      TO  SPA-FREE
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE  "I20"             TO  SPA-MOTOPG
           MOVE  "I21"             TO  SPA-SAKIPG
      *
           MOVE  "CHANGE"          TO  MCP-PUTTYPE
           MOVE  "I21"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
      *
           .
       440-I21-SENI-EXT.
           EXIT.
      *************************************************************
      *    前頁処理
      *************************************************************
       440-PAGE-BACK-SEC               SECTION.
      *
           IF    ( SPA-GMN-PAGE   <=   1 )
               GO  TO  440-PAGE-BACK-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE
               =   SPA-GMN-PAGE   -   1
      *
           PERFORM 450-KENSAKU-SEC
      *
           .
       440-PAGE-BACK-EXT.
           EXIT.
      *************************************************************
      *    次頁処理
      *************************************************************
       440-PAGE-NEXT-SEC               SECTION.
      *
           IF    ( SPA-GMN-TBL-NEXT    NOT =   1 )
               GO  TO  440-PAGE-NEXT-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE
               =   SPA-GMN-PAGE    +   1
      *
           PERFORM 450-KENSAKU-SEC
      *
           .
       440-PAGE-NEXT-EXT.
           EXIT.
      *************************************************************
      *    情報削除画面遷移処理
      *************************************************************
       440-XB01-SENI-SEC               SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
      *
      *    画面移行用のパラメタ変更
           MOVE    "I20"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-SHELLID       TO  SPA-JOB-SHELLID
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I2KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I2FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       440-XB01-SENI-EXT.
           EXIT.
      *************************************************************
      *    印刷指示画面遷移処理
      *************************************************************
       440-I22-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =  ZERO )
               MOVE    "7777"      TO  SPA-ERRCD
               MOVE    SPACE       TO  WRK-SHORIERR
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  440-I22-SENI-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-I22-SENI-AREA
           INITIALIZE                  SPA-I22-SENI-AREA
      *
           MOVE    WRK-PROC-FLG    TO  SPA-I22-PROC-FLG
           MOVE    1               TO  SPA-I22-STNUM
      *
           IF  ( WRK-PROC-FLG      =   ZERO )
               MOVE    SPA-NAI-TOTAL
                                   TO  SPA-I22-EDNUM
               MOVE    "OUTPUT.csv"
                                   TO  SPA-I22-FILENAME
           ELSE
               COMPUTE SPA-I22-EDNUM
                   =   SPA-NAI-TOTAL   /   CONST-LINE  +   0.99
           END-IF
      *
           MOVE    "I20"           TO  SPA-I22-MOTOPG
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI22"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      *
           MOVE  "B12"             TO  MCP-WIDGET
      *
           MOVE  "I20"             TO  SPA-MOTOPG
           MOVE  "I22"             TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "I22"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
           .
      *
       440-I22-SENI-EXT.
           EXIT.
      *************************************************************
      *    処理結果確認画面遷移処理
      *************************************************************
       440-I23-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡにセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                  SPA-I23-AREA
      *
           MOVE    "I20"           TO  SPA-I23-MOTOPG
           MOVE    CONST-SHELLID   TO  SPA-I23-SHELLID
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI23"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      *
           MOVE  "B01"             TO  MCP-WIDGET
      *
           MOVE  "I20"             TO  SPA-MOTOPG
           MOVE  "I23"             TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "I23"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
           .
      *
       440-I23-SENI-EXT.
           EXIT.
      *****************************************************************
      *    検索前処理
      *****************************************************************
       450-KENSAKU-MAE-SEC                SECTION.
      *         
      *    入力個所のセット
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1           TO  SPA-GMN-PAGE
               MOVE    SPACE       TO  SPA-GMN-SELYMD
                                       SPA-GMN-SELHMS
      *
               PERFORM 800-MCNDATE-SEC
               PERFORM 4501-KENSAKU-SEC
               PERFORM 450-KENSAKU-SEC
               PERFORM 4504-COUNT-GET-SEC
               IF    ( SPA-NAI-TOTAL   >   ZERO )
                   MOVE    SMCNDATE-YMD    TO  SPA-GMN-SELYMD
                   MOVE    SMCNDATE-HMS    TO  SPA-GMN-SELHMS
               END-IF
           END-IF
           .
       450-KENSAKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       450-KENSAKU-SEC                SECTION.
      *
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-NAI-TBL
                                       SPA-NAI-SQL
                                       SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           COMPUTE WRK-LIMIT
               =   CONST-GMN-MAX   +   1
      *
           COMPUTE WRK-OFFSET
               = ( SPA-GMN-PAGE -  1 )
               *   CONST-GMN-MAX
      *
           PERFORM 900-PTSRH-ODR-SEL-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX             >   CONST-GMN-MAX
                   OR      FLG-PTSRH   NOT =   ZERO
      *
               PERFORM 4502-LIST-HENSYU-SEC
      *
               PERFORM 900-PTSRH-ODR-FET-SEC
      *
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "odr"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    "対象の患者はありません"
                                           TO  SPA-GMN-TNAME (1)
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    12                  TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( FLG-PTSRH               =   ZERO )
               MOVE    1                   TO  SPA-GMN-TBL-NEXT
           END-IF
      *
           .
       450-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       4501-KENSAKU-SEC                SECTION.
      *
           INITIALIZE                      I2SUB02-AREA
      *
           MOVE    SPA-NAI-PTID        TO  I2SUB02-PTID
      *
           MOVE    SPA-GMN-NAME        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-NAME
      *
           MOVE    SPA-GMN-ADRS        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-ADRS
      *
           MOVE    SPA-NAI-STNYUINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-STNYUINYMD
      *
           MOVE    SPA-NAI-EDNYUINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-EDNYUINYMD
      *
           MOVE    SPA-NAI-STTAIINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-STTAIINYMD
      *
           MOVE    SPA-NAI-EDTAIINYMD  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
      *
           MOVE    PTSRH-DBSTR         TO  I2SUB02-EDTAIINYMD
           MOVE    SPA-GMN-SRYKA       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-SRYKA
      *
      *    開始年齢
           IF    ( SPA-GMN-STAGE       =   SPACE )
               MOVE    ZERO            TO  I2SUB02-STAGE
           ELSE
               MOVE    SPA-GMN-STAGE   TO  IDX
               MOVE    WRK-CONS-STAGE (IDX)
                                       TO  I2SUB02-STAGE
           END-IF
      *
      *    終了年齢
           IF    ( SPA-GMN-EDAGE       =   SPACE )
               MOVE    999             TO  I2SUB02-EDAGE
           ELSE
               MOVE    SPA-GMN-EDAGE   TO  IDX
               MOVE    WRK-CONS-EDAGE (IDX)
                                       TO  I2SUB02-EDAGE
           END-IF
      *
           MOVE    SPA-GMN-BTUNUM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-BTUNUM
      *
           MOVE    SPA-GMN-JOTAI       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-JOTAI
      *
           IF    ( SPA-GMN-CHKTESTPT   =   "T" )
               MOVE    "b"             TO  I2SUB02-TSTPT
           ELSE
               MOVE    "a"             TO  I2SUB02-TSTPT
           END-IF
      *
           MOVE    SPA-GMN-TAIINCD     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-TAIINCD
      *
           MOVE    SPA-GMN-DEATHKBN    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  I2SUB02-DEATHKBN
      *
           IF    ( SPA-GMN-SRT1KBN     NOT =   SPACE )
               MOVE    SPA-GMN-SRT1KBN TO  I2SUB02-SORT-CD (1)
               MOVE    SPA-GMN-SRT1KBN TO  WRK-SRTKBN
               PERFORM 45031-SQL-ORDER-SEC
               MOVE    WRK-ORDER       TO  I2SUB02-SORT-ORDER (1)
           END-IF
      *
           IF    ( SPA-GMN-SRT2KBN     NOT =   SPACE )
               MOVE    SPA-GMN-SRT2KBN TO  I2SUB02-SORT-CD (2)
               MOVE    SPA-GMN-SRT2KBN TO  WRK-SRTKBN
               PERFORM 45031-SQL-ORDER-SEC
               MOVE    WRK-ORDER       TO  I2SUB02-SORT-ORDER (2)
           END-IF
      *
           IF    ( SPA-GMN-SRT3KBN     NOT =   SPACE )
               MOVE    SPA-GMN-SRT3KBN TO  I2SUB02-SORT-CD (3)
               MOVE    SPA-GMN-SRT3KBN TO  WRK-SRTKBN
               PERFORM 45031-SQL-ORDER-SEC
               MOVE    WRK-ORDER       TO  I2SUB02-SORT-ORDER (3)
           END-IF
           MOVE    SMCNDATE-YMD        TO  I2SUB02-SELYMD
           MOVE    SMCNDATE-HMS        TO  I2SUB02-SELHMS
           MOVE    SPACE               TO  I2SUB02-ORCA-UID
      *
           CALL    "ORCGI2SUB02"       USING
                                       I2SUB02-AREA
                                       I001-REC
                                       SPA-AREA
           DISPLAY "I2SUB02-RC[" I2SUB02-RC "]"
      *
           .
       4501-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名画面編集処理
      *****************************************************************
       4502-LIST-HENSYU-SEC        SECTION.
      *
           INITIALIZE                  I2SUB02-AREA
           MOVE    "I"             TO  I2SUB02-MODE
           MOVE    PTSRH-MEMO      TO  I2SUB02-MEMO
           CALL    "ORCGI2SUB02"       USING
                                       I2SUB02-AREA
                                       I001-REC
                                       SPA-AREA
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
      *
           MOVE    1               TO  FLG-GMN-INIT
      *
           MOVE    IDX             TO  SPA-GMN-MAX
      *
           COMPUTE SPA-GMN-TRENNUM (IDX) 
               = ( SPA-GMN-PAGE   -   1 )
               *   CONST-GMN-MAX
               +   IDX
      *
           MOVE    I001-PTID       TO  SPA-NAI-TPTID   (IDX) 
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    I001-PTID       TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        =   ZERO
               MOVE    SPTID-PTNUM         TO  SPA-GMN-TPTNUM  (IDX)
           END-IF
           MOVE    I001-KANANAME   TO  SPA-NAI-TKANANAME (IDX)
           MOVE    I001-NAME       TO  SPA-GMN-TNAME   (IDX)
           EVALUATE    I001-SEX
               WHEN    "1"
                   MOVE    "男"        TO  SPA-GMN-TSEX    (IDX)
               WHEN    "2"
                   MOVE    "女"        TO  SPA-GMN-TSEX    (IDX)
           END-EVALUATE
      *
           IF  (   I001-BIRTHDAY   <   SPA-SYSYMD )
               MOVE    I001-BIRTHDAY   TO  WRK-CALC-YMD1
               MOVE    SPA-SYSYMD      TO  WRK-CALC-YMD2
               PERFORM 45021-AGE-CALC-SEC
           ELSE
               MOVE    SPA-SYSYMD      TO  WRK-CALC-YMD1
               MOVE    I001-BIRTHDAY   TO  WRK-CALC-YMD2
               PERFORM 45021-AGE-CALC-SEC
               COMPUTE WRK-AGE-YY   =   WRK-AGE-YY
                                    *   -1
               COMPUTE WRK-AGE-MM   =   WRK-AGE-MM
                                    *   -1
           END-IF
      *
           IF    ( WRK-AGE-YY          =   ZERO )
               MOVE    WRK-AGE-MM      TO  WRK-AGE-Z
               MOVE    "ヶ月"          TO  WRK-AGE-MEI
           ELSE
               MOVE    WRK-AGE-YY      TO  WRK-AGE-Z
               MOVE    "歳"            TO  WRK-AGE-MEI
           END-IF
      *
           MOVE    WRK-AGE-G           TO  SPA-GMN-TAGE (IDX)
      *
           PERFORM 800-ORCSHKNSET-SEC
           IF    ( ORCSHKN-IDX    >=   1 )
            AND  ( ORCSHKN-IDX    <=   ORCSHKN-HKN-MAX )
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                       TO  SPA-GMN-THKN (IDX)
           END-IF
      *
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-BTUNUM-MAX
               IF      I001-BTUNUM    =   SPA-GMN-BTUNUML (IDY)
                   MOVE    SPA-GMN-BTUNUMMEIL (IDY)
                                               TO  SPA-GMN-TBTUNAME(IDX)
                   MOVE    SPA-GMN-BTUNUM-MAX  TO  IDY
               END-IF
           END-PERFORM
           IF      I001-BRMNUM (3:)    NUMERIC
               MOVE    I001-BRMNUM (3:)    TO  WRK-BRMNUM-9
               MOVE    WRK-BRMNUM-9        TO  WRK-BRMNUM-Z
               MOVE    WRK-BRMNUM-Z        TO  SPA-GMN-TBRMNUM (IDX)
           ELSE            
               MOVE    I001-BRMNUM (3:)    TO  SPA-GMN-TBRMNUM (IDX)
           END-IF
           MOVE    I001-BTUNUM     TO  SPA-NAI-TBTUNUM     (IDX)    
           MOVE    I001-NYUINYMD   TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-TNYUINYMD   (IDX)
           MOVE    I001-NYUINYMD   TO  SPA-NAI-TNYUINYMD   (IDX)
           IF    ( I001-TAIINYMD   =   "99999999" )
               MOVE    SPACE           TO  SPA-GMN-TTAIINYMD   (IDX)
           ELSE
               MOVE    I001-TAIINYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TTAIINYMD   (IDX)
           END-IF
           MOVE    I001-TAIINYMD       TO  SPA-NAI-TTAIINYMD   (IDX)
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-TAIINCD-MAX
               IF      I001-TAIINCD   =   SPA-GMN-TAIINCDL (IDY)
                   MOVE    SPA-GMN-TAIINCDMEIL (IDY)
                                              TO  SPA-GMN-TTAIINCD (IDX)
                   MOVE    SPA-GMN-TAIINCD-MAX  TO  IDY
               END-IF
           END-PERFORM
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-SRYKA-MAX
               IF      I001-NYUINKA   =   SPA-GMN-SRYKAL (IDY)
                   MOVE    SPA-GMN-SRYKAMEIL (IDY)
                                              TO  SPA-GMN-TSRYKA (IDX)
                   MOVE    SPA-GMN-SRYKA-MAX  TO  IDY
               END-IF
           END-PERFORM
           IF    ( I001-HOME-POST      NOT =   SPACE )
               MOVE    I001-HOME-POST (1:3)    TO  SPA-GMN-TPOST (IDX)
                                                                 (1:3)
               MOVE    "-"                     TO  SPA-GMN-TPOST (IDX)
                                                                 (4:1)
               MOVE    I001-HOME-POST (4:4)    TO  SPA-GMN-TPOST (IDX)
                                                                 (5:4)
           END-IF
           STRING  I001-HOME-ADRS      DELIMITED  BY  SPACE
                   I001-HOME-BANTI     DELIMITED  BY  SPACE
                   INTO    SPA-GMN-TADRS (IDX)
           END-STRING
           MOVE   I001-BIRTHDAY    TO  SPA-NAI-TBIRTHDAY (IDX)          
           .
       4502-LIST-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       45021-AGE-CALC-SEC              SECTION.
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
           .
      *
       45021-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    ＳＱＬ文編集処理（並べ替え条件）
      *****************************************************************
       45031-SQL-ORDER-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-ORDER
      *
           EVALUATE    WRK-SRTKBN
           WHEN    "1"
               MOVE    "1"         TO  WRK-ORDER
           WHEN    "2"
               MOVE    "1"         TO  WRK-ORDER
           WHEN    "3"
               MOVE    "1"         TO  WRK-ORDER
           WHEN    "4"
               MOVE    "2"         TO  WRK-ORDER
           WHEN    "5"
               MOVE    "1"         TO  WRK-ORDER
           WHEN    "6"
               MOVE    "2"         TO  WRK-ORDER
           WHEN    "7"
               MOVE    "1"         TO  WRK-ORDER
           END-EVALUATE
      *
           .
       45031-SQL-ORDER-EXT.
           EXIT.
      *****************************************************************
      *    件数取得処理
      *****************************************************************
       4504-COUNT-GET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-TOTAL
                                       SPA-NAI-TOTAL
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           PERFORM 900-PTSRH-CNT-SEL-SEC
      *
           IF    ( FLG-PTSRH           =   ZERO )
               IF    ( PTSRH-PTID      >   ZERO )
                   MOVE    PTSRH-PTID      TO  SPA-NAI-TOTAL
                   MOVE    SPACE           TO  SPA-GMN-TOTAL
                   MOVE    PTSRH-PTID      TO  WRK-STR-LEFT-ZX99
                   PERFORM 800-STR-HIDARIZUME-HEN-SEC
                   STRING  "件数："            DELIMITED BY SIZE
                           WRK-STR-LEFT        DELIMITED BY SPACE
                   INTO    SPA-GMN-TOTAL
                   END-STRING
               END-IF
           END-IF
      *
           .
       4504-COUNT-GET-EXT.
           EXIT.
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I2FREE          TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "I20"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "I20"               TO  SPA-MOTOPG3
      *     
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I2FREE          TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "I20"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "U01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日編集処理
      *****************************************************************
       5002-GETUMATU-HEN-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       5002-GETUMATU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-I2IDCD      NOT =   SPACE
               PERFORM 520-I2IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I20    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
           MOVE    I20-ROW             TO  WRK-KANJYALST-ROW
      * 
           MOVE    SPACE               TO  I20
      *
           INITIALIZE                      I20
           IF    ( FLG-GMN-INIT        =   1 )
               MOVE    ZERO            TO  I20-ROW
               MOVE    ZERO            TO  I20-ROWATTR
           ELSE
               MOVE    WRK-KANJYALST-ROW
                                       TO  I20-ROW
               MOVE    ZERO            TO  I20-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  I20-PTNUM
           MOVE    SPA-GMN-NAME        TO  I20-NAME
           MOVE    SPA-GMN-STAGE-G     TO  I20-STAGE
           MOVE    SPA-GMN-EDAGE-G     TO  I20-EDAGE
           MOVE    SPA-GMN-ADRS        TO  I20-ADRS
           MOVE    SPA-GMN-STNYUINYMD  TO  I20-STNYUINYMD
           MOVE    SPA-GMN-EDNYUINYMD  TO  I20-EDNYUINYMD
           MOVE    SPA-GMN-STTAIINYMD  TO  I20-STTAIINYMD
           MOVE    SPA-GMN-EDTAIINYMD  TO  I20-EDTAIINYMD
           MOVE    SPA-GMN-SRYKA-G     TO  I20-SRYKA
           MOVE    SPA-GMN-BTUNUM-G    TO  I20-BTUNUM
           MOVE    SPA-GMN-JOTAI-G     TO  I20-JOTAI
           MOVE    SPA-GMN-CHKTESTPT   TO  I20-CHKTESTPT
           MOVE    SPA-GMN-TAIINCD-G   TO  I20-TAIINCD
           MOVE    SPA-GMN-DEATHKBN-G  TO  I20-DEATHKBN
           MOVE    SPA-GMN-SRT1KBN-G   TO  I20-SRT1KBN
           MOVE    SPA-GMN-SRT2KBN-G   TO  I20-SRT2KBN
           MOVE    SPA-GMN-SRT3KBN-G   TO  I20-SRT3KBN
           MOVE    SPA-GMN-SELNUM      TO  I20-SELNUM
           MOVE    SPA-GMN-TOTAL       TO  I20-TOTAL
      *     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-STAGE-MAX
               MOVE    SPA-GMN-STAGELST (IDX)  TO  I20-STAGELST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-STAGE-MAX     TO  I20-STAGE-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-EDAGE-MAX
               MOVE    SPA-GMN-EDAGELST (IDX)  TO  I20-EDAGELST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-EDAGE-MAX     TO  I20-EDAGE-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  I20-SRYKALST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-MAX   TO  I20-SRYKA-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-BTUNUM-MAX
               MOVE    SPA-GMN-BTUNUMLST (IDX) TO  I20-BTUNUMLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-BTUNUM-MAX  TO  I20-BTUNUM-COUNT
      *         
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-JOTAI-MAX
               MOVE    SPA-GMN-JOTAILST (IDX) TO  I20-JOTAILST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-JOTAI-MAX  TO  I20-JOTAI-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-TAIINCD-MAX
               MOVE    SPA-GMN-TAIINCDLST (IDX) TO  I20-TAIINCDLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-TAIINCD-MAX  TO  I20-TAIINCD-COUNT
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-DEATHKBN-MAX
               MOVE    SPA-GMN-DEATHKBNLST (IDX)
                                        TO  I20-DEATHKBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-DEATHKBN-MAX TO  I20-DEATHKBN-COUNT
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-SRTKBN-MAX
               MOVE    SPA-GMN-SRTKBNLST (IDX) TO  I20-SRT1KBNLST (IDX)
                                                   I20-SRT2KBNLST (IDX)
                                                   I20-SRT3KBNLST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRTKBN-MAX  TO  I20-SRT1KBN-COUNT
                                           I20-SRT2KBN-COUNT
                                           I20-SRT3KBN-COUNT
      *
           IF      SPA-GMN-MAX             =   ZERO
           AND     SPA-GMN-TNAME (1)   NOT =   SPACE
               MOVE    SPA-GMN-TNAME     (1)   TO  I20-TNAME     (1)
               MOVE    1                       TO  I20-COUNT   
           ELSE       
               PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-MAX
                 MOVE    SPA-GMN-TRENNUM   (IDX) TO  WRK-ZZZ
                 MOVE    WRK-ZZZ                 TO  I20-TRENNUM   (IDX)
                 MOVE    SPA-GMN-TPTNUM    (IDX) TO  I20-TPTNUM    (IDX)
                 MOVE    SPA-GMN-TNAME     (IDX) TO  I20-TNAME     (IDX)
                 MOVE    SPA-GMN-TSEX      (IDX) TO  I20-TSEX      (IDX)
                 MOVE    SPA-GMN-TAGE      (IDX) TO  I20-TAGE      (IDX)
                 MOVE    SPA-GMN-THKN      (IDX) TO  I20-THKN      (IDX)
                 MOVE    SPA-GMN-TBTUNAME  (IDX) TO  I20-TBTUNAME  (IDX) 
                 MOVE    SPA-GMN-TBRMNUM   (IDX) TO  I20-TBRMNUM   (IDX) 
                 MOVE    SPA-GMN-TNYUINYMD (IDX) TO  I20-TNYUINYMD (IDX)
                 MOVE    SPA-GMN-TTAIINYMD (IDX) TO  I20-TTAIINYMD (IDX)
                 MOVE    SPA-GMN-TTAIINCD  (IDX) TO  I20-TTAIINCD  (IDX)
                 MOVE    SPA-GMN-TSRYKA    (IDX) TO  I20-TSRYKA    (IDX)
                 MOVE    SPA-GMN-TPOST     (IDX) TO  I20-TPOST     (IDX)
                 MOVE    SPA-GMN-TADRS     (IDX) TO  I20-TADRS     (IDX)
      *         
                 IF      IDX             =   SPA-GMN-TBL-SEL
                     MOVE    "T"             TO  I20-SELECT (IDX)
                 ELSE
                     MOVE    "F"             TO  I20-SELECT (IDX)
                 END-IF
               END-PERFORM
               MOVE    SPA-GMN-MAX         TO  I20-COUNT   
           END-IF                  
      *
           EVALUATE    SPA-GMN-JOTAI
           WHEN    "3"
               MOVE    SPA-SYSYMDWH (1:6)  TO  I20-KJNYMDMES
           WHEN    "4"
               MOVE    SPA-SYSYMDWH        TO  I20-KJNYMDMES
           END-EVALUATE
      *
           IF    ( SPA-GMN-PAGE           <=   1 )
               MOVE    WIDGET-INSENSITIVE  TO  I20-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  I20-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-TBL-NEXT        =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  I20-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  I20-B07-STATE
           END-IF
      *
           IF    ( SPA-GMN-MAX            <=   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  I20-B10-STATE
                                               I20-B11-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  I20-B10-STATE
                                               I20-B11-STATE
           END-IF
      *
           IF  ( SPA-MW                    =   SPA-GINBEE )
            AND( SPA-I20-CLIENT-SAVE   NOT =   "1" )
               MOVE    WIDGET-INSENSITIVE  TO  I20-B10-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次へ
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "PTNUM"           TO  MCP-WIDGET 
               WHEN    2
                   MOVE  "NAME"            TO  MCP-WIDGET 
               WHEN    3
                   MOVE  "STAGE"           TO  MCP-WIDGET 
               WHEN    4
                   MOVE  "EDAGE"           TO  MCP-WIDGET 
               WHEN    5
                   MOVE  "ADRS"            TO  MCP-WIDGET 
               WHEN    6
                   MOVE  "STNYUINYMD"      TO  MCP-WIDGET 
               WHEN    7
                   MOVE  "EDNYUINYMD"      TO  MCP-WIDGET 
               WHEN    8
                   MOVE  "SRYKA"           TO  MCP-WIDGET 
               WHEN    9
                   MOVE  "BTUNUM"          TO  MCP-WIDGET 
               WHEN    10
                   MOVE  "JOTAI"           TO  MCP-WIDGET 
               WHEN    11
                   MOVE  "CHKTESTPT"       TO  MCP-WIDGET 
               WHEN    12
                   MOVE  "SELNUM"          TO  MCP-WIDGET 
               WHEN    13
                   MOVE  "STTAIINYMD"      TO  MCP-WIDGET 
               WHEN    14
                   MOVE  "EDTAIINYMD"      TO  MCP-WIDGET 
               WHEN    15
                   MOVE  "SRT1KBN"         TO  MCP-WIDGET 
               WHEN    17
                   MOVE  "SRT2KBN"         TO  MCP-WIDGET 
               WHEN    18
                   MOVE  "SRT3KBN"         TO  MCP-WIDGET 
               WHEN    16
                   MOVE  "TAIINCD"         TO  MCP-WIDGET 
               WHEN    19
                   MOVE  "DEATHKBN"        TO  MCP-WIDGET 
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    SPA-MCP-WIDGET
               WHEN    "PTNUM"
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    "NAME"
                   MOVE    3                   TO  SPA-GMN-CUR
               WHEN    "STAGE"
                   MOVE    4                   TO  SPA-GMN-CUR
               WHEN    "EDAGE"
                   MOVE    5                   TO  SPA-GMN-CUR
               WHEN    "ADRS"
                   MOVE    6                   TO  SPA-GMN-CUR
               WHEN    "STNYUINYMD"
                   MOVE    7                   TO  SPA-GMN-CUR
               WHEN    "EDNYUINYMD"
                   MOVE    13                  TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    9                   TO  SPA-GMN-CUR
               WHEN    "BTUNUM"
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "JOTAI"
                   MOVE    16                  TO  SPA-GMN-CUR
               WHEN    "SRT1KBN"
                   MOVE    17                  TO  SPA-GMN-CUR
               WHEN    "SRT2KBN"
                   MOVE    18                  TO  SPA-GMN-CUR
               WHEN    "SRT3KBN"
                   MOVE    12                  TO  SPA-GMN-CUR
               WHEN    "CHKTESTPT"
                   MOVE    12                  TO  SPA-GMN-CUR
               WHEN    "STTAIINYMD"
                   MOVE    14                  TO  SPA-GMN-CUR
               WHEN    "EDTAIINYMD"
                   MOVE    8                   TO  SPA-GMN-CUR
               WHEN    "TAIINCD"
                   MOVE    19                  TO  SPA-GMN-CUR
               WHEN    "DEATHKBN"
                   MOVE    15                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
     *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           IF    ( SPA-ERRCD           =    "7777" )
               MOVE    WRK-SHORIERR    TO  SPA-ERRMSG
           ELSE
               CALL    "ORCGI2ERRMSG"  USING
                                       SPA-AREA
           END-IF
      *
           MOVE    SPACE                TO  I2ERR
           MOVE    SPA-ERRCD            TO  I2ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  I2ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "I20"                TO  SPA-MOTOPG
           MOVE    "I2ERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I2ERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面表示処理
      *****************************************************************
       520-I2IDSET-SEC              SECTION.
      *
           EVALUATE    SPA-I2IDCD
               WHEN    "1001"
                   MOVE    "検索を開始します"
                                       TO  WRK-I2IDMSG
               WHEN    "6666"
                   MOVE    WRK-SHORIERR
                                       TO  WRK-I2IDMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-I2IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-I2ID1-FLG
      *
           MOVE    SPACE               TO  I2ID1
           INITIALIZE                      I2ID1
           MOVE    SPA-I2IDCD          TO  I2ID1-ID1CODE
           MOVE    WRK-I2IDMSG         TO  I2ID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I20"               TO  SPA-MOTOPG
           MOVE    "I2ID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I2ID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I2IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    WRK-STR-LEFT    TO  IDXA
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXA:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXA:)
                                           TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                           TO  WRK-STR-LEFT
                   MOVE    500             TO  IDXA
               END-IF
      *
           END-PERFORM
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集サブ呼出処理
      *****************************************************************
       800-ORCSHKNSET-SEC              SECTION.
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    SPA-AREA        TO  WRK-SPA-AREA
      *
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SPA-SRYYMD
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-SPA-SRYKA
           MOVE    "1"             TO  WRK-SPA-NYUGAIKBN
           MOVE    I001-PTID       TO  WRK-SPA-PTID
           MOVE    I001-BIRTHDAY   TO  WRK-SPA-BIRTHDAY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  WRK-SPA-HKNCOMBINUM
           MOVE    "1"             TO  ORCSHKN-KBN
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           .
       800-ORCSHKNSET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読込（主キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *     
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       900-JOBKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       900-JOBKANRI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOBKANRI-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-JOBEND
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-JOBEND
               END-IF
           ELSE
                   MOVE    2           TO  FLG-JOBEND
           END-IF
      *
           .
       900-JOBKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴情報検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO   FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM        TO   PTNYUINRRK-HOSPNUM
           MOVE    I001-PTID          TO   PTNYUINRRK-PTID
           MOVE    I001-RRKNUM        TO   PTNYUINRRK-RRKNUM
           MOVE    I001-RRKEDANUM-MAX TO   PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC     TO   MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"   TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-ODR-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTSRH
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "1"             TO  PTSRH-DATAKBN
           MOVE    "%"             TO  PTSRH-UPYMD
           MOVE    "%"             TO  PTSRH-UPHMS
           STRING  " ORDER BY SEQNUM " DELIMITED BY SIZE
                   " LIMIT "           DELIMITED BY SIZE
                   WRK-LIMIT           DELIMITED BY SIZE
                   " OFFSET "          DELIMITED BY SIZE
                   WRK-OFFSET          DELIMITED BY SIZE
           INTO    PTSRH-SQL
           END-STRING
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "odr"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTSRH-REC
           ELSE
               MOVE    1           TO  FLG-PTSRH
               INITIALIZE              PTSRH-REC
           END-IF
      *
           .
      *
       900-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-ODR-FET-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "odr"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTSRH-REC
           ELSE
               MOVE    1           TO  FLG-PTSRH
               INITIALIZE              PTSRH-REC
           END-IF
      *
           .
      *
       900-PTSRH-ODR-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-CNT-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTSRH
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "1"             TO  PTSRH-DATAKBN
           MOVE    "%"             TO  PTSRH-UPYMD
           MOVE    "%"             TO  PTSRH-UPHMS
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "cnt"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTSRH-REC
           ELSE
               MOVE    1           TO  FLG-PTSRH
               INITIALIZE              PTSRH-REC
           END-IF
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "cnt"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTSRH-CNT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "4"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    患者照会追加処理
      *****************************************************************
       900-PTSRH-INS2-SEC              SECTION.
      *
           PERFORM 900-PTSRH-DEL1-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "4"             TO  PTSRH-DATAKBN
           MOVE    SPA-GMN-SELYMD  TO  PTSRH-UPYMD
           MOVE    SPA-GMN-SELHMS  TO  PTSRH-UPHMS
           MOVE    "1"             TO  PTSRH-UPDATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "ins2"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-PTSRH-INS2-EXT.
           EXIT.
      *****************************************************************
      *    検索文字エスケープ処理
      *****************************************************************
       900-PTSRH-DBESCAPE-SEC          SECTION.
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBESCAPE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    MCPDATA-REC     TO  PTSRH-REC
      *
           .
      *
       900-PTSRH-DBESCAPE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    ORCA-BLOB       TO  MCPDATA-REC
      *
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    MCPDATA-REC     TO  ORCA-BLOB
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ORCA-BLOB-FILE
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
