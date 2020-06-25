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
       PROGRAM-ID.             ORCBI201.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院患者照会
      *  コンポーネント名  : 入院患者検索結果一覧出力（ＯＲＣＢ�毅横娃院�
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/10/14    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付       内容
      *  03.05.00    NACL-太田    07.05.29  グループ診療対応
      *  04.01.01    NACL-藤原    07/10/19  クライアント保存対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  BI201-FILE      ASSIGN  PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-BI201.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    ＣＳＶファイル
           SELECT  CSV-FILE        ASSIGN  CSVNAME
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BI201-FILE.
           COPY    "CPBI201.INC".
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "CPPRTALL.INC".
      *
      *    ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC                     PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ワークパラメータファイル
           COPY    "CPBI201.INC"       REPLACING  //BI201//
                                       BY         //WKBI201//.
      *
      *
           COPY    "CPASGNPARA.INC".
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PRFNAME//.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //CSVNAME//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BI201               PIC X(02).
           03  STS-PRT                 PIC X(02).
           03  STS-CSV                 PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(09).
           03  IDX-LEN                 PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(09).
           03  CNT-LINE                PIC 9(05).
           03  CNT-KNS                 PIC 9(05).
           03  CNT-CSV                 PIC 9(09).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-STR                 PIC X(20000).
           03  WRK-STR-LEN             PIC 9(05).
           03  WRK-STR-PNT             PIC 9(05).
           03  WRK-PTINF-ADR-LEN       PIC 9(05).
           03  WRK-PTINF-BANTI-LEN     PIC 9(05).
           03  WRK-ADRS                PIC X(400).
           03  WRK-PARA.
               05  WRK-PARA-PAFNAME
                                       PIC X(200).
               05  WRK-PARA-TERMID     PIC X(64).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-SYSTIME    PIC X(08).
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-STAFFCD    PIC X(05).
               05  WRK-PARA-FILE       PIC X(01).
               05  WRK-PARA-FILENM     PIC X(100).
               05  WRK-PARA-FILENM-FOLDER
                                       PIC X(100).
               05  WRK-PARA-CSVFILENM  PIC X(100).
           03  WRK-PROC-KNS            PIC 9(09).
           03  WRK-ZZZZ9               PIC ZZZZ9.
           03  WRK-ZZZZ9C              PIC ZZ,ZZ9.
           03  WRK-SYMD                PIC 9(08).
           03  WRK-HENYMDG1            PIC X(09).
           03  WRK-HENYMDG3            PIC X(22).
           03  WRK-PRTNM               PIC X(20).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1        PIC 9(04).
               05  WRK-CALC-MM1        PIC 9(02).
               05  WRK-CALC-DD1        PIC 9(02).
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2        PIC 9(04).
               05  WRK-CALC-MM2        PIC 9(02).
               05  WRK-CALC-DD2        PIC 9(02).
           03  WRK-AGE-YY              PIC S9(04).
           03  WRK-AGE-MM              PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z           PIC ---9.
               05  WRK-AGE-MEI         PIC X(04).
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-ENCODING            PIC X(01).
           03  WRK-ENCODING9           REDEFINES WRK-ENCODING
                                       PIC 9(01).
           03  WRK-HKNMEI              PIC X(50).
           03  WRK-OFFSET              PIC 9(05).
           03  WRK-UUID                PIC X(36).
      *
       01  CSV-EDIT-AREA.
           03  CSV-NO                  PIC Z(9)9.
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-PTNUM               PIC X(20).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NAME                PIC X(100).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SEX                 PIC X(2).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-AGENUM              PIC X(04).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-AGETANI             PIC X(04).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-BTU-NAME            PIC X(20).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-BRMNUM              PIC X(06).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NYUINYMD            PIC X(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-TAIINYMD            PIC X(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-TAIINCD             PIC X(40).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYKANAME           PIC X(80).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-POST                PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-JYUSYO              PIC X(400).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-KANANAME            PIC X(100).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-HKNMEI              PIC X(50).
      * 
       01  CONST-AREA.
           03  CONST-FORMID            PIC X(20)   VALUE  "HCM40.red".
           03  CONST-LINE              PIC 9(05)   VALUE   30.
           03  CONST-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-JOB-SHELLID       PIC X(08)
               VALUE   "ORCBI201".
      * 
       01  CONST-CSV-HEADER.
           03  CONST-CSV-NO            PIC X(20)   VALUE "番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-PTNUM         PIC X(20)   VALUE "患者番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-NAME          PIC X(20)   VALUE "氏名".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SEX           PIC X(20)   VALUE "性別".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-AGENUM        PIC X(20)   VALUE "年齢".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-AGETANI       PIC X(20)   VALUE "年齢ラベル".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-BTU-NANE      PIC X(20)   VALUE "病棟".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-BRMNUM        PIC X(20)   VALUE "病室".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-NYUINYMD      PIC X(20)   VALUE "入院日".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-TAIINYMD      PIC X(20)   VALUE "退院日".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-TAIINCD       PIC X(20)   VALUE "退院事由".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYKANAME     PIC X(20)   VALUE "診療科".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-POST          PIC X(20)   VALUE "郵便番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-JYUSYO        PIC X(20)   VALUE "住所".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-KANANAME      PIC X(20)   VALUE "カナ氏名".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-HKNMEI        PIC X(20)   VALUE "保険組合せ".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
           COPY  "CPSK1044.INC".
      *
      *    病棟マスタ
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK5013.INC".
      *
      *    入院患者照会ビュー
       01  I001-REC.
           COPY    "CPI001.INC".
      *
      *    入院患者検索結果一覧
           COPY    "HCM40.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者照会
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *    入院患者照会サブ
           COPY    "CPI2SUB02.INC".
      *
      *    monpe印刷データ泣き別れ修正サブ
           COPY  "CPORCSFIXREDDATA.INC".
      *
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    カナチェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険組合せ取得処理
           COPY    "CPORCSHKNSET.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
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
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       CNT-AREA
      *
           MOVE    SPACE           TO  SPA-AREA
           INITIALIZE                  SPA-AREA
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-PAFNAME
                                               WRK-PARA-TERMID
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-SYSTIME
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-STAFFCD
                                               WRK-PARA-FILE
                                               WRK-PARA-FILENM-FOLDER
                                               WRK-PARA-FILENM
                                               WRK-PARA-CSVFILENM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-STAFFCD    TO  SPA-STAFFCD
           MOVE    WRK-PARA-TERMID     TO  SPA-TERMID
           MOVE    WRK-PARA-SYSYMD     TO  SPA-SYSYMD
           MOVE    MCP-USER            TO  SPA-OPID
      *
           INITIALIZE              ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING
                                   ORCSMCNDATEAREA
      *
           INITIALIZE              UIDIO-AREA.
           CALL    "orcuidset"     USING
                                   UIDIO-ST
                                   UIDIO-AREA
      *
           MOVE    UIDIO-UID           TO  WRK-UUID
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING        TO  WRK-ENCODING9
      *
      *
           PERFORM 900-JOB-STEP-START-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    WRK-PARA-PAFNAME    TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PRFNAME
           MOVE    SGETTEMP-TEMPDIR    TO  MCP-TEMPDIR
      *
      *    パラメータファイルオープン
           OPEN    INPUT               BI201-FILE
      *
      *    パラメータファイル読み込み
           READ    BI201-FILE
           END-READ
      *
           MOVE    BI201           TO  WKBI201
      *
           CLOSE   BI201-FILE
      *
           IF          WKBI201-PROC-FLG    =   ZERO
               INITIALIZE                  SGETTEMP-AREA
               MOVE    WRK-PARA-CSVFILENM
                                       TO  SGETTEMP-BASENAMES (1)
               CALL    "ORCSGETTEMP"   USING
                                       SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  CSVNAME
               MOVE    SGETTEMP-ST     TO  CSVNAME-ST
      *
               OPEN    OUTPUT              CSV-FILE
           ELSE
      *        印刷用ファイルオープン
               MOVE    WRK-PARA-HOSPNUM    TO  HC01PARA-HOSPNUM
               MOVE    "HC99"              TO  HC01PARA-FORM-ID
               MOVE    WRK-PARA-TERMID     TO  HC01PARA-TERMID
               MOVE    WRK-PARA-SYSTIME    TO  HC01PARA-SYOHMS
               MOVE    1                   TO  HC01PARA-CNT
      *
               INITIALIZE                  SGETTEMP-AREA
               MOVE    HC01PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
               CALL    "ORCSGETTEMP"   USING
                                       SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGNPARA
               MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
      *
               OPEN    OUTPUT              PRT-FILE
      *
      *        プリンタ名取得処理
               PERFORM 110-PRTNUM-GET-SEC
               MOVE    SYS-1031-PRTNM(6)   TO  WRK-PRTNM
      *
               PERFORM 800-OFFSET-GET-SEC
      *
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ名取得処理
      *****************************************************************
       110-PRTNUM-GET-SEC          SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    WRK-PARA-SYSYMD TO  ORCSPRTNM-SRYYMD
           MOVE    "2"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
           .
      *
       110-PRTNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
                                       CNT-KNS
      *
      *    患者対象データ検索処理
           PERFORM 900-PTSRH-ODR-SEL-SEC
      *
      *    処理件数算出
           COMPUTE WRK-PROC-KNS    =   WKBI201-EDKNS
                                   -   WKBI201-STKNS
                                   +   1
      *
           COMPUTE CNT-PAGE        =   WKBI201-STPAGE
                                   -   1
      *
           INITIALIZE                  HCM40
      *
           PERFORM VARYING CNT-KNS     FROM    1   BY  1
                   UNTIL ( CNT-KNS     >   WRK-PROC-KNS )
                   OR    ( FLG-PTSRH   =   1 )
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
               IF          WKBI201-PROC-FLG    =  ZERO
      *            ＣＳＶ編集処理
                   PERFORM 800-CSV-EDIT-SEC
               ELSE
      *            帳票編集処理
                   PERFORM 800-PRT-EDIT-SEC
               END-IF
      *
      *        入院患者照会ビュー読込処理
               PERFORM 900-PTSRH-ODR-FET-SEC
      *
           END-PERFORM
      *
           IF          WKBI201-PROC-FLG    =   1
               IF          CNT-LINE        >   ZERO
      *            帳票出力処理
                   PERFORM 800-PRT-WRITE-SEC
               END-IF
           END-IF
      *
      *    入院患者照会ビュークローズ処理
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "odr"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF          WKBI201-PROC-FLG    =  ZERO
               CLOSE   CSV-FILE
           ELSE
      *
               CLOSE   PRT-FILE
      *
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
      *
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG1
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
               MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
               MOVE    "0"             TO  SHELLTBL-ARG4
               IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                   MOVE    "入院患者検索結果一覧表"
                                       TO  SHELLTBL-ARG5
                   MOVE    SPA-CLIENT-SHOWDIALOG
                                       TO  SHELLTBL-ARG6
               END-IF
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "daily"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
      *
           IF          WKBI201-PROC-FLG    =  ZERO
      *        ファイル保存情報更新
               PERFORM 3001-FILE-INFO-INSERT-SEC
           END-IF
      *
           PERFORM 900-PTSRH-DEL1-SEC
      *
           PERFORM 900-JOB-STEP-END-SEC
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
           MOVE    ZERO            TO  WRK-RENNUM
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    CONST-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           IF    ( WRK-PARA-FILE       =   "1"             )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "orcbi201.sh"   TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    WRK-PARA-SYSTIME TO SFILESV-SRYYM    (WRK-RENNUM)
                                       
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
               MOVE    MCP-TEMPDIR     TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               MOVE    WRK-PARA-CSVFILENM
                                       TO  SFILESV-FOR-DATA (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM-FOLDER
                                       TO  SFILESV-TO-FOLDER(WRK-RENNUM)
               MOVE    WRK-PARA-FILENM TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    WRK-PROC-KNS    TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "入院患者検索結果一覧" 
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
      *********MOVE    "1"             TO  SFILESV-DATA-TYPE(WRK-RENNUM) 
           END-IF
      *
           IF      WRK-RENNUM          >   ZERO
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1044-HOSPNUM
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    CONST-FORMID        TO  SYS-1044-FORMID
           STRING  WRK-PRTNM           DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
           INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE  MCPDATA-REC       TO  SYS-1044-REC
           ELSE
               INITIALIZE                  SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF   (  SYS-1044-FORMID  =   CONST-FORMID  )
            AND (  SYS-1044-PRTNM   =   WRK-PRTNM    )
               CONTINUE
           ELSE
               INITIALIZE  SYS-1044-REC
           END-IF
      *
           .
       800-OFFSET-GET-EXT.
            EXIT.
      *****************************************************************
      *    ＣＳＶ編集処理
      *****************************************************************
       800-CSV-EDIT-SEC            SECTION.
      *
           INITIALIZE              CSV-NO
                                   CSV-PTNUM
                                   CSV-NAME
                                   CSV-SEX
                                   CSV-AGENUM
                                   CSV-AGETANI
                                   CSV-BTU-NAME
                                   CSV-BRMNUM
                                   CSV-NYUINYMD
                                   CSV-TAIINYMD
                                   CSV-TAIINCD
                                   CSV-SRYKANAME
                                   CSV-POST
                                   CSV-JYUSYO
                                   CSV-KANANAME
                                   CSV-HKNMEI
      *
           COMPUTE CNT-CSV =   CNT-CSV +   1
           IF    ( CNT-CSV =   1 )
               MOVE    CONST-CSV-HEADER
                                   TO  CSV-REC
               WRITE   CSV-REC
           END-IF
      *
           MOVE    CNT-KNS         TO  CSV-NO
           MOVE    I001-PTNUM      TO  CSV-PTNUM
           MOVE    I001-NAME       TO  CSV-NAME
      *
           EVALUATE    I001-SEX
           WHEN    1
               MOVE    "男"            TO  CSV-SEX
           WHEN    2
               MOVE    "女"            TO  CSV-SEX
           END-EVALUATE
      *
           PERFORM 900-AGE-EDIT-SEC
           MOVE    WRK-AGE-Z       TO  CSV-AGENUM
           MOVE    WRK-AGE-MEI     TO  CSV-AGETANI
      *
      *    病棟名称
           MOVE    SPACE           TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
           MOVE    WRK-PARA-HOSPNUM TO  SYS-5001-HOSPNUM
           MOVE    "5001"          TO  SYS-5001-KANRICD
           MOVE    I001-BTUNUM     TO  SYS-5001-KBNCD
           MOVE    WRK-PARA-SYSYMD TO  SYS-5001-STYUKYMD
                                       SYS-5001-EDYUKYMD
           MOVE    SYS-5001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               MOVE    SYS-5001-BTU-NAME
                                       TO  CSV-BTU-NAME
           ELSE
               MOVE    SPACE           TO  CSV-BTU-NAME
           END-IF
      *
      *    病室名編集
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    I001-BRMNUM(3:) TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"        USING
                                       ORCSBRMNUMAREA
           MOVE    SBRMNUM-OT-BRMNUM   TO   CSV-BRMNUM
      *
      *    入院日
           MOVE    I001-NYUINYMD(1:4)  TO  CSV-NYUINYMD(1:4)
           MOVE    "/"                 TO  CSV-NYUINYMD(5:1)
           MOVE    I001-NYUINYMD(5:2)  TO  CSV-NYUINYMD(6:2)
           MOVE    "/"                 TO  CSV-NYUINYMD(8:1)
           MOVE    I001-NYUINYMD(7:2)  TO  CSV-NYUINYMD(9:2)
      *    退院日
           IF          I001-TAIINYMD       =   "99999999"
               MOVE    SPACE               TO  CSV-TAIINYMD
           ELSE
               MOVE    I001-TAIINYMD(1:4)  TO  CSV-TAIINYMD(1:4)
               MOVE    "/"                 TO  CSV-TAIINYMD(5:1)
               MOVE    I001-TAIINYMD(5:2)  TO  CSV-TAIINYMD(6:2)
               MOVE    "/"                 TO  CSV-TAIINYMD(8:1)
               MOVE    I001-TAIINYMD(7:2)  TO  CSV-TAIINYMD(9:2)
           END-IF
      *
      *    退院事由
           INITIALIZE                  SYS-5013-REC
           MOVE    WRK-PARA-HOSPNUM TO  SYS-5013-HOSPNUM
           MOVE    "5013"          TO  SYS-5013-KANRICD
           MOVE    I001-TAIINCD    TO  SYS-5013-KBNCD
           MOVE    WRK-PARA-SYSYMD TO  SYS-5013-STYUKYMD
                                       SYS-5013-EDYUKYMD
           MOVE    SYS-5013-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5013-REC
               MOVE    SYS-5013-NAME
                                       TO  CSV-TAIINCD
           ELSE
               MOVE    SPACE           TO  CSV-TAIINCD
           END-IF
      *
      *    診療科名称
           MOVE    SPACE           TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    WRK-PARA-HOSPNUM TO  SYS-1005-HOSPNUM
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    I001-NYUINKA    TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SYSYMD TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME
                                       TO  CSV-SRYKANAME
           ELSE
               MOVE    SPACE           TO  CSV-SRYKANAME
           END-IF
      *
           MOVE    I001-HOME-POST  TO  CSV-POST
      *
           PERFORM 900-ADRS-EDIT-SEC
      *
           MOVE    WRK-ADRS        TO  CSV-JYUSYO
      *
           MOVE    I001-KANANAME   TO  CSV-KANANAME
      *
      *    保険組合せ
           PERFORM 800-ORCSHKNSET-SEC
           MOVE    WRK-HKNMEI      TO  CSV-HKNMEI
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    CSV-HKNMEI      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT
                                   TO  CSV-HKNMEI
      *
           MOVE    CSV-EDIT-AREA   TO  CSV-REC
      *
           WRITE   CSV-REC
      *
           .
       800-CSV-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       800-PRT-EDIT-SEC            SECTION.
      *
           COMPUTE CNT-LINE        =   CNT-LINE    +   1
      *
           MOVE    CNT-KNS         TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9       TO  HCM40-CD(CNT-LINE)
      *    患者番号の実際の文字列長を求める
           MOVE    20              TO  WRK-STR-LEN
           MOVE    I001-PTNUM      TO  WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           IF         WRK-STR-LEN      > 10
              MOVE    I001-PTNUM(1:10) TO  HCM40-PTNUMUP(CNT-LINE)
              MOVE    I001-PTNUM(11:)  TO  HCM40-PTNUMDN(CNT-LINE)
           ELSE
              MOVE    I001-PTNUM       TO  HCM40-PTNUM(CNT-LINE)
           END-IF
      *
      *    患者氏名の実際の文字列長を求める
           MOVE    100             TO  WRK-STR-LEN
           MOVE    I001-NAME       TO  WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           IF          WRK-STR-LEN         >   20
               MOVE    I001-NAME       TO  HCM40-NAME2(CNT-LINE)
           ELSE
               MOVE    I001-NAME       TO  HCM40-NAME(CNT-LINE)
           END-IF
      *
           EVALUATE    I001-SEX
           WHEN    1
               MOVE    "男"            TO  HCM40-SEX(CNT-LINE)
           WHEN    2
               MOVE    "女"            TO  HCM40-SEX(CNT-LINE)
           END-EVALUATE
      *
      *    年齢編集
           PERFORM 900-AGE-EDIT-SEC
           MOVE    WRK-AGE-Z       TO  HCM40-AGEZ(CNT-LINE)
           MOVE    WRK-AGE-MEI     TO  HCM40-AGEMEI(CNT-LINE)
      *
      *    保険組合せ
           PERFORM 800-ORCSHKNSET-SEC
           MOVE    WRK-HKNMEI      TO  HCM40-HKNMEI(CNT-LINE)
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    HCM40-HKNMEI(CNT-LINE)
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT
                                   TO  HCM40-HKNMEI(CNT-LINE)
      *
      *    病棟名称
           MOVE    SPACE           TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
           MOVE    WRK-PARA-HOSPNUM TO  SYS-5001-HOSPNUM
           MOVE    "5001"          TO  SYS-5001-KANRICD
           MOVE    I001-BTUNUM     TO  SYS-5001-KBNCD
           MOVE    WRK-PARA-SYSYMD TO  SYS-5001-STYUKYMD
                                       SYS-5001-EDYUKYMD
           MOVE    SYS-5001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               MOVE    SYS-5001-BTU-TANNAME
                                       TO  HCM40-TANNAME(CNT-LINE)
           END-IF
      *
      *    病室名編集
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    I001-BRMNUM(3:) TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"        USING
                                       ORCSBRMNUMAREA
           MOVE    SBRMNUM-OT-BRMNUM
                                   TO  HCM40-BRMNUM(CNT-LINE)
      *
           MOVE    I001-NYUINYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG1    TO  HCM40-NYUINYMD(CNT-LINE)
      *
           MOVE    I001-TAIINYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG1    TO  HCM40-TAIINYMD(CNT-LINE)
      *
      *    退院事由
           INITIALIZE                  SYS-5013-REC
           MOVE    WRK-PARA-HOSPNUM TO  SYS-5013-HOSPNUM
           MOVE    "5013"          TO  SYS-5013-KANRICD
           MOVE    I001-TAIINCD    TO  SYS-5013-KBNCD
           MOVE    WRK-PARA-SYSYMD TO  SYS-5013-STYUKYMD
                                       SYS-5013-EDYUKYMD
           MOVE    SYS-5013-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5013-REC
               MOVE    SYS-5013-TANNAME
                                       TO  HCM40-TAIINCD(CNT-LINE)
           END-IF
      *
      *    診療科名称
           MOVE    SPACE           TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    WRK-PARA-HOSPNUM TO  SYS-1005-HOSPNUM
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    I001-NYUINKA    TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SYSYMD TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF          FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME
                                       TO  HCM40-SRYKA(CNT-LINE)
           END-IF
      *
           IF          I001-HOME-POST  NOT =   SPACE
               MOVE    I001-HOME-POST (1:3)
                                       TO  HCM40-YUBINNUM(CNT-LINE)(1:3)
               MOVE    "-"             TO  HCM40-YUBINNUM(CNT-LINE)(4:1)
               MOVE    I001-HOME-POST (4:4)
                                       TO  HCM40-YUBINNUM(CNT-LINE)(5:4)
           ELSE
               MOVE    SPACE           TO  HCM40-YUBINNUM(CNT-LINE)
           END-IF
      *
           PERFORM 900-ADRS-EDIT-SEC
      *
           IF          WRK-ADRS(37:)   =   SPACE
               MOVE    WRK-ADRS        TO  HCM40-JYUSYO (CNT-LINE)
           ELSE
               MOVE    WRK-ADRS        TO  HCM40-JYUSYO2 (CNT-LINE)
           END-IF
      *
           IF          CNT-LINE        >=   CONST-LINE
      *        帳票出力処理
               PERFORM 800-PRT-WRITE-SEC
               INITIALIZE                  HCM40
               MOVE    ZERO            TO  CNT-LINE
           END-IF
      *
           .
       800-PRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       800-PRT-WRITE-SEC           SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
      *
      *    ヘッダ部編集
      *
           MOVE    WRK-PARA-SYSYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG3    TO  HCM40-SAKUSEIBI
      *
           MOVE    CNT-PAGE        TO  WRK-ZZZZ9C
           MOVE    WRK-ZZZZ9C      TO  HCM40-PAGE
      *
      *
           MOVE    SPACE           TO  PRT-REC
           INITIALIZE                  PRT-REC
           MOVE    CONST-FORMID   TO  PRT-PRTID
           MOVE    WRK-PRTNM       TO  PRT-PRTNM
           MOVE    "1"             TO  PRT-SITEKBN
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
           INITIALIZE                  SFIXREDDATA-AREA
           MOVE    "1"             TO  SFIXREDDATA-SITEKBN
           MOVE    PRT-PRTID       TO  SFIXREDDATA-FORMID
           MOVE    HCM40           TO  SFIXREDDATA-DATA
      *
      *
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
               MOVE    "5"         TO  PRT-PRTKBN
           ELSE
               MOVE    "6"         TO  PRT-PRTKBN
           END-IF
           MOVE    MCP-TEMPDIR     TO  PRT-OUT
           MOVE    "ORCA0361"      TO  PRT-TBL-KEY
           MOVE    1               TO  PRT-RENNUM
           MOVE    SMCNDATE-YMD    TO  PRT-TBL-GROUP
           MOVE    SMCNDATE-HMS    TO  PRT-TBL-GROUP(9:)
           MOVE    1               TO  PRT-SHORI-RENNUM
           MOVE    CNT-PAGE        TO  PRT-PAGE
           MOVE    WRK-UUID        TO  PRT-UUID
      *
           CALL    "ORCSFIXREDDATA"    USING
                                       SFIXREDDATA-AREA
                                       SPA-AREA
           MOVE    SFIXREDDATA-DATA
                                   TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           .
       800-PRT-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF          STS-DAY-RC1     =  ZERO
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  WRK-HENYMDG1
               MOVE    LNK-DAY2-EDTYMD3
                                       TO  WRK-HENYMDG3
               INSPECT WRK-HENYMDG3    REPLACING  ALL "  "  BY  "　"
           ELSE
               MOVE    SPACE           TO  WRK-HENYMDG1
               MOVE    SPACE           TO  WRK-HENYMDG3
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集サブ呼出処理
      *****************************************************************
       800-ORCSHKNSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-HKNMEI
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  SPA-SRYYMD
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           MOVE    I001-PTID       TO  SPA-PTID
           MOVE    I001-BIRTHDAY   TO  SPA-BIRTHDAY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  SPA-HKNCOMBINUM
           MOVE    "1"             TO  ORCSHKN-KBN
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           IF    ( ORCSHKN-IDX    >=   1 )
            AND  ( ORCSHKN-IDX    <=   ORCSHKN-HKN-MAX )
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                   TO  WRK-HKNMEI
           END-IF
      *
           .
       800-ORCSHKNSET-EXT.
           EXIT.
      *****************************************************************
      *    年齢編集処理
      *****************************************************************
       900-AGE-EDIT-SEC            SECTION.
      *
           IF          I001-BIRTHDAY   <   BI201-KJYNYMD
               MOVE    I001-BIRTHDAY   TO  WRK-CALC-YMD1
               MOVE    BI201-KJYNYMD   TO  WRK-CALC-YMD2
               PERFORM 900-AGE-CALC-SEC
           ELSE
               MOVE    BI201-KJYNYMD   TO  WRK-CALC-YMD1
               MOVE    I001-BIRTHDAY   TO  WRK-CALC-YMD2
               PERFORM 900-AGE-CALC-SEC
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       *   -1
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
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
           .
       900-AGE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       900-AGE-CALC-SEC                SECTION.
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
      *
           .
       900-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    住所編集処理
      *****************************************************************
       900-ADRS-EDIT-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-ADRS
      *
      *    住所の項目長を求める
           MOVE    1               TO  WRK-STR-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  I001-HOME-ADRS  DELIMITED   BY  SIZE
           INTO    WRK-STR
           WITH POINTER WRK-STR-LEN
           END-STRING
           COMPUTE WRK-STR-LEN     =   WRK-STR-LEN -   1
      *
      *    住所の実際の文字列長を求める
           MOVE    I001-HOME-ADRS  TO  WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           MOVE    WRK-STR-LEN     TO  WRK-PTINF-ADR-LEN
      *
      *    番地の項目長を求める
           MOVE    1               TO  WRK-STR-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  I001-HOME-BANTI DELIMITED   BY  SIZE
           INTO    WRK-STR
           WITH POINTER WRK-STR-LEN
           END-STRING
           COMPUTE WRK-STR-LEN     =   WRK-STR-LEN -   1
      *
      *    番地の実際の文字列長を求める
           MOVE    I001-HOME-BANTI TO  WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           MOVE    WRK-STR-LEN     TO  WRK-PTINF-BANTI-LEN
      *
           MOVE    1               TO  WRK-STR-PNT
           IF          WRK-PTINF-ADR-LEN   >   0
               STRING  I001-HOME-ADRS(1: WRK-PTINF-ADR-LEN)
                                       DELIMITED   BY  SIZE
                       "　"            DELIMITED   BY  SIZE
               INTO    WRK-ADRS
               WITH    POINTER WRK-STR-PNT
               END-STRING
           END-IF
      *
           IF          WRK-PTINF-BANTI-LEN >   0
               STRING  I001-HOME-BANTI(1: WRK-PTINF-BANTI-LEN)
                                       DELIMITED   BY  SIZE
               INTO    WRK-ADRS
               WITH    POINTER WRK-STR-PNT
               END-STRING
           END-IF
      *
           .
       900-ADRS-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       900-STR-LEN-GET-SEC         SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
                   UNTIL ( IDX-LEN     <   1 )
                   OR    ( WRK-STR(IDX-LEN:1)  NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN         TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    年齢文字列長取得処理
      *****************************************************************
       900-STR-LEN-AGEGET-SEC      SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
                   UNTIL ( IDX-LEN     <   1 )
                   OR    ( WRK-STR(IDX-LEN:1)  NOT =   "-" )
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN         TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-AGEGET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ステップ管理開始処理
      *****************************************************************
       900-JOB-STEP-START-SEC      SECTION.
      *
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "入院患者検索結果一覧出力"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCBI201"      TO  JOB-PGID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       900-JOB-STEP-START-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOB-STEP-END-SEC        SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           IF          WKBI201-PROC-FLG    =   ZERO
               MOVE    WRK-PROC-KNS
                                   TO  JOB-UPDCNT
           ELSE
               COMPUTE JOB-UPDCNT  =   CNT-PAGE
                                   -   WKBI201-STPAGE
                                   +   1
           END-IF
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       900-JOB-STEP-END-EXT.
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
           COMPUTE WRK-OFFSET = WKBI201-STKNS - 1
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "4"             TO  PTSRH-DATAKBN
           MOVE    "%"             TO  PTSRH-UPYMD
           MOVE    "%"             TO  PTSRH-UPHMS
           STRING  " ORDER BY SEQNUM " DELIMITED BY SIZE
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
      *    ＤＢ　接続処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
