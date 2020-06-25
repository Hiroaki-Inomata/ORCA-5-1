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
       PROGRAM-ID.             ORCBI203.
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
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
      *
           COPY    "CPASGNPARA.INC".
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //CSVNAME//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PRT                 PIC X(02).
           03  STS-CSV                 PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(09).
           03  IDXL                    PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(09).
           03  CNT-JOB                 PIC 9(09).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-PARA.
               05  WRK-PARA-PROC-FLG   PIC 9(1).
               05  WRK-PARA-TERMID     PIC X(64).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-DUMMY2     PIC X(64).
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-STAFFCD    PIC X(05).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-FILE       PIC X(01).
               05  WRK-PARA-FILENM     PIC X(100).
               05  WRK-PARA-FILENM-FOLDER
                                       PIC X(100).
               05  WRK-PARA-CSVFILENM  PIC X(100).
               05  WRK-PARA-SELYMD     PIC X(08).
               05  WRK-PARA-SELHMS     PIC X(06).
               05  WRK-PARA-STSRYYMD   PIC X(08).
               05  WRK-PARA-EDSRYYMD   PIC X(08).
           03  WRK-PROC-KNS            PIC 9(09).
           03  WRK-ZZZZ9               PIC ZZZZ9.
           03  WRK-ZZZZ9C              PIC ZZ,ZZ9.
           03  WRK-SYMD                PIC 9(08).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-EDTYMD3             PIC X(22).
           03  WRK-DATE                PIC X(10).
           03  WRK-PRTNM               PIC X(20).
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-KIKAN-G.
               05  WRK-KIKAN-ST        PIC X(22).
               05  WRK-KIKAN-MID       PIC X(06).
               05  WRK-KIKAN-ED        PIC X(22).
           03  WRK-NISSU               PIC 9(04).
           03  WRK-NISSUZ              PIC Z,ZZ9.
           03  WRK-NISSUZ-CSV          PIC ZZZ9.
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
           03  WRK-NISSU-G.
               05  WRK-NISSU1          PIC 9(05).
               05  WRK-NISSU2          PIC 9(05).
           03  WRK-SAKYMD              PIC X(22).
           03  WRK-BTUNUM-G.
               05  WRK-BTUNUM          PIC 9(02).
           03  WRK-NINZUZ              PIC Z,ZZZ,ZZ9.
           03  WRK-NINZUZ-CSV          PIC ZZZZZZ9.
           03  WRK-AVG                 PIC 999999V9.
           03  WRK-AVGZ                PIC ZZZ,ZZ9.9.
           03  WRK-AVGZ-CSV            PIC ZZZZ9.9.
           03  WRK-PAGE-G.
               05  WRK-PAGEZ           PIC Z,ZZ9.
               05  WRK-PAGE-LBL        PIC X(02).
           03  WRK-UUID                PIC X(36).
           03  WRK-ENCODING            PIC X(01).
           03  WRK-ENCODING9           REDEFINES WRK-ENCODING
                                       PIC 9(01).
      *
       01  CSV-HEADER-AREA.
           03  CSV-STSRYYMD            PIC X(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-EDSRYYMD            PIC X(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NISSU               PIC X(04).
      *
       01  CSV-BODY-AREA.
           03  CSV-BTUNUM              PIC X(02).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-BTUNAME             PIC X(20).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-KIKANST-NINZU       PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NYUIN-NINZU         PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-TENNYU-NINZU        PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-TENSHUTSU-NINZU     PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-TAIIN-NINZU         PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-KIKANED-NINZU       PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NOBE-NINZU          PIC X(07).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-HEIKIN-NINZU        PIC X(07).
      * 
       01  CONST-CSV-HEADER-AREA.
           03  CONST-CSV-STSRYYMD      PIC X(20) VALUE "対象期間開始日".
           03  FILLER                  PIC X(01) VALUE ",".
           03  CONST-CSV-EDSRYYMD      PIC X(20) VALUE "対象期間終了日".
           03  FILLER                  PIC X(01) VALUE ",".
           03  CONST-CSV-NISSU         PIC X(20) VALUE "日数".
      *
       01  CONST-CSV-BODY-AREA.
           03  CONST-CSV-BTUNUM    PIC X(20)   VALUE "病棟番号".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-BTUNAME   PIC X(20)   VALUE "病棟名".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-KIKANST-NINZU
                                   PIC X(20)   VALUE "期間開始日患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-NYUIN-NINZU
                                   PIC X(20)   VALUE "入院患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-TENNYU-NINZU
                                   PIC X(20)   VALUE "転入患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-TENSHUTSU-NINZU
                                   PIC X(20)   VALUE "転出患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-TAIIN-NINZU
                                   PIC X(20)   VALUE "退院患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-KIKANED-NINZU
                                   PIC X(20)   VALUE "期間終了日患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-NOBE-NINZU
                                   PIC X(20)   VALUE "入院延患者数".
           03  FILLER              PIC X(01)   VALUE ",".
           03  CONST-CSV-HEIKIN-NINZU
                                   PIC X(20)   VALUE "１日平均患者数".
      * 
       01  CONST-AREA.
           03  CONST-FORMID            PIC X(20)   VALUE  "HCN71.red".
           03  CONST-LINE              PIC 9(05)   VALUE   16.
           03  CONST-DATAKBN           PIC X(01)   VALUE   "c".
      *
       01  BTUSUM-AREA.
           COPY    "CPBTUSUM.INC".
      *
           COPY    "HCN71.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK1044.INC".
      *
      *    病棟マスタ
           COPY    "CPSK5001.INC".
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
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
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
           02  FILLER                  PIC X(512).
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
           PERFORM 800-MCNDATE-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-PROC-FLG
                                               WRK-PARA-TERMID
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-DUMMY2
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-STAFFCD
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-FILE
                                               WRK-PARA-FILENM-FOLDER
                                               WRK-PARA-FILENM
                                               WRK-PARA-CSVFILENM
                                               WRK-PARA-SELYMD
                                               WRK-PARA-SELHMS
                                               WRK-PARA-STSRYYMD
                                               WRK-PARA-EDSRYYMD
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-TERMID     TO  SPA-TERMID
           MOVE    WRK-PARA-STAFFCD    TO  SPA-STAFFCD
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
           PERFORM 900-JOB-STEP-START-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-TEMPDIR    TO  MCP-TEMPDIR
      *
           MOVE    WRK-PARA-STSRYYMD   TO  WRK-STYMD
           MOVE    WRK-PARA-EDSRYYMD   TO  WRK-EDYMD
           PERFORM 800-KIKAN-NISSU-SEC
           MOVE    WRK-NISSU2          TO  WRK-NISSU
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
           IF    ( WRK-PARA-PROC-FLG   =   ZERO )
               PERFORM 2001-EDIT-CSV-SEC
           ELSE
               PERFORM 2001-EDIT-REPORT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2001-EDIT-REPORT-SEC        SECTION.
      *
           PERFORM 20011-INIT-REPORT-SEC
      *
           PERFORM 900-PTSRH-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTSRH   NOT =   ZERO )
      *
               PERFORM 210-HEADER-SEC
      *
               PERFORM UNTIL ( FLG-PTSRH   NOT =   ZERO )
                        OR   ( IDXL            >=  CONST-LINE )
      *
                   PERFORM 220-BODY-SEC
      *
                   PERFORM 900-PTSRH-KEY3-FET-SEC
      *
               END-PERFORM
      *
               PERFORM 225-PAGE-BREAK-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   PRT-FILE
      *
           IF    ( CNT-PAGE         >   ZERO )
               PERFORM 230-PRINT-SEC
           END-IF
      *
           MOVE    CNT-PAGE        TO  CNT-JOB
      *
           .
       2001-EDIT-REPORT-EXT.
           EXIT.
      *****************************************************************
      *    初期処理
      *****************************************************************
       20011-INIT-REPORT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-PAGE
      *
      *    印刷用ファイルオープン
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC99"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SMCNDATE-TIME       TO  HC01PARA-SYOHMS
           MOVE    1                   TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGNPARA
           MOVE    SGETTEMP-ST         TO  ASGNPARA-ST
      *
           OPEN    OUTPUT                  PRT-FILE
      *
      *    プリンタ名取得処理
           PERFORM 110-PRTNUM-GET-SEC
           MOVE    SYS-1031-PRTNM(6)   TO  WRK-PRTNM
      *
           PERFORM 800-OFFSET-GET-SEC
      *
           MOVE    SPACE               TO  WRK-KIKAN-G
           MOVE    WRK-PARA-STSRYYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-KIKAN-ST
           MOVE    "　〜　"            TO  WRK-KIKAN-MID
           MOVE    WRK-PARA-EDSRYYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-KIKAN-ED
      *
           MOVE    WRK-PARA-SYSYMD     TO  WRK-SYMD
           PERFORM 800-SEIWA-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD
      *
           .
       20011-INIT-REPORT-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダー処理
      *****************************************************************
       210-HEADER-SEC                    SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           MOVE    SPACE               TO  HCN71
           INITIALIZE                      HCN71
      *
           MOVE    WRK-KIKAN-G         TO  HCN71-KIKAN
           MOVE    WRK-NISSU           TO  WRK-NISSUZ
           MOVE    WRK-NISSUZ          TO  HCN71-NISSU
           MOVE    WRK-SAKYMD          TO  HCN71-SAKYMD
      *
           COMPUTE CNT-PAGE = CNT-PAGE +   1
      *
           .
       210-HEADER-EXT.
           EXIT.
      *****************************************************************
      *    明細処理
      *****************************************************************
       220-BODY-SEC                    SECTION.
      *
           COMPUTE IDXL    =   IDXL    +   1
      *
           MOVE    PTSRH-MEMO          TO  BTUSUM-AREA
      *
           IF    ( BTUSUM-TTLKBN       =   "1" )
               MOVE    "合計"          TO  HCN71-BTUNAME (IDXL)
           ELSE
               MOVE    BTUSUM-BTUNUM   TO  HCN71-BTUNUM (IDXL)
                                           WRK-BTUNUM
      *
               PERFORM 2201-GET-SYS5001-SEC
               MOVE    SYS-5001-BTU-NAME
                                       TO  HCN71-BTUNAME (IDXL)
           END-IF
      *
           MOVE    BTUSUM-ST           TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-KIKANST-NINZU (IDXL)
           MOVE    BTUSUM-NYUIN        TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-NYUIN-NINZU (IDXL)
           MOVE    BTUSUM-TENNYU       TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-TENNYU-NINZU (IDXL)
           MOVE    BTUSUM-TENSTU       TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-TENSHUTSU-NINZU (IDXL)
           MOVE    BTUSUM-TAIIN        TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-TAIIN-NINZU (IDXL)
           MOVE    BTUSUM-ED           TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-KIKANED-NINZU (IDXL)
           MOVE    BTUSUM-TOTAL        TO  WRK-NINZUZ
           MOVE    WRK-NINZUZ          TO  HCN71-NOBE-NINZU (IDXL)
           COMPUTE WRK-AVG
                   =   BTUSUM-TOTAL
                   /   WRK-NISSU
                   +   0.05
           MOVE    WRK-AVG             TO  WRK-AVGZ
           MOVE    WRK-AVGZ            TO  HCN71-HEIKIN-NINZU (IDXL)
      *
           .
       220-BODY-EXT.
           EXIT.
      *****************************************************************
      *    改ページ処理
      *****************************************************************
       225-PAGE-BREAK-SEC                    SECTION.
      *
           IF    (  FLG-PTSRH      NOT =   ZERO )
            AND  (  CNT-PAGE           =   1    )
               CONTINUE
           ELSE
               INITIALIZE                  WRK-PAGE-G
               MOVE    CNT-PAGE        TO  WRK-PAGEZ
               MOVE    "頁"            TO  WRK-PAGE-LBL
               MOVE    WRK-PAGE-G      TO  HCN71-PAGE
           END-IF
      *
           INITIALIZE                      PRT-REC
           MOVE    CONST-FORMID        TO  PRT-PRTID
           MOVE    WRK-PRTNM           TO  PRT-PRTNM
           MOVE    "1"                 TO  PRT-SITEKBN
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
      *
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
               MOVE    "5"             TO  PRT-PRTKBN
           ELSE
               MOVE    "6"             TO  PRT-PRTKBN
           END-IF
           MOVE    MCP-TEMPDIR         TO  PRT-OUT
           MOVE    "ORCA0362"          TO  PRT-TBL-KEY
           MOVE    1                   TO  PRT-RENNUM
           MOVE    SMCNDATE-YMD        TO  PRT-TBL-GROUP
           MOVE    SMCNDATE-HMS        TO  PRT-TBL-GROUP(9:)
           MOVE    1                   TO  PRT-SHORI-RENNUM
           MOVE    CNT-PAGE            TO  PRT-PAGE
           MOVE    WRK-UUID            TO  PRT-UUID
      *
           MOVE     HCN71              TO  PRT-PRTDATA
      *
           WRITE    PRT-REC
      *
           .
       225-PAGE-BREAK-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報取得処理
      *****************************************************************
       2201-GET-SYS5001-SEC            SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    WRK-BTUNUM-G        TO  SYS-KBNCD
           MOVE    WRK-PARA-EDSRYYMD
                                       TO  SYS-STYUKYMD
           MOVE    WRK-PARA-STSRYYMD
                                       TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY26-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           .
       2201-GET-SYS5001-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-PRINT-SEC                   SECTION.
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
      *
           MOVE    ASGNPARA (ASGNPARA-ST:)
                                   TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
           MOVE    "0"             TO  SHELLTBL-ARG4
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
               MOVE    "病棟別入院患者一覧表"
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
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ編集処理
      *****************************************************************
       2001-EDIT-CSV-SEC           SECTION.
      *
           PERFORM 2001-INIT-CSV-SEC
      *
           PERFORM 900-PTSRH-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTSRH   NOT =   ZERO )
      *
               PERFORM 20011-EDIT-CSV-SEC
      *
               PERFORM 900-PTSRH-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptsrh"      TO  MCP-TABLE
           MOVE    "key3"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE    CSV-FILE
      *
           PERFORM 3001-FILE-INFO-INSERT-SEC
      *
           .
       2001-EDIT-CSV-EXT.
           EXIT.
      *****************************************************************
      *    初期処理
      *****************************************************************
       2001-INIT-CSV-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-JOB
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    WRK-PARA-CSVFILENM
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  CSVNAME
           MOVE    SGETTEMP-ST         TO  CSVNAME-ST
      *
           OPEN    OUTPUT              CSV-FILE
      *
           MOVE    CONST-CSV-HEADER-AREA
                                       TO  CSV-REC
           PERFORM 230-WRITE-CSV-SEC
      *
      *    (FILLER項目はINITIALIZEはで初期化されない)
           INITIALIZE                    CSV-HEADER-AREA
      *
           MOVE    WRK-PARA-STSRYYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  CSV-STSRYYMD
      *
           MOVE    WRK-PARA-EDSRYYMD   TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  CSV-EDSRYYMD
      *
           MOVE    WRK-NISSU           TO  WRK-NISSUZ-CSV
           MOVE    WRK-NISSUZ-CSV      TO  CSV-NISSU
      *
           MOVE    CSV-HEADER-AREA     TO  CSV-REC
           PERFORM 230-WRITE-CSV-SEC
      *
           MOVE    CONST-CSV-BODY-AREA TO  CSV-REC
           PERFORM 230-WRITE-CSV-SEC
      *
           .
       2001-INIT-CSV-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ編集処理
      *****************************************************************
       20011-EDIT-CSV-SEC           SECTION.
      *
      *    (FILLER項目はINITIALIZEはで初期化されない)
           INITIALIZE                      CSV-BODY-AREA
      *
           MOVE    PTSRH-MEMO          TO  BTUSUM-AREA
      *
           IF    ( BTUSUM-TTLKBN       =   "1" )
               MOVE    "合計"          TO  CSV-BTUNAME
           ELSE
               MOVE    BTUSUM-BTUNUM   TO  CSV-BTUNUM
                                           WRK-BTUNUM
      *
               PERFORM 2201-GET-SYS5001-SEC
               MOVE    SYS-5001-BTU-NAME
                                       TO  CSV-BTUNAME
           END-IF
      *
           MOVE    BTUSUM-ST           TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-KIKANST-NINZU
           MOVE    BTUSUM-NYUIN        TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-NYUIN-NINZU
           MOVE    BTUSUM-TENNYU       TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-TENNYU-NINZU
           MOVE    BTUSUM-TENSTU       TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-TENSHUTSU-NINZU
           MOVE    BTUSUM-TAIIN        TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-TAIIN-NINZU
           MOVE    BTUSUM-ED           TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-KIKANED-NINZU
           MOVE    BTUSUM-TOTAL        TO  WRK-NINZUZ-CSV
           MOVE    WRK-NINZUZ-CSV      TO  CSV-NOBE-NINZU
           COMPUTE WRK-AVG
                   =   BTUSUM-TOTAL
                   /   WRK-NISSU
                   +   0.05
           MOVE    WRK-AVG             TO  WRK-AVGZ-CSV
           MOVE    WRK-AVGZ-CSV        TO  CSV-HEIKIN-NINZU
      *
           MOVE    CSV-BODY-AREA       TO  CSV-REC
           PERFORM 230-WRITE-CSV-SEC
      *
           .
       20011-EDIT-CSV-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-WRITE-CSV-SEC               SECTION.
      *
           COMPUTE CNT-JOB =   CNT-JOB +   1
      *
           WRITE   CSV-REC
      *
           .
       230-WRITE-CSV-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-JOB-STEP-END-SEC
      *
           PERFORM 900-PTSRH-DEL1-SEC
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
           MOVE    ZERO                TO  WRK-RENNUM
      *
           INITIALIZE                      ORCSFILESVAREA
           MOVE    "I"                 TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                       TO  SFILESV-TBL-KEY
           IF    ( WRK-PARA-FILE       =   "1" )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "orcbi203.sh"   TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    SPACE           TO SFILESV-SRYYM    (WRK-RENNUM)
                                       
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
               MOVE    CNT-JOB         TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "病棟別入院患者一覧表"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
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
      *    オフセット取得処理
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    SPA-HOSPNUM    TO  SYS-1044-HOSPNUM
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-SEC           SECTION.
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
                                       TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3
                                       TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
           ELSE
               MOVE    SPACE           TO  WRK-EDTYMD1
               MOVE    SPACE           TO  WRK-EDTYMD3
           END-IF
           .
       800-SEIWA-EXT.
           EXIT.
      *****************************************************************
      *    期間計算処理
      *****************************************************************
       800-KIKAN-NISSU-SEC              SECTION.
      *
           INITIALIZE                  WRK-NISSU-G
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
           IF      ( STS-DAY-RC1       =   ZERO )
               MOVE    LNK-DAY5-NISSU1 TO  WRK-NISSU1
               MOVE    LNK-DAY5-NISSU2 TO  WRK-NISSU2
           END-IF
      *
           .
       800-KIKAN-NISSU-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理 (YYYY-MM-DD）
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD            NOT =   SPACE )
               MOVE    WRK-SYMD (1:4)  TO  WRK-DATE (1:4)
               MOVE    "/"             TO  WRK-DATE (5:1)
               MOVE    WRK-SYMD (5:2)  TO  WRK-DATE (6:2)
               MOVE    "/"             TO  WRK-DATE (8:1)
               MOVE    WRK-SYMD (7:2)  TO  WRK-DATE (9:2)
           END-IF
      *
           .
       800-DATE-EXT.
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
           MOVE    SPA-HOSPNUM TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "病棟別入院患者一覧表"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCBI203"      TO  JOB-PGID
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
           MOVE    SPA-HOSPNUM     TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-JOB         TO  JOB-UPDCNT
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY26-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    CONST-DATAKBN       TO  PTSRH-DATAKBN
           MOVE    WRK-PARA-SELYMD     TO  PTSRH-UPYMD
           MOVE    WRK-PARA-SELHMS     TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    CONST-DATAKBN   TO  PTSRH-DATAKBN
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
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
