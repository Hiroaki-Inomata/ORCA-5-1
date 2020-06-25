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
       PROGRAM-ID.             ORCBG003.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 入院会計未作成者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/05/08    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03/11/19  患者入院履歴よりデータを取得
      *                                     するよう修正
      *  01.00.02    NACL-太田    03/12/03  通算日数チェックに失敗時
      *                                     処理がループする不具合を修正
      *  03.05.01    NACL-森脇    07/06/07  グループ診療対応
      *  04.08.01    NACL-森脇    14/07/22  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
grpsys*DD  COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
grpsys*DD                              BY         //RECEERR//.
grpsys*DD  03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCMG003.INC".
      *
      *    スパ領域
       01  STS-AREA. 
           03  STS-RECEERR                         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-DBRC                            PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-NYUINACCT                       PIC 9(01).
           03  FLG-NYUINACCTARI                    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                            PIC 9(02).
           03  CNT-PAGE                            PIC 9(03).
           03  CNT-BG003                           PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID                     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM                    PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR                             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SRYYM                      PIC X(06).
           03  WRK-PARA-SYORIKBN                   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH                          PIC X(100).
grpsys     03  WRK-DBTABLE                         PIC X(100).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-SRYYM                           PIC X(16).
           03  WRK-SYORIYMD                        PIC X(22).
           03  WRK-PAGE                            PIC ZZ9.
           03  WRK-HCMG003.
               05  WRK-HCMG003-BTUNAME             PIC X(10).
               05  WRK-HCMG003-BRMNUM              PIC X(6).
               05  WRK-HCMG003-SRYKA               PIC X(8).
           03  WRK-BRM-NUM-IN                      PIC X(06).
           03  WRK-BRM-NUM-EDT                     PIC X(06).
           03  WRK-STR1                            PIC X(20).
           03  WRK-STR2                            PIC X(20).
           03  WRK-IDX1                            PIC 9(05).
           03  WRK-IDX2                            PIC 9(05).
           03  WRK-STR                             PIC X(200).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE 40.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
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
           PERFORM UNTIL ( FLG-END     =   1 )
               PERFORM 200-MAIN-SEC
           END-PERFORM
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
grpsys     INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
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
grpsys*DD                              WRK-PARA-HOSPID
grpsys                                 WRK-PARA-HOSPNUM
grpsys                                 RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-SYORIKBN
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG003"          TO  JOB-PGID
           MOVE    "入院会計未作成者一覧表"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
grpsys*DD  MOVE    "RECEERR"           TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID
grpsys*DD                              TO  RECEERR-TERMID
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
grpsys     MOVE    ORC-DBYMD           TO  SYS-1001-STYUKYMD
grpsys                                     SYS-1001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    診療年月
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD (7 : )
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (1 : 16)
                                       TO  WRK-SRYYM
      *
      *    作成日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SYORIYMD
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO                TO  CNT-PAGE
      *
           MOVE    ZERO                TO  CNT-BG003
      *
      *    患者入院履歴検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
grpsys*DD  MOVE    SYS-1001-HOSPID     TO  PTNYUINRRK-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
grpsys*DD  MOVE    "PTNYUINRRK-KEY27"  TO  ORC-DBPATH
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
grpsys     MOVE    "key27"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"     USING   MCPAREA
grpsys*DD                                  ORCMCPAREA
grpsys*DD                                  MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 2001-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  =   1 )
                   OR    ( FLG-END     NOT =   ZERO )
      *
               MOVE    SPACE       TO  HCMG003
      *
      *        ヘッダ編集処理
               PERFORM 210-HEAD-EDIT-SEC
      *
               MOVE    ZERO        TO  CNT-LINE
               PERFORM UNTIL ( FLG-PTNYUINRRK  =   1 )
                       OR    ( CNT-LINE       >=   CONST-LINE-MAX )
                       OR    ( FLG-END     NOT =   ZERO )
      *
      *            明細編集処理
                   PERFORM 210-BODY-EDIT-SEC
                   IF    ( FLG-END     NOT =   ZERO )
                       GO  TO  200-MAIN-EXT
                   END-IF
      *
               END-PERFORM
      *
      *        出力処理
               PERFORM 400-PRINT-OUT-SEC
      *
           END-PERFORM
      *
      *
grpsys*DD  MOVE    "PTNYUINRRK-KEY27"  TO  WRK-DBPATH
grpsys     MOVE    "tbl_ptnyuinrrk"    TO  WRK-DBTABLE
grpsys     MOVE    "key27"             TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴読込処理
      *****************************************************************
       2001-PTNYUINRRK-READ-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  =   1 )
                   OR    ( FLG-LOOPEND     =   1 )
      *
grpsys*DD      MOVE    "PTNYUINRRK-KEY27" 
grpsys*DD                              TO  WRK-DBPATH
grpsys         MOVE    "tbl_ptnyuinrrk"    TO  WRK-DBTABLE
grpsys         MOVE    "key27"             TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    MCPDATA-REC
                                       TO  PTNYUINRRK-REC
      *
      *            該当診療年月の入院基本料の剤が存在するかチェック
      *            （存在する場合は読み飛ばし）
                   PERFORM 2001-NYUINACCT-CHK-SEC
                   IF    ( FLG-NYUINACCTARI    =   ZERO )
                       MOVE    1       TO  FLG-LOOPEND
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       2001-NYUINACCT-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCTARI
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
grpsys*DD  MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
grpsys*DD  MOVE    "NYUINACCT-KEY7"    TO  WRK-DBPATH
grpsys     MOVE    "tbl_nyuinacct"     TO  WRK-DBTABLE
grpsys     MOVE    "key7"              TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
                    OR   ( FLG-NYUINACCTARI    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
      *
               IF    ( NACCT-ZAISKBKBN     =    "1"    )
                   MOVE    1           TO  FLG-NYUINACCTARI
               END-IF
      *
grpsys*DD      MOVE    "NYUINACCT-KEY7"    TO  WRK-DBPATH
grpsys         MOVE    "tbl_nyuinacct"     TO  WRK-DBTABLE
grpsys         MOVE    "key7"              TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE     1          TO  FLG-NYUINACCT
               END-IF
      *
           END-PERFORM
      *
grpsys*DD  MOVE    "NYUINACCT-KEY7"    TO  WRK-DBPATH
grpsys     MOVE    "tbl_nyuinacct"     TO  WRK-DBTABLE
grpsys     MOVE    "key7"              TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       2001-NYUINACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       210-HEAD-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    WRK-SRYYM       TO  HCMG003-SRYYM
      *
           MOVE    WRK-SYORIYMD    TO  HCMG003-SYORIYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCMG003-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細部>処理
      *****************************************************************
       210-BODY-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    SPACE           TO      WRK-HCMG003-BTUNAME
                                           WRK-HCMG003-BRMNUM
                                           WRK-HCMG003-SRYKA
      *
      *    患者番号取得処理
           PERFORM 2101-PTNUM-GET-SEC
      *
      *    患者情報取得処理
           PERFORM 2101-PTINF-GET-SEC
      *
      *    患者番号
           IF    ( PTNUM-PTNUM (11 : 1)    =   SPACE )
               MOVE    PTNUM-PTNUM         TO  HCMG003-PTNUM
                                                           (CNT-LINE)
           ELSE
               MOVE    PTNUM-PTNUM         TO  HCMG003-PTNUM2
                                                           (CNT-LINE)
           END-IF
      *
      *    氏名
           MOVE    PTINF-NAME              TO  HCMG003-NAME(CNT-LINE)
      *
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  HCMG003-SEX (CNT-LINE)
               WHEN    "2"
                   MOVE    "女"            TO  HCMG003-SEX (CNT-LINE)
           END-EVALUATE
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1             TO  HCMG003-BIRTHDAY
                                                           (CNT-LINE)
      *    病棟名称
           PERFORM 2101-BODY-BTUNAME-EDIT-SEC
           MOVE    WRK-HCMG003-BTUNAME     TO  HCMG003-BTUNAME
                                                           (CNT-LINE)
      *
      *    病室名称
           PERFORM 2101-BODY-BRMNUM-EDIT-SEC
           MOVE    WRK-HCMG003-BRMNUM      TO  HCMG003-BRMNUM
                                                           (CNT-LINE)
      *
      *    入院科
           PERFORM 2101-BODY-SRYKA-EDIT-SEC
           MOVE    WRK-HCMG003-SRYKA       TO  HCMG003-SRYKA
                                                           (CNT-LINE)
      *
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1             TO  HCMG003-NYUINYMD
                                                           (CNT-LINE)
      *
           COMPUTE CNT-BG003   =   CNT-BG003   +   1
      *
           PERFORM 2001-PTNYUINRRK-READ-SEC
      *
           .
       210-BODY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       2101-PTNUM-GET-SEC              SECTION.
      *
           INITIALIZE                      PTNUM-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  PTNUM-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
grpsys     MOVE    "tbl_ptnum"     TO  WRK-DBTABLE
grpsys     MOVE    "key"           TO  WRK-DBPATH
grpsys*DD  MOVE    "PTNUM-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
           END-IF
      *
grpsys*DD  MOVE    "PTNUM-KEY"         TO  WRK-DBPATH
grpsys     MOVE    "tbl_ptnum"     TO  WRK-DBTABLE
grpsys     MOVE    "key"           TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       2101-PTNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       2101-PTINF-GET-SEC              SECTION.
      *
           INITIALIZE                      PTINF-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  PTINF-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
grpsys     MOVE    "tbl_ptinf"     TO  WRK-DBTABLE
grpsys     MOVE    "key"           TO  WRK-DBPATH
grpsys*DD  MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               INITIALIZE                  PTINF-REC
           END-IF
      *
grpsys*DD  MOVE    "PTINF-KEY"         TO  WRK-DBPATH
grpsys     MOVE    "tbl_ptinf"     TO  WRK-DBTABLE
grpsys     MOVE    "key"           TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       2101-PTINF-GET-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称編集処理
      *****************************************************************
       2101-BODY-BTUNAME-EDIT-SEC      SECTION.
      *
      *    病棟情報
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM         TO  SYS-5001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD
           MOVE    "01"                TO  ORC-DBYMD (7 :)
grpsys     MOVE    ORC-DBYMD           TO  SYS-5001-STYUKYMD
grpsys                                     SYS-5001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE                  SYS-5001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-5001-BTU-TANNAME
                                       TO  WRK-HCMG003-BTUNAME
      *
           .
       2101-BODY-BTUNAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室名称編集処理
      *****************************************************************
       2101-BODY-BRMNUM-EDIT-SEC        SECTION.
      *
           MOVE   PTNYUINRRK-BRMNUM ( 3 : )
                                       TO  WRK-BRM-NUM-IN
           IF    ( WRK-BRM-NUM-IN (1 : 1)  =   SPACE )
               MOVE    SPACE           TO  WRK-STR1
               MOVE    1               TO  WRK-IDX1
               UNSTRING    WRK-BRM-NUM-IN
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX1
               END-UNSTRING
               MOVE    WRK-BRM-NUM-IN (WRK-IDX1 : )
                                       TO  WRK-BRM-NUM-EDT
           ELSE
               MOVE    WRK-BRM-NUM-IN  TO  WRK-BRM-NUM-EDT
           END-IF
      *
           PERFORM VARYING WRK-IDX1    FROM    6   BY  -1
               UNTIL     ( WRK-IDX1    <   1 )
                 OR      ( WRK-BRM-NUM-EDT (WRK-IDX1 : 1)
                                   NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    WRK-BRM-NUM-EDT TO  WRK-HCMG003-BRMNUM
               GO  TO  2101-BODY-BRMNUM-EDIT-EXT
           END-IF
      *
      *    病室番号が数字の場合前ゼロをとる
           IF    ( WRK-BRM-NUM-EDT ( 1 : WRK-IDX1 )
                                       IS  NUMERIC )
               IF    ( WRK-BRM-NUM-EDT ( 1 : 1 )
                                       =   ZERO )
                   MOVE    1               TO  WRK-IDX2
                   MOVE    SPACE           TO  WRK-STR1
                   UNSTRING    WRK-BRM-NUM-EDT
                       DELIMITED BY  ALL ZERO
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX2
                   END-UNSTRING
                   MOVE    WRK-BRM-NUM-EDT (WRK-IDX2 : )
                                           TO  WRK-HCMG003-BRMNUM
               ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  WRK-HCMG003-BRMNUM
               END-IF
           ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  WRK-HCMG003-BRMNUM
           END-IF
      *
           .
       2101-BODY-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2101-BODY-SRYKA-EDIT-SEC        SECTION.
      *
      *    診療科情報
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    PTNYUINRRK-NYUINKA  TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD
           MOVE    "01"                TO  ORC-DBYMD (7 :)
grpsys     MOVE    ORC-DBYMD           TO  SYS-1005-STYUKYMD
grpsys                                     SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE                  SYS-1005-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-1005-SRYKANAME2 TO  WRK-HCMG003-SRYKA
      *
           .
       2101-BODY-SRYKA-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCMG003.red"        TO  SPRT-PRTID
           MOVE    "入院会計未作成者一覧表"
                                       TO  SPRT-TITLE
           MOVE    HCMG003             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF    ( CNT-BG003           =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG003 IN " CNT-BG003  " ***"
           DISPLAY "*** ORCBG003 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys     MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
grpsys*DD      MOVE    WRK-DBPATH  TO  ORC-DBPATH
               MOVE    "DBFETCH"   TO  MCP-FUNC
grpsys         MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys         MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                         MCPDATA-REC
grpsys                                         SPA-AREA
grpsys*DD      CALL    "ORCMCPSUB"     USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys     MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    WRK-DBTABLE     TO  MCP-TABLE 
grpsys     MOVE    WRK-DBPATH      TO  MCP-PATHNAME 
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC 
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE 
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME 
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC 
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE 
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME 
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC 
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE 
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME 
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC 
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC 
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE 
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME 
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
           .
       900-DBDISCONNECT-EXT.
           EXIT.
