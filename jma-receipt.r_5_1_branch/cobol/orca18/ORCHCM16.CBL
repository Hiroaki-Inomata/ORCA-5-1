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
       PROGRAM-ID.             ORCHCM16.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 受付
      *  コンポーネント名  : 受付一覧表印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/10    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/11/06  出力先プリンタサブ追加
      *                                     出力先を予約一覧（７）へ
      * 01.00.01     NACL-多々納  05/12/07  MONFUNC対応
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.08.00    NACL-多々納  14/08/01  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCM16.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷データ名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-HCM16           PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-DATE            PIC 9(08).
           03  SYS-TIME            PIC 9(08).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZ9            PIC ZZZ9.
      *
      *    編集用作業領域
       01  WRK-HEN-AREA.
           03  WRK-HEN-DATE.
               05  WRK-HDYY        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDMM        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDDD        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   "/".
           03  WRK-HEN-TIME.
               05  WRK-HTHH        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-HTMM        PIC 9(02).
      *
      ****************************************************************
      *    ファイルレイアウト
      ****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    診療内容情報
           COPY    "CPSK1012.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      ****************************************************************
      *    サブプロ用領域
      ****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      ****************************************************************
      *    連絡領域
      ****************************************************************
       LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE               DIVISION    USING
                                           SPA-AREA.
      *
      ****************************************************************
      *    主処理
      ****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主　　処理
           PERFORM 200-MAIN-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      ****************************************************************
      *    初期処理
      ****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-DATE    FROM    DATE
           ACCEPT  SYS-TIME    FROM    TIME
      *
      *    出力先プリンタ名割り当て情報編集
           PERFORM 900-SYS1031-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      ****************************************************************
      *    主　　処理
      ****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-UKETUKE
           MOVE    ZERO            TO  CNT-AREA
      *
      *    受付読み込み
           PERFORM 900-UKETUKE-SELECT-SEC
      *
           PERFORM UNTIL   FLG-UKETUKE =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
      *    受付読み込み
           PERFORM 900-UKETUKE-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      ****************************************************************
      *    帳票編集<ヘッダー部>処理
      ****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           INITIALIZE                  HCM16
           MOVE    SPACE           TO  HCM16
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCM16-SRYYMD
      *
           MOVE    SYS-DATE(3:2)   TO  WRK-HDYY
           MOVE    SYS-DATE(5:2)   TO  WRK-HDMM
           MOVE    SYS-DATE(7:2)   TO  WRK-HDDD
           MOVE    SYS-TIME(1:2)   TO  WRK-HTHH
           MOVE    SYS-TIME(3:2)   TO  WRK-HTMM
           MOVE    WRK-HEN-AREA    TO  HCM16-SAKUSEI
      *
           MOVE    CNT-PAGE        TO  WRK-ZZ9
           MOVE    WRK-ZZ9         TO  HCM16-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    帳票編集<明細行>処理
      ****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           PERFORM UNTIL   FLG-UKETUKE =   1
                   OR      CNT-LINE    >=  35
               ADD     1               TO  CNT-LINE
               ADD     1               TO  CNT-HCM16
      *
      *        番号
               MOVE    CNT-HCM16       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  HCM16-NUM(CNT-LINE)
      *
      *        患者名称
               MOVE    UKE-NAME        TO  HCM16-NAME(CNT-LINE)
      *
      *        患者番号変換
               IF      UKE-PTID        NOT =   ZERO
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    UKE-PTID        TO  SPTID-PTID
                   CALL    "ORCSPTID"          USING
                                               ORCSPTIDAREA
                                               SPA-AREA
                   IF      SPTID-RC    =   ZERO
                       MOVE    SPTID-PTNUM     TO  HCM16-PTNUM(CNT-LINE)
                   END-IF
               END-IF
      *
      *        診療科読み込み
               PERFORM 900-SYS1005-READ-SEC
               MOVE    SYS-1005-SRYKANAME1     TO  HCM16-SRYKA(CNT-LINE)
      *
      *        診療内容読み込み
               PERFORM 900-SYS1012-READ-SEC
               MOVE    SYS-1012-SRYNAIYODSP    TO  HCM16-NAIYO(CNT-LINE)
      *
      *        職員情報読み込み
               IF      UKE-DRCD(2:4)   NOT =   SPACE
                   PERFORM 900-SYS1010-READ-SEC
                   MOVE    SYS-1010-NAME   TO  HCM16-DRNAME(CNT-LINE)
               END-IF
      *
      *        受付時間
               MOVE    UKE-UKETIME(1:2)    TO  WRK-HTHH
               MOVE    UKE-UKETIME(3:2)    TO  WRK-HTMM
               MOVE    WRK-HEN-TIME    TO  HCM16-UKETUKE(CNT-LINE)
      *
      *        会計時間
               MOVE    UKE-KAIKEITIME(1:2) TO  WRK-HTHH
               MOVE    UKE-KAIKEITIME(3:2) TO  WRK-HTMM
               MOVE    WRK-HEN-TIME    TO  HCM16-KAIKEI(CNT-LINE)
      *
      *        受付読み込み
               MOVE    ZERO            TO  FLG-UKETUKE
               MOVE    ZERO            TO  UKE-UKEID
               PERFORM 900-UKETUKE-FETCH-SEC
                       UNTIL   (FLG-UKETUKE   =  1  )  OR
                               (UKE-UKEID     >  ZERO)
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    帳票印刷処理
      ****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC16"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE        TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           WRITE   HCM16
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM16.red"     TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
      *****MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
      *****MOVE    SYS-1031-PRTNM(9)   TO  MKPRT-PRTNM
           MOVE    SYS-1031-PRTNM(7)   TO  MKPRT-PRTNM
           MOVE    "受付一覧"          TO  MKPRT-INFO
           CALL    "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      ****************************************************************
      *    西暦日本語変換処理
      ****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    システム管理マスタ読み込み
      ****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    システム管理１００５読み込み
      ****************************************************************
       900-SYS1005-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    UKE-SRYKA       TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       900-SYS1005-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    システム管理１０１０読み込み
      ****************************************************************
       900-SYS1010-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    UKE-DRCD        TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           .
       900-SYS1010-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    システム管理１０１２読み込み
      ****************************************************************
       900-SYS1012-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1012-REC
           INITIALIZE                  SYS-1012-REC
           MOVE    "1012"          TO  SYS-1012-KANRICD
           MOVE    UKE-SRYNAIYO    TO  SYS-1012-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1012-REC
           ELSE
               INITIALIZE                  SYS-1012-REC
           END-IF
      *
           .
       900-SYS1012-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    システム管理１０３１読み込み
      ****************************************************************
       900-SYS1031-READ-SEC        SECTION.
      *
      **** MOVE    SPACE           TO  SYS-1031-REC
      *    INITIALIZE                  SYS-1031-REC
      *    MOVE    "1031"          TO  SYS-1031-KANRICD
      *    MOVE    "*"             TO  SYS-1031-KBNCD
      *    MOVE    SYS-1031-REC    TO  MCPDATA-REC
      *    PERFORM 800-SYSKANRI-READ-SEC
      *    IF      FLG-SYSKANRI    =   ZERO
      *        MOVE    MCPDATA-REC     TO  SYS-1031-REC
      *    ELSE
      *        INITIALIZE                  SYS-1031-REC
      **** END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-SYS1031-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    受付読み込み
      ****************************************************************
       900-UKETUKE-SELECT-SEC      SECTION.
      *
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM     TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD      TO  UKE-UKEYMD
           MOVE    UKETUKE-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-UKETUKE
               PERFORM 900-UKETUKE-FETCH-SEC
                       UNTIL   (FLG-UKETUKE   =  1  )  OR
                               (UKE-UKEID     >  ZERO)
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-SELECT-EXT.
           EXIT.
      *
      ****************************************************************
      *    受付読み込み
      ****************************************************************
       900-UKETUKE-FETCH-SEC       SECTION.
      *
           MOVE    "tbl_uketuke"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC      =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-FETCH-EXT.
           EXIT.
      *
      ****************************************************************
      *    受付ＣＬＯＳＥ
      ****************************************************************
       900-UKETUKE-CLOSE-SEC       SECTION.
      *
           MOVE    "tbl_uketuke"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC 
      *
           .
       900-UKETUKE-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
