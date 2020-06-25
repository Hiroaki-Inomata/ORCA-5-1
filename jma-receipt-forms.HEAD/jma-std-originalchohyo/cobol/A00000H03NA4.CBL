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
       PROGRAM-ID.             A00000H03NA4.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院請求書兼領収書　印刷（ＨＣＮ０３Ｖ０２）
      *  管理者            : 
      *  作成日付    作業者         記述
      *  04/02/24    @@@@−半田    新規作成(jma-receipt-hosp 2.1.0 カスタマイズ)
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/04/04  DB検索時の不具合を修正
      *  01.00.03    @@@@-門間    11/05/26  主保険＋マル長の請求対応
      *  01.00.04    @@@@-門間    11/12/07 「1 期別にまとめて発行する」を指定した場合、
      *                                     複数科保険でまとめて計算する
      *  01.00.05    @@@@-門間    12/01/30  主保険＋マル長の請求対応を削除
      *  01.00.06    @@@@-門間    13/12/13  月途中の保険組合せ変更による請求期間印字を修正
      *  01.00.07    @@@@-門間    14/01/17  第三者行為対応
      *  01.00.08    @@@@-門間    14/02/07  プログラムオプション対応（訂正印字）
      *  01.00.09    @@@@-門間    14/03/24  消費税対応
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    16/08/08  「消費税(再掲)」欄へ前回分の消費税が記載される不具合を修正
      *                                     「外来未収額」「入院未収額」欄へ不明な未収金が記載される不具合を修正
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
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
      *
           03  PRT-SRYYMD              PIC X(08).
           03  PRT-PTID                PIC 9(10).
      ****
           03  PRT-PRTID               PIC X(30).
           03  PRT-PRTNM               PIC X(20).
           03  PRT-SITEKBN             PIC X(01).
           03  PRT-PRTDATA             PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUNOU2         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-HIZUKECHK       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX-M               PIC 9(04).
           03  IDX-JIHI            PIC 9(04).
           03  IDXD                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-YMD             PIC X(10).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-HKNCOMBI        PIC X(40).
      *
           03  WRK-GOKTEN          PIC S9(11).
      *
           03  WRK-SKJRYO          PIC S9(11).
           03  WRK-LIFERYO         PIC S9(11).
      *
           03  WRK-DENPNUM         PIC 9(07).
      *
           03  WRK-WARI            PIC 9(02)V9.
           03  WRK-Z6              PIC ZZZZZZ.
           03  WRK-Z62             PIC ZZZ9.9.
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-NO              PIC Z(7).
      *
           03  WRK-KINGAKU-Z           PIC ----,---,---.
           03  WRK-KINGAKU-9           PIC ----,---,--9.
      *
           03  WRK-ZHENSYU-G.
               05  WRK-ZHENSYU12       PIC X(12).
               05  WRK-ZHENSYU24       PIC X(24).
      *    自費集計
           03  WRK-JIHI-REC.
               05  WRK-JIHI            PIC S9(07)   OCCURS  10.
           03  WRK-JIHI-TOTAL          PIC S9(08).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-PRTNM               PIC X(20).
           03  WRK-ROOM                PIC X(50).
           03  WRK-BRMNUM-NAME         PIC X(50).
           03  WRK-BRMNUM-IN           PIC X(06).
           03  WRK-BRMNUM-EDT          PIC X(06).
           03  WRK-BRMNUM-OT           PIC X(06).
           03  WRK-STR1                PIC X(20).
           03  WRK-STR2                PIC X(20).
           03  WRK-IDX1                PIC 9(05).
           03  WRK-IDX2                PIC 9(05).
           03  WRK-IDX3                PIC 9(05).
           03  WRK-SKYMISYUPRTKBN      PIC X(01).
           03  WRK-GAI-MISYU           PIC S9(09).
           03  WRK-NYU-MISYU           PIC S9(09).
           03  WRK-SKYMONEY-SKJ        PIC S9(09).
           03  WRK-BTUNUM              PIC X(02).
           03  WRK-BRMNUM              PIC X(06).
      *
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
           03  WRK-MSG                 PIC X(1000).
           03  WRK-HONKZKKBN           PIC X(04).
      *
      *    日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    帳票カウント
       01  PRT-CNT                     PIC 9(10)   VALUE   ZERO.
      *
      *    未収編集用ワーク
       01  MISYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //MISYU-//.
      *
       01  CONST-AREA.
           03  CONST-H200401      PIC X(08)    VALUE   "20080401".
      *
      *    プログラムオプションパラメタ
       01  TPARA-AREA.
           03  TPARA-TEISEIKBN          PIC X(01).
           03  TPARA-CHUI-YUKOSTYMD     PIC X(08).
           03  TPARA-CHUI-YUKOEDYMD     PIC X(08).
           03  TPARA-CHUI               PIC X(100).
      *
           03  WRK-PARA-NAME            PIC X(25).
           03  WRK-PARA                 PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *    請求書自費名称情報
           COPY    "CPSK1013.INC".
      *    収納情報
           COPY    "CPSK1039.INC".
      *    医療機関基本情報
           COPY    "CPSK5000.INC".
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1034.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院請求書
           COPY "A00000H03NA4.INC" REPLACING  //HCN03V02//
                                   BY         //HCN03//.
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSGDAY.INC".
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
      *    請求期間編集サブ
           COPY    "CPORCHCN03S03.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    未収額編集
           COPY    "CPORCSMISYU.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    請求書兼領収書共通パラメタ
           COPY    "COMMON-SPA".
           COPY    "CPORCHCN03.INC".
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       STS-AREA
                                       WRK-AREA
                                       IDX-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
           PERFORM VARYING IDX-M   FROM    1   BY  1
                   UNTIL ( IDX-M   >   ORCHCN03-CNT )
      *
      *        主処理
               PERFORM 200-MAIN-SEC
      *
           END-PERFORM
      *
      *    帳票印刷処理
           PERFORM 300-PRINT-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1001-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
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
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key10"             TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1002-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
            MOVE    "tbl_syskanri"     TO  MCP-TABLE
            MOVE    "key10"            TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"        USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
            MOVE    "tbl_syskanri"         TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC TO  SYS-1002-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key10"             TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    ORCHCN03-SYSYMD     TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
      *
           MOVE    SYS-1034-PRTNM(3)
                                       TO  WRK-PRTNM
      *
      *    収納機能情報
           MOVE    SPACE               TO  SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
      *
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
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
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1039-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1039-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    労災医療機関情報編集
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    ORCHCN03-SYSYMD     TO  SYS-4001-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-4001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
      *    医療機関基本情報
           MOVE    SPACE               TO  SYS-5000-REC
           INITIALIZE                  SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
           MOVE    ORCHCN03-SYSYMD     TO  SYS-5000-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-5000-EDYUKYMD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-5000-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-5000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           MOVE    SYS-5000-SKYMISYUPRTKBN
                                       TO  WRK-SKYMISYUPRTKBN
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HCN3"              TO  HC01PARA-FORM-ID
           MOVE    ORCHCN03-TERMID     TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    1                   TO  HC01PARA-CNT
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT               PRT-FILE
      *
           PERFORM 100-OPTION-SELECT-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  TPARA-AREA
           MOVE    "1"             TO  TPARA-TEISEIKBN
           MOVE    "00000000"      TO  TPARA-CHUI-YUKOSTYMD
           MOVE    "99999999"      TO  TPARA-CHUI-YUKOEDYMD
      *
      *    （訂正）表示
           MOVE    "TEISEIKBN"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-TEISEIKBN
           END-IF
      *
      *    注意書き有効開始日
           MOVE    "CHUI_YUKOSTYMD" TO WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-CHUI-YUKOSTYMD
           END-IF
      *
      *    注意書き有効終了日
           MOVE    "CHUI_YUKOEDYMD" TO WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-CHUI-YUKOEDYMD
           END-IF
      *
      *    注意書き
           MOVE    "CHUI"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  TPARA-CHUI
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA
      *
           INITIALIZE                      ORCSPRGOPTIONAREA
           MOVE    "A00000H03N"        TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
      *
           EVALUATE    WRK-PARA-NAME
           WHEN    "CHUI"
               MOVE    ORCSPRGOPTION-OT-PARA (1:100)
                                       TO  KANACHK-MAE-INPUT
           WHEN    "CHUI_YUKOSTYMD"
           WHEN    "CHUI_YUKOEDYMD"
               MOVE    ORCSPRGOPTION-OT-PARA(1:10)
                                       TO  KANACHK-MAE-INPUT
           WHEN    OTHER
               MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  KANACHK-MAE-INPUT
           END-EVALUATE
      *
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                      SECTION.
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    ORCHCN03-HOSPNUM (IDX-M)
                                       TO  PTINF-HOSPNUM
           MOVE    ORCHCN03-PTID   (IDX-M)
                                       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF           =   1 )
               DISPLAY "*** PTINF  INV :"   PTINF-KEY
               GO      TO      200-MAIN-EXT
           END-IF
      *    収納マスタ検索処理
           PERFORM 2001-SYUNOU-SEL-SEC
      *
           INITIALIZE                  ORCHCN03S02AREA
           MOVE    IDX-M       TO      ORCHCN03S02-IDX
           CALL    "ORCHCN03S02"       USING
                                       ORCHCN03AREA
                                       ORCHCN03S02AREA
                                       SYUNOU-REC
                                       SPA-AREA
           IF    ( ORCHCN03S02-PRTFLG  =   1 )
      *      入院請求書編集処理
             PERFORM 2001-HCN03-HEN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       2001-SYUNOU-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    ORCHCN03-HOSPNUM (IDX-M)
                                       TO  SYU-HOSPNUM
           MOVE    ORCHCN03-NYUGAIKBN (IDX-M)
                                       TO  SYU-NYUGAIKBN
           MOVE    ORCHCN03-PTID (IDX-M)
                                       TO  SYU-PTID
           MOVE    ORCHCN03-DENPNUM (IDX-M)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"       TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       2001-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2001-HCN03-HEN-SEC              SECTION.
      *
           INITIALIZE                  ORCHCN03S03AREA
           MOVE    SYU-PTID            TO  ORCHCN03S03-PTID
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   ORCHCN03S02-GRP-CNT )
               MOVE    ORCHCN03S02-GRP-DENPNUM (IDXD)
                                   TO  ORCHCN03S03-DENPNUM (IDXD)
           END-PERFORM
           MOVE    SYU-SKYSTYMD    TO  ORCHCN03S03-SYUSTYMD
           MOVE    SYU-SKYEDYMD    TO  ORCHCN03S03-SYUEDYMD
           CALL    "ORCHCN03S03"       USING
                                       SPA-AREA
                                       ORCHCN03S03AREA
      *
           INITIALIZE                  HCN03
      *
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  WRK-NO
           MOVE    WRK-NO              TO  HCN03-NO
      *
      *    診療科名検索処理
           PERFORM 3102-SRYKA-SEL-SEC
      *
      *    診療科
           MOVE    SYS-1005-SRYKANAME2 TO  HCN03-MIDASI
      *    患者番号
           MOVE    ORCHCN03-PTNUM (IDX-M)
                                       TO  HCN03-KNJNO
      *    患者カナ氏名
           MOVE    PTINF-KANANAME      TO  HCN03-KANANAME
      
      *    患者名称
           MOVE    PTINF-NAME
                                       TO  HCN03-KNJNAMW
      *
      *    病棟・病室名編集処理
           PERFORM 3102-ROOM-HEN-SEC
           MOVE    WRK-ROOM            TO  HCN03-ROOM
      *
      *    保険組合せ名称編集処理
           INITIALIZE       HKNCOMBI-REC
           MOVE    ORCHCN03-HOSPNUM (IDX-M)  TO   COMB-HOSPNUM
           MOVE    ORCHCN03-PTID (IDX-M)     TO   COMB-PTID
           MOVE	   SYU-HKNCOMBINUM           TO   COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF  FLG-HKNCOMBI  =  1
             INITIALIZE    HKNCOMBI-REC
           END-IF
      *
           INITIALIZE       PTHKNINF-REC
           MOVE    ORCHCN03-HOSPNUM (IDX-M)  TO   PTHKN-HOSPNUM
           MOVE    ORCHCN03-PTID (IDX-M)     TO   PTHKN-PTID
           MOVE    COMB-HKNID                TO   PTHKN-HKNID
           PERFORM 910-PTHKNINF-KEY-SEL-SEC
      *
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
      *        患者労災保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    ORCHCN03-HOSPNUM(IDX-M)
                                           TO  PTRSI-HOSPNUM
               MOVE    ORCHCN03-PTID  (IDX-M)
                                           TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM 910-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
      *    保険組合せ名称編集処理
           PERFORM 3102-HKNCOMBI-HEN-SEC
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY         TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:22)      TO  HCN03-TANJYOYMD(1:22)
           MOVE   "生"   TO     HCN03-TANJYOYMD(23:2)
      *    性別
           EVALUATE    PTINF-SEX
             WHEN    "1"
               MOVE    "男"           TO  HCN03-SEX
             WHEN    "2"
               MOVE    "女"           TO  HCN03-SEX
           END-EVALUATE
      *    本人家族区分
           MOVE    WRK-HONKZKKBN          TO  HCN03-HONKZKKBN
      *    保険種別
           MOVE    WRK-HKNCOMBI        TO  HCN03-HOKEN
      *    負担割合
           IF      SYU-PTFTNRATE       >   ZERO
               COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
               IF      SYU-PTFTNRATE(3:1)  =   ZERO
                   MOVE    WRK-WARI            TO  WRK-Z6
                   MOVE    WRK-Z6              TO  HCN03-FUTAN
               ELSE
                   MOVE    WRK-WARI            TO  WRK-Z62
                   MOVE    WRK-Z62             TO  HCN03-FUTAN
               END-IF
           END-IF
      *    請求開始日
           MOVE    ORCHCN03S03-SKYSTYMD
                                       TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCN03-NYUINSTRYMD
      *    請求終了日
           MOVE    ORCHCN03S03-SKYEDYMD
                                       TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCN03-NYUINENDYMD
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCN03-HAKYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCN03-IRYOADRS
           INSPECT HCN03-IRYOADRS      REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCN03-IRYOMEI
           INSPECT HCN03-IRYOMEI       REPLACING ALL "  " BY "　"
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCN03-IRYOTEL
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
           MOVE    ZERO                TO  WRK-KINGAKU-Z
           MOVE    SPACE               TO  WRK-ZHENSYU-G
      *
      *    請求内容
           PERFORM VARYING     IDX1    FROM  1   BY  1
                   UNTIL       IDX1    >     17
               MOVE    SYU-HKNTEN(IDX1)    TO  WRK-KINGAKU-Z
               MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU24       TO  HCN03-HKNRYO(IDX1)
           END-PERFORM
      *
           IF    ( COMB-HKNNUM             =    "971"   AND
                   PTRSI-HKNKBN        NOT =    "5"     AND
                   SYS-4001-RSIIRYOKBN     =    "1" )   OR
                 ( COMB-HKNNUM             =    "971"   AND
                   PTRSI-HKNKBN            =    "5"     AND
                   SYU-PTFTNRATE           =    ZERO )  OR
                 ( COMB-HKNNUM             =    "973"   AND
                   SYU-PTFTNRATE           =    ZERO )
               PERFORM VARYING     IDX1    FROM  1   BY  1
                       UNTIL       IDX1    >     16
                   MOVE    SPACE           TO  HCN03-HKNRYO(IDX1)
               END-PERFORM
               MOVE    ALL "*"             TO  HCN03-HKNRYO(17)
           END-IF
      *
      *    本人家族区分
           EVALUATE  COMB-HONKZKKBN
             WHEN  1
               MOVE    "本人"   TO    HCN03-HONKZKKBN
             WHEN  2
               MOVE    "家族"   TO    HCN03-HONKZKKBN
             WHEN  OTHER
               MOVE    SPACE    TO    HCN03-HONKZKKBN
           END-EVALUATE
      *
           IF    ( ORCHCN03S02-FUKUSU-COMBI   =    1 )
               MOVE    "複数保険使用"  TO  HCN03-HOKEN
               MOVE    SPACE           TO  HCN03-FUTAN
               MOVE    SPACE           TO  HCN03-HONKZKKBN
           END-IF
      *
      *    労災・自賠の診察料等の記載
           IF   (SYU-RSI-TOTAL     NOT =   ZERO) AND
                (SYS-4001-RSIPRTKBN    =   "2")
             EVALUATE    COMB-HKNNUM
               WHEN    "971"
                 MOVE  "（内労災診察等）"     TO      HCN03-RSIMIDASI
               WHEN    "973"
                 MOVE  "（内自賠診察等）"     TO      HCN03-RSIMIDASI
               WHEN    OTHER
                 IF    ( COMB-KOHHKNNUM (1)   =   "970" )
                   MOVE  "（内自賠文書等）"     TO    HCN03-RSIMIDASI
                 END-IF
             END-EVALUATE
             MOVE    SYU-RSI-TOTAL      TO  WRK-KINGAKU-Z
             MOVE    WRK-KINGAKU-Z      TO  WRK-ZHENSYU12
             PERFORM 800-HENKAN-SEC
             MOVE    WRK-ZHENSYU24      TO      HCN03-RSIBUN
             MOVE    "円"               TO      HCN03-RSIEN
           ELSE
             MOVE    SPACE              TO      HCN03-RSIMIDASI
             MOVE    SPACE              TO      HCN03-RSIBUN
             MOVE    SPACE              TO      HCN03-RSIEN
           END-IF
      *
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY         TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z           TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24           TO  HCN03-HOKENBUN
      *
      *    食事療養、生活療養の編集
           MOVE    ZERO                    TO  WRK-SKJRYO
           COMPUTE    WRK-SKJRYO   =
                      SYU-RYOYOHI-SKJ      +    SYU-RYOYOHI-SKJ-JIHI
           MOVE    WRK-SKJRYO              TO   WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z           TO   WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24           TO   HCN03-SKJRYO
      *
           MOVE    ZERO                    TO   WRK-LIFERYO
           COMPUTE    WRK-LIFERYO   =
                      SYU-RYOYOHI-LIFE     +    SYU-RYOYOHI-LIFE-JIHI
           MOVE    WRK-LIFERYO             TO   WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z           TO   WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24           TO   HCN03-LIFERYO
      *
      *    労災・自賠の時、点数表示なし
           IF    ( COMB-HKNNUM             =    "971"
                                           OR   "973" )
            OR   ( COMB-KOHHKNNUM (1)      =    "970"
              AND  SYU-PTFTNRATE       NOT =     ZERO )
             IF  ( COMB-HKNNUM             =    "971"   AND
                   PTRSI-HKNKBN        NOT =    "5"     AND
                   SYS-4001-RSIIRYOKBN     =    "1" )   OR
                 ( COMB-HKNNUM             =    "971"   AND
                   PTRSI-HKNKBN            =    "5"     AND
                   SYU-PTFTNRATE           =    ZERO )  OR
                 ( COMB-HKNNUM             =    "973"   AND
                   SYU-PTFTNRATE           =    ZERO )
               MOVE    SPACE    TO    HCN03-HOKENBUN
                                      HCN03-SKJRYO
                                      HCN03-LIFERYO
             ELSE
               COMPUTE  WRK-GOKTEN  =  SYU-HKNTEKMONEY
                                    +  SYU-RSISHOSHIN-MONEY
                                    +  SYU-RSISAISHIN-MONEY
                                    +  SYU-RSISDO-MONEY
                                    +  SYU-RSIETC-MONEY
                                    +  SYU-KOH-FTN
               MOVE  WRK-GOKTEN     TO  WRK-KINGAKU-Z
               MOVE  WRK-KINGAKU-Z  TO  WRK-ZHENSYU12
               PERFORM  800-HENKAN-SEC
               MOVE  WRK-ZHENSYU24  TO  HCN03-HOKENBUN
             END-IF
           END-IF
      *
      *    保険適用外分
           COMPUTE WRK-GOKTEN
                =  SYU-TOTAL-TGMONEY
                +  SYU-TOTAL-TGMONEY-TAX
      *
           MOVE    WRK-GOKTEN          TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-HOKENGAI
      *
      *    保険適用外内訳編集
           PERFORM     VARYING   IDX-JIHI FROM  1  BY  1
                       UNTIL     IDX-JIHI  >  16
               COMPUTE   WRK-KINGAKU-Z   =  SYU-TGMONEY(IDX-JIHI)  +
                                            SYU-TGMONEY-TAX(IDX-JIHI)
               MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
               PERFORM   800-HENKAN-SEC
               MOVE    WRK-ZHENSYU24       TO  HCN03-JIHIBUN(IDX-JIHI)
           END-PERFORM
      *
      *    保険適用外消費税（再掲）
           MOVE    SYU-TGMONEY-TAX-SAI     TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z           TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24            TO  HCN03-HOKENZEI
      *
      *    自費
           MOVE    ZERO                TO  WRK-JIHI-REC
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL       IDX1     >   10
               COMPUTE WRK-JIHI (IDX1)     =   SYU-JIHI-TAXNASI (IDX1)
                                           +   SYU-JIHI-TAXARI  (IDX1)
               MOVE    WRK-JIHI (IDX1)     TO  WRK-KINGAKU-Z
               MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU24       TO  HCN03-JIHI(IDX1)
           END-PERFORM
      *
      *    自費計
           COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
           MOVE    WRK-JIHI-TOTAL      TO  WRK-KINGAKU-Z
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-JIHI(12)
      *    自費合計
           IF      SYU-SRYYMD          <=  "20070331"
             COMPUTE WRK-GOKTEN        =   WRK-JIHI-TOTAL
                                       +   SYU-JIHI-TAX
           ELSE
             MOVE    WRK-JIHI-TOTAL    TO  WRK-GOKTEN
           END-IF
           MOVE    WRK-GOKTEN          TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-JIHI(11)
      *    食事・入院時生活療養費負担額
           COMPUTE WRK-SKYMONEY-SKJ
               =   SYU-SKYMONEY-SKJ-KEI
               +   SYU-SKYMONEY-SKJ-JIHI-KEI
               +   SYU-SKYMONEY-LIFE-KEI
               +   SYU-SKYMONEY-LIFE-JIHI-KEI
           MOVE    WRK-SKYMONEY-SKJ    TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-SYOKUJI
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-ROUJIN
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-KOUHI
      *    室料差額
           MOVE    SYU-RMSAGAKU        TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-RMSAGAKU
      *    室料差額消費税（再掲）
           MOVE    SYU-RMSAGAKU-TAX-SAI TO WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-RMZEI
      *    調整金
           MOVE    SYU-CHOSEI          TO  WRK-KINGAKU-Z
           MOVE    WRK-KINGAKU-Z       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-CHOSEI
      *    今回請求額
           MOVE    ORCHCN03-SEIKYU (IDX-M)
                                       TO  WRK-KINGAKU-9
           MOVE    WRK-KINGAKU-9       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-KONSEIKYU
      *    今回請求額消費税（再掲）
           MOVE    ORCHCN03-SEIKYU-TAX-SAI (IDX-M)
                                       TO  WRK-KINGAKU-9
           MOVE    WRK-KINGAKU-9       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-KONZEI
      *    今回入金額
           MOVE    ORCHCN03-NYUKIN (IDX-M)
                                       TO  WRK-KINGAKU-9
           MOVE    WRK-KINGAKU-9       TO  WRK-ZHENSYU12
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU24       TO  HCN03-KONNYUKIN
      *
           IF    ( WRK-SKYMISYUPRTKBN  =   "1" )
      *        未収額編集処理
               PERFORM 3102-MISYU-EDIT-SEC
               MOVE    "外　来　未　収　額"
                                       TO  HCN03-LBL-G-MISYU
               MOVE    "円"            TO  HCN03-LBL-G-MISYU-EN
               MOVE    WRK-GAI-MISYU   TO  WRK-KINGAKU-9
               MOVE    WRK-KINGAKU-9   TO  WRK-ZHENSYU12
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU24   TO  HCN03-GAI-MISYUKIN
      *
               MOVE    "入　院　未　収　額"
                                       TO  HCN03-LBL-N-MISYU
               MOVE    "円"            TO  HCN03-LBL-N-MISYU-EN
               MOVE    WRK-NYU-MISYU   TO  WRK-KINGAKU-9
               MOVE    WRK-KINGAKU-9   TO  WRK-ZHENSYU12
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU24   TO  HCN03-NYU-MISYUKIN
           END-IF
      *
      *    見出し
           PERFORM  20011-MEISAI-HEN-SEC
      *
      *    発行区分
           EVALUATE    ORCHCN03-HAKKOFLG (IDX-M)
               WHEN    2
                   IF    ( TPARA-TEISEIKBN =   "1" )
                       MOVE    "（訂正）"  TO  HCN03-MIDASHI
                   END-IF
           END-EVALUATE
      *
      *    注意書き
           IF    ( SMCNDATE-YMD        >=  TPARA-CHUI-YUKOSTYMD )
            AND  ( SMCNDATE-YMD        <=  TPARA-CHUI-YUKOEDYMD )
               MOVE    TPARA-CHUI      TO  HCN03-CHUI
           END-IF
      *
           MOVE    HCN03-TYPE-A        TO  HCN03-TYPE-B
      *
      *    印刷用データ出力
           MOVE    SPACE               TO  PRT-REC
      *
           MOVE    SYU-SRYYMD          TO  PRT-SRYYMD
           MOVE    SYU-PTID            TO  PRT-PTID
      
           IF (SYU-SRYYMD  >=  CONST-H200401)
             MOVE    "A00000H03NA4N.red"  TO  PRT-PRTID
           ELSE
             MOVE    "A00000H03NA4O.red"  TO  PRT-PRTID
           END-IF
           MOVE    WRK-PRTNM           TO  PRT-PRTNM
           MOVE    "2"                 TO  PRT-SITEKBN
           MOVE    HCN03               TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           .
       2001-HCN03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細見出し編集処理
      *****************************************************************
       20011-MEISAI-HEN-SEC             SECTION.
      *
           MOVE  "初 ・ 再 診 料"      TO  HCN03-MEISAI (1)
           MOVE  "医 学 管 理 等"      TO  HCN03-MEISAI (2)
           MOVE  "在　宅　医　療"      TO  HCN03-MEISAI (3)
           MOVE  "投　　　　　薬"      TO  HCN03-MEISAI (4)
           MOVE  "注　　　　　射"      TO  HCN03-MEISAI (5)
           MOVE  "処　　　　　置"      TO  HCN03-MEISAI (6)
           MOVE  "手　　　　　術"      TO  HCN03-MEISAI (7)
           MOVE  "麻　　　　　酔"      TO  HCN03-MEISAI (8)
           MOVE  "検　　　　　査"      TO  HCN03-MEISAI (9)
           MOVE  "画　像　診　断"      TO  HCN03-MEISAI (10)
           MOVE  "リハビリテーション"  TO  HCN03-MEISAI (11)
           MOVE  "精神科専門療法"      TO  HCN03-MEISAI (12)
           MOVE  "放 射 線 治 療"      TO  HCN03-MEISAI (13)
           MOVE  "病　理　診　断"      TO  HCN03-MEISAI (14)
           MOVE  "入　院　料　等"      TO  HCN03-MEISAI (15)
           MOVE  "療養担当手当"        TO  HCN03-MEISAI (16)
      *    療養担当手当て
           IF    (SYU-HKNTEN (16)    =  ZERO)
             MOVE  SPACE               TO  HCN03-MEISAI (16)
           END-IF
      *
           .
       20011-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       800-HENKAN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZHENSYU12       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(7:24)    TO  WRK-ZHENSYU24
           END-IF
           .
       800-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科検索処理
      *****************************************************************
       3102-SRYKA-SEL-SEC              SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SYU-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1005-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO   MCP-TABLE
           MOVE    "key10"         TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key10"             TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                         SPA-AREA
      *
           .
       3102-SRYKA-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病棟・病室名編集処理
      *****************************************************************
       3102-ROOM-HEN-SEC                 SECTION.
      *
           MOVE    SYU-BYOTONUM        TO  WRK-BTUNUM
           MOVE    SYU-ROOMNUM(3:)     TO  WRK-BRMNUM
      *
      *    入院履歴検索処理
           PERFORM  900-PTNYUINRRK-KEY45-SEL-SEC
           IF  ( FLG-PTNYUINRRK  =  ZERO )
               MOVE    PTNYUINRRK-BTUNUM      TO  WRK-BTUNUM
               MOVE    PTNYUINRRK-BRMNUM(3:)  TO  WRK-BRMNUM
           END-IF
      *    病棟情報検索処理
           PERFORM 3102-SYS-5001-SEL-SEC
      *
      *    病室名編集処理
           MOVE    WRK-BRMNUM          TO  WRK-BRMNUM-IN
           PERFORM 3102-BRMNUM-EDIT-SEC
      *
           MOVE    SPACE               TO  WRK-ROOM
      *
           IF    ( WRK-BRMNUM-NAME     NOT =   SPACE )
               STRING  SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
                       "　"                DELIMITED   BY  SIZE
                       WRK-BRMNUM-NAME     DELIMITED   BY  SPACE
                       "号室"              DELIMITED   BY  SIZE
               INTO    WRK-ROOM
               END-STRING
           ELSE
               STRING  SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
                       "　　　　"          DELIMITED   BY  SIZE
                       "号室"              DELIMITED   BY  SIZE
               INTO    WRK-ROOM
               END-STRING
           END-IF
      *
           .
       3102-ROOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       3102-SYS-5001-SEL-SEC             SECTION.
      *
      *    病棟情報検索
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    WRK-BTUNUM          TO  SYS-5001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    ORCHCN03-SYSYMD     TO  SYS-5001-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-5001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-5001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key10"             TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       3102-SYS-5001-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       3102-BRMNUM-EDIT-SEC            SECTION.
      *
      *    病室番号左詰め処理
           PERFORM 3102-BRM-NUM-LEFT-EDIT-SEC
      *
      *    全角編集処理
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-BRMNUM-OT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-BRMNUM-NAME
           END-IF
      *
           .
       3102-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室番号左詰処理
      *****************************************************************
       3102-BRM-NUM-LEFT-EDIT-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-BRMNUM-OT
      *
           IF    ( WRK-BRMNUM-IN       =   SPACE )
               GO  TO  3102-BRMNUM-LEFT-EDIT-EXT
           END-IF
      *
           IF    ( WRK-BRMNUM-IN (1 : 1)  =   SPACE )
               MOVE    SPACE           TO  WRK-STR1
               MOVE    1               TO  WRK-IDX1
               UNSTRING    WRK-BRMNUM-IN
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX1
               END-UNSTRING
               MOVE    WRK-BRMNUM-IN (WRK-IDX1 : )
                                       TO  WRK-BRMNUM-EDT
           ELSE
               MOVE    WRK-BRMNUM-IN  TO  WRK-BRMNUM-EDT
           END-IF
      *
           PERFORM VARYING WRK-IDX1    FROM    6   BY  -1
               UNTIL     ( WRK-IDX1    <   1 )
                 OR      ( WRK-BRMNUM-EDT (WRK-IDX1 : 1)
                                   NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    WRK-BRMNUM-EDT TO  WRK-BRMNUM-OT
               GO  TO  3102-BRMNUM-LEFT-EDIT-EXT
           END-IF
      *
      *    病室番号が数字の場合前ゼロをとる
           IF    ( WRK-BRMNUM-EDT ( 1 : WRK-IDX1 )
                                       IS  NUMERIC )
               IF    ( WRK-BRMNUM-EDT ( 1 : 1 )
                                       =   ZERO )
                   MOVE    1               TO  WRK-IDX2
                   MOVE    SPACE           TO  WRK-STR1
                   UNSTRING    WRK-BRMNUM-EDT
                       DELIMITED BY  ALL ZERO
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX2
                   END-UNSTRING
                   MOVE    WRK-BRMNUM-EDT (WRK-IDX2 : )
                                           TO  WRK-BRMNUM-OT
               ELSE
                   MOVE    WRK-BRMNUM-EDT TO  WRK-BRMNUM-OT
               END-IF
           ELSE
                   MOVE    WRK-BRMNUM-EDT TO  WRK-BRMNUM-OT
           END-IF
      *
           .
       3102-BRMNUM-LEFT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       3102-HKNCOMBI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           IF  ( COMB-HKNNUM        =  "971" )
               AND  (PTRSI-HKNKBN   =   "5" )
             MOVE   PTHKN-KIGO     TO    WRK-HKNCOMBI
           ELSE
               IF    ( SYU-SRYYMD      >=     "20081001" )
                 AND  ( COMB-HKNNUM      =      "001" )
                    INSPECT COMB-SYU-TANSEIDONAME
                           REPLACING    ALL    "政管"   BY  "協会"
               END-IF
             STRING  COMB-SYU-TANSEIDONAME    DELIMITED  BY  SPACE
                     "　"                     DELIMITED  BY  SIZE
                     COMB-KOH1-TANSEIDONAME   DELIMITED  BY  SPACE
                     "　"                     DELIMITED  BY  SIZE
                     COMB-KOH2-TANSEIDONAME   DELIMITED  BY  SPACE
                     "　"                     DELIMITED  BY  SIZE
                     COMB-KOH3-TANSEIDONAME   DELIMITED  BY  SPACE
                     "　"                     DELIMITED  BY  SIZE
                     COMB-KOH4-TANSEIDONAME   DELIMITED  BY  SPACE
             INTO              WRK-HKNCOMBI
             END-STRING
           END-IF
           .
       3102-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE     SPA-HOSPNUM        TO SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1013-STYUKYMD
           MOVE    ORCHCN03-SYSYMD     TO  SYS-1013-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1013-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL       FLG-SYSKANRI    =   1
               IF      ORCHCN03-NYUGAIKBN (IDX-M)
                                       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (10)
                   END-EVALUATE
               END-IF
      *
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1013-REC
               END-IF
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    未収額編集処理
      *****************************************************************
       3102-MISYU-EDIT-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-NYU-MISYU
                                       WRK-GAI-MISYU
      *
           INITIALIZE              SMISYU-AREA
           CALL    "ORCSMISYU"     USING
                                   SYUNOU-REC
                                   SMISYU-AREA
                                   SPA-AREA
      *
           MOVE    SMISYU-NYUIN    TO  WRK-NYU-MISYU
           MOVE    SMISYU-GAIRAI   TO  WRK-GAI-MISYU
      *
           IF    ( SYS-1039-MISYUKIKANKBN  =   "1" )
               CONTINUE
           ELSE
               COMPUTE WRK-NYU-MISYU
                   =   WRK-NYU-MISYU   -   ORCHCN03S02-MYMISYU
           END-IF
      *
           .
       3102-MISYU-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       300-PRINT-SEC                SECTION.
      *
           CLOSE   PRT-FILE
      *
           CALL    "ORCHCN03S01"   USING   ORCHCN03AREA
                                           ASGNPARA
                                           SPA-AREA
      *
           .
       300-PRINT-EXT.
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
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-HIZUKECHK
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKECHK
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
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
           MOVE    ORCHCN03-SYSYMD TO  SYS-1900-STYUKYMD
           MOVE    ORCHCN03-SYSYMD TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(11) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
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
           MOVE    SYS-1900-PRTKBN(11)
                                   TO  SYS-1901-KBNCD
           MOVE    ORCHCN03-SYSYMD TO  SYS-1901-STYUKYMD
           MOVE    ORCHCN03-SYSYMD TO  SYS-1901-EDYUKYMD
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
           MOVE    SYS-1900-PRTKBN(11)
                                   TO  SYS-1902-KBNCD
           MOVE    ORCHCN03-SYSYMD TO  SYS-1902-STYUKYMD
           MOVE    ORCHCN03-SYSYMD TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF   FLG-SYSKANRI   =   ZERO
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
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             MOVE    "DBFETCH"         TO  MCP-FUNC
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
      *
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
           
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
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
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       910-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理(KEY45)
      *****************************************************************
       900-PTNYUINRRK-KEY45-SEL-SEC         SECTION.
      *
           MOVE    ZERO                 TO  FLG-PTNYUINRRK
      *
           INITIALIZE                   PTNYUINRRK-REC
           MOVE    SYU-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SYU-PTID             TO  PTNYUINRRK-PTID
           MOVE    SYU-DENPPRTYMD       TO  PTNYUINRRK-TENNYUYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key45"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  ( MCP-RC  =  ZERO )
             MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
             MOVE    "key45"            TO  MCP-PATHNAME
             MOVE    "DBFETCH"          TO  MCP-FUNC
             CALL    "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  PTNYUINRRK-REC
               MOVE    ZERO             TO  FLG-PTNYUINRRK
             ELSE
                 MOVE    1              TO  FLG-PTNYUINRRK
             END-IF
           ELSE
             INITIALIZE                 PTNYUINRRK-REC
             MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key45"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNYUINRRK-KEY45-SEL-EXT.
           EXIT.
      
      *****************************************************************
      *    患者保険情報読み込み
      *****************************************************************
       910-PTHKNINF-KEY-SEL-SEC    SECTION.
      *
           MOVE    ZERO               TO  FLG-PTHKNINF
      * 
           MOVE    PTHKNINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  ( MCP-RC  =  ZERO )
             MOVE    "tbl_pthkninf"   TO  MCP-TABLE
             MOVE    "key"            TO  MCP-PATHNAME
             MOVE    "DBFETCH"        TO  MCP-FUNC
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  ( MCP-RC  =  ZERO )
               MOVE    MCPDATA-REC    TO  PTHKNINF-REC
               MOVE    ZERO           TO  FLG-PTHKNINF
             ELSE
               INITIALIZE             PTHKNINF-REC
               MOVE    1              TO  FLG-PTHKNINF
             END-IF
           ELSE
             INITIALIZE               PTHKNINF-REC
             MOVE    1                TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-PTHKNINF-KEY-SEL-EXT.
           EXIT.
