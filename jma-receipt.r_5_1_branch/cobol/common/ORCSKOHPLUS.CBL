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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSKOHPLUS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 特別公費対応
      *  コンポーネント名  : 公費付加情報取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/11    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−小豆沢 03/10/07  引数にレセプト請求区分の追加
      *  01.00.02    NACL−小豆沢 06/12/15  MONFUNC対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/02/29  平成２０年４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RJNHKN          PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-KOHPLUS         PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  KOH-IDX             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-RYAKU-SRYCD             PIC X(09).
           03  WRK-RYAKUSHO                PIC X(14).
           03  WRK-SRYKBN                  PIC X(02).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-ZAISKBKBN               PIC X(01).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    公費付加情報
       01  KOHPLUS-REC.
           COPY    "CPKOHPLUS.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    公費付加情報取得用パラメタ
           COPY    "CPORCSKOHPLUS.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSKOHPLUS-AREA
grpsys     SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-SKOHPLUS-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-SKOHPLUS-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SKOHPLUS-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-SKOHPLUS-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      ***  DISPLAY "KOHPLUS PRM-CHK-SEC"
           IF    ( LNK-SKOHPLUS-SRYYMD    =   SPACE )
               MOVE    1               TO  LNK-SKOHPLUS-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-SKOHPLUS-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-SKOHPLUS-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      ***  DISPLAY "KOHPLUS MAIN-SEC"
      *保険組合せ情報取得
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE                       HKNCOMBI-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM
                                       TO   COMB-HOSPNUM
           MOVE    LNK-SKOHPLUS-PTID   TO   COMB-PTID
           MOVE    LNK-SKOHPLUS-HKNCOMBI
                                       TO   COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI        =    ZERO
               MOVE    "tbl_hkncombi"  TO   MCP-TABLE
               MOVE    "key"           TO   MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
      *    保険組合せクローズ
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *老人保健該当者かを判定
           IF     COMB-KOHHKNNUM(1)    =    "027"
           OR     COMB-HKNNUM          =    "039"
                  MOVE     1           TO   FLG-RJNHKN
           END-IF
      *
      *地方公費の格納位置特定
           PERFORM  VARYING    IDX   FROM   1   BY  1
                    UNTIL     (COMB-KOHHKNNUM(IDX)  =   SPACE)  OR
                              (IDX   >   4)
                 IF   (COMB-KOHHKNNUM(IDX)  >=    "100")   AND
                      (COMB-KOHHKNNUM(IDX)  <=    "899")
                      MOVE   IDX      TO    KOH-IDX
                 END-IF
           END-PERFORM
      *
           IF       KOH-IDX   =   ZERO
                MOVE    3              TO  LNK-SKOHPLUS-RC
                GO   TO    200-MAIN-EXT
           END-IF
      *市町村番号の取得（公費負担者番号の５桁目から長さ３桁）
           INITIALIZE                         PTKOHINF-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM   TO  PTKOH-HOSPNUM
           MOVE    LNK-SKOHPLUS-PTID      TO  PTKOH-PTID
           MOVE    COMB-KOHID(KOH-IDX)    TO  PTKOH-KOHID
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           PERFORM 910-PTKOHINF-SELECT-SEC
           IF      FLG-PTKOHINF           =   ZERO
               MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM   910-PTKOHINF-READ-SEC
           END-IF
      *    患者公費情報クローズ
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *公費付加情報テーブルより情報取得
           INITIALIZE                         KOHPLUS-REC
grpsys     MOVE    LNK-SKOHPLUS-HOSPNUM   TO  KOHPLUS-HOSPNUM
           MOVE    LNK-SKOHPLUS-PREFNUM   TO  KOHPLUS-PREFNUM
           MOVE    PTKOH-FTNJANUM(5:3)    TO  KOHPLUS-CITYNUM
           MOVE    COMB-KOHHKNNUM(KOH-IDX) TO KOHPLUS-KOHNUM
           MOVE    "0"                    TO  KOHPLUS-PTKBN
           MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOSTYMD
           MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOEDYMD
           MOVE    "tbl_kohplus"          TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           PERFORM 910-KOHPLUS-SELECT-SEC
           IF      FLG-KOHPLUS            =   ZERO
               MOVE    "tbl_kohplus"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM   910-KOHPLUS-READ-SEC
               IF    FLG-KOHPLUS          =   ZERO
      ***          DISPLAY "KOHPLUS FETCH COMPLETE"
      *            返却領域のセット
                   MOVE   KOHPLUS-FTNKBN  TO LNK-SKOHPLUS-FTNKBN
                   MOVE   KOHPLUS-NYUSKJRYYKBN
                                          TO  LNK-SKOHPLUS-NYUSKJRYYKBN
                   MOVE   KOHPLUS-RECESKYKBN
                                          TO  LNK-SKOHPLUS-RECESKYKBN
               ELSE
      *            公費付加情報クローズ
                   MOVE    "tbl_kohplus"          TO  MCP-TABLE
                   MOVE    "key2"                 TO  MCP-PATHNAME
                   PERFORM   910-DB-CLOSE-SEC
                   INITIALIZE                         KOHPLUS-REC
grpsys             MOVE    LNK-SKOHPLUS-HOSPNUM   TO  KOHPLUS-HOSPNUM
                   MOVE    LNK-SKOHPLUS-PREFNUM   TO  KOHPLUS-PREFNUM
                   MOVE    PTKOH-FTNJANUM(5:3)    TO  KOHPLUS-CITYNUM
                   MOVE    COMB-KOHHKNNUM(KOH-IDX) TO KOHPLUS-KOHNUM
                   IF      FLG-RJNHKN             =   ZERO
                       MOVE    "1"                TO  KOHPLUS-PTKBN
                   ELSE
                       MOVE    "2"                TO  KOHPLUS-PTKBN
                   END-IF
                   MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOSTYMD
                   MOVE    LNK-SKOHPLUS-SRYYMD    TO  KOHPLUS-YUKOEDYMD
                   MOVE    "tbl_kohplus"          TO  MCP-TABLE
                   MOVE    "key2"                 TO  MCP-PATHNAME
                   PERFORM 910-KOHPLUS-SELECT-SEC
                   IF      FLG-KOHPLUS            =   ZERO
                       MOVE    "tbl_kohplus"      TO  MCP-TABLE
                       MOVE    "key2"             TO  MCP-PATHNAME
                       PERFORM   910-KOHPLUS-READ-SEC
                       IF    FLG-KOHPLUS          =   ZERO
      ***                  DISPLAY "KOHPLUS FETCH COMPLETE"
      *                    返却領域のセット
                           MOVE   KOHPLUS-FTNKBN TO LNK-SKOHPLUS-FTNKBN
                           MOVE   KOHPLUS-NYUSKJRYYKBN
                                          TO  LNK-SKOHPLUS-NYUSKJRYYKBN
                           MOVE   KOHPLUS-RECESKYKBN
                                          TO  LNK-SKOHPLUS-RECESKYKBN
                       ELSE
      ***                  DISPLAY "KOHPLUS FETCH ERR "
                           MOVE    3              TO  LNK-SKOHPLUS-RC
                       END-IF
                   ELSE
                       DISPLAY "KOHPLUS SELECT ERR "
                       MOVE    3              TO  LNK-SKOHPLUS-RC
                   END-IF
               END-IF
           ELSE
               DISPLAY "KOHPLUS SELECT ERR "
               MOVE    3              TO  LNK-SKOHPLUS-RC
           END-IF
      *    公費付加情報クローズ
           MOVE    "tbl_kohplus"          TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"       TO  MCP-TABLE
           MOVE     "key"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報検索
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC        SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    MCPDATA-REC      TO  PTKOHINF-REC
               MOVE    ZERO             TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                   PTKOHINF-REC
               MOVE    1                TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費付加情報マスター検索
      *****************************************************************
       910-KOHPLUS-SELECT-SEC        SECTION.
      *
           MOVE    KOHPLUS-REC         TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-KOHPLUS
           ELSE
               MOVE    1               TO  FLG-KOHPLUS
           END-IF
      *
           .
       910-KOHPLUS-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費付加情報マスター読込
      *****************************************************************
       910-KOHPLUS-READ-SEC        SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    MCPDATA-REC      TO  KOHPLUS-REC
               MOVE    ZERO             TO  FLG-KOHPLUS
           ELSE
               INITIALIZE                   KOHPLUS-REC
               MOVE    1                TO  FLG-KOHPLUS
           END-IF
      *
           .
       910-KOHPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
