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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSFTNRATE.
      ****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 保険組み合わせ負担割合取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
      *    添え字領域
       01  IDX-AREA.
           03  SYOTOKU-IDX                 PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-SYOTOKU                 PIC X(01).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-1001-CHIHOUNUMKBN       PIC X(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    システム管理
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    患者所得情報編集サブ
           COPY    "CPORCSSYOTOKU.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSFTNRATE.INC".
           COPY    "COMMON-SPA".
      *    保険組み合わせ
       01  LNK-HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC"    REPLACING
                                       //COMB-// BY //LNK-COMB-//.
      *
       PROCEDURE                    DIVISION    USING
           ORCSFTNRATEAREA
           SPA-AREA
           LNK-HKNCOMBI-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE             LNK-FTNRATE-HENKYAKU-AREA
           MOVE     ZERO    TO    LNK-FTNRATE-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
           IF    LNK-FTNRATE-RC   NOT =   ZERO
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-FTNRATE-RC       =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    患者ＩＤチェック
           IF      LNK-FTNRATE-PTID          =   ZERO
               MOVE    1                 TO  LNK-FTNRATE-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    処理年月日チェック
           IF      LNK-FTNRATE-SRYYMD        =   SPACE
               MOVE    2                 TO  LNK-FTNRATE-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    保険組合せ番号チェック
           IF      LNK-FTNRATE-HKNCOMBINUM   =   SPACE
               MOVE    3                 TO  LNK-FTNRATE-RC
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
           MOVE    ZERO                    TO  FLG-OK
           MOVE    SPACE                   TO  FLG-SYOTOKU
           MOVE    SPACE                   TO  WRK-1001-CHIHOUNUMKBN
      *
           IF      LNK-FTNRATE-KBN         =   "1"
      *        保険組合せパラメタ編集
               MOVE    LNK-HKNCOMBI-REC    TO  HKNCOMBI-REC
           ELSE
      *        保険組合せ検索
               PERFORM 2001-HKNCOMBI-GET-SEC
               IF    FLG-OK  =  ZERO
                   MOVE     4              TO  LNK-FTNRATE-RC
               END-IF
           END-IF
      *
      *
           IF      LNK-FTNRATE-RC      =   ZERO
      *
              MOVE  COMB-NYU-KFTNRATE  TO  LNK-FTNRATE-NYUIN
              MOVE  COMB-GAI-KFTNRATE  TO  LNK-FTNRATE-GAIRAI
      *
              IF   (COMB-KOH1HKNNUM(1:1) >= "1" AND <= "8")
              OR   (COMB-KOH2HKNNUM(1:1) >= "1" AND <= "8")
              OR   (COMB-KOH3HKNNUM(1:1) >= "1" AND <= "8")
              OR   (COMB-KOH4HKNNUM(1:1) >= "1" AND <= "8")
                 INITIALIZE                    ORCSSYOTOKUAREA
                 MOVE  LNK-FTNRATE-HOSPNUM TO  LNK-SYOTOKU-HOSPNUM
                 MOVE  LNK-FTNRATE-PTID    TO  LNK-SYOTOKU-PTID
                 MOVE  LNK-FTNRATE-SRYYMD(1:6)
                                           TO  LNK-SYOTOKU-SRYYM
                 MOVE  SPACE               TO  LNK-SYOTOKU-SKJCHKFLG
                 CALL  "ORCSSYOTOKU"       USING  ORCSSYOTOKUAREA
                                                  SPA-AREA
                 IF    LNK-SYOTOKU-RC    =   ZERO
                   IF    LNK-FTNRATE-SRYYMD(1:6)   >=  "200604"
                     MOVE   LNK-FTNRATE-SRYYMD(7:2)
                                           TO  SYOTOKU-IDX
                   ELSE
                     MOVE   1              TO  SYOTOKU-IDX
                   END-IF
                   MOVE LNK-SYOTOKU-CHIKBN(SYOTOKU-IDX)
                                           TO  FLG-SYOTOKU
                 END-IF
      *
                 EVALUATE   FLG-SYOTOKU
                   WHEN     "2"
                     PERFORM   2001-CHIHOUNUMKBN-GET-SEC
                     IF  WRK-1001-CHIHOUNUMKBN  =  "1"
                       IF  COMB-TSY-NYUFTNKBN  NOT =  "0"
                         MOVE  COMB-TSY-NYUKFTNRATE
                                                 TO  LNK-FTNRATE-NYUIN
                       END-IF
                       IF  COMB-TSY-GAIFTNKBN  NOT =  "0"
                         MOVE  COMB-TSY-GAIKFTNRATE
                                                 TO  LNK-FTNRATE-GAIRAI
                       END-IF
                     END-IF
                   WHEN     "1"
                   WHEN     "4"
                     PERFORM   2001-CHIHOUNUMKBN-GET-SEC
                     IF  WRK-1001-CHIHOUNUMKBN  =  "1"
                       IF  COMB-TNK-NYUFTNKBN  NOT =  "0"
                         MOVE  COMB-TNK-NYUKFTNRATE
                                                 TO  LNK-FTNRATE-NYUIN
                       END-IF
                       IF  COMB-TNK-GAIFTNKBN  NOT =  "0"
                         MOVE  COMB-TNK-GAIKFTNRATE
                                                 TO  LNK-FTNRATE-GAIRAI
                       END-IF
                     END-IF
                 END-EVALUATE
              END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費保険番号タブ区分取得
      *****************************************************************
       2001-HKNCOMBI-GET-SEC         SECTION.
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-FTNRATE-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    LNK-FTNRATE-PTID        TO  COMB-PTID
           MOVE    LNK-FTNRATE-HKNCOMBINUM TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC            =     ZERO
              MOVE    "tbl_hkncombi"       TO  MCP-TABLE
              MOVE    "key"                TO  MCP-PATHNAME
              PERFORM  900-HKNCOMBI-READ-SEC
              IF   FLG-HKNCOMBI      =     ZERO
                   MOVE     1              TO  FLG-OK
              END-IF
           ELSE
              INITIALIZE                       HKNCOMBI-REC
              MOVE     1                   TO  FLG-HKNCOMBI
           END-IF
           MOVE       "tbl_hkncombi"       TO  MCP-TABLE
           MOVE       "key"                TO  MCP-PATHNAME
           PERFORM     910-DB-CLOSE-SEC
      *
           .
       2001-HKNCOMBI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費保険番号タブ区分取得
      *****************************************************************
       2001-CHIHOUNUMKBN-GET-SEC         SECTION.
      *
           INITIALIZE                         SYS-1001-REC
           MOVE    "1001"               TO    SYS-1001-KANRICD
           MOVE    "*"                  TO    SYS-1001-KBNCD
           MOVE    LNK-FTNRATE-HOSPNUM  TO    SYS-1001-HOSPNUM
           MOVE    LNK-FTNRATE-SRYYMD   TO    SYS-1001-STYUKYMD
                                              SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE    "tbl_syskanri"   TO    MCP-TABLE
               MOVE    "key10"          TO    MCP-PATHNAME
               PERFORM   900-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI   =   ZERO
                  MOVE    SYS-1001-CHIHOUNUMKBN
                                        TO    WRK-1001-CHIHOUNUMKBN
               END-IF
           END-IF
           MOVE     "tbl_syskanri"      TO    MCP-TABLE
           MOVE     "key10"             TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       2001-CHIHOUNUMKBN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
      *
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
