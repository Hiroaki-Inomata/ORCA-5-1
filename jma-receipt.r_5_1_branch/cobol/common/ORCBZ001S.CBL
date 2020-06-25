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
       PROGRAM-ID.                 ORCBZ001S.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 随時処理
      *  コンポーネント名  : 空き患者番号編集サブ
      *  管理者            : 
      *  03/06/19    NACL-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/05/15  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-S                   PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
      *
           03  WRK-RENNUM              PIC 9(10).
           03  WRK-PTNUM2              PIC X(20).
           03  WRK-K1                  PIC 9(04).
      *
           03  WRK-ST-PTNUM            PIC X(20).
           03  WRK-ED-PTNUM            PIC X(20).
           03  WRK-HEN-PTNUM           PIC X(20).
      *
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-KETAN           PIC 9(04).
      *
      *****************************************************************
      *    サブプロ定義
      *****************************************************************
      *
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    空き患者番号編集サブ
           COPY    "CPORCBZ001S.INC".
      *    システム管理（患者番号構成情報）
           COPY    "CPSK1009.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCBZ001SAREA
           SYS-1009-REC
           .
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           IF      ORCBZ001S-RC    NOT =   ZERO
               GO      TO      000-PROC-EXT
           END-IF
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
      *    自由構成の数字と標準構成以外は対象外とする
           IF     ((SYS-1009-PTNUMKSIKBN   =   "1" )  AND
                   (SYS-1009-JIYKSIKBN     =   "2" ))   OR
                  ( SYS-1009-PTNUMKSIKBN   =   "2" )
               MOVE    ZERO                TO  ORCBZ001S-RC
           ELSE
               MOVE    1                   TO  ORCBZ001S-RC
               GO      TO      100-INIT-EXT
           END-IF
      *
      *    患者番号桁数計算
           MOVE    ZERO                    TO WRK-KETA
           EVALUATE    SYS-1009-PTNUMKSIKBN
               WHEN    "1"
                   MOVE    SYS-1009-JIYKSIKETA     TO  WRK-KETA
               WHEN    "2"
                   MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  WRK-KETA
                   IF      SYS-1009-HJNKSINENKBN   =   "1"
      *                和暦
                       MOVE    3                  TO  WRK-KETA2
                   ELSE
      *                西暦
                       MOVE    4                  TO  WRK-KETA2
                   END-IF
                   EVALUATE    SYS-1009-HJNKSIKBN
                       WHEN    "1"
                           COMPUTE WRK-KETA    =   WRK-KETA
                                               +   WRK-KETA2
                                               +   1
                       WHEN    "2"
                           COMPUTE WRK-KETA    =   WRK-KETA
                                               +   WRK-KETA2
                       WHEN    "3"
                           COMPUTE WRK-KETA    =   WRK-KETA
                                               +   1
                       WHEN    "4"
                           CONTINUE
                   END-EVALUATE
           END-EVALUATE
           .
       100-INIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    開始患者番号
           IF      ORCBZ001S-MAX-CNT   =   ZERO
               MOVE    SPACE               TO  WRK-ST-PTNUM
           ELSE
               MOVE    ORCBZ001S-PTNUM (ORCBZ001S-MAX-CNT)
                                           TO  WRK-ST-PTNUM
           END-IF
           MOVE    SPACE               TO  WRK-ED-PTNUM
      *    最大件数
           INITIALIZE                  ORCBZ001S-MAX-CNT
           INITIALIZE                  ORCBZ001S-PTNUM-AREA
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    WRK-ST-PTNUM        TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
      *    対象がない時、システム管理の連番を判定する
           IF      FLG-PTNUM           =   1
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               PERFORM 2002-RENNUM-CHK-SEC
               GO      TO     200-MAIN-EXT
           END-IF
           MOVE    ZERO                TO  IDX
           COMPUTE WRK-KETA2           =   WRK-KETA    +   1
           PERFORM UNTIL   (FLG-PTNUM      =   1  )  OR
                           (IDX            >=  120)  OR
                           (FLG-END        =   1  )
      *
      *        現在の構成以外を対象外とする
               IF     (PTNUM-PTNUM(1:WRK-KETA)    =   ZERO  )
                OR    (PTNUM-PTNUM(WRK-KETA2:)    NOT =   SPACE )
                OR    (PTNUM-PTNUM(1:WRK-KETA)    NOT NUMERIC)
                OR    (PTNUM-PTNUM                =   WRK-ST-PTNUM)
                   CONTINUE
               ELSE
      *        開始から終了までの空き番を編集する
                   MOVE    PTNUM-PTNUM         TO  WRK-ED-PTNUM
                   PERFORM 2001-PTNUM-HEN-SEC
      *
                   MOVE    PTNUM-PTNUM         TO  WRK-ST-PTNUM
               END-IF
      *        最大患者番号までとする
               IF      (ORCBZ001S-LST-PTNUM    NOT =   SPACE )  AND
                       (ORCBZ001S-LST-PTNUM    <   WRK-ST-PTNUM)
                   MOVE    1                   TO  FLG-END
               ELSE
      *
                   MOVE    "tbl_ptnum"         TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-PTNUM-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    対象がない時、システム管理の連番を判定する
           IF     (FLG-PTNUM           =   1  )  AND
                  (FLG-END             =   ZERO) AND
                  (ORCBZ001S-MAX-CNT   =   ZERO)
               PERFORM 2002-RENNUM-CHK-SEC
               GO      TO     200-MAIN-EXT
           END-IF
      *
      *    終了
           IF      FLG-PTNUM           =   1
      *        システム管理の連番号以下の場合
               IF      WRK-RENNUM          <   SYS-1009-RENNUM
                   MOVE    ZERO                TO  ORCBZ001S-ENDFLG
               ELSE
                   MOVE    1                   TO  ORCBZ001S-ENDFLG
               END-IF
           ELSE
               MOVE    ZERO                TO  ORCBZ001S-ENDFLG
           END-IF
      *    終了
           IF      FLG-END             =   1
               MOVE    1                   TO  ORCBZ001S-ENDFLG
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    空き番号編集処理
      *****************************************************************
       2001-PTNUM-HEN-SEC              SECTION.
      *
           IF      WRK-ST-PTNUM        =   SPACE
               MOVE    ZERO                TO  WRK-ST-PTNUM
           END-IF
      *
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL      (FLG-ERR     =   1   )  OR
                              (FLG-END     =   1   )  OR
                              (IDX         >=  120 )
      *        患者番号構成決定処理
               PERFORM 20011-NUM-HENSYU-SEC
      *        最大患者番号までとする
               IF      (ORCBZ001S-LST-PTNUM    NOT =   SPACE )  AND
                       (ORCBZ001S-LST-PTNUM    <   WRK-ST-PTNUM)
                   MOVE    1                   TO  FLG-END
               END-IF
               IF     (FLG-ERR             =   1    )  OR
                      (FLG-END             =   1    )  OR
                      (WRK-ST-PTNUM        =   WRK-ED-PTNUM)
                   MOVE    1                   TO  FLG-ERR
               ELSE
      *            対象番号編集
                   ADD     1                   TO  IDX
                   MOVE    IDX                 TO  ORCBZ001S-MAX-CNT
                   MOVE    WRK-ST-PTNUM        TO  ORCBZ001S-PTNUM(IDX)
               END-IF
           END-PERFORM
      *
           .
       2001-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者構成連番編集処理
      *****************************************************************
       20011-NUM-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  WRK-RENNUM
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *    自由構成
             WHEN    "1"
      *        数字タイプの時、連番
               IF      SYS-1009-JIYKSIKBN      =   "2"
      *            自由構成桁数
                   MOVE    SYS-1009-JIYKSIKETA     TO  WRK-K1
                   MOVE    WRK-ST-PTNUM(1:WRK-K1)  TO  WRK-RENNUM
      *            最終連番より小さいこと
                   IF      WRK-RENNUM          >=  SYS-1009-RENNUM
                       MOVE    1               TO  FLG-END
                       GO      TO      20011-NUM-HENSYU-EXT
                   END-IF
                   ADD     1                       TO  WRK-RENNUM
                   IF      SYS-1009-JIYKSIKETA     <=  10
                       COMPUTE IDY         =   10
                                           -   SYS-1009-JIYKSIKETA
                                           +   1
                       MOVE    SPACE                   TO  WRK-ST-PTNUM
                       MOVE    WRK-RENNUM(IDY:WRK-K1)  TO  WRK-ST-PTNUM
                                                         (1:WRK-K1)
                   ELSE
                       COMPUTE IDY         =   SYS-1009-JIYKSIKETA
                                           -   10
                       ADD     1               TO  IDY
                       MOVE    WRK-RENNUM (1:10)
                                               TO  WRK-ST-PTNUM
                                                       (IDY:10)
                   END-IF
               END-IF
      *    標準構成
           WHEN    "2"
               EVALUATE    SYS-1009-HJNKSIKBN
      *        年 + 連番号 + 検証番号
               WHEN    "1"
      *        年 + 連番号
               WHEN   "2"
      *            同年のものを対象とする
                   IF      SYS-1009-HJNKSINENKBN  =   "1"
      *                和暦
                       IF     (WRK-ST-PTNUM(1:3)  =   SPA-SYSYMDW(1:3))
                           OR (WRK-ED-PTNUM(1:3)  >=  SPA-SYSYMDW(1:3))
                           CONTINUE
                       ELSE
                           MOVE    1                  TO  FLG-END
                           GO      TO      20011-NUM-HENSYU-EXT
                       END-IF
                   ELSE
      *                西暦
                       IF     (WRK-ST-PTNUM(1:4)  =   SPA-SYSYMD (1:4))
                           OR (WRK-ED-PTNUM(1:4)  >=  SPA-SYSYMD (1:4))
                           CONTINUE
                       ELSE
                           MOVE    1                  TO  FLG-END
                           GO      TO      20011-NUM-HENSYU-EXT
                       END-IF
                   END-IF
                   IF      SYS-1009-HJNKSINENKBN  =   "1"
      *                和暦
                       IF      WRK-ST-PTNUM(1:3)  NOT =   SPA-SYSYMDW
                                                                   (1:3)
                           MOVE    ZERO               TO  WRK-ST-PTNUM
                           MOVE    SPA-SYSYMDW(1:3)   TO  WRK-ST-PTNUM
                                                                (1:3)
                       END-IF
                       MOVE    4                  TO  IDX-S
                   ELSE
      *                西暦
                       IF      WRK-ST-PTNUM(1:4)  NOT =   SPA-SYSYMD
                                                                   (1:4)
                           MOVE    ZERO               TO  WRK-ST-PTNUM
                           MOVE    SPA-SYSYMD (1:4)   TO  WRK-ST-PTNUM
                                                                  (1:4)
                       END-IF
                       MOVE    5                  TO  IDX-S
                   END-IF
                   MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  WRK-K1
                   MOVE    ZERO                        TO  WRK-RENNUM
                   MOVE    WRK-ST-PTNUM(IDX-S:WRK-K1)  TO  WRK-RENNUM
      *            最終連番より小さいこと
                   IF      WRK-RENNUM          >=  SYS-1009-RENNUM
                       MOVE    1               TO  FLG-END
                       GO      TO      20011-NUM-HENSYU-EXT
                   END-IF
                   ADD     1                       TO  WRK-RENNUM
                   IF      SYS-1009-HJNKSIRENNUMKETA   <=  10
                       COMPUTE IDY         =   10
                                           -   SYS-1009-HJNKSIRENNUMKETA
                                           +   1
                       MOVE    SPACE           TO  WRK-ST-PTNUM(IDX-S:)
                       MOVE    WRK-RENNUM(IDY:WRK-K1)
                                               TO  WRK-ST-PTNUM
                                                         (IDX-S:WRK-K1)
                   ELSE
                       COMPUTE IDY         =   SYS-1009-HJNKSIRENNUMKETA
                                           -   10
                       COMPUTE IDY             =   IDY
                                               +   1
                       MOVE    SPACE           TO  WRK-ST-PTNUM(IDY:)
                       MOVE    WRK-RENNUM(1:10)
                                               TO  WRK-ST-PTNUM
                                                         (IDY:10)
                   END-IF
      *
      *        連番号 + 検証番号
               WHEN   "3"
      *        連番号
               WHEN   "4"
                   MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  WRK-K1
                   MOVE    WRK-ST-PTNUM(1:WRK-K1)  TO  WRK-RENNUM
      *            最終連番より小さいこと
                   IF      WRK-RENNUM          >=  SYS-1009-RENNUM
                       MOVE    1               TO  FLG-END
                       GO      TO      20011-NUM-HENSYU-EXT
                   END-IF
                   ADD     1                       TO  WRK-RENNUM
                   IF      SYS-1009-HJNKSIRENNUMKETA   <=  10
                       COMPUTE IDY         =   10
                                           -   SYS-1009-HJNKSIRENNUMKETA
                                           +   1
                       MOVE    SPACE                   TO  WRK-ST-PTNUM
                       MOVE    WRK-RENNUM(IDY:WRK-K1)  TO  WRK-ST-PTNUM
                                                         (1:WRK-K1)
                   ELSE
                       COMPUTE IDY         =   SYS-1009-HJNKSIRENNUMKETA
                                           -   10
                       ADD     1               TO  IDY
                       MOVE    WRK-RENNUM (1:10)
                                               TO  WRK-ST-PTNUM
                                                       (IDY:10)
                   END-IF
               END-EVALUATE
      *
      *        検証番号設定
               EVALUATE    SYS-1009-HJNKSIKBN
      *        <年 + 連番号 + 検証番号>
               WHEN    "1"
      *        <連番号 + 検証番号>
               WHEN   "3"
                   MOVE    SPACE               TO  WRK-PTNUM2
                   INITIALIZE                  LNK-CHKDGT-AREA
                   MOVE    1                   TO  LNK-I-SYORIKBN
                   MOVE    WRK-ST-PTNUM        TO  LNK-I-CODE
                   CALL    "ORCSCHKDGT"        USING
                                               LNK-CHKDGT-AREA
                   IF      LNK-O-ERRCD         =   ZERO
                       MOVE    LNK-O-CHKDGT    TO  WRK-PTNUM2 (1:1)
                   END-IF
                   MOVE    SPACE               TO  WRK-HEN-PTNUM
                   STRING  WRK-ST-PTNUM        DELIMITED  BY  " "
                           WRK-PTNUM2          DELIMITED  BY  " "
                        INTO                   WRK-HEN-PTNUM
                   END-STRING
                   MOVE    WRK-HEN-PTNUM       TO  WRK-ST-PTNUM
               END-EVALUATE
           END-EVALUATE
      *
           .
       20011-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理の連番処理
      *****************************************************************
       2002-RENNUM-CHK-SEC                     SECTION.
      *
           IF      WRK-ST-PTNUM        =   SPACE
               MOVE    ZERO                TO  ORCBZ001S-MAX-CNT
               MOVE    1                   TO  ORCBZ001S-ENDFLG
               GO      TO      2002-RENNUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ED-PTNUM
      *    標準構成で年が構成にある時
           IF     (SYS-1009-PTNUMKSIKBN    =   "2"         )  AND
                  (SYS-1009-HJNKSIKBN      =   "1"  OR  "2")
               IF      SYS-1009-HJNKSINENKBN  =   "1"
      *            和暦
                   MOVE    SPA-SYSYMDW(1:3)   TO  WRK-ED-PTNUM(1:3)
               ELSE
      *            西暦
                   MOVE    SPA-SYSYMD (1:4)   TO  WRK-ED-PTNUM(1:4)
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-ERR     =   1   )  OR
                              (FLG-END     =   1   )  OR
                              (IDX         >=  120 )
      *        患者番号構成決定処理
               PERFORM 20011-NUM-HENSYU-SEC
      *        最大患者番号までとする
               IF      (ORCBZ001S-LST-PTNUM    NOT =   SPACE )  AND
                       (ORCBZ001S-LST-PTNUM    <   WRK-ST-PTNUM)
                   MOVE    1                   TO  FLG-END
               END-IF
      *
               IF      FLG-END             =   1
                   CONTINUE
               ELSE
      *            対象番号編集
                   ADD     1                   TO  IDX
                   MOVE    IDX                 TO  ORCBZ001S-MAX-CNT
                   MOVE    WRK-ST-PTNUM        TO  ORCBZ001S-PTNUM(IDX)
               END-IF
           END-PERFORM
      *
      *    終了
           IF      FLG-END             =   ZERO
               MOVE    ZERO                TO  ORCBZ001S-ENDFLG
           ELSE
               MOVE    1                   TO  ORCBZ001S-ENDFLG
           END-IF
           .
       2002-RENNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読み込み
      *****************************************************************
       910-PTNUM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
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
      *    ＤＢクロース処理
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
