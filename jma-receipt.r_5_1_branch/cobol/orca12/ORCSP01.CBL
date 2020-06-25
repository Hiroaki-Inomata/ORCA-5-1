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
       PROGRAM-ID.             ORCSP01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者番号発行サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/12/04    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  05/01/06  年をシステム管理へ修正
      * 01.00.02     NACL-多々納  05/11/14  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 04.03.00     NACL-多々納  09/03/30  連番号判定は同年分のみとする
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-KANRIMST        PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHKNO           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PTNUM1          PIC X(20).
           03  WRK-PTNUM2          PIC X(20).
           03  WRK-PTNUM3          PIC X(20).
      *
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-KETAN           PIC 9(04).
           03  WRK-CODSU           PIC 9(04).
           03  WRK-RENNUM          PIC 9(10).
      *
           03  WRK-RENNUM-X        PIC X(10).
      *
           03  WRK-NEN-W.
               05  WRK-NEN-WGO         PIC X(01).
               05  WRK-NEN-WY          PIC 9(02).
           03  WRK-NEN-9               PIC 9(04).
      *
           03  WRK-K-NEN           PIC 9(04).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換（管理）
       01  PTNUM-REC.
           COPY  "CPPTNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *
           COPY    "MCPAREA".
      *****COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    パラメタ
           COPY    "CPORCSP01.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSP01AREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *    患者番号採番検索
           PERFORM 980-PTGETNUM-GET-SEC
           IF    FLG-KANRIMST      NOT =   ZERO
               MOVE    "1011"          TO  SPA-ERRCD
               DISPLAY "P012 SYS1009(PTGETNUM)ERR:" MCP-RC  "."
           END-IF
      *
           EVALUATE    ORCSP01-SYORI
      *        患者番号発行
               WHEN    "1"
               WHEN    "3"
                   PERFORM 100-HAKKOU-SEC
                           UNTIL    FLG-SYORI      =   1
      *        患者番号チェック
               WHEN    "2"
                   PERFORM 200-CHECK-SEC
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    患者番号発行処理
      *****************************************************************
       100-HAKKOU-SEC               SECTION.
      *
           MOVE    1                    TO  FLG-SYORI
      *    年の和暦変換
           PERFORM 1000-NEN-WAREKI-SEC
      *    連番の更新
           PERFORM 1001-RENNUM-SEC
           IF      SPA-ERRCD           =   SPACE   OR  "8000"
               CONTINUE
           ELSE
               GO      TO      100-HAKKOU-EXT
           END-IF
      *
      *    患者番号取得
           PERFORM 320-PTNUM-SET-SEC
      *
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *        2:標準構成
               WHEN    "2"
                   MOVE    SPACE               TO  ORCSP01-PTNUM
                   MOVE    SPACE               TO  ORCSP01-PTNUM1
                   MOVE    SPACE               TO  ORCSP01-PTNUM2
                   MOVE    SPACE               TO  ORCSP01-PTNUM3
                   STRING  WRK-PTNUM1  DELIMITED  BY  " "
                           WRK-PTNUM2  DELIMITED  BY  " "
                           WRK-PTNUM3  DELIMITED  BY  " "
                        INTO            ORCSP01-PTNUM
                   END-STRING
      *        3:拡張構成
               WHEN    "3"
                   MOVE    SPACE               TO  ORCSP01-PTNUM
                   MOVE    WRK-PTNUM1          TO  ORCSP01-PTNUM1
                   MOVE    WRK-PTNUM2          TO  ORCSP01-PTNUM2
                   MOVE    WRK-PTNUM3          TO  ORCSP01-PTNUM3
           END-EVALUATE
      *
      *    登録済みかのチェック
           IF      ORCSP01-PTNUM   NOT =   SPACE
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    ORCSP01-PTNUM       TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
               IF      SPTID-RC            =   ZERO
      *            既に患者番号が登録されている場合、再度取得
                   MOVE    ZERO            TO  FLG-SYORI
               END-IF
           END-IF
      *
           .
       100-HAKKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    再発行処理
      *****************************************************************
       1001-RENNUM-SEC               SECTION.
      *
      *!   患者ＩＤ発行（再発行以外の時）
      *!   IF      ORCSP01-SYORI   NOT =   "3"
      *!       IF      SYS-1009-PTID       =   ALL "9"
      *!           MOVE    "8001"              TO  SPA-ERRCD
      *!           MOVE    ZERO                TO  SYS-1009-PTID
      *!       END-IF
      *!       COMPUTE SYS-1009-PTID       =   SYS-1009-PTID  +   1
      *!       MOVE    SYS-1009-PTID       TO  ORCSP01-PTID
      *!   END-IF
      *
      *    連番桁数算定
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDXS
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *    標準構成
               WHEN    "2"
                   MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  IDX1
      *    拡張構成
               WHEN    "3"
                   MOVE    SYS-1009-KKCKSIRENNUMKETA   TO  IDX1
           END-EVALUATE
           IF      IDX1                >   ZERO
               IF      IDX1            <   10
                   COMPUTE IDXS        =   10
                                       -   IDX1
                                       +   1
               ELSE
                   MOVE    1           TO  IDXS
                   MOVE    10          TO  IDX1
               END-IF
           END-IF
      *
           IF     (IDXS                >   ZERO )  AND
                  (IDX1                >   ZERO )
               IF      SYS-1009-RENNUM(IDXS:IDX1)   =   ALL "9"
      *            標準構成の連番号のみの場合
                   IF      (SYS-1009-PTNUMKSIKBN   =   "2"  )  AND
                           (SYS-1009-HJNKSIKBN     =   "3"  OR  "4")
                       MOVE    "8002"              TO  SPA-ERRCD
                       GO      TO      1001-RENNUM-EXT
                   ELSE
                       MOVE    "8000"              TO  SPA-ERRCD
                       MOVE    ZERO                TO  SYS-1009-RENNUM
                   END-IF
               END-IF
           ELSE
               IF      SYS-1009-RENNUM         =   ALL "9"
                   MOVE    "8000"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SYS-1009-RENNUM
               END-IF
           END-IF
           COMPUTE   SYS-1009-RENNUM   =   SYS-1009-RENNUM  +   1
      *
      *    患者番号採番  更新
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1011"          TO  SPA-ERRCD
               DISPLAY "P012 SYSKANRI UPDATE:" MCP-RC "."
           END-IF
      *
           .
       1001-RENNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       320-PTNUM-SET-SEC               SECTION.
      *
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *    ● 2:標準構成
           WHEN    "2"
               EVALUATE    SYS-1009-HJNKSIKBN
      *        <年 + 連番号 + 検証番号>
               WHEN    "1"
                   INITIALIZE                     LNK-CHKDGT-AREA
                   IF  SYS-1009-HJNKSINENKBN  =   "1"
      *                和暦
      **************** MOVE    SPA-SYSYMDW(1:3)   TO  WRK-PTNUM1
                       MOVE    WRK-NEN-W          TO  WRK-PTNUM1
                   ELSE
      *                西暦
      *****************MOVE    SPA-SYSYMD (1:4)   TO  WRK-PTNUM1
                       MOVE    SYS-1009-NEN       TO  WRK-PTNUM1
                   END-IF
                   IF  SYS-1009-HJNKSIRENNUMKETA  <=  10
                       COMPUTE     IDY    =   10
                                          -   SYS-1009-HJNKSIRENNUMKETA
                       ADD         1      TO  IDY
                       MOVE    SYS-1009-RENNUM
                                   (IDY:SYS-1009-HJNKSIRENNUMKETA)
                                          TO  WRK-PTNUM2 (1:)
                   ELSE
                       MOVE    ALL "0"    TO  WRK-PTNUM2
                                          (1:SYS-1009-HJNKSIRENNUMKETA)
                       COMPUTE     IDY    =   SYS-1009-HJNKSIRENNUMKETA
                                          -   10
                       ADD         1      TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                          TO  WRK-PTNUM2 (IDY:10)
                   END-IF
                   MOVE    1              TO  LNK-I-SYORIKBN
                   STRING  WRK-PTNUM1 DELIMITED  BY  " "
                           WRK-PTNUM2 DELIMITED  BY  " "
                           INTO           LNK-I-CODE
                   END-STRING
                   CALL    "ORCSCHKDGT"   USING   LNK-CHKDGT-AREA
                   IF      LNK-O-ERRCD    =   ZERO
                       MOVE    LNK-O-CHKDGT   TO  WRK-PTNUM3 (1:1)
                   ELSE
                       MOVE    "1010"          TO  SPA-ERRCD
                   END-IF
      *        <年 + 連番号>
               WHEN   "2"
                   IF  SYS-1009-HJNKSINENKBN     =   "1"
      *                和暦
      *****************MOVE    SPA-SYSYMDW(1:3)  TO  WRK-PTNUM1
                       MOVE    WRK-NEN-W         TO  WRK-PTNUM1
                   ELSE
      *                西暦
      **************** MOVE    SPA-SYSYMD (1:4)  TO  WRK-PTNUM1
                       MOVE    SYS-1009-NEN      TO  WRK-PTNUM1
                   END-IF
                   IF  SYS-1009-HJNKSIRENNUMKETA <=  10
                       COMPUTE   IDY   =   10
                                       -   SYS-1009-HJNKSIRENNUMKETA
                       ADD         1       TO  IDY
                       MOVE    SYS-1009-RENNUM
                                (IDY:SYS-1009-HJNKSIRENNUMKETA)
                                           TO  WRK-PTNUM2 (1:)
                   ELSE
                       MOVE    ALL "0"     TO  WRK-PTNUM2
                                    (1:SYS-1009-HJNKSIRENNUMKETA)
                       COMPUTE IDY     =   SYS-1009-HJNKSIRENNUMKETA
                                       -   10
                       ADD     1           TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                           TO  WRK-PTNUM2 (IDY:10)
                   END-IF
      *
      *        <連番号 + 検証番号>
               WHEN   "3"
                   MOVE    SPACE           TO  WRK-PTNUM1
                   IF  SYS-1009-HJNKSIRENNUMKETA   <=  10
                       COMPUTE   IDY   =   10
                                       -   SYS-1009-HJNKSIRENNUMKETA
                       ADD         1       TO  IDY
                       MOVE    SYS-1009-RENNUM
                                    (IDY:SYS-1009-HJNKSIRENNUMKETA)
                                           TO  WRK-PTNUM1 (1:)
                   ELSE
                       MOVE    ALL "0"     TO  WRK-PTNUM1
                                    (1:SYS-1009-HJNKSIRENNUMKETA)
                       COMPUTE IDY     =   SYS-1009-HJNKSIRENNUMKETA
                                       -   10
                       ADD     1           TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                           TO  WRK-PTNUM1 (IDY:10)
                   END-IF
                   INITIALIZE                   LNK-CHKDGT-AREA
                   MOVE    1                TO  LNK-I-SYORIKBN
                   MOVE    WRK-PTNUM1   TO  LNK-I-CODE
                   CALL    "ORCSCHKDGT"     USING   LNK-CHKDGT-AREA
                   IF      LNK-O-ERRCD      =   ZERO
                       MOVE    LNK-O-CHKDGT TO  WRK-PTNUM2 (1:1)
                   END-IF
      *
      *        <連番号>
               WHEN   "4"
                   IF  SYS-1009-HJNKSIRENNUMKETA   <=   10
                       COMPUTE IDY     =   10
                                       -   SYS-1009-HJNKSIRENNUMKETA
                       ADD     1           TO  IDY
                       MOVE    SYS-1009-RENNUM
                                      (IDY:SYS-1009-HJNKSIRENNUMKETA)
                                           TO  WRK-PTNUM1
                   ELSE
                       MOVE    ALL "0"     TO  WRK-PTNUM1
                                        (1:SYS-1009-HJNKSIRENNUMKETA)
                       COMPUTE IDY     =   SYS-1009-HJNKSIRENNUMKETA
                                       -   10
                       ADD     1           TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                           TO  WRK-PTNUM1 (IDY:10)
                   END-IF
               WHEN   OTHER
                   MOVE    "1010"          TO  SPA-ERRCD
               END-EVALUATE
      *
      *
      *    ● 3:拡張構成
           WHEN    "3"
               EVALUATE    SYS-1009-KKCKSIKBN
      *        <フリー + 連番号 + フリー>
               WHEN    "1"
                   IF  SYS-1009-KKCKSIRENNUMKETA  <=  10
                       COMPUTE     IDY    =   10
                                          -   SYS-1009-KKCKSIRENNUMKETA
                       ADD         1      TO  IDY
                       MOVE    SYS-1009-RENNUM
                               (IDY:SYS-1009-KKCKSIRENNUMKETA)
                                          TO  WRK-PTNUM2 (1:)
                   ELSE
                       MOVE    ALL "0"    TO  WRK-PTNUM2
                                          (1:SYS-1009-KKCKSIRENNUMKETA)
                       COMPUTE     IDY    =   SYS-1009-KKCKSIRENNUMKETA
                                          -   10
                       ADD         1      TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                          TO  WRK-PTNUM2 (IDY:10)

                   END-IF
      *        <フリー + 連番号>
               WHEN   "2"
                   IF  SYS-1009-KKCKSIRENNUMKETA <=  10
                       COMPUTE   IDY    =   10
                                        -   SYS-1009-KKCKSIRENNUMKETA
                       ADD         1        TO  IDY
                       MOVE    SYS-1009-RENNUM
                                    (IDY:SYS-1009-KKCKSIRENNUMKETA)
                                            TO  WRK-PTNUM2 (1:)
                   ELSE
                       MOVE    ALL "0"     TO  WRK-PTNUM2
                                    (1:SYS-1009-KKCKSIRENNUMKETA)
                       COMPUTE IDY     =   SYS-1009-KKCKSIRENNUMKETA
                                       -   10
                       ADD     1           TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                           TO  WRK-PTNUM2 (IDY:10)

                   END-IF
      *
      *        <連番号 + フリー>
               WHEN   "3"
                   IF  SYS-1009-KKCKSIRENNUMKETA <=  10
                       COMPUTE   IDY    =   10
                                        -   SYS-1009-KKCKSIRENNUMKETA
                       ADD         1        TO  IDY
                       MOVE    SYS-1009-RENNUM
                                    (IDY:SYS-1009-KKCKSIRENNUMKETA)
                                            TO  WRK-PTNUM1 (1:)
                   ELSE
                       MOVE    ALL "0"      TO  WRK-PTNUM1
                                    (1:SYS-1009-KKCKSIRENNUMKETA)
                       COMPUTE   IDY    =   SYS-1009-KKCKSIRENNUMKETA
                                        -   10
                       ADD         1        TO  IDY
                       MOVE    SYS-1009-RENNUM (1:10)
                                            TO  WRK-PTNUM1 (IDY:10)
                   END-IF
      *
               WHEN   OTHER
                   MOVE    "1010"          TO  SPA-ERRCD
               END-EVALUATE
           WHEN    OTHER
               MOVE    "1010"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       320-PTNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       200-CHECK-SEC               SECTION.
      *
      *    年の和暦変換
           PERFORM 1000-NEN-WAREKI-SEC
      *    桁数チェック
           MOVE    ZERO                TO  WRK-CODSU
           PERFORM VARYING     IDX     FROM    20  BY  -1
                   UNTIL       IDX         <   1
               IF      ORCSP01-PTNUM(IDX:1)    NOT =   SPACE
                   MOVE    IDX                 TO  WRK-CODSU
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
      *
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
               WHEN    "3"
                   COMPUTE WRK-KETA        =  SYS-1009-KKCKSIMAEKETA
                                           +  SYS-1009-KKCKSIRENNUMKETA
                                           +  SYS-1009-KKCKSIATOKETA
           END-EVALUATE
           IF      WRK-CODSU       NOT =   WRK-KETA
               MOVE    "1005"          TO  SPA-ERRCD
               GO      TO      200-CHECK-EXT
           END-IF
      *
      *    連番チェック
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *        自由構成
               WHEN    "1"
      *            数字チェック
                   IF     (SYS-1009-JIYKSIKBN  =   "2"  )  AND
                          (ORCSP01-PTNUM(1:WRK-KETA)    NOT NUMERIC)
                       MOVE    "1006"          TO  SPA-ERRCD
                   END-IF
      *        標準構成
               WHEN    "2"
      *            数字チェック（すべて数値となる）
                   IF      ORCSP01-PTNUM(1:WRK-KETA)   NOT NUMERIC
                       MOVE    "1006"          TO  SPA-ERRCD
                       GO      TO      200-CHECK-EXT
                   END-IF
      *            連番号とのチェック
                   MOVE    SPACE               TO  WRK-RENNUM-X
                   MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  WRK-KETA
                   COMPUTE WRK-KETAN       =   WRK-KETA2   +   1
                   EVALUATE    SYS-1009-HJNKSIKBN
                       WHEN    "1"
                       WHEN    "2"
                           MOVE    ORCSP01-PTNUM (WRK-KETAN:WRK-KETA)
                                               TO  WRK-RENNUM-X
                       WHEN    "3"
                       WHEN    "4"
                           MOVE    ORCSP01-PTNUM (1:WRK-KETA)
                                               TO  WRK-RENNUM-X
                   END-EVALUATE
      *TEST
      *            年がある時前年は対象外
                   MOVE    ZERO                TO  FLG-CHKNO
                   EVALUATE    SYS-1009-HJNKSIKBN
                       WHEN    "2"
                           COMPUTE WRK-K-NEN       =   WRK-KETAN
                                                   -   1
      *                    和暦
                           IF      SYS-1009-HJNKSINENKBN  =   "1"
                               IF      ORCSP01-PTNUM(1:WRK-K-NEN)
                                                   <   WRK-NEN-W
                                   MOVE    1           TO  FLG-CHKNO
                               END-IF
                           ELSE
      *                    西暦
                               IF      ORCSP01-PTNUM(1:WRK-K-NEN)
                                                   <   SYS-1009-NEN
                                   MOVE    1           TO  FLG-CHKNO
                               END-IF
                           END-IF
                   END-EVALUATE
      *
                   PERFORM 2002-SNUM-SEC
                   IF      SYS-1009-RENNUM     <   WRK-RENNUM
                   AND   ( FLG-CHKNO           =   ZERO      )
                       MOVE    "1008"          TO  SPA-ERRCD
                       GO      TO      200-CHECK-EXT
                   END-IF
      *
      *            検証番号チェック
                   EVALUATE    SYS-1009-HJNKSIKBN
                       WHEN    "1"
                       WHEN    "3"
                           IF     (ORCSP01-PTNUM (WRK-CODSU:1)
                                                   NUMERIC   )  OR
                                  (ORCSP01-PTNUM (WRK-CODSU:1)
                                                   =   "*"  OR  "?" )
                               PERFORM 2001-KENBAN-CHK-SEC
                           ELSE
                               MOVE    "1007"      TO  SPA-ERRCD
                           END-IF
                   END-EVALUATE
      *        拡張構成
               WHEN    "3"
                   COMPUTE WRK-KETA        =  SYS-1009-KKCKSIMAEKETA
                                           +  1
                   MOVE    SPACE               TO  WRK-RENNUM-X
                   MOVE    ORCSP01-PTNUM
                                (WRK-KETA:SYS-1009-KKCKSIRENNUMKETA)
                                               TO  WRK-RENNUM-X
                   PERFORM 2002-SNUM-SEC
                   IF      SYS-1009-RENNUM     <    WRK-RENNUM
                       MOVE    "1008"          TO  SPA-ERRCD
                       GO      TO      200-CHECK-EXT
                   END-IF
           END-EVALUATE
      *
      *H30.10
      *    \ はエラー
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-CODSU )
                         OR   (SPA-ERRCD   NOT =   SPACE)
      *        \ はエラー
               IF      ORCSP01-PTNUM(IDX:1)    =   "\"
                  MOVE    "1013"           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
      *
      *!   IF      SPA-ERRCD       =   SPACE
      *!       IF      ORCSP01-PTID    =   ZERO
      *!           患者ＩＤの取得
      *!           PERFORM 2001-PTID-SET-SEC
      *!       END-IF
      *!   END-IF
           .
       200-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    数字変換取得処理
      *****************************************************************
       2002-SNUM-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-RENNUM-X        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC         NOT =   ZERO
               MOVE    ZERO                TO  WRK-RENNUM
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-RENNUM
           END-IF
           .
       2002-SNUM-EXT.
           EXIT.
      *****************************************************************
      *    患者ＩＤの取得処理
      *****************************************************************
       2001-PTID-SET-SEC               SECTION.
      *
      *    患者ＩＤ発行（再発行以外の時）
           IF      ORCSP01-SYORI   NOT =   "3"
               IF      SYS-1009-PTID       =   ALL "9"
                   MOVE    "8001"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SYS-1009-PTID
               END-IF
               COMPUTE SYS-1009-PTID       =   SYS-1009-PTID  +   1
               MOVE    SYS-1009-PTID       TO  ORCSP01-PTID
           END-IF
      *
      *    患者番号採番  更新
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1011"          TO  SPA-ERRCD
               DISPLAY "P012 SYSKANRI UPDATE:" MCP-RC "."
           END-IF
      *
           .
       2001-PTID-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検証番号チェック検索処理
      *****************************************************************
       2001-KENBAN-CHK-SEC          SECTION.
      *
           INITIALIZE                   LNK-CHKDGT-AREA
           IF      ORCSP01-PTNUM (WRK-CODSU:1)
                                       =   "*"  OR  "?"
               COMPUTE IDX2            =   WRK-CODSU    -  1
               MOVE    ORCSP01-PTNUM (1:IDX2)
                                            TO  LNK-I-CODE
               MOVE    1                    TO  LNK-I-SYORIKBN
           ELSE
               MOVE    ORCSP01-PTNUM        TO  LNK-I-CODE
               MOVE    2                    TO  LNK-I-SYORIKBN
           END-IF
      *
           CALL    "ORCSCHKDGT"     USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD         =   ZERO
               IF      ORCSP01-PTNUM (WRK-CODSU:1)
                                           =   "*"  OR  "?"
                   MOVE    LNK-O-CODE      TO  ORCSP01-PTNUM
               END-IF
           ELSE
               MOVE    "1007"              TO  SPA-ERRCD
           END-IF
      *
           .
       2001-KENBAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年和暦変換処理
      *****************************************************************
       1000-NEN-WAREKI-SEC             SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY1-YMD
           MOVE    SYS-1009-NEN        TO  LNK-DAY1-YMD(1:4)
      *R02.1
      *    令和対応(2019年12月とする
           IF     (SYS-1009-NEN    =   "2019"    )
              AND (SPA-SYSYMD      >=  "20200101")
               MOVE    20191201          TO  LNK-DAY1-YMD
           END-IF
      *
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:3)  TO  WRK-NEN-W
           ELSE
      *        エラーの時、平成に変換する
             IF     (SYS-1009-NEN    <   "2020"    )
                AND (SPA-SYSYMD      <   "20200101")
               MOVE    SYS-1009-NEN    TO  WRK-NEN-9
               COMPUTE WRK-NEN-WY      =   WRK-NEN-9
                                       -   1988
               MOVE    "4"             TO  WRK-NEN-WGO
      *R02.1
             ELSE
      *        令和
               MOVE    SYS-1009-NEN    TO  WRK-NEN-9
               COMPUTE WRK-NEN-WY      =   WRK-NEN-9
                                       -   2018
               MOVE    "5"             TO  WRK-NEN-WGO
             END-IF
           END-IF
      *
           .
       1000-NEN-WAREKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号採番検索
      *****************************************************************
       980-PTGETNUM-GET-SEC          SECTION.
      *
           INITIALIZE                  SYS-1009-REC.
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-KANRIMST
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       980-PTGETNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    ZERO                TO  FLG-KANRIMST
           ELSE
               MOVE    1                   TO  FLG-KANRIMST
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
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
      *
