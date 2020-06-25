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
       PROGRAM-ID.             ORCSORDERPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 入院オーダー印刷ＤＢ作成サブ
      *                     （Ｋ０２Ｎ，Ｋ０８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/16    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−多々納 05/05/18  処方せんコメント追加
      *  01.00.02    NACL-多々納  06/01/25  MONFUNCへ変更
      *  03.05.00    NACL-多々納  07/05/23  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/22  器材商品名コード対応
      *  04.03.00    NACL-多々納  08/08/26  院外処方対応追加
      *  04.04.00    NACL-多々納  08/12/22  出来高算定コード対応
      *  04.05.00    NACL-多々納  09/10/XX  換算値数量追加対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.08.00    NACL-多々納  14/05/XX  一時保存データ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-DAY             PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-MAYAKU          PIC 9(01).
           03  FLG-MAYAKU-ARI      PIC 9(01).
           03  FLG-MAYAKU-NASI     PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-COMMENT         PIC 9(01).
           03  FLG-SYOHOU-ARI      PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDY                     PIC 9(04).
      *
           03  IDX-KAI1            PIC 9(04).
           03  IDX-KAI2            PIC 9(02).
           03  IDX-KAI3            PIC 9(02).
           03  IDX-KAI4            PIC 9(04).
           03  IDX-2               PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD              PIC X(08).
      *
           03  WRK-PRTKBN              PIC  X(01).
           03  WRK-ORDERNUM            PIC  9(02).
           03  WRK-RENNUM              PIC  9(02).
           03  WRK-HZN-PRTKBN          PIC  X(01).
      *
           03  WRK-ORDERNUM-1          PIC  9(02).
           03  WRK-ORDERNUM-2          PIC  9(02).
           03  WRK-ORDERNUM-3          PIC  9(02).
           03  WRK-ORDERNUM-4          PIC  9(02).
           03  WRK-ORDERNUM-5          PIC  9(02).
           03  WRK-ORDERNUM-6          PIC  9(02).
           03  WRK-RENNUM-1            PIC  9(02).
           03  WRK-RENNUM-2            PIC  9(02).
           03  WRK-RENNUM-3            PIC  9(02).
      *
       01  TBL-KAISUU-AREA.
           03  TBL-KAISUU-OCCU         OCCURS   10.
               05  TBL-KAISU           PIC 9(03).
               05  TBL-DAY             PIC 9(02)
                                       OCCURS   31.
      *回数保存
       01  TBL-ORDER-DAY-AREA.
           03  TBL-ORDER-DAY-G.
               05  TBL-ORDER-DAY       PIC 9(03)
                                       OCCURS   31.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院帳票オーダ
       01  ORDERPRT-REC.
           COPY    "CPORDERPRT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   200.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *v4.8
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      **** COPY    "ORCA-DBPATH".
      *
           COPY    "MCPAREA".
      *
           COPY    "ORCA-SPA".
      *
      *    診療行為入力項目変換パラメタ（回数の設定に使用）
      *    COPY    "CPORCSC97.INC".
      *
      *    ＳＰＡパラメタ（ORCSC97.CBL 用のDAMMY)
      *01  SPA-SCR-REC.
      *    COPY    "CPK02SPASCR.INC".
      *H25.12 変更
      *    一括日変換パラメタ
           COPY    "CPORCSNDAYCHG.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    入院指示せんオーダ作成サブ
           COPY    "CPORCSORDERPRT.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSORDERPRTAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *****INITIALIZE                  STS-AREA
           INITIALIZE                  LNK-ORDERPRT-RCD
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    キーの編集発行日
           MOVE    SMCNDATE-YMD        TO  LNK-ORDERPRT-PRINTYMD
      *    発行時間
           MOVE    SMCNDATE-HMS        TO  LNK-ORDERPRT-PRINTHMS
      *
           EVALUATE    LNK-ORDERPRT-KBN
      *        ワーク診療行為マスタより作成
               WHEN    "1"
                   PERFORM 100-WKSRYACT-SYORI-SEC
      *        パラメタより作成
               WHEN    "2"
                   PERFORM 200-PARA-SYORI-SEC
           END-EVALUATE
      *    コメントのみの場合、削除
           IF      FLG-COMMENT         =   1
               IF      FLG-SYOHOU-ARI      =   ZERO
                   PERFORM 300-COMMENT-DELETE-SEC
               END-IF
           END-IF
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ワーク診療行為マスタより作成処理
      *****************************************************************
       100-WKSRYACT-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
           MOVE    LNK-ORDERPRT-DOUJITSUKBN
                                       TO  WKSRY-DOUJITSUKBN
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WKSRYACT-REC
               MOVE    ZERO                TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
      *        剤毎に保存する
               IF     (LNK-WKSRYACT-MAX   >    ZERO  )  AND
                      (LNK-WKSRY-ZAINUM(LNK-WKSRYACT-MAX)
                                      NOT =   WKSRY-ZAINUM)
                   PERFORM 1001-ORDERPRT-SYORI-SEC
                   MOVE    ZERO                TO  LNK-WKSRYACT-MAX
                   MOVE    SPACE               TO  LNK-WKSRYACT-AREA
               END-IF
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   200
                   DISPLAY "ORCSORDER LNK-WKSRYACT-MAX 200 OVR"
                   MOVE    2               TO  LNK-ORDERPRT-RCD
               ELSE
                   MOVE    WKSRYACT-REC    TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  WKSRYACT-REC
                   MOVE    ZERO                TO  FLG-WKSRYACT
               ELSE
                   INITIALIZE                  WKSRYACT-REC
                   MOVE    1                   TO  FLG-WKSRYACT
               END-IF
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    剤毎に保存する
           IF      LNK-WKSRYACT-MAX   >    ZERO
               PERFORM 1001-ORDERPRT-SYORI-SEC
           END-IF
      *
           .
       100-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   入院オーダー印刷マスタ登録処理
      *****************************************************************
       1001-ORDERPRT-SYORI-SEC          SECTION.
      *
      *    対象の判定
           MOVE    ZERO                TO  FLG-SYORI
      *    入院処方箋発行
           IF      (LNK-ORDERPRT-SYOHOU    =   1   )  AND
                   (LNK-WKSRY-SRYKBN(1)    =   "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "14" )
      *        院外処方は対象外
               IF      (LNK-WKSRY-SRYSYUKBN(1) =   "212"
                                               OR  "222"
                                               OR  "232"
                                               OR  "148"
                                               OR  "149"
      *H26.4       臨時
                                               OR  "292"
                                               OR  "298"
                                               OR  "295")
                   CONTINUE
               ELSE
                   MOVE     1                  TO  FLG-SYORI
               END-IF
           END-IF
      *    注射処方箋発行
           IF      (LNK-ORDERPRT-CHUSYA    =   1   )  AND
                   (LNK-WKSRY-SRYKBN(1)    =   "31"
                                           OR  "32"
                                           OR  "33" )
               MOVE     2                  TO  FLG-SYORI
           END-IF
      *    指示箋発行
           IF      (LNK-ORDERPRT-SIJISEN   =   1   )  AND
                   (LNK-WKSRY-SRYKBN(1)    =   "40"
                                           OR  "80" )
               MOVE     3                  TO  FLG-SYORI
           END-IF
      *    処方せんコメント
           IF      (LNK-ORDERPRT-SYOHOU    =   1   )  AND
                   (LNK-WKSRY-SRYSYUKBN(1) =   "980")
               MOVE     4                  TO  FLG-SYORI
           END-IF
      *    自動発生分は対象外
           IF      LNK-WKSRY-AUTOKBN (1 1) =   "3" 
               MOVE    ZERO                TO  FLG-SYORI
           END-IF
      *
      *    薬評分は対象外
           IF      LNK-WKSRY-ZAIKBN  (1)   =   1
               MOVE    ZERO                TO  FLG-SYORI
           END-IF
      *    減点は対象外
           IF      LNK-WKSRY-ZAIKBN  (1)   =   2
               MOVE    ZERO                TO  FLG-SYORI
           END-IF
      *H28.9
      *    持参薬は対象外
           IF      LNK-WKSRY-ZAIKBN  (1)   =   3
                                           OR  4
               MOVE    ZERO                TO  FLG-SYORI
           END-IF
      *TEST
      *
           IF      FLG-SYORI           =   ZERO
               CONTINUE
           ELSE
      *        回数・日数決定
               PERFORM 1002-KAISU-NISSU-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   1                TO  LNK-ORDERPRT-RCD
           ELSE
               EVALUATE    FLG-SYORI
                   WHEN    1
      *                院内処方箋
                       PERFORM 10031-SYOHOU-SYORI-SEC
                   WHEN    2
      *                注射処方箋
                       PERFORM 10032-CHUSYA-SYORI-SEC
                   WHEN    3
      *                指示箋
                       PERFORM 10033-SIJISEN-SYORI-SEC
                   WHEN    4
      *                コメント
                       PERFORM 10034-COMMENT-SYORI-SEC
               END-EVALUATE
           END-IF
           .
       1001-ORDERPRT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    回数・日付決定処理
      *****************************************************************
       1002-KAISU-NISSU-SEC          SECTION.
      *
      *    回数・日付決定
           INITIALIZE                      TBL-KAISUU-AREA
           INITIALIZE                      TBL-ORDER-DAY-AREA
           MOVE    ZERO                TO  IDX-KAI1
           MOVE    ZERO                TO  IDX-KAI2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX 
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL     ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                   IF      LNK-WKSRY-INPUTCD(IDX IDY)(1:1)
                                               =   "*"
                       PERFORM 10021-KAISUU-SYORI-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *    ’＊’の行がなかった時、１回の診療日とする
           IF      TBL-KAISU (01)          =   ZERO
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI (LNK-WKSRYACT-MAX )
                                           TO  TBL-KAISU (01)
               IF      TBL-KAISU (01)      =   ZERO
                   MOVE    1               TO  TBL-KAISU (01)
               END-IF
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-KAI2
               MOVE    1                   TO  TBL-DAY   (01 IDX-KAI2)
           END-IF
      *    算定日が重複した時、前の回数とする
           PERFORM VARYING     IDX-KAI1    FROM    1   BY  1
                   UNTIL       IDX-KAI1    >   10
               PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                       UNTIL       IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2) >   ZERO
                       MOVE    TBL-DAY (IDX-KAI1 IDX-KAI2) TO  IDX-KAI3
                       IF      TBL-ORDER-DAY (IDX-KAI2)    =   ZERO
                           MOVE    TBL-KAISU (IDX-KAI1)
                                           TO  TBL-ORDER-DAY (IDX-KAI2)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    最初の算定日（点数マスタ検索用）
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                   UNTIL       IDX-KAI2    >   31
               IF      TBL-ORDER-DAY (IDX-KAI2)    >   ZERO
                   MOVE    IDX-KAI2        TO  WRK-SRYYMD(7:2)
                   MOVE    31              TO  IDX-KAI2
               END-IF
           END-PERFORM
      *
           .
       1002-SAIHEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *     回数決定処理
      *****************************************************************
       10021-KAISUU-SYORI-SEC              SECTION.
      *
      *    INITIALIZE                  ORCSC97AREA
      *    MOVE    SPACE               TO  SPA-SCR-REC
      *    ＰＧ名
      *    MOVE    "K02N"              TO  LNK-SC97-PG
      *    MOVE    LNK-WKSRY-INPUTCD(IDX IDY)
      *                                TO  LNK-SC97-INPUTCD
      *    CALL    "ORCSC97"           USING
      *                                SPA-AREA
      *                                ORCSC97AREA
      *                                SPA-SCR-REC
      *
           INITIALIZE                      ORCSNDAYCHGAREA
           MOVE    SPA-SRYYMD          TO  ORCSNDAYCHG-SRYYMD
           MOVE    LNK-WKSRY-INPUTCD(IDX IDY)
                                       TO  ORCSNDAYCHG-INPUTCD
           MOVE    54                  TO  ORCSNDAYCHG-KETA
           CALL    "ORCSNDAYCHG"       USING
                                       SPA-AREA
                                       ORCSNDAYCHGAREA
           MOVE    ORCSNDAYCHG-ERRCD   TO  SPA-ERRCD
      *
      *    回数
           IF      ORCSNDAYCHG-KAISU   =   ZERO
               MOVE    1               TO  ORCSNDAYCHG-KAISU
           END-IF
      *    外用薬の時、回数は１回とする
           IF      LNK-WKSRY-SRYKBN (01)   =   "23"
               MOVE    1               TO  ORCSNDAYCHG-KAISU
           END-IF
      *
           MOVE    ZERO                TO  IDX-KAI1
           MOVE    ZERO                TO  IDX-KAI2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   10   )  OR
                              (IDX-KAI1    >   ZERO )
               IF      TBL-KAISU (IDX2)    =   ZERO
                   MOVE    IDX2                TO  IDX-KAI1
                   MOVE    ORCSNDAYCHG-KAISU   TO  TBL-KAISU (IDX-KAI1)
               ELSE
                   IF       ORCSNDAYCHG-KAISU  =   TBL-KAISU (IDX2)
                       MOVE    IDX2            TO  IDX-KAI1
                   END-IF
               END-IF
           END-PERFORM
      *
      *    算定日
           MOVE    ZERO                TO  FLG-DAY
           PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                   UNTIL       IDX-KAI2    >   31
               IF      ORCSNDAYCHG-NYU-DAY (IDX-KAI2) NOT =   ZERO
                   MOVE    1               TO  TBL-DAY
                                                   (IDX-KAI1 IDX-KAI2)
                   MOVE    1               TO  FLG-DAY
               END-IF
           END-PERFORM
           IF      FLG-DAY             =   ZERO
               MOVE    SPA-SRYYMD(7:2) TO  IDX-2
               MOVE    1               TO  TBL-DAY(IDX-KAI1 IDX-2)
           END-IF
           .
       10021-KAISUU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内処方箋明細出力処理
      *****************************************************************
       10031-SYOHOU-SYORI-SEC              SECTION.
      *
      *    麻薬の判定
           MOVE    ZERO                TO  FLG-MAYAKU-ARI
           MOVE    ZERO                TO  FLG-MAYAKU-NASI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX 
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL     ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                   IF      LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                               =   "6"
                       MOVE    ZERO                TO  FLG-MAYAKU
                       PERFORM 10039-MAYAKU-CHK-SEC
                       IF      FLG-MAYAKU      =   1
                           MOVE    1               TO  FLG-MAYAKU-ARI
                           MOVE    "1"             TO  LNK-WKSRY-AUTOKBN
                                                             (IDX IDY)
                       ELSE
                           MOVE    SPACE           TO  LNK-WKSRY-AUTOKBN
                                                             (IDX IDY)
                           MOVE    1               TO  FLG-MAYAKU-NASI
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    枝番（剤毎に１からの連番）
           MOVE    ZERO                TO  WRK-RENNUM
      *    麻薬以外か、薬剤なしの時
           IF     (FLG-MAYAKU-NASI     =   1   )  OR
                 ((FLG-MAYAKU-ARI      =   ZERO) AND
                  (FLG-MAYAKU-NASI     =   ZERO))
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   LNK-WKSRYACT-MAX 
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL  ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                       MOVE    ZERO                TO  FLG-OK
                       IF     (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                       =   "6"   ) AND
                              (LNK-WKSRY-AUTOKBN(IDX IDY)  =   SPACE )
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "7"
                                                   OR  "8"
                                                   OR  "0"
                           MOVE    1               TO  FLG-OK
                       END-IF
      *                表示コメント対象外
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:4)
                                                   =   "0086"
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
      *!!
      *                出来高算定コード対象外
                       IF       LNK-WKSRY-SRYCD (IDX IDY)
                                                   =   "099999903"
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
      *!!
                       IF       FLG-OK             =   1
      *                    臨時投薬
                           IF      LNK-WKSRY-SRYSYUKBN(IDX)(1:2)
                                                   =   "29"
                               MOVE    "3"             TO  WRK-PRTKBN
                           ELSE
                               MOVE    "1"             TO  WRK-PRTKBN
                           END-IF
                           PERFORM 10030-ORDERPRT-HEN-SEC
                           MOVE    1               TO  FLG-SYOHOU-ARI
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *    麻薬
           IF      FLG-MAYAKU-ARI      =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   LNK-WKSRYACT-MAX 
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL  ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                       MOVE    ZERO                TO  FLG-OK
                       IF     (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                       =   "6"   ) AND
                              (LNK-WKSRY-AUTOKBN(IDX IDY)  =   "1")
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF      (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "7"  )  AND
                               (FLG-MAYAKU-NASI    =   ZERO )
                           MOVE    1               TO  FLG-OK
                       END-IF
      *!               器材商品名
                       IF      (LNK-WKSRY-SRYCD (IDX IDY)(1:3)
                                                   =   "058")  AND
                               (FLG-MAYAKU-NASI    =   ZERO )
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "8"
                                                   OR  "0"
                           MOVE    1               TO  FLG-OK
                       END-IF
      *                表示コメント対象外
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:4)
                                                   =   "0086"
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
      *!!
      *                出来高算定コード対象外
                       IF       LNK-WKSRY-SRYCD (IDX IDY)
                                                   =   "099999903"
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
      *!!
                       IF       FLG-OK             =   1
                           MOVE    "2"             TO  WRK-PRTKBN
                           PERFORM 10030-ORDERPRT-HEN-SEC
                           MOVE    1               TO  FLG-SYOHOU-ARI
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       10031-SYOHOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻薬判定処理
      *****************************************************************
       10039-MAYAKU-CHK-SEC              SECTION.
      *
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)  TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    麻薬判定
           IF      TNS-MADOKUKBN       =   1
               MOVE    1                   TO  FLG-MAYAKU
           END-IF
      *
           .
       10039-MAYAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎に剤点数を編集処理
      *****************************************************************
       10030-ORDERPRT-HEN-SEC              SECTION.
      *
      *    剤番、枝番（剤毎に１からの連番）
           EVALUATE    WRK-PRTKBN
               WHEN     "1"
                   IF      WRK-RENNUM      =   ZERO
                       ADD     1           TO  WRK-ORDERNUM-1
                   END-IF
                   MOVE    WRK-ORDERNUM-1  TO  WRK-ORDERNUM
               WHEN     "2"
      *            麻薬
                   IF      WRK-RENNUM      =   ZERO
                       ADD     1           TO  WRK-ORDERNUM-2
                   END-IF
                   MOVE    WRK-ORDERNUM-2  TO  WRK-ORDERNUM
               WHEN     "3"
                   IF      WRK-RENNUM      =   ZERO
                       ADD     1           TO  WRK-ORDERNUM-3
                   END-IF
                   MOVE    WRK-ORDERNUM-3  TO  WRK-ORDERNUM
               WHEN     "4"
                   IF      WRK-RENNUM      =   ZERO
                       ADD     1           TO  WRK-ORDERNUM-4
                   END-IF
                   MOVE    WRK-ORDERNUM-4  TO  WRK-ORDERNUM
               WHEN     "5"
                   IF      WRK-RENNUM      =   ZERO
                       ADD     1           TO  WRK-ORDERNUM-5
                   END-IF
                   MOVE    WRK-ORDERNUM-5  TO  WRK-ORDERNUM
               WHEN     "6"
                   IF      WRK-RENNUM      =   ZERO
                       ADD     1           TO  WRK-ORDERNUM-6
                   END-IF
                   MOVE    WRK-ORDERNUM-6  TO  WRK-ORDERNUM
           END-EVALUATE
      *
           ADD     1                   TO  WRK-RENNUM
      *
           INITIALIZE                  ORDERPRT-REC
           MOVE    SPA-HOSPNUM         TO  ORDERPRT-HOSPNUM
           MOVE    SPA-PTID            TO  ORDERPRT-PTID
           MOVE    SPA-PTNUM           TO  ORDERPRT-PTNUM
           MOVE    SPA-NYUGAIKBN       TO  ORDERPRT-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  ORDERPRT-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  ORDERPRT-HKNCOMBI
      *    システム日付
           MOVE    SPA-SYSYMD          TO  ORDERPRT-SYSYMD
      *    帳票種類
           MOVE    WRK-PRTKBN          TO  ORDERPRT-PRTKBN
      *    番号
           MOVE    WRK-ORDERNUM        TO  ORDERPRT-ORDERNUM
      *    枝番
           MOVE    WRK-RENNUM          TO  ORDERPRT-RENNUM
      *    発行日
           MOVE    SMCNDATE-YMD        TO  ORDERPRT-PRINTYMD
      *    発行時間
           MOVE    SMCNDATE-HMS        TO  ORDERPRT-PRINTHMS
      *
           MOVE    LNK-ORDERPRT-DRCD   TO  ORDERPRT-DRCD
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  ORDERPRT-SRYKBN
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                       TO  ORDERPRT-SRYSYUKBN
           MOVE    SPA-SRYYMD          TO  ORDERPRT-SRYYMD
           IF      WRK-RENNUM          =   1
      *        回数
               MOVE    TBL-ORDER-DAY-G     TO  ORDERPRT-DAY-G
           END-IF
      *
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                       TO  ORDERPRT-SRYCD
           MOVE    LNK-WKSRY-SRYSURYO (IDX IDY)
                                       TO  ORDERPRT-SRYSURYO
           MOVE    LNK-WKSRY-KANSURYO (IDX IDY)
                                       TO  ORDERPRT-KANSURYO
      *    数量（外用の個別数量）
      *    外用の回数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
      *        数量の再計算
               IF      LNK-WKSRY-SRYKAISU (IDX IDY)  >   ZERO
                   COMPUTE ORDERPRT-SRYSURYO2
                                       =   LNK-WKSRY-SRYSURYO (IDX IDY)
                                       /   LNK-WKSRY-SRYKAISU (IDX IDY)
               END-IF
           END-IF
      *    撮影回数・外用の個別回数
           MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  ORDERPRT-SRYKAISU
      *    入力コメント（フリーコメント等）
           MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDY)
                                       TO  ORDERPRT-INPUTCOMENT
      *
           MOVE    SPA-TERMID          TO  ORDERPRT-TERMID
           MOVE    SPA-OPID            TO  ORDERPRT-OPID
      *    発行日
           MOVE    SMCNDATE-YMD        TO  ORDERPRT-CREYMD
           MOVE    SMCNDATE-YMD        TO  ORDERPRT-UPYMD
      *    発行時間
           MOVE    SMCNDATE-HMS        TO  ORDERPRT-UPHMS
      *
           MOVE    ORDERPRT-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    2                   TO  LNK-ORDERPRT-RCD
               DISPLAY "ORDERPRT INS ERR:" MCP-RC 
                        ",KEY:" ORDERPRT-KEY "]"
           END-IF
           .
       10030-ORDERPRT-HEN-EXT.
           EXIT.
      *****************************************************************
      *   注射処方箋明細出力処理
      *****************************************************************
       10032-CHUSYA-SYORI-SEC              SECTION.
      *
      *    麻薬の判定
           MOVE    ZERO                TO  FLG-MAYAKU-ARI
           MOVE    ZERO                TO  FLG-MAYAKU-NASI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX 
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL     ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                   IF      LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                               =   "6"
                       MOVE    ZERO                TO  FLG-MAYAKU
                       PERFORM 10039-MAYAKU-CHK-SEC
                       IF      FLG-MAYAKU      =   1
                           MOVE    1               TO  FLG-MAYAKU-ARI
                           MOVE    "1"             TO  LNK-WKSRY-AUTOKBN
                                                             (IDX IDY)
                       ELSE
                           MOVE    SPACE           TO  LNK-WKSRY-AUTOKBN
                                                             (IDX IDY)
                           MOVE    1               TO  FLG-MAYAKU-NASI
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    枝番（剤毎に１からの連番）
           MOVE    ZERO                TO  WRK-RENNUM
      *    麻薬以外
           IF      FLG-MAYAKU-NASI     =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   LNK-WKSRYACT-MAX 
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL  ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                       MOVE    ZERO                TO  FLG-OK
                       IF     (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                       =   "6"   ) AND
                              (LNK-WKSRY-AUTOKBN(IDX IDY)  =   SPACE )
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF      LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "1"
                           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                                   TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      TNS-DATAKBN     =   "1"
                               MOVE    1               TO  FLG-OK
                           END-IF
                       END-IF
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "7"
                                                   OR  "8"
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:4)
                                                   =   "0082"
                                                   OR  "0083"
                                                   OR  "0084"
                                                   OR  "0085"
                           MOVE    1               TO  FLG-OK
                       END-IF
      *!               器材商品名
                       IF       LNK-WKSRY-SRYCD (IDX IDY)(1:3)
                                                   =   "058"
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF       FLG-OK             =   1
                           MOVE    "4"             TO  WRK-PRTKBN
                           PERFORM 10030-ORDERPRT-HEN-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *    麻薬
           IF      FLG-MAYAKU-ARI      =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   LNK-WKSRYACT-MAX 
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL  ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                       MOVE    ZERO                TO  FLG-OK
                       IF     (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "6"   ) AND
                              (LNK-WKSRY-AUTOKBN(IDX IDY)  =   "1")
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF      LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "1"
                           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                                   TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      TNS-DATAKBN     =   "1"
                               MOVE    1               TO  FLG-OK
                           END-IF
                       END-IF
                       IF      (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "7"  )  AND
                               (FLG-MAYAKU-NASI    =   ZERO )
                           MOVE    1               TO  FLG-OK
                       END-IF
      *!               器材商品名
                       IF      (LNK-WKSRY-SRYCD (IDX IDY)(1:3)
                                                   =   "058")  AND
                               (FLG-MAYAKU-NASI    =   ZERO )
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF      (LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "8"   )
                           MOVE    1               TO  FLG-OK
                       END-IF
      *                ユーザコメント
                       IF      (LNK-WKSRY-SRYCD (IDX IDY)(1:4)
                                                   =   "0082"
                                                   OR  "0083"
                                                   OR  "0084"
                                                   OR  "0085")
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF       FLG-OK             =   1
                           MOVE    "5"             TO  WRK-PRTKBN
                           PERFORM 10030-ORDERPRT-HEN-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       10032-CHUSYA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   指示箋明細出力処理
      *****************************************************************
       10033-SIJISEN-SYORI-SEC              SECTION.
      *
      *    枝番（剤毎に１からの連番）
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX 
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL     ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                   MOVE    ZERO                TO  FLG-OK
      *
                   IF       LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "1"
                       IF     LNK-WKSRY-AUTOKBN (IDX IDY)  =   "2"
                           CONTINUE
                       ELSE
                           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                                   TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      TNS-DATAKBN     =   "1"
                               MOVE    1               TO  FLG-OK
                           END-IF
                       END-IF
                   END-IF
                   IF       LNK-WKSRY-SRYCD (IDX IDY)(1:1)
                                                   =   "6"
                                                   OR  "7"
                                                   OR  "8"
                       MOVE    1               TO  FLG-OK
                   END-IF
      *            ユーザコメント
                   IF       LNK-WKSRY-SRYCD (IDX IDY)(1:4)
                                                   =   "0082"
                                                   OR  "0083"
                                                   OR  "0084"
                                                   OR  "0085"
                       MOVE    1               TO  FLG-OK
                   END-IF
      *!           器材商品名
                   IF      LNK-WKSRY-SRYCD (IDX IDY)(1:3)
                                               =   "058"
                       MOVE    1               TO  FLG-OK
                   END-IF
                   IF      FLG-OK              =   1
                       MOVE    "6"             TO  WRK-PRTKBN
                       PERFORM 10030-ORDERPRT-HEN-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       10033-SIJISEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   コメント作成処理
      *****************************************************************
       10034-COMMENT-SYORI-SEC          SECTION.
      *
      *    枝番（剤毎に１からの連番）
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX 
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL     ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                   MOVE    "1"             TO  WRK-PRTKBN
                   PERFORM 10030-ORDERPRT-HEN-SEC
                   MOVE    1               TO  FLG-COMMENT
               END-PERFORM
           END-PERFORM
      *
           .
       10034-COMMENT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタより作成処理
      *****************************************************************
       200-PARA-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *
      **   MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      **** END-IF
      *ver.4.8
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PARA-REC
               MOVE    ZERO                TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1                   TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               IF     (PARA-DATA-REC   NOT =   SPACE      )
                   MOVE    PARA-DATA-REC   TO  WKSRYACT-REC
      *            剤毎に保存する
                   IF     (LNK-WKSRYACT-MAX   >    ZERO  )  AND
                          (LNK-WKSRY-ZAINUM(LNK-WKSRYACT-MAX)
                                          NOT =   WKSRY-ZAINUM)
                       PERFORM 1001-ORDERPRT-SYORI-SEC
                       MOVE    ZERO                TO  LNK-WKSRYACT-MAX
                       MOVE    SPACE               TO  LNK-WKSRYACT-AREA
                   END-IF
                   ADD     1                   TO  LNK-WKSRYACT-MAX
                   IF      LNK-WKSRYACT-MAX    >   200
                       DISPLAY "ORCSORDER LNK-WKSRYACT-MAX 200 OVR"
                       MOVE    2               TO  LNK-ORDERPRT-RCD
                   ELSE
                       MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    剤毎に保存する
           IF      LNK-WKSRYACT-MAX   >    ZERO
               PERFORM 1001-ORDERPRT-SYORI-SEC
           END-IF
      *
           .
       200-PARA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントのみの削除処理
      *****************************************************************
       300-COMMENT-DELETE-SEC         SECTION.
      *    入院帳票オーダー検索
           INITIALIZE                  ORDERPRT-REC
           MOVE    SPA-HOSPNUM         TO  ORDERPRT-HOSPNUM
           MOVE    SPA-PTID            TO  ORDERPRT-PTID
           MOVE    SPA-PTNUM           TO  ORDERPRT-PTNUM
           MOVE    SPA-NYUGAIKBN       TO  ORDERPRT-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  ORDERPRT-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  ORDERPRT-HKNCOMBI
      *    システム日付
           MOVE    SPA-SYSYMD          TO  ORDERPRT-SYSYMD
      *    発行日
           MOVE    SMCNDATE-YMD        TO  ORDERPRT-PRINTYMD
      *    発行時間
           MOVE    SMCNDATE-HMS        TO  ORDERPRT-PRINTHMS
      *    帳票区分
           MOVE    "1"                 TO  ORDERPRT-PRTKBN
      *
           MOVE    ORDERPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_orderprt"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSORCERPRT DEL ERR:"  MCP-RC
                       ",KEY:" ORDERPRT-KEY
           END-IF
           .
       300-COMMENT-DELETEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *v.4.8
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO            TO  MCP-RC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
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
       990-DBCLOSE-SEC           SECTION.
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
