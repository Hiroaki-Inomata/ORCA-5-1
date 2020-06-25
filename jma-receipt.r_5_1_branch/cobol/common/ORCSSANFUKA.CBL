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
       PROGRAM-ID.             ORCSSANFUKA.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 内分泌負荷試験検査点数サブ
      *  返却エラーコード  : 00:正常　10:パラメータ不正・対象データなし
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/01    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/29  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *  04.05.00    NACL-多々納  10/03/15  平成２２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-T2              PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDD                 PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09).
           03  WRK-DAY-X.
               05  WRK-DAY-9           PIC 9(02).
           03  WRK-SRYYMD              PIC X(08).
      *
           03  WRK-KEI-TENSU           PIC 9(08).
           03  WRK-KEI-TENSU-DEL       PIC 9(08).
           03  WRK-SUM-TENSU           PIC 9(08).
           03  WRK-SUM-TENSU-ALL       PIC 9(08).
           03  WRK-MSANTEITEN          PIC 9(08).
      *
           03  WRK-DAY                 PIC 9(02).
           03  WRK-IDX                 PIC 9(04).
           03  WRK-TENSU-KEI           PIC 9(08).
           03  WRK-TENSU-SA            PIC 9(08).
      *
           03  WRK-TENSU1              PIC 9(08).
           03  WRK-TENSU2              PIC 9(08).
           03  WRK-CNT                 PIC 9(08).
      *
       01  TBL-HENSYU-AREA.
           03  TBL-HENSYU-OCC          OCCURS  20.
               05  TBL-SRYCD           PIC X(09).
               05  TBL-IDX             PIC 9(04).
               05  TBL-DAY             PIC 9(02).
      *
       01  TBL2-HENSYU-AREA.
           03  TBL2-HENSYU-OCC          OCCURS  20.
               05  TBL2-SRYCD           PIC X(09).
               05  TBL2-IDX             PIC 9(04).
               05  TBL2-DAY             PIC 9(02).
      *
      *    内分泌負荷試験包括コード
       01  WRK-HOU-SRYCD               PIC X(09)   VALUE    "099600801".
      *
      *    エラーメッセージ
      *
      *    算定履歴
       01  TBL-SANTEI-AREA.
           02  TBL-SANTEI-REC          OCCURS   20.
           COPY    "CPSANTEI.INC"   REPLACING
                                    //SANTEI-// BY //TBL-SANTEI-//.
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"   REPLACING
                                    //SANTEI-// BY //HZN-SANTEI-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *****COPY    "ORCA-DBPATH".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
     *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSANFUKA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSSANFUKAAREA
           SPA-AREA
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
           EVALUATE    LNK-SANFUKA-SYORI
      *        点数集計
               WHEN    1
                   PERFORM 100-SYUKEI-SYORI-SEC
      *        点数計算
               WHEN    2
                   PERFORM 200-KEISAN-SYORI-SEC
      *        更新後チェック
               WHEN    3
                   PERFORM 300-KOUSIN-SYORI-SEC
           END-EVALUATE
      *
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    点数集計処理
      *****************************************************************
       100-SYUKEI-SYORI-SEC            SECTION.
      *
      *    内分泌負荷試験上限点数編集
           MOVE    WRK-HOU-SRYCD       TO  TNS-SRYCD
           MOVE    LNK-SANFUKA-SRYYMD  TO  WRK-SRYYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-TEN             TO  LNK-SANFUKA-TEN-MAX
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-HOU-SRYCD       TO  SANTEI-SRYCD
           MOVE    LNK-SANFUKA-SRYYMD(1:6)
                                       TO  SANTEI-SRYYM
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-SEC
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SANTEI          =   ZERO
               MOVE    SANTEI-MSANTEITEN   TO  WRK-MSANTEITEN
      *        訂正分の点数編集と点数チェック
               PERFORM 1001-TEISEI-SUM-SEC
           ELSE
               MOVE    ZERO                TO  LNK-SANFUKA-MSANTESU
           END-IF
           .
      *
       100-SYUKEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正分の点数集計点数集計処理
      *****************************************************************
       1001-TEISEI-SUM-SEC            SECTION.
      *
           INITIALIZE                  TBL-SANTEI-AREA
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  WRK-SUM-TENSU
           MOVE    ZERO                TO  WRK-SUM-TENSU-ALL
           MOVE    ZERO                TO  FLG-CHK
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    LNK-SANFUKA-SRYYMD(1:6)
                                       TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-SEC
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL   FLG-SANTEI      =   1
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL      (IDX-T   >   SPA-MAX-LINE )  OR
                                  (FLG-OK  =   1    )  OR
                                  (LNK-SANFUKA-TEI-SRYCD(IDX-T)
                                           =   SPACE )
                   IF      SANTEI-SRYCD    =   LNK-SANFUKA-TEI-SRYCD
                                                             (IDX-T)
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      SANTEI-SRYCD    NOT =   WRK-HOU-SRYCD
                   IF      SANTEI-MSANTEITENKBN    =   2
                        MOVE    SANTEI-REC         TO  HZN-SANTEI-REC
                   END-IF
                   IF      FLG-OK              =   1
      *                訂正時
                       PERFORM 1001-TEI-TENSU-SEC
                   ELSE
      *                月２回は1200点の検査とする
                       IF     (SANTEI-MSANTEICNT   >   1    )
                       AND    (SANTEI-MSANTEITEN   <   1200 )
                           COMPUTE WRK-KEI-TENSU   =   SANTEI-MSANTEITEN
                                                   +   1200
                       ELSE
                           COMPUTE WRK-KEI-TENSU   =   SANTEI-MSANTEITEN
                                                   *   SANTEI-MSANTEICNT
                       END-IF
                       MOVE    ZERO                TO  WRK-KEI-TENSU-DEL
                   END-IF
                   ADD     WRK-KEI-TENSU       TO  WRK-SUM-TENSU-ALL
                   ADD     WRK-KEI-TENSU-DEL   TO  WRK-SUM-TENSU
                   IF      FLG-OK              =   1
                       CONTINUE
                   ELSE
                       MOVE    SANTEI-MSANTEID     TO
                                                   LNK-SANFUKA-MSANTEID
                       IF      SANTEI-MSANTEITENKBN    =   2
                           MOVE    1                   TO  FLG-CHK
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    今月算定済み点数
           MOVE    WRK-MSANTEITEN      TO  LNK-SANFUKA-MSANTESU
           MOVE    WRK-SUM-TENSU       TO  LNK-SANFUKA-MSANTESU-DEL
      *
           IF     (FLG-CHK             =   1  )  AND
                  (WRK-MSANTEITEN  NOT =   LNK-SANFUKA-TEN-MAX)
               MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
               PERFORM 10011-ERRHENSYU-SEC
           END-IF
      *
           .
       1001-TEISEI-SUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正点数処理
      *****************************************************************
       1001-TEI-TENSU-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-KEI-TENSU-DEL
           MOVE    ZERO                TO  WRK-KEI-TENSU
      *     月２回は1200点の検査とする
           IF     (SANTEI-MSANTEICNT   >   1    )
           AND    (SANTEI-MSANTEITEN   <   1200 )
               MOVE    1200                TO  WRK-TENSU1
               MOVE    SANTEI-MSANTEITEN   TO  WRK-TENSU2
           ELSE
               MOVE    SANTEI-MSANTEITEN   TO  WRK-TENSU1
               MOVE    SANTEI-MSANTEITEN   TO  WRK-TENSU2
           END-IF
      *
           MOVE    ZERO                TO  WRK-CNT
           MOVE    LNK-SANFUKA-SRYYMD(7:2)     TO  IDD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      SANTEI-DAY (IDX2)   >   ZERO
                   IF      IDD             =   IDX2
                       IF      WRK-CNT         =   ZERO
                           ADD     WRK-TENSU1      TO  WRK-KEI-TENSU-DEL
                       ELSE
                           ADD     WRK-TENSU2      TO  WRK-KEI-TENSU-DEL
                       END-IF
                   ELSE
                       IF      WRK-CNT         =   ZERO
                           ADD     WRK-TENSU1      TO  WRK-KEI-TENSU
                       ELSE
                           ADD     WRK-TENSU2      TO  WRK-KEI-TENSU
                       END-IF
                   END-IF
                   ADD     1               TO  WRK-CNT
               END-IF
           END-PERFORM
      *
           .
       1001-TEI-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー編集処理
      *****************************************************************
       10011-ERRHENSYU-SEC            SECTION.
      *
           MOVE    SANTEI-MSANTEID     TO  LNK-SANFUKA-ERRMSG(1:2)
           MOVE    "日算定の内分泌負荷試験は算定点数を見直して下さい"
                                       TO  LNK-SANFUKA-ERRMSG(3:)
           .
       10011-ERRHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の点数計算処理
      *****************************************************************
       200-KEISAN-SYORI-SEC                SECTION.
      *
           IF      LNK-SANFUKA-TEN-MAX     =   ZERO
      *        点数集計
               PERFORM 100-SYUKEI-SYORI-SEC
           END-IF
      *
      *    今回合計点数
           INITIALIZE                  TBL-HENSYU-AREA
           MOVE    ZERO                TO  IDX-T
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )  AND
                      (SPA-COM-SRYKBN  (IDX)       =   "60")  AND
                      (SPA-COM-DATAKBN (IDX)       =   "1" )  AND
                      (SPA-COM-HOUKNSKBN (IDX)     =   08  )
                   PERFORM 2001-TBLHENSYU-SEC
               END-IF
           END-PERFORM
      *    日順にＳＯＲＴする
           IF      IDX-T               >   1
               PERFORM 2002-TBL-SORT-SEC
           END-IF
      *    点数計算
           COMPUTE WRK-TENSU-KEI       =   LNK-SANFUKA-MSANTESU
                                       -   LNK-SANFUKA-MSANTESU-DEL
      *****MOVE    LNK-SANFUKA-MSANTESU    TO  WRK-TENSU-KEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-T   )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               MOVE    TBL-IDX (IDX)       TO  IDY
               IF      WRK-TENSU-KEI       >=  LNK-SANFUKA-TEN-MAX
      *            上限オーバー
                   MOVE    "1060"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (IDY)
               ELSE
                   COMPUTE WRK-TENSU-SA    =   LNK-SANFUKA-TEN-MAX
                                           -   WRK-TENSU-KEI
                   IF      TBL-DAY (IDX)   <   LNK-SANFUKA-MSANTEID
      *                最終算定日より前
                       IF     (WRK-TENSU-KEI   +   SPA-COM-TEN (IDY))
                                           >   LNK-SANFUKA-TEN-MAX
      *                    最終算定日の上限オーバー
                           PERFORM 2001-ERRHENSYU-SEC
                           MOVE    SPA-COM-TEN (IDY)
                                                   TO  WRK-TENSU-SA
                       END-IF
                   END-IF
      *
                   IF      WRK-TENSU-SA    >=  SPA-COM-TEN (IDY)
                       MOVE    SPA-COM-TEN (IDY)   TO  SPA-COM-KISOTEN
                                                              (IDY)
                   ELSE
                       MOVE    WRK-TENSU-SA        TO  SPA-COM-KISOTEN
                                                              (IDY)
                   END-IF
                   ADD     SPA-COM-KISOTEN (IDY)   TO  WRK-TENSU-KEI
               END-IF
           END-PERFORM
      *
           .
      *
       200-KEISAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終算定日の上限オーバーエラー処理
      *****************************************************************
       2001-ERRHENSYU-SEC                SECTION.
      *
           IF      SPA-COM-KZMFLG(IDY) =   SPACE
               MOVE    "K060"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDY)
               MOVE    "1"             TO  SPA-COM-KZMFLG (IDY)
      *
               MOVE    LNK-SANFUKA-MSANTEID
                                       TO  LNK-SANFUKA-ERRMSG(1:2)
               MOVE    "日算定の内分泌負荷試験の点数が逓減になります。"
                                       TO  LNK-SANFUKA-ERRMSG(3:)
               MOVE    "登録後、訂正をして下さい。"
                                       TO  LNK-SANFUKA-ERRMSG(49:)
           END-IF
           .
       2001-ERRHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルＳＯＲＴ処理
      *****************************************************************
       2002-TBL-SORT-SEC                SECTION.
      *
           INITIALIZE                  TBL2-HENSYU-AREA
           MOVE    ZERO                TO  IDX-T2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-T
               MOVE    99                  TO  WRK-DAY
               MOVE    ZERO                TO  WRK-IDX
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   IDX-T
                   IF     (TBL-DAY (IDY)   <   WRK-DAY) AND
                          (TBL-DAY (IDY)   >   ZERO   )
                       MOVE    IDY             TO  WRK-IDX
                       MOVE    TBL-DAY (IDY)   TO  WRK-DAY
                   END-IF
               END-PERFORM
               IF      WRK-IDX             >   ZERO
                   ADD     1               TO  IDX-T2
                   MOVE    TBL-HENSYU-OCC(WRK-IDX)
                                           TO  TBL2-HENSYU-OCC(IDX-T2)
                   MOVE    ZERO            TO  TBL-DAY (WRK-IDX)
               END-IF
           END-PERFORM
           MOVE    TBL2-HENSYU-AREA    TO  TBL-HENSYU-AREA
           .
      *
       2002-TBL-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算テーブル編集処理
      *****************************************************************
       2001-TBLHENSYU-SEC                SECTION.
      *
           ADD     1                   TO  IDX-T
           MOVE    SPA-COM-INPUTCD(IDX)
                                       TO  TBL-SRYCD (IDX-T)
           MOVE    IDX                 TO  TBL-IDX   (IDX-T)
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               IF      SPA-COM-NYUINDAY (IDX IDX2) >   ZERO
                   MOVE    IDX2            TO  TBL-DAY (IDX-T)
                   MOVE    31              TO  IDX2
               END-IF
           END-PERFORM
           IF      TBL-DAY (IDX-T)     =   ZERO
               MOVE    LNK-SANFUKA-SRYYMD(7:2)  TO  TBL-DAY (IDX-T)
           END-IF
           .
      *
       2001-TBLHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新チェック処理
      *****************************************************************
       300-KOUSIN-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  LNK-SANFUKA-ERRMSG
      *    内分泌負荷試験上限点数編集
           MOVE    WRK-HOU-SRYCD       TO  TNS-SRYCD
           MOVE    LNK-SANFUKA-SRYYMD  TO  WRK-SRYYMD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-TEN             TO  LNK-SANFUKA-TEN-MAX
      *
           INITIALIZE                  TBL-SANTEI-AREA
           INITIALIZE                  HZN-SANTEI-REC
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  WRK-SUM-TENSU
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    LNK-SANFUKA-SRYYMD(1:6)
                                       TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-SEC
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    ZERO                TO  WRK-TENSU-KEI
           PERFORM UNTIL   FLG-SANTEI      =   1
               IF      SANTEI-MSANTEICNT   >   ZERO
                   IF      SANTEI-SRYCD        =   WRK-HOU-SRYCD
                       MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                   ELSE
      ***              ADD     SANTEI-MSANTEITEN   TO  WRK-TENSU-KEI
      *            月２回は1200点の検査とする
                   IF      SANTEI-MSANTEICNT   >   1
                       IF      SANTEI-MSANTEITEN   <   1200
                           COMPUTE WRK-KEI-TENSU   =   SANTEI-MSANTEITEN
                                                   +   1200
                       ELSE
                           COMPUTE WRK-KEI-TENSU   =   SANTEI-MSANTEITEN
                                                   *   SANTEI-MSANTEICNT
                       END-IF
                   ELSE
                       MOVE    SANTEI-MSANTEITEN   TO  WRK-KEI-TENSU
                   END-IF
                       ADD     WRK-KEI-TENSU       TO  WRK-TENSU-KEI
      *
                       IF     (SANTEI-MSANTEITENKBN    =   2  )
                          OR  (WRK-TENSU-KEI   >   LNK-SANFUKA-TEN-MAX)
                           PERFORM 10011-ERRHENSYU-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      LNK-SANFUKA-ERRMSG  NOT =   SPACE
               IF      LNK-SANFUKA-TEN-MAX     =   HZN-SANTEI-MSANTEITEN
                   MOVE    SPACE           TO  LNK-SANFUKA-ERRMSG
               END-IF
           END-IF
      *
           .
       300-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
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
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
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
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
