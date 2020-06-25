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
       PROGRAM-ID.             ORCSPOPUPSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 患者情報通知編集処理
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/12/14    NACL- 多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMMDD.
                   07  WRK-SMM             PIC 9(02).
                   07  WRK-SDD             PIC 9(02).
           03  WRK-KAKUNINYMD          PIC X(08).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKN-MSG             PIC X(30).
           03  WRK-HEDMESG             PIC X(08).
      *
           03  WRK-MEMO-HEN            PIC X(1060).
      *
           03  WRK-MEMOKBN             PIC 9(01).
           03  WRK-MEMO-G.
               05  WRK-MEMO-YMD        PIC X(08).
               05  WRK-MEMO            PIC X(1000).
               05  WRK-MEMO-1          PIC X(1000).
               05  WRK-MEMO-2          PIC X(1000).
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    患者情報通知パラメタ
           COPY    "CPORCSPOPUPSET.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA
           ORCSPOPUPSETAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSPOPUPSET-BODY
      *
      *    保険確認日
           IF     ORCSPOPUPSET-HKNCHK  =   "1"
               PERFORM 100-PTHKNINF-HEN-SEC
           END-IF
      *    メモ１
           IF     (ORCSPOPUPSET-MEMO1  =   "1"  )  OR
                  (ORCSPOPUPSET-MEMO2  =   "1"  )
               PERFORM 200-PTMEMOINF-SYORI-SEC
           END-IF
      *
      *    出力内容編集
           IF      WRK-MEMO-HEN    NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    1000                TO  KANACHK-MAX-CNT
               MOVE    WRK-MEMO-HEN        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  ORCSPOPUPSET-BODY
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    保険確認日処理
      *****************************************************************
       100-PTHKNINF-HEN-SEC                SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    SPA-SYSYMD          TO  PTHKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  PTHKN-TEKEDYMD
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           PERFORM UNTIL      (FLG-PTHKNINF    =   1   )
               IF     (PTHKN-SAKJOKBN      =   "1" )
                  OR  (PTHKN-HKNNUM (1:1)  =   "9" )
                   CONTINUE
               ELSE
                   IF     (WRK-KAKUNINYMD      =   SPACE         )
                      OR  (PTHKN-KAKUNINYMD    >=  WRK-KAKUNINYMD)
                       MOVE    PTHKN-KAKUNINYMD    TO  WRK-KAKUNINYMD
                       MOVE    PTHKN-HKNNUM        TO  WRK-HKNNUM
                   END-IF
               END-IF
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-KAKUNINYMD  NOT =   SPACE
               PERFORM 1001-HKNNUM-HEN-SEC
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-KAKUNINYMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "保険最終確認日："
                       HKN-TANSEIDONAME
                                           DELIMITED  BY  SPACE
                       "　"
                       LNK-DAY2-EDTYMD1
                       X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-MEMO-HEN
               END-STRING
           END-IF
           .
       100-PTHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報処理
      *****************************************************************
       1001-HKNNUM-HEN-SEC                SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-HKNNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ表示処理
      *****************************************************************
       200-PTMEMOINF-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO-G
           IF     (ORCSPOPUPSET-MEMO1  =   "1"  )
               MOVE    1                   TO  WRK-MEMOKBN
               MOVE    "メモ１："          TO  WRK-HEDMESG
               PERFORM 2001-PTMEMOINF-HEN-SEC
           END-IF
      *
           IF     (ORCSPOPUPSET-MEMO2  =   "1"  )
               MOVE    2                   TO  WRK-MEMOKBN
               MOVE    "メモ２："          TO  WRK-HEDMESG
               PERFORM 2001-PTMEMOINF-HEN-SEC
           END-IF
      *
           .
       200-PTMEMOINF-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ表示処理
      *****************************************************************
       2001-PTMEMOINF-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO
           MOVE    SPACE               TO  WRK-MEMO-YMD
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-SYSYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           PERFORM UNTIL    FLG-PTMEMOINF  =   1
      *
               IF      PTMEMOINF-SYSYMD    =   SPA-SYSYMD
                   STRING  WRK-HEDMESG
                           PTMEMOINF-MEMO
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-MEMO
                   END-STRING
                   MOVE    1                   TO  FLG-PTMEMOINF
               ELSE
                   MOVE    PTMEMOINF-SYSYMD    TO  WRK-MEMO-YMD
                   IF      PTMEMOINF-SYSYMD    <   SPA-SYSYMD
      *                システム日付より前で終了
                       MOVE    1               TO  FLG-PTMEMOINF
                   END-IF
               END-IF
               IF      FLG-PTMEMOINF       =   ZERO
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-PTMEMOINF-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (WRK-MEMO-YMD    NOT =   SPACE )  AND
                  (WRK-MEMO            =   SPACE )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-MEMO-YMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "【"
                       WRK-HEDMESG
                       LNK-DAY2-EDTYMD1
                       "に登録があります】"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-MEMO
               END-STRING
           END-IF
      *
           IF      WRK-MEMO        NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    400                 TO  KANACHK-MAX-CNT
               MOVE    WRK-MEMO            TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MEMO
               IF      KANACHK-MAX         >   ZERO
      *            最終文字が改行以外は、改行を設定する
                   IF      WRK-MEMO(KANACHK-MAX:1)
                                           NOT =   X"0A"
                       COMPUTE IDX         =   KANACHK-MAX  +  1
                       MOVE    X"0A"       TO  WRK-MEMO(IDX:1)
                   END-IF
               END-IF
      *
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    1000                TO  KANACHK-MAX-CNT
               MOVE    WRK-MEMO-HEN        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-MAX         <   1000
                   COMPUTE IDX                 =   KANACHK-MAX   +  1
                   MOVE    WRK-MEMO            TO  WRK-MEMO-HEN
                                                    (IDX:)
               END-IF
           END-IF
      *
           .
       2001-PTMEMOINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
           EXIT.
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       920-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       920-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込み処理
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
