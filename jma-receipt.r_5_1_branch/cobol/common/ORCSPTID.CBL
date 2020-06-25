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
       PROGRAM-ID.             ORCSPTID.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 患者ＩＤ取得処理
      *  返却エラーコード  : 00:正常　10:パラメータ不正・対象データなし
      *                    : 02:システム管理（患者番号採番）読込エラー
      *  管理者            : 
      *  作成日付    作業者        記述
      *  XX.XX.XX    ＮＮＮ        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  01.00.01    MCC-太田      01/05/22  患者番号ゼロ詰め処理
      *  01.00.02    MCC-多々納    01/11/27  自由構成区分の追加修正
      *  02.07.00    NACL-太田     05/10/24  MONFUNC 対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.04.00    NACL-多々納  09/01/29  本院分院対応
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
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *----(01.00.01) LINE ADD START --------------------------------
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-END             PIC 9(02).
           03  WRK-RENKETA         PIC 9(02).
           03  WRK-PTNUM           PIC X(20).
           03  WRK-ST              PIC 9(02).
      *----(01.00.01) LINE ADD END   --------------------------------
           03  WRK-SADDSIGN-STR    PIC X(1000).
      *
      *    保存スパ領域
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCSADDSIGN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSPTID.INC".
      *
          COPY     "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSPTIDAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SPTID-RC
      *    スパ領域
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           IF      SPTID-HOSPNUM       =   ZERO
               MOVE    1                   TO  SPTID-RC
           END-IF
      *
           IF     (SPTID-PTNUM      NOT =   SPACE )  AND
                  (SPTID-PTID       NOT =   ZERO  )
               MOVE    1                   TO  SPTID-RC
           ELSE
               IF      SPTID-PTNUM      NOT =   SPACE
                   PERFORM 100-PTID-SET-SEC
               ELSE
                   PERFORM 110-PTNUM-SET-SEC
               END-IF
           END-IF
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    患者ＩＤ検索処理
      *****************************************************************
       100-PTID-SET-SEC                SECTION.
      *
      *    患者番号構成編集
           PERFORM 120-SYS1009HEN-SEC
      *
      *----(01.00.01) LINE ADD START --------------------------------
           PERFORM 120-KETA-CNT-SEC
           IF  SPTID-RC            NOT =   ZERO
               GO  TO                  100-PTID-SET-EXT
           END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPTID-HOSPNUM       TO  PTNUM-HOSPNUM
      *
           MOVE    SPTID-PTNUM         TO  WRK-SADDSIGN-STR
           PERFORM 800-ORCSADDSIGN-SEC
           MOVE    WRK-SADDSIGN-STR    TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
           ELSE
               MOVE    2                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
           PERFORM
               UNTIL      (FLG-PTNUM   NOT =   ZERO )  OR
                          (FLG-OK          =   1    )
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
      *
                   IF      PTNUM-PTNUM         =   SPTID-PTNUM
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   1
               MOVE    PTNUM-PTID      TO  SPTID-PTID
           ELSE
               MOVE    FLG-PTNUM       TO  SPTID-RC
           END-IF
      *
           .
       100-PTID-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号構成管理情報編集処理
      *****************************************************************
       120-SYS1009HEN-SEC                SECTION.
      *
      *    医療機関番号がＳＰＡと違う
           IF     (SPTID-HOSPNUM    NOT =   ZERO  )
           AND    (SPTID-SYS1009-KBN    =   1     )
      *        患者番号構成管理情報編集
               MOVE    SPTID-HOSPNUM       TO  WRK-SPA-HOSPNUM
               PERFORM 1201-SYS1009-SPA-SEC
           END-IF
           .
       120-SYS1009HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号構成管理情報編集処理
      *****************************************************************
       1201-SYS1009-SPA-SEC                SECTION.
      *
           INITIALIZE                       WRK-SPA-1009-TBL
      *
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPTID-HOSPNUM       TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  WRK-SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  WRK-SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  WRK-SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  WRK-SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  WRK-SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  WRK-SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  WRK-SPA-1009-HJNKSIRENNUMKETA
      *拡張構成区分
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  WRK-SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  WRK-SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  WRK-SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  WRK-SPA-1009-KKCKSIATOKETA
           ELSE
               MOVE    1               TO  SPTID-RC
           END-IF
           .
       1201-SYS1009-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       110-PTNUM-SET-SEC                SECTION.
      *
           MOVE    SPTID-HOSPNUM       TO  PTNUM-HOSPNUM
           MOVE    SPTID-PTID          TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   ZERO
               MOVE    PTNUM-PTNUM     TO  SPTID-PTNUM
           ELSE
               MOVE    2               TO  SPTID-RC
           END-IF
      *
           .
       110-PTNUM-SET-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD START --------------------------------
      *****************************************************************
      *    患者ＩＤ検索処理
      *****************************************************************
       120-KETA-CNT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
      *
      *    標準構成時のみ変換
      *****IF  WRK-SPA-1009-PTNUMKSIKBN    =   "1"  OR  "3"
           IF  WRK-SPA-1009-PTNUMKSIKBN    =   "3"
               GO  TO                  120-KETA-CNT-EXT
           END-IF
      *    自由構成で数字型の時変換
           IF  (WRK-SPA-1009-PTNUMKSIKBN   =   "1"  )  AND
               (WRK-SPA-1009-JIYKSIKBN NOT =   "2"  )
               GO  TO                  120-KETA-CNT-EXT
           END-IF
      *
      *    パラメータ（IN）の患者番号桁数をカウント
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX                    >   20   )
                       OR       (SPTID-PTNUM(IDX:1)     =   SPACE)
               MOVE    IDX             TO  WRK-END
           END-PERFORM
      *
      *    自由構成の桁数編集
           IF  WRK-SPA-1009-PTNUMKSIKBN    =   "1"
               MOVE    WRK-SPA-1009-JIYKSIKETA     TO  WRK-RENKETA
           ELSE
      *    標準構成の桁数
      *    システム管理マスタで設定された桁数をカウント
               MOVE    WRK-SPA-1009-HJNKSIRENNUMKETA   TO  WRK-RENKETA
               IF  WRK-SPA-1009-HJNKSIKBN      =   "1"  OR  "2"
                   EVALUATE    WRK-SPA-1009-HJNKSINENKBN
                   WHEN    "1"
                       ADD     3           TO  WRK-RENKETA
                   WHEN    "2"
                       ADD     4           TO  WRK-RENKETA
                   END-EVALUATE
               END-IF
               IF  WRK-SPA-1009-HJNKSIKBN      =   "1"  OR  "3"
                   ADD     1               TO  WRK-RENKETA
               END-IF
           END-IF
      *
      *    パラメータ（IN）の患者番号桁数が設定された桁数を超えた場合
           IF  WRK-END                 >   WRK-RENKETA
               MOVE    1               TO  SPTID-RC
               GO      TO              120-KETA-CNT-EXT
           END-IF
      *    開始位置算出
           COMPUTE     WRK-ST          =   WRK-RENKETA
                                       -   WRK-END   +   1
           IF  ( WRK-ST                =   ZERO )  OR
               ( WRK-ST                >   20   )
               MOVE    1               TO  SPTID-RC
               GO      TO              120-KETA-CNT-EXT
           END-IF
      *    検索するため、患者番号をゼロ詰編集
           MOVE    SPACE               TO  SPTID-PTNUM
           MOVE    ZERO                TO  SPTID-PTNUM (1:WRK-RENKETA)
           MOVE    WRK-PTNUM (1:WRK-END)
                                       TO  SPTID-PTNUM (WRK-ST:)
      *
           .
       120-KETA-CNT-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD END   --------------------------------
      *
      *****************************************************************
      *    検索文字列編集処理
      *****************************************************************
       800-ORCSADDSIGN-SEC             SECTION.
      *
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    WRK-SADDSIGN-STR    TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR        TO  WRK-SADDSIGN-STR
      *
           .
       800-ORCSADDSIGN-EXT.
           EXIT.
      *****************************************************************
      *    システム基本チェック処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC     SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               WRK-SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
