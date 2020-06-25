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
       PROGRAM-ID.                 ORCSDRSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : ドクター編集・チェック処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/02/24    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−多々納 04/06/25  診療所で２０名以上を科別へ
      *  01.00.02    NACL−多々納 06/08/16  当日担当医設定追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.03.00    NACL-多々納  08/06/XX  最大９９件対応
      *  04.04.00    NACL-多々納  09/03/05  当日担当医対応なし追加
      *  04.08.00    NACL-多々納  16/01/XX  診療科担当医・非表示対応追加
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
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
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
           03  FLG-CHK3            PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OVER            PIC 9(01).
           03  FLG-TANTOU          PIC 9(01).
      *
           03  FLG-LSDOCTOR            PIC 9(01).
           03  FLG-KADR-ARI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DRCD-TBL-G.
               05  WRK-DRCD-TBL        OCCURS   99.
      *            ドクターコード
                   07  WRK-DRCD             PIC  X(04).
                   07  WRK-DRCD-H           PIC  X(01).
                   07  WRK-DRCDMEI          PIC  X(24).
               05  WRK-DRCD-IDX             PIC  9(04).
      *
           03  WRK-KEY-DRCD            PIC X(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ドクター
           COPY    "CPSK1010.INC".
      *    担当医設定 
           COPY  "CPSK1011.INC".
      *
      *    診療科別ドクター
       01  LIST-DOCTOR-REC.
           COPY    "CPLIST_DOCTOR.INC".
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ドクターセット領域
           COPY    "CPORCSDRSET.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSDRSETAREA
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
           INITIALIZE                  ORCSDR-ERRCD
      *
      *
           EVALUATE    ORCSDR-KBN
      *        ドクターテーブル編集
               WHEN    "1"
                   PERFORM 100-DR-HENSYU-SEC
      *            ドクターコードがある時、ドクター編集
                   IF      ORCSDR-IN-DRCD  NOT =   SPACE
                       PERFORM 200-DR-CHECK-SEC
                   END-IF
      *        ドクターチェック
               WHEN    "2"
                   PERFORM 200-DR-CHECK-SEC
      *        ドクターチェック（初期表示用）
               WHEN    "3"
                   PERFORM 300-DR-CHECK03-SEC
      *        ドクターテーブル編集
               WHEN    "4"
                   PERFORM 100-DR-HENSYU-SEC
                   IF      FLG-TANTOU          =   1
      *                ドクター追加編集
                       PERFORM 1003-SYS1010-ADDHEN-SEC
                   END-IF
      *            ドクターコードがある時、ドクター編集
                   IF      ORCSDR-IN-DRCD  NOT =   SPACE
                       PERFORM 200-DR-CHECK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ドクターテーブル編集処理
      *****************************************************************
       100-DR-HENSYU-SEC            SECTION.
      *
           INITIALIZE                  ORCSDR-OUT-AREA
           INITIALIZE                  WRK-DRCD-TBL-G
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  FLG-TANTOU
      *    当日担当医
           IF      ORCSDR-KBN2         =   "1"
               CONTINUE
           ELSE
               IF      ORCSDR-SRYKA    NOT =   SPACE
      *H27.12
      *            診療科別の判定
                   PERFORM 1003-LSDOCTOR-HEN-SEC
                   IF      FLG-KADR-ARI        =   ZERO
      *                当日担当医判定
                       PERFORM 1000-SYS1011-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
      *H27.12
           IF      FLG-KADR-ARI        =   1
               GO  TO      100-HENSYU-SYORI-EXT
           END-IF
      *
           IF      FLG-TANTOU          =   ZERO
               PERFORM 1001-SYS1010-HEN-SEC
           ELSE
               PERFORM 1002-SYS1011-HEN-SEC
           END-IF
      *
           .
       100-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターテーブル編集処理
      *****************************************************************
       1001-SYS1010-HEN-SEC            SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-OVER
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               IF     (SYS-1010-KBNCD(1:1)     =   "1"  )
      *H27.12
      *            表示分のみ
                 AND  (SYS-1010-HYOJIFLG   NOT =   "1"  )
      *
                   MOVE    ZERO                TO  FLG-CHK
      *            科別の時（病院の）
                   IF     (ORCSDR-SRYKA    NOT =   SPACE )  AND
                          (SPA-HOSPSBT         =   1     )
                       PERFORM 1001-SEMONKACD-CHK-SEC
                   ELSE
                       IF      ORCSDR-SRYKA    NOT =   SPACE
                           PERFORM 1001-SEMONKACD-CHK-SEC
                           IF      FLG-CHK         =   1
                               PERFORM 1002-DRCD-TBL-SEC
                           END-IF
                       END-IF
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   IF      FLG-CHK             =   1
                       IF      IDX                 >=  99
                           MOVE    1               TO  FLG-OVER
                       ELSE
                           ADD     1                   TO  IDX
                           MOVE    SYS-1010-KBNCD(2:4)
                                                   TO  ORCSDR-DRCD
                                                                (IDX)
                           MOVE    SYS-1010-NAME   TO  ORCSDR-DRCDMEI
                                                                (IDX)
                           IF     (FLG-CHK2        =   1  )  AND
                                  (ORCSDR-DRCD-IDX =   ZERO )
                               MOVE    IDX         TO  ORCSDR-DRCD-IDX
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
      *
           IF     (FLG-OVER            =   1  )  AND
                  (IDX3                >   ZERO)
               MOVE    WRK-DRCD-TBL-G      TO  ORCSDR-DRCD-TBL-G
               MOVE    IDX3                TO  ORCSDR-DRCD-MAX
               MOVE    WRK-DRCD-IDX        TO  ORCSDR-DRCD-IDX
           END-IF
      *
           .
       1001-SYS1010-HEN-EXT.
           EXIT.
      *****************************************************************
      *    当日担当医処理
      *****************************************************************
       1000-SYS1011-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1011-REC
           INITIALIZE                  SYS-1011-REC
           MOVE    "1011"              TO  SYS-1011-KANRICD
           MOVE    ORCSDR-SRYKA        TO  SYS-1011-KBNCD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1011-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1011-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC           TO  SYS-1011-REC
                   IF      SYS-1011-DRCD(01)   NOT =   SPACE
                       MOVE    1               TO  FLG-TANTOU
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1000-SYS1011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクターコード検索処理
      *****************************************************************
       1002-SYS1011-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   20
               IF      SYS-1011-DRCD(IDX2) NOT =   SPACE
                   MOVE    SYS-1011-DRCD(IDX2) TO  WRK-KEY-DRCD
                   PERFORM 10001-DRCDNAME-HEN-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       ADD     1               TO  IDX
                       MOVE    SYS-1011-DRCD(IDX2)(2:4)
                                               TO  ORCSDR-DRCD (IDX)
                       MOVE    SYS-1010-NAME       TO  ORCSDR-DRCDMEI
                                                              (IDX)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
           MOVE    1                   TO  ORCSDR-DRCD-IDX
           .
       1002-SYS1011-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ドクターコード検索処理
      *****************************************************************
       10001-DRCDNAME-HEN-SEC            SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-KEY-DRCD        TO  SYS-1010-KBNCD(1:5)
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10001-DRCDNAME-HEN-EXT.
           EXIT.
      *H27.11
      *****************************************************************
      *    診療科担当医報編集処理
      *****************************************************************
       1003-LSDOCTOR-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-KADR-ARI
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    ORCSDR-SRYKA        TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_list_doctor"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
           PERFORM
               UNTIL  (FLG-LSDOCTOR    =   1  )
                   OR (IDX             >=  99 )
      *
               MOVE    LSDOCTOR-DRCD        TO  WRK-KEY-DRCD
               PERFORM 10001-DRCDNAME-HEN-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   ADD     1                   TO  IDX
                   MOVE    LSDOCTOR-DRCD(2:4)  TO  ORCSDR-DRCD (IDX)
                   MOVE    SYS-1010-NAME       TO  ORCSDR-DRCDMEI
                                                              (IDX)
                   MOVE    1                   TO  FLG-KADR-ARI
               END-IF
      *
               MOVE    "tbl_list_doctor"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_list_doctor"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
           MOVE    1                   TO  ORCSDR-DRCD-IDX
           .
       1003-LSDOCTOR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科担当医登録チェック処理
      *****************************************************************
       10031-LSDOCTOR-CHK-SEC               SECTION.
      *
      *
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    ORCSDR-SRYKA        TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_list_doctor"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
           IF      FLG-LSDOCTOR         =   ZERO
      *        診療科別あり
               MOVE    1                   TO  FLG-KADR-ARI
           ELSE
               MOVE    ZERO                TO  FLG-KADR-ARI
           END-IF
      *
           MOVE    "tbl_list_doctor"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10031-LSDOCTOR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    科別の編集（診療所）処理
      *****************************************************************
       1002-DRCD-TBL-SEC            SECTION.
      *
           IF      IDX3                >=  99
               GO      TO      1002-DRCD-TBL-EXT
           END-IF
           ADD     1                   TO  IDX3
           MOVE    SYS-1010-KBNCD(2:4) TO  WRK-DRCD    (IDX3)
           MOVE    SYS-1010-NAME       TO  WRK-DRCDMEI (IDX3)
           IF     (FLG-CHK2            =   1  )  AND
                  (WRK-DRCD-IDX        =   ZERO )
               MOVE    IDX3            TO  WRK-DRCD-IDX
           END-IF
      *
           .
       1002-DRCD-TBL-EXT.
           EXIT.
      *****************************************************************
      *    科別の専門家チェック処理
      *****************************************************************
       1001-SEMONKACD-CHK-SEC            SECTION.
      *
      *    専門科コード
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )   OR
                              (FLG-CHK     NOT =   ZERO )
               IF      SYS-1010-SEMONKACD (IDX2)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
                   IF      SYS-1010-SEMONKACD (IDX2)
                                           =   ORCSDR-SRYKA
                       MOVE    1               TO  FLG-CHK
                       MOVE    1               TO  FLG-CHK2
                   END-IF
               END-IF
           END-PERFORM
      *
      *    専門科がない時、全科対象とする
           IF      FLG-OK              =   ZERO
               MOVE    1               TO  FLG-CHK
           END-IF
           .
       1001-SEMONKACD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクターテーブル追加編集処理
      *****************************************************************
       1003-SYS1010-ADDHEN-SEC            SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ORCSDR-DRCD-MAX     TO  IDX
           MOVE    ZERO                TO  FLG-OVER
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1)     =   "1"
                   MOVE    ZERO                TO  FLG-CHK
                   MOVE    ZERO                TO  FLG-CHK3
                   PERFORM VARYING     IDX-D   FROM    1   BY  1
                           UNTIL      (IDX-D   >   ORCSDR-DRCD-MAX)
                                  OR  (FLG-CHK3    =   1     )
                       IF      SYS-1010-KBNCD(2:4)
                                               =   ORCSDR-DRCD (IDX-D)
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK3            =   ZERO
                       IF      ORCSDR-SRYKA    NOT =   SPACE
                           PERFORM 1001-SEMONKACD-CHK-SEC
                       END-IF
                   END-IF
                   IF      FLG-CHK             =   1
                       IF      IDX                 >=  99
                           MOVE    1               TO  FLG-OVER
                       ELSE
                           ADD     1                   TO  IDX
                           MOVE    SYS-1010-KBNCD(2:4)
                                                   TO  ORCSDR-DRCD
                                                                (IDX)
                           MOVE    SYS-1010-NAME   TO  ORCSDR-DRCDMEI
                                                                (IDX)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
      *
           .
       1003-SYS1010-ADDHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       200-DR-CHECK-SEC            SECTION.
      *
           IF      ORCSDR-IN-DRCD      =   SPACE
               MOVE    SPACE           TO  ORCSDR-IN-DRCD-G
               GO      TO      200-DR-CHECK-EXT
           END-IF
      *    コード変換・テーブルチェック
           PERFORM 2001-DRCD-TBLCHK-SEC
           IF     (ORCSDR-ERRCD    NOT =   ZERO  )  OR
                  (FLG-OK              =   1     )
               GO      TO      200-DR-CHECK-EXT
           END-IF
      *
      *    リストにない時
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    ORCSDR-IN-DRCD      TO  SYS-1010-KBNCD(2:4)
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SPACE               TO  ORCSDR-IN-DRCD-H
               MOVE    SYS-1010-NAME       TO  ORCSDR-OUT-DRCDMEI
      *        最後の行へ編集する
               IF      ORCSDR-DRCD-MAX     =   99
                   MOVE    99              TO  ORCSDR-DRCD-MAX
               ELSE
                   ADD     1               TO  ORCSDR-DRCD-MAX
               END-IF
               MOVE    ORCSDR-IN-DRCD-G    TO  ORCSDR-DRCD-TBL
                                                      (ORCSDR-DRCD-MAX)
           ELSE
               MOVE    1                   TO  ORCSDR-ERRCD
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-DR-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    リスト内容チェック処理（共通）
      *****************************************************************
       2001-DRCD-TBLCHK-SEC         SECTION.
      *
      *    コード変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    ORCSDR-IN-DRCD      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    2                   TO  ORCSDR-ERRCD
               GO      TO      2001-DRCD-TBLCHK-EXT
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  ORCSDR-IN-DRCD
           END-IF
           MOVE    SPACE               TO  ORCSDR-IN-DRCD-H
           MOVE    SPACE               TO  ORCSDR-OUT-DRCDMEI
      *
      *    コードチェック
           MOVE    ZERO                TO  ORCSDR-DRCD-IDX
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   ORCSDR-DRCD-MAX
               IF      ORCSDR-DRCD (IDX)   =   ORCSDR-IN-DRCD
                   MOVE    ORCSDR-DRCD-TBL (IDX)
                                           TO  ORCSDR-IN-DRCD-G
                   MOVE    IDX             TO  ORCSDR-DRCD-IDX
                   MOVE    ORCSDR-DRCD-MAX TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           .
       2001-DRCD-TBLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理（外来初期表示用）
      *****************************************************************
       300-DR-CHECK03-SEC            SECTION.
      *
           IF      ORCSDR-IN-DRCD      =   SPACE
               MOVE    SPACE           TO  ORCSDR-IN-DRCD-G
           END-IF
           MOVE    ZERO                TO  FLG-TANTOU
           IF      ORCSDR-DRCD-MAX     =    ZERO
      *        ドクターテーブル編集
               PERFORM 100-DR-HENSYU-SEC
           ELSE
      *        当日担当医
      *        IF      ORCSDR-SRYKA    NOT =   SPACE
      *            PERFORM 1000-SYS1011-CHK-SEC
      *        END-IF
      *H27.12
               IF      ORCSDR-SRYKA    NOT =   SPACE
      *            診療科別の判定
                   PERFORM 10031-LSDOCTOR-CHK-SEC
                   IF      FLG-KADR-ARI        =   ZERO
      *                当日担当医判定
                       PERFORM 1000-SYS1011-CHK-SEC
                   END-IF
               END-IF
           END-IF
           IF      FLG-TANTOU          =   ZERO
             AND   FLG-KADR-ARI        =   ZERO
      *        当日担当医無し
               IF      ORCSDR-IN-DRCD      =   SPACE
                   IF      ORCSDR-DRCD-IDX     =   ZERO
                       MOVE    1               TO  ORCSDR-DRCD-IDX
                   END-IF
                   MOVE    ORCSDR-DRCD-TBL (ORCSDR-DRCD-IDX)
                                           TO  ORCSDR-IN-DRCD-G
               ELSE
      *            ドクターチェック
                   PERFORM 200-DR-CHECK-SEC
               END-IF
           ELSE
      *        リストにない場合、１件目を初期とする
               PERFORM 2001-DRCD-TBLCHK-SEC
               IF     (ORCSDR-ERRCD        =   ZERO  ) AND
                      (FLG-OK              =   ZERO  )
                   MOVE    ORCSDR-DRCD-TBL (01)
                                           TO  ORCSDR-IN-DRCD-G
                   MOVE    01              TO  ORCSDR-DRCD-IDX
               END-IF
           END-IF
           .
       300-DR-CHECK03-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    担当医マスタ読み込み処理
      *****************************************************************
       900-LSDOCTOR-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  LIST-DOCTOR-REC
               MOVE    ZERO            TO  FLG-LSDOCTOR
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
      *
           .
       900-LSDOCTOR-READ-EXT.
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
