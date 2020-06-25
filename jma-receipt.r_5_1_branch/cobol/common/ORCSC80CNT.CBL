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
       PROGRAM-ID.                 ORCSC80CNT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : リハビリ算定回数検索サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/10/08    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  04/03/23  Ｈ１６年４月改正追加
      * 01.00.02     NACL-多々納  05/11/22  MONFUNC 対応 他
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
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
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-SANTEI-ARI      PIC 9(01).
           03  FLG-RIGAKU          PIC 9(01).
      *
           03  FLG-SOUKI-ALL       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX2                PIC 9(02).
      *
           03  IDD                 PIC 9(02).
           03  IDD2                PIC 9(02).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-RIGAKU-KBN      PIC X(01).
           03  WRK-RIGAKU-RYO      PIC X(01).
           03  WRK-RIGAKU-RYO-A    PIC X(01).
           03  WRK-RIGAK-CNT       PIC 9(03).
      *    訂正回数
           03  WRK-TEI-CNT                 PIC 9(03).
           03  WRK-MCNT                    PIC 9(03).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *    リハビリの早期リハ加算テーブル
           COPY   "CMRIHAKSNTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    リハビリ算定カウントパラメタ
           COPY    "CPORCSC80CNT.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
      *??  COPY    "K02SCR-SPA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
      *??  SPA-K02
           ORCSC00AREA
           ORCSC80CNTAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    当日まで算定済みのすべてのリハビリカウント
           PERFORM 100-RIGAKU-ALLCNT-SEC
      *
      *    早期リハビリ加算等のカウント
           PERFORM 200-SOUKI-ALLCNT-SEC
      *
           .
       000-PROC-EXT
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    当日まで算定済みのすべてのリハビリカウント処理
      *****************************************************************
       100-RIGAKU-ALLCNT-SEC           SECTION.
      *
           INITIALIZE                  ORCSC80CNT-RIGAKU-AREA
      *
      *    当日算定済みのすべての理学療法をカウントする
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL       FLG-SANTEI      =   1
      *
               MOVE    SANTEI-SRYCD        TO  WRK-SRYCD
               PERFORM 20021-RIGAKU-TBL-SEC
      *        リハビリであること
               MOVE    ZERO                TO  FLG-SANTEI-ARI
               IF      WRK-RIGAKU-RYO      =   "K"  OR  "S"
      *            労災の時、保険毎
                   IF      SPA-HKNKBN      NOT =   ZERO
                      IF      SPA-HKNCOMBINUM  =   SANTEI-HKNCOMBINUM
                           MOVE    1               TO  FLG-SANTEI-ARI
                       END-IF
                   ELSE
                      IF       SANTEI-HKNCOMBINUM  =   SPACE OR  ZERO
                           MOVE    1               TO  FLG-SANTEI-ARI
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI-ARI      =   1
      *            カウント
                   PERFORM 20011-SANTEI-CNT-SEC
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-RIGAKU-ALLCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリテーブル判定処理
      *****************************************************************
       20021-RIGAKU-TBL-SEC         SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH      TBL-SANTEI-OCC  VARYING   TBL-SAN
                   AT  END
                       MOVE    ZERO        TO  FLG-RIGAKU
                       MOVE    SPACE       TO  WRK-RIGAKU-KBN
                       MOVE    SPACE       TO  WRK-RIGAKU-RYO
               WHEN    WRK-SRYCD           =   TBL-SANTEI-SRYCD
                                                           (TBL-SAN)
                   MOVE    1                   TO  FLG-RIGAKU
                   MOVE    TBL-RIGAKU-KBN   (TBL-SAN)
                                               TO  WRK-RIGAKU-KBN
                   MOVE    TBL-RIGAKU-RYO   (TBL-SAN)
                                               TO  WRK-RIGAKU-RYO
           END-SEARCH
      *
           .
       20021-RIGAKU-TBL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       20011-SANTEI-CNT-SEC              SECTION.
      *
      *    訂正の時、当日算定有無の判定
           MOVE    ZERO                TO  WRK-TEI-CNT
           IF      ORCSC80CNT-SYORIFLG     =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   SPA-MAX-LINE)  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      WRK-SRYCD           =   LNK-ORCSC-TEI-SRYCD
                                                                 (IDZ)
                       ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
      *    当日までのカウント
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
      *    訂正分のクリア
           IF      ORCSC80CNT-SYORIFLG     =   1
               IF      SANTEI-MSANTEICNT   <   WRK-TEI-CNT
                   MOVE    ZERO            TO  SANTEI-MSANTEICNT
               ELSE
                   COMPUTE SANTEI-MSANTEICNT   =   SANTEI-MSANTEICNT
                                               -   WRK-TEI-CNT
               END-IF
               IF      SANTEI-DAY (IDD)    <   WRK-TEI-CNT
                   MOVE    ZERO                TO  SANTEI-DAY (IDD)
               ELSE
                   COMPUTE SANTEI-DAY (IDD)    =   SANTEI-DAY (IDD)
                                               -   WRK-TEI-CNT
               END-IF
           END-IF
      *
      *    入院の時、当月
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    31                  TO  IDD
           END-IF
      *
      *    当日までの回数
           MOVE    ZERO                TO  WRK-MCNT
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   IDD
               ADD     SANTEI-DAY (IDX2)   TO  WRK-MCNT
           END-PERFORM
      *
      *    リハビリの種別毎に集計する
      *    当日・当月分
           EVALUATE    WRK-RIGAKU-RYO
                                       ALSO  WRK-RIGAKU-KBN
      *        個別の理学
               WHEN    "K"             ALSO  "R"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-K
                                                                   (01)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-KN
                                                                   (01)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-KN
                                                              (01 IDX2)
                   END-PERFORM
      *        集団の理学
               WHEN    "S"             ALSO  "R"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-S
                                                                   (01)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-SN
                                                                   (01)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-SN
                                                              (01 IDX2)
                   END-PERFORM
      *        個別の作業
               WHEN    "K"             ALSO  "S"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-K
                                                                   (02)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-KN
                                                                   (02)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-KN
                                                              (02 IDX2)
                   END-PERFORM
      *        集団の作業
               WHEN    "S"             ALSO  "S"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-S
                                                                   (02)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-SN
                                                                   (02)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-SN
                                                              (02 IDX2)
                   END-PERFORM
      *        個別の言語
               WHEN    "K"             ALSO  "G"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-K
                                                                   (03)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-KN
                                                                   (03)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-KN
                                                              (03 IDX2)
                   END-PERFORM
      *        集団の言語
               WHEN    "S"             ALSO  "G"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-S
                                                                   (03)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-SN
                                                                   (03)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-SN
                                                              (03 IDX2)
                   END-PERFORM
           END-EVALUATE
      *
           .
       20011-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    早期リハビリ加算等のカウント処理
      *****************************************************************
       200-SOUKI-ALLCNT-SEC         SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "80"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
               IF     (ACCT-ZAITEN         =   ZERO )  OR
                      (ACCT-ZAIKAISU       =   ZERO )
      *          包括まとめ入力はリハビリ対象外
                 OR   (ACCT-HKNCOMBI       =   "9999")
                   CONTINUE
               ELSE
      *            診療行為マスター編集
                   PERFORM 2001-SRYACT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-SOUKI-ALLCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       2001-SRYACT-HEN-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  FLG-SOUKI-ALL
           MOVE    SPACE               TO  WRK-RIGAKU-RYO-A
      *
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
      *            リハビリであること
                   MOVE    SRY-SRYCD(IDX2)     TO  WRK-SRYCD
                   PERFORM 20021-RIGAKU-TBL-SEC
                   IF      WRK-RIGAKU-RYO  NOT =   SPACE
      **************   IF      WRK-RIGAKU-RYO-A    NOT =   SPACE
      **                  PERFORM 2004-SOUKI-HENSYU-SEC
      **************   END-IF
                       MOVE    WRK-RIGAKU-RYO      TO  WRK-RIGAKU-RYO-A
                   END-IF
      *            加算判定
                   MOVE    SRY-SRYCD(IDX2)     TO  WRK-SRYCD
                   PERFORM 20023-SOUKI-TBL-SEC
                   IF      FLG-SOUKI           =   1
                       MOVE    1                   TO  FLG-SOUKI-ALL
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    早期なし
           IF     (WRK-RIGAKU-RYO-A    =   SPACE  )  OR
                  (FLG-SOUKI-ALL       =   ZERO   )
               GO      TO      2001-SRYACT-HEN-EXT
           END-IF
      *
      *    早期リハビリ加算等加算処理
           PERFORM 2004-SOUKI-HENSYU-SEC
      *
           .
       2001-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    早期リハビリ加算等加算処理
      *****************************************************************
       2004-SOUKI-HENSYU-SEC         SECTION.
      *
      *    当日までのカウントとする
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
      *    訂正の場合
           IF      ORCSC80CNT-SYORIFLG     =   1
               IF      ACCT-DAY (1 IDD)    >=  WRK-TEI-CNT
                   COMPUTE ACCT-DAY (1 IDD)    =   ACCT-DAY (1 IDD)
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  ACCT-DAY (1 IDD)
               END-IF
           END-IF
      *
      *    入院の時、当月
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    31                  TO  IDD
           END-IF
      *
      *    早期あり
           PERFORM VARYING     IDD2    FROM    1   BY  1
                   UNTIL       IDD2    >   IDD
               ADD     ACCT-DAY (1 IDD2)  TO  ORCSC80CNT-GCNT  (IDD2)
           END-PERFORM
      *
      *
           .
       2004-SOUKI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    早期リハビリ加算等テーブル検索処理
      *****************************************************************
       20023-SOUKI-TBL-SEC         SECTION.
      *
      *Ｈ１６．４以前
           IF      SPA-SRYYMD          <   "20040401"
      *        早期リハビリ加算
               SET     TBL-RIDX            TO  1
               SEARCH      TBL-RIHABIRI-OCC    VARYING   TBL-RIDX
                       AT  END
                           MOVE    ZERO            TO  FLG-SOUKI
                       WHEN    WRK-SRYCD           =   TBL-RIHA-SRYCD
                                                           (TBL-RIDX)
                           MOVE    1               TO  FLG-SOUKI
               END-SEARCH
           ELSE
      *Ｈ１６．４から
      *        早期リハビリ加算
               SET     TBL-RIDX2           TO  1
               SEARCH      TBL-RIHABIRI2-OCC    VARYING   TBL-RIDX2
                       AT  END
                           MOVE    ZERO            TO  FLG-SOUKI
                       WHEN    WRK-SRYCD           =   TBL-RIHA2-SRYCD
                                                           (TBL-RIDX2)
                           MOVE    1               TO  FLG-SOUKI
               END-SEARCH
           END-IF
      *
           IF      FLG-SOUKI           =   1
      *        訂正の時、当日算定有無の判定
               MOVE    ZERO                TO  WRK-TEI-CNT
               IF      ORCSC80CNT-SYORIFLG     =   1
                   PERFORM VARYING     IDZ     FROM    1   BY  1
                           UNTIL      (IDZ     >   SPA-MAX-LINE )  OR
                                      (LNK-ORCSC-TEI-SRYCD(IDZ)
                                               =   SPACE )
                       IF      WRK-SRYCD       =   LNK-ORCSC-TEI-SRYCD
                                                                 (IDZ)
                           ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                                   TO  WRK-TEI-CNT
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       20023-SOUKI-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
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
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ読み込み
      *****************************************************************
       950-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       950-SRYACT-READ-EXT.
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
           .
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
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
