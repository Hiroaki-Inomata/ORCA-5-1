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
       PROGRAM-ID.                 ORCSCKNKCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 禁忌薬剤チェック・編集処理
      *                      （Ｋ０２、Ｋ０２ＮＹＵ共通使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/02/08    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.07.00    NACL-多々納  11/10/XX  チェックマスタの併用禁忌対応
      *  04.07.00    NACL-多々納  13/07/12  相互の症状コード編集対応
      *  04.08.00    NACL-多々納  16/08/XX  相互作用チェック区分対応
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
           03  FLG-END             PIC 9(01).
           03  FLG-INTERACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-ALLFLG          PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-KINKI-ARI       PIC 9(01).
           03  FLG-KINKI-ARI2      PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
      *
           03  FLG-KINKI-CHK       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-K4              PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-C               PIC 9(04).
      *
           03  IDX-S2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    禁忌期間
           03  WRK-KIKAN               PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
           03  WRK-TOUYMD.
               05  WRK-TOUYY       PIC 9(04).
               05  WRK-TOUMM       PIC 9(02).
               05  WRK-TOUDD       PIC 9(02).
           03  WRK-SRYYM-CHK           PIC X(06).
           03  WRK-NYUGAIKBN           PIC X(01).
      *    禁忌薬剤チェック
           03  WRK-KNKYAK-AREA.
              05  WRK-KNKYAK-TBL             OCCURS   100.
                   09  WRK-KNKYAK-SRYCD           PIC  X(09).
                   09  WRK-KNKYAK-TOUYMD          PIC  X(08).
              05  WRK-KNKYAK-MAX                  PIC  9(04).
      *
      *    禁忌チェック用テーブル
       01  WRK-KNKCHK-AREA.
           03  WRK-KNK-TBL             OCCURS   30.
               05  WRK-KNK-TOUMEI          PIC X(100). 
               05  WRK-KNK-TOUSRYCD        PIC X(09). 
               05  WRK-KNK-TBL2A.
                   07  WRK-KNK-TBL2            OCCURS   30.
                       09  WRK-KNK-KNKMEI          PIC X(100).
                       09  WRK-KNK-TOUYMD          PIC X(08).
                       09  WRK-KNK-TOUYMDW         PIC X(09).
                       09  WRK-KNK-SRYCD           PIC X(09).
                       09  WRK-KNK-SYOJYOUCD       PIC X(07).
                       09  WRK-KNK-CHKFLG          PIC X(01).
           03  WRK-KNK-MAX             PIC 9(04).
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   09  WRK-SRT-KNKMEI          PIC X(100).
                   09  WRK-SRT-TOUYMD          PIC X(08).
                   09  WRK-SRT-TOUYMDW         PIC X(09).
                   09  WRK-SRT-SRYCD           PIC X(09).
                   09  WRK-SRT-SYOJYOUCD       PIC X(07).
                   09  WRK-SRT-CHKFLG          PIC X(01).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-SYSTIME               PIC 9(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    相互作用マスタ
       01  INTERACT-REC.
           COPY    "CPINTERACT.INC".
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    パラメタ
           COPY    "CPORCSCKNKCHK.INC".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCKNKCHKAREA
           SPA-AREA
           SPA-K02
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           EVALUATE    ORCSCKNKCHK-KBN
      *TE      WHEN    "1"
      *TE          PERFORM 100-SRYACT-SPASET-SEC
               WHEN    "2"
                   PERFORM 200-KNKCHK-IN-SEC
               WHEN    "3"
                   PERFORM 300-KNKCHK-ALL-SEC
      *API用
               WHEN    "4"
                   PERFORM 100-SRYACT-SPASET-SEC
           END-EVALUATE
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    禁忌薬剤チェック用投薬編集処理
      *****************************************************************
       100-SRYACT-SPASET-SEC           SECTION.
      *
      *    相互作用チェック期間
           INITIALIZE                  SPA-KNKYAK-AREA
           INITIALIZE                  WRK-KNKYAK-AREA
           MOVE    ZERO                TO  IDX
      *    対象年月−１月を求める
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-SRYYM
           MOVE    SPA-INTERACT-KIKAN  TO  WRK-KIKAN
           PERFORM UNTIL     WRK-KIKAN     <   12
               COMPUTE WRK-SRYYY           =   WRK-SRYYY     -   1
               COMPUTE WRK-KIKAN           =   WRK-KIKAN
                                           -   12
           END-PERFORM
           IF      WRK-SRYMM           >   WRK-KIKAN
               COMPUTE WRK-SRYMM           =   WRK-SRYMM
                                           -   WRK-KIKAN
           ELSE
               COMPUTE WRK-SRYMM           =  (WRK-SRYMM
                                           +   12   )
                                           -   WRK-KIKAN
               COMPUTE WRK-SRYYY           =   WRK-SRYYY
                                           -   1
           END-IF
      *
           IF     (SPA-LSTYMD          =   SPACE ) OR
                 ((SPA-NYUGAIKBN       =   "2"   )
              AND (SPA-GMN-MAX1        =   ZERO  )
              AND (SPA-K02N-TAIINYMD   =   SPACE ))
               MOVE    1                   TO  FLG-END
           END-IF
      *    禁忌薬剤チェック用編集処理
           IF      FLG-END              =  ZERO
               PERFORM 1001-KNKYAKSET-HENSYU-SEC
           END-IF
      *
      *    相互作用レコードより、禁忌有無のチェックを行う
           INITIALIZE                  SPA-KNKYAK-AREA
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL      (IDX3        >   100 )  OR
                              (FLG-END     =   1   )
               IF      WRK-KNKYAK-SRYCD (IDX3) NOT =   SPACE
                   INITIALIZE                  INTERACT-REC
                   MOVE    WRK-KNKYAK-SRYCD (IDX3)
                                       TO  INTERACT-DRUGCD
      *
                   MOVE    ZERO                TO  FLG-INTERACT
                   MOVE    INTERACT-REC        TO  MCPDATA-REC
                   MOVE    "tbl_interact"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_interact"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 990-INTERACT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-INTERACT
                   END-IF
                   MOVE    "tbl_interact"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *            禁忌あり
                   IF      FLG-INTERACT          =   ZERO
                       ADD     1             TO  SPA-KNK-MAX
                       MOVE    WRK-KNKYAK-SRYCD (IDX3)
                                             TO  SPA-KNK-SRYCD
                                                (SPA-KNK-MAX)
                       MOVE    WRK-KNKYAK-TOUYMD (IDX3)
                                             TO  SPA-KNK-TOUYMD
                                                (SPA-KNK-MAX)
      *ver.4.7
                   ELSE
      *                チェックマスタ判定
                       PERFORM 1001-CHK-CHECK-SEC
      *ver.4.7
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100-SRYACT-SPASET-EXT.
           EXIT.
      *
      *ver.4.7
      *****************************************************************
      *    チェックマスタ対象判定処理
      *****************************************************************
       1001-CHK-CHECK-SEC              SECTION.
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    WRK-KNKYAK-SRYCD (IDX3)
                                       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
           MOVE    SPA-SRYYMD          TO  CHK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  CHK-YUKOEDYMD
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    チェックマスタあり
           IF      FLG-CHKMST          =   ZERO
               ADD     1               TO  SPA-KNK-MAX
               MOVE    WRK-KNKYAK-SRYCD (IDX3)
                                       TO  SPA-KNK-SRYCD
                                                (SPA-KNK-MAX)
               MOVE    WRK-KNKYAK-TOUYMD (IDX3)
                                       TO  SPA-KNK-TOUYMD
                                                (SPA-KNK-MAX)
           END-IF
      *
           .
       1001-CHK-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       1001-KNKYAKSET-HENSYU-SEC              SECTION.
      *
      *    外来分
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 10011-KNKYAKSET-HENSYU-02-SEC
      *
      *    入院
           IF    ( SPA-NYUGAIKBN       =   "1"  )  OR
                 ((SPA-NYUGAIKBN       =   "2"  )  AND
                  (SPA-K02N-TAIINYMD   NOT =   SPACE ) AND
                  (SPA-K02N-TAIINYMD(1:6)  >=  WRK-SRYYM ))
               MOVE    "1"                 TO  WRK-NYUGAIKBN
               PERFORM 10011-KNKYAKSET-HENSYU-02-SEC
           END-IF
           .
       1001-KNKYAKSET-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       10011-KNKYAKSET-HENSYU-02-SEC    SECTION.
      *
      *    診療会計マスタよりチェックする
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
      *    診療年月検索用項目
           MOVE    WRK-SRYYM           TO  ACCT-STSRYYM
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-EDSRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key46"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL     (FLG-SRYACCT     =   1          )
               PERFORM 1002-KNK-KOUISET-SEC
      *
               MOVE    "key46"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-KNKYAKSET-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤編集処理
      *****************************************************************
       1002-KNK-KOUISET-SEC              SECTION.
      *
      *    最終投薬日
           MOVE    ZERO                TO  WRK-TOUYMD
           MOVE    ACCT-SRYYM          TO  WRK-TOUYMD(1:6)
           PERFORM VARYING     IDX-Y2  FROM    31  BY  -1
                   UNTIL       IDX-Y2  <   1
               IF      ACCT-DAY (1 IDX-Y2) >   ZERO
                   MOVE    IDX-Y2          TO  WRK-TOUDD
                   MOVE    1               TO  IDX-Y2
               END-IF
           END-PERFORM
      *
           IF      WRK-TOUDD           =   ZERO
               GO      TO      1002-KNK-KOUISET-EXT
           END-IF
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
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
               IF      SRY-SRYCD (IDX2)(1:1)   =   "6"
                   PERFORM VARYING     IDX3    FROM    1   BY  1
                           UNTIL       IDX3        >   100
                       IF      WRK-KNKYAK-SRYCD (IDX3)  =   SPACE
                           MOVE    SRY-SRYCD (IDX2) TO  WRK-KNKYAK-SRYCD
                                                             (IDX3)
                           MOVE    IDX3             TO  WRK-KNKYAK-MAX
                       END-IF
                       IF      WRK-KNKYAK-SRYCD (IDX3)
                                                   =  SRY-SRYCD (IDX2)
      *                    最終投薬日のセット
                           IF     (WRK-KNKYAK-TOUYMD(IDX3)
                                                   =   SPACE )  OR
                                  (WRK-KNKYAK-TOUYMD(IDX3)
                                                     <  WRK-TOUYMD )
                               MOVE    WRK-TOUYMD
                                               TO  WRK-KNKYAK-TOUYMD
                                                              (IDX3)
                           END-IF
      *
                           MOVE    100             TO  IDX3
                       END-IF
                   END-PERFORM
               END-IF
      *
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1002-KNK-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       200-KNKCHK-IN-SEC           SECTION.
      *
           INITIALIZE                  SPA-K021GMN-AREA
           MOVE    ZERO                TO  FLG-KINKI
           MOVE    ZERO                TO  IDX-K1
           MOVE    1                   TO  FLG-ALLFLG
           INITIALIZE                  WRK-KNKCHK-AREA
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (FLG-KINKI   NOT =   ZERO )   OR
                             ((SPA-GMN-INPUTCD(IDX-Z)  =   SPACE ) AND
                              (SPA-COM-INPUTCD(IDX-Z)  =   SPACE ))
               IF     (SPA-COM-INPUTCD (IDX-Z)(1:1)    =  "6" )
      *            禁忌薬剤チェック処理
                   IF      (SPA-COM-KNKFLG (IDX-Z)  =   SPACE )  AND
                           (SPA-COM-KNKUMU (IDX-Z)  =   SPACE )
      *H28.8
      *                相互作用チェック区分判定
                       IF      SPA-INTERACT-CHKKBN  =   "1"
      *                    チェックマスタのみ対象
                           PERFORM 2003-CHKMSTONLY-CHK-SEC
                       ELSE
      *
                           PERFORM 2001-KNKYAK-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    エラー画面編集
           IF      FLG-KINKI           =   1
               PERFORM 2002-K021-SYORI-SEC
           END-IF
           .
       200-KNKCHK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       2001-KNKYAK-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI-ARI2
      *    相互作用レコードより、禁忌有無のチェックを行う
           INITIALIZE                  INTERACT-REC
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  INTERACT-DRUGCD
      *
           MOVE    ZERO                TO  FLG-INTERACT
           MOVE    INTERACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-INTERACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INTERACT
           END-IF
      *    禁忌なし
           IF      FLG-INTERACT        =   1
               MOVE    "1"             TO  SPA-COM-KNKUMU (IDX-Z)
           END-IF
      *
      *    １回目
           IF     (FLG-INTERACT        =   ZERO )  AND
                  (SPA-KNK-SRYCD (100) =   SPACE )
               PERFORM 2000-KNK-SPASET-SEC
           END-IF
      *
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K2
           ADD     1                   TO  IDX-K1
           MOVE    ZERO                TO  FLG-ARI2
           PERFORM
                   UNTIL      FLG-INTERACT     =   1
               PERFORM 20011-KNKCHK-SYORI-SEC
      *
               MOVE    "tbl_interact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-INTERACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-K2              >   ZERO
               MOVE    "1"                 TO  SPA-COM-KNKFLG (IDX-Z)
               MOVE    SPA-COM-NAME   (IDX-Z)
                                           TO  WRK-KNK-TOUMEI (IDX-K1)
               MOVE    SPA-COM-INPUTCD(IDX-Z)
                                           TO  WRK-KNK-TOUSRYCD (IDX-K1)
               MOVE    1                   TO  FLG-KINKI
               MOVE    1                   TO  FLG-ARI2
           END-IF
      *ver.4.7
      *    チェックマスタとのチェック
           PERFORM 2009-KNKYKCHKMST-CHK-SEC
      *    禁忌なし
           IF      FLG-ARI2            =   ZERO
               INITIALIZE                  WRK-KNK-TBL (IDX-K1)
               COMPUTE IDX-K1          =   IDX-K1   -   1
           END-IF
      *
           .
       2001-KNKYAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為薬剤編集処理
      *****************************************************************
       2000-KNK-SPASET-SEC           SECTION.
      *
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    禁忌薬剤チェック用投薬編集処理
           PERFORM 100-SRYACT-SPASET-SEC
           IF      SPA-KNK-SRYCD (100) =   SPACE
               MOVE    "999999999"         TO  SPA-KNK-SRYCD (100)
           END-IF
      *
      *    再検索
           INITIALIZE                  INTERACT-REC
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  INTERACT-DRUGCD
           MOVE    INTERACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-INTERACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INTERACT
           END-IF
           .
       2000-KNK-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       20011-KNKCHK-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI-ARI
           MOVE    ZERO                TO  FLG-KINKI-CHK
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   SPA-KNK-MAX )  OR
                              (FLG-KINKI-ARI   =   1   ) OR
                              (IDX-K2          >   30  )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
      *H25.7
               MOVE    ZERO                    TO  FLG-KINKI-CHK
               IF    ( SPA-COM-INPUTCD (IDX-Z)
                                           NOT =   SPA-KNK-SRYCD(IDX-S))
      *            前
                   IF     (SPA-KNK-SRYCD (IDX-S)   =   INTERACT-DRUGCD2)
                       MOVE    1                   TO  FLG-KINKI-CHK
                   END-IF
      *            後
                   IF     (SPA-KNK-SRYCD (IDX-S)   =   INTERACT-DRUGCD)
                       MOVE    2                   TO  FLG-KINKI-CHK
                   END-IF
      *            今回入力分は対象外
                   PERFORM 200111-INPUT-CHK-SEC
               END-IF
               IF      FLG-KINKI-CHK       NOT =   ZERO
                   MOVE    1                   TO  FLG-KINKI-ARI
                   MOVE    1                   TO  FLG-KINKI-ARI2
                   ADD     1                   TO  IDX-K2
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       MOVE    SPA-KNK-SRYCD (IDX-S)   TO  TNS-SRYCD
                       PERFORM 919-TENSU-READ2-SEC
                   END-IF
                   MOVE    TNS-NAME                TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  WRK-KNK-SRYCD
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-KNK-TOUYMD (IDX-S)  TO  WRK-KNK-TOUYMD
                                                        (IDX-K1 IDX-K2)
                   MOVE    INTERACT-SYOJYOUCD      TO  WRK-KNK-SYOJYOUCD
                                                        (IDX-K1 IDX-K2)
                   MOVE    FLG-KINKI-CHK           TO  WRK-KNK-CHKFLG
                                                        (IDX-K1 IDX-K2)
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           IF      FLG-ALLFLG          =   1
               MOVE    1                   TO  IDX-K3
               COMPUTE IDX-K4              =   IDX-Z   -   1
           ELSE
      *H25.7   COMPUTE IDX-K3              =   IDX-Z   +   1
               MOVE    1                   TO  IDX-K3
               MOVE    SPA-MAX-LINE        TO  IDX-K4
           END-IF
           PERFORM VARYING     IDX-S       FROM    IDX-K3  BY  1
                   UNTIL      (IDX-S           >   IDX-K4 )  OR
                              (FLG-KINKI-ARI   =   1    ) OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-S)  =   SPACE )
      *****    IF    ((SPA-COM-INPUTCD (IDX-S) =   INTERACT-DRUGCD)  OR
      *******         (SPA-COM-INPUTCD (IDX-S) =   INTERACT-DRUGCD2))
               IF     (SPA-COM-INPUTCD (IDX-S) =   INTERACT-DRUGCD2)
                 AND  (SPA-COM-INPUTCD (IDX-S)
                                           NOT =   SPA-COM-INPUTCD
                                                           (IDX-Z))
                 AND  (IDX-S               NOT =   IDX-Z          )
                   ADD     1                   TO  IDX-K2
      **           MOVE    SPA-GMN-MEISYO (IDX-S)  TO  WRK-KNK-KNKMEI
      **                                                (IDX-K1 IDX-K2)
                   MOVE    SPA-COM-NAME   (IDX-S)  TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-COM-INPUTCD (IDX-S) TO  WRK-KNK-SRYCD
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-NAI-SRYYMD          TO  WRK-KNK-TOUYMD
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-GMN-SRYYMD(1:9)     TO  WRK-KNK-TOUYMDW
                                                        (IDX-K1 IDX-K2)
      *
                   MOVE    1                       TO  FLG-KINKI-ARI
                   MOVE    1                       TO  FLG-KINKI-ARI2
               END-IF
           END-PERFORM
      *
           IF      FLG-KINKI-ARI       =   1
               MOVE    INTERACT-SYOJYOUCD  TO  WRK-KNK-SYOJYOUCD
                                                        (IDX-K1 IDX-K2)
           END-IF
      *
           .
       20011-KNKCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回入力分チェック処理
      *****************************************************************
       200111-INPUT-CHK-SEC              SECTION.
      *
           PERFORM VARYING     IDX-S2      FROM    1   BY  1
                   UNTIL      (IDX-S2          >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD(IDX-S2) =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-S2) =   SPACE )
               IF     (SPA-COM-INPUTCD (IDX-S2)
                                            =   SPA-KNK-SRYCD (IDX-S))
                 AND  (SPA-COM-INPUTCD (IDX-S2)
                                        NOT =   SPA-COM-INPUTCD
                                                           (IDX-Z))
                   MOVE    ZERO             TO  FLG-KINKI-CHK
               END-IF
           END-PERFORM
           .
       200111-INPUT-CHK-EXT.
           EXIT.
      *
      *
      *ver.4.7
      *****************************************************************
      *    チェックマスタとのチェック処理
      *****************************************************************
       2009-KNKYKCHKMST-CHK-SEC              SECTION.
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
           MOVE    SPA-SRYYMD          TO  CHK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  CHK-YUKOEDYMD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
           PERFORM
                   UNTIL      FLG-CHKMST       =   1
               MOVE    ZERO                    TO  FLG-ARI
               PERFORM VARYING     IDX-C   FROM    1   BY  1
                       UNTIL      (IDX-C       >   30  )  OR 
                                  (WRK-KNK-SRYCD (IDX-K1 IDX-C)
                                               =   SPACE)
                   IF      WRK-KNK-SRYCD (IDX-K1 IDX-C) =   CHK-CD
                                                        OR  CHK-SRYCD
                        MOVE    1              TO  FLG-ARI
                        MOVE    30             TO  IDX-C
                   END-IF
               END-PERFORM
      *        １回目
               IF     (FLG-ARI             =   ZERO  )  AND
                      (SPA-KNK-SRYCD (100) =   SPACE )
                   PERFORM 20091-KINCHK-SPASET-SEC
               END-IF
               IF      FLG-ARI             =   ZERO
                   MOVE    CHK-SRYCD           TO  INTERACT-DRUGCD
                   MOVE    CHK-CD              TO  INTERACT-DRUGCD2
                   MOVE    ALL "*"             TO  INTERACT-SYOJYOUCD
                   PERFORM 20011-KNKCHK-SYORI-SEC
               END-IF
      *
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      IDX-K2              >   ZERO
               MOVE    "1"                 TO  SPA-COM-KNKFLG (IDX-Z)
               MOVE    SPA-COM-NAME   (IDX-Z)
                                           TO  WRK-KNK-TOUMEI (IDX-K1)
               MOVE    SPA-COM-INPUTCD(IDX-Z)
                                           TO  WRK-KNK-TOUSRYCD (IDX-K1)
               MOVE    1                   TO  FLG-KINKI
      *        禁忌あり
               MOVE    SPACE               TO  SPA-COM-KNKUMU (IDX-Z)
               MOVE    1                   TO  FLG-ARI2
           END-IF
           .
       2009-KNKYKCHKMST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為薬剤編集処理（チェックマスタ）
      *****************************************************************
       20091-KINCHK-SPASET-SEC           SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    禁忌薬剤チェック用投薬編集処理
           PERFORM 100-SRYACT-SPASET-SEC
           IF      SPA-KNK-SRYCD (100) =   SPACE
               MOVE    "999999999"         TO  SPA-KNK-SRYCD (100)
           END-IF
      *
      *    再検索
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
           MOVE    SPA-SRYYMD          TO  CHK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  CHK-YUKOEDYMD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
           .
       20091-KINCHK-SPASET-EXT.
           EXIT.
      *
      *H28.8
      *****************************************************************
      *    チェックマスタのみ対象処理
      *****************************************************************
       2003-CHKMSTONLY-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI-ARI2
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K2
           ADD     1                   TO  IDX-K1
           MOVE    ZERO                TO  FLG-ARI2
      *    チェックマスタとのチェック
           PERFORM 2009-KNKYKCHKMST-CHK-SEC
      *    禁忌なし
           IF      FLG-ARI2            =   ZERO
               MOVE    "1"             TO  SPA-COM-KNKUMU (IDX-Z)
      *
               INITIALIZE                  WRK-KNK-TBL (IDX-K1)
               COMPUTE IDX-K1          =   IDX-K1   -   1
           END-IF
           .
       2003-CHKMSTONLY-CHK-EXT.
           EXIT.
      *H28.8
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示処理
      *****************************************************************
       2002-K021-SYORI-SEC              SECTION.
      *
      *    同一薬剤を削除する
           PERFORM 20021-KNK-SORT-SEC
      *
      *    ＳＰＡセット
           INITIALIZE                  SPA-K021GMN-AREA
           MOVE    FLG-ALLFLG          TO  SPA-K021-SYORI
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (IDX-K           >=  30  )
             IF        WRK-KNK-TOUSRYCD (IDX-K1)   NOT =   SPACE
               ADD     1                   TO  IDX-K
               MOVE    IDX-K               TO  SPA-K021-TSEQNO (IDX-K)
               MOVE    WRK-KNK-TOUMEI (IDX-K1)
                                           TO  SPA-K021-TTOUMEI(IDX-K)
               PERFORM VARYING     IDX-K2      FROM    1   BY  1
                       UNTIL      (IDX-K2          >   30  )  OR
                                  (IDX-K           >   30  )  OR
                                  (WRK-KNK-KNKMEI(IDX-K1 IDX-K2)
                                                   =   SPACE)
                   IF      IDX-K2              >   1
                       ADD     1                   TO  IDX-K
                       MOVE    IDX-K               TO  SPA-K021-TSEQNO
                                                               (IDX-K)
                   END-IF
      *            方向設定
                   IF     (WRK-KNK-CHKFLG (IDX-K1 IDX-K2)
                                               =  "2"  )
                       STRING   "←"
                               WRK-KNK-KNKMEI (IDX-K1 IDX-K2)
                               DELIMITED  BY  SIZE
                                       INTO    SPA-K021-TKNKMEI(IDX-K)
                       END-STRING
                   ELSE
                       STRING   "→"
                               WRK-KNK-KNKMEI (IDX-K1 IDX-K2)
                               DELIMITED  BY  SIZE
                                       INTO    SPA-K021-TKNKMEI(IDX-K)
                       END-STRING
                   END-IF
      *            チェックマスタは方向なし
                   IF      WRK-KNK-SYOJYOUCD (IDX-K1 IDX-K2)
                                               =   ALL "*"
                       MOVE    WRK-KNK-KNKMEI (IDX-K1 IDX-K2)
                                           TO  SPA-K021-TKNKMEI(IDX-K)
                   END-IF
                   MOVE    WRK-KNK-TOUYMD (IDX-K1 IDX-K2)
                                           TO  SPA-K021-TTOUYMD (IDX-K)
                   MOVE    WRK-KNK-SYOJYOUCD (IDX-K1 IDX-K2)
                                           TO  SPA-K021-TSYOJYOUCD
                                                               (IDX-K)
                   IF      WRK-KNK-TOUYMDW (IDX-K1 IDX-K2) =   SPACE
                       MOVE    WRK-KNK-TOUYMD (IDX-K1 IDX-K2)
                                               TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG
                                           TO  SPA-K021-TTOUYMDW(IDX-K)
                   ELSE
                       MOVE    "今回入力"  TO  SPA-K021-TTOUYMDW(IDX-K)
                   END-IF
               END-PERFORM
             END-IF
           END-PERFORM
           MOVE    IDX-K               TO  SPA-K021-MAX
      *
           .
       2002-K021-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤 同一薬剤を削除処理
      *****************************************************************
       20021-KNK-SORT-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-KNK-MAX
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (WRK-KNK-TOUSRYCD (IDX-K1)  =   SPACE )
               ADD     1                   TO  WRK-KNK-MAX
      *
               MOVE    WRK-KNK-TBL2A(IDX-K1)   TO  WRK-SRT-TBL2A
               INITIALIZE                      WRK-KNK-TBL2A(IDX-K1)
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   PERFORM VARYING     IDX-K2      FROM    1   BY  1
                           UNTIL       IDX-K2      >   30
                       IF     (IDX-K           NOT =   IDX-K2)  AND
                              (WRK-SRT-SRYCD (IDX-K)
                                              =   WRK-SRT-SRYCD(IDX-K2))
      *!!!
                        AND   (WRK-SRT-SYOJYOUCD (IDX-K)
                                              =   WRK-SRT-SYOJYOUCD
                                                              (IDX-K2))
                           IF      WRK-SRT-TOUYMD (IDX-K)
                                               <   WRK-SRT-TOUYMD
                                                               (IDX-K2)
                               MOVE    WRK-SRT-TOUYMD(IDX-K2)
                                                   TO  WRK-SRT-TOUYMD
                                                                (IDX-K)
                           END-IF
                           MOVE    SPACE           TO  WRK-SRT-TBL2
                                                           (IDX-K2)
                       END-IF
                   END-PERFORM
               END-PERFORM
               MOVE    ZERO                    TO  IDX-K2
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   IF      WRK-SRT-TBL2  (IDX-K)   NOT =   SPACE
                       IF      WRK-SRT-CHKFLG (IDX-K)  NOT =   "2"
                           ADD     1               TO  IDX-K2
                           MOVE    WRK-SRT-TBL2  (IDX-K)
                                                   TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K2)
                       END-IF
                   END-IF
               END-PERFORM
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   IF      WRK-SRT-TBL2  (IDX-K)   NOT =   SPACE
                       IF      WRK-SRT-CHKFLG (IDX-K)  =   "2"
                           ADD     1               TO  IDX-K2
                           MOVE    WRK-SRT-TBL2  (IDX-K)
                                                   TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K2)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    今回入力分は１明細のみとする
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL       IDX-K1          >   WRK-KNK-MAX
               PERFORM VARYING     IDX-K2      FROM    1   BY  1
                       UNTIL       IDX-K2      >   30
                   IF      WRK-KNK-TOUYMDW(IDX-K1 IDX-K2)
                                           NOT =   SPACE
                       PERFORM 200211-KNK-KONCHK-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL       IDX-K1          >   WRK-KNK-MAX
               MOVE    WRK-KNK-TBL2A(IDX-K1)   TO  WRK-SRT-TBL2A
               INITIALIZE                      WRK-KNK-TBL2A(IDX-K1)
               MOVE    ZERO                TO  IDX-K2
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   IF      WRK-SRT-TBL2  (IDX-K)   NOT =   SPACE
                       ADD     1               TO  IDX-K2
                       MOVE    WRK-SRT-TBL2  (IDX-K)
                                               TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K2)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       20021-KNK-SORT-EXT.
           EXIT.
      *****************************************************************
      *    今回入力分処理
      *****************************************************************
       200211-KNK-KONCHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX-K3  FROM   IDX-K1 BY  1
                       UNTIL       IDX-K3  >   WRK-KNK-MAX
               IF      WRK-KNK-SRYCD (IDX-K1 IDX-K2)
                                           =   WRK-KNK-TOUSRYCD
                                                            (IDX-K3)
                   PERFORM VARYING     IDX-K4      FROM    1   BY  1
                           UNTIL       IDX-K4      >   30
                       IF     (WRK-KNK-TOUYMDW(IDX-K3 IDX-K4)
                                               NOT =   SPACE )  AND
                              (WRK-KNK-SRYCD (IDX-K3 IDX-K4)
                                                   =   WRK-KNK-TOUSRYCD
                                                            (IDX-K1))
                       AND    (WRK-KNK-SYOJYOUCD(IDX-K3 IDX-K4)
                                                   =   WRK-KNK-SYOJYOUCD
                                                        (IDX-K1 IDX-K2))
                           MOVE    SPACE           TO  WRK-KNK-TBL2
                                                        (IDX-K3 IDX-K4)
                       END-IF
                   END-PERFORM
                   IF      WRK-KNK-TBL2A (IDX-K3)  =   SPACE
                        MOVE    SPACE       TO  WRK-KNK-TOUSRYCD
                                                            (IDX-K3)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200211-KNK-KONCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理（全体）
      *****************************************************************
       300-KNKCHK-ALL-SEC           SECTION.
      *
           INITIALIZE                  SPA-K021GMN-AREA
           MOVE    ZERO                TO  FLG-KINKI
           MOVE    ZERO                TO  IDX-K1
           MOVE    2                   TO  FLG-ALLFLG
           INITIALIZE                  WRK-KNKCHK-AREA
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE    )   OR
                             ((SPA-GMN-INPUTCD(IDX-Z)  =   SPACE ) AND
                              (SPA-COM-INPUTCD(IDX-Z)  =   SPACE ))
               IF     (SPA-COM-INPUTCD(IDX-Z)(1:1)   =   "6" ) AND
                      (SPA-COM-KNKUMU (IDX-Z)  =   SPACE     )
                   MOVE    ZERO                TO  FLG-KINKI-ARI
      *H28.8
      *            相互作用チェック区分判定
                   IF      SPA-INTERACT-CHKKBN  =   "1"
      *                チェックマスタのみ対象
                       PERFORM 2003-CHKMSTONLY-CHK-SEC
                   ELSE
                       PERFORM 2001-KNKYAK-CHK-SEC
                   END-IF
      *
                   IF      FLG-KINKI-ARI2      =   1
                       MOVE    "3"             TO  SPA-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    初期画面の時、エラー画面なし
           IF      FLG-KINKI           =   1
               PERFORM 2002-K021-SYORI-SEC
           END-IF
           .
       300-KNKCHK-ALL-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
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
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
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
       940-SINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
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
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       919-TENSU-READ2-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
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
           PERFORM UNTIL     FLG-TENSU     =   1
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       919-TENSU-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    相互作用マスタ読込
      *****************************************************************
       990-INTERACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_interact"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INTERACT-REC
               MOVE    ZERO            TO  FLG-INTERACT
           ELSE
               INITIALIZE                  INTERACT-REC
               MOVE    1               TO  FLG-INTERACT
           END-IF
      *
           .
       990-INTERACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ読込
      *****************************************************************
       950-CHK-READ-SEC         SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHKMST
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           .
       950-CHK-READ-EXT.
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
