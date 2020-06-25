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
       PROGRAM-ID.                 ORCSHKNSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 保険組合編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/02/24    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/05/12  自賠責割合変更追加
      * 01.00.02     NACL-多々納  03/07/29  労災割合変更追加
      * 01.00.03     NACL-多々納  04/01/06  包括まとめ入力追加
      * 01.00.04     NACL-多々納  05/08/17  保険者番号、公費ＩＤ返却追加
      * 02.07.00     NACL-太田    05/10/24   MONFUNC 対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.01.00     NACL-多々納  07/10/19  公務災害追加
      * 04.01.01     NACL-多々納  07/11/12  非表示区分対応追加
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０．４改正対応
      * 04.05.00     NACL-多々納  10/01/XX  治験対応
      * 04.06.00     NACL-太田    10/08/30  補助区分、本人家族区分追加
      * 04.06.00     NACL-多々納  10/09/29  入院・外来非該当対応
      * 04.08.00     NACL-多々納  13/07/XX  第三者行為対応
      * 04.08.00     NACL-多々納  14/01/XX  負担割合公費対応
      * 05.00.00     NACL-多々納  19/02/08  処理区分４ 件数200件追加
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
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-HKNCOMBICHK     PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-HKNCOMBIERR     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *    年齢計算
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
           03  WRK-NENREI-YY           PIC 9(03).
           03  WRK-NENREI-MM           PIC 9(02).
           03  WRK-NENREI              PIC 9(03).
      *
           03  WRK-ZZ9                 PIC ZZZ.
      *
           03  WRK-HKNCOMBI            PIC X(100).
      *
           03  WRK-HKNCOMBINUM         PIC X(04).
      **   03  WRK-HKNKBN-MEI          PIC X(02).
           03  WRK-HKNKBN-MEI          PIC X(06).
      *
           03  WRK-JIBAISEKIKBN        PIC X(01).
      *
      *    表示用保険
           03  WRK-IDXH                PIC 9(04).
           03  WRK-IDXH2               PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *
           03  WRK-SRYYMD              PIC X(08).
      *    最大件数
           03  WRK-MAX-CNT             PIC 9(04).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    チェック機能制御
           COPY  "CPSK1008.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
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
      *    年齢・負担割合サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *!!
      *    保険組み合わせ負担割合取得サブ
           COPY    "CPORCSFTNRATE.INC".
      *!!
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *****COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSHKNSETAREA
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
           INITIALIZE                  ORCSHKN-ERRCD
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           IF      SPA-SRYYMD(7:2)     =   "00"
               MOVE    "01"            TO  WRK-SRYYMD(7:2)
           END-IF
      *H31.2
      *    最大件数変更
           MOVE    100                 TO  WRK-MAX-CNT
           IF      ORCSHKN-KBN4        =   "1"
      *        最大 200件へ
               MOVE    200                 TO  WRK-MAX-CNT
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 100-HKNCOMBI-HENSYU-SEC
      *
      *    包括保険追加編集
           IF      ORCSHKN-KBN2        =   "1"
               PERFORM 400-HOUKATU-HENSYU-SEC
           END-IF
      *
           IF      ORCSHKN-HKN-MAX     >   ZERO
      *        今回対象保険
               PERFORM 200-HKNCOMBI-SELECT-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       1001-NENREI-SEC            SECTION.
      *
           MOVE    SPA-BIRTHDAY        TO  WRK-BYMD
      *
      *    保険算定年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    WRK-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    保険年齢
           MOVE    AGECHK-NENREI       TO  WRK-NENREI
      *
           .
       1001-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       100-HKNCOMBI-HENSYU-SEC            SECTION.
      *
           INITIALIZE                  ORCSHKN-OUT-AREA
           MOVE    ZERO                TO  FLG-HKNCOMBICHK
      *
      *    システム管理（労災情報）検索
           PERFORM 1002-RSI-SYSKANRI-SEC
      *
      *    年齢計算処理
           PERFORM 1001-NENREI-SEC
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (FLG-HKNCOMBI        =   1      )  OR
      ******************  (IDX                 >=  100    )
                          (IDX                 >=  WRK-MAX-CNT)
      *
      *        対象の保険組合せのみ編集
               MOVE    ZERO                TO  FLG-HKNCOMBICHK
               MOVE    ZERO                TO  FLG-HKNCOMBIERR
               IF      COMB-DELKBN         =   "1"
                   MOVE    1                   TO  FLG-HKNCOMBIERR
               ELSE
      *            非表示分は処理区分３＝１ のときのみ対象
      *                      処理区分３＝２　第三者行為のみ対象外追加
                   IF      COMB-HYOJIKBN       =   "1"
                       IF      ORCSHKN-KBN3        =   "1"
                                                   OR  "2"
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-HKNCOMBIERR
                       END-IF
                   END-IF
      *            入院非表示分は外来・処理区分３＝１ のときのみ対象
      *                      処理区分３＝２　第三者行為のみ対象外追加
                   IF      COMB-HYOJIKBN       =   "2"
                       IF     (ORCSHKN-KBN3        =   "1" )
                         OR   (ORCSHKN-KBN3        =   "2" )
                         OR   (SPA-NYUGAIKBN       =   "2" )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-HKNCOMBIERR
                       END-IF
                   END-IF
      *            外来非表示分は入院・処理区分３＝１ のときのみ対象
      *                      処理区分３＝２　第三者行為のみ対象外追加
                   IF      COMB-HYOJIKBN       =   "3"
                       IF     (ORCSHKN-KBN3        =   "1" )
                         OR   (ORCSHKN-KBN3        =   "2" )
                         OR   (SPA-NYUGAIKBN       =   "1" )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-HKNCOMBIERR
                       END-IF
                   END-IF
               END-IF
               IF      FLG-HKNCOMBIERR         =   ZERO
                   IF      ORCSHKN-KBN         =   "2"
      *                全体
                       MOVE    1               TO  FLG-HKNCOMBICHK
                   ELSE
      *                診療年月
                       IF      SPA-SRYYMD(7:2) =   ZERO
                           IF      (SPA-SRYYMD(1:6)
                                               >=  COMB-TEKSTYMD(1:6))
                               AND (SPA-SRYYMD(1:6)
                                               <=  COMB-TEKEDYMD(1:6))
                               MOVE    1           TO  FLG-HKNCOMBICHK
                           END-IF
                       ELSE
      *                診療日対象のみ
                           IF      (SPA-SRYYMD     >=  COMB-TEKSTYMD )
                             AND   (SPA-SRYYMD     <=  COMB-TEKEDYMD )
                               MOVE    1           TO  FLG-HKNCOMBICHK
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *H25.7
      *        処理区分３＝２の時、第三者行為は対象外
               IF     (FLG-HKNCOMBICHK     =    1    )  AND
                      (ORCSHKN-KBN3        =   "2"   )
                   IF      COMB-KOH1HKNNUM     =   "970" 
                       MOVE    ZERO            TO  FLG-HKNCOMBICHK
                   END-IF
               END-IF
      *
      *        労災・自賠責保険検索
               IF     (FLG-HKNCOMBICHK     =    1    )  AND
                     ((COMB-HKNNUM         =   "971"  OR  "973")
                  OR  (COMB-KOH1HKNNUM     =   "970"   ))
                   PERFORM 1100-PTRSIINF-CHK-SEC
                   IF      FLG-PTRSIINF        =   1
                       MOVE    ZERO            TO  FLG-HKNCOMBICHK
                   END-IF
               END-IF
      *
               IF      FLG-HKNCOMBICHK     =    1
                   PERFORM 110-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM
      *    最大件数
           MOVE    IDX                 TO  ORCSHKN-HKN-MAX
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-HKNCOMBI-HENSYU-EXT.
      *
           EXIT.
      *
      *****************************************************************
      *    労災保険情報検索
      *****************************************************************
       1100-PTRSIINF-CHK-SEC             SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *    第三者行為
           IF      COMB-KOH1HKNNUM     =   "970"
               MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
               MOVE    ZERO                TO  PTRSI-HKNID
           END-IF
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1100-PTRSIINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       110-HKNCOMBI-HEN-SEC             SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    COMB-HKNCOMBINUM    TO  ORCSHKN-GMN-HKNCOMBINUM
                                                               (IDX)
      *
           MOVE    COMB-HKNID          TO  ORCSHKN-NAI-HKNID   (IDX)
           MOVE    COMB-HKNNUM         TO  ORCSHKN-NAI-HKNNUM  (IDX)
           MOVE    COMB-HOJOKBN        TO  ORCSHKN-NAI-HOJOKBN (IDX)
           MOVE    COMB-HONKZKKBN      TO  ORCSHKN-NAI-HONKZKKBN (IDX)
           MOVE    COMB-HKNNUM         TO  ORCSHKN-NAI-HKNNUM  (IDX)
           MOVE    COMB-KOH1HKNNUM     TO  ORCSHKN-NAI-KOH1HKNNUM
                                                               (IDX)
           MOVE    COMB-KOH2HKNNUM     TO  ORCSHKN-NAI-KOH2HKNNUM 
                                                               (IDX)
           MOVE    COMB-KOH3HKNNUM     TO  ORCSHKN-NAI-KOH3HKNNUM
                                                               (IDX)
           MOVE    COMB-KOH4HKNNUM     TO  ORCSHKN-NAI-KOH4HKNNUM 
                                                               (IDX)
           MOVE    COMB-KOH1ID         TO  ORCSHKN-NAI-KOH1ID  (IDX)
           MOVE    COMB-KOH2ID         TO  ORCSHKN-NAI-KOH2ID  (IDX)
           MOVE    COMB-KOH3ID         TO  ORCSHKN-NAI-KOH3ID  (IDX)
           MOVE    COMB-KOH4ID         TO  ORCSHKN-NAI-KOH4ID  (IDX)
      *
           MOVE    COMB-TEKSTYMD       TO  ORCSHKN-NAI-TEKSTYMD
                                                               (IDX)
           MOVE    COMB-TEKEDYMD       TO  ORCSHKN-NAI-TEKEDYMD
                                                               (IDX)
      *
           MOVE    COMB-HYOJIKBN       TO  ORCSHKN-NAI-HYOJIKBN (IDX)
      *
           MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  ORCSHKN-GMN-TEKSTYMD(IDX)
           MOVE    COMB-TEKEDYMD       TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  ORCSHKN-GMN-TEKEDYMD(IDX)
      *
      *    保険者番号
           IF     (COMB-HKNID          =   ZERO )  OR
                  (COMB-HKNNUM         =   "971"
                                       OR  "973" )
               CONTINUE
           ELSE
                PERFORM 1102-HKNJANUM-SET-SEC
           END-IF
      *
      *    保険名称編集・労災編集
           PERFORM 1101-HKNCOMBIMEI-HEN-SEC
           MOVE    WRK-HKNCOMBI        TO  ORCSHKN-GMN-HKNCOMBIMEI
                                                                (IDX)
      *    特定疾患の受給者番号編集処理
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL       IDK     >   4
               IF     (COMB-KOHHKNNUM (IDK)    =   "051"
                                               OR  "052"
                                               OR  "091" )
                   PERFORM 1103-JKYSNUM-SET-SEC
                   MOVE    4               TO  IDK
               END-IF
           END-PERFORM
      *
      *!!!!
           IF     (COMB-KOH1HKNNUM     =   "970" )
               OR (COMB-KOH1HKNNUM     =   SPACE )
               CONTINUE
           ELSE
      *        地方公費対応負担割合設定
               INITIALIZE                  ORCSFTNRATEAREA
               MOVE    COMB-HOSPNUM        TO  LNK-FTNRATE-HOSPNUM
               MOVE    COMB-PTID           TO  LNK-FTNRATE-PTID
               MOVE    WRK-SRYYMD          TO  LNK-FTNRATE-SRYYMD
               MOVE    COMB-HKNCOMBINUM    TO  LNK-FTNRATE-HKNCOMBINUM
               MOVE    "1"                 TO  LNK-FTNRATE-KBN
               CALL    "ORCSFTNRATE"       USING
                                           ORCSFTNRATEAREA
                                           SPA-AREA
                                           HKNCOMBI-REC
               IF      LNK-FTNRATE-RC      =   ZERO
                   MOVE    LNK-FTNRATE-NYUIN   TO  COMB-NYU-KFTNRATE
                   MOVE    LNK-FTNRATE-GAIRAI  TO  COMB-GAI-KFTNRATE
               END-IF
           END-IF
      *
      *!!!!
      *
      *    負担割合
           IF      SPA-NYUGAIKBN       =   "1"
      *        入院
               MOVE    COMB-NYU-KFTNRATE   TO  ORCSHKN-NAI-FTNRATE
                                                               (IDX)
           ELSE
      *        外来
               MOVE    COMB-GAI-KFTNRATE   TO  ORCSHKN-NAI-FTNRATE
                                                               (IDX)
           END-IF
      *    保険番号マスタ検索
           PERFORM 3001-HKNNUM-CHK-SEC
      *    治験判定
           IF     (COMB-HKNNUM         >=  "900"   AND
                                       <=  "919"   )   AND
                  (HKN-HKNKOHSBTKBN    =   "9"     )
               MOVE    "1"             TO  ORCSHKN-NAI-CHIKENKBN
                                                             (IDX)
           END-IF
      *    ３歳未満の時、２割を上限とする
           IF    ((COMB-TEKEDYMD       >=  "20021001") AND
                  (WRK-SRYYMD          >=  "20021001") AND
                  (WRK-NENREI          <   03        ))
      *    Ｈ２０．４改正
             OR  ((COMB-TEKEDYMD       >=  "20080401") AND
                  (WRK-SRYYMD          >=  "20080401") AND
                  (AGECHK-SYUGAKUKBN   =   1         ))
      *        自費・治験
               IF     (HKN-HKNKOHSBTKBN    =   "8"  OR  "9"    )
      *                特別療養費
                   OR (COMB-HKNNUM         =   "068"  OR  "069")
                   CONTINUE
               ELSE
                   IF      ORCSHKN-NAI-FTNRATE (IDX)   >   20
                       MOVE    20          TO  ORCSHKN-NAI-FTNRATE (IDX)
                   END-IF
               END-IF
           END-IF
      *    労災の時、割合の変更
           IF      COMB-HKNNUM         =   "971"
               IF      SYS-4001-RSIIRYOKBN     =   "1"
                   MOVE    ZERO            TO  ORCSHKN-NAI-FTNRATE
                                                               (IDX)
               ELSE
                   MOVE    100             TO  ORCSHKN-NAI-FTNRATE
                                                               (IDX)
               END-IF
           END-IF
      *
      *    自賠責・公務災害の時、請求区分より割合を決定する
           IF      ORCSHKN-NAI-RSI-HKNKBN(IDX) =   "4"
                                               OR  "5"
               EVALUATE    WRK-JIBAISEKIKBN
                   WHEN    "1"
                       MOVE    100             TO  ORCSHKN-NAI-FTNRATE
                                                               (IDX)
                   WHEN    "2"
                       MOVE    ZERO            TO  ORCSHKN-NAI-FTNRATE
                                                               (IDX)
               END-EVALUATE
           END-IF
      *
           IF      ORCSHKN-NAI-FTNRATE (IDX)   =   ZERO
               MOVE    SPACE               TO  ORCSHKN-GMN-FTNRATEMEI
                                                               (IDX)
           ELSE
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  ORCSHKN-GMN-FTNRATEMEI
                                                               (IDX)
                                                               (1:3)
               MOVE    "%"                 TO  ORCSHKN-GMN-FTNRATEMEI
                                                               (IDX)
                                                               (4:)
           END-IF
      *
           .
       110-HKNCOMBI-HEN-EXT.
      *
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       1101-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
               END-STRING
           END-IF
      *
      *    保険区分
           MOVE    ZERO                TO  ORCSHKN-NAI-HKNKBN(IDX)
      *    公害
           IF      COMB-HKNNUM         =   "975"
      *        保険区分
               MOVE    2                   TO  ORCSHKN-NAI-HKNKBN(IDX)
           END-IF
      *H29.5
      *    自賠責点数区分とシステム管理判定
           MOVE    SPACE               TO  WRK-HKNKBN-MEI
           IF      COMB-HKNNUM         =   "973"
               IF      (PTRSI-JIBAIKBN     =   "1"  )
      *            労災準拠
                   MOVE   "[労]"             TO  WRK-HKNKBN-MEI
               END-IF
               IF      (PTRSI-JIBAIKBN     =   "2"  )
      *            健保準拠
                   MOVE   "[健]"             TO  WRK-HKNKBN-MEI
               END-IF
           END-IF
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        保険区分
               MOVE    1                   TO  ORCSHKN-NAI-HKNKBN(IDX)
      *
      **       MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               END-IF
      *
      *        四肢区分
               MOVE    PTRSI-SISIKBN       TO  ORCSHKN-NAI-RSI-SISIKBN
                                                                 (IDX)
      *        傷病年月日
               IF      PTRSI-HKNKBN        =   "1"  OR  "2"  OR  "4"
                                           OR  "5"
                   MOVE    PTRSI-SHOBYOYMD     TO
                                               ORCSHKN-NAI-RSI-SHOBYOYMD
                                                                  (IDX)
               ELSE
                   MOVE    PTRSI-SINSATUYMD    TO
                                               ORCSHKN-NAI-RSI-SHOBYOYMD
                                                                  (IDX)
               END-IF
      *        労災区分
               MOVE    PTRSI-HKNKBN        TO  ORCSHKN-NAI-RSI-HKNKBN
                                                                  (IDX)
      *        自賠責・公務災害患者請求区分
               IF      PTRSI-HKNKBN        =   "4"
                                           OR  "5"
                   MOVE    PTRSI-JIBAISEIKBN   TO  WRK-JIBAISEKIKBN
               END-IF
           END-IF
      *ver.4.7
      *    第三者行為
           IF      COMB-KOH1HKNNUM     =   "970"
      *        保険区分
               MOVE    1               TO  ORCSHKN-NAI-HKNKBN (IDX)
      *        労災保険区分
               MOVE    PTRSI-HKNKBN    TO  ORCSHKN-NAI-RSI-HKNKBN
                                                               (IDX)
      *        名称編集
               IF      PTRSI-COMMENT   NOT =   SPACE
      *            保険＋公費１のみとなる
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               END-IF
      *        傷病年月日
               MOVE    PTRSI-SHOBYOYMD     TO  ORCSHKN-NAI-RSI-SHOBYOYMD
                                                                  (IDX)
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HKNCOMBI        TO  KANACHK-MAE-INPUT
           MOVE    50                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBI
           .
       1101-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号編集処理
      *****************************************************************
       1102-HKNJANUM-SET-SEC             SECTION.
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTHKNINF        =   ZERO
               MOVE    PTHKN-HKNJANUM      TO  ORCSHKN-NAI-HKNJANUM
                                                               (IDX)
           END-IF
      *
           .
       1102-HKNJANUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定疾患の受給者番号編集処理
      *****************************************************************
       1103-JKYSNUM-SET-SEC             SECTION.
      *    患者公費情報
           INITIALIZE                  PTKOHINF-REC
           MOVE    COMB-HOSPNUM        TO  PTKOH-KOHID
           MOVE    COMB-PTID           TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDK)    TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTKOHINF        =   ZERO
               MOVE    PTKOH-JKYSNUM       TO  ORCSHKN-NAI-JKYSNUM
                                                               (IDX)
           END-IF
      *
           .
       1103-JKYSNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回対象保険編集処理
      *****************************************************************
       200-HKNCOMBI-SELECT-SEC             SECTION.
      *
           IF      SPA-HKNCOMBINUM     =   SPACE
      *        受診履歴より前回の保険組合せを指定する
               PERFORM 2001-JYURRK-CHK-SEC
           ELSE
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
           END-IF
      *
      *    前回の保険組合せが使用できるかのチェック
           MOVE    ZERO                TO  FLG-HKNCOMBICHK
           MOVE    ZERO                TO  ORCSHKN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )  OR
                              (FLG-HKNCOMBICHK =   1   )
               IF      (WRK-SRYYMD     >=  ORCSHKN-NAI-TEKSTYMD(IDX))
                   AND (WRK-SRYYMD     <=  ORCSHKN-NAI-TEKEDYMD(IDX))
                   IF      WRK-HKNCOMBINUM
                                       =   ORCSHKN-GMN-HKNCOMBINUM
                                                                (IDX)
                       MOVE    IDX             TO  ORCSHKN-IDX
                       MOVE    1               TO  FLG-HKNCOMBICHK
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-HKNCOMBICHK     =   ZERO
      *        省略時の保険組合せ決定
               PERFORM 2002-HKNCOMBI-SET-SEC
           END-IF
      *
           .
       200-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ決定処理
      *****************************************************************
       2002-HKNCOMBI-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
           MOVE    ZERO                TO  WRK-IDXH2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCSHKN-HKN-MAX
               MOVE    ZERO                TO  WRK-CNT-HKN2
               IF      (WRK-SRYYMD     >=  ORCSHKN-NAI-TEKSTYMD(IDX))
                   AND (WRK-SRYYMD     <=  ORCSHKN-NAI-TEKEDYMD(IDX))
                   AND (ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       NOT =   "9999"    )
      *
                   IF      ORCSHKN-NAI-HKNNUM (IDX)    NOT =   SPACE
                       ADD     1                   TO  WRK-CNT-HKN2
                   END-IF
                   IF      ORCSHKN-NAI-KOH1HKNNUM(IDX) NOT =   SPACE
                       ADD     1                   TO  WRK-CNT-HKN2
                   END-IF
                   IF      ORCSHKN-NAI-KOH2HKNNUM(IDX) NOT =   SPACE
                       ADD     1                   TO  WRK-CNT-HKN2
                   END-IF
                   IF      ORCSHKN-NAI-KOH3HKNNUM(IDX) NOT =   SPACE
                       ADD     1                   TO  WRK-CNT-HKN2
                   END-IF
                   IF      ORCSHKN-NAI-KOH4HKNNUM(IDX) NOT =   SPACE
                       ADD     1                   TO  WRK-CNT-HKN2
                   END-IF
      *            治験・自費は他の保険がない時に有効とする
                   IF     (ORCSHKN-NAI-HKNNUM (IDX)(1:2) =   "98"
                                                     OR  "90"
                                                     OR  "91" )
                       IF      WRK-IDXH2           =   ZERO
                           MOVE    IDX             TO  WRK-IDXH2
                       END-IF
                   ELSE
                       IF      WRK-CNT-HKN         <=  WRK-CNT-HKN2
                           MOVE    IDX                 TO  WRK-IDXH
                           MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
                       END-IF
                   END-IF
      *            労災・自賠責があれば対象とする
                   IF      ORCSHKN-NAI-HKNNUM (IDX)    =   "971"
                                                       OR  "973"
                       MOVE    IDX                 TO  WRK-IDXH
                       MOVE    ORCSHKN-HKN-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    WRK-IDXH2           TO  WRK-IDXH
           END-IF
           IF      WRK-IDXH            =   ZERO
               MOVE    1               TO  ORCSHKN-IDX
           ELSE
               MOVE    WRK-IDXH        TO  ORCSHKN-IDX
           END-IF
      *
           .
       2002-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴より前回の保険組合せを指定処理
      *****************************************************************
       2001-JYURRK-CHK-SEC         SECTION.
      *
      *    受診履歴より前回の保険組合せを指定する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           PERFORM UNTIL    FLG-JYURRK     =   1
      *        包括保険以外であること
               IF      JYURRK-HKNCOMBINUM  NOT =   "9999"
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   MOVE    1                   TO  FLG-JYURRK
               ELSE
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key11"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報処理
      *****************************************************************
       3001-HKNNUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-SRYYMD          TO  HKN-TEKSTYMD
           MOVE    WRK-SRYYMD          TO  HKN-TEKEDYMD
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理（労災情報）検索処理
      *****************************************************************
       1002-RSI-SYSKANRI-SEC             SECTION.
      *
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1002-RSI-SYSKANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括保険追加編集処理
      *****************************************************************
       400-HOUKATU-HENSYU-SEC             SECTION.
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-1008-STYUKYMD
                                           SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1008-REC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    包括登録
           IF      SYS-1008-HOUKATU-FLG    =   "1"
               CONTINUE
           ELSE
               GO     TO      400-HOUKATU-HENSYU-EXT
           END-IF
      *
           IF      ORCSHKN-HKN-MAX     >   ZERO
               ADD     1                   TO  ORCSHKN-HKN-MAX
               MOVE    "9999"              TO  ORCSHKN-GMN-HKNCOMBINUM
                                                      (ORCSHKN-HKN-MAX)
               MOVE    "包括分入力"        TO  ORCSHKN-GMN-HKNCOMBIMEI
                                                      (ORCSHKN-HKN-MAX)
               MOVE    ZERO                TO  ORCSHKN-NAI-TEKSTYMD
                                                      (ORCSHKN-HKN-MAX)
               MOVE    ALL "9"             TO  ORCSHKN-NAI-TEKEDYMD
                                                      (ORCSHKN-HKN-MAX)
               MOVE    "00.00.00"          TO  ORCSHKN-GMN-TEKSTYMD
                                                      (ORCSHKN-HKN-MAX)
               MOVE    "99.99.99"          TO  ORCSHKN-GMN-TEKEDYMD
                                                      (ORCSHKN-HKN-MAX)
      *        保険区分
               MOVE    ZERO                TO  ORCSHKN-NAI-HKNKBN
                                                      (ORCSHKN-HKN-MAX)
      *
           END-IF
           .
       400-HOUKATU-HENSYU-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
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
       921-HKNNUM-NEXT-EXT.
           EXIT.
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
      *    患者公費読み込み
      *****************************************************************
       910-PTKOHINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
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
      *****************************************************************
