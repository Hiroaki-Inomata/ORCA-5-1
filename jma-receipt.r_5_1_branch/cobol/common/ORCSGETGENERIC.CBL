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
       PROGRAM-ID.             ORCSGETGENERIC.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 院外処方一般名編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/12/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04/08/00    NACL-多々納  16/03/XX  後発品変更不可（処方）対応他
      *  04.08.00    NACL-多々納  16/03/XX  最低薬価テーブル対応
      *  04.08.00    NACL-多々納  17/03/XX  後発品変更可（処方毎）対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-GENERICNAME     PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-GENERICPRICE    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-SRY             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXB                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
           03  WRK-SRYKBN              PIC X(02).
      *
           03  WRK-IPN-AREA.
               05  WRK-YAKKAKJNCD      PIC X(12).
               05  WRK-GENERIC-KBN     PIC X(01).
               05  WRK-GENERICNAME     PIC X(200).
               05  WRK-NAME-KBN        PIC X(01).
               05  WRK-GEN-TANICD      PIC X(03).
               05  WRK-GEN-TANINAME    PIC X(24).
      *
               05  WRK-IPN-KISAIKBN    PIC 9(01).
               05  FLG-IPNMEI          PIC 9(01).
               05  FLG-FUKACHK         PIC 9(01).
               05  FLG-FUKACHK2        PIC 9(01).
      *
           03  WRK-GENERIC-CODE        PIC X(12).
           03  WRK-GENERIC-STYMD       PIC X(08).
           03  WRK-GENERIC-EDYMD       PIC X(08).
           03  WRK-YAKKAKJNCD-OLD      PIC X(12).
      *
      *
      *
           03  WRK-SRYKA               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  TBL-GENEINP-AREA.
      *    予約コードの剤
           03  TBL-GENINP-TBL1         OCCURS   40.
               05  TBL-G-DENPNUM        PIC 9(07).
               05  TBL-G-SRYYMD         PIC X(08).
      *    予約コード算定日
           03  TBL-GENINP2-TBL2.
               05  TBL-GENINP2-OCCURS   OCCURS   31.
                   07  TBL-H-ALLFUKA       PIC 9(01).
      *
           03  WRK-YMD.
               05  WRK-YM              PIC X(06).
               05  WRK-DD              PIC 9(02).
           03  WRK-SRYCD               PIC X(09).
           03  FLG-ALLFUKA             PIC 9(01).
           03  FLG-ALLFUKA-ARI         PIC 9(01).
           03  FLG-GENENASI            PIC 9(01).
           03  FLG-GENE-ARI            PIC 9(01).
           03  IDX-G                   PIC 9(04).
           03  IDX-G2                  PIC 9(04).
           03  IDX-JYU                 PIC 9(04).
           03  IDD                     PIC 9(02).
           03  FLG-KOUHATU             PIC 9(01).
      *
      *    剤内容テーブル
       01  WRK-SRYACT-GRP-AREA.
           03  WRK-SRYACT-TBL          OCCURS   50.
      *診療コード
               05  WRK-SRY-SRYCD           PIC  X(09).
      *数量
               05  WRK-SRY-SRYSURYO        PIC  9(05)V9(05).
      *回数
               05  WRK-SRY-SRYKAISU        PIC  9(03).
      *明細請求区分
               05  WRK-SRY-MEISKYFLG       PIC  X(01).
      *自動算定区分
               05  WRK-SRY-AUTOKBN         PIC  X(01).
      *名称入力番号
               05  WRK-SRY-INPUTNUM        PIC  9(03).
      *自費金額
               05  WRK-SRY-JIHIMONEY       PIC  9(07).
      *名称
               05  WRK-SRY-COM-NAME        PIC X(200).
      *
           03  WRK-SRY-MAX                 PIC 9(03).
      *
      *
      *    一般名例外コードテーブル
           COPY    "CMGENERICTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *    COPY    "CPSYSKANRI.INC".
      *    処方箋ＧＥ表示
      *    COPY    "CPSK1030.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスタワーク
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS   10.
           COPY    "CPSRYACT.INC"     REPLACING
                                      //SRY-// BY //TBL-SRY-//.
      *H28.4
      *    最低薬価マスタ
       01  GENERICPRICE-REC.
           COPY    "CPGENERICPRICE.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *
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
      *    院外処方一般名編集パラメタ
           COPY    "CPORCSGETGENERIC.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSGETGENERICAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSGETGE-OUT-AREA
      *
           PERFORM 100-INIT-SEC
      *
           IF      ORCSGETGE-ERRCD  =   ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　　処理
      *****************************************************************
       100-INIT-SEC                      SECTION.
      *
           MOVE    ORCSGETGE-SRYYM     TO  WRK-SRYYM
           MOVE    "01"                TO  WRK-SRYDD
      *
           IF      ORCSGETGE-PTID      =   ZERO
               MOVE    1               TO  ORCSGETGE-ERRCD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-SRY
      *    診療行為検索
           INITIALIZE                  SRYACT-REC
           MOVE    ORCSGETGE-HOSPNUM   TO  SRY-HOSPNUM
           MOVE    ORCSGETGE-NYUGAIKBN TO  SRY-NYUGAIKBN
           MOVE    ORCSGETGE-PTID      TO  SRY-PTID
           MOVE    ORCSGETGE-SRYYM     TO  SRY-SRYYM
           MOVE    ORCSGETGE-ZAINUM    TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    SRY-SRYKBN      TO  WRK-SRYKBN
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        在宅・投薬
               IF     (SRY-SRYKBN          =   "14"
                                           OR  "21"
                                           OR  "22"
                                           OR  "23")
      *            院内対象外
                   IF     (SRY-SRYSYUKBN       =   "211"
                                               OR  "213"
                                               OR  "221"
                                               OR  "223"
                                               OR  "231"
                                               OR  "233"
                                               OR  "291"
                                               OR  "294"
                                               OR  "297")
                       CONTINUE
                   ELSE
      *
                   ADD     1               TO  IDX-SRY
                   MOVE    SRYACT-REC      TO  TBL-SRYACT-REC (IDX-SRY)
                   MOVE    SRY-SRYKBN      TO  WRK-SRYKBN
                   MOVE    SRY-SRYKA       TO  WRK-SRYKA
               END-IF
      *
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H28.2
           IF      IDX-SRY             >   ZERO
      *        後発品変更不可編集
               PERFORM 2009-SYORI-HENSYU-SEC
           END-IF
      *
           IF      IDX-SRY             >   ZERO
               PERFORM 2001-SRYACT-CHK-SEC
           ELSE
               MOVE    9               TO  ORCSGETGE-ERRCD
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎の対象有無チェック処理
      *****************************************************************
       2001-SRYACT-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-SRYACT-GRP-AREA
           MOVE    ZERO                TO  WRK-SRY-MAX
           PERFORM VARYING     IDXB    FROM    1   BY  1
                   UNTIL      (IDXB    >   IDX-SRY   )
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   5   )
                              OR  (TBL-SRY-SRYCD (IDXB IDXA)
                                           =   SPACE)
                   ADD     1                       TO  WRK-SRY-MAX
                   MOVE    TBL-SRY-SRYCD (IDXB IDXA)
                                                   TO  WRK-SRY-SRYCD
                                                           (WRK-SRY-MAX)
                   MOVE    TBL-SRY-SRYSURYO (IDXB IDXA)
                                                   TO  WRK-SRY-SRYSURYO
                                                           (WRK-SRY-MAX)
                   MOVE    TBL-SRY-SRYKAISU (IDXB IDXA)
                                                   TO  WRK-SRY-SRYKAISU
                                                           (WRK-SRY-MAX)
                   MOVE    TBL-SRY-MEISKYFLG (IDXB IDXA)
                                                   TO  WRK-SRY-MEISKYFLG
                                                           (WRK-SRY-MAX)
                   MOVE    TBL-SRY-AUTOKBN (IDXB IDXA)
                                                   TO  WRK-SRY-AUTOKBN
                                                           (WRK-SRY-MAX)
                   MOVE    TBL-SRY-INPUTNUM (IDXB IDXA)
                                                   TO  WRK-SRY-INPUTNUM
                                                           (WRK-SRY-MAX)
                   MOVE    TBL-SRY-JIHIMONEY (IDXB IDXA)
                                                   TO  WRK-SRY-JIHIMONEY
                                                           (WRK-SRY-MAX)
               END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   WRK-SRY-MAX      )   OR
                              (WRK-SRY-SRYCD (IDXA)    =   SPACE)
               INITIALIZE                  WRK-IPN-AREA
               IF      WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"
                   COMPUTE IDX-STR             =   IDXA    +   1
                   PERFORM VARYING     IDXB    FROM    IDX-STR BY  1
                           UNTIL      (IDXB    >   WRK-SRY-MAX )   OR
                                      (WRK-SRY-SRYCD (IDXB) =   SPACE)
                       IF      WRK-SRY-SRYCD (IDXB)(1:1)   =   "6"
                           MOVE    WRK-SRY-MAX     TO  IDXB
                       ELSE
      *                    予約コードチェック処理
                           PERFORM 20011-INP-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-IF
      *        明細編集処理
               PERFORM 20012-ORCSGETGE-HEN-SEC
           END-PERFORM
           MOVE    IDX                 TO  ORCSGETGE-TBLMAX 
      *
      *    剤に後発品変更不可（処方）あり
           IF     (FLG-ALLFUKA-ARI     =   0      )
             AND  (FLG-GENE-ARI        =   1      )
               MOVE    "1"             TO  ORCSGETGE-GENEFLG
           END-IF
      *H29.3
      *    剤に後発品変更可（処方）あり
           IF     (FLG-ALLFUKA-ARI     =   0      )
             AND  (FLG-GENE-ARI        =   2      )
               MOVE    "2"             TO  ORCSGETGE-GENEFLG
           END-IF
      *
           .
       20021-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約コードチェック処理
      *****************************************************************
       20011-INP-HENSYU-SEC      SECTION.
      *
           EVALUATE    WRK-SRY-SRYCD (IDXB)
               WHEN    "099209903"
      *            後発医薬品へ変更不可
                   MOVE    1                   TO  FLG-FUKACHK
               WHEN    "099209905"
      *            含量規格変更不可
                   MOVE    1                   TO  FLG-FUKACHK2
               WHEN    "099209906"
      *            剤型変更不可
                   MOVE    1                   TO  FLG-FUKACHK2
               WHEN    "099209907"
      *            銘柄名記載
                   MOVE    1                   TO  FLG-IPNMEI
               WHEN    "099209908"
      *            一般名記載
                   MOVE    2                   TO  FLG-IPNMEI
           END-EVALUATE
      *
           .
       20011-INP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     明細編集処理
      *****************************************************************
       20012-ORCSGETGE-HEN-SEC      SECTION.
      *
           IF      WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"
      *        薬剤チェック
               PERFORM 200121-GENERIC-CHK-SEC
      *        薬剤明細編集
               PERFORM 200122-ORCSGETGE-HEN-SEC
           ELSE
               ADD     1                   TO  IDX
      *        診療コード
               MOVE    WRK-SRY-SRYCD (IDXA)
                                           TO  ORCSGETGE-SRYCD(IDX)
           END-IF
           .
       20012-ORCSGETGE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤明細編集処理
      *****************************************************************
       200122-ORCSGETGE-HEN-SEC     SECTION.
      *
           ADD     1                   TO  IDX
      *    診療コード
           MOVE    WRK-SRY-SRYCD (IDXA)    TO  ORCSGETGE-SRYCD(IDX)
      *    一般名コード（薬価基準コード）
           MOVE    WRK-YAKKAKJNCD      TO  ORCSGETGE-YAKKAKJNCD
                                                                (IDX)
      *    一般名区分（１：一般名加算）
           MOVE    WRK-GENERIC-KBN     TO  ORCSGETGE-GENERIC-KBN
                                                                (IDX)
      *    一般名称
           MOVE    WRK-GENERICNAME     TO  ORCSGETGE-GENERICNAME
                                                                (IDX)
      *    一般名単位コード
           MOVE    WRK-GEN-TANICD      TO  ORCSGETGE-GEN-TANICD
                                                                (IDX)
           MOVE    WRK-GEN-TANINAME    TO  ORCSGETGE-GEN-TANINAME
                                                                (IDX)
      *    一般名名称区分（1:一般名,2：処方名称）
           MOVE    WRK-NAME-KBN        TO  ORCSGETGE-NAME-KBN(IDX)
      *
      *    換算値
           MOVE    TNSP-IPN-KANZANCHI  TO  ORCSGETGE-KANZANCHI
                                                                (IDX)
      *    換算値の単位
           MOVE    TNSP-IPN-KANZANTANICD
                                       TO  ORCSGETGE-KNS-TANICD
                                                                (IDX)
           MOVE    TNSP-IPN-KANZANTANINAME
                                       TO  ORCSGETGE-KNS-TANINAME
                                                                (IDX)
           .
       200122-ORCSGETGE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤明細編集処理
      *****************************************************************
       200121-GENERIC-CHK-SEC      SECTION.
      *
           MOVE    WRK-SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    点数附加情報検索
           IF     (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               INITIALIZE                      WRK-IPN-AREA
               GO      TO      200121-GENERIC-CHK-EXT
           END-IF
      *
      *一般名称記載
           MOVE    TNSP-IPN-KISAIKBN   TO  WRK-IPN-KISAIKBN
      *
      *    システム管理の変更可によって一般名記載とする
           IF     (WRK-IPN-KISAIKBN        =   ZERO    )
             AND ((ORCSGETGE-KOUHATUKA     =   "1"   ) OR
      *H29.3
      *   後発品変更可（処方）あり
                  (FLG-GENE-ARI            =   2     ))
             AND  (TNS-KOUHATUKBN          =   1
                                           OR  2
                                           OR  3
                                           OR  4
                                           OR  7       )
      *
      *H28.2
      *        後発品変更不可（処方）あり
               IF     (FLG-ALLFUKA-ARI     =   0      )
                 AND  (FLG-GENE-ARI        =   1      )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  WRK-IPN-KISAIKBN
               END-IF
           END-IF
      *
           IF      WRK-IPN-KISAIKBN    =   1
                                       OR  3
      *        一般名記載ありをなしへ
               IF     (FLG-IPNMEI          =   1    )
                 OR   (FLG-FUKACHK         =   1    )
      *            銘柄名記載・後発品変更不可の入力あり
                   MOVE    ZERO            TO  WRK-IPN-KISAIKBN
               END-IF
           ELSE
      *        一般名記載なしをありへ
               IF      FLG-IPNMEI          =   2
      *            一般名記載の入力あり
                   MOVE    1               TO  WRK-IPN-KISAIKBN
               END-IF
           END-IF
      *
      *    一般名記載あり
           IF      WRK-IPN-KISAIKBN    =   1
               PERFORM 9102-GENERICNAME-READ-SEC
      *        一般名テーブルなしは記載なし
               IF      FLG-GENERICNAME     =   1
                   MOVE    ZERO            TO  WRK-IPN-KISAIKBN
               END-IF
           END-IF
      *
           EVALUATE    WRK-IPN-KISAIKBN
      *        一般名記載
               WHEN    1
                   MOVE    SPACE               TO  WRK-GENERICNAME
                   STRING  "【般】"                DELIMITED  BY  SIZE
                           GENERIC-GENERICNAME     DELIMITED  BY  SPACE
                                               INTO    WRK-GENERICNAME
                   END-STRING
                   MOVE    GENERIC-TANICD      TO  WRK-GEN-TANICD
                   MOVE    GENERIC-TANINAME    TO  WRK-GEN-TANINAME
                   MOVE    TNS-YAKKAKJNCD      TO  WRK-YAKKAKJNCD
                   MOVE    "1"                 TO  WRK-NAME-KBN
      *        処方名称を一般名称とする
               WHEN    3
                   MOVE    SPACE               TO  WRK-GENERICNAME
                   STRING  "【般】"                DELIMITED  BY  SIZE
                           TNSP-SHONAME            DELIMITED  BY  SPACE
                                               INTO    WRK-GENERICNAME
                   END-STRING
                   MOVE    TNS-TANICD          TO  WRK-GEN-TANICD
                   MOVE    TNS-TANINAME        TO  WRK-GEN-TANINAME
                   MOVE    TNS-YAKKAKJNCD      TO  WRK-YAKKAKJNCD
                   MOVE    "2"                 TO  WRK-NAME-KBN
           END-EVALUATE
      *
      *    一般名加算設定
           MOVE    SPACE               TO  WRK-GENERIC-KBN
           IF      WRK-SRYKBN          =   "21"
                                       OR  "22"
                                       OR  "23"
               PERFORM 200122-IPNKSN-HEN-SEC
           END-IF
      *
      *    一般名例外コード変換処理
           IF     (WRK-YAKKAKJNCD  NOT =   SPACE )
              AND (WRK-GENERIC-KBN NOT =   SPACE )
      *H28.4
               IF      WRK-SRYYMD      <   "20160401"
                   PERFORM 200123-GENERICTBL-HEN-SEC
               ELSE
                   PERFORM 200123-GENERICCD-HEN-SEC
               END-IF
           END-IF
           .
       200121-GENERIC-CHK-EXT.
           EXIT.
      *****************************************************************
      *     一般名加算編集処理
      *****************************************************************
       200122-IPNKSN-HEN-SEC           SECTION.
      *
      *    一般名記載で先発品のある後発、後発品のある先発
           IF     (WRK-IPN-KISAIKBN    =   1
                                       OR  3  )
             AND  (TNS-KOUHATUKBN      =   1
                                       OR  2
                                       OR  3
                                       OR  4  )
               MOVE    "1"             TO  WRK-GENERIC-KBN
           END-IF
           .
       200122-IPNKSN-HEN-EXT.
           EXIT.
      *H28.4
      *****************************************************************
      *    一般名例外コード変換処理
      *****************************************************************
       200123-GENERICCD-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-GENERIC-CODE
      *
           MOVE    SPACE               TO  GENERICPRICE-REC
           INITIALIZE                      GENERICPRICE-REC
           MOVE    WRK-YAKKAKJNCD      TO  GENERICPRICE-YAKKAKJNCD
           MOVE    WRK-SRYYMD          TO  GENERICPRICE-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  GENERICPRICE-YUKOEDYMD
           MOVE    GENERICPRICE-REC    TO  MCPDATA-REC
           MOVE    "tbl_generic_price" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_generic_price" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICPRICE-REC
      *            一般名コード
                   MOVE    GENERICPRICE-GECODE TO  WRK-GENERIC-CODE
               ELSE
                   INITIALIZE                  GENERICPRICE-REC
                   MOVE    1                   TO  FLG-GENERICPRICE
               END-IF
           ELSE
               INITIALIZE                  GENERICPRICE-REC
               MOVE    1                   TO  FLG-GENERICPRICE
           END-IF
           MOVE    "tbl_generic_price" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-GENERIC-CODE      =   SPACE
               MOVE    WRK-YAKKAKJNCD        TO  WRK-YAKKAKJNCD-OLD
               MOVE    SPACE                 TO  WRK-YAKKAKJNCD
               STRING  WRK-YAKKAKJNCD-OLD(1:9)
                       "ZZZ"               DELIMITED  BY  SIZE
                                           INTO    WRK-YAKKAKJNCD
               END-STRING
           ELSE
               MOVE    WRK-GENERIC-CODE    TO  WRK-YAKKAKJNCD
           END-IF
           .
       200123-GENERICCD-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    一般名例外コード変換処理
      *****************************************************************
       200123-GENERICTBL-HEN-SEC         SECTION.
      *
           SET     TBL-GEN             TO  1
           SEARCH      TBL-GENERIC-OCC     VARYING   TBL-GEN
                   AT  END
                       MOVE    SPACE           TO  WRK-GENERIC-CODE
                       MOVE    SPACE           TO  WRK-GENERIC-STYMD
                       MOVE    SPACE           TO  WRK-GENERIC-EDYMD
                   WHEN    WRK-YAKKAKJNCD      =   TBL-YAKKAKJNCD
                                                           (TBL-GEN)
                       MOVE    TBL-GENERIC-CODE (TBL-GEN)
                                                TO  WRK-GENERIC-CODE
                       MOVE    TBL-GENERIC-STYMD (TBL-GEN)
                                                TO  WRK-GENERIC-STYMD
                       MOVE    TBL-GENERIC-EDYMD (TBL-GEN)
                                                TO  WRK-GENERIC-EDYMD
           END-SEARCH
      *
           IF      WRK-GENERIC-CODE    NOT =   SPACE
      *        有効期間判定
               IF    (WRK-SRYYMD           >=  WRK-GENERIC-STYMD)
                 AND (WRK-SRYYMD           <=  WRK-GENERIC-EDYMD)
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-GENERIC-CODE
               END-IF
           END-IF
      *
           IF      WRK-GENERIC-CODE      =   SPACE
               MOVE    WRK-YAKKAKJNCD        TO  WRK-YAKKAKJNCD-OLD
               MOVE    SPACE                 TO  WRK-YAKKAKJNCD
               STRING  WRK-YAKKAKJNCD-OLD(1:9)
                       "ZZZ"               DELIMITED  BY  SIZE
                                           INTO    WRK-YAKKAKJNCD
               END-STRING
           ELSE
               MOVE    WRK-GENERIC-CODE    TO  WRK-YAKKAKJNCD
           END-IF
           .
       20039-GENERICTBL-HEN-EXT.
           EXIT.
      *
      *H28.2
      *****************************************************************
      *    一般名記載編集
      *****************************************************************
       2009-SYORI-HENSYU-SEC         SECTION.
      *
      *    予約コードの剤番号取得
           INITIALIZE                      TBL-GENEINP-AREA
           MOVE    ZERO                TO  IDX-G
      *H29.3
      *    後発品変更可（処方単位）
           MOVE    "099209911"         TO  WRK-SRYCD
           MOVE    2                   TO  FLG-ALLFUKA
           PERFORM 20091-GENEINP-HEN-SEC
      *
           IF      IDX-G               =   ZERO
      *!!
      *!!!!後発品変更可（一般名記載）を優先。
      *
      *    後発品変更不可（処方単位）
           MOVE    "099209910"         TO  WRK-SRYCD
           MOVE    1                   TO  FLG-ALLFUKA
           PERFORM 20091-GENEINP-HEN-SEC
      *
           END-IF
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    ORCSGETGE-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    ORCSGETGE-PTID      TO  JYURRK-PTID
           MOVE    ORCSGETGE-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )
               MOVE    ORCSGETGE-ZAINUM    TO  JYURRK-ZAINUM(IDX-JYU)
           END-PERFORM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key63"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key63"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           MOVE    ZERO                TO  FLG-ALLFUKA-ARI
           MOVE    ZERO                TO  FLG-GENE-ARI
      *
           PERFORM UNTIL   (FLG-JYURRK     =   1 )
               MOVE    ZERO                 TO  FLG-GENENASI
               PERFORM VARYING     IDX-G   FROM    1   BY  1
                       UNTIL      (IDX-G   >   40 )
                              OR  (TBL-G-DENPNUM(IDX-G) =   ZERO)
                   IF      JYURRK-DENPNUM      =   TBL-G-DENPNUM(IDX-G)
                       MOVE    1               TO  FLG-GENENASI
                       MOVE    40              TO  IDX-G
                   END-IF
               END-PERFORM
               IF      FLG-GENENASI        =   ZERO
      *            後発品変更不可（処方）なしの受診履歴なし
                   IF      FLG-ALLFUKA     =   1
                       MOVE    1               TO  FLG-ALLFUKA-ARI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-GENE-ARI
                   IF      FLG-ALLFUKA     =   2
                       MOVE    2           TO  FLG-GENE-ARI
                   END-IF
               END-IF
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key63"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key63"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2009-SYORI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    予約コードの剤番号取得編集
      *****************************************************************
       20091-GENEINP-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    ORCSGETGE-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    ORCSGETGE-PTID      TO  JYURRK-PTID
           MOVE    ORCSGETGE-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           MOVE    ORCSGETGE-SRYYM     TO  JYURRK-UPHMS
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           IF      FLG-ALLFUKA         =   1
               MOVE    "key62"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key68"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
           PERFORM
                   UNTIL   FLG-JYURRK      =   1
               IF      JYURRK-HKNCOMBINUM  NOT =   "9999"
                   ADD     1                   TO  IDX-G
                   IF      IDX-G               <   40
                       MOVE    JYURRK-DENPNUM  TO  TBL-G-DENPNUM(IDX-G)
                       MOVE    JYURRK-SRYYMD   TO  TBL-G-SRYYMD (IDX-G)
                   END-IF
               END-IF
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
      *
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20091-GENEINP-HEN-EXT.
           EXIT.
      *H28.2
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
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    一般名テーブル検索処理
      *****************************************************************
       9102-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
           MOVE    TNS-YAKKAKJNCD      TO  GENERIC-YAKKAKJNCD
      **** MOVE    SPA-HOSPNUM         TO  GENERIC-HOSPNUM
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
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
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               INITIALIZE                      JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読み込み
      *****************************************************************
      *930-SRYACCT-READ-SEC         SECTION.
      *
      *    PERFORM 920-DBFETCH-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    MCPDATA-REC         TO  SRYACCT-REC
      *        MOVE    ZERO                TO  FLG-SRYACCT
      *    ELSE
      *        INITIALIZE                      SRYACCT-REC
      *        MOVE    1                   TO  FLG-SRYACCT
      *    END-IF
      *
      *    .
      *930-SRYACCT-READ-EXT.
      *    EXIT.
      *
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
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
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
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
