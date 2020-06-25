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
       PROGRAM-ID.             ORCGH02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 請求管理−返戻情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/22    NaCl−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *  04.06.02    NACL-藤原    10/12/20  保険者からの返戻対応
      *  04.06.03    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.04    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加
      *
      *  04.07.01    NACL-藤原    13/10/07  労災レセ電返戻対応
      *
      *  04.08.01    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.02    NACL-藤原    18/03/28  平成３０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求管理
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *    設定済返戻データ
           02  SPA-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //SPA-HENREI-B-//.
      *
      *    対象一覧用返戻データ　
           02  TBL-HENREI-B-REC    OCCURS  30.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //TBL-HENREI-B-//.
           02  TBL-HENREI-B-ERR    PIC 9(01)   OCCURS  30.
           02  TBL-HENREI-B-MAX    PIC 9(02).
      *
      *    更新対象返戻データ
           02  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-Z9              PIC Z9.
           03  WRK-SELNO           PIC 9(02).
      *
           03  WRK-H02-ROW         PIC S9(09).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
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
        01  WRK-CONS-HENREI-B-MAX  PIC 9(02)   VALUE   30. 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "H01.INC".
           COPY    "H02.INC".
           COPY    "H03.INC".
           COPY    "H99.INC".
           COPY    "HERR.INC".
           COPY    "HID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "HERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-HIDCD       NOT =   SPACE
                   PERFORM 520-HIDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "H02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "HID1"
                   PERFORM 310-HID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 320-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面処理
      *****************************************************************
       310-HID1-SET-SEC            SECTION.
      *
           IF      SPA-HID1-FLG    =   "OK"
               EVALUATE    SPA-HIDCD
                   WHEN    "1003"
                      MOVE    99               TO  SPA-H02GMN-CUR
                   WHEN    OTHER
                       MOVE    SPA-H02NAI-TINDEX (SPA-H02NAI-SELNUM)
                                               TO  IDX
                       EVALUATE    TBL-HENREI-B-ERR (IDX)
                           WHEN    ZERO
                               MOVE    99          TO  SPA-H02GMN-CUR
                           WHEN    1
      *                        既に返戻設定済
      *****                    MOVE    "0002"      TO  SPA-ERRCD
                              MOVE    99           TO  SPA-H02GMN-CUR
                           WHEN    2
      *                        再再請求
                               MOVE    SPACE       TO  SPA-HID1-FLG
                               MOVE    "1003"      TO  SPA-HIDCD
                               GO  TO  310-HID1-SET-EXT
                       END-EVALUATE
               END-EVALUATE
           ELSE
              MOVE    SPA-H02GMN-SELNUM-OLD
                                       TO  SPA-H02GMN-SELNUM
              MOVE    SPA-H02NAI-SELNUM-OLD
                                       TO  SPA-H02NAI-SELNUM
              INITIALIZE                   WRK-HENREI-B-REC
              MOVE    1                TO  SPA-H02GMN-CUR
           END-IF
      *
           MOVE    SPACE           TO  SPA-HID1-FLG
           MOVE    SPACE           TO  SPA-HIDCD
                 
           .
       310-HID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       320-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-H02-AREA
           MOVE    ZERO                TO  IDY
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       TBL-HENREI-B-MAX
                   OR      IDY     >=      WRK-CONS-HENREI-B-MAX
               ADD     1           TO  IDY
               MOVE    IDZZ        TO  SPA-H02NAI-TINDEX     (IDY)
      *
               MOVE    IDY         TO  SPA-H02GMN-TRENNUM    (IDY)
               MOVE    TBL-HENREI-B-SYORIYM (IDZZ)
                                   TO  SPA-H02NAI-TSYORIYM(IDY)
                                       WRK-SYMD  (1:6)
               MOVE    "01"        TO  WRK-SYMD  (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
      *
               IF    ( WRK-HENREI-B-HOSPNUM
                                   =   TBL-HENREI-B-HOSPNUM     (IDZZ) )
               AND   ( WRK-HENREI-B-SYORIYM
                                   =   TBL-HENREI-B-SYORIYM     (IDZZ) )
               AND   ( WRK-HENREI-B-TEISYUTUSAKI
                                   =   TBL-HENREI-B-TEISYUTUSAKI(IDZZ) )
               AND   ( WRK-HENREI-B-TEISYUTUSAKI2
                                   =   TBL-HENREI-B-TEISYUTUSAKI2(IDZZ))
               AND   ( WRK-HENREI-B-RECEIPT-NO
                                   =   TBL-HENREI-B-RECEIPT-NO  (IDZZ) )
                   MOVE    "*"             TO
                                       SPA-H02GMN-TSYORIYM(IDY) (1:1)
                   MOVE    WRK-HENYMDG(1:6) 
                                           TO
                                       SPA-H02GMN-TSYORIYM(IDY) (2:)
               ELSE
                   MOVE    WRK-HENYMDG(1:6) 
                                           TO  SPA-H02GMN-TSYORIYM(IDY)
               END-IF
      *
               MOVE    TBL-HENREI-B-TEISYUTUSAKI2 (IDZZ)
                                   TO  SPA-H02NAI-TTEISYUTUSAKI2
                                                          (IDY)
               EVALUATE    SPA-H02NAI-TTEISYUTUSAKI2 (IDY)
                   WHEN    "0"
                       MOVE    "　"    TO  SPA-H02GMN-TTEISYUTUSAKI2
                                                              (IDY)
                   WHEN    "1"
                       MOVE    "保"    TO  SPA-H02GMN-TTEISYUTUSAKI2
                                                              (IDY)
               END-EVALUATE
      *
               MOVE    TBL-HENREI-B-NYUGAIKBN (IDZZ)
                                   TO  SPA-H02NAI-TNYUGAIKBN
                                                          (IDY)
               EVALUATE    SPA-H02NAI-TNYUGAIKBN    (IDY)
                   WHEN    "1"
                       MOVE    "入"    TO  SPA-H02GMN-TNYUGAIKBN  (IDY)
                   WHEN    "2"
                       MOVE    "外"    TO  SPA-H02GMN-TNYUGAIKBN  (IDY)
               END-EVALUATE
      *
               EVALUATE    TBL-HENREI-B-TEISYUTUSAKI (IDZZ)
                   WHEN    "3"
                       EVALUATE    TBL-HENREI-B-RECESYUBETU(IDZZ)
                           WHEN    "7101"
                           WHEN    "7091"
                               MOVE    "傷病・業務"
                                   TO  SPA-H02GMN-TRECESYUBETU    (IDY)
                           WHEN    "7102"
                           WHEN    "7092"
                               MOVE    "傷病・通勤"
                                   TO  SPA-H02GMN-TRECESYUBETU    (IDY)
                           WHEN    "7071"
                           WHEN    "7081"
                               MOVE    "短期・業務"
                                   TO  SPA-H02GMN-TRECESYUBETU    (IDY)
                           WHEN    "7072"
                           WHEN    "7082"
                               MOVE    "短期・通勤"
                                   TO  SPA-H02GMN-TRECESYUBETU    (IDY)
                           WHEN    OTHER
                               MOVE    TBL-HENREI-B-RECESYUBETU(IDZZ)
                                   TO  SPA-H02GMN-TRECESYUBETU    (IDY)
                       END-EVALUATE
                   WHEN    OTHER
                       MOVE    TBL-HENREI-B-RECESYUBETU(IDZZ)
                                   TO  SPA-H02GMN-TRECESYUBETU    (IDY)
               END-EVALUATE
      * 
               MOVE    TBL-HENREI-B-HKNJANUM   (IDZZ)
                                   TO  SPA-H02GMN-THKNJANUM       (IDY)
      *
      *        公費単独で生保又は中国残留邦人等のとき負担者番号、
      *        受給者番号をセット
               IF      TBL-HENREI-B-RECESYUBETU(IDZZ)
                                       =   "1211" OR "1212"
                   IF    ( TBL-HENREI-B-FTNNUM (IDZZ 1) (1:2)
                                           =   "12" OR  "25" OR  "20"
                                           OR  "53"   )
                       STRING  TBL-HENREI-B-FTNNUM (IDZZ 1)
                                               DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               TBL-HENREI-B-JKYNUM  (IDZZ 1)
                                               DELIMITED   BY  SPACE
                                       INTO    SPA-H02GMN-TKIGO   (IDY)
                       END-STRING
                   END-IF
               ELSE
                   STRING  TBL-HENREI-B-KIGO (IDZZ)
                                               DELIMITED   BY  SPACE
                           "　"            DELIMITED   BY  SIZE
                           TBL-HENREI-B-NUM  (IDZZ)
                                           DELIMITED   BY  SPACE
                                           INTO    SPA-H02GMN-TKIGO(IDY)
                   END-STRING
               END-IF
               EVALUATE    TBL-HENREI-B-RECEIPT-KBN (IDZZ)(1:1)
                   WHEN    "1"
                       MOVE    "老併"      TO  SPA-H02GMN-TRECEIPT-KBN
                                                                  (IDY)
                   WHEN    "2"
                       MOVE    "老健"      TO  SPA-H02GMN-TRECEIPT-KBN
                                                                  (IDY)
                   WHEN    "3"
                       MOVE    "医併"      TO  SPA-H02GMN-TRECEIPT-KBN
                                                                  (IDY)
                   WHEN    "4"
                       MOVE    "医療"      TO  SPA-H02GMN-TRECEIPT-KBN
                                                                  (IDY)
               END-EVALUATE
               EVALUATE    TBL-HENREI-B-DISASTER (IDZZ)
                   WHEN    "00"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "災"            TO  SPA-H02GMN-TDISASTER
                                                                  (IDY)
               END-EVALUATE
               MOVE    TBL-HENREI-B-HR-MSG (IDZZ)
                                       TO  SPA-H02GMN-TERR        (IDY)
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      IDY     >=      WRK-CONS-HENREI-B-MAX
                       OR      TBL-HENREI-B-S-TEISYUTUSAKI (IDZZ IDX)
                                       =   SPACE
      *??          IF      IDY         >=  WRK-CONS-HENREI-B-MAX
      *??              MOVE    "????"          TO  SPA-ERRCD
      *??              GO  TO  320-SPASET-EXT
      *??          END-IF
      *
                   IF      IDX         >   1
                       ADD     1           TO  IDY
                       MOVE    IDY         TO  SPA-H02GMN-TRENNUM(IDY)
                       MOVE    IDZZ        TO  SPA-H02NAI-TINDEX (IDY)
                   END-IF
      *
                   MOVE    TBL-HENREI-B-S-MAKE-FLG         (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-MAKE-FLG    (IDY)
                   IF      SPA-H02NAI-TS-MAKE-FLG    (IDY)
                                   =   1
                       MOVE    "○"    TO  SPA-H02GMN-TS-MAKE-FLG(IDY)
                   END-IF
      *
                   MOVE    TBL-HENREI-B-S-HENREI-SIJI-FLG  (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-HENREI-SIJI-FLG
                                                                 (IDY)
                   IF      SPA-H02NAI-TS-HENREI-SIJI-FLG (IDY)
                                   =   1
                       MOVE    "○"    TO  SPA-H02GMN-TS-HENREI-SIJI-FLG
                                                                 (IDY)
                   END-IF
      *
                   MOVE    TBL-HENREI-B-S-HENREIYM (IDZZ IDX)
                                       TO  WRK-SYMD  (1:6)
                   MOVE    "01"        TO  WRK-SYMD  (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6) 
                                       TO  SPA-H02GMN-TS-SKYYM (IDY)
                   MOVE    TBL-HENREI-B-S-TEISYUTUSAKI     (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-TEISYUTUSAKI  (IDY)
                   MOVE    TBL-HENREI-B-S-NYUGAIKBN        (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-NYUGAIKBN    (IDY)
                   EVALUATE    SPA-H02NAI-TS-NYUGAIKBN (IDY)
                       WHEN    "1"
                           MOVE    "入"
                                       TO  SPA-H02GMN-TS-NYUGAIKBN (IDY)
                       WHEN    "2"
                           MOVE    "外"
                                       TO  SPA-H02GMN-TS-NYUGAIKBN (IDY)
                   END-EVALUATE
                   MOVE    TBL-HENREI-B-S-RECEKA           (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-RECEKA       (IDY)
                   MOVE    TBL-HENREI-B-S-RECESYUBETU      (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-RECESYUBETU  (IDY)
                   EVALUATE    SPA-H02NAI-TS-TEISYUTUSAKI  (IDY)
                       WHEN    "3"
                           EVALUATE    SPA-H02NAI-TS-RECESYUBETU  (IDY)
                               WHEN    "7101"
                               WHEN    "7091"
                                   MOVE    "傷病・業務"    TO
                                       SPA-H02GMN-TS-RECESYUBETU  (IDY)
                               WHEN    "7102"
                               WHEN    "7092"
                                   MOVE    "傷病・通勤"    TO
                                       SPA-H02GMN-TS-RECESYUBETU  (IDY)
                               WHEN    "7071"
                               WHEN    "7081"
                                   MOVE    "短期・業務"    TO
                                       SPA-H02GMN-TS-RECESYUBETU  (IDY)
                               WHEN    "7072"
                               WHEN    "7082"
                                   MOVE    "短期・通勤"    TO
                                       SPA-H02GMN-TS-RECESYUBETU  (IDY)
                               WHEN    OTHER
                                   MOVE
                                       SPA-H02NAI-TS-RECESYUBETU (IDY)
                                                           TO
                                       SPA-H02GMN-TS-RECESYUBETU  (IDY)
                           END-EVALUATE
                       WHEN    OTHER
                           MOVE    SPA-H02NAI-TS-RECESYUBETU  (IDY)
                                   TO  SPA-H02GMN-TS-RECESYUBETU  (IDY)
                   END-EVALUATE
                   MOVE    TBL-HENREI-B-S-HKNJANUM         (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-HKNJANUM     (IDY)
                   MOVE    TBL-HENREI-B-S-HOJOKBN-KEY      (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-HOJOKBN-KEY  (IDY)
                   MOVE    TBL-HENREI-B-S-KOHID-KEY        (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-KOHID-KEY    (IDY)
                   MOVE    TBL-HENREI-B-S-TEKSTYMD         (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-TEKSTYMD     (IDY)
                   MOVE    TBL-HENREI-B-S-RECEIPT-KBN      (IDZZ IDX)
                                   TO  SPA-H02NAI-TS-RECEIPT-KBN  (IDY)
                   MOVE    TBL-HENREI-B-S-KIGO             (IDZZ IDX)
                                   TO  SPA-H02GMN-TS-KIGO         (IDY)
      *
                   EVALUATE    TBL-HENREI-B-S-RECEIPT-KBN(IDZZ IDX)(1:1)
                       WHEN    "1"
                           MOVE    "老併"          TO
                                       SPA-H02GMN-TS-RECEIPT-KBN (IDY)
                       WHEN    "2"
                           MOVE    "老健"          TO
                                       SPA-H02GMN-TS-RECEIPT-KBN (IDY)
                       WHEN    "3"
                           MOVE    "医併"          TO
                                       SPA-H02GMN-TS-RECEIPT-KBN (IDY)
                       WHEN    "4"
                           MOVE    "医療"          TO
                                       SPA-H02GMN-TS-RECEIPT-KBN (IDY)
                   END-EVALUATE
      *
                   EVALUATE    TBL-HENREI-B-S-DISASTER (IDZZ IDX)
                       WHEN    "00"
                       WHEN    SPACE
                           MOVE    SPACE           TO
                                       SPA-H02GMN-TS-DISASTER (IDY)
                       WHEN    OTHER
                           MOVE    "災"            TO
                                       SPA-H02GMN-TS-DISASTER (IDY)
                   END-EVALUATE
               END-PERFORM
           END-PERFORM
      *
           MOVE    IDY                 TO  SPA-H02GMN-MAX
      *
      *    設定予定欄の設定
           IF    ( SPA-H01-SYOKBN      =   3    )
           AND   ( WRK-HENREI-B-HOSPNUM
                                       >   ZERO )
               MOVE    "設定予定："    TO  SPA-H02GMN-LABEL
               MOVE    WRK-HENREI-B-SYORIYM
                                       TO  WRK-SYMD  (1:6)
               MOVE    "01"            TO  WRK-SYMD  (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG(1:6) 
                                       TO  SPA-H02GMN-SYORIYM
               EVALUATE    WRK-HENREI-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入院"      TO  SPA-H02GMN-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "入院外"    TO  SPA-H02GMN-NYUGAIKBN
               END-EVALUATE
               MOVE    WRK-HENREI-B-RECESYUBETU
                                       TO  SPA-H02GMN-RECESYUBETU
               MOVE    WRK-HENREI-B-HKNJANUM
                                       TO  SPA-H02GMN-HKNJANUM
      *
      *        公費単独で生保又は中国残留邦人等のとき負担者番号、
      *        受給者番号をセット
               IF      WRK-HENREI-B-RECESYUBETU
                                       =   "1211" OR "1212"
                   IF    ( WRK-HENREI-B-FTNNUM (1) (1:2)
                                           =   "12" OR  "25" OR  "20"
                                           OR  "53" )
                       STRING  WRK-HENREI-B-FTNNUM (1)
                                               DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               WRK-HENREI-B-JKYNUM (1)
                                               DELIMITED   BY  SPACE
                                       INTO    SPA-H02GMN-KIGO
                       END-STRING
                   END-IF
               ELSE
                   STRING  WRK-HENREI-B-KIGO   DELIMITED   BY  SPACE
                           "　"                DELIMITED   BY  SIZE
                           WRK-HENREI-B-NUM    DELIMITED   BY  SPACE
                                               INTO    SPA-H02GMN-KIGO
                   END-STRING
               END-IF
               MOVE    WRK-HENREI-B-HR-MSG
                                       TO  SPA-H02GMN-ERR
           END-IF
      *
           IF      SPA-H01-SYOKBN      =   ZERO
               IF      SPA-H02GMN-MAX      =   1
                   MOVE    1                   TO  SPA-H02GMN-SELNUM
                                                   SPA-H02NAI-SELNUM
                                                   H02-HENREILIST-ROW
                   MOVE    99                  TO  SPA-H02GMN-CUR
               ELSE
                   MOVE    1                   TO  SPA-H02GMN-CUR 
               END-IF
           ELSE
               MOVE    98                  TO  SPA-H02GMN-CUR 
           END-IF
      *
           .
       320-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE               TO  SPA-H02-FLG
                   PERFORM 210-BACK
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    ANY             ALSO    "HENREILIST"
                   PERFORM 4102-HENREILIST-SEL-SEC
               WHEN    "ACTIVATE"      ALSO   ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-H02GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4101-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
          MOVE    SPA-H02GMN-SELNUM
                                   TO  SPA-H02GMN-SELNUM-OLD
          MOVE    SPA-H02NAI-SELNUM
                                   TO  SPA-H02NAI-SELNUM-OLD
      *
           MOVE    H02-SELNUM      TO  SPA-H02GMN-SELNUM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41021-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41021-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-H02NAI-SELNUM
      *
           IF    ( SPA-H02GMN-SELNUM   <   1              )
           OR    ( SPA-H02GMN-SELNUM   >   SPA-H02GMN-MAX )
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-H02GMN-SELNUM   TO  H02-HENREILIST-ROW
                                               SPA-H02NAI-SELNUM
      *
               PERFORM 41011-HENREILIST-SEL-SEC
           ELSE
               MOVE    1               TO  SPA-H02GMN-CUR
           END-IF
           .
       41021-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選択処理
      *****************************************************************
       41011-HENREILIST-SEL-SEC               SECTION.
      * 
           MOVE    SPA-H02NAI-TINDEX (SPA-H02NAI-SELNUM)
                                   TO  IDX
      *
           PERFORM 410111-HENREILST-SEL-CHK-SEC
           IF    ( SPA-HIDCD       =   SPACE )
           AND   ( SPA-ERRCD       =   SPACE )
               EVALUATE    TBL-HENREI-B-ERR (IDX)
                   WHEN    ZERO
                       MOVE    99              TO  SPA-H02GMN-CUR
                   WHEN    1
      *                既に返戻設定済
      *****            MOVE    "0002"              TO  SPA-ERRCD
                       CONTINUE
                   WHEN    2
      *                再再請求
                       MOVE    "1003"              TO  SPA-HIDCD
               END-EVALUATE
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE    
              MOVE    SPA-H02GMN-SELNUM-OLD
                                       TO  SPA-H02GMN-SELNUM
              MOVE    SPA-H02NAI-SELNUM-OLD
                                       TO  SPA-H02NAI-SELNUM
           END-IF 
           .
       41011-HENREILIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データチェック処理
      *****************************************************************
       410111-HENREILST-SEL-CHK-SEC               SECTION.
      *
      *    選択した処理年月より新しい処理年月にデータがあるとき
           IF    ( IDX                 >   1                          )
           AND   ( TBL-HENREI-B-SYORIYM (1)
                                       >   TBL-HENREI-B-SYORIYM (IDX) )
           AND   (  SPA-RECE10-TEISYUTUSAKI
                                       =   1                          ) 
      *        社保のときは無条件にダイアログ表示
               MOVE    "1007"          TO  SPA-HIDCD
               GO  TO  410111-HENREILST-SEL-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       TBL-HENREI-B-MAX
                   OR      TBL-HENREI-B-SYORIYM (IDZZ)
                                   <=      TBL-HENREI-B-SYORIYM (IDX)
               IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                          =   SPA-RECE10-RECESYUBETU )
               AND   ( TBL-HENREI-B-HKNJANUM    (IDZZ)
                                          =   SPA-RECE10-HKNJANUM    )
               AND   ( TBL-HENREI-B-KIGO        (IDZZ)
                                           =   SPA-RECE10-KIGO        )
                   IF      TBL-HENREI-B-NUM         (IDZZ) 
                                               =   SPA-RECE10-NUM
                       MOVE    "0003"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "1007"          TO  SPA-HIDCD
                   END-IF
                   GO  TO  410111-HENREILST-SEL-CHK-EXT
               END-IF
           END-PERFORM         
      *
      *    選択した処理年月より新しい処理年月に条件の一致するデータがあるか
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       TBL-HENREI-B-MAX
                   OR      TBL-HENREI-B-SYORIYM (IDZZ)
                                   <=      TBL-HENREI-B-SYORIYM (IDX)
               IF      SPA-RECE10-HKNNUM   =   SPACE
                   IF    ( TBL-HENREI-B-RECESYUBETU (IDX)
                                       =   SPA-RECE10-RECESYUBETU )
                       IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU  )
                           IF    ( TBL-HENREI-B-FTNNUM      (IDZZ 1)
                                               =   SPA-RECE10-FTNJANUM )
                               MOVE    "2004"          TO  SPA-HIDCD
                               GO  TO  410111-HENREILST-SEL-CHK-EXT
                           END-IF
                       END-IF
                   ELSE
                       IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU  )
                           MOVE    "2002"          TO  SPA-HIDCD
                       END-IF
                   END-IF
               ELSE
      *            違う提出先を選択したとき
                   IF  ( ( SPA-RECE10-TEISYUTUSAKI
                                             =   2   ) AND
                         ( TBL-HENREI-B-RECESYUBETU (IDX)(2:1)
                                             =   "3" ) )
                   OR  ( ( SPA-RECE10-TEISYUTUSAKI
                                             =   6   ) AND
                         ( TBL-HENREI-B-RECESYUBETU (IDX)(2:1)
                                         NOT =   "3" ) )
                       IF   (( SPA-RECE10-TEISYUTUSAKI
                                                   =   2   ) AND
                             ( TBL-HENREI-B-RECESYUBETU (IDZZ)(2:1)
                                               NOT =   "3" ) )
                       OR  ( ( SPA-RECE10-TEISYUTUSAKI
                                                   =   6   ) AND
                             ( TBL-HENREI-B-RECESYUBETU (IDZZ)(2:1)
                                                   =   "3" ) )
                           MOVE    "2006"      TO  SPA-HIDCD
                           GO  TO  410111-HENREILST-SEL-CHK-EXT
                       END-IF
                   ELSE
      *                同じ保険者を選択したとき
                       IF    ( TBL-HENREI-B-HKNJANUM    (IDX)
                                           =   SPA-RECE10-HKNJANUM )
                           IF      TBL-HENREI-B-HKNJANUM    (IDZZ)
                                               =   SPA-RECE10-HKNJANUM
                               IF      TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU
                                   MOVE    "2002"  TO  SPA-HIDCD
                                   GO  TO  410111-HENREILST-SEL-CHK-EXT
                               END-IF
                           END-IF
                       ELSE
                           IF      TBL-HENREI-B-HKNJANUM    (IDZZ)
                                               =   SPA-RECE10-HKNJANUM
                               MOVE    "2001"      TO  SPA-HIDCD
                           ELSE
                               IF    ( TBL-HENREI-B-RECESYUBETU
                                                              (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU )
                               AND   ( SPA-HIDCD
                                           =   SPACE                  )
                                   MOVE    "2002"      TO  SPA-HIDCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM         
      *
      *    選択データのチェック
           IF      SPA-RECE10-HKNNUM   =   SPACE
               IF    ( TBL-HENREI-B-RECESYUBETU (IDX)
                                       =   SPA-RECE10-RECESYUBETU )
               AND   ( TBL-HENREI-B-FTNNUM      (IDX 1)
                                       =   SPA-RECE10-FTNJANUM    )
                   GO  TO  410111-HENREILST-SEL-CHK-EXT
               END-IF
           ELSE
               IF    ( TBL-HENREI-B-RECESYUBETU (IDX)
                                       =   SPA-RECE10-RECESYUBETU )
               AND   ( TBL-HENREI-B-HKNJANUM    (IDX)
                                       =   SPA-RECE10-HKNJANUM    )
                   GO  TO  410111-HENREILST-SEL-CHK-EXT
               END-IF
           END-IF
      *
      *    同一の保険情報のデータがあるとき
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       TBL-HENREI-B-MAX
                   OR      TBL-HENREI-B-SYORIYM (IDZZ)
                                   <       TBL-HENREI-B-SYORIYM (IDX)
               IF      IDZZ            =   IDX
                   CONTINUE
               ELSE
                   IF      TBL-HENREI-B-SYORIYM (IDZZ)
                                       >   TBL-HENREI-B-SYORIYM (IDX)
                       CONTINUE
                   ELSE
      *                選択した処理年月と同じ処理年月にデータがあるとき
                       IF      SPA-RECE10-HKNNUM
                                           =   SPACE
                           IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU )
                           AND   ( TBL-HENREI-B-FTNNUM      (IDZZ 1)
                                           =   SPA-RECE10-FTNJANUM    )
                               MOVE    "0004"          TO  SPA-ERRCD
                               GO  TO  410111-HENREILST-SEL-CHK-EXT
                           END-IF
                       ELSE
                           IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU )
                           AND   ( TBL-HENREI-B-HKNJANUM    (IDZZ)
                                           =   SPA-RECE10-HKNJANUM    )
                               MOVE    "0004"          TO  SPA-ERRCD
                               GO  TO  410111-HENREILST-SEL-CHK-EXT
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM         
      *
      *    選択した処理年月に条件の一致する他のデータがあるか
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       TBL-HENREI-B-MAX
                   OR      TBL-HENREI-B-SYORIYM (IDZZ)
                                   <       TBL-HENREI-B-SYORIYM (IDX)
               IF    ( TBL-HENREI-B-SYORIYM (IDZZ)
                                       =   TBL-HENREI-B-SYORIYM (IDX) )
               AND   ( IDZZ        NOT =   IDX                        )
                   IF      SPA-RECE10-HKNNUM   =   SPACE
                       IF    ( TBL-HENREI-B-RECESYUBETU (IDX)
                                       =   SPA-RECE10-RECESYUBETU )
                           IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU  )
                               IF    ( TBL-HENREI-B-FTNNUM      (IDZZ 1)
                                               =   SPA-RECE10-FTNJANUM )
                                   MOVE    "1004"          TO  SPA-HIDCD
                                   GO  TO  410111-HENREILST-SEL-CHK-EXT
                               END-IF
                           END-IF
                       ELSE
                           IF    ( TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU  )
                               MOVE    "1002"          TO  SPA-HIDCD
                           END-IF
                       END-IF
                   ELSE
      *              違う提出先を選択したとき
                     IF  ( ( SPA-RECE10-TEISYUTUSAKI
                                             =   2   ) AND
                           ( TBL-HENREI-B-RECESYUBETU (IDX)(2:1)
                                             =   "3" ) )
                     OR  ( ( SPA-RECE10-TEISYUTUSAKI
                                             =   6   ) AND
                           ( TBL-HENREI-B-RECESYUBETU (IDX)(2:1)
                                         NOT =   "3" ) )
                       IF   (( SPA-RECE10-TEISYUTUSAKI
                                                   =   2   ) AND
                             ( TBL-HENREI-B-RECESYUBETU (IDZZ)(2:1)
                                               NOT =   "3" ) )
                       OR  ( ( SPA-RECE10-TEISYUTUSAKI
                                                   =   6   ) AND
                             ( TBL-HENREI-B-RECESYUBETU (IDZZ)(2:1)
                                                   =   "3" ) )
                           MOVE    "1006"      TO  SPA-HIDCD
                           GO  TO  410111-HENREILST-SEL-CHK-EXT
                       END-IF
                     ELSE
      *                同じ保険者を選択したとき
                       IF    ( TBL-HENREI-B-HKNJANUM    (IDX)
                                           =   SPA-RECE10-HKNJANUM )
                           IF      TBL-HENREI-B-HKNJANUM    (IDZZ)
                                               =   SPA-RECE10-HKNJANUM
                               IF      TBL-HENREI-B-RECESYUBETU (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU
                                   MOVE    "1002"  TO  SPA-HIDCD
                                   GO  TO  410111-HENREILST-SEL-CHK-EXT
                               END-IF
                           END-IF
                       ELSE
                           IF      TBL-HENREI-B-HKNJANUM    (IDZZ)
                                               =   SPA-RECE10-HKNJANUM
                               MOVE    "1001"      TO  SPA-HIDCD
                           ELSE
                               IF    ( TBL-HENREI-B-RECESYUBETU
                                                              (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU )
                               AND   ( SPA-HIDCD
                                           =   SPACE                  )
                                   MOVE    "1002"      TO  SPA-HIDCD
                               END-IF
                           END-IF
                       END-IF
                     END-IF
                   END-IF
               END-IF
           END-PERFORM         
           .
       410111-HENREILST-SEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4102-HENREILIST-SEL-SEC               SECTION.
      *
           IF      SPA-H01-SYOKBN      =   1
               MOVE    98                  TO  SPA-H02GMN-CUR
               GO  TO  4102-HENREILIST-SEL-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-H02GMN-MAX
               IF      H02-HENREILIST-SELECT (IDX)
                                       =   "T"
                   IF      IDX             =   SPA-H02NAI-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNO
                       MOVE    SPA-H02GMN-MAX      TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
      *
               MOVE    SPA-H02GMN-SELNUM   TO  SPA-H02GMN-SELNUM-OLD
               MOVE    SPA-H02NAI-SELNUM   TO  SPA-H02NAI-SELNUM-OLD
               MOVE    WRK-SELNO           TO  SPA-H02GMN-SELNUM
                                               SPA-H02NAI-SELNUM
                                               H02-HENREILIST-ROW
      *
               PERFORM 41011-HENREILIST-SEL-SEC
           END-IF
           .
       4102-HENREILIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "H01"               TO  SPA-SAKIPG
           MOVE    "H02"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC                SECTION.
      *
           MOVE    1                  TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-KAKUTEI-EXT
           END-IF
      *
           MOVE    "OK"                TO  SPA-H02-FLG
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-HIDCD       NOT =   SPACE
               PERFORM 520-HIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW         =   1
               MOVE    ZERO                TO  WRK-H02-ROW
           ELSE
               MOVE    H02-HENREILIST-ROW  TO  WRK-H02-ROW
           END-IF
      *
           INITIALIZE                       H02
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-H02GMN-MAX
               MOVE    SPA-H02GMN-TRENNUM      (IDX)
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  H02-TRENNUM      (IDX)
               MOVE    SPA-H02GMN-TSYORIYM     (IDX)
                                       TO  H02-TSYORIYM     (IDX)
               MOVE    SPA-H02GMN-TTEISYUTUSAKI2  (IDX)
                                       TO  H02-TTEISYUTUSAKI2 (IDX)
               MOVE    SPA-H02GMN-TNYUGAIKBN   (IDX)
                                       TO  H02-TNYUGAIKBN   (IDX)
               MOVE    SPA-H02GMN-TRECESYUBETU (IDX)
                                       TO  H02-TRECESYUBETU (IDX)
               MOVE    SPA-H02GMN-THKNJANUM    (IDX)
                                       TO  H02-THKNJANUM    (IDX)
               MOVE    SPA-H02GMN-TKIGO        (IDX)
                                       TO  H02-TKIGO        (IDX)
               MOVE    SPA-H02GMN-TRECEIPT-KBN (IDX)
                                       TO  H02-TRECEIPT-KBN (IDX)
               MOVE    SPA-H02GMN-TDISASTER    (IDX)
                                       TO  H02-TDISASTER    (IDX)
               MOVE    SPA-H02GMN-TERR         (IDX)
                                       TO  H02-TERR         (IDX)
               MOVE    SPA-H02GMN-TS-MAKE-FLG  (IDX)
                                       TO  H02-TS-MAKE-FLG  (IDX)
               MOVE    SPA-H02GMN-TS-SKYYM     (IDX)
                                       TO  H02-TS-SKYYM     (IDX)
               MOVE    SPA-H02GMN-TS-NYUGAIKBN (IDX)
                                       TO  H02-TS-NYUGAIKBN (IDX)
               MOVE    SPA-H02GMN-TS-RECESYUBETU (IDX)
                                       TO  H02-TS-RECESYUBETU(IDX)
               IF      SPA-H02NAI-TS-HKNJANUM   (IDX)
                                       NOT =   "00000000"
                   MOVE    SPA-H02NAI-TS-HKNJANUM   (IDX)
                                           TO  H02-TS-HKNJANUM  (IDX)
               END-IF
               MOVE    SPA-H02GMN-TS-KIGO       (IDX)
                                       TO  H02-TS-KIGO      (IDX)
               MOVE    SPA-H02GMN-TS-HOJOKBN-KEY(IDX)
                                       TO  H02-TS-HOJOKBN   (IDX)
               MOVE    SPA-H02GMN-TS-RECEIPT-KBN(IDX)
                                       TO  H02-TS-RECEIPT-KBN (IDX)
               MOVE    SPA-H02GMN-TS-DISASTER   (IDX)
                                       TO  H02-TS-DISASTER  (IDX)
               MOVE    SPA-H02GMN-TS-HENREI-SIJI-FLG (IDX)
                                       TO  H02-TS-HENREI-SIJI-FLG (IDX)
      *
               IF      IDX             =   SPA-H02NAI-SELNUM
                   MOVE    "T"         TO  H02-HENREILIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  H02-HENREILIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-H02GMN-MAX  TO  H02-HENREILIST-COUNT
           MOVE    ZERO            TO  H02-HENREILIST-ROW
           IF      WRK-H02-ROW     >   ZERO
               MOVE    WRK-H02-ROW     TO  H02-HENREILIST-ROW
           END-IF
      *
           MOVE    SPA-H02GMN-SELNUM
                                   TO  H02-SELNUM
      *
           MOVE    "darkgreen"     TO  H02-LABEL-STYLE
                                       H02-SYORIYM-STYLE
                                       H02-NYUGAIKBN-STYLE
                                       H02-RECESYUBETU-STYLE
                                       H02-HKNJANUM-STYLE
                                       H02-KIGO-STYLE
                                       H02-ERR-STYLE
           MOVE    SPA-H02GMN-LABEL
                                   TO  H02-LABEL
           MOVE    SPA-H02GMN-SYORIYM
                                   TO  H02-SYORIYM
           MOVE    SPA-H02GMN-NYUGAIKBN
                                   TO  H02-NYUGAIKBN
           MOVE    SPA-H02GMN-RECESYUBETU
                                   TO  H02-RECESYUBETU
           MOVE    SPA-H02GMN-HKNJANUM
                                   TO  H02-HKNJANUM
           MOVE    SPA-H02GMN-KIGO TO  H02-KIGO
           MOVE    SPA-H02GMN-ERR  TO  H02-ERR
      *
           IF      SPA-H01-SYOKBN      =   1
               MOVE    WIDGET-INSENSITIVE  TO  H02-B12-STATE
                                               H02-SELNUM-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  H02-B12-STATE
                                               H02-SELNUM-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-H02GMN-CUR      =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-H02GMN-CUR
             WHEN    1
                 MOVE    "SELNUM"          TO  MCP-WIDGET
             WHEN    98
                 MOVE    "B01"             TO  MCP-WIDGET
             WHEN    99
                 MOVE    "B12"             TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    99              TO  SPA-H02GMN-CUR
               WHEN    OTHER
                   MOVE    99              TO  SPA-H02GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "既に返戻設定済です"    TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE
                  "選択されたデータより後の日付の返戻データが存在します"
                                                   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
                  "保険情報が一致する別の返戻データが存在します"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  HERR
           MOVE    SPA-ERRCD            TO  HERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  HERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "H02"                TO  SPA-MOTOPG
           MOVE    "HERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-HIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  HID1
           INITIALIZE                       HID1
           MOVE    SPA-HIDCD            TO  HID1-ID1CODE
      *
           EVALUATE    SPA-HIDCD
               WHEN    "1001"
                   STRING  "保険者番号が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "1002"
                   STRING  "レセプト種別が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "1003"
                   MOVE
                   "既に返戻分再請求が行なわれていますが選択しますか。"
                                        TO  HID1-ID1MSG
               WHEN    "1004"
                   STRING  "負担者番号が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "1005"
                   STRING  "記号が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "1006"
                   STRING  "提出先が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "1007"
                   STRING  "選択されたデータより後の日付の返戻データが"
                                        DELIMITED  BY  SIZE 
                           "存在しますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "2001"
                   STRING  "選択されたデータより後の日付の返戻データに"
                                        DELIMITED  BY  SIZE 
                           "保険者番号が一致する別の返戻データがありま"
                                        DELIMITED  BY  SIZE 
                           "すがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "2002"
                   STRING  "選択されたデータより後の日付の返戻データに"
                                        DELIMITED  BY  SIZE 
                           "レセプト種別が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "2004"
                   STRING  "選択されたデータより後の日付の返戻データに"
                                        DELIMITED  BY  SIZE 
                           "負担者番号が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
               WHEN    "2006"
                   STRING  "選択されたデータより後の日付の返戻データに"
                                        DELIMITED  BY  SIZE 
                           "提出先が一致する別の"
                                        DELIMITED  BY  SIZE 
                           "返戻データがありますがよろしいですか。"
                                        DELIMITED  BY  SIZE 
                                        INTO  HID1-ID1MSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "H02"                TO  SPA-MOTOPG
           MOVE    "HID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-HIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD (1:6)          =   ZERO    OR  SPACE
               MOVE    SPACE                   TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
