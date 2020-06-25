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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSSYOKIKSN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 初期加算サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/03    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-小豆沢  04/03/11  有床診療所１群入院基本料１
      *                                     （医師配置適合）の追加
      *  00.00.02    NACL-小豆沢  04/04/21  有床診療所１群入院基本料１
      *                                     （看護・医師配置適合）の追加
      *平成１８年４月改定対応
      *  00.00.03    NACL-小豆沢  06/03/15  有床診療所入院基本料対応
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *平成２２年４月改定対応
      *  00.00.00    NACL-小豆沢  10/03/02  有床診療所入院基本料３対応
      *平成２６年４月改定対応
      *  00.00.00    NACL-小豆沢  14/02/27  有床診療所入院基本料１
      *                                     有床診療所入院基本料２
      *                                     有床診療所入院基本料３追加対応
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
           03  FLG-ERR                     PIC 9(01).
           03  FLG-SINRYOJO-VAL-1804       PIC 9(01).
           03  FLG-SINRYOJO-VAL-2204       PIC 9(01).
           03  FLG-SINRYOJO-VAL-2604       PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-KIKAN-TBL.
               05  WRK-KIKAN-OCC           OCCURS  7.
                   07  WRK-KIKAN-STZOGENPTN
                                           PIC X(01).
                   07  WRK-KIKAN-STZOGEN   PIC 9(03).
                   07  WRK-KIKAN-EDZOGENPTN
                                           PIC X(01).
                   07  WRK-KIKAN-EDZOGEN   PIC 9(03).
           03  WRK-KGNTEN-TBL.
               05  WRK-KGNTEN-OCC          OCCURS  7.
                   07  WRK-KGNTEN-SRYCD    PIC X(09).
                   07  WRK-KGNTEN-YUKOSTYMD
                                           PIC X(08).
                   07  WRK-KGNTEN-YUKOEDYMD
                                           PIC X(08).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-KGNTEN-VAL.
               05  CONST-KGNTEN-01-ST      PIC X(4)    VALUE   "1001".
               05  CONST-KGNTEN-01-ED      PIC X(4)    VALUE   "1014".
               05  CONST-KGNTEN-02-ST      PIC X(4)    VALUE   "1015".
               05  CONST-KGNTEN-02-ED      PIC X(4)    VALUE   "1030".
               05  CONST-KGNTEN-03-ST      PIC X(4)    VALUE   "1031".
               05  CONST-KGNTEN-03-ED      PIC X(4)    VALUE   "1090".
               05  CONST-KGNTEN-04-ST      PIC X(4)    VALUE   "1091".
               05  CONST-KGNTEN-04-ED      PIC X(4)    VALUE   "1180".
               05  CONST-KGNTEN-05-ST      PIC X(4)    VALUE   "1091".
               05  CONST-KGNTEN-05-ED      PIC X(4)    VALUE   "1180".
               05  CONST-KGNTEN-06-ST      PIC X(4)    VALUE   "1181".
               05  CONST-KGNTEN-06-ED      PIC X(4)    VALUE   "3001".
               05  CONST-KGNTEN-07-ST      PIC X(4)    VALUE   "1180".
               05  CONST-KGNTEN-07-ED      PIC X(4)    VALUE   "9999".
           03  CONST-SINRYOJO-VAL.
               05  CONST-SINRYOJO-01-ST    PIC X(4)    VALUE   "1001".
               05  CONST-SINRYOJO-01-ED    PIC X(4)    VALUE   "1007".
               05  CONST-SINRYOJO-02-ST    PIC X(4)    VALUE   "1008".
               05  CONST-SINRYOJO-02-ED    PIC X(4)    VALUE   "1014".
               05  CONST-SINRYOJO-03-ST    PIC X(4)    VALUE   "1015".
               05  CONST-SINRYOJO-03-ED    PIC X(4)    VALUE   "1030".
               05  CONST-SINRYOJO-04-ST    PIC X(4)    VALUE   "1031".
               05  CONST-SINRYOJO-04-ED    PIC X(4)    VALUE   "1090".
               05  CONST-SINRYOJO-05-ST    PIC X(4)    VALUE   "1091".
               05  CONST-SINRYOJO-05-ED    PIC X(4)    VALUE   "1180".
               05  CONST-SINRYOJO-06-ST    PIC X(4)    VALUE   "1181".
               05  CONST-SINRYOJO-06-ED    PIC X(4)    VALUE   "3001".
               05  CONST-SINRYOJO-07-ST    PIC X(4)    VALUE   "1180".
               05  CONST-SINRYOJO-07-ED    PIC X(4)    VALUE   "9999".
           03  CONST-SINRYOJO-VAL-1804.
               05  CONST-SINRYOJO-01-ST    PIC X(4)    VALUE   "1001".
               05  CONST-SINRYOJO-01-ED    PIC X(4)    VALUE   "1007".
               05  CONST-SINRYOJO-02-ST    PIC X(4)    VALUE   "1008".
               05  CONST-SINRYOJO-02-ED    PIC X(4)    VALUE   "1014".
               05  CONST-SINRYOJO-03-ST    PIC X(4)    VALUE   "1015".
               05  CONST-SINRYOJO-03-ED    PIC X(4)    VALUE   "1030".
               05  CONST-SINRYOJO-04-ST    PIC X(4)    VALUE   "1031".
               05  CONST-SINRYOJO-04-ED    PIC X(4)    VALUE   "1999".
               05  CONST-SINRYOJO-05-ST    PIC X(4)    VALUE   "1091".
               05  CONST-SINRYOJO-05-ED    PIC X(4)    VALUE   "1180".
               05  CONST-SINRYOJO-06-ST    PIC X(4)    VALUE   "1181".
               05  CONST-SINRYOJO-06-ED    PIC X(4)    VALUE   "3001".
               05  CONST-SINRYOJO-07-ST    PIC X(4)    VALUE   "1180".
               05  CONST-SINRYOJO-07-ED    PIC X(4)    VALUE   "9999".
      *    有床診療所入院基本料１、２、３（平成２２年度改定）
           03  CONST-SINRYOJO-VAL-2204.
               05  CONST-SINRYOJO-01-ST    PIC X(4)    VALUE   "1001".
               05  CONST-SINRYOJO-01-ED    PIC X(4)    VALUE   "1014".
               05  CONST-SINRYOJO-02-ST    PIC X(4)    VALUE   "1015".
               05  CONST-SINRYOJO-02-ED    PIC X(4)    VALUE   "1030".
               05  CONST-SINRYOJO-03-ST    PIC X(4)    VALUE   "1031".
               05  CONST-SINRYOJO-03-ED    PIC X(4)    VALUE   "1999".
               05  CONST-SINRYOJO-04-ST    PIC X(4)    VALUE   "1031".
               05  CONST-SINRYOJO-04-ED    PIC X(4)    VALUE   "1999".
               05  CONST-SINRYOJO-05-ST    PIC X(4)    VALUE   "1032".
               05  CONST-SINRYOJO-05-ED    PIC X(4)    VALUE   "1033".
               05  CONST-SINRYOJO-06-ST    PIC X(4)    VALUE   "1034".
               05  CONST-SINRYOJO-06-ED    PIC X(4)    VALUE   "1035".
               05  CONST-SINRYOJO-07-ST    PIC X(4)    VALUE   "1036".
               05  CONST-SINRYOJO-07-ED    PIC X(4)    VALUE   "9999".
      *    有床診療所入院基本料１、２、３、４、５、６（平成２６年度改定）
           03  CONST-SINRYOJO-VAL-2604.
               05  CONST-SINRYOJO-01-ST    PIC X(4)    VALUE   "1001".
               05  CONST-SINRYOJO-01-ED    PIC X(4)    VALUE   "1014".
               05  CONST-SINRYOJO-02-ST    PIC X(4)    VALUE   "1015".
               05  CONST-SINRYOJO-02-ED    PIC X(4)    VALUE   "1030".
               05  CONST-SINRYOJO-03-ST    PIC X(4)    VALUE   "1031".
               05  CONST-SINRYOJO-03-ED    PIC X(4)    VALUE   "1999".
               05  CONST-SINRYOJO-04-ST    PIC X(4)    VALUE   "1031".
               05  CONST-SINRYOJO-04-ED    PIC X(4)    VALUE   "1999".
               05  CONST-SINRYOJO-05-ST    PIC X(4)    VALUE   "1032".
               05  CONST-SINRYOJO-05-ED    PIC X(4)    VALUE   "1033".
               05  CONST-SINRYOJO-06-ST    PIC X(4)    VALUE   "1034".
               05  CONST-SINRYOJO-06-ED    PIC X(4)    VALUE   "1035".
               05  CONST-SINRYOJO-07-ST    PIC X(4)    VALUE   "1036".
               05  CONST-SINRYOJO-07-ED    PIC X(4)    VALUE   "9999".
      *
      *    結核病棟入院基本料（平成２８年度改定）
           03  CONST-KEKKAKU-VAL-2804.
               05  CONST-KEKKAKU-01-ST      PIC X(4)    VALUE   "1001".
               05  CONST-KEKKAKU-01-ED      PIC X(4)    VALUE   "1014".
      *
               05  CONST-KEKKAKU-02-ST      PIC X(4)    VALUE   "1015".
               05  CONST-KEKKAKU-02-ED      PIC X(4)    VALUE   "1030".
      *
               05  CONST-KEKKAKU-03-ST      PIC X(4)    VALUE   "1031".
               05  CONST-KEKKAKU-03-ED      PIC X(4)    VALUE   "1060".
      *
               05  CONST-KEKKAKU-04-ST      PIC X(4)    VALUE   "1061".
               05  CONST-KEKKAKU-04-ED      PIC X(4)    VALUE   "1090".
      *
               05  CONST-KEKKAKU-05-ST      PIC X(4)    VALUE   "1091".
               05  CONST-KEKKAKU-05-ED      PIC X(4)    VALUE   "1180".
      *
               05  CONST-KEKKAKU-06-ST      PIC X(4)    VALUE   "1181".
               05  CONST-KEKKAKU-06-ED      PIC X(4)    VALUE   "3001".
      *
               05  CONST-KEKKAKU-07-ST      PIC X(4)    VALUE   "1180".
               05  CONST-KEKKAKU-07-ED      PIC X(4)    VALUE   "9999".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院料算定マスタ
       01  NYUINKHN-REC.
           COPY  "CPNYUINKHN.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPORCSSYOKIKSN.INC".
           COPY  "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               ORCSSYOKIKSNAREA
                                               SPA-AREA.
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              SYOKIKSN-OT-AREA
                                   FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
      *    パラメータチェック処理
           PERFORM 100-PARA-CHK-SEC
           IF    ( FLG-ERR NOT =   ZERO )
               MOVE    FLG-ERR TO  SYOKIKSN-OT-RC
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    編集処理
           IF     SYOKIKSN-IN-SANTEISTYMD   >=  "20140401"
               PERFORM 202-EDIT-SEC
           ELSE
               IF     SYOKIKSN-IN-SANTEISTYMD   >=  "20100401"
                   PERFORM 201-EDIT-SEC
               ELSE
                   PERFORM 200-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( FLG-ERR NOT =   ZERO )
               MOVE    FLG-ERR TO  SYOKIKSN-OT-RC
               GO  TO  000-PROC-EXT
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック処理
      *****************************************************************
       100-PARA-CHK-SEC            SECTION.
      *
      *    通算日数に数字が設定されていること
           IF    ( SYOKIKSN-IN-TUSANNISSU  IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1       TO  FLG-ERR
               GO  TO  100-PARA-CHK-EXT
           END-IF
      *
      *    基本点数診療コード が設定されていること
           IF    ( SYOKIKSN-IN-KHNSRYCD    =   SPACE )
               MOVE    1       TO  FLG-ERR
               GO  TO  100-PARA-CHK-EXT
           END-IF
      *
      *    算定開始日が設定されていること
           IF    ( SYOKIKSN-IN-SANTEISTYMD =   SPACE )
               MOVE    1       TO  FLG-ERR
               GO  TO  100-PARA-CHK-EXT
           END-IF
      *
      *    算定開始日が日付として妥当であること
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( FLG-ERR         NOT =   ZERO )
               GO  TO  100-PARA-CHK-EXT
           END-IF
      *
           .
       100-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    編集処理（平成２２年３月以前）
      *****************************************************************
       200-EDIT-SEC                SECTION.
      *
           IF    ( SYOKIKSN-IN-KHNSRYCD    =   "190097010"
            OR                                 "190097110"
            OR                                 "190097210"
            OR                                 "190097310"
            OR                                 "190109510"
            OR                                 "190109670"
            OR                                 "190805710"
            OR                                 "190805810"
            OR                                 "190805910"
            OR                                 "190806010"
            OR                                 "190808710"
            OR                                 "190808870"
            OR                                 "190117070"
            OR                                 "190813270"
            OR                                 "190117670"
            OR                                 "190813870"
            OR                                 "099999950"
            OR                                 "099999951"   )
      *        平成１８年４月改定対応（有床診療所入院基本料）
               IF  (SYOKIKSN-IN-KHNSRYCD  = "099999950"  OR
                                            "099999951") AND
                   SYOKIKSN-IN-SANTEISTYMD(1:6) >= "200604"
                   MOVE    1       TO    FLG-SINRYOJO-VAL-1804
                   MOVE    CONST-SINRYOJO-VAL-1804  TO  WRK-KIKAN-TBL
               ELSE
                   MOVE    CONST-SINRYOJO-VAL  TO  WRK-KIKAN-TBL
               END-IF
           ELSE
               MOVE    CONST-KGNTEN-VAL    TO  WRK-KIKAN-TBL
           END-IF
      *
      *    基準日取得処理
           PERFORM 210-KJNYMD-GET-SEC
      *
      *    入院料算定マスタ検索処理
           PERFORM 210-NYUINKHN-SEL-SEC
           IF    ( FLG-ERR NOT =   ZERO )
               GO  TO  200-EDIT-EXT
           END-IF
      *
      *    平成１８年４月改定対応（有床診療所入院基本料）
      **     DISPLAY  "FLG-SINRYOJO-VAL-1804 = " FLG-SINRYOJO-VAL-1804
           IF   FLG-SINRYOJO-VAL-1804     =    1
               IF   SYOKIKSN-IN-KHNSRYCD  = "099999950"
                   MOVE    SPACE        TO  SYOKIKSN-IN-KHNSRYCD
      *            有床診療所入院基本料１
      **             MOVE   "190097010"   TO  NYUKHN-KAGENCD (1)
      **             MOVE   "190097110"   TO  NYUKHN-KAGENCD (2)
      **             MOVE   "190119510"   TO  NYUKHN-KAGENCD (3)
      **             MOVE   "190119610"   TO  NYUKHN-KAGENCD (4)
               ELSE
                   MOVE    SPACE        TO  SYOKIKSN-IN-KHNSRYCD
      *            有床診療所入院基本料１
      **             MOVE   "190097210"   TO  NYUKHN-KAGENCD (1)
      **             MOVE   "190097310"   TO  NYUKHN-KAGENCD (2)
      **             MOVE   "190119710"   TO  NYUKHN-KAGENCD (3)
      **             MOVE   "190119810"   TO  NYUKHN-KAGENCD (4)
               END-IF
           END-IF
      *    ワーク加減算テーブルに診療行為コードと有効期間を設定
           INITIALIZE              WRK-KGNTEN-TBL
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7 )
      *
               MOVE    NYUKHN-KAGENCD (IDX1)
                                   TO  WRK-KGNTEN-SRYCD (IDX1)
      *        基準日
               MOVE    WRK-KJNYMD
                                   TO WRK-YMD
               MOVE    WRK-KIKAN-STZOGENPTN (IDX1)
                                   TO WRK-ZOGENPTN
               MOVE    WRK-KIKAN-STZOGEN (IDX1)
                                   TO WRK-ZOGEN
               PERFORM 220-HIZUKE-GET-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-KGNTEN-YUKOSTYMD (IDX1)
      *
      *        有効期間終了日
               MOVE    WRK-KJNYMD
                                   TO WRK-YMD
               MOVE    WRK-KIKAN-EDZOGENPTN (IDX1)
                                   TO WRK-ZOGENPTN
               MOVE    WRK-KIKAN-EDZOGEN (IDX1)
                                   TO WRK-ZOGEN
               PERFORM 220-HIZUKE-GET-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-KGNTEN-YUKOEDYMD (IDX1)
      *
           END-PERFORM
      *
      *    ワーク加減算テーブルの該当の有効期間開始日に算定開始日を
      *    設定する
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7 )
      *
               IF    ( SYOKIKSN-IN-SANTEISTYMD
                                   >=  WRK-KGNTEN-YUKOSTYMD (IDX1) )
                AND  ( SYOKIKSN-IN-SANTEISTYMD
                                   <=  WRK-KGNTEN-YUKOEDYMD (IDX1) )
                       MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-KGNTEN-YUKOSTYMD (IDX1)
               ELSE
                   IF    ( SYOKIKSN-IN-SANTEISTYMD
                                   >   WRK-KGNTEN-YUKOEDYMD (IDX1))
                       MOVE    SPACE   TO  WRK-KGNTEN-SRYCD  (IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    同一点数のデータが存在する場合はまとめる
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   6 )
               COMPUTE IDX2    =   IDX1    +   1
               IF    ( WRK-KGNTEN-SRYCD (IDX1) NOT =   SPACE )
                   IF    ( WRK-KGNTEN-SRYCD (IDX1)
                                   =   WRK-KGNTEN-SRYCD (IDX2))
                       PERFORM VARYING IDX3    FROM    IDX2    BY  1
                           UNTIL     ( IDX3    >   7 )
                               OR    ( WRK-KGNTEN-SRYCD (IDX1)
                                       NOT =   WRK-KGNTEN-SRYCD (IDX3))
                           MOVE    WRK-KGNTEN-YUKOEDYMD (IDX3)
                               TO  WRK-KGNTEN-YUKOEDYMD (IDX1)
                           MOVE    SPACE   TO  WRK-KGNTEN-SRYCD (IDX3)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ワーク加減算テーブルの内容を返却値に設定
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7)
      *
               IF    ( WRK-KGNTEN-SRYCD  (IDX1)
                                   NOT =   SPACE )
                   COMPUTE IDX2    =   IDX2    +   1
                   MOVE    WRK-KGNTEN-SRYCD (IDX1)
                                   TO  SYOKIKSN-OT-KGNSRYCD  (IDX2)
                   MOVE    WRK-KGNTEN-YUKOSTYMD (IDX1)
                                   TO  SYOKIKSN-OT-YUKOSTYMD (IDX2)
                   MOVE    WRK-KGNTEN-YUKOEDYMD (IDX1)
                                   TO  SYOKIKSN-OT-YUKOEDYMD (IDX2)
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  SYOKIKSN-OT-RC
      *
           .
       200-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    編集処理（平成２２年４月改定以降）
      *****************************************************************
       201-EDIT-SEC                SECTION.
      *
           IF    ( SYOKIKSN-IN-KHNSRYCD    =   "099999950"
            OR                                 "099999951"
            OR                                 "099999952"   )
      *        平成２２年４月改定対応（有床診療所入院基本料）
               MOVE    1       TO    FLG-SINRYOJO-VAL-2204
               MOVE    CONST-SINRYOJO-VAL-2204  TO  WRK-KIKAN-TBL
           ELSE
               MOVE    CONST-KGNTEN-VAL    TO  WRK-KIKAN-TBL
           END-IF
      *
      *    基準日取得処理
           PERFORM 210-KJNYMD-GET-SEC
      *
      *    入院料算定マスタ検索処理
           PERFORM 210-NYUINKHN-SEL-SEC
           IF    ( FLG-ERR NOT =   ZERO )
               GO  TO  201-EDIT-EXT
           END-IF
      *
      *    平成２２年４月改定対応（有床診療所入院基本料）
           IF   FLG-SINRYOJO-VAL-2204     =    1
               IF   SYOKIKSN-IN-KHNSRYCD  = "099999950"  OR
                                            "099999951"  OR
                                            "099999952"
                   MOVE    SPACE        TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *    ワーク加減算テーブルに診療行為コードと有効期間を設定
           INITIALIZE              WRK-KGNTEN-TBL
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7 )
      *
               MOVE    NYUKHN-KAGENCD (IDX1)
                                   TO  WRK-KGNTEN-SRYCD (IDX1)
      *        基準日
               MOVE    WRK-KJNYMD
                                   TO WRK-YMD
               MOVE    WRK-KIKAN-STZOGENPTN (IDX1)
                                   TO WRK-ZOGENPTN
               MOVE    WRK-KIKAN-STZOGEN (IDX1)
                                   TO WRK-ZOGEN
               PERFORM 220-HIZUKE-GET-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-KGNTEN-YUKOSTYMD (IDX1)
      *
      *        有効期間終了日
               MOVE    WRK-KJNYMD
                                   TO WRK-YMD
               MOVE    WRK-KIKAN-EDZOGENPTN (IDX1)
                                   TO WRK-ZOGENPTN
               MOVE    WRK-KIKAN-EDZOGEN (IDX1)
                                   TO WRK-ZOGEN
               PERFORM 220-HIZUKE-GET-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-KGNTEN-YUKOEDYMD (IDX1)
      *
           END-PERFORM
      *
      *    ワーク加減算テーブルの該当の有効期間開始日に算定開始日を
      *    設定する
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7 )
      *
               IF    ( SYOKIKSN-IN-SANTEISTYMD
                                   >=  WRK-KGNTEN-YUKOSTYMD (IDX1) )
                AND  ( SYOKIKSN-IN-SANTEISTYMD
                                   <=  WRK-KGNTEN-YUKOEDYMD (IDX1) )
                       MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-KGNTEN-YUKOSTYMD (IDX1)
               ELSE
                   IF    ( SYOKIKSN-IN-SANTEISTYMD
                                   >   WRK-KGNTEN-YUKOEDYMD (IDX1))
                       MOVE    SPACE   TO  WRK-KGNTEN-SRYCD  (IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    同一点数のデータが存在する場合はまとめる
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   6 )
               COMPUTE IDX2    =   IDX1    +   1
               IF    ( WRK-KGNTEN-SRYCD (IDX1) NOT =   SPACE )
                   IF    ( WRK-KGNTEN-SRYCD (IDX1)
                                   =   WRK-KGNTEN-SRYCD (IDX2))
                       PERFORM VARYING IDX3    FROM    IDX2    BY  1
                           UNTIL     ( IDX3    >   7 )
                               OR    ( WRK-KGNTEN-SRYCD (IDX1)
                                       NOT =   WRK-KGNTEN-SRYCD (IDX3))
                           MOVE    WRK-KGNTEN-YUKOEDYMD (IDX3)
                               TO  WRK-KGNTEN-YUKOEDYMD (IDX1)
                           MOVE    SPACE   TO  WRK-KGNTEN-SRYCD (IDX3)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ワーク加減算テーブルの内容を返却値に設定
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7)
      *
               IF    ( WRK-KGNTEN-SRYCD  (IDX1)
                                   NOT =   SPACE )
                   COMPUTE IDX2    =   IDX2    +   1
                   MOVE    WRK-KGNTEN-SRYCD (IDX1)
                                   TO  SYOKIKSN-OT-KGNSRYCD  (IDX2)
                   MOVE    WRK-KGNTEN-YUKOSTYMD (IDX1)
                                   TO  SYOKIKSN-OT-YUKOSTYMD (IDX2)
                   MOVE    WRK-KGNTEN-YUKOEDYMD (IDX1)
                                   TO  SYOKIKSN-OT-YUKOEDYMD (IDX2)
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  SYOKIKSN-OT-RC
      *
           .
       201-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    編集処理（平成２６年４月改定以降）
      *****************************************************************
       202-EDIT-SEC                SECTION.
      *
           IF    ( SYOKIKSN-IN-KHNSRYCD    =   "099999950"
            OR                                 "099999951"
            OR                                 "099999952"
            OR                                 "099999955"
            OR                                 "099999956"
            OR                                 "099999957"   )
      *        平成２６年４月改定対応（有床診療所入院基本料）
               MOVE    1       TO    FLG-SINRYOJO-VAL-2604
               MOVE    CONST-SINRYOJO-VAL-2604  TO  WRK-KIKAN-TBL
           ELSE
               MOVE    CONST-KGNTEN-VAL    TO  WRK-KIKAN-TBL
           END-IF
      *
      *    結核病棟入院基本料（平成２８年度改定）
           IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=    "201604"
      *        結核病棟入院基本料の判定の為、点数マスタ参照
               INITIALIZE                        TNS-TENSU-REC
               MOVE    SPA-HOSPNUM           TO  TNS-HOSPNUM
               MOVE    SYOKIKSN-IN-KHNSRYCD  TO  TNS-SRYCD
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                             TO  TNS-YUKOSTYMD
               MOVE    TNS-YUKOSTYMD         TO  TNS-YUKOEDYMD
               PERFORM   910-TENSU-READ-SEC
               IF    ( TNS-CDKBN-KBN        =    "A"  )   AND
                     ( TNS-CDKBN-KBNNUM     =    102  )
                   MOVE    CONST-KEKKAKU-VAL-2804  TO  WRK-KIKAN-TBL
               END-IF
           END-IF
      *
      *    基準日取得処理
           PERFORM 210-KJNYMD-GET-SEC
      *
      *    入院料算定マスタ検索処理
           PERFORM 210-NYUINKHN-SEL-SEC
           IF    ( FLG-ERR NOT =   ZERO )
               GO  TO  202-EDIT-EXT
           END-IF
      *
      *    平成２６年４月改定対応（有床診療所入院基本料）
           IF   FLG-SINRYOJO-VAL-2604     =    1
               IF   SYOKIKSN-IN-KHNSRYCD  = "099999950"  OR
                                            "099999951"  OR
                                            "099999952"  OR
                                            "099999955"  OR
                                            "099999956"  OR
                                            "099999957"
                   MOVE    SPACE        TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *    ワーク加減算テーブルに診療行為コードと有効期間を設定
           INITIALIZE              WRK-KGNTEN-TBL
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7 )
      *
               MOVE    NYUKHN-KAGENCD (IDX1)
                                   TO  WRK-KGNTEN-SRYCD (IDX1)
      *        基準日
               MOVE    WRK-KJNYMD
                                   TO WRK-YMD
               MOVE    WRK-KIKAN-STZOGENPTN (IDX1)
                                   TO WRK-ZOGENPTN
               MOVE    WRK-KIKAN-STZOGEN (IDX1)
                                   TO WRK-ZOGEN
               PERFORM 220-HIZUKE-GET-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-KGNTEN-YUKOSTYMD (IDX1)
      *
      *        有効期間終了日
               MOVE    WRK-KJNYMD
                                   TO WRK-YMD
               MOVE    WRK-KIKAN-EDZOGENPTN (IDX1)
                                   TO WRK-ZOGENPTN
               MOVE    WRK-KIKAN-EDZOGEN (IDX1)
                                   TO WRK-ZOGEN
               PERFORM 220-HIZUKE-GET-SEC
               MOVE    WRK-KEISANYMD
                                   TO  WRK-KGNTEN-YUKOEDYMD (IDX1)
      *
           END-PERFORM
      *
      *    ワーク加減算テーブルの該当の有効期間開始日に算定開始日を
      *    設定する
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7 )
      *
               IF    ( SYOKIKSN-IN-SANTEISTYMD
                                   >=  WRK-KGNTEN-YUKOSTYMD (IDX1) )
                AND  ( SYOKIKSN-IN-SANTEISTYMD
                                   <=  WRK-KGNTEN-YUKOEDYMD (IDX1) )
                       MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-KGNTEN-YUKOSTYMD (IDX1)
               ELSE
                   IF    ( SYOKIKSN-IN-SANTEISTYMD
                                   >   WRK-KGNTEN-YUKOEDYMD (IDX1))
                       MOVE    SPACE   TO  WRK-KGNTEN-SRYCD  (IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    同一点数のデータが存在する場合はまとめる
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   6 )
               COMPUTE IDX2    =   IDX1    +   1
               IF    ( WRK-KGNTEN-SRYCD (IDX1) NOT =   SPACE )
                   IF    ( WRK-KGNTEN-SRYCD (IDX1)
                                   =   WRK-KGNTEN-SRYCD (IDX2))
                       PERFORM VARYING IDX3    FROM    IDX2    BY  1
                           UNTIL     ( IDX3    >   7 )
                               OR    ( WRK-KGNTEN-SRYCD (IDX1)
                                       NOT =   WRK-KGNTEN-SRYCD (IDX3))
                           MOVE    WRK-KGNTEN-YUKOEDYMD (IDX3)
                               TO  WRK-KGNTEN-YUKOEDYMD (IDX1)
                           MOVE    SPACE   TO  WRK-KGNTEN-SRYCD (IDX3)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    ワーク加減算テーブルの内容を返却値に設定
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   7)
      *
               IF    ( WRK-KGNTEN-SRYCD  (IDX1)
                                   NOT =   SPACE )
                   COMPUTE IDX2    =   IDX2    +   1
                   MOVE    WRK-KGNTEN-SRYCD (IDX1)
                                   TO  SYOKIKSN-OT-KGNSRYCD  (IDX2)
                   MOVE    WRK-KGNTEN-YUKOSTYMD (IDX1)
                                   TO  SYOKIKSN-OT-YUKOSTYMD (IDX2)
                   MOVE    WRK-KGNTEN-YUKOEDYMD (IDX1)
                                   TO  SYOKIKSN-OT-YUKOEDYMD (IDX2)
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  SYOKIKSN-OT-RC
      *
           .
       202-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    基準日取得処理
      *****************************************************************
       210-KJNYMD-GET-SEC              SECTION.
      *
      *
      *    基準となる日付をもとめる（算定開始日−通算日数）
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO WRK-YMD
      *
           MOVE    "1"             TO WRK-ZOGENPTN
      *
           COMPUTE WRK-ZOGEN       =  SYOKIKSN-IN-TUSANNISSU
                                   *  -1
           PERFORM 5002-CALENDAR-SEC
      *
           MOVE    WRK-KEISANYMD   TO WRK-KJNYMD
      *
           .
       210-KJNYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院料算定マスタ検索処理
      *****************************************************************
       210-NYUINKHN-SEL-SEC            SECTION.
      *
           INITIALIZE  NYUINKHN-REC
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO  NYUKHN-KHNSRYCD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  NYUKHN-YUKOSTYMD
                                           NYUKHN-YUKOEDYMD
           MOVE    NYUINKHN-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinkhn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1               TO  FLG-ERR
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINKHN-REC
           END-IF
      *
           MOVE    "tbl_nyuinkhn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-NYUINKHN-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院料算定マスタ検索処理
      *****************************************************************
       220-HIZUKE-GET-SEC              SECTION.
      *
           IF    ( WRK-ZOGEN       = 999 )
               MOVE    "99999999"  TO  WRK-KEISANYMD
               GO  TO  220-HIZUKE-GET-EXT
           END-IF
      *
           PERFORM 5002-CALENDAR-SEC
      *
           MOVE    WRK-KEISANYMD   TO WRK-YMD
           MOVE    "1"             TO WRK-ZOGENPTN
           MOVE    -1              TO WRK-ZOGEN
           PERFORM 5002-CALENDAR-SEC
      *
           .
       220-HIZUKE-GET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-ERR
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   911-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE              TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
