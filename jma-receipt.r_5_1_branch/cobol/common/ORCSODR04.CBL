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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODR04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集（食事）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/05  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-NACCT-NOT-FOUND     PIC 9(01).
           03  FLG-END                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-NYUINKA             PIC 9(05).
           03  CNT-HKNCOMBI            PIC 9(05).
           03  CNT-SKJTIME             PIC 9(05).
           03  CNT-SKJTYPE             PIC 9(05).
           03  CNT-NISSU               PIC 9(05).
           03  CNT-SAME-AFTER          PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXF                    PIC 9(05).
           03  IDX-ODR                 PIC 9(05).
           03  IDX-DAY                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                PIC X(08).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-EOMYMD.
               05  WRK-EOMYM           PIC X(06).
               05  WRK-EOMDD           PIC 9(02).
           03  WRK-COMBI-HKNCOMBI      PIC 9(04).
           03  WRK-SYS-KANRICD         PIC X(04).
           03  WRK-SYS-KBNCD           PIC X(08).
           03  WRK-DIALOG              PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-MCP-WINDOW          PIC X(64).
           03  WRK-ORDERID             PIC X(04).
           03  WRK-ENDYM               PIC X(06).
           03  WRK-LASTYM              PIC X(06).
           03  WRK-NACCTYM             PIC X(06).
      *
       01  ITEM-AREA.
           03  ITEM-RRKNUM             PIC 9(03).
           03  ITEM-NYUINKA            PIC X(02).
           03  ITEM-HKNCOMBI           PIC 9(04).
           03  ITEM-ENDYMD             PIC X(08).
           03  ITEM-SAGAKUCD           PIC X(02).
           03  ITEM-SKJ-AREA.
               05  ITEM-SKJ-OCC        OCCURS 3.
                   07  ITEM-SKJTIME    PIC X(02).
                   07  ITEM-SKJTYPE    PIC X(01).
                   07  ITEM-NISSU      PIC X(100).
                   07  ITEM-DAY-G.
                       09  ITEM-DAY    PIC 9(01)
                                       OCCURS  31.
           03  ITEM-SAME-AFTER-G.
               05  ITEM-SAME-AFTER     PIC X(01)
                                       OCCURS 3.
      *
       01  ERRCD-AREA.
           03  ERRCD-ITEM-G.
               05  ERRCD-ITEM          PIC X(04).
               05  ERRCD-ORDER         PIC X(04).
           03  ERRCD-ERRCD             PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-H280401           PIC X(08)   VALUE "20160401".
           03  CONST-ITEMNUM-MAX       PIC 9(05)   VALUE 30.
           03  CONST-SKJRYOYO2         PIC X(09)   VALUE "197000710".
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    患者入院履歴退避
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目テーブル
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為算定日一括入力
           COPY    "CPORCSNDAYCHG.INC".
      *
      *    オーダー共通サブ
           COPY    "CPORCSODRS01.INC".
      *
      *    オーダー入院会計作成サブ
           COPY    "CPORCSODRS04.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSODRNACCT.INC".
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //LNK-PTNYUINRRK-//.
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ERRCD-AREA
           INITIALIZE                  ITEM-AREA
           INITIALIZE                  SODR-OT
           INITIALIZE                  TRRK-REC
      *
           IF    ( SODR-SRCKBN     =    "1" OR "2" )
               PERFORM 990-LOCK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G  =   SPACE )
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SODR-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
               MOVE    SODR-SRYYMD     TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
               PERFORM 900-PTNYUINRRK-KEY42-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-REC  TO  TRRK-REC
               ELSE
                   MOVE    "0075"          TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( SODR-ORDERID        =   NORDERID-FOODID
                                        OR NORDERID-CRENACCTID
                                        OR NORDERID-SAGAKUID
                                                           )
               MOVE    PTNYUINRRK-NYUINKA  TO  SODR-SRYKA
                                               ITEM-NYUINKA
               MOVE    PTNYUINRRK-HKNCOMBINUM
                                           TO  SODR-HKNCOMBI
                                               ITEM-HKNCOMBI
               PERFORM 1001-NACCT-SEC
           END-IF
      *
           MOVE    SODR-SRYYMD         TO  WRK-SYMD
           PERFORM 800-EOMYMD-SEC
           MOVE    WRK-SYMD            TO  WRK-EOMYMD
      *
           PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
           MOVE    PTNYUINRRK-RRKNUM   TO  ITEM-RRKNUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       1001-NACCT-SEC                  SECTION.
      *
           MOVE    SPA-SYSYMD              TO  WRK-SYMD
           MOVE    "2"                     TO  WRK-ZOGENPTN
           MOVE    3                       TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD(1:6)      TO  WRK-ENDYM
      *
           MOVE    SPA-SYSYMD              TO  WRK-SYMD
           MOVE    "2"                     TO  WRK-ZOGENPTN
           MOVE    -1                      TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD(1:6)      TO  WRK-LASTYM
      *
           PERFORM 900-NYUINACCT-KEY58-SEL-SEC
           IF    ( FLG-NYUINACCT           =   ZERO )
               MOVE    NACCT-SRYYM         TO  WRK-NACCTYM
               IF    ( SODR-ORDERID        =   NORDERID-FOODID
                                            OR NORDERID-SAGAKUID )
                AND  ( WRK-NACCTYM         <   SODR-SRYYMD (1:6))
                   IF    ( PTNYUINRRK-TAIINYMD =   "99999999" )
                       IF    ( WRK-NACCTYM <   WRK-LASTYM )
                           MOVE    1           TO  FLG-NACCT-NOT-FOUND
                       END-IF
                   ELSE
                       IF    ( WRK-NACCTYM <   PTNYUINRRK-TAIINYMD(1:6))
                           MOVE    1           TO  FLG-NACCT-NOT-FOUND
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NACCT-NOT-FOUND
           END-IF
      *
           .
       1001-NACCT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               EVALUATE    SODR-ORDERID
               WHEN    NORDERID-CRENACCTID
                   PERFORM 2003-KANREN-CHK-SEC
               WHEN    NORDERID-SAGAKUID
                   PERFORM 2004-KANREN-CHK-SEC
               WHEN    OTHER
                   PERFORM 2002-KANREN-CHK-SEC
               END-EVALUATE
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
      *
               IF    ( FLG-END  =   ZERO )
                  PERFORM 2101-ORCSODRNACCT-CALL-SEC
               END-IF
      *
      *        入院履歴の更新は会計の更新後に行う
               IF    ( FLG-END =   ZERO )
                   EVALUATE    TRUE
                   WHEN  ( SODR-ORDERID        =   NORDERID-SAGAKUID )
                    AND  ( TRRK-RRKNUM         =   ITEM-RRKNUM )
                    AND  ( ITEM-ENDYMD         =   SPACE )
                       PERFORM 2101-PTNYUINRRK-UPDATE-SEC
                   WHEN  ( SODR-ORDERID        =   NORDERID-FOODID )
                    AND  ( TRRK-RRKNUM         =   ITEM-RRKNUM )
                    AND  ( ITEM-SAME-AFTER-G   NOT =   SPACE )
                       PERFORM 2101-PTNYUINRRK-UPDATE-SEC
                   END-EVALUATE
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "1" )
                       PERFORM 2101-ORDER-RECONF-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "2" )
                       CONTINUE
                   ELSE
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
           ELSE
               IF    ( SODR-PROCKBN    =   "1" OR "2" )
                       CONTINUE
               ELSE
                   IF    ( FLG-END  =   ZERO )
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
      *
           PERFORM VARYING IDX-ODR    FROM    1   BY  1
                       UNTIL ( IDX-ODR    >   LNK-ODRITEM-MAX )
      *
               EVALUATE    SODR-ORDERID
               WHEN    NORDERID-FOODID
                   EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
                   WHEN    NCLAIMID-NYUINKAJMA
                       PERFORM 20011-NYUINKA-CHK-SEC
                   WHEN    NCLAIMID-HKNCOMBIJMA
                       PERFORM 20011-HKNCOMBI-CHK-SEC
                   WHEN    NCLAIMID-SKJTIMEJMA
                       PERFORM 20012-SKJTIME-CHK-SEC
                   WHEN    NCLAIMID-SKJTYPEJMA
                       PERFORM 20011-SKJTYPE-CHK-SEC
                   WHEN    NCLAIMID-NISSUJMA
                       PERFORM 20012-NISSU-CHK-SEC
                   WHEN    NCLAIMID-SAME-AFTERJMA
                       PERFORM 20011-SAME-AFTER-CHK-SEC
                   END-EVALUATE
               WHEN    NORDERID-CRENACCTID
                   CONTINUE
               WHEN    NORDERID-SAGAKUID
                   EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
                   WHEN    NCLAIMID-SAGAKUJMA
                       PERFORM 20011-SAGAKU-CHK-SEC
                   WHEN    NCLAIMID-ENDYMDJMA
                       PERFORM 20011-ENDYMD-CHK-SEC
                   END-EVALUATE
               WHEN    OTHER
                   EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
                   WHEN    NCLAIMID-NYUINKAJMA
                       PERFORM 20011-NYUINKA-CHK-SEC
                   WHEN    NCLAIMID-HKNCOMBIJMA
                       PERFORM 20011-HKNCOMBI-CHK-SEC
                   WHEN    NCLAIMID-SKJTIMEJMA
                       PERFORM 20011-SKJTIME-CHK-SEC
                   WHEN    NCLAIMID-NISSUJMA
                       PERFORM 20011-NISSU-CHK-SEC
                   END-EVALUATE
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20011-NYUINKA-CHK-SEC           SECTION.
      *
           COMPUTE CNT-NYUINKA  =   CNT-NYUINKA  +   1
      *
           IF    ( CNT-NYUINKA  =   1 )
            AND  ( ITEM-NYUINKA =   SPACE )
      *
               MOVE    "1005"          TO  WRK-SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-SYS-KBNCD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
      *
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-NYUINKA
      *
               ELSE
                   MOVE    "0002"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC           SECTION.
      *
           COMPUTE CNT-HKNCOMBI  =   CNT-HKNCOMBI  +   1
      *
           IF    ( CNT-HKNCOMBI  =   1 )
            AND  ( ITEM-HKNCOMBI =   ZERO )
      *
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  WRK-COMBI-HKNCOMBI
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI        =   ZERO )
                   IF    ( COMB-DELKBN NOT =   "1" )
                     AND ( SODR-SRYYMD    >=  COMB-TEKSTYMD )
                     AND ( SODR-SRYYMD    <=  COMB-TEKEDYMD )
      *
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-HKNCOMBI
                   ELSE
                       MOVE    "0005"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                    END-IF
               ELSE
                       MOVE    "0005"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック処理
      *****************************************************************
       20011-SKJTIME-CHK-SEC           SECTION.
      *
           COMPUTE CNT-SKJTIME  =   CNT-SKJTIME  +   1
      *
           IF    ( CNT-SKJTIME  =   1 )
               PERFORM 200111-SKJTIME-CHK-SEC
           ELSE
               PERFORM 2100-ITEM-UNPROCSTS-SET-SEC
               MOVE    "0086"  TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック処理
      *****************************************************************
       20012-SKJTIME-CHK-SEC           SECTION.
      *
           COMPUTE CNT-SKJTIME  =   CNT-SKJTIME  +   1
      *
           IF    ( CNT-SKJTIME  <=  3 )
                IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                            NOT =   SPACE )
                    PERFORM 200111-SKJTIME-CHK-SEC
                END-IF
           END-IF
      *
           .
       20012-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック処理
      *****************************************************************
       200111-SKJTIME-CHK-SEC          SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)   =  "01"
                                                OR "02"
                                                OR "03" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-SKJTIME (CNT-SKJTIME)
           ELSE
               MOVE    "0085"  TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       200111-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事の種類チェック処理
      *****************************************************************
       20011-SKJTYPE-CHK-SEC           SECTION.
      *
           COMPUTE CNT-SKJTYPE =   CNT-SKJTYPE +   1
      *
           IF    ( CNT-SKJTYPE <=  3 )
      *
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  =   SPACE )
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                   WHEN  ( SODR-SRYYMD     >=  CONST-H280401 )
                       IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       = "0" OR "1" OR "2" OR "3" )
                           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-SKJTYPE (CNT-SKJTYPE)
                       ELSE
                           MOVE    "0145"      TO  ERRCD-ERRCD
                           PERFORM 2100-ITEM-ERR-SET-SEC
                       END-IF
                   WHEN  OTHER
                       IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       = "0" OR "1" OR "2" )
                           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-SKJTYPE (CNT-SKJTYPE)
                       ELSE
                           MOVE    "0145"      TO  ERRCD-ERRCD
                           PERFORM 2100-ITEM-ERR-SET-SEC
                       END-IF
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       20011-SKJTYPE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック処理
      *****************************************************************
       20011-NISSU-CHK-SEC             SECTION.
      *
           MOVE    1               TO  CNT-NISSU
      *
           PERFORM 200111-NISSU-CHK-SEC
      *
           .
       20011-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック処理
      *****************************************************************
       20012-NISSU-CHK-SEC             SECTION.
      *
           COMPUTE CNT-NISSU = CNT-NISSU   +   1
      *
           IF    ( CNT-NISSU  <=   3 )
      *
               PERFORM 200111-NISSU-CHK-SEC
      *
           END-IF
      *
           .
       20012-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック処理
      *****************************************************************
       200111-NISSU-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  ITEM-NISSU (CNT-NISSU)
      *
           IF    ( ITEM-NISSU (CNT-NISSU) (1:1) =   "*" )
               INITIALIZE                  ORCSNDAYCHGAREA
               MOVE    ITEM-NISSU (CNT-NISSU)
                                       TO  ORCSNDAYCHG-INPUTCD
               MOVE    SODR-SRYYMD     TO  ORCSNDAYCHG-SRYYMD
               MOVE    "1"             TO  ORCSNDAYCHG-KBN
               MOVE    100             TO  ORCSNDAYCHG-KETA
               MOVE    TRRK-NYUINYMD   TO  ORCSNDAYCHG-NYUINYMD
               MOVE    TRRK-TAIINYMD   TO  ORCSNDAYCHG-TAIINYMD
               CALL    "ORCSNDAYCHG"       USING
                                           SPA-AREA
                                           ORCSNDAYCHGAREA
               IF    ( ORCSNDAYCHG-ERRCD   =   SPACE )
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                       IF    ( ORCSNDAYCHG-NYU-DAY (IDX-DAY)
                                               >   ZERO )
                           MOVE    1   TO  ITEM-DAY (CNT-NISSU IDX-DAY)
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( ITEM-DAY-G (CNT-NISSU)  =   ZERO )
                       MOVE    "0137"      TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
      *
               ELSE
                   EVALUATE    ORCSNDAYCHG-ERRCD
                   WHEN    "0002"
                       MOVE    "0138"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   WHEN    OTHER
                       MOVE    "0136"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       200111-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       20011-ENDYMD-CHK-SEC            SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-ENDYMD
      *
           .
       20011-ENDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事継続区分処理
      *****************************************************************
       20011-SAME-AFTER-CHK-SEC        SECTION.
      *
           COMPUTE CNT-SAME-AFTER  =   CNT-SAME-AFTER  +   1
      *
           IF    ( CNT-SAME-AFTER <=  3 )
               IF    ( TRRK-RRKNUM =   ITEM-RRKNUM )
                   IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                   =   SPACE  OR "0" OR "1" )
                       IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  =   "1" )
                         MOVE  "1" TO  ITEM-SAME-AFTER (CNT-SAME-AFTER)
                       END-IF
                   ELSE
                       MOVE    "0201"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-SAME-AFTER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       20011-SAGAKU-CHK-SEC            SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-SAGAKUCD
      *
           .
       20011-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    項目未処理状態設定処理
      *****************************************************************
       2100-ITEM-UNPROCSTS-SET-SEC        SECTION.
      *
           MOVE    3               TO  LNK-ODRITEM-STATUS (IDX-ODR)
      *
           .
       2100-ITEM-UNPROCSTS-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目チェックエラー設定処理
      *****************************************************************
       2100-ITEM-ERR-SET-SEC           SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
               MOVE    ERRCD-ERRCD TO  ERRCD-ITEM
           END-IF
      *
           IF  ( LNK-ODRITEM-STATUS (IDX-ODR)  =   SPACE )
               MOVE    2       TO  LNK-ODRITEM-STATUS (IDX-ODR)
           END-IF
      *
           MOVE    ERRCD-ERRCD TO  LNK-ODRITEM-ERRCD (IDX-ODR)
      *
           .
       2100-ITEM-ERR-SET-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院科チェック処理
               PERFORM 20021-NYUINKA-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        保険組合せチェック処理
               PERFORM 20021-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        食事時間チェック処理
               PERFORM 20021-SKJTIME-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
                    OR   ( ERRCD-ORDER NOT =   SPACE )
      *
               IF    ( ITEM-SKJTIME (IDXA) NOT =   SPACE )
      *
                   IF    ( ERRCD-ORDER     =   SPACE )
      *                食事の種類チェック処理
                       PERFORM 20021-SKJTYPE-CHK-SEC
                   END-IF
      *
                   IF    ( ERRCD-ORDER     =   SPACE )
      *                特別食チェック
                       PERFORM 20021-TOKSYOKU-CHK-SEC
                   END-IF
      *
                   IF    ( ERRCD-ORDER     =   SPACE )
      *                日数チェック処理
                       PERFORM 20021-NISSU-CHK-SEC
                   END-IF
      *
                   IF    ( ERRCD-ORDER     =   SPACE )
      *                食事継続区分チェック
                       PERFORM 20021-SAME-AFTER-CHK-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院履歴チェック処理
               PERFORM 20021-NYUINRRK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院会計チェック処理
               PERFORM 20021-NACCT-CHK-SEC
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20021-NYUINKA-CHK-SEC           SECTION.
      *
           IF    ( ITEM-NYUINKA    =   SPACE )
               MOVE    "0002"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20021-HKNCOMBI-CHK-SEC          SECTION.
      *
           IF    ( ITEM-HKNCOMBI   =   ZERO )
               MOVE    "0005"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック処理
      *****************************************************************
       20021-SKJTIME-CHK-SEC           SECTION.
      *
           IF    ( ITEM-SKJTIME (1)   =   SPACE )
            AND  ( ITEM-SKJTIME (2)   =   SPACE )
            AND  ( ITEM-SKJTIME (3)   =   SPACE )
               MOVE    "0085"         TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事の種類チェック処理
      *****************************************************************
       20021-SKJTYPE-CHK-SEC          SECTION.
      *
           IF    ( SODR-ORDERID   =   NORDERID-FOODID )
               IF    ( ITEM-SKJTYPE (IDXA)     =   SPACE )
                   MOVE    "0145"     TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           .
       20021-SKJTYPE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特別食チェック処理
      *****************************************************************
       20021-TOKSYOKU-CHK-SEC          SECTION.
      *
           IF    ( SODR-ORDERID        =   NORDERID-FSPCLID )
            OR  (( SODR-ORDERID        =   NORDERID-FOODID )
              AND( ITEM-SKJTYPE (IDXA) =   "2" ))
               MOVE    "5000"          TO  WRK-SYS-KANRICD
               MOVE    "*"             TO  WRK-SYS-KBNCD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
                   IF    ( SYS-5000-SHOKUJIRYOYOCD
                                       =   CONST-SKJRYOYO2 )
                       MOVE    "0160"  TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           .
       20021-TOKSYOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴チェック処理
      *****************************************************************
       20021-NYUINRRK-CHK-SEC          SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
      *
      *    診療科と保険組合せの変更は不可
           IF    ( PTNYUINRRK-NYUINKA  NOT =   ITEM-NYUINKA )
               MOVE    "0069"      TO  ERRCD-ORDER
           END-IF
      *
      *    IF    ( PTNYUINRRK-HKNCOMBINUM  NOT =   ITEM-HKNCOMBI )
      *        MOVE    "0070"      TO  ERRCD-ORDER
      *        GO  TO  20021-NYUINRRK-CHK-EXT
      *    END-IF
      *
           .
       20021-NYUINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック処理
      *****************************************************************
       20021-NISSU-CHK-SEC             SECTION.
      *
      D    DISPLAY "TRRK-TAIINYMD=" TRRK-TAIINYMD
           IF    ( ITEM-DAY-G (IDXA)   =   ZERO )
                MOVE   SODR-SRYYMD     TO  WRK-SRYYMD
                MOVE    1              TO  ITEM-DAY (IDXA WRK-SRYDD)
           END-IF
      *
           IF  (  ITEM-SAME-AFTER (IDXA)   =   "1" )
                MOVE   SODR-SRYYMD     TO  WRK-SRYYMD
                PERFORM VARYING WRK-SRYDD  FROM    WRK-EOMDD   BY  -1
                        UNTIL ( WRK-SRYDD  <       1 )
                    IF    ( WRK-SRYYMD  >=      TRRK-NYUINYMD )
                     AND  ( WRK-SRYYMD  <=      TRRK-TAIINYMD )
                        IF    ( ITEM-DAY (IDXA WRK-SRYDD)
                                           =   1 )
                           MOVE    1   TO  WRK-SRYDD
                        ELSE
                           MOVE    1   TO  ITEM-DAY (IDXA WRK-SRYDD)
                        END-IF
                    END-IF
                END-PERFORM
           END-IF
      *
      D    DISPLAY "ITEM-DAY [         1         2         3 ]" IDXA
      D    DISPLAY "ITEM-DAY [1234567890123456789012345678901]" IDXA
      D    DISPLAY "ITEM-DAY [" ITEM-DAY-G (IDXA) "]" IDXA
      *
           .
       20021-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事継続区分チェック処理
      *****************************************************************
       20021-SAME-AFTER-CHK-SEC        SECTION.
      *
           IF    ( ITEM-SAME-AFTER (IDXA)  =   "1" )
               IF    ( SODR-SRYYMD (1:6)   <   WRK-NACCTYM )
                   MOVE    SODR-SRYYMD (1:6)   TO  WRK-SYMD
                   MOVE    "01"                TO  WRK-SYMD(7:2)
                   MOVE    "2"                 TO  WRK-ZOGENPTN
                   MOVE    11                  TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   IF    ( WRK-KEISANYMD (1:6) <   WRK-NACCTYM )
                       MOVE    "0199"  TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           .
       20021-SAME-AFTER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2003-KANREN-CHK-SEC             SECTION.
      *
           IF    ( FLG-NACCT-NOT-FOUND NOT =   ZERO )
            OR   ( TRRK-TAIINYMD       NOT =   "99999999" )
               MOVE    "0195"          TO  ERRCD-ORDER
           END-IF
      *
           .
       2003-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2004-KANREN-CHK-SEC             SECTION.
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院会計チェック処理
               PERFORM 20021-NACCT-CHK-SEC
           END-IF
      *
           .
       2004-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       20021-NACCT-CHK-SEC             SECTION.
      *
           IF    ( SODR-ORDERID    =   NORDERID-FOODID
                                    OR NORDERID-SAGAKUID )
            AND  ( WRK-NACCTYM     <   SODR-SRYYMD (1:6))
               IF    ( FLG-NACCT-NOT-FOUND NOT =   ZERO )
                   MOVE    "0084"      TO  ERRCD-ORDER
               END-IF
           ELSE
               PERFORM 900-NYUINACCT-KEY17-SEL-SEC
               IF    ( FLG-NYUINACCT   NOT =   ZERO )
                   MOVE    "0084"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
               IF    ( SODR-ORDERID      =   NORDERID-FOODID
                                          OR NORDERID-SAGAKUID )
                   IF    ( WRK-NACCTYM  >=   SODR-SRYYMD (1:6) )
                       CONTINUE
                   ELSE
                       IF  ( SODR-SRYYMD (1:6)
                                   >   WRK-ENDYM )
                           MOVE    "0194"
                                   TO  ERRCD-ORDER
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       20021-NACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダー情報再編成処理
      *****************************************************************
       2101-ORDER-RECONF-SEC           SECTION.
      *
           CALL    "ORCSODRS01"        USING   SODR-AREA
                                               SODRS01-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           MOVE    SODRS01-RC      TO  FLG-END
      *
           .
       2101-ORDER-RECONF-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       2101-ORDER-INSERT-SEC           SECTION.
      *
           PERFORM 21011-ORDER-HEN-SEC
      *
           PERFORM VARYING IDX-ODR FROM    1   BY  1
                   UNTIL ( IDX-ODR >   LNK-ODRITEM-MAX )
                    OR   ( FLG-END =   1 )
      *
                PERFORM 21011-ODRITEM-HEN-SEC
      *
           END-PERFORM
      *
           .
       2101-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    オーダ編集処理
      *****************************************************************
       21011-ORDER-HEN-SEC             SECTION.
      *
           INITIALIZE                      ORDER-REC
      *
           MOVE   SODR-HOSPNUM          TO  ORDER-HOSPNUM
           MOVE   SODR-KARTE-KEY       TO  ORDER-KARTE-KEY
           MOVE   SODR-ORDERNUM        TO  ORDER-ORDERNUM
           MOVE   SODR-NYUGAIKBN       TO  ORDER-NYUGAIKBN
           MOVE   SODR-PTNUM           TO  ORDER-PTNUM
           MOVE   SODR-PTID            TO  ORDER-PTID
           MOVE   SODR-SRYKA           TO  ORDER-SRYKA
           MOVE   SODR-SRYYMD          TO  ORDER-SRYYMD
           MOVE   SODR-HKNCOMBI        TO  ORDER-HKNCOMBI
           MOVE   SODR-ORDERID         TO  ORDER-ORDERID
           MOVE   SODR-UKEYMD          TO  ORDER-UKEYMD
           MOVE   SODR-UKEHMS          TO  ORDER-UKEHMS
           MOVE   SODR-KEYGENERATE     TO  ORDER-KEYGENERATE
      *
           MOVE   ERRCD-ORDER          TO  ORDER-ERRCD1
           MOVE   ERRCD-ITEM           TO  ORDER-ERRCD2
      *
           IF    ( ERRCD-ITEM          =   SPACE )
            AND  ( ERRCD-ORDER         =   SPACE )
               MOVE    TRRK-RRKNUM     TO  ORDER-RRKNUM
               MOVE    TRRK-RRKEDANUM  TO  ORDER-RRKEDANUM
           ELSE
               IF    ( SODR-KEYGENERATE    =   2 )
                       MOVE    1       TO  ORDER-STATUS
               ELSE
                   IF    ( WRK-DIALOG      =   ZERO )
                       MOVE    2       TO  ORDER-STATUS
                   ELSE
                       MOVE    5       TO  ORDER-STATUS
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SODR-PROCKBN    =   "1" )
               MOVE     1              TO  ORDER-MODIFY
           END-IF
      *
           MOVE    SODR-OPID           TO  ORDER-OPID
           MOVE    SODR-UPYMD          TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    SODR-UPHMS          TO  ORDER-UPHMS
      *
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           IF    ( FLG-ORDER           =   ZERO )
               DISPLAY "ORCSDR04 ERROR 001:ORDER INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ORDER-HEN-EXT
           END-IF
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR04 ERROR 002:ORDER INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ORDER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目編集処理
      *****************************************************************
       21011-ODRITEM-HEN-SEC           SECTION.
      *
           MOVE    LNK-ODRITEM-OCC (IDX-ODR)
                                       TO  ODRITEM-REC
      *
           IF    ( LNK-ODRITEM-ITEMID (IDX-ODR)
                                       =   NCLAIMID-NYUINKAJMA
                                       OR  NCLAIMID-HKNCOMBIJMA
                                       OR  NCLAIMID-SKJTIMEJMA
                                       OR  NCLAIMID-SKJTYPEJMA
                                       OR  NCLAIMID-NISSUJMA
                                       OR  NCLAIMID-SAME-AFTERJMA
                                       OR  NCLAIMID-SAGAKUJMA
                                       OR  NCLAIMID-ENDYMDJMA )
               CONTINUE
           ELSE
               MOVE    3               TO  ODRITEM-STATUS
               MOVE    "9001"          TO  ODRITEM-ERRCD
           END-IF
      *
           MOVE    SODR-OPID           TO  ODRITEM-OPID
           MOVE    SODR-UPYMD          TO  ODRITEM-CREYMD
                                           ODRITEM-UPYMD
           MOVE    SODR-UPHMS          TO  ODRITEM-UPHMS
      *
      *
           PERFORM 900-ODRITEM-KEY-SEL-SEC
           IF    ( FLG-ODRITEM           =   ZERO )
               DISPLAY "ORCSDR04 ERROR 003:ODRITEM INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ODRITEM-HEN-EXT
           END-IF
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR04 ERROR 004:ODRITEM INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴更新処理
      *****************************************************************
       2101-PTNYUINRRK-UPDATE-SEC      SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
      *
               EVALUATE    TRUE
               WHEN  ( SODR-ORDERID        =   NORDERID-SAGAKUID )
                   PERFORM 21011-PTNYUINRRK-UPDATE-SEC
               WHEN  ( SODR-ORDERID        =   NORDERID-FOODID )
                   PERFORM 21012-PTNYUINRRK-UPDATE-SEC
               END-EVALUATE
      *
               MOVE    SODR-OPID   TO  PTNYUINRRK-OPID
               MOVE    SODR-UPYMD  TO  PTNYUINRRK-UPYMD
               MOVE    SODR-UPHMS  TO  PTNYUINRRK-UPHMS
      *
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   DISPLAY "ORCSDR04 ERROR 002 :PTNYUINRRK UPDATE ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
               END-IF
      *
               IF    ( FLG-END     =   ZERO )
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "upd4"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                     DISPLAY "ORCSDR04 ERROR 009 :PTNYUINRRK UPDATE ERR"
                             " PTID="        SODR-PTID
                             " KARTE-KEY="   SODR-KARTE-KEY
                     MOVE    1               TO  FLG-END
                   END-IF
               END-IF
           END-IF
      *
           .
       2101-PTNYUINRRK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴更新処理（室料差額）
      *****************************************************************
       21011-PTNYUINRRK-UPDATE-SEC      SECTION.
      *
           IF    ( ITEM-SAGAKUCD   =   "00" )
               MOVE    SPACE   TO  PTNYUINRRK-RM-SAGAKU
           ELSE
               MOVE    ITEM-SAGAKUCD
                               TO  PTNYUINRRK-RM-SAGAKU
           END-IF
      *
           .
       21011-PTNYUINRRK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴追加処理（食事区分）
      *****************************************************************
       21012-PTNYUINRRK-UPDATE-SEC      SECTION.
      *
           IF    ( PTNYUINRRK-SKJKBN =   "B" OR "C" )
               CONTINUE
           ELSE
               MOVE    "0"     TO  PTNYUINRRK-SKJKBN-ASA
                                   PTNYUINRRK-SKJKBN-HIRU
                                   PTNYUINRRK-SKJKBN-YU
           END-IF
      *
           MOVE    "C"         TO  PTNYUINRRK-SKJKBN
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
      *
               IF    ( ITEM-SKJTIME (IDXA)     NOT =   SPACE )
                AND  ( ITEM-SAME-AFTER (IDXA)  NOT =   SPACE )
                   EVALUATE    ITEM-SKJTIME (IDXA)
                   WHEN    "01"
                       MOVE    ITEM-SKJTYPE (IDXA)
                                   TO  PTNYUINRRK-SKJKBN-ASA
                   WHEN    "02"
                       MOVE    ITEM-SKJTYPE (IDXA)
                                   TO  PTNYUINRRK-SKJKBN-HIRU
                   WHEN    "03"
                       MOVE    ITEM-SKJTYPE (IDXA)
                                   TO  PTNYUINRRK-SKJKBN-YU
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           .
       21012-PTNYUINRRK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成サブ呼出
      *****************************************************************
       2101-ORCSODRNACCT-CALL-SEC      SECTION.
      *
           EVALUATE     SODR-ORDERID
           WHEN    NORDERID-FOODID
           WHEN    NORDERID-SAGAKUID
               PERFORM 21011-CREATE-NACCT-SEC
           WHEN    NORDERID-CRENACCTID
               PERFORM 21012-CREATE-NACCT-SEC
           END-EVALUATE
      *
           IF    ( FLG-END         =   ZERO )
               IF    ( SODR-ORDERID    =   NORDERID-CRENACCTID )
                   CONTINUE
               ELSE
                   PERFORM 21011-ORCSODRNACCT-CALL-SEC
               END-IF
           END-IF
      *
           .
       2101-ORCSODRNACCT-CALL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成処理
      *****************************************************************
       21011-CREATE-NACCT-SEC          SECTION.
      *
           IF    ( WRK-NACCTYM   >=    SODR-SRYYMD(1:6) )
               CONTINUE
           ELSE
               INITIALIZE                      SODRS04-AREA
               MOVE    WRK-NACCTYM         TO  WRK-SYMD
               MOVE    "01"                TO  WRK-SYMD(7:2)
               MOVE    "2"                 TO  WRK-ZOGENPTN
               MOVE    1                   TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD (1:6) TO  SODRS04-STARTYM
               MOVE    SODR-SRYYMD(1:6)    TO  SODRS04-ENDYM
      *
               CALL    "ORCSODRS04"            USING
                                               SODR-AREA
                                               SODRS04-AREA
                                               TRRK-REC
                                               SPA-AREA
               IF    ( SODRS04-RC      NOT =   ZERO )
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           .
       21011-CREATE-NACCT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成処理
      *****************************************************************
       21012-CREATE-NACCT-SEC          SECTION.
      *
           IF    ( WRK-NACCTYM             >=  WRK-ENDYM )
               CONTINUE
           ELSE
               INITIALIZE                      SODRS04-AREA
               MOVE    WRK-NACCTYM         TO  WRK-SYMD
               MOVE    "01"                TO  WRK-SYMD(7:2)
               MOVE    "2"                 TO  WRK-ZOGENPTN
               MOVE    1                   TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD (1:6) TO  SODRS04-STARTYM
               MOVE    WRK-NACCTYM     TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:2)
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    6               TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD (1:6)
                                       TO  SODRS04-ENDYM
               IF    ( WRK-ENDYM       <   SODRS04-ENDYM )
                   MOVE    WRK-ENDYM   TO  SODRS04-ENDYM
               END-IF
      *
               CALL    "ORCSODRS04"            USING
                                               SODR-AREA
                                               SODRS04-AREA
                                               TRRK-REC
                                               SPA-AREA
               IF    ( SODRS04-RC      NOT =   ZERO )
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           .
       21012-CREATE-NACCT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成サブ呼出
      *****************************************************************
       21011-ORCSODRNACCT-CALL-SEC     SECTION.
      *
           EVALUATE    SODR-ORDERID
           WHEN  NORDERID-FOODID
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   3 )
      *
                   MOVE    SPACE       TO  WRK-ORDERID
      *
                   EVALUATE    ITEM-SKJTYPE (IDXA)
                   WHEN    "0"
                       MOVE    NORDERID-FSTOPID
                                   TO  WRK-ORDERID
                   WHEN    "1"
                       MOVE    NORDERID-FNORLID
                                   TO  WRK-ORDERID
                   WHEN    "2"
                       MOVE    NORDERID-FSPCLID
                                   TO  WRK-ORDERID
                   WHEN    "3"
                       MOVE    NORDERID-FLIQUIDID
                                   TO  WRK-ORDERID
                   END-EVALUATE
      *
                   IF    ( WRK-ORDERID NOT =   SPACE )
                       PERFORM 210111-ORCSODRNACCT-CALL-SEC
                   END-IF
      *
               END-PERFORM
      *
           WHEN  NORDERID-SAGAKUID
      *
               IF    ( ITEM-SAGAKUCD   =   "00" )
                   MOVE    NORDERID-SAGAKUCANCELID
                                       TO  WRK-ORDERID
               ELSE
                   MOVE    SODR-ORDERID
                                       TO  WRK-ORDERID
                   MOVE    ITEM-SAGAKUCD
                                       TO  TRRK-RM-SAGAKU
               END-IF
               MOVE    1               TO  IDXA
               PERFORM 210111-ORCSODRNACCT-CALL-SEC
      *
           WHEN  OTHER
               MOVE    SODR-ORDERID    TO  WRK-ORDERID
               MOVE    1               TO  IDXA
               PERFORM 210111-ORCSODRNACCT-CALL-SEC
           END-EVALUATE
      *
           .
       21011-ORCSODRNACCT-CALL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成サブ呼出
      *****************************************************************
       210111-ORCSODRNACCT-CALL-SEC     SECTION.
      *
           INITIALIZE                  SODRNACCT-AREA
           MOVE    WRK-ORDERID     TO  SODRNACCT-ORDERID
           MOVE    ZERO            TO  SODRNACCT-NYUKBN
           MOVE    SODR-SRYYMD     TO  SODRNACCT-STARTYMD
           MOVE    ITEM-ENDYMD     TO  SODRNACCT-ENDYMD
           MOVE    SODR-OPID       TO  SODRNACCT-OPID
           MOVE    SODR-UPYMD      TO  SODRNACCT-UPYMD
           MOVE    SODR-UPHMS      TO  SODRNACCT-UPHMS
           MOVE    ITEM-SKJTIME(IDXA)
                                   TO  SODRNACCT-SKJTIME
           MOVE    ITEM-DAY-G (IDXA)
                                   TO  SODRNACCT-DAY-G
           MOVE    ITEM-SAME-AFTER (IDXA)
                                   TO  SODRNACCT-SAME-AFTER
           MOVE    TRRK-REC        TO  LNK-PTNYUINRRK-REC
      *
           CALL    "ORCSODRNACCT"      USING
                                       SODRNACCT-AREA
                                       LNK-PTNYUINRRK-REC
                                       SPA-AREA
           IF    ( SODRNACCT-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           .
       210111-ORCSODRNACCT-CALL-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
            AND  ( ERRCD-ORDER     =   SPACE )
               MOVE    ZERO        TO  SODR-ERRFLG
           ELSE
               MOVE    1           TO  SODR-ERRFLG
           END-IF
      *
           MOVE    ERRCD-ORDER     TO  SODR-ERRCD
      D    DISPLAY "SODR-ERRCD=" SODR-ERRCD
      *
           MOVE    FLG-END         TO  SODR-RC
      *
           IF    ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               MOVE    TRRK-RRKNUM TO  SODR-RRKNUM
               MOVE    TRRK-RRKEDANUM
                                   TO  SODR-RRKEDANUM
               MOVE    TRRK-SHONUM TO  SODR-SHONUM
           END-IF
      *
           IF    ( FLG-END         NOT =   ZERO )
               MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
               MOVE    "abort"     TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理 (ZOGENPTN 1:Day/2:Month/3:Year)
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-EOMYMD-SEC                  SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-EOMYMD-EXT.
            EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY3)
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    TRRK-REC            TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY4)
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    TRRK-REC            TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY42)
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY42-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理(KEY17)
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SODR-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SODR-PTID           TO  NACCT-PTID
           MOVE    SODR-SRYYMD (1:6)   TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY58-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SODR-HOSPNUM        TO  NACCT-HOSPNUM
           MOVE    SODR-PTID           TO  NACCT-PTID
           IF    ( SODR-ORDERID        =   NORDERID-FOODID
                                        OR NORDERID-SAGAKUID )
               MOVE    "99999999"      TO  NACCT-UPYMD
           END-IF
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key58"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key58"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY58-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目検索処理(KEY)
      *****************************************************************
       900-ODRITEM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ODRITEM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    WRK-SYS-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-SYS-KBNCD       TO  SYS-KBNCD
           MOVE    SODR-SRYYMD         TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SODR-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SODR-PTID           TO  COMB-PTID
           MOVE    WRK-COMBI-HKNCOMBI  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理（チェックのみ）
      *****************************************************************
       990-LOCK-CHK-SEC                SECTION.
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    排他制御処理
           MOVE    "I01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SODR-PTID           TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  ERRCD-ORDER
           END-IF
      *
      *    排他制御解除処理
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
      *
           .
       990-LOCK-CHK-EXT.
           EXIT.
      *
