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
       PROGRAM-ID.                     ORCSODR11S01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集サブ（出産育児一時金情報登録）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/05/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
       01  IDX-AREA.
           03  IDX-ITM                 PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-ODRITEM-MAX       PIC 9(05)   VALUE 30.
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
      *
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "CPBDV2REQ.INC"     REPLACING  //BDREQ//
                                       BY         //LNKBD//.
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               XML-LNKBD
                                               TRRK-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
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
           INITIALIZE                  IDX-AREA
                                       WRK-AREA
                                       LNK-ODRITEM-REC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      D    DISPLAY "LNKBD-NYUINYMD      = " LNKBD-NYUINYMD
      D    DISPLAY "LNKBD-RIYOUKBN      = " LNKBD-RIYOUKBN
      D    DISPLAY "LNKBD-SKYYM         = " LNKBD-SKYYM
      D    DISPLAY "LNKBD-TEISYUTUBIKBN = " LNKBD-TEISYUTUBIKBN
      D    DISPLAY "LNKBD-SKYKBN        = " LNKBD-SKYKBN
      D    DISPLAY "LNKBD-BUNBENKBN     = " LNKBD-BUNBENKBN
      D    DISPLAY "LNKBD-TEISYUTUSAKI  = " LNKBD-TEISYUTUSAKI
      D    DISPLAY "LNKBD-ZAIWEEK       = " LNKBD-ZAIWEEK
      D    DISPLAY "LNKBD-BIRTHDAY      = " LNKBD-BIRTHDAY
      D    DISPLAY "LNKBD-SIZANKBN      = " LNKBD-SIZANKBN
      D    DISPLAY "LNKBD-BIRTHCNT      = " LNKBD-BIRTHCNT
      D    DISPLAY "LNKBD-SANKAKBN      = " LNKBD-SANKAKBN
      D    DISPLAY "LNKBD-BIKOU         = " LNKBD-BIKOU
      D    DISPLAY "LNKBD-HKNCOMBI      = " LNKBD-HKNCOMBI
      D    DISPLAY "LNKBD-HKNJANUM      = " LNKBD-HKNJANUM
      D    DISPLAY "LNKBD-KIGO          = " LNKBD-KIGO
      D    DISPLAY "LNKBD-NUM           = " LNKBD-NUM
      D    DISPLAY "LNKBD-HONKZKKBN     = " LNKBD-HONKZKKBN
      D    DISPLAY "LNKBD-NYUIN-DAY     = " LNKBD-NYUIN-DAY
      D    DISPLAY "LNKBD-NYU-MONEY     = " LNKBD-NYU-MONEY
      D    DISPLAY "LNKBD-RMSGK-MONEY   = " LNKBD-RMSGK-MONEY
      D    DISPLAY "LNKBD-KAIJO-MONEY   = " LNKBD-KAIJO-MONEY
      D    DISPLAY "LNKBD-BUNBEN-MONEY  = " LNKBD-BUNBEN-MONEY
      D    DISPLAY "LNKBD-SHIN-MONEY    = " LNKBD-SHIN-MONEY
      D    DISPLAY "LNKBD-KNS-MONEY     = " LNKBD-KNS-MONEY
      D    DISPLAY "LNKBD-SYC-MONEY     = " LNKBD-SYC-MONEY
      D    DISPLAY "LNKBD-SNK-MONEY     = " LNKBD-SNK-MONEY
      D    DISPLAY "LNKBD-ETC-MONEY     = " LNKBD-ETC-MONEY
      D    DISPLAY "LNKBD-ICBFTN-MONEY  = " LNKBD-ICBFTN-MONEY
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-NYUINYMD-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-RIYOUKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX    >  IDX-ITM )
               PERFORM 2300-SKYYM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX    >  IDX-ITM )
               PERFORM 2300-TEISYUTUBIKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-SKYKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
               PERFORM 2300-BUNBENKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-TEISYUTUSAKI-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-ZAIWEEK-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-BIRTHDAY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-SIZANKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-BIRTHCNT-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-SANKAKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-BIKOU-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-HKNCOMBI-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-HKNJANUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-KIGO-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-NUM-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX     >   IDX-ITM )
              PERFORM 2300-HONKZKKBN-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-NYUIN-DAY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-NYU-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-RMSGK-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-KAIJO-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-BUNBEN-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-SHIN-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-KNS-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-SYC-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-SNK-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-ETC-MONEY-SET-SEC
           END-IF
      *
           IF    ( CONST-ODRITEM-MAX   >   IDX-ITM )
               PERFORM 2300-ICBFTN-MONEY-SET-SEC
           END-IF
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院日設定処理
      *****************************************************************
       2300-NYUINYMD-SET-SEC           SECTION.
      *
          IF    ( LNKBD-NYUINYMD   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
              MOVE    NCLAIMID-NYUINYMDJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              IF    ( LNKBD-NYUINYMD (5:1) =   "-" )
                  MOVE    LNKBD-NYUINYMD
                                   TO  WRK-DATE
                  PERFORM 800-SYMD-SEC
                  MOVE    WRK-SYMD TO  LNK-ODRITEM-ITEM (IDX-ITM)
              ELSE
                  MOVE    LNKBD-NYUINYMD
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
              END-IF
      *
          END-IF
      *
           .
       2300-NYUINYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分設定処理
      *****************************************************************
       2300-RIYOUKBN-SET-SEC              SECTION.
      *
           IF  (   LNKBD-RIYOUKBN  NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-RIYOUKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-RIYOUKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SKYYM-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求年月設定処理
      *****************************************************************
       2300-SKYYM-SET-SEC              SECTION.
      *
          IF    (  LNKBD-SKYYM     NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-SKYYMJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              IF    ( LNKBD-SKYYM (5:1) =   "-" )
                  MOVE    LNKBD-SKYYM  TO  WRK-DATE
                  MOVE    1            TO  WRK-DATE-DD
                  PERFORM 800-SYMD-SEC
                  MOVE    WRK-SYMD (1:6) TO LNK-ODRITEM-ITEM (IDX-ITM)
              ELSE
                  MOVE    LNKBD-SKYYM  TO  LNK-ODRITEM-ITEM (IDX-ITM)
              END-IF
      *
          END-IF
      *
           .
       2300-SKYYM-SET-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分設定処理
      *****************************************************************
       2300-TEISYUTUBIKBN-SET-SEC              SECTION.
      *
          IF    ( LNKBD-TEISYUTUBIKBN  NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-TEISYUTUBIKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-TEISYUTUBIKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-TEISYUTUBIKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求区分設定処理
      *****************************************************************
       2300-SKYKBN-SET-SEC              SECTION.
      *
           IF    ( LNKBD-SKYKBN        NOT =   SPACE )
      *
               PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
               MOVE    NCLAIMID-SKYKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
               MOVE    LNKBD-SKYKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SKYKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分設定処理
      *****************************************************************
       2300-BUNBENKBN-SET-SEC              SECTION.
      *
           IF    ( LNKBD-BUNBENKBN     NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-BUNBENKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-BUNBENKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-BUNBENKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    提出先区分設定処理
      *****************************************************************
       2300-TEISYUTUSAKI-SET-SEC              SECTION.
      *
           IF    ( LNKBD-TEISYUTUSAKI  NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-TEISYUTUSAKIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-TEISYUTUSAKI
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-TEISYUTUSAKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    在胎週数設定処理
      *****************************************************************
       2300-ZAIWEEK-SET-SEC              SECTION.
      *
           IF    ( LNKBD-ZAIWEEK   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-ZAIWEEKJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-ZAIWEEK
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-ZAIWEEK-SET-EXT.
           EXIT.
      *****************************************************************
      *    出産年月日設定処理
      *****************************************************************
       2300-BIRTHDAY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-BIRTHDAY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-BIRTHDAYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              IF    ( LNKBD-BIRTHDAY (5:1) =   "-" )
                  MOVE    LNKBD-BIRTHDAY
                                   TO  WRK-DATE
                  PERFORM 800-SYMD-SEC
                  MOVE    WRK-SYMD TO  LNK-ODRITEM-ITEM (IDX-ITM)
              ELSE
                  MOVE    LNKBD-BIRTHDAY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
              END-IF
      *
          END-IF
      *
           .
       2300-BIRTHDAY-SET-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分設定処理
      *****************************************************************
       2300-SIZANKBN-SET-SEC              SECTION.
      *
           IF    ( LNKBD-SIZANKBN   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-SIZANKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-SIZANKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SIZANKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    出産数設定処理
      *****************************************************************
       2300-BIRTHCNT-SET-SEC              SECTION.
      *
           IF    ( LNKBD-BIRTHCNT   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-BIRTHCNTJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-BIRTHCNT
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-BIRTHCNT-SET-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度対象区分設定処理
      *****************************************************************
       2300-SANKAKBN-SET-SEC              SECTION.
      *
           IF    ( LNKBD-SANKAKBN  NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-SANKAKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-SANKAKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SANKAKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    備考設定処理
      *****************************************************************
       2300-BIKOU-SET-SEC              SECTION.
      *
           IF    ( LNKBD-BIKOU     NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-BIKOUJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-BIKOU  TO  LNK-ODRITEM-MEMO   (IDX-ITM)
      *
          END-IF
      *
           .
       2300-BIKOU-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       2300-HKNCOMBI-SET-SEC              SECTION.
      *
           IF    ( LNKBD-HKNCOMBI   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-HKNCOMBIJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-HKNCOMBI
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号設定処理
      *****************************************************************
       2300-HKNJANUM-SET-SEC              SECTION.
      *
           IF    ( LNKBD-HKNJANUM   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-HKNJANUMJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-HKNJANUM
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-HKNJANUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    被保険者証記号設定処理
      *****************************************************************
       2300-KIGO-SET-SEC              SECTION.
      *
           IF    ( LNKBD-KIGO   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-KIGOJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-KIGO   TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
          END-IF
      *
           .
       2300-KIGO-SET-EXT.
           EXIT.
      *****************************************************************
      *    被保険者証番号設定処理
      *****************************************************************
       2300-NUM-SET-SEC              SECTION.
      *
           IF    ( LNKBD-NUM   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-NUMJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-NUM    TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
          END-IF
      *
           .
       2300-NUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分設定処理
      *****************************************************************
       2300-HONKZKKBN-SET-SEC              SECTION.
      *
           IF    ( LNKBD-HONKZKKBN   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-HONKZKKBNJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-HONKZKKBN
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-HONKZKKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院日数設定処理
      *****************************************************************
       2300-NYUIN-DAY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-NYUIN-DAY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-NYUIN-DAYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-NYUIN-DAY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-NYUIN-DAY-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料設定処理
      *****************************************************************
       2300-NYU-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-NYU-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-NYU-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-NYU-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-NYU-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       2300-RMSGK-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-RMSGK-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-RMSGK-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-RMSGK-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-RMSGK-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩介助料設定処理
      *****************************************************************
       2300-KAIJO-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-KAIJO-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-KAIJO-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-KAIJO-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-KAIJO-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    分娩料設定処理
      *****************************************************************
       2300-BUNBEN-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-BUNBEN-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-BUNBEN-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-BUNBEN-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-BUNBEN-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    新生児管理保育料設定処理
      *****************************************************************
       2300-SHIN-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-SHIN-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-SHIN-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-SHIN-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SHIN-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    検査・薬剤料設定処理
      *****************************************************************
       2300-KNS-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-KNS-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-KNS-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-KNS-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-KNS-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    処置・手当料設定処理
      *****************************************************************
       2300-SYC-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-SYC-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-SYC-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-SYC-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SYC-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度掛金設定処理
      *****************************************************************
       2300-SNK-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-SNK-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-SNK-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-SNK-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-SNK-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    その他設定処理
      *****************************************************************
       2300-ETC-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-ETC-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-ETC-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-ETC-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-ETC-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金等設定処理
      *****************************************************************
       2300-ICBFTN-MONEY-SET-SEC              SECTION.
      *
           IF    ( LNKBD-ICBFTN-MONEY   NOT =   SPACE )
      *
              PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
              MOVE    NCLAIMID-ICBFTN-MONEYJMA
                                   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
              MOVE    LNKBD-ICBFTN-MONEY
                                   TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
          END-IF
      *
           .
       2300-ICBFTN-MONEY-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-ODRITEM-COMMON-SET-SEC       SECTION.
      *
           COMPUTE    IDX-ITM  =   IDX-ITM +   1
      *
           MOVE    SPA-HOSPNUM     TO  LNK-ODRITEM-HOSPNUM  (IDX-ITM)
           MOVE    SODR-KARTE-KEY  TO  LNK-ODRITEM-KARTE-KEY(IDX-ITM)
           MOVE    1               TO  LNK-ODRITEM-ORDERNUM(IDX-ITM)
           MOVE    IDX-ITM         TO  LNK-ODRITEM-ITEMNUM (IDX-ITM)
      *
           .
       2300-ODRITEM-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
