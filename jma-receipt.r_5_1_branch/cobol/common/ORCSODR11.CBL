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
       PROGRAM-ID.                     ORCSODR11.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集（出産育児一時金情報登録）
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-HIZUKE-ERR          PIC 9(01).
           03  FLG-ZENKAKU-ERR         PIC 9(01).
           03  FLG-MONEY-ERR           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXI                    PIC 9(05).
           03  IDX-ODR                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ITEMID              PIC X(05).
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
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-SYS-KANRICD         PIC X(04).
           03  WRK-SYS-KBNCD           PIC X(08).
           03  WRK-DIALOG              PIC 9(02).
           03  WRK-MCP-WINDOW          PIC X(64).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-KANACHK-STR         PIC X(100).
           03  WRK-KANACHK-MAX         PIC 9(05).
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-MONEY               PIC X(10).
           03  WRK-NUM-MONEY           PIC 9(07).
           03  WRK-SKYYMD.
               05  WRK-SKYYM           PIC X(06).
               05  WRK-SKYDD           PIC X(02).
           03  WRK-CREYMD              PIC X(08).
           03  WRK-BASE-MONEY          PIC S9(08).
      *
       01  ITEM-AREA.
           03  ITEM-HKNNUM             PIC X(03).
           03  ITEM-KANYUKBN           PIC X(01).
           03  ITEM-SRYYMD.
               05  ITEM-SRYYM          PIC X(06).
               05  ITEM-SRYDD          PIC 9(02).
           03  ITEM-NUM-ZAIWEEK        PIC 9(02).
           03  ITEM-NUM-BIRTHCNT       PIC 9(01).
           03  ITEM-NUM-NYUIN-DAY      PIC 9(03).
           03  ITEM-NUM-NYU-MONEY      PIC 9(10).
           03  ITEM-NUM-RMSGK-MONEY    PIC 9(10).
           03  ITEM-NUM-KAIJO-MONEY    PIC 9(10).
           03  ITEM-NUM-BUNBEN-MONEY   PIC 9(10).
           03  ITEM-NUM-SHIN-MONEY     PIC 9(10).
           03  ITEM-NUM-KNS-MONEY      PIC 9(10).
           03  ITEM-NUM-SYC-MONEY      PIC 9(10).
           03  ITEM-NUM-SNK-MONEY      PIC 9(10).
           03  ITEM-NUM-ETC-MONEY      PIC 9(10).
           03  ITEM-NUM-ICBFTN-MONEY   PIC 9(10).
           03  ITEM-NUM-NINPUFTN-MONEY PIC 9(10).
           03  ITEM-NUM-DAIRI-MONEY    PIC 9(10).
      *
       01  ERRCD-AREA.
           03  ERRCD-ITEM-G.
               05  ERRCD-ITEM          PIC X(04).
               05  ERRCD-ORDER         PIC X(04).
           03  ERRCD-ERRCD             PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-H270101           PIC X(08)   VALUE "20150101".
           03  CONST-H2904             PIC X(08)   VALUE "201704".
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
      *    分娩管理退避
       01  TBBN-REC.
           COPY    "CPBUNBEN.INC"      REPLACING  //BUNBEN-//
                                       BY         //TBBN-//.
      *
           COPY    "CPBDV2REQ.INC"     REPLACING  //BDREQ//
                                       BY         //ITEM//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
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
      *    分娩負担金計算サブ
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"    REPLACING  //BUNBEN-//
                                   BY       //LNKBUNBEN-//.
      *
      *    オーダー共通サブ
           COPY    "CPORCSODRS01.INC".
      *
      *    オーダー保険情報チェックサブ
           COPY    "CPORCSODR11S02.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ERRCD-AREA
           INITIALIZE                  SODR-OT
           INITIALIZE                  TRRK-REC
           INITIALIZE                  TBBN-REC
           INITIALIZE                  XML-ITEM
           INITIALIZE                  ITEM-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF    ( SODR-SRCKBN     =    "1" OR "2" )
               PERFORM 990-LOCK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G    =   SPACE )
               PERFORM 1001-NYUINYMD-CHK-SEC
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
               MOVE    ZERO        TO  SYS-1001-SANKANUM
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       1001-NYUINYMD-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX-ODR         FROM    1   BY  1
                   UNTIL ( IDX-ODR         >   LNK-ODRITEM-MAX )
                    OR   ( ITEM-NYUINYMD  NOT =   SPACE )
                    OR   ( ERRCD-ITEM-G    NOT =   SPACE )
      *
               IF  (   LNK-ODRITEM-ITEMID (IDX-ODR)
                                           =   NCLAIMID-NYUINYMDJMA )
                   PERFORM 10011-NYUINYMD-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       1001-NYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       10011-NYUINYMD-CHK-SEC          SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-NYUINYMD
      *
           IF    ( ITEM-NYUINYMD  =   SPACE )
               MOVE    "8001"      TO  ERRCD-ORDER
           ELSE
               MOVE    ITEM-NYUINYMD
                                   TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1    =   SPACE )
                   MOVE    "8001"     TO  ERRCD-ORDER
               END-IF
      *
           END-IF
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
      *
               PERFORM 900-PTNYUINRRK-KEY39-SEL-SEC
               IF    ( FLG-PTNYUINRRK      =   ZERO )
      *
                   MOVE    PTNYUINRRK-NYUINYMD
                                           TO  WRK-SYMD
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    1               TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-SYMD        TO  WRK-SRYYMD
      *
                   MOVE    PTNYUINRRK-REC  TO  TRRK-REC
      *
                   IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                    AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
                    AND  ( PTNYUINRRK-NYUINYMD =   ITEM-NYUINYMD )
                       CONTINUE
                   ELSE
                       IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                        AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                           PERFORM 900-PTNYUINRRK-KEY20-SEL-SEC
                           IF    ( FLG-PTNYUINRRK      =   ZERO )
                            AND  ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                            AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
                            AND  ( PTNYUINRRK-NYUINYMD =   WRK-SRYYMD )
                               MOVE    PTNYUINRRK-REC  TO  TRRK-REC
                           ELSE
                               MOVE    "8005"  TO  ERRCD-ORDER
                           END-IF
                       ELSE
                           MOVE    "8005"  TO  ERRCD-ORDER
                       END-IF
                   END-IF
               ELSE
                       MOVE    "8005"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
               IF    ( FLG-BUNBEN  =   ZERO )
                   MOVE    BUNBEN-REC  TO  TBBN-REC
               ELSE
                   MOVE    "8916"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
               PERFORM 800-ORCSBUNBENFTN-SEC
           END-IF
      *
           .
       10011-NYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
      *
               IF    ( FLG-END  =   ZERO )
                  PERFORM 2101-BUNBEN-INSERT-SEC
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
           MOVE    NCLAIMID-HKNCOMBIJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( ITEM-HKNCOMBI   =   SPACE )
      *
               MOVE    NCLAIMID-HKNJANUMJMA
                                       TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-HKNJANUM-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-KIGOJMA
                                       TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-KIGO-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-NUMJMA TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-NUM-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-HONKZKKBNJMA
                                       TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-HONKZKKBN-CHK-SEC
               END-IF
      *
           END-IF
      *
           MOVE    NCLAIMID-RIYOUKBNJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-RIYOUKBN-CHK-SEC
           END-IF
      *
           IF    ( ITEM-RIYOUKBN       =   "1" )
            AND  ( TRRK-TAIINYMD   NOT =   "99999999" )
      *
               MOVE    NCLAIMID-SKYYMJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-SKYYM-CHK-SEC
               END-IF

               MOVE    NCLAIMID-TEISYUTUBIKBNJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-TEISYUTUBIKBN-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-SKYKBNJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-SKYKBN-CHK-SEC
               END-IF
           END-IF
      *
           MOVE    NCLAIMID-BUNBENKBNJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-BUNBENKBN-CHK-SEC
           END-IF
      *
           IF    ( ITEM-RIYOUKBN       =   "1" )
               MOVE    NCLAIMID-TEISYUTUSAKIJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-TEISYUTUSAKI-CHK-SEC
               END-IF
           END-IF
      *
           MOVE    NCLAIMID-ZAIWEEKJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-ZAIWEEK-CHK-SEC
           END-IF
      *
           MOVE    NCLAIMID-BIRTHDAYJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-BIRTHDAY-CHK-SEC
           END-IF
      *
           MOVE    NCLAIMID-SIZANKBNJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-SIZANKBN-CHK-SEC
           END-IF
      *
           MOVE    NCLAIMID-BIRTHCNTJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-BIRTHCNT-CHK-SEC
           END-IF
      *
           MOVE    NCLAIMID-SANKAKBNJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-SANKAKBN-CHK-SEC
           END-IF
      *
           MOVE    NCLAIMID-BIKOUJMA
                                   TO  WRK-ITEMID
           PERFORM 20011-ITEMID-CHK-SEC
           IF    ( IDX-ODR         >   ZERO )
               PERFORM 20011-BIKOU-CHK-SEC
           END-IF
      *
           IF    ( TRRK-TAIINYMD   NOT =   "99999999" )
               MOVE    NCLAIMID-NYUIN-DAYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-NYUIN-DAY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-NYU-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-NYU-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-RMSGK-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-RMSGK-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-KAIJO-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-KAIJO-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-BUNBEN-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-BUNBEN-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-SHIN-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-SHIN-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-KNS-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-KNS-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-SYC-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-SYC-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-SNK-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-SNK-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-ETC-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-ETC-MONEY-CHK-SEC
               END-IF
      *
               MOVE    NCLAIMID-ICBFTN-MONEYJMA
                                   TO  WRK-ITEMID
               PERFORM 20011-ITEMID-CHK-SEC
               IF    ( IDX-ODR         >   ZERO )
                   PERFORM 20011-ICBFTN-MONEY-CHK-SEC
               END-IF
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    項目ＩＤチェック処理
      *****************************************************************
       20011-ITEMID-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                 TO  IDX-ODR
      *
           PERFORM VARYING IDXI         FROM    1   BY  1
                   UNTIL ( IDXI         >   LNK-ODRITEM-MAX )
                    OR   ( IDX-ODR      >   ZERO )
      *
               IF    ( LNK-ODRITEM-ITEMID (IDXI)
                                       =   WRK-ITEMID )
                   MOVE    IDXI        TO  IDX-ODR
               END-IF
      *
           END-PERFORM
      *
           .
       20011-ITEMID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック処理
      *****************************************************************
       20011-RIYOUKBN-CHK-SEC              SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-RIYOUKBN
      *
           IF    ( ITEM-RIYOUKBN       =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-RIYOUKBN   =   "0" OR "1" )
                   CONTINUE
               ELSE
                   MOVE    "0163"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-RIYOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求年月チェック処理
      *****************************************************************
       20011-SKYYM-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SKYYM
      *
           IF    ( ITEM-SKYYM          =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-SKYYM      TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0175"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SKYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分チェック処理
      *****************************************************************
       20011-TEISYUTUBIKBN-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-TEISYUTUBIKBN
      *
           IF    ( ITEM-TEISYUTUBIKBN  =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-TEISYUTUBIKBN  =   "1" OR "2" )
                   CONTINUE
               ELSE
                   MOVE    "0176"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-TEISYUTUBIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求区分チェック処理
      *****************************************************************
       20011-SKYKBN-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SKYKBN
      *
           IF    ( ITEM-SKYKBN    =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-SKYKBN    =    "0" OR "1" OR "2" )
                   CONTINUE
               ELSE
                   MOVE    "0177"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SKYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック処理
      *****************************************************************
       20011-BUNBENKBN-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-BUNBENKBN
      *
           IF    ( ITEM-BUNBENKBN =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-BUNBENKBN =  "1" OR "2" )
                   CONTINUE
               ELSE
                   MOVE    "0162"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-BUNBENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出先区分チェック処理
      *****************************************************************
       20011-TEISYUTUSAKI-CHK-SEC          SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-TEISYUTUSAKI
      *
           IF    ( ITEM-TEISYUTUSAKI       =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-TEISYUTUSAKI   =  "1" OR "2" )
                   CONTINUE
               ELSE
                   MOVE    "0178"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-TEISYUTUSAKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    在胎週数チェック処理処理
      *****************************************************************
       20011-ZAIWEEK-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-ZAIWEEK
      *
           IF    ( ITEM-ZAIWEEK =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-ZAIWEEK    TO  WRK-SNUM-INX
               PERFORM 800-ORCSNUM-SEC
               IF    ( FLG-ORCSNUM     =   ZERO )
                AND  ( WRK-SNUM-OUTNUM <   100 )
                   IF    ( WRK-SNUM-OUTNUM >=  12 )
                       MOVE    WRK-SNUM-OUTNUM
                                       TO  ITEM-NUM-ZAIWEEK
                   ELSE
                       MOVE    "0179"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
               ELSE
                   MOVE    "0180"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-ZAIWEEK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出産年月日チェック処理
      *****************************************************************
       20011-BIRTHDAY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-BIRTHDAY
      *
           IF    ( ITEM-BIRTHDAY  =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-BIRTHDAY   TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0181"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-BIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分チェック処理
      *****************************************************************
       20011-SIZANKBN-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SIZANKBN
      *
           IF    ( ITEM-SIZANKBN  =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-SIZANKBN  =    "1" OR "2" OR "3" )
                   CONTINUE
               ELSE
                   MOVE    "0182"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SIZANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出産数チェック処理
      *****************************************************************
       20011-BIRTHCNT-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-BIRTHCNT
      *
           IF    ( ITEM-BIRTHCNT  =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-BIRTHCNT
                                  TO  WRK-SNUM-INX
               PERFORM 800-ORCSNUM-SEC
               IF    ( FLG-ORCSNUM     =   ZERO )
                AND  ( WRK-SNUM-OUTNUM <   10 )
                AND  ( WRK-SNUM-OUTNUM >=  ZERO )
                   MOVE    WRK-SNUM-OUTNUM
                                       TO  ITEM-NUM-BIRTHCNT
               ELSE
                   MOVE    "0183"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-BIRTHCNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度対象区分チェック処理
      *****************************************************************
       20011-SANKAKBN-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SANKAKBN
      *
           IF    ( ITEM-SANKAKBN  =   SPACE )
               CONTINUE
           ELSE
               IF    ( ITEM-SANKAKBN  =    "1" OR "2" OR "3" )
                   CONTINUE
               ELSE
                   MOVE    "0184"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SANKAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    備考チェック処理
      *****************************************************************
       20011-BIKOU-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                  TO  ITEM-BIKOU
      *
           IF    ( ITEM-BIKOU     =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-BIKOU TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR
                                  TO  ITEM-BIKOU
               IF    ( FLG-ZENKAKU-ERR NOT =   ZERO )
                   MOVE    "0185"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-BIKOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-HKNCOMBI
      *
           IF    ( ITEM-HKNCOMBI   =   SPACE )
               CONTINUE
           ELSE
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI   =   ZERO )
                   IF    ( COMB-HKNNUM    >=   "001" )
                     AND ( COMB-HKNNUM    <=   "099" )
                     AND ( COMB-HKNNUM NOT =   "039" )
                     AND ( COMB-HKNNUM NOT =   "040" )
                       PERFORM 900-PTHKNINF-KEY-SEL-SEC
                       MOVE    PTHKN-HKNJANUM  TO  ITEM-HKNJANUM
                       MOVE    PTHKN-KIGO      TO  ITEM-KIGO
                       MOVE    PTHKN-NUM       TO  ITEM-NUM
                       IF    ( PTHKN-HONKZKKBN =   "2" )
                           MOVE    "5"         TO  ITEM-HONKZKKBN
                       ELSE
                           MOVE    "1"         TO  ITEM-HONKZKKBN
                       END-IF
                       IF    ( COMB-HKNNUM     =   "060"
                                                OR "067"
                                                OR "068"
                                                OR "069" )
                           MOVE    "2"         TO  ITEM-KANYUKBN
                       ELSE
                           MOVE    "1"         TO  ITEM-KANYUKBN
                       END-IF
                   ELSE
                       MOVE    "0164"          TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
               ELSE
                   MOVE    "0005"     TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       20011-HKNJANUM-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-HKNJANUM
      *
           IF    ( ITEM-HKNJANUM   =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE              SODR11S02-AREA
               MOVE    ITEM-HKNJANUM
                                   TO  SODR11S02-HKNJANUM
               PERFORM 800-ORCSODR11S02-SEC
               IF    ( SODR11S02-ERRCD NOT =    SPACE )
                   MOVE    SODR11S02-ERRCD
                                   TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               ELSE
                   MOVE    SODR11S02-HKNNUM
                                   TO  ITEM-HKNNUM
                   MOVE    SODR11S02-KANYUKBN
                                   TO  ITEM-KANYUKBN
               END-IF
      *
           END-IF
      *
           .
       20011-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者証記号チェック処理
      *****************************************************************
       20011-KIGO-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                  TO  ITEM-KIGO
      *
           IF    ( ITEM-KIGO      =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE              SODR11S02-AREA
               MOVE    ITEM-KIGO  TO   SODR11S02-KIGO
               PERFORM 800-ORCSODR11S02-SEC
               IF    ( SODR11S02-ERRCD NOT =    SPACE )
                   MOVE    SODR11S02-ERRCD
                                   TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-KIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者証番号チェック処理
      *****************************************************************
       20011-NUM-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                   TO  ITEM-NUM
      *
           IF    ( ITEM-NUM        =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE              SODR11S02-AREA
               MOVE    ITEM-NUM    TO  SODR11S02-NUM
               PERFORM 800-ORCSODR11S02-SEC
               IF    ( SODR11S02-ERRCD NOT =    SPACE )
                   MOVE    SODR11S02-ERRCD
                                   TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分チェック処理
      *****************************************************************
       20011-HONKZKKBN-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-HONKZKKBN
      *
           IF    ( ITEM-HONKZKKBN =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE              SODR11S02-AREA
               MOVE    ITEM-HONKZKKBN
                                   TO  SODR11S02-HONKZKKBN
               PERFORM 800-ORCSODR11S02-SEC
               IF    ( SODR11S02-ERRCD NOT =    SPACE )
                   MOVE    SODR11S02-ERRCD
                                   TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-HONKZKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日数チェック処理
      *****************************************************************
       20011-NYUIN-DAY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-NYUIN-DAY
      *
           IF    ( ITEM-NYUIN-DAY =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-NYUIN-DAY
                                  TO  WRK-SNUM-INX
               PERFORM 800-ORCSNUM-SEC
               IF    ( FLG-ORCSNUM     =   ZERO )
                AND  ( WRK-SNUM-OUTNUM <   1000 )
                AND  ( WRK-SNUM-OUTNUM >=  ZERO )
                   MOVE    WRK-SNUM-OUTNUM
                                       TO  ITEM-NUM-NYUIN-DAY
               ELSE
                   MOVE    "0165"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-NYUIN-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       20011-NYU-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-NYU-MONEY
      *
           IF    ( ITEM-NYU-MONEY =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-NYU-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-NYU-MONEY
               ELSE
                   MOVE    "0166" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-NYU-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       20011-RMSGK-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-RMSGK-MONEY
      *
           IF    ( ITEM-RMSGK-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-RMSGK-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-RMSGK-MONEY
               ELSE
                   MOVE    "0024" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-RMSGK-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩介助料チェック処理
      *****************************************************************
       20011-KAIJO-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-KAIJO-MONEY
      *
           IF    ( ITEM-KAIJO-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-KAIJO-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-MONEY
                                  TO  ITEM-NUM-KAIJO-MONEY
               ELSE
                   MOVE    "0167"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-KAIJO-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩料チェック処理
      *****************************************************************
       20011-BUNBEN-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-BUNBEN-MONEY
      *
           IF    ( ITEM-BUNBEN-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-BUNBEN-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-BUNBEN-MONEY
               ELSE
                   MOVE    "0168" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-BUNBEN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    新生児管理保育料チェック処理
      *****************************************************************
       20011-SHIN-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SHIN-MONEY
      *
           IF    ( ITEM-SHIN-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-SHIN-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-SHIN-MONEY
               ELSE
                   MOVE    "0169" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SHIN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査・薬剤料チェック処理
      *****************************************************************
       20011-KNS-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-KNS-MONEY
      *
           IF    ( ITEM-KNS-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-KNS-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-KNS-MONEY
               ELSE
                   MOVE    "0170" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-KNS-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処置・手当料チェック処理
      *****************************************************************
       20011-SYC-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SYC-MONEY
      *
           IF    ( ITEM-SYC-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-SYC-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-SYC-MONEY
               ELSE
                   MOVE    "0171" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SYC-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度掛金チェック処理
      *****************************************************************
       20011-SNK-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-SNK-MONEY
      *
           IF    ( ITEM-SNK-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-SNK-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-SNK-MONEY
               ELSE
                   MOVE    "0172" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-SNK-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他チェック処理
      *****************************************************************
       20011-ETC-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-ETC-MONEY
      *
           IF    ( ITEM-ETC-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-ETC-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-ETC-MONEY
               ELSE
                   MOVE    "0173" TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-ETC-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金等チェック処理
      *****************************************************************
       20011-ICBFTN-MONEY-CHK-SEC             SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                  TO  ITEM-ICBFTN-MONEY
      *
           IF    ( ITEM-ICBFTN-MONEY      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ITEM-ICBFTN-MONEY
                                  TO  WRK-MONEY
               PERFORM 20011-MONEY-COMMON-CHK-SEC
               IF    ( FLG-MONEY-ERR   =   ZERO )
                   MOVE    WRK-NUM-MONEY
                                  TO  ITEM-NUM-ICBFTN-MONEY
               ELSE
                   MOVE    "0174"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-ICBFTN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額共通チェック処理
      *****************************************************************
       20011-MONEY-COMMON-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-MONEY-ERR
      *
           MOVE    WRK-MONEY           TO  WRK-SNUM-INX
           PERFORM 800-ORCSNUM-SEC
           IF    ( FLG-ORCSNUM     =   ZERO )
            AND  ( WRK-SNUM-OUTNUM <   10000000 )
            AND  ( WRK-SNUM-OUTNUM >=  ZERO )
               MOVE    WRK-MONEY       TO  WRK-NUM-MONEY
           ELSE
               MOVE    1               TO  FLG-MONEY-ERR
           END-IF
      *
           .
       20011-MONEY-COMMON-CHK-EXT.
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
           INITIALIZE                      SODR11S02-AREA
           MOVE    "1"                 TO  SODR11S02-KANREN
           MOVE    ITEM-HKNJANUM       TO  SODR11S02-HKNJANUM
           MOVE    ITEM-HONKZKKBN      TO  SODR11S02-HONKZKKBN
           MOVE    ITEM-KIGO           TO  SODR11S02-KIGO
           MOVE    ITEM-NUM            TO  SODR11S02-NUM
           PERFORM 800-ORCSODR11S02-SEC
           IF    ( SODR11S02-ERRCD     NOT =   SPACE )
               MOVE    SODR11S02-ERRCD TO  ERRCD-ORDER
           END-IF
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( ITEM-RIYOUKBN   =   SPACE )
                   MOVE    "8025"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER           =   SPACE )
               IF    ( ITEM-BUNBENKBN    =   SPACE )
                   MOVE    "8029"        TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ITEM-RIYOUKBN        =   "1" )
            AND  ( TRRK-TAIINYMD    NOT =    "99999999" )
      *
               IF    ( ERRCD-ORDER      =   SPACE )
                   IF    ( ITEM-SKYYM   =   SPACE )
                       MOVE    "8026"   TO  ERRCD-ORDER
                   END-IF
               END-IF
      *
               IF    ( ERRCD-ORDER      =   SPACE )
                   IF    ( ITEM-TEISYUTUBIKBN
                                        =   SPACE )
                       MOVE    "8027"   TO  ERRCD-ORDER
                   END-IF
               END-IF
      *
               IF    ( ERRCD-ORDER      =   SPACE )
                   IF    ( ITEM-SKYKBN
                                        =   SPACE )
                       MOVE    "8028"   TO  ERRCD-ORDER
                   END-IF
               END-IF
      *
               IF    ( ERRCD-ORDER             =   SPACE )
                   IF    ( ITEM-BUNBENKBN      =   "2" )
                    AND  ( ITEM-TEISYUTUBIKBN  =   "2" )
                       MOVE    "8046"   TO  ERRCD-ORDER
                   END-IF
               END-IF
      *
               IF    ( ERRCD-ORDER      =   SPACE )
                   IF    ( ITEM-BUNBENKBN   =   "1" )
                       MOVE    ITEM-SKYYM
                                       TO  WRK-SKYYM
                       IF    ( ITEM-TEISYUTUBIKBN   =   "1" )
                           MOVE    "10"    TO  WRK-SKYDD
                       ELSE
                           MOVE    "25"    TO  WRK-SKYDD
                       END-IF
                       IF    ( WRK-SKYYMD  <   TRRK-TAIINYMD )
                           MOVE    "8015"  TO  ERRCD-ORDER
                       END-IF
                   ELSE
                       IF    ( ITEM-SKYYM  <=  TRRK-TAIINYMD (1:6) )
                           MOVE    "8016"  TO  ERRCD-ORDER
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER             =   SPACE )
               IF    ( ITEM-RIYOUKBN       =   "1" )
                AND  ( ITEM-TEISYUTUSAKI   =   SPACE )
                   IF    ( ITEM-SKYYM  NOT =   SPACE )
                       IF    ( ITEM-SKYYM           >=   CONST-H2904 )
                        OR   ( ITEM-NYUINYMD        >=   CONST-H2904 )
                        OR   ( TRRK-TAIINYMD        >=   CONST-H2904 )
                           EVALUATE    ITEM-KANYUKBN
                           WHEN    "1"
                               MOVE    "1"     TO  ITEM-TEISYUTUSAKI
                           WHEN    "2"
                               MOVE    "2"     TO  ITEM-TEISYUTUSAKI
                           END-EVALUATE
                       ELSE
                           MOVE    "1"         TO  ITEM-TEISYUTUSAKI
                           IF    ( ITEM-BUNBENKBN       =   "1" )
                               MOVE    "2"     TO  ITEM-TEISYUTUSAKI
                           ELSE
                               MOVE    LNKBUNBEN-KANYUKBN
                                               TO  ITEM-TEISYUTUSAKI
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER             =   SPACE )
               IF    ( ITEM-RIYOUKBN       =   "1" )
                   IF    ( ITEM-SKYYM  NOT =   SPACE )
                       PERFORM 20021-TEISYUTUSAKI-CHK-SEC
                   END-IF
               END-IF
            END-IF
      *
           IF    ( ERRCD-ORDER           =   SPACE )
               IF    ( ITEM-NUM-ZAIWEEK  =   ZERO )
                   MOVE    "8044"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( ITEM-BIRTHDAY   =   SPACE )
                   MOVE    "8022"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( ITEM-SIZANKBN   =   SPACE )
                   MOVE    "8023"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER           =   SPACE )
               IF    ( ITEM-NUM-BIRTHCNT =   ZERO )
                   MOVE    "8024"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( TRRK-TAIINYMD   NOT =    "99999999" )
                AND  ( ITEM-NUM-NYUIN-DAY =  ZERO )
                   MOVE    "8045"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( ITEM-SANKAKBN   =   SPACE )
                   IF    ( SYS-1001-SANKANUM    =   ZERO )
                       MOVE    "2"     TO  ITEM-SANKAKBN
                   ELSE
                       MOVE    "8038"  TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER                 =   SPACE )
               IF    ( ITEM-BUNBENKBN          =   "1" )
                AND  ( ITEM-NUM-KAIJO-MONEY    >   ZERO )
                   MOVE    "8030"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER                 =   SPACE )
               IF    ( ITEM-BUNBENKBN          =   "2" )
                AND  ( ITEM-NUM-BUNBEN-MONEY   >   ZERO )
                   MOVE    "8031"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ITEM-BIRTHCNT    =   1 )
               IF    ( ERRCD-ORDER           =   SPACE )
                   IF    ( ITEM-SIZANKBN    =   "3" )
                       MOVE    "8036"      TO  ERRCD-ORDER
                   END-IF
               END-IF
               IF    ( ERRCD-ORDER           =   SPACE )
                   IF    ( ITEM-SANKAKBN    =   "3" )
                       MOVE    "8037"      TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER               =   SPACE )
               IF    ( ITEM-SANKAKBN    =   "3" )
                AND  ( SYS-1001-SANKANUM    =   ZERO )
                   MOVE    "8040"          TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER           =   SPACE )
               IF    ( ITEM-SANKAKBN         =   "1" )
                   IF    ( ITEM-ZAIWEEK     >=   22 )
                    AND  ( SYS-1001-SANKANUM NOT =   ZERO )
                       CONTINUE
                   ELSE
                       IF    ( SYS-1001-SANKANUM =   ZERO )
                           MOVE    "8040"  TO  ERRCD-ORDER
                       END-IF
                       IF    ( ERRCD-ORDER        =   SPACE )
                           IF    ( ITEM-ZAIWEEK >=  22 )
                               CONTINUE
                           ELSE
                               MOVE    "8041"  TO  ERRCD-ORDER
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER              =   SPACE )
               IF    ( ITEM-SANKAKBN        =   "1" OR "3" )
                   CONTINUE
               ELSE
                   IF    ( ITEM-SNK-MONEY   >   ZERO )
                       MOVE    "8039"      TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           COMPUTE ITEM-NUM-NINPUFTN-MONEY
               =   ITEM-NUM-NYU-MONEY
               +   ITEM-NUM-RMSGK-MONEY
               +   ITEM-NUM-KAIJO-MONEY
               +   ITEM-NUM-BUNBEN-MONEY
               +   ITEM-NUM-SHIN-MONEY
               +   ITEM-NUM-KNS-MONEY
               +   ITEM-NUM-SYC-MONEY
               +   ITEM-NUM-SNK-MONEY
               +   ITEM-NUM-ETC-MONEY
               +   ITEM-NUM-ICBFTN-MONEY
      *
           IF    ( ITEM-NUM-NINPUFTN-MONEY >=   10000000 )
               MOVE    "8017"          TO  ERRCD-ORDER
           END-IF
      *
           MOVE    ZERO                TO  WRK-BASE-MONEY
           IF    ( ITEM-RIYOUKBN        =   "1" )
      *
               EVALUATE    TRUE
               WHEN  ( SYS-1001-SANKANUM    =   ZERO )
               WHEN  ( ITEM-NUM-ZAIWEEK     <   22 )
               WHEN  ( ITEM-SANKAKBN    =   "2" OR "3" )
                   IF    ( ITEM-BIRTHDAY   NOT =   SPACE )
                       EVALUATE   TRUE
                       WHEN    ( ITEM-BIRTHDAY
                                          >= CONST-H270101 )
                           MOVE    404000  TO  WRK-BASE-MONEY
                       WHEN    OTHER
                           MOVE    390000  TO  WRK-BASE-MONEY
                       END-EVALUATE
                   END-IF
               WHEN    OTHER
                   MOVE    420000      TO  WRK-BASE-MONEY
               END-EVALUATE
      *
               IF    ( ITEM-SANKAKBN    =   "3" )
                   COMPUTE ITEM-NUM-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   ITEM-NUM-BIRTHCNT
                       +   ITEM-NUM-SNK-MONEY
               ELSE
                   COMPUTE ITEM-NUM-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   ITEM-NUM-BIRTHCNT
               END-IF
      *
           END-IF
      *
           IF  ( ITEM-NUM-DAIRI-MONEY >   ZERO )
            AND( ITEM-NUM-DAIRI-MONEY >   ITEM-NUM-NINPUFTN-MONEY )
               MOVE    ITEM-NUM-NINPUFTN-MONEY
                                   TO  ITEM-NUM-DAIRI-MONEY
           END-IF
      *
           IF    ( ERRCD-ORDER           =   SPACE )
      *
               PERFORM 900-BUNBEN-KEY5-SEL-SEC
      *
               PERFORM UNTIL ( FLG-BUNBEN      NOT =   ZERO )
                         OR  ( ERRCD-ORDER     NOT =   SPACE )
      *
                   IF    ( TBBN-HOSPNUM     =   BUNBEN-HOSPNUM )
                    AND  ( TBBN-PTID        =   BUNBEN-PTID )
                    AND  ( TBBN-NYUGAIKBN   =   BUNBEN-NYUGAIKBN )
                    AND  ( TBBN-RRKNUM      =   BUNBEN-RRKNUM )
                       CONTINUE
                   ELSE
                       MOVE    "8043"  TO  ERRCD-ORDER
                   END-IF
      *
                   PERFORM 900-BUNBEN-KEY5-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
           
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（提出先チェック）
      *****************************************************************
       20021-TEISYUTUSAKI-CHK-SEC              SECTION.
      *
           IF    ( ITEM-SKYYM         >=   CONST-H2904 )
               EVALUATE    ITEM-KANYUKBN
               WHEN    "1"
                   IF    ( ITEM-TEISYUTUSAKI    =   "2" )
                       MOVE    "8034"  TO  ERRCD-ORDER
                   END-IF
               WHEN    "2"
                   IF    ( ITEM-TEISYUTUSAKI    =   "1" )
                       MOVE    "8035"  TO  ERRCD-ORDER
                   END-IF
               END-EVALUATE
           ELSE
      *
               IF    ( ERRCD-ORDER           =   SPACE )
                   IF    ( ITEM-BUNBENKBN        = "1" )
                       IF    ( ITEM-TEISYUTUSAKI = "1" )
                           MOVE    "8033"      TO  ERRCD-ORDER
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( ERRCD-ORDER           =   SPACE )
                   IF    ( ITEM-BUNBENKBN        = "2" )
                    AND  ( LNKBUNBEN-KANYUKBN    = "1" )
                    AND  ( ITEM-KANYUKBN         = "1" )
                       IF    ( ITEM-TEISYUTUSAKI = "2" )
                           MOVE    "8034"      TO  ERRCD-ORDER
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( ERRCD-ORDER             =   SPACE )
                   IF    ( ITEM-BUNBENKBN        = "2" )
                    AND  ( LNKBUNBEN-KANYUKBN    = "2" )
                    AND  ( ITEM-KANYUKBN         = "2" )
                       IF    ( ITEM-TEISYUTUSAKI = "1" )
                           MOVE    "8035"  TO  ERRCD-ORDER
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( ERRCD-ORDER           =   SPACE )
               IF    ( ITEM-TEISYUTUSAKI   =   SPACE )
                   MOVE    "8032"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           .
       20021-TEISYUTUSAKI-CHK-EXT.
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
           MOVE   SODR-HOSPNUM         TO  ORDER-HOSPNUM
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
               DISPLAY "ORCSDR11 ERROR 001:ORDER INSERT CHK ERR"
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
               DISPLAY "ORCSDR11 ERROR 002:ORDER INSERT ERR"
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
                                       =   NCLAIMID-NYUINYMDJMA
                                        OR NCLAIMID-RIYOUKBNJMA
                                        OR NCLAIMID-SKYYMJMA
                                        OR NCLAIMID-TEISYUTUBIKBNJMA
                                        OR NCLAIMID-SKYKBNJMA
                                        OR NCLAIMID-BUNBENKBNJMA
                                        OR NCLAIMID-TEISYUTUSAKIJMA
                                        OR NCLAIMID-ZAIWEEKJMA
                                        OR NCLAIMID-BIRTHDAYJMA
                                        OR NCLAIMID-SIZANKBNJMA
                                        OR NCLAIMID-BIRTHCNTJMA
                                        OR NCLAIMID-SANKAKBNJMA
                                        OR NCLAIMID-BIKOUJMA
                                        OR NCLAIMID-HKNCOMBIJMA
                                        OR NCLAIMID-HKNJANUMJMA
                                        OR NCLAIMID-KIGOJMA
                                        OR NCLAIMID-NUMJMA
                                        OR NCLAIMID-HONKZKKBNJMA
                                        OR NCLAIMID-NYUIN-DAYJMA
                                        OR NCLAIMID-NYU-MONEYJMA
                                        OR NCLAIMID-RMSGK-MONEYJMA
                                        OR NCLAIMID-KAIJO-MONEYJMA
                                        OR NCLAIMID-BUNBEN-MONEYJMA
                                        OR NCLAIMID-SHIN-MONEYJMA
                                        OR NCLAIMID-KNS-MONEYJMA
                                        OR NCLAIMID-SYC-MONEYJMA
                                        OR NCLAIMID-SNK-MONEYJMA
                                        OR NCLAIMID-ETC-MONEYJMA
                                        OR NCLAIMID-ICBFTN-MONEYJMA
                                                                )
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
           IF    ( FLG-ODRITEM         =   ZERO )
               DISPLAY "ORCSDR11 ERROR 003:ODRITEM INSERT CHK ERR"
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
               DISPLAY "ORCSDR11 ERROR 004:ODRITEM INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理登録処理
      *****************************************************************
       2101-BUNBEN-INSERT-SEC          SECTION.
      *
           MOVE    TBBN-CREYMD             TO  WRK-CREYMD
      *
           INITIALIZE                          BUNBEN-REC
           MOVE    TBBN-HOSPNUM            TO  BUNBEN-HOSPNUM
           MOVE    TBBN-PTID               TO  BUNBEN-PTID
           MOVE    TBBN-NYUGAIKBN          TO  BUNBEN-NYUGAIKBN
           IF    ( ITEM-SKYYM              =   SPACE )
               MOVE    "000000"            TO  BUNBEN-SKYYM
           ELSE
               MOVE    ITEM-SKYYM          TO  BUNBEN-SKYYM
           END-IF
           MOVE    TBBN-RRKNUM             TO  BUNBEN-RRKNUM
           MOVE    TBBN-PTNUM              TO  BUNBEN-PTNUM
           MOVE    ITEM-TEISYUTUBIKBN      TO  BUNBEN-TEISYUTUBIKBN
           MOVE    ITEM-SKYKBN             TO  BUNBEN-SKYKBN
           MOVE    ITEM-RIYOUKBN           TO  BUNBEN-RIYOUKBN
           MOVE    ITEM-BUNBENKBN          TO  BUNBEN-BUNBENKBN
      *
           IF    ( ITEM-RIYOUKBN           =   "1" )
               MOVE    ITEM-TEISYUTUSAKI   TO  BUNBEN-TEISYUTUSAKI
           END-IF
      *
           MOVE    ITEM-KANYUKBN           TO  BUNBEN-KANYUKBN
           MOVE    ITEM-HONKZKKBN          TO  BUNBEN-HONKZKKBN
           MOVE    ITEM-HKNJANUM           TO  BUNBEN-HKNJANUM
           MOVE    ITEM-KIGO               TO  BUNBEN-KIGO
           MOVE    ITEM-NUM                TO  BUNBEN-NUM
           MOVE    LNKBUNBEN-NAME          TO  BUNBEN-NAME
           MOVE    ITEM-NUM-ZAIWEEK        TO  BUNBEN-ZAIWEEK
           MOVE    ITEM-BIRTHDAY           TO  BUNBEN-BIRTHDAY
           MOVE    ITEM-SIZANKBN           TO  BUNBEN-SIZANKBN
           MOVE    ITEM-NUM-BIRTHCNT       TO  BUNBEN-BIRTHCNT
           MOVE    ITEM-NUM-NYUIN-DAY      TO  BUNBEN-NYUIN-DAY
           MOVE    ITEM-SANKAKBN           TO  BUNBEN-SANKAKBN
           MOVE    ITEM-NUM-NYU-MONEY      TO  BUNBEN-NYU-MONEY
           MOVE    ITEM-NUM-RMSGK-MONEY    TO  BUNBEN-RMSGK-MONEY
           MOVE    ITEM-NUM-KAIJO-MONEY    TO  BUNBEN-KAIJO-MONEY
           MOVE    ITEM-NUM-BUNBEN-MONEY   TO  BUNBEN-BUNBEN-MONEY
           MOVE    ITEM-NUM-SHIN-MONEY     TO  BUNBEN-SHIN-MONEY
           MOVE    ITEM-NUM-KNS-MONEY      TO  BUNBEN-KNS-MONEY
           MOVE    ITEM-NUM-SYC-MONEY      TO  BUNBEN-SYC-MONEY
           MOVE    ITEM-NUM-SNK-MONEY      TO  BUNBEN-SNK-MONEY
           MOVE    ITEM-NUM-ETC-MONEY      TO  BUNBEN-ETC-MONEY
           MOVE    ITEM-NUM-ICBFTN-MONEY   TO  BUNBEN-ICBFTN-MONEY
           MOVE    ITEM-NUM-NINPUFTN-MONEY TO  BUNBEN-NINPUFTN-MONEY
           MOVE    ITEM-NUM-DAIRI-MONEY    TO  BUNBEN-DAIRI-MONEY
           MOVE    ITEM-BIKOU              TO  BUNBEN-BIKOU
           IF    ( ITEM-HKNJANUM (1:2)     =   "67" )
               MOVE    ITEM-HKNJANUM (3:6) TO  BUNBEN-HKNJANUM-S
           END-IF
           IF    ( TRRK-TAIINYMD           NOT =   "99999999" )
               MOVE    "1"                 TO  BUNBEN-UPDKBN
           ELSE
               MOVE    "3"                 TO  BUNBEN-UPDKBN
           END-IF
           PERFORM 800-MCNDATE-SEC
           MOVE    WRK-CREYMD              TO  BUNBEN-CREYMD
           MOVE    SMCNDATE-YMD            TO  BUNBEN-UPYMD
           MOVE    SMCNDATE-HMS            TO  BUNBEN-UPHMS
           MOVE    BUNBEN-REC              TO  MCPDATA-REC
           MOVE    "tbl_bunben"            TO  MCP-TABLE
           MOVE    "del3"                  TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
                   MOVE    1               TO  FLG-END
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           .
       2101-BUNBEN-INSERT-EXT.
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
      *
           MOVE    FLG-END         TO  SODR-RC
      *
           IF    ( FLG-END         NOT =   ZERO )
               MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
               MOVE    "abort"     TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    出産育児一時金情報オーダー保険情報チェック処理
      *****************************************************************
       800-ORCSODR11S02-SEC        SECTION.
      *
           CALL    "ORCSODR11S02"  USING
                                   SODR11S02-AREA
                                   MCPAREA
                                   SPA-AREA
      *
           .
       800-ORCSODR11S02-EXT.
           EXIT.
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-ZENKAKU-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-STR
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ZENKAKU-ERR
           END-IF
      *
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ZENKAKU-ERR
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
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
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"        TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
           IF    ( STS-DAY-RC1     =   ZERO )
               MOVE    LNK-DAY2-EDTYMD1
                               TO  WRK-EDTYMD1
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    分娩負担金サブ
      *****************************************************************
       800-ORCSBUNBENFTN-SEC              SECTION.
      *
           INITIALIZE                     LNKBUNBEN-REC
           MOVE    TBBN-HOSPNUM        TO LNKBUNBEN-HOSPNUM
           MOVE    TBBN-PTID           TO LNKBUNBEN-PTID
           MOVE    TBBN-NYUGAIKBN      TO LNKBUNBEN-NYUGAIKBN
           MOVE    TBBN-RRKNUM         TO LNKBUNBEN-RRKNUM
           CALL    "ORCSBUNBENFTN"     USING
                                       LNKBUNBEN-REC
                                       SPA-AREA
      *
           .
       800-ORCSBUNBENFTN-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY39-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ITEM-NYUINYMD       TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY39-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY20-SEL-EXT.
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
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
           MOVE    SODR-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    SODR-PTID           TO  COMB-PTID
           MOVE    ITEM-HKNCOMBI       TO  COMB-HKNCOMBINUM
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
      *    患者保険情報検索処理
      *****************************************************************
       900-PTHKNINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SODR-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTHKNINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
      *
           INITIALIZE                  BUNBEN-REC
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SODR-PTID       TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    TRRK-RRKNUM     TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SODR-PTID       TO  BUNBEN-PTID
           MOVE    ITEM-BIRTHDAY   TO  BUNBEN-BIRTHDAY
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           .
      *
       900-BUNBEN-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY5-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           .
      *
       900-BUNBEN-KEY5-FET-EXT.
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
