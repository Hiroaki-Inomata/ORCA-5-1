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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSHOLIDAY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 休日取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/11    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL−太田   03/11/11  新規作成
      *****************************************************************
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  IDX-AREA.
           03  IDX-DAY             PIC 9(05).
           03  IDX-MM              PIC 9(05).
           03  IDX-DD              PIC 9(05).
           03  IDX-NH              PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SHO             PIC 9(05).
           03  WRK-AMARI           PIC 9(05).
           03  WRK-DOW             PIC 9(05).
           03  WRK-HH1             PIC 9(05).
           03  WRK-HH2             PIC 9(05).
           03  WRK-YY1             PIC 9(05).
           03  WRK-YY2             PIC 9(05).
           03  WRK-MONDAY-2ND      PIC 9(02).
           03  WRK-MONDAY-3RD      PIC 9(02).
           03  WRK-HOLIDAY.
               05  WRK-HOLIDAY-YY  PIC 9(04).
               05  WRK-HOLIDAY-MM  PIC 9(02).
               05  WRK-HOLIDAY-DD  PIC 9(02).
           03  WRK-DATE.
               05  WRK-DATE-YY     PIC 9(04).
               05  WRK-DATE-MM     PIC 9(02).
               05  WRK-DATE-DD     PIC 9(02).
           03  WRK-SEIJINNOHI      PIC X(08).
           03  WRK-UMINOHI         PIC X(08).
           03  WRK-KEIROUNOHI      PIC X(08).
           03  WRK-TAIIKUNOHI      PIC X(08).
           03  WRK-SYUNBUNNOHI     PIC X(08).
           03  WRK-SYUUBUNNOHI     PIC X(08).
      *
       01  TYEAR-AREA.
           03  TYEAR-URUU          PIC 9(01).
           03  TYEAR-MAX           PIC 9(05).
           03  TYEAR-OCC           OCCURS  400.
               05  TYEAR-DOW       PIC 9(01).
               05  TYEAR-DATE.
                   07  TYEAR-DATE-YY PIC 9(04).
                   07  TYEAR-DATE-MM PIC 9(02).
                   07  TYEAR-DATE-DD PIC 9(02).
               05  TYEAR-NATIONAL-HOLIDAY
                                   PIC 9(01).
               05  TYEAR-HOLIDAY   PIC 9(01).
               05  TYEAR-CHK-FURIKAE
                                   PIC 9(01).
      *
       01  CONST-AREA.
           03  CONST-R02               PIC X(04)   VALUE   "2020".
           03  CONST-DOW-ITEM.
               05  CONST-MON           PIC 9(01)   VALUE   1.
               05  CONST-TUE           PIC 9(01)   VALUE   2.
               05  CONST-WED           PIC 9(01)   VALUE   3.
               05  CONST-THU           PIC 9(01)   VALUE   4.
               05  CONST-FRI           PIC 9(01)   VALUE   5.
               05  CONST-SAT           PIC 9(01)   VALUE   6.
               05  CONST-SUN           PIC 9(01)   VALUE   7.
           03  CONST-DOW-R REDEFINES   CONST-DOW-ITEM.
               05  CONST-DOW           PIC 9(01)   OCCURS  7.
           03  CONST-EOM-ITEM.
               05  CONST-EOM-JAN       PIC 9(02)   VALUE   31.
               05  CONST-EOM-FEB       PIC 9(02)   VALUE   28.
               05  CONST-EOM-MAR       PIC 9(02)   VALUE   31.
               05  CONST-EOM-APR       PIC 9(02)   VALUE   30.
               05  CONST-EOM-MAY       PIC 9(02)   VALUE   31.
               05  CONST-EOM-JUN       PIC 9(02)   VALUE   30.
               05  CONST-EOM-JUL       PIC 9(02)   VALUE   31.
               05  CONST-EOM-AUG       PIC 9(02)   VALUE   31.
               05  CONST-EOM-SEP       PIC 9(02)   VALUE   30.
               05  CONST-EOM-OCT       PIC 9(02)   VALUE   31.
               05  CONST-EOM-NOV       PIC 9(02)   VALUE   30.
               05  CONST-EOM-DEC       PIC 9(02)   VALUE   31.
           03  CONST-EOM-R  REDEFINES  CONST-EOM-ITEM.
               05  CONST-EOM           PIC 9(02)   OCCURS  12.
           03  CONST-EOM-URUU          PIC 9(02)   VALUE   29.
      *    2000年〜2006年の国民の祝日
           03  CONST-HOLIDAY-ITEM.
               05   CONST-HOLIDAY-20000101
                                       PIC X(08)   VALUE   "20000101".
               05   CONST-HOLIDAY-20000110
                                       PIC X(08)   VALUE   "20000110".
               05   CONST-HOLIDAY-20000211
                                       PIC X(08)   VALUE   "20000211".
               05   CONST-HOLIDAY-20000320
                                       PIC X(08)   VALUE   "20000320".
               05   CONST-HOLIDAY-20000429
                                       PIC X(08)   VALUE   "20000429".
               05   CONST-HOLIDAY-20000503
                                       PIC X(08)   VALUE   "20000503".
               05   CONST-HOLIDAY-20000505
                                       PIC X(08)   VALUE   "20000505".
               05   CONST-HOLIDAY-20000720
                                       PIC X(08)   VALUE   "20000720".
               05   CONST-HOLIDAY-20000915
                                       PIC X(08)   VALUE   "20000915".
               05   CONST-HOLIDAY-20000923
                                       PIC X(08)   VALUE   "20000923".
               05   CONST-HOLIDAY-20001009
                                       PIC X(08)   VALUE   "20001009".
               05   CONST-HOLIDAY-20001103
                                       PIC X(08)   VALUE   "20001103".
               05   CONST-HOLIDAY-20001123
                                       PIC X(08)   VALUE   "20001123".
               05   CONST-HOLIDAY-20001223
                                       PIC X(08)   VALUE   "20001223".
               05   CONST-HOLIDAY-20010101
                                       PIC X(08)   VALUE   "20010101".
               05   CONST-HOLIDAY-20010108
                                       PIC X(08)   VALUE   "20010108".
               05   CONST-HOLIDAY-20010211
                                       PIC X(08)   VALUE   "20010211".
               05   CONST-HOLIDAY-20010320
                                       PIC X(08)   VALUE   "20010320".
               05   CONST-HOLIDAY-20010429
                                       PIC X(08)   VALUE   "20010429".
               05   CONST-HOLIDAY-20010503
                                       PIC X(08)   VALUE   "20010503".
               05   CONST-HOLIDAY-20010505
                                       PIC X(08)   VALUE   "20010505".
               05   CONST-HOLIDAY-20010720
                                       PIC X(08)   VALUE   "20010720".
               05   CONST-HOLIDAY-20010915
                                       PIC X(08)   VALUE   "20010915".
               05   CONST-HOLIDAY-20010923
                                       PIC X(08)   VALUE   "20010923".
               05   CONST-HOLIDAY-20011008
                                       PIC X(08)   VALUE   "20011008".
               05   CONST-HOLIDAY-20011103
                                       PIC X(08)   VALUE   "20011103".
               05   CONST-HOLIDAY-20011123
                                       PIC X(08)   VALUE   "20011123".
               05   CONST-HOLIDAY-20011223
                                       PIC X(08)   VALUE   "20011223".
               05   CONST-HOLIDAY-20020101
                                       PIC X(08)   VALUE   "20020101".
               05   CONST-HOLIDAY-20020114
                                       PIC X(08)   VALUE   "20020114".
               05   CONST-HOLIDAY-20020211
                                       PIC X(08)   VALUE   "20020211".
               05   CONST-HOLIDAY-20020321
                                       PIC X(08)   VALUE   "20020321".
               05   CONST-HOLIDAY-20020429
                                       PIC X(08)   VALUE   "20020429".
               05   CONST-HOLIDAY-20020503
                                       PIC X(08)   VALUE   "20020503".
               05   CONST-HOLIDAY-20020505
                                       PIC X(08)   VALUE   "20020505".
               05   CONST-HOLIDAY-20020720
                                       PIC X(08)   VALUE   "20020720".
               05   CONST-HOLIDAY-20020915
                                       PIC X(08)   VALUE   "20020915".
               05   CONST-HOLIDAY-20020923
                                       PIC X(08)   VALUE   "20020923".
               05   CONST-HOLIDAY-20021014
                                       PIC X(08)   VALUE   "20021014".
               05   CONST-HOLIDAY-20021103
                                       PIC X(08)   VALUE   "20021103".
               05   CONST-HOLIDAY-20021123
                                       PIC X(08)   VALUE   "20021123".
               05   CONST-HOLIDAY-20021223
                                       PIC X(08)   VALUE   "20021223".
               05   CONST-HOLIDAY-20030101
                                       PIC X(08)   VALUE   "20030101".
               05   CONST-HOLIDAY-20030113
                                       PIC X(08)   VALUE   "20030113".
               05   CONST-HOLIDAY-20030211
                                       PIC X(08)   VALUE   "20030211".
               05   CONST-HOLIDAY-20030321
                                       PIC X(08)   VALUE   "20030321".
               05   CONST-HOLIDAY-20030429
                                       PIC X(08)   VALUE   "20030429".
               05   CONST-HOLIDAY-20030503
                                       PIC X(08)   VALUE   "20030503".
               05   CONST-HOLIDAY-20030505
                                       PIC X(08)   VALUE   "20030505".
               05   CONST-HOLIDAY-20030721
                                       PIC X(08)   VALUE   "20030721".
               05   CONST-HOLIDAY-20030915
                                       PIC X(08)   VALUE   "20030915".
               05   CONST-HOLIDAY-20030923
                                       PIC X(08)   VALUE   "20030923".
               05   CONST-HOLIDAY-20031013
                                       PIC X(08)   VALUE   "20031013".
               05   CONST-HOLIDAY-20031103
                                       PIC X(08)   VALUE   "20031103".
               05   CONST-HOLIDAY-20031123
                                       PIC X(08)   VALUE   "20031123".
               05   CONST-HOLIDAY-20031223
                                       PIC X(08)   VALUE   "20031223".
               05   CONST-HOLIDAY-20040101
                                       PIC X(08)   VALUE   "20040101".
               05   CONST-HOLIDAY-20040112
                                       PIC X(08)   VALUE   "20040112".
               05   CONST-HOLIDAY-20040211
                                       PIC X(08)   VALUE   "20040211".
               05   CONST-HOLIDAY-20040320
                                       PIC X(08)   VALUE   "20040320".
               05   CONST-HOLIDAY-20040429
                                       PIC X(08)   VALUE   "20040429".
               05   CONST-HOLIDAY-20040503
                                       PIC X(08)   VALUE   "20040503".
               05   CONST-HOLIDAY-20040505
                                       PIC X(08)   VALUE   "20040505".
               05   CONST-HOLIDAY-20040719
                                       PIC X(08)   VALUE   "20040719".
               05   CONST-HOLIDAY-20040920
                                       PIC X(08)   VALUE   "20040920".
               05   CONST-HOLIDAY-20040923
                                       PIC X(08)   VALUE   "20040923".
               05   CONST-HOLIDAY-20041011
                                       PIC X(08)   VALUE   "20041011".
               05   CONST-HOLIDAY-20041103
                                       PIC X(08)   VALUE   "20041103".
               05   CONST-HOLIDAY-20041123
                                       PIC X(08)   VALUE   "20041123".
               05   CONST-HOLIDAY-20041223
                                       PIC X(08)   VALUE   "20041223".
               05   CONST-HOLIDAY-20050101
                                       PIC X(08)   VALUE   "20050101".
               05   CONST-HOLIDAY-20050110
                                       PIC X(08)   VALUE   "20050110".
               05   CONST-HOLIDAY-20050211
                                       PIC X(08)   VALUE   "20050211".
               05   CONST-HOLIDAY-20050320
                                       PIC X(08)   VALUE   "20050320".
               05   CONST-HOLIDAY-20050429
                                       PIC X(08)   VALUE   "20050429".
               05   CONST-HOLIDAY-20050503
                                       PIC X(08)   VALUE   "20050503".
               05   CONST-HOLIDAY-20050505
                                       PIC X(08)   VALUE   "20050505".
               05   CONST-HOLIDAY-20050718
                                       PIC X(08)   VALUE   "20050718".
               05   CONST-HOLIDAY-20050919
                                       PIC X(08)   VALUE   "20050919".
               05   CONST-HOLIDAY-20050923
                                       PIC X(08)   VALUE   "20050923".
               05   CONST-HOLIDAY-20051010
                                       PIC X(08)   VALUE   "20051010".
               05   CONST-HOLIDAY-20051103
                                       PIC X(08)   VALUE   "20051103".
               05   CONST-HOLIDAY-20051123
                                       PIC X(08)   VALUE   "20051123".
               05   CONST-HOLIDAY-20051223
                                       PIC X(08)   VALUE   "20051223".
               05   CONST-HOLIDAY-20060101
                                       PIC X(08)   VALUE   "20060101".
               05   CONST-HOLIDAY-20060109
                                       PIC X(08)   VALUE   "20060109".
               05   CONST-HOLIDAY-20060211
                                       PIC X(08)   VALUE   "20060211".
               05   CONST-HOLIDAY-20060321
                                       PIC X(08)   VALUE   "20060321".
               05   CONST-HOLIDAY-20060429
                                       PIC X(08)   VALUE   "20060429".
               05   CONST-HOLIDAY-20060503
                                       PIC X(08)   VALUE   "20060503".
               05   CONST-HOLIDAY-20060505
                                       PIC X(08)   VALUE   "20060505".
               05   CONST-HOLIDAY-20060717
                                       PIC X(08)   VALUE   "20060717".
               05   CONST-HOLIDAY-20060918
                                       PIC X(08)   VALUE   "20060918".
               05   CONST-HOLIDAY-20060923
                                       PIC X(08)   VALUE   "20060923".
               05   CONST-HOLIDAY-20061009
                                       PIC X(08)   VALUE   "20061009".
               05   CONST-HOLIDAY-20061103
                                       PIC X(08)   VALUE   "20061103".
               05   CONST-HOLIDAY-20061123
                                       PIC X(08)   VALUE   "20061123".
               05   CONST-HOLIDAY-20061223
                                       PIC X(08)   VALUE   "20061223".
           03  CONST-HOLIDAY-R REDEFINES   CONST-HOLIDAY-ITEM.
               05   CONST-HOLIDAY      PIC X(08)   OCCURS  98.
           03  CONST-HOLIDAY-MAX       PIC 9(05)   VALUE   98.
           03  CONST-NATIONAL-HOLIDAY-G.
               05  CONST-NH-0101-G.
                   07  CONST-NH-0101-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-0101-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0101-MMDD  PIC X(04)   VALUE   "0101".
               05  CONST-NH-0211-G.
                   07  CONST-NH-0211-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-0211-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0211-MMDD  PIC X(04)   VALUE   "0211".
               05  CONST-NH-0223-G.
                   07  CONST-NH-0211-STYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0211-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0211-MMDD  PIC X(04)   VALUE   "0223".
               05  CONST-NH-0429-G.
                   07  CONST-NH-0429-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-0429-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0429-MMDD  PIC X(04)   VALUE   "0429".
               05  CONST-NH-0430-G.
                   07  CONST-NH-0430-STYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0430-EDYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0430-MMDD  PIC X(04)   VALUE   "0430".
               05  CONST-NH-0501-G.
                   07  CONST-NH-0501-STYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0501-EDYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0501-MMDD  PIC X(04)   VALUE   "0501".
               05  CONST-NH-0502-G.
                   07  CONST-NH-0502-STYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0502-EDYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0502-MMDD  PIC X(04)   VALUE   "0502".
               05  CONST-NH-0503-G.
                   07  CONST-NH-0503-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-0503-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0503-MMDD  PIC X(04)   VALUE   "0503".
               05  CONST-NH-0504-G.
                   07  CONST-NH-0504-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-0504-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0504-MMDD  PIC X(04)   VALUE   "0504".
               05  CONST-NH-0505-G.
                   07  CONST-NH-0505-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-0505-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0505-MMDD  PIC X(04)   VALUE   "0505".
               05  CONST-NH-0723-G.
                   07  CONST-NH-0723-STYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0723-EDYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0723-MMDD  PIC X(04)   VALUE   "0723".
               05  CONST-NH-0724-G.
                   07  CONST-NH-0724-STYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0724-EDYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0724-MMDD  PIC X(04)   VALUE   "0724".
               05  CONST-NH-0811-2016-2019-G.
                   07  CONST-NH-0811-STYY  PIC X(04)   VALUE   "2016".
                   07  CONST-NH-0811-EDYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-0811-MMDD  PIC X(04)   VALUE   "0811".
               05  CONST-NH-0810-G.
                   07  CONST-NH-0810-STYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0810-EDYY  PIC X(04)   VALUE   "2020".
                   07  CONST-NH-0810-MMDD  PIC X(04)   VALUE   "0810".
               05  CONST-NH-0811-G.
                   07  CONST-NH-0811-STYY  PIC X(04)   VALUE   "2021".
                   07  CONST-NH-0811-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-0811-MMDD  PIC X(04)   VALUE   "0811".
      *
               05  CONST-NH-1022-G.
                   07  CONST-NH-1022-STYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-1022-EDYY  PIC X(04)   VALUE   "2019".
                   07  CONST-NH-1022-MMDD  PIC X(04)   VALUE   "1022".
               05  CONST-NH-1103-G.
                   07  CONST-NH-1103-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-1103-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-1103-MMDD  PIC X(04)   VALUE   "1103".
               05  CONST-NH-1123-G.
                   07  CONST-NH-1123-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-1123-EDYY  PIC X(04)   VALUE   "9999".
                   07  CONST-NH-1123-MMDD  PIC X(04)   VALUE   "1123".
               05  CONST-NH-1223-G.
                   07  CONST-NH-1223-STYY  PIC X(04)   VALUE   "0000".
                   07  CONST-NH-1223-EDYY  PIC X(04)   VALUE   "2018".
                   07  CONST-NH-1223-MMDD  PIC X(04)   VALUE   "1223".
           03  CONST-NATIONAL-HOLIDAY-R    REDEFINES
                                           CONST-NATIONAL-HOLIDAY-G.
               05  CONST-NATIONAL-HOLIDAY              OCCURS  19.
                   07  CONST-NH-STYY       PIC X(04).
                   07  CONST-NH-EDYY       PIC X(04).
                   07  CONST-NH-MMDD       PIC X(04).
           03  CONST-NATIONAL-HOLIDAY-MAX  PIC 9(05)   VALUE   19.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSHOLIDAY.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               SHOLIDAY-AREA.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              WRK-AREA
                                   TYEAR-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO            TO  SHOLIDAY-RC
           MOVE    ZERO            TO  SHOLIDAY-HOLIDAY
           MOVE    ZERO            TO  SHOLIDAY-HOLIDAY-PLUS
      *
           EVALUATE    TRUE
           WHEN  ( SHOLIDAY-DATE   IS NOT NUMERIC )
               MOVE    1           TO  SHOLIDAY-RC
           WHEN  ( SHOLIDAY-DATE (1:4)
                                   <   "2000"  )
               MOVE    2           TO  SHOLIDAY-RC
           END-EVALUATE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-URUU-SEC
      *
           PERFORM 2001-DOW-SEC
      *
           PERFORM 2001-HOLIDAY-SEC
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   TYEAR-MAX )
      *
      D       IF    ( TYEAR-NATIONAL-HOLIDAY(IDX-DAY)
      D                                   =   1 )
      D        OR   ( TYEAR-CHK-FURIKAE(IDX-DAY)
      D                                   =   1 )
      D           DISPLAY TYEAR-DATE(IDX-DAY)
      D       END-IF
      *
               IF    ( SHOLIDAY-DATE
                                   =   TYEAR-DATE (IDX-DAY))
                   IF    ( TYEAR-HOLIDAY (IDX-DAY) =   1 )
                       MOVE    1       TO  SHOLIDAY-HOLIDAY
                                           SHOLIDAY-HOLIDAY-PLUS
                   END-IF
                   MOVE    TYEAR-MAX   TO  IDX-DAY
               END-IF
      *
           END-PERFORM
      *
           IF    ( SHOLIDAY-DATE (5:4) =   "1229" OR "1230" OR "1231"
                                        OR "0101" OR "0102" OR "0103" )
               MOVE    1               TO  SHOLIDAY-HOLIDAY-PLUS
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    閏年チェック処理
      *****************************************************************
       2001-URUU-SEC           SECTION.
      *
           MOVE    SHOLIDAY-DATE   TO  WRK-DATE
      *
           DIVIDE  WRK-DATE-YY BY  4   GIVING  WRK-SHO
                   REMAINDER   WRK-AMARI
      *
           IF    ( WRK-AMARI   =   ZERO )
               MOVE    1       TO  TYEAR-URUU
           END-IF
      *
           .
       2001-URUU-EXT.
           EXIT.
      *****************************************************************
      *    曜日設定処理
      *****************************************************************
       2001-DOW-SEC           SECTION.
      *
           MOVE    SHOLIDAY-DATE   TO  WRK-DATE
      *
           COMPUTE WRK-DATE-YY =   WRK-DATE-YY - 1
      *
           DIVIDE  WRK-DATE-YY BY  100 GIVING  WRK-HH1
                   REMAINDER   WRK-YY1
           COMPUTE WRK-YY2 =   WRK-YY1 /   4
           COMPUTE WRK-HH2 =   WRK-HH1 /   4
           COMPUTE WRK-DOW
               = ( WRK-YY1 + WRK-YY2 + WRK-HH2 -   2 * WRK-HH1 + 36 + 1)
           DIVIDE  WRK-DOW     BY  7   GIVING  WRK-SHO
                   REMAINDER   WRK-AMARI
           COMPUTE WRK-DOW     =   WRK-AMARI
           IF    ( WRK-DOW     <   2 )
               COMPUTE WRK-DOW =   WRK-DOW +   6
           ELSE
               COMPUTE WRK-DOW =   WRK-DOW -   1
           END-IF
      *
           MOVE    SHOLIDAY-DATE   TO  WRK-DATE
      *
           IF    ( TYEAR-URUU  =   1 )
               MOVE    366     TO  TYEAR-MAX
           ELSE
               MOVE    365     TO  TYEAR-MAX
           END-IF
      *
           MOVE    1           TO  IDX-MM
                                   IDX-DD
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   TYEAR-MAX )
      *
               IF    ( IDX-DD      =   1 )
                   EVALUATE    WRK-DOW
                   WHEN    CONST-MON
                       COMPUTE WRK-MONDAY-2ND  =   8
                   WHEN    CONST-TUE
                       COMPUTE WRK-MONDAY-2ND  =   14
                   WHEN    CONST-WED
                       COMPUTE WRK-MONDAY-2ND  =   13
                   WHEN    CONST-THU
                       COMPUTE WRK-MONDAY-2ND  =   12
                   WHEN    CONST-FRI
                       COMPUTE WRK-MONDAY-2ND  =   11
                   WHEN    CONST-SAT
                       COMPUTE WRK-MONDAY-2ND  =   10
                   WHEN    CONST-SUN
                       COMPUTE WRK-MONDAY-2ND  =   09
                   END-EVALUATE
      *
                   COMPUTE WRK-MONDAY-3RD
                       =   WRK-MONDAY-2ND
                       +   7
      *
                   MOVE    WRK-DATE-YY     TO  WRK-HOLIDAY-YY
                   MOVE    IDX-MM          TO  WRK-HOLIDAY-MM
                   EVALUATE    IDX-MM
                   WHEN    1
                       MOVE    WRK-MONDAY-2ND
                                           TO  WRK-HOLIDAY-DD
                       MOVE    WRK-HOLIDAY TO  WRK-SEIJINNOHI
                   WHEN    7
                       IF    ( WRK-DATE-YY =   CONST-R02 )
                           CONTINUE
                       ELSE
                           MOVE    WRK-MONDAY-3RD
                                           TO  WRK-HOLIDAY-DD
                           MOVE    WRK-HOLIDAY
                                           TO  WRK-UMINOHI
                       END-IF
                   WHEN    9
                       MOVE    WRK-MONDAY-3RD
                                           TO  WRK-HOLIDAY-DD
                       MOVE    WRK-HOLIDAY TO  WRK-KEIROUNOHI
                   WHEN   10
                       IF    ( WRK-DATE-YY =   CONST-R02 )
                           CONTINUE
                       ELSE
                           MOVE    WRK-MONDAY-2ND
                                           TO  WRK-HOLIDAY-DD
                           MOVE    WRK-HOLIDAY
                                           TO  WRK-TAIIKUNOHI
                       END-IF
                   END-EVALUATE
      *
               END-IF
      *
               MOVE    WRK-DOW TO  TYEAR-DOW (IDX-DAY)
      *
               IF    ( TYEAR-DOW (IDX-DAY) =   CONST-SUN )
                   MOVE    1   TO  TYEAR-HOLIDAY (IDX-DAY)
               END-IF
      *
               MOVE    WRK-DATE-YY
                               TO  TYEAR-DATE-YY (IDX-DAY)
               MOVE    IDX-MM  TO  TYEAR-DATE-MM (IDX-DAY)
               MOVE    IDX-DD  TO  TYEAR-DATE-DD (IDX-DAY)
      *
               MOVE    TYEAR-DOW (IDX-DAY)  TO  WRK-DOW
      *
               IF    ( WRK-DOW     =   CONST-SUN )
                   COMPUTE WRK-DOW =   CONST-MON
               ELSE
                   COMPUTE WRK-DOW =   WRK-DOW +   1
               END-IF
      *
               COMPUTE IDX-DD  =   IDX-DD  +   1
      *
               IF    ( TYEAR-URUU  =   1 )
                AND  ( IDX-MM      =   2 )
                   IF    ( IDX-DD  >   CONST-EOM-URUU )
                       COMPUTE IDX-MM  =   IDX-MM  +   1
                       COMPUTE IDX-DD  =   1
                   END-IF
               ELSE
                   IF    ( IDX-DD  >   CONST-EOM (IDX-MM))
                       COMPUTE IDX-MM  =   IDX-MM  +   1
                       COMPUTE IDX-DD  =   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    SHOLIDAY-DATE   TO  WRK-HOLIDAY
           COMPUTE WRK-HOLIDAY-DD
               = ( WRK-DATE-YY - 1980) / 4
           COMPUTE WRK-HOLIDAY-DD
               =   20.8431
               +   0.242194
               * ( WRK-HOLIDAY-YY - 1980)
               -   WRK-HOLIDAY-DD
           MOVE    3               TO  WRK-HOLIDAY-MM
           MOVE    WRK-HOLIDAY     TO  WRK-SYUNBUNNOHI
      *
           MOVE    SHOLIDAY-DATE   TO  WRK-HOLIDAY
           COMPUTE WRK-HOLIDAY-DD
               = ( WRK-DATE-YY - 1980) / 4
           COMPUTE WRK-HOLIDAY-DD
               =   23.2488
               +   0.242194
               * ( WRK-HOLIDAY-YY - 1980)
               -   WRK-HOLIDAY-DD
           MOVE    9               TO  WRK-HOLIDAY-MM
           MOVE    WRK-HOLIDAY     TO  WRK-SYUUBUNNOHI
      *
           .
       2001-DOW-EXT.
           EXIT.
      *****************************************************************
      *    祝日設定処理
      *****************************************************************
       2001-HOLIDAY-SEC            SECTION.
      *
           PERFORM 20011-HOLIDAY-SEC
      *
           PERFORM 20011-ETC-HOLIDAY-SEC
      *
           .
       2001-HOLIDAY-EXT.
           EXIT.
      *****************************************************************
      *    祝日設定処理
      *****************************************************************
       20011-HOLIDAY-SEC            SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   TYEAR-MAX )
      *
               EVALUATE    SHOLIDAY-DATE (1:4)
               WHEN    2000    THRU    2006
                   PERFORM VARYING IDX-NH      FROM    1   BY  1
                           UNTIL ( IDX-NH  >   CONST-HOLIDAY-MAX )
                        IF    ( TYEAR-DATE (IDX-DAY)
                                           =   CONST-HOLIDAY (IDX-NH))
                               MOVE    1   TO  TYEAR-NATIONAL-HOLIDAY
                                                             (IDX-DAY)
                               MOVE    CONST-HOLIDAY-MAX
                                           TO  IDX-NH
                        END-IF
                   END-PERFORM
               WHEN    2007    THRU    2999
                   PERFORM VARYING IDX-NH   FROM    1   BY  1
                           UNTIL ( IDX-NH > CONST-NATIONAL-HOLIDAY-MAX )
                       IF    ( TYEAR-DATE-YY (IDX-DAY)
                                          >=   CONST-NH-STYY (IDX-NH))
                        AND  ( TYEAR-DATE-YY (IDX-DAY)
                                          <=   CONST-NH-EDYY (IDX-NH))
                           IF    ( TYEAR-DATE  (IDX-DAY) (5:4)
                                           =   CONST-NH-MMDD (IDX-NH))
                               MOVE    1   TO  TYEAR-NATIONAL-HOLIDAY
                                                             (IDX-DAY)
                               MOVE    CONST-NATIONAL-HOLIDAY-MAX
                                           TO  IDX-NH
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF    ( TYEAR-DATE (IDX-DAY)    =   WRK-SEIJINNOHI
                                                    OR WRK-UMINOHI
                                                    OR WRK-KEIROUNOHI
                                                    OR WRK-TAIIKUNOHI
                                                    OR WRK-SYUNBUNNOHI
                                                    OR WRK-SYUUBUNNOHI )
                               MOVE    1   TO  TYEAR-NATIONAL-HOLIDAY
                                                             (IDX-DAY)
                   END-IF
      *
               END-EVALUATE
           END-PERFORM
      *
           .
       20011-HOLIDAY-EXT.
           EXIT.
      *****************************************************************
      *    振替休日・国民の休日設定処理
      *****************************************************************
       20011-ETC-HOLIDAY-SEC               SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   TYEAR-MAX )
      *
               IF    ( TYEAR-NATIONAL-HOLIDAY (IDX-DAY)        =   1 )
                   MOVE    1               TO  TYEAR-HOLIDAY
                                                         (IDX-DAY)
               END-IF
      *
               IF    ( TYEAR-NATIONAL-HOLIDAY (IDX-DAY)        =   1 )
                AND  ( TYEAR-NATIONAL-HOLIDAY (IDX-DAY + 2)    =   1 )
                   MOVE    1               TO  TYEAR-HOLIDAY
                                                         (IDX-DAY + 1)
               END-IF
      *
               IF    ( TYEAR-NATIONAL-HOLIDAY (IDX-DAY)   =   1 )
                   IF    ( TYEAR-DOW (IDX-DAY)            =   CONST-SUN)
                    OR   ( TYEAR-CHK-FURIKAE (IDX-DAY)    =   1 )
                       MOVE    1           TO  TYEAR-CHK-FURIKAE
                                                         (IDX-DAY + 1)
                   END-IF
               ELSE
                   IF    ( TYEAR-DOW (IDX-DAY)        NOT =   CONST-SUN)
                    AND  ( TYEAR-CHK-FURIKAE (IDX-DAY)    =   1 )
                       MOVE    1           TO  TYEAR-HOLIDAY
                                                         (IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       20011-ETC-HOLIDAY-EXT.
           EXIT.
