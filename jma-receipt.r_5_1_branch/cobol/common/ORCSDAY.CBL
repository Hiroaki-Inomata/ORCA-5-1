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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSDAY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日付変換サブ
      *  コンポーネント名  : 日付変換
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−    　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.05.07  明治の開始年の修正
      *  01.00.02    MCC-藤原     01.05.07  日のニューメリックチェック追加
      *  01.00.03    NACL-藤原    02.10.21  漢字項目の空白を全角に修正
      *  01.00.04    NACL-多々納  03.04.16  END-IF,END-SERACH追加
      *  01.00.05    NACL-多々納  04.09.22  海の日、敬老の日変更
      *  04.03.00    NACL-多々納  08.07.08  明治以前、和暦９９年以上エラー他
      *  04.08.00    NACL-多々納  15.10.20  H27(2015) 振替休日追加
      *  04.08.00    NACL-多々納  15.11.26  山の日（8/11)追加
      *  05.00.00    NACL-多々納  19.02.XX  2019年休日判定、元号改正仮対応
      *  05.00.00    NACL-多々納  19/04/01  新元号対応
      *  05.00.00    NACL-多々納  19.12.XX  2020年休日判定
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       DATA                DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-AREA.
           03  WK-YYMMDD                   PIC 9(08).
           03  WK-SYYMMDD      REDEFINES   WK-YYMMDD.
               05  WK-SYY                  PIC 9(04).
               05  WK-SMD                  PIC 9(04).
           03  WK-WYYMMDD      REDEFINES   WK-YYMMDD.
               05  FILLER                  PIC X(01).
               05  WK-WYYMMDD-7.
                 07  WK-WG                 PIC 9(01).
                 07  WK-WYY                PIC 9(02).
                 07  WK-WMD                PIC 9(04).
           03  WK-YYMMDD-R     REDEFINES   WK-YYMMDD.
               05  WK-YY1.
                 07  FILLER                PIC 9(01).
                 07  WK-GYY1               PIC 9(01).
               05  WK-YMD.
                 07  WK-YM.
                   09  WK-YY               PIC 9(02).
                   09  WK-MM               PIC 9(02).
                 07  WK-DD                 PIC 9(02).
           03  WK-YYYYMMDD-R   REDEFINES   WK-YYMMDD.
               05  WK-YYYYMM               PIC 9(06).
               05  FILLER                  PIC 9(02).
      *
           03  WK-NENDO                    PIC 9(04).
           03  WK-NISU                     PIC S9(05).
           03  WK-SUUTI                    PIC 9(05).
           03  WK-SYO                      PIC 9(05).
           03  WK-AMARI                    PIC 9(05).
           03  WK-SANSYUTS                 PIC S9(05).
           03  WK-ST                       PIC 9(08).
           03  WK-ST-R         REDEFINES   WK-ST.
               05  WK-ST1                  PIC 9(04).
               05  WK-ST2                  PIC 9(04).
           03  WK-END                      PIC 9(08).
           03  SYURYO-YMD.
               05  SYURYO-YY               PIC 9(04).
               05  SYURYO-MM               PIC 9(02).
               05  SYURYO-DD               PIC 9(02).
           03  KISAN-YMD.
               05  KISAN-YY                PIC 9(04).
               05  KISAN-MM                PIC 9(02).
               05  KISAN-DD                PIC 9(02).
           03  WK-FLG.
               05  WK-ERR                  PIC 9(01).
               05  WK-UERR                 PIC 9(04).
           03  WK-YSKKBN.
               05  YSK-YOUBI               PIC 9(01).
               05  YSK-YOUBI2              PIC 9(01).
               05  YSK-SYUKJTS             PIC 9(01).
               05  YSK-KYUJTS              PIC 9(01).
           03  WK-YSK-YYMM.
               05  WK-YSK-YYYY             PIC 9(04).
               05  WK-YSK-MM               PIC 9(02).
           03  WK-MNS-YMD.
               05  WK-MNS-YY               PIC 9(04).
               05  WK-MNS-MM               PIC 9(02).
               05  WK-MNS-DD               PIC 9(02).
           03  WK-YUKO.
               05  WK-YUKO-YY              PIC 9(04).
               05  WK-YUKO-MM              PIC 9(02).
               05  WK-YUKO-DD              PIC 9(02).
      *
           03  WK-SYSZYB.
               05  WK-SY                   PIC 9(04).
               05  WK-SZ                   PIC 9(04).
               05  WK-SSYB                 PIC 9(05)V9(08).
               05  WK-WORKA                PIC 9(02).
           03  WK-YOUB.
               05  FILLER                  PIC 9(04).
               05  WYOUB-MMDD              PIC 9(04).
           03  WK-SKJYMD.
               05  WK-SKJTYY               PIC 9(04).
               05  WK-SKJTMM               PIC 9(02).
               05  WK-SKJTDD               PIC 9(02).
           03  WK-DYMMY.
               05  WK-DUMMYN               PIC 9(10).
           03  WK-NISSU-A.
               05  NISU-A                  PIC 9(03).
               05  NISU-B                  PIC 9(05).
               05  KIJYUN-A                PIC S9(05).
               05  KIJYUN-YY               PIC 9(04).
      *
       01  CST-AREA.
           03  CST1        PIC 999V9(08)   VALUE  365.24217878.
           03  CST2        PIC 99V9(08)    VALUE  20.98819444.
           03  CST3        PIC 9V9(07)     VALUE  2.4194444.
       01  WK-NISSU-B.
           03  SYU-W                   PIC S9(03)  VALUE  365.
      *
       01  GENGO-TBL.
      *
      *2019.5.1からの新元号（令和）対応
           03  FILLER.
             05  FILLER    PIC X(10)   VALUE   "5R20190501".
             05  FILLER    PIC X(04)   VALUE   "令和".
      *
           03  FILLER.
             05  FILLER    PIC X(10)   VALUE   "4H19890108".
             05  FILLER    PIC X(04)   VALUE   "平成".
           03  FILLER.
             05  FILLER    PIC X(10)   VALUE   "3S19261225".
             05  FILLER    PIC X(04)   VALUE   "昭和".
           03  FILLER.
             05  FILLER    PIC X(10)   VALUE   "2T19120730".
             05  FILLER    PIC X(04)   VALUE   "大正".
           03  FILLER.
      *---- (01.00.01) LINE MOD START ----------------------------*
      ****   05  FILLER    PIC X(10)   VALUE   "1M18730101".
             05  FILLER    PIC X(10)   VALUE   "1M18680101".
      *---- (01.00.01) LINE MOD END   ----------------------------*
             05  FILLER    PIC X(04)   VALUE   "明治".
       01  GENGO-TBL-R     REDEFINES   GENGO-TBL.
      *
      *****03  GENGO-TBL-R2   OCCURS   4   INDEXED  BY  IX1.
           03  GENGO-TBL-R2   OCCURS   5   INDEXED  BY  IX1.
             05  GENGO-T1              PIC 9(01).
             05  GENGO-T2              PIC X(01).
             05  GENGO-YYMMDD-X.
               07  GENGO-YYYY.
                 09  GENGO-YY          PIC 9(02).
                 09  GENGO-YY2         PIC 9(02).
               07  GENGO-YYYY-R        REDEFINES    GENGO-YYYY.
                 09  GENGO-YYYY-RR     PIC 9(04).
               07  GENGO-MMDD          PIC 9(04).
             05  GENGO-YYMMDD-R        REDEFINES    GENGO-YYMMDD-X.
               07  GENGO-YYMMDD        PIC 9(08).
             05  GENMEI                PIC N(2).
      *
      *    ２０００年以前の祝日テーブル（固定日）
       01  SKJT-TABLE.
           03  FILLER      PIC X(12)   VALUE "010101150211".
           03  FILLER      PIC X(12)   VALUE "042905030504".
           03  FILLER      PIC X(12)   VALUE "050507200915".
           03  FILLER      PIC X(12)   VALUE "101011031123".
           03  FILLER      PIC X(12)   VALUE "112300000000".
       01  SKJT-TABLE-R    REDEFINES   SKJT-TABLE.
           03  SKJT-TBL    OCCURS      15  INDEXED  BY  IX2.
             05  SKJT-T1               PIC 9(04).
      *
      *    ２０００年以降の祝日テーブル（固定日）
       01  SKJT-TABLE-NEW.
           03  FILLER      PIC X(12)   VALUE "010102110429".
           03  FILLER      PIC X(12)   VALUE "050305040505".
           03  FILLER      PIC X(12)   VALUE "072009151103".
           03  FILLER      PIC X(12)   VALUE "112312230000".
       01  SKJT-TABLE-RNEW  REDEFINES  SKJT-TABLE-NEW.
           03  SKJT-TBLNEW  OCCURS     12  INDEXED  BY IX6.
             05  SKJT-NEW              PIC 9(04).
      *
      *    ２００３年以降の祝日テーブル（固定日）
      *    (7/20,9/15 を第３月曜へ）
       01  SKJT-TABLE-NEW2.
           03  FILLER      PIC X(12)   VALUE "010102110429".
           03  FILLER      PIC X(12)   VALUE "050305040505".
           03  FILLER      PIC X(12)   VALUE "110311231223".
       01  SKJT-TABLE-RNEW2  REDEFINES  SKJT-TABLE-NEW2.
           03  SKJT-TBLNEW2  OCCURS     9   INDEXED  BY IX7.
             05  SKJT-NEW2              PIC 9(04).
      *
      *R01.12
      *    ２０２１年以降の祝日テーブル（固定日）
      *    (12/23 を 2/23へ）
       01  SKJT-TABLE-NEW3.
           03  FILLER      PIC X(12)   VALUE "010102110223".
           03  FILLER      PIC X(12)   VALUE "042905030504".
           03  FILLER      PIC X(12)   VALUE "050508111103".
           03  FILLER      PIC X(12)   VALUE "1123".
       01  SKJT-TABLE-RNEW3  REDEFINES  SKJT-TABLE-NEW3.
           03  SKJT-TBLNEW3  OCCURS     12  INDEXED  BY IX23.
             05  SKJT-NEW3              PIC 9(04).
      *
      *!
      *H31.1
      *    ２０１９年の祝日テーブル（固定日）
       01  SKJT-TABLE-NEW19.
           03  FILLER      PIC X(12)   VALUE "010102110429".
           03  FILLER      PIC X(12)   VALUE "043005010502".
           03  FILLER      PIC X(12)   VALUE "050305040505".
           03  FILLER      PIC X(12)   VALUE "050608110812".
           03  FILLER      PIC X(12)   VALUE "102211031123".
       01  SKJT-TABLE-RNEW19  REDEFINES  SKJT-TABLE-NEW19.
           03  SKJT-TBLNEW19  OCCURS     15  INDEXED  BY IX19.
             05  SKJT-NEW19              PIC 9(04).
      *!
      *!
      *R01.12
      *２０２０年の祝日テーブル（固定日）
       01  SKJT-TABLE-NEW20.
           03  FILLER      PIC X(12)   VALUE "010101130211".
           03  FILLER      PIC X(12)   VALUE "022302240320".
           03  FILLER      PIC X(12)   VALUE "042905030504".
           03  FILLER      PIC X(12)   VALUE "050505060723".
           03  FILLER      PIC X(12)   VALUE "072408100921".
           03  FILLER      PIC X(12)   VALUE "092211031123".
       01  SKJT-TABLE-RNEW20  REDEFINES  SKJT-TABLE-NEW20.
           03  SKJT-TBLNEW20  OCCURS     18  INDEXED  BY IX20.
             05  SKJT-NEW20              PIC 9(04).
      *!
      *
      *    祝日テーブル２（春分の日・秋分の日）
       01  SKJT-TABLE2.
           03  SKJT-TBL2   OCCURS      2   INDEXED  BY  IX3.
             05  SKJT-M                PIC 9(02).
             05  SKJT-D                PIC 9(02).
      *
      *    祝日テーブル３（成人の日・体育の日）
       01  SKJT-TABLE3.
           03  SKJT-TBL3   OCCURS      2   INDEXED  BY  IX5.
             05  SKJT-M3               PIC 9(02).
             05  SKJT-D3               PIC 9(02).
      *
      *    祝日テーブル４（海日・敬老の日）
       01  SKJT-TABLE4.
           03  SKJT-TBL4   OCCURS      2   INDEXED  BY  IX8.
             05  SKJT-M4               PIC 9(02).
             05  SKJT-D4               PIC 9(02).
      *
      *    年間祝日管理テーブル  ( 99 => サンシュツ ニヨリ セッテイ )
       01  SKJT-KNR-TBL.
           03  FILLER      PIC X(14)   VALUE "01019900000002".
           03  FILLER      PIC X(14)   VALUE "02110000000001".
           03  FILLER      PIC X(14)   VALUE "03990000000001".
           03  FILLER      PIC X(14)   VALUE "04290000000001".
           03  FILLER      PIC X(14)   VALUE "05030405000003".
           03  FILLER      PIC X(14)   VALUE "06000000000000".
           03  FILLER      PIC X(14)   VALUE "07200000000001".
           03  FILLER      PIC X(14)   VALUE "08000000000000".
           03  FILLER      PIC X(14)   VALUE "09159900000002".
           03  FILLER      PIC X(14)   VALUE "10990000000001".
           03  FILLER      PIC X(14)   VALUE "11032300000002".
           03  FILLER      PIC X(14)   VALUE "12230000000001".
       01  SKJT-KNRTBL      REDEFINES  SKJT-KNR-TBL.
           03  SKJT-KNROC  OCCURS      12.
             05  SKJT-KNRMM            PIC 9(02).
             05  SKJT-KNRDD            PIC 9(02)    OCCURS   5.
             05  SKJT-KNRCNT           PIC 9(02).
      *
      *    休日テーブル
       01  KYUJT-TABLE.
           03  FILLER      PIC X(08)   VALUE "01020103".
           03  FILLER      PIC X(12)   VALUE "122912301231".
           03  FILLER      PIC X(20)   VALUE "00000000000000000000".
       01  KYUJT-TABLE-R   REDEFINES   KYUJT-TABLE.
           03  KYUJT-TBL   OCCURS      10  INDEXED  BY  IX4.
             05  KYUJT-T2              PIC 9(04).
      *
      *    カレンダーテーブル
       01  CAL-TABLE.
           03  FILLER      PIC X(24)   VALUE
                                       "312831303130313130313031".
       01  CAL-TABLE-R     REDEFINES   CAL-TABLE.
           03  CAL-TBL     OCCURS      12.
             05  CAL-T                 PIC 9(02).
      *
      *    日数テーブル
       01  NISU-TABLE.
           03  FILLER      PIC X(36)   VALUE
                          "000031059090120151181212243273304334".
       01  NISU-TABLE2     REDEFINES   NISU-TABLE.
           03  NISU-TBL    OCCURS      12.
             05  NISU-T                PIC 9(03).
      *
      *    曜日テーブル
       01  YOUBI-TABLE.
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "1MON".
             05  FILLER    PIC X(02)   VALUE    "月".
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "2TUE".
             05  FILLER    PIC X(02)   VALUE    "火".
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "3WED".
             05  FILLER    PIC X(02)   VALUE    "水".
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "4THU".
             05  FILLER    PIC X(02)   VALUE    "木".
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "5FRI".
             05  FILLER    PIC X(02)   VALUE    "金".
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "6SAT".
             05  FILLER    PIC X(02)   VALUE    "土".
           03  FILLER.
             05  FILLER    PIC X(04)   VALUE    "7SUN".
             05  FILLER    PIC X(02)   VALUE    "日".
       01  YOUBI-TABLE2    REDEFINES   YOUBI-TABLE.
           03  YOUBI-TBL   OCCURS      7   INDEXED  BY  IX-YOUBI.
             05  YOUBI-KBN             PIC X(01).
             05  YOUBI-KIGO            PIC X(03).
             05  YOUBI-MEI             PIC X(02).
      *
      *    漢字編集テーブル
       01  EDT-GYMD.
           03  EDT-GYMD-XX             PIC  9(02).
           03  EDT-GYMD-YMD.
               05  EDT-GYMD-Y          PIC  9(02).
               05  EDT-GYMD-M          PIC  9(02).
               05  EDT-GYMD-D          PIC  9(02).
       01  EDT-GYMDZ.
           03  EDT-GYMDZ-YMD.
               05  EDT-GYMDZ-Y         PIC  Z9.
               05  EDT-GYMDZ-M         PIC  Z9.
               05  EDT-GYMDZ-D         PIC  Z9.
       01  EDT-GYMDX.
           03  EDT-GYMDX-OC            PIC  X(01) OCCURS   6.
      *
       01  EDT-GYMD1.
           03  EDT-GYMD1-G             PIC  X(01).
           03  EDT-GYMD1-YMD.
               05  EDT-GYMD1-Y         PIC  Z(02).
               05  EDT-GYMD1-P1        PIC  X(01).
               05  EDT-GYMD1-M         PIC  Z(02).
               05  EDT-GYMD1-P2        PIC  X(01).
               05  EDT-GYMD1-D         PIC  Z(02).
      *
       01  EDT-GYMD2.
           03  EDT-GYMD2-G             PIC  X(04).
           03  EDT-GYMD2-YMD.
               05  EDT-GYMD2-Y         PIC  X(04).
               05  EDT-GYMD2-P1        PIC  X(02).
               05  EDT-GYMD2-M         PIC  X(04).
               05  EDT-GYMD2-P2        PIC  X(02).
               05  EDT-GYMD2-D         PIC  X(04).
      *
       01  EDT-GYMD3.
           03  EDT-GYMD3-G             PIC  X(04).
           03  EDT-GYMD3-YMD.
               05  EDT-GYMD3-Y         PIC  X(04).
               05  EDT-GYMD3-NEN       PIC  X(02).
               05  EDT-GYMD3-M         PIC  X(04).
               05  EDT-GYMD3-TUKI      PIC  X(02).
               05  EDT-GYMD3-D         PIC  X(04).
               05  EDT-GYMD3-HI        PIC  X(02).
      *
      *01  EDT-YMD                     PIC  N(06).
       01  EDT-YMD                     PIC  X(12).
       01  EDT-YMDR                    REDEFINES  EDT-YMD.
           03  EDT-YMD-OC              PIC  X(02) OCCURS 6.
       01  EDT-YMDR1                   REDEFINES  EDT-YMD.
           03  EDT-YMD-Y               PIC  X(04).
           03  EDT-YMD-M               PIC  X(04).
           03  EDT-YMD-D               PIC  X(04).
      *
      *    数字テーブル
       01  SUUJI-AREA.
           03  SUUJIN-TBL.
               05  FILLER              PIC  X(22)  VALUE
                 "  ０１２３４５６７８９".
           03  SUUJIN-TBL-R            REDEFINES   SUUJIN-TBL.
               05  SUUJIN              PIC  X(02)  OCCURS  11.
           03  SUUJI-TBL.
               05  FILLER              PIC  X(11)  VALUE
               " 0123456789".
           03  SUUJI-TBL-R             REDEFINES   SUUJI-TBL.
               05  SUUJI               PIC  X(01)  OCCURS  11.
      *
      *    コンスタント文字領域
       01  CST-MOJI-AREA.
           03  CST-SPACE               PIC  X(02)  VALUE
                 "  ".
           03  CST-NEN                 PIC  X(02)  VALUE
                 "年".
           03  CST-TUKI                PIC  X(02)  VALUE
                 "月".
           03  CST-HI                  PIC  X(02)  VALUE
                 "日".
           03  CST-PN                  PIC  X(02)  VALUE
                 "．".
           03  CST-PX                  PIC  X(01)  VALUE
                 ".".
      *
      *    添え字領域
       01  IDX-AREA.
           03  I                       PIC  9(02).
           03  J                       PIC  9(02).
           03  IDX                     PIC  9(04).
           03  IDX-T                   PIC  9(04).
       01  FLG-AREA.
           03  FLG-SYORI-END           PIC 9(01).
      *
      ***
           COPY  "CPORCSDAY.INC".
      ***
       LINKAGE             SECTION.
      *D   COPY  "CPLNKSTS.INC".
           COPY  "CPORCSLNK.INC".
       01  LNK-DAYX-AREA.
           03  LNK-DAYX-IRAI           PIC X(02).
           03  FILLER                  PIC X(78).
      *
       PROCEDURE           DIVISION  USING   STS-AREA-DAY
                                             LNK-DAYX-AREA.
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
           INITIALIZE              IDX-AREA
           INITIALIZE              FLG-AREA
           INITIALIZE              WK-AREA 
           MOVE    SPACE           TO  LNK-DAY1-AREA
                                       LNK-DAY2-AREA
                                       LNK-DAY3-AREA
                                       LNK-DAY4-AREA
                                       LNK-DAY5-AREA
                                       LNK-DAY6-AREA
                                       LNK-DAY7-AREA.
           MOVE    ZERO            TO  STS-DAY-RC1
                                       STS-DAY-RC2
                                       WK-YYMMDD
                                       WK-FLG.
      *
           EVALUATE    LNK-DAYX-IRAI
               WHEN    "11"
               WHEN    "12"
      *            暦変換
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY1-AREA
                   PERFORM  DTCONV-RTN
                   MOVE     LNK-DAY1-AREA      TO  LNK-DAYX-AREA
               WHEN    "21"
               WHEN    "22"
      *            日付編集
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY2-AREA
                   PERFORM  DTEDIT-RTN
                   MOVE     LNK-DAY2-AREA      TO  LNK-DAYX-AREA
               WHEN    "31"
               WHEN    "32"
      *            曜日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY3-AREA
                   PERFORM  DTYOUB-RTN
                   MOVE     LNK-DAY3-AREA      TO  LNK-DAYX-AREA
               WHEN    "41"
               WHEN    "42"
      *            祝日算出
      *!!!         2019年から対応なし
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY4-AREA
                   PERFORM  DTSKJT-RTN
                   MOVE     LNK-DAY4-AREA      TO  LNK-DAYX-AREA
               WHEN    "51"
               WHEN    "52"
      *            期間日数計算
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY5-AREA
                   PERFORM  DTKIKN-RTN
                   MOVE     LNK-DAY5-AREA      TO  LNK-DAYX-AREA
               WHEN    "61"
               WHEN    "62"
      *            該当日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY6-AREA
                   PERFORM  DTPLMN-RTN
                   MOVE     LNK-DAY6-AREA      TO  LNK-DAYX-AREA
               WHEN    "71"
               WHEN    "72"
      *            月末日算出
                   MOVE     LNK-DAYX-AREA      TO  LNK-DAY7-AREA
                   PERFORM  DTMEND-RTN
                   MOVE     LNK-DAY7-AREA      TO  LNK-DAYX-AREA
               WHEN    OTHER
                   MOVE     30             TO  STS-DAY-RC1
                   MOVE     01             TO  STS-DAY-RC2
           END-EVALUATE
           .
       MAIN-EXT.
           EXIT PROGRAM
           .
      *****STOP RUN.
      ******************************************************************
      *        暦変換処理                         (IRAI = 11 / 12)     *
      ******************************************************************
       DTCONV-RTN           SECTION.
       DTCONV-000.
           EVALUATE    LNK-DAY1-IRAI
               WHEN    "11"
               WHEN    "21"
               WHEN    "61"
               WHEN    "71"
      *            西暦
                   IF      LNK-DAY1-YMD(1:8)   NOT NUMERIC
                       MOVE    20              TO  STS-DAY-RC1
                   ELSE
                       MOVE    LNK-DAY1-YMD    TO  WK-YYMMDD
                       PERFORM 200-SEIREKI-CHK-SEC
                   END-IF
               WHEN    "12"
               WHEN    "22"
               WHEN    "32"
               WHEN    "42"
               WHEN    "52"
               WHEN    "62"
               WHEN    "72"
      *            和暦
                   IF      LNK-DAY1-WYMD(1:7)  NOT NUMERIC
                       MOVE    20              TO  STS-DAY-RC1
                   ELSE
                       MOVE    LNK-DAY1-WYMD   TO  WK-YYMMDD
                       PERFORM 201-WAREKI-CHK-SEC
                   END-IF
               WHEN    OTHER
                       MOVE    30              TO  STS-DAY-RC1
                       MOVE    01              TO  STS-DAY-RC2
           END-EVALUATE
           .
       DTCONV-EXT.
           EXIT.
      ******************************************************************
      *        西暦編集チェック処理                                    *
      ******************************************************************
       200-SEIREKI-CHK-SEC          SECTION.
      *
      *    日付チェック
           PERFORM YMD-CHK-RTN
           IF      WK-ERR          =   1
               MOVE    20              TO  STS-DAY-RC1
           ELSE
               MOVE    ZERO            TO  IDX-T
      *        和暦変換
               SET     IX1    TO     1
               SEARCH  GENGO-TBL-R2  VARYING  IX1
               AT  END
                   MOVE    20          TO    STS-DAY-RC1
               WHEN
                   GENGO-YYMMDD (IX1)  <=   WK-YYMMDD
                   MOVE    IX1         TO  IDX-T
               END-SEARCH
           END-IF
      *    和暦年変換
           IF     (STS-DAY-RC1         =   ZERO  )
              AND (IDX-T           NOT =   ZERO  )
               SET      IX1            TO  IDX-T
               IF      WK-SYY          <   GENGO-YYYY-RR (IX1)
                   MOVE    20          TO  STS-DAY-RC1
               ELSE
                   COMPUTE WK-SYY          =   WK-SYY
                                           -   GENGO-YYYY-RR (IX1)  +  1
                   IF      WK-SYY          >   99
                       MOVE    20              TO  STS-DAY-RC1
                   END-IF
               END-IF
           END-IF
           IF      STS-DAY-RC1         =   ZERO
               MOVE    GENGO-T1 (IX1)      TO  LNK-DAY1-GENGO
               MOVE    GENGO-T2 (IX1)      TO  LNK-DAY1-GENKIGO
               MOVE    GENMEI   (IX1)      TO  LNK-DAY1-GENMEI
               MOVE    GENGO-T1 (IX1)      TO  WK-WG
               MOVE    WK-YYMMDD           TO  LNK-DAY1-YMD
           END-IF
           .
       200-SEIREKI-CHK-EXT.
           EXIT.
      ******************************************************************
      *        和暦編集チェック処理                                    *
      ******************************************************************
       201-WAREKI-CHK-SEC          SECTION.
      *
           MOVE    WK-WG         TO    LNK-DAY1-GENGO.
           SET     IX1    TO     1.
           SEARCH  GENGO-TBL-R2  VARYING  IX1
             AT  END
               MOVE    30        TO    STS-DAY-RC1
               MOVE    01        TO    STS-DAY-RC2
             WHEN
               GENGO-T1 (IX1)  =  LNK-DAY1-GENGO
               COMPUTE  WK-SYY  =  WK-YY  +  GENGO-YYYY-RR (IX1)  -  1
               MOVE    GENGO-T2 (IX1)    TO    LNK-DAY1-GENKIGO
               MOVE    GENMEI   (IX1)    TO    LNK-DAY1-GENMEI 
             WHEN
               GENGO-T2 (IX1)  =  LNK-DAY1-GENGO
               COMPUTE  WK-SYY  =  WK-YY  +  GENGO-YYYY-RR (IX1)  -  1
               MOVE    GENGO-T1 (IX1)    TO    LNK-DAY1-GENGO
               MOVE    GENGO-T2 (IX1)    TO    LNK-DAY1-GENKIGO
               MOVE    GENMEI   (IX1)    TO    LNK-DAY1-GENMEI
           END-SEARCH
           IF      STS-DAY-RC1         =   ZERO
               PERFORM YMD-CHK-RTN
               IF      WK-ERR          =   1
                   MOVE    20              TO    STS-DAY-RC1
               ELSE
      *            西暦編集
                   MOVE    WK-YYMMDD       TO  LNK-DAY1-YMD
               END-IF
           END-IF
           .
       201-WAREKI-CHK-EXT.
           EXIT.
      ******************************************************************
      *        日付編集処理                       (IRAI = 21 / 22)     *
      ******************************************************************
       DTEDIT-RTN           SECTION.
       DTEDIT-000.
      *
      *    *受取日付の西暦 OR 和暦判定
           IF  LNK-DAY2-IRAI   =   "21"
      *        *西暦日付による場合
               MOVE    LNK-DAY2-IRAI     TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY2-YMD      TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               IF  STS-DAY-RC1  NOT =  ZERO
                   MOVE    SPACE         TO    LNK-DAY1-AREA
                   GO  TO      DTEDIT-EXT
               END-IF
           ELSE
      *        *和暦日付による場合
               MOVE    LNK-DAY2-IRAI     TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY2-YMD      TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               IF  STS-DAY-RC1  NOT =  ZERO
                   MOVE    SPACE         TO    LNK-DAY1-AREA
                   GO  TO      DTEDIT-EXT
               END-IF
               MOVE    LNK-DAY2-YMD      TO    WK-YYMMDD
           END-IF.
      *
           PERFORM  SUUJI-EDT-RTN.
      *
      *    *編集１ → X ZZ.ZZ.ZZ
           MOVE    LNK-DAY1-GENKIGO         TO    EDT-GYMD1-G.
           MOVE    WK-YY                    TO    EDT-GYMD1-Y.
           MOVE    CST-PX                   TO    EDT-GYMD1-P1.
           MOVE    WK-MM                    TO    EDT-GYMD1-M.
           MOVE    CST-PX                   TO    EDT-GYMD1-P2.
           MOVE    WK-DD                    TO    EDT-GYMD1-D.
           MOVE    EDT-GYMD1                TO    LNK-DAY2-EDTYMD1.
      *
      *    *編集２ → NN NN.NN.NN
           MOVE    LNK-DAY1-GENMEI          TO    EDT-GYMD2-G.
           MOVE    EDT-YMD-Y                TO    EDT-GYMD2-Y.
           MOVE    CST-PN                   TO    EDT-GYMD2-P1.
           MOVE    EDT-YMD-M                TO    EDT-GYMD2-M.
           MOVE    CST-PN                   TO    EDT-GYMD2-P2.
           MOVE    EDT-YMD-D                TO    EDT-GYMD2-D.
           MOVE    EDT-GYMD2                TO    LNK-DAY2-EDTYMD2.
           INSPECT LNK-DAY2-EDTYMD2         REPLACING   ALL "  "
                                                        BY  "　". 
      *
      *    *編集３ → NN NN年NN月NN日
           MOVE    LNK-DAY1-GENMEI          TO    EDT-GYMD3-G.
           MOVE    EDT-YMD-Y                TO    EDT-GYMD3-Y.
           MOVE    CST-NEN                  TO    EDT-GYMD3-NEN.
           MOVE    EDT-YMD-M                TO    EDT-GYMD3-M.
           MOVE    CST-TUKI                 TO    EDT-GYMD3-TUKI.
           MOVE    EDT-YMD-D                TO    EDT-GYMD3-D.
           MOVE    CST-HI                   TO    EDT-GYMD3-HI.
           MOVE    EDT-GYMD3                TO    LNK-DAY2-EDTYMD3.
           INSPECT LNK-DAY2-EDTYMD3         REPLACING   ALL "  "
                                                        BY  "　". 
      *
           MOVE    SPACE                    TO    LNK-DAY1-AREA
           .
       DTEDIT-EXT.
           EXIT.
      ******************************************************************
      *        曜日算出処理                       (IRAI = 31 / 32)     *
      ******************************************************************
       DTYOUB-RTN           SECTION.
       DTYOUB-000.
      *
      *    *受取日付の西暦 OR 和暦判定
           IF  LNK-DAY3-IRAI   =   "31"
      *        *西暦日付による場合
               MOVE    LNK-DAY3-YMD      TO    WK-YYMMDD
               PERFORM YMD-CHK-RTN
               IF      WK-ERR      =  1
                   MOVE    20            TO    STS-DAY-RC1
               END-IF
           ELSE
      *        *和暦日付による場合
               MOVE    LNK-DAY3-IRAI     TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY3-YMD      TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
           END-IF
      *
           IF      STS-DAY-RC1         =   ZERO
      *        休日判定
               PERFORM YSK-KBN-RTN
               MOVE    YSK-SYUKJTS             TO  LNK-DAY3-SYUKU
               MOVE    YSK-KYUJTS              TO  LNK-DAY3-YASUMI
      *        曜日設定
               PERFORM YOUBI-SET-RTN
               MOVE    YOUBI-KBN  (IX-YOUBI)   TO  LNK-DAY3-YOBI
               MOVE    YOUBI-KIGO (IX-YOUBI)   TO  LNK-DAY3-YOBIKIGO
               MOVE    YOUBI-MEI  (IX-YOUBI)   TO  LNK-DAY3-YOBIMEI
           END-IF
           .
       DTYOUB-EXT.
           EXIT.
      ******************************************************************
      *        祝日算出処理                       (IRAI = 41 / 42)     *
      ******************************************************************
       DTSKJT-RTN          SECTION.
       DTSKJT-000.
      *
      *    *受取日付の西暦 OR 和暦判定
           IF  LNK-DAY4-IRAI   =   "41"
      *        *西暦日付による場合
               MOVE    LNK-DAY4-YM       TO    WK-YYYYMM
               MOVE    01                TO    WK-DD
               PERFORM  YMD-CHK-RTN
               IF  WK-ERR  =  1
                   MOVE    20            TO    STS-DAY-RC1
                   GO  TO      DTSKJT-EXT
               END-IF
           ELSE
      *        *和暦日付による場合
               MOVE    LNK-DAY4-IRAI     TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY4-YM       TO    LNK-DAY1-YMD
               COMPUTE  LNK-DAY1-YMD  =  LNK-DAY1-YMD  *  100  +  1
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  NOT =  ZERO
                   GO  TO      DTSKJT-EXT
               END-IF
           END-IF.
      *
      *    祝日（春分の日，秋分の日）算出
           IF  WK-MM   =   3   OR   9
               PERFORM SKJT-KEISAN1-RTN
           END-IF.
      *    祝日（成人の日，体育の日）算出
           IF  WK-MM   =   1   OR   10
               PERFORM SKJT-KEISAN2-RTN
           END-IF.
      *    祝日（海の日，敬老の日）算出
           IF  WK-MM   =   7   OR   9
               PERFORM SKJT-KEISAN3-SEC
           END-IF
      *
           PERFORM     VARYING     I       FROM  1  BY  1
                         UNTIL     I       >     5
               MOVE    SKJT-KNRDD (WK-MM, I) TO  LNK-DAY4-SKJTDD (I)
           END-PERFORM.
           MOVE    SKJT-KNRCNT (WK-MM)       TO  LNK-DAY4-COUNT
           .
       DTSKJT-EXT.
           EXIT.
      ******************************************************************
      *        期間日数計算処理                   (IRAI = 51 / 52)     *
      ******************************************************************
       DTKIKN-RTN           SECTION.
       DTKIKN-000.
      *
           MOVE    ZERO                TO  WK-ST
           MOVE    ZERO                TO  WK-END
      *    *受取日付の西暦 OR 和暦判定（開始年月日）
           EVALUATE    LNK-DAY5-IRAI
           WHEN    "51"
      *        *西暦日付による場合
      *       （開始年月日）
               MOVE    LNK-DAY5-START    TO    WK-YYMMDD
               PERFORM YMD-CHK-RTN
               IF      WK-ERR          =   1
                   MOVE    30              TO  STS-DAY-RC1
                   MOVE    02              TO  STS-DAY-RC2
               ELSE
                   MOVE    WK-YYMMDD       TO  WK-ST
               END-IF
      *       （終了年月日）
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY5-END    TO    WK-YYMMDD
                   PERFORM YMD-CHK-RTN
                   IF      WK-ERR          =   1
                       MOVE    30              TO  STS-DAY-RC1
                       MOVE    03              TO  STS-DAY-RC2
                   ELSE
                       MOVE    WK-YYMMDD       TO  WK-END
                   END-IF
               END-IF
      *
           WHEN    "52"
      *        *和暦日付による場合
      *       （開始年月日）
               MOVE    LNK-DAY5-IRAI       TO  LNK-DAY1-IRAI
               MOVE    LNK-DAY5-START      TO  LNK-DAY1-YMD
               PERFORM DTCONV-RTN
               MOVE    SPACE               TO  LNK-DAY1-AREA
               IF      STS-DAY-RC1         =  ZERO
                   MOVE    WK-YYMMDD       TO  WK-ST
               ELSE
                   MOVE    30              TO  STS-DAY-RC1
                   MOVE    02              TO  STS-DAY-RC2
               END-IF
      *       （終了年月日）
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY5-IRAI       TO  LNK-DAY1-IRAI
                   MOVE    LNK-DAY5-END        TO  LNK-DAY1-YMD
                   PERFORM DTCONV-RTN
                   MOVE    SPACE               TO  LNK-DAY1-AREA
                   IF      STS-DAY-RC1         =  ZERO
                       MOVE    WK-YYMMDD       TO  WK-END
                   ELSE
                       MOVE    30              TO  STS-DAY-RC1
                       MOVE    03              TO  STS-DAY-RC2
                   END-IF
               END-IF
           END-EVALUATE
      *
           IF      STS-DAY-RC1         =   ZERO
               IF      WK-ST           >   WK-END
                   MOVE    ZERO            TO  LNK-DAY5-NISSU1
                   MOVE    ZERO            TO  LNK-DAY5-NISSU2
                   MOVE    20              TO  STS-DAY-RC1
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  WK-NISU
           MOVE    ZERO                TO  FLG-SYORI-END
           PERFORM
                   UNTIL  (STS-DAY-RC1 NOT =   ZERO )
                   OR     (WK-ST           =   WK-END )
                   OR     (FLG-SYORI-END   =   1    )
      *        １年分計算
               IF     (WK-ST  +  10000 )   <=  WK-END
                   IF      WK-ST2      <   0301
                       MOVE    WK-ST       TO  WK-YYMMDD
                   ELSE
                       COMPUTE WK-ST1      =   WK-ST1  +  1
                       MOVE    WK-ST       TO  WK-YYMMDD
                   END-IF
      *            閏年判定
                   PERFORM URUDOSHI-CHK-RTN
                   IF      WK-UERR         =   1
                       COMPUTE  WK-NISU    =   WK-NISU   +  366
                   ELSE
                       COMPUTE  WK-NISU    =   WK-NISU   +  365
                   END-IF
                   IF      WK-ST2          <   0301
                       COMPUTE  WK-ST1     =   WK-ST1    +  1
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SYORI-END
               END-IF
           END-PERFORM
           IF      STS-DAY-RC1         =   ZERO
               IF      WK-ST           NOT =   WK-END
      *        月日の計算
                   MOVE    WK-ST1          TO  KISAN-YY
                   MOVE    1               TO  KISAN-MM
                                               KISAN-DD
                   MOVE    WK-ST           TO  SYURYO-YMD
                   PERFORM NISSU-SANSYUTS-RTN
                   COMPUTE WK-NISU         =   WK-NISU  -  WK-SANSYUTS
                   MOVE    WK-END          TO  SYURYO-YMD
                   PERFORM NISSU-SANSYUTS-RTN
                   COMPUTE WK-NISU         =  WK-NISU   +  WK-SANSYUTS
               END-IF
           END-IF
           IF      STS-DAY-RC1         =   ZERO
               MOVE    WK-NISU         TO  LNK-DAY5-NISSU1
               COMPUTE WK-NISU         =   WK-NISU   +  1
               MOVE    WK-NISU         TO  LNK-DAY5-NISSU2
           END-IF
           .
       DTKIKN-EXT.
           EXIT.
      ******************************************************************
      *        該当日算出処理                     (IRAI = 61 / 62)     *
      ******************************************************************
       DTPLMN-RTN          SECTION.
       DTPLMN-000.
      *
      *    *受取日付の西暦 OR 和暦判定
           IF  LNK-DAY6-IRAI   =   "61"
      *        *西暦日付による場合
               MOVE    LNK-DAY6-KIJUN    TO    WK-YYMMDD
               PERFORM  YMD-CHK-RTN
               IF  WK-ERR  =  1
                   MOVE    20            TO    STS-DAY-RC1
                   GO  TO      DTPLMN-EXT
               END-IF
           ELSE
      *        *和暦日付による場合
               MOVE    LNK-DAY6-IRAI     TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KIJUN    TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  NOT =  ZERO
                   GO  TO      DTPLMN-EXT
               END-IF
           END-IF.
      *
      *    *日数パターン判定
           EVALUATE    LNK-DAY6-ZOGENPTN
               WHEN    "1"
                       PERFORM  DTPLMN-DAY-RTN
               WHEN    "2"
                       PERFORM  DTPLMN-MON-RTN
               WHEN    "3"
                       PERFORM  DTPLMN-NEN-RTN
               WHEN    OTHER
                       MOVE    30        TO    STS-DAY-RC1
                       MOVE    03        TO    STS-DAY-RC2
                       GO  TO      DTPLMN-EXT
           END-EVALUATE.
      *
           IF  LNK-DAY6-IRAI   =   "62"
      *        *和暦日付による受取りの場合
               MOVE    "61"              TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KOYOMI   TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD       TO    LNK-DAY6-KOYOMI
               ELSE
                   MOVE  ZERO            TO    LNK-DAY6-KOYOMI
                   GO  TO      DTPLMN-EXT
               END-IF
      *
               MOVE    "61"                TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KOYOMIZEN  TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE               TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD         TO    LNK-DAY6-KOYOMIZEN
               ELSE
                   MOVE  ZERO              TO    LNK-DAY6-KOYOMIZEN
                   GO  TO      DTPLMN-EXT
               END-IF
      *
               MOVE    "61"                TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KOYOMIGO   TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE               TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD         TO    LNK-DAY6-KOYOMIGO
               ELSE
                   MOVE  ZERO              TO    LNK-DAY6-KOYOMIGO
                   GO  TO      DTPLMN-EXT
               END-IF
      *
               MOVE    "61"                TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KEISAN     TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE               TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD         TO    LNK-DAY6-KEISAN
               ELSE
                   MOVE  ZERO              TO    LNK-DAY6-KEISAN
                   GO  TO      DTPLMN-EXT
               END-IF
      *
               MOVE    "61"                TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KEISANZEN  TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE               TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD         TO    LNK-DAY6-KEISANZEN
               ELSE
                   MOVE  ZERO              TO    LNK-DAY6-KEISANZEN
                   GO  TO      DTPLMN-EXT
               END-IF
      *
               MOVE    "61"                TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY6-KEISANGO   TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE               TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD         TO    LNK-DAY6-KEISANGO
               ELSE
                   MOVE  ZERO              TO    LNK-DAY6-KEISANGO
                   GO  TO      DTPLMN-EXT
               END-IF
           END-IF.
       DTPLMN-EXT.
           EXIT.
      ******************************************************************
      *        日数による該当日算出処理
      ******************************************************************
       DTPLMN-DAY-RTN      SECTION.
       DTPLMN-DAY-000.
      *    *計算日
           MOVE     LNK-DAY6-ZOGEN        TO    WK-NISU.
           PERFORM  NISSU-KANSAN-RTN.
           MOVE     WK-YYMMDD             TO    LNK-DAY6-KEISAN.
           PERFORM  YSK-KBN-RTN.
           IF       YSK-KYUJTS         =   1
               PERFORM DT-YUKOMNS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KEISANZEN
               PERFORM DT-YUKOPLS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KEISANGO
           ELSE
               MOVE     WK-YYMMDD          TO  LNK-DAY6-KEISANZEN
                                               LNK-DAY6-KEISANGO
           END-IF
      *
      *    *暦上日
           MOVE     LNK-DAY6-KEISAN       TO    WK-MNS-YMD.
           PERFORM  DT-MINUS-RTN.
           MOVE     WK-MNS-YMD            TO    LNK-DAY6-KOYOMI
                                                WK-YYMMDD.
           PERFORM YSK-KBN-RTN.
           IF      YSK-KYUJTS          =   1
               PERFORM DT-YUKOMNS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KOYOMIZEN
               PERFORM DT-YUKOPLS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KOYOMIGO
           ELSE
               MOVE     WK-YYMMDD          TO  LNK-DAY6-KOYOMIZEN
                                               LNK-DAY6-KOYOMIGO
           END-IF
           .
       DTPLMN-DAY-EXT.
           EXIT.
      ******************************************************************
      *        月数による該当日算出処理
      ******************************************************************
       DTPLMN-MON-RTN      SECTION.
       DTPLMN-MON-000.
           MOVE    LNK-DAY6-ZOGEN      TO    WK-NISU.
           MOVE    WK-NISU       TO    WK-SUUTI.
           DIVIDE  WK-SUUTI  BY  12    GIVING      WK-SYO
                                       REMAINDER   WK-AMARI.
           IF  WK-NISU  <  ZERO
               IF  WK-AMARI  <  WK-MM
                   COMPUTE   WK-MM     =  WK-MM    - WK-AMARI
               ELSE
                   COMPUTE   WK-AMARI  =  WK-AMARI - WK-MM
                   COMPUTE   WK-MM     =  12       - WK-AMARI
                   COMPUTE   WK-SYO    =  WK-SYO   + 1
               END-IF
               COMPUTE   WK-SYY   =    WK-SYY    -   WK-SYO
           ELSE
               ADD       WK-MM         TO    WK-AMARI
               IF  WK-AMARI  >  12
                   COMPUTE   WK-AMARI  =  WK-AMARI - 12
                   MOVE      WK-AMARI  TO    WK-MM
                   COMPUTE   WK-SYO    =  WK-SYO   + 1
               ELSE
                   MOVE      WK-AMARI  TO    WK-MM
               END-IF
               COMPUTE   WK-SYY   =    WK-SYY    +   WK-SYO
           END-IF.
      *
           MOVE    WK-YYMMDD           TO    WK-YUKO.
           PERFORM  DT-MMLAST-RTN.
           IF  WK-DD  >  WK-YUKO-DD
               MOVE     WK-YUKO-DD     TO    WK-DD
           END-IF
      *
      *    *計算日
           MOVE    WK-YYMMDD           TO    LNK-DAY6-KEISAN.
           PERFORM YSK-KBN-RTN
           IF      YSK-KYUJTS      NOT =   1
               MOVE     WK-YYMMDD      TO  LNK-DAY6-KEISANZEN
                                           LNK-DAY6-KEISANGO
           ELSE
               PERFORM DT-YUKOMNS-RTN
               MOVE    WK-YUKO         TO  LNK-DAY6-KEISANZEN
               PERFORM DT-YUKOPLS-RTN
               MOVE    WK-YUKO         TO  LNK-DAY6-KEISANGO
           END-IF
      *
      *    *暦上日
           MOVE     LNK-DAY6-KEISAN   TO    WK-MNS-YMD.
           PERFORM  DT-MINUS-RTN.
           MOVE     WK-MNS-YMD        TO    LNK-DAY6-KOYOMI
                                            WK-YYMMDD.
           PERFORM YSK-KBN-RTN.
           IF      YSK-KYUJTS      NOT =   1
               MOVE    WK-YYMMDD       TO  LNK-DAY6-KOYOMIZEN
                                           LNK-DAY6-KOYOMIGO
           ELSE
               PERFORM  DT-YUKOMNS-RTN
               MOVE     WK-YUKO        TO  LNK-DAY6-KOYOMIZEN
               PERFORM  DT-YUKOPLS-RTN
               MOVE     WK-YUKO        TO  LNK-DAY6-KOYOMIGO
           END-IF
           .
       DTPLMN-MON-EXT.
           EXIT.
      ******************************************************************
      *        年数による該当日算出処理
      ******************************************************************
       DTPLMN-NEN-RTN      SECTION.
       DTPLMN-NEN-000.
           MOVE    LNK-DAY6-ZOGEN      TO    WK-NISU.
           COMPUTE    WK-SYY      =    WK-SYY    +   LNK-DAY6-ZOGEN
      *
           MOVE    WK-YYMMDD           TO    WK-YUKO.
           PERFORM  DT-MMLAST-RTN.
           IF  WK-DD  >  WK-YUKO-DD
               MOVE     WK-YUKO-DD     TO    WK-DD
           END-IF.
      *
      *    *計算日
           MOVE     WK-YYMMDD          TO    LNK-DAY6-KEISAN.
           PERFORM  YSK-KBN-RTN
           IF      YSK-KYUJTS          =   1
               PERFORM DT-YUKOMNS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KEISANZEN
               PERFORM DT-YUKOPLS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KEISANGO
           ELSE
               MOVE    WK-YYMMDD           TO  LNK-DAY6-KEISANZEN
               MOVE    WK-YYMMDD           TO  LNK-DAY6-KEISANGO
           END-IF
      *
      *    *暦上日
           MOVE     LNK-DAY6-KEISAN    TO    WK-MNS-YMD.
           PERFORM  DT-MINUS-RTN.
           MOVE     WK-MNS-YMD         TO    LNK-DAY6-KOYOMI
                                             WK-YYMMDD.
           PERFORM  YSK-KBN-RTN.
           IF      YSK-KYUJTS          =   1
               PERFORM DT-YUKOMNS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KOYOMIZEN
               PERFORM DT-YUKOPLS-RTN
               MOVE    WK-YUKO             TO  LNK-DAY6-KOYOMIGO
           ELSE
               MOVE    WK-YYMMDD           TO  LNK-DAY6-KOYOMIZEN
               MOVE    WK-YYMMDD           TO  LNK-DAY6-KOYOMIGO
           END-IF
      *
           .
       DTPLMN-NEN-EXT.
           EXIT.
      ******************************************************************
      *        月末日算出処理                     (IRAI = 71 / 72)     *
      ******************************************************************
       DTMEND-RTN          SECTION.
       DTMEND-000.
      *
      *    *受取日付の西暦 OR 和暦判定
           IF  LNK-DAY7-IRAI   =   "71"
      *        *西暦日付による場合
               MOVE    LNK-DAY7-YM       TO    WK-YYYYMM
               MOVE    01                TO    WK-DD
               PERFORM  YMD-CHK-RTN
               IF  WK-ERR  =  1
                   MOVE    20            TO    STS-DAY-RC1
                   GO  TO      DTMEND-EXT
               END-IF
           ELSE
      *        *和暦日付による場合
               MOVE    LNK-DAY7-IRAI     TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY7-YM       TO    LNK-DAY1-YMD
               COMPUTE  LNK-DAY1-YMD   =  LNK-DAY1-YMD  *  100  +  1
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
               IF  STS-DAY-RC1  NOT =  ZERO
                   GO  TO      DTMEND-EXT
               END-IF
           END-IF.
      *
           IF  WK-MM  =  4  OR  6  OR  9  OR  11
               MOVE    30              TO    WK-DD
           ELSE
               IF  WK-MM  =  2
                   MOVE    29              TO    WK-DD
               ELSE
                   MOVE    31              TO    WK-DD
               END-IF
           END-IF.
           PERFORM  YMD-CHK-RTN.
           IF  WK-ERR  =  1
               IF  WK-MM  =  2
                   MOVE    28              TO    WK-DD
                   PERFORM  YMD-CHK-RTN
                   IF  WK-ERR  =  1
                       MOVE    20          TO    STS-DAY-RC1
                       GO  TO  DTMEND-EXT
                   ELSE
                       CONTINUE
                   END-IF
               ELSE
                   MOVE    20              TO    STS-DAY-RC1
                   GO  TO  DTMEND-EXT
               END-IF
           END-IF.
      *
           PERFORM  YSK-KBN-RTN.
           MOVE    WK-YYMMDD       TO    LNK-DAY7-KOYOMI.
           IF  YSK-KYUJTS  =  1
               PERFORM  DT-YUKOMNS-RTN
               MOVE     WK-YUKO    TO    WK-YYMMDD
           END-IF.
           MOVE    WK-YYMMDD       TO    LNK-DAY7-EIGYO.
      *
           IF  LNK-DAY7-IRAI   =   "72"
      *        *和暦日付による受取りの場合
               MOVE    "71"              TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY7-KOYOMI   TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
               IF      STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD       TO    LNK-DAY7-KOYOMI
               ELSE
                   MOVE  ZERO            TO    LNK-DAY7-KOYOMI
                   GO  TO      DTMEND-EXT
               END-IF
      *
               MOVE    "71"              TO    LNK-DAY1-IRAI
               MOVE    LNK-DAY7-EIGYO    TO    LNK-DAY1-YMD
               PERFORM  DTCONV-RTN
               MOVE    SPACE             TO    LNK-DAY1-AREA
               IF      STS-DAY-RC1  =  ZERO
                   MOVE  WK-YYMMDD       TO    LNK-DAY7-EIGYO
               ELSE
                   MOVE  ZERO            TO    LNK-DAY7-EIGYO
               END-IF
           END-IF.
       DTMEND-EXT.
           EXIT.
      ******************************************************************
      *        日付チェック処理
      ******************************************************************
       YMD-CHK-RTN         SECTION.
       YMD-CHK-000.
      *
           MOVE    ZERO                TO    WK-ERR.
      *    数値チェック
           IF     (WK-MM (1:2)  IS  NOT  NUMERIC )
               OR (WK-DD (1:2)  IS  NOT  NUMERIC )
               MOVE    1               TO    WK-ERR
           END-IF
      *    月チェック
           IF      WK-ERR              =   ZERO
               IF     (WK-MM           <   1   )
                  OR  (WK-MM           >   12  )
                   MOVE    1               TO    WK-ERR
               END-IF
           END-IF
           IF      WK-ERR              =   ZERO
      *        日チェック
               EVALUATE    WK-MM
                   WHEN    2
      *                閏年判定
                       PERFORM URUDOSHI-CHK-RTN
                       IF      WK-UERR         =   ZERO
                           IF     (WK-DD       <   1  )
                               OR (WK-DD       >   28 )
                               MOVE    1               TO    WK-ERR
                           END-IF
                       ELSE
                           IF     (WK-DD       <   1  )
                               OR (WK-DD       >   29 )
                               MOVE    1               TO    WK-ERR
                           END-IF
                       END-IF
                   WHEN    4
                   WHEN    6
                   WHEN    9
                   WHEN    11
                       IF     (WK-DD       <   1  )
                           OR (WK-DD       >   30 )
                           MOVE    1               TO    WK-ERR
                       END-IF
                   WHEN    OTHER
                       IF     (WK-DD       <   1  )
                           OR (WK-DD       >   31 )
                           MOVE    1               TO    WK-ERR
                       END-IF
               END-EVALUATE
           END-IF
           .
       YMD-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *        潤年判定処理
      ******************************************************************
       URUDOSHI-CHK-RTN    SECTION.
       URUDOSHI-CHK-000.
           MOVE    ZERO           TO    WK-UERR.
      *
      *     COMPUTE  WK-UERR  =  WK-SYY  -  (WK-SYY  /  4  *  4).
           DIVIDE  4  INTO  WK-SYY  GIVING WK-DUMMYN
                      REMAINDER WK-UERR.
           IF  WK-UERR  =  ZERO
      *         COMPUTE  WK-UERR  =  WK-SYY  -  (WK-SYY  /  100  *  100)
               DIVIDE  100  INTO  WK-SYY  GIVING WK-DUMMYN
                         REMAINDER WK-UERR
               IF  WK-UERR  =  ZERO
      *             COMPUTE  WK-UERR  =
      *                      WK-SYY  -  (WK-SYY  /  400  *  400)
                   DIVIDE  400  INTO  WK-SYY  GIVING WK-DUMMYN
                             REMAINDER WK-UERR
                   IF  WK-UERR  NOT =  ZERO
                       MOVE    ZERO              TO  WK-UERR
                   ELSE
                       MOVE     1                TO  WK-UERR
                   END-IF
               ELSE
                   MOVE     1          TO  WK-UERR
               END-IF
           ELSE
               MOVE        ZERO        TO  WK-UERR
           END-IF.
       URUDOSHI-CHK-EXT.
           EXIT.
      ******************************************************************
      *        曜日・祝日区分・休日区分  算出処理
      ******************************************************************
       YSK-KBN-RTN         SECTION.
       YSK-KBN-000.
      *    祝日（春分の日，秋分の日）算出
           PERFORM SKJT-KEISAN1-RTN.
      *    祝日（成人の日，体育の日）算出
           PERFORM SKJT-KEISAN2-RTN.
      *    祝日（海の日，敬老の日）算出
           PERFORM SKJT-KEISAN3-SEC
      *
           MOVE    ZERO          TO    YSK-SYUKJTS   YSK-KYUJTS.
           MOVE    WK-YYMMDD     TO    WK-YOUB.
      *    祝日テーブル検索
           PERFORM  TABLE-SRH-RTN.
           IF  YSK-SYUKJTS  =  ZERO
      *        月曜の振替休日判定
               IF  YSK-YOUBI  =  1
                   MOVE    WK-YOUB       TO    WK-MNS-YMD
                   PERFORM  DT-MINUS-RTN
                   MOVE    WK-MNS-YMD    TO    WK-YOUB
                   PERFORM  TABLE-SRH-RTN
               END-IF
           END-IF.
           IF  YSK-YOUBI  =  ZERO
      *        日曜日
               MOVE    1                 TO    YSK-KYUJTS
           ELSE
               IF  YSK-SYUKJTS  =  1
                   MOVE    1             TO    YSK-KYUJTS
               ELSE
      *            12/29から1/3まで休日
                   SET     IX4    TO     1
                   SEARCH  KYUJT-TBL     VARYING  IX4
                     AT  END
                       MOVE    ZERO      TO    YSK-KYUJTS
                     WHEN
                       KYUJT-T2 (IX4)  =  WK-SMD
                       MOVE    1         TO  YSK-KYUJTS
                   END-SEARCH
               END-IF
           END-IF.
      *H27.10
      *    H27(2015) 5/6,9/22を休日とする
           IF     (WK-SYY              =   2015 )
              AND (WK-SMD              =   0922
                                       OR  0506 )
               MOVE    1               TO  YSK-KYUJTS
           END-IF.
      *H27.12
      *TABLE-SRH-RTN に移動
      *    山の日
      *    H28(2016) から8/11を山の日
      *    IF     (WK-SYY              >=  2016 )
      *       AND (WK-SMD              =   0811 )
      *        MOVE    1               TO  YSK-KYUJTS
      *    END-IF.
      *
      *    IF  WK-SYYMMDD  >  "19830731"
      *        IF  YSK-YOUBI  =  6
      *            IF  (WK-DD  >  7)  AND  (WK-DD  <  15)
      *                MOVE    1             TO    YSK-KYUJTS.
      *    IF  WK-SYYMMDD  >  "19860731"
      *        IF  YSK-YOUBI  =  6
      *            IF  (WK-DD  >  7)  AND  (WK-DD  <  22)
      *                MOVE    1             TO    YSK-KYUJTS.
      *    IF  WK-SYYMMDD  >  "19890131"
      *        IF  YSK-YOUBI  =  6
      *            MOVE    1             TO    YSK-KYUJTS.
       YSK-KBN-EXT.
           EXIT.
      ******************************************************************
      *        テーブル検索処理
      ******************************************************************
       TABLE-SRH-RTN       SECTION.
       TABLE-SRH-000.
      *
      *H31.1
      *    2019年の祝日
           IF      WK-SYY          =   2019
               PERFORM 2019-TABLE-SRH-SEC
               GO      TO      TABLE-SRH-EXT
           END-IF
      *!!
      *R01.12
      *    2020年の祝日
           IF      WK-SYY          =   2020
               PERFORM 2020-TABLE-SRH-SEC
               GO      TO      TABLE-SRH-EXT
           END-IF
      *!!
      *
           EVALUATE    TRUE
               WHEN    WK-SYY  <  2000
                   SET    IX2    TO     1
                   SEARCH  SKJT-TBL     VARYING  IX2
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-T1 (IX2)  =  WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
      *
               WHEN   (WK-SYY  >= 2000   )  AND
                      (WK-SYY  <  2003   )
                   SET    IX6    TO     1
                   SEARCH  SKJT-TBLNEW   VARYING   IX6
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-NEW(IX6)     =   WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
      *
               WHEN   (WK-SYY  >= 2003   )
                  AND (WK-SYY  <  2020   )
                   SET    IX7    TO     1
                   SEARCH  SKJT-TBLNEW2  VARYING   IX7
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-NEW2(IX7)    =   WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
      *        2020 から
               WHEN   (WK-SYY  >= 2020   )
                   SET    IX23   TO     1
                   SEARCH  SKJT-TBLNEW3  VARYING   IX23
                     AT  END
                         MOVE    ZERO      TO    YSK-SYUKJTS
                     WHEN
                         SKJT-NEW3(IX23)   =   WYOUB-MMDD
                         MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
           END-EVALUATE
      *
      *    春分の日・秋分の日
           IF  YSK-SYUKJTS  =  ZERO
               SET     IX3    TO     1
               SEARCH  SKJT-TBL2     VARYING  IX3
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL2 (IX3)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
           END-IF.
      *    成人の日・体育の日
           IF  (YSK-SYUKJTS  =   ZERO ) AND
               (WK-SYY       >=  2000 )
               SET     IX5    TO     1
               SEARCH  SKJT-TBL3     VARYING  IX5
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL3 (IX5)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
                END-SEARCH
           END-IF.
      *    海の日・敬老の日
           IF  (YSK-SYUKJTS  =   ZERO ) AND
               (WK-SYY       >=  2003 )
               SET     IX8    TO     1
               SEARCH  SKJT-TBL4     VARYING  IX8
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL4 (IX8)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
                END-SEARCH
           END-IF
      *H27.12
      *    山の日
      *    H28(2016) から8/11を山の日
           IF     (WK-SYY              >=  2016 )
               IF    (WYOUB-MMDD       =   0811 )
                   MOVE    1               TO  YSK-KYUJTS
               END-IF
           END-IF
           .
       TABLE-SRH-EXT.
           EXIT.
      *
      *H31.1
      ******************************************************************
      *        2019年の祝日算出処理
      ******************************************************************
       2019-TABLE-SRH-SEC        SECTION.
      *
      *    ２０１９年の祝日テーブル（固定日）
           SET    IX19    TO     1
              SEARCH  SKJT-TBLNEW19  VARYING   IX19
              AT  END
                     MOVE    ZERO      TO    YSK-SYUKJTS
                 WHEN
                     SKJT-NEW19(IX19)   =   WYOUB-MMDD
                     MOVE    1         TO    YSK-SYUKJTS
           END-SEARCH
      *
      *    春分の日・秋分の日
           IF  YSK-SYUKJTS  =  ZERO
               SET     IX3    TO     1
               SEARCH  SKJT-TBL2     VARYING  IX3
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL2 (IX3)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
               END-SEARCH
           END-IF.
      *    成人の日・体育の日
           IF  (YSK-SYUKJTS  =   ZERO ) AND
               (WK-SYY       >=  2000 )
               SET     IX5    TO     1
               SEARCH  SKJT-TBL3     VARYING  IX5
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL3 (IX5)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
                END-SEARCH
           END-IF.
      *    海の日・敬老の日
           IF  (YSK-SYUKJTS  =   ZERO ) AND
               (WK-SYY       >=  2003 )
               SET     IX8    TO     1
               SEARCH  SKJT-TBL4     VARYING  IX8
                 AT  END
                   MOVE   ZERO       TO    YSK-SYUKJTS
                 WHEN
                   SKJT-TBL4 (IX8)  =  WYOUB-MMDD
                   MOVE    1         TO    YSK-SYUKJTS
                END-SEARCH
           END-IF
           .
       2019-TABLE-SRH-EXT.
           EXIT.
      **
      *
      *R01.12
      ******************************************************************
      *        2020年の祝日算出処理
      ******************************************************************
       2020-TABLE-SRH-SEC        SECTION.
      *
      *    ２０２０年の祝日テーブル（固定日）
           SET    IX20    TO     1
              SEARCH  SKJT-TBLNEW20  VARYING   IX20
              AT  END
                     MOVE    ZERO      TO    YSK-SYUKJTS
                 WHEN
                     SKJT-NEW20(IX20)   =   WYOUB-MMDD
                     MOVE    1         TO    YSK-SYUKJTS
           END-SEARCH
      *
           .
       2020-TABLE-SRH-EXT.
           EXIT.
      **
      *
      ******************************************************************
      *        前日算出処理
      ******************************************************************
       DT-MINUS-RTN        SECTION.
       DT-MINUS-000.
           IF  WK-MNS-DD  >  1
               COMPUTE  WK-MNS-DD  =  WK-MNS-DD  -  1
           ELSE
               IF  WK-MNS-MM  =  1
                   MOVE    12            TO    WK-MNS-MM
                   COMPUTE  WK-MNS-YY  =  WK-MNS-YY  -  1
                   MOVE    31            TO    WK-MNS-DD
               ELSE
                   COMPUTE  WK-MNS-MM  =  WK-MNS-MM  -  1
                   MOVE    WK-MNS-YMD    TO    WK-YUKO
                   PERFORM  DT-MMLAST-RTN
                   MOVE    WK-YUKO-DD    TO    WK-MNS-DD
               END-IF
           END-IF.
       DT-MINUS-EXT.
           EXIT.
      *
      ******************************************************************
      *        月末日算出処理
      ******************************************************************
       DT-MMLAST-RTN       SECTION.
       DT-MMLAST-000.
           IF  WK-YUKO-MM  =  2
               PERFORM  URUDOSHI-CHK-RTN
               IF  WK-UERR  =  1
                   MOVE    29            TO    CAL-T (2)
               ELSE
                   MOVE    28            TO    CAL-T (2)
               END-IF
           END-IF.
           MOVE    CAL-T (WK-YUKO-MM)    TO    WK-YUKO-DD.
       DT-MMLAST-EXT.
           EXIT.
      ******************************************************************
      *        前日有効日算出処理
      ******************************************************************
       DT-YUKOMNS-RTN      SECTION.
       DT-YUKOMNS-000.
           IF  WK-DD  =  1
               IF  WK-MM  =  1
                   MOVE    12            TO    WK-MM
                   MOVE    31            TO    WK-DD
                   COMPUTE  WK-SYY =  WK-SYY -  1
               ELSE
                   COMPUTE  WK-MM  =  WK-MM  -  1
                   MOVE    WK-YYMMDD     TO    WK-YUKO
                   PERFORM  DT-MMLAST-RTN
                   MOVE    WK-YUKO-DD    TO    WK-DD
               END-IF
           ELSE
               COMPUTE  WK-DD  =  WK-DD  -  1
           END-IF.
           PERFORM  YSK-KBN-RTN.
           IF  YSK-KYUJTS  =  1
               GO  TO    DT-YUKOMNS-000
           END-IF.
           MOVE    WK-YYMMDD     TO    WK-YUKO.
       DT-YUKOMNS-EXT.
           EXIT.
      ******************************************************************
      *        翌日有効日算出処理
      ******************************************************************
       DT-YUKOPLS-RTN      SECTION.
       DT-YUKOPLS-000.
           MOVE    WK-YYMMDD     TO    WK-YUKO.
           PERFORM  DT-MMLAST-RTN.
           IF  WK-DD  =  WK-YUKO-DD
               IF  WK-MM  =  12
                   MOVE    1             TO    WK-MM  WK-DD
                   COMPUTE  WK-SYY =  WK-SYY +  1
               ELSE
                   COMPUTE  WK-MM  =  WK-MM  +  1
                   MOVE    1             TO    WK-DD
               END-IF
           ELSE
               COMPUTE  WK-DD  =  WK-DD  +  1
           END-IF.
           PERFORM  YSK-KBN-RTN.
           IF  YSK-KYUJTS  =  1
               GO  TO    DT-YUKOPLS-000
           END-IF.
           MOVE    WK-YYMMDD     TO    WK-YUKO
           .
       DT-YUKOPLS-EXT.
           EXIT.
      ******************************************************************
      *        日数算出処理
      ******************************************************************
       NISSU-SANSYUTS-RTN  SECTION.
       NISSU-SANSYUTS-000.
           MOVE    NISU-T (KISAN-MM)   TO    NISU-A.
           IF  KISAN-MM  >  2
               MOVE    KISAN-YMD       TO    WK-YYMMDD
               PERFORM  URUDOSHI-CHK-RTN
               IF  WK-UERR  =  1
                   COMPUTE  NISU-A  =  NISU-A  +  1
               END-IF
           END-IF.
           COMPUTE  NISU-A  =  NISU-A  +  KISAN-DD.
           MOVE    ZERO            TO  NISU-B.
           MOVE    KISAN-YMD       TO  WK-YYMMDD.
      ********************
      *NISSU-SANSYUTS-100.
      ********************
           PERFORM
                   UNTIL   WK-SYY      =   SYURYO-YY
               PERFORM URUDOSHI-CHK-RTN
               IF      WK-UERR         =  1
                   COMPUTE  NISU-B  =  NISU-B  +  366
               ELSE
                   COMPUTE  NISU-B  =  NISU-B  +  365
               END-IF
               COMPUTE  WK-SYY  =  WK-SYY  +  1
           END-PERFORM
      *
      ********************
      *NISSU-SANSYUTS-200.
      ********************
           COMPUTE  NISU-B  =  NISU-B  +  NISU-T (SYURYO-MM)
           IF  SYURYO-MM  >  2
               PERFORM  URUDOSHI-CHK-RTN
               IF  WK-UERR  =  1
                   COMPUTE  NISU-B  =  NISU-B  +  1
               END-IF
           END-IF.
           COMPUTE  NISU-B  =  NISU-B  +  SYURYO-DD.
           COMPUTE  WK-SANSYUTS  =  NISU-B  -  NISU-A.
      *
       NISSU-SANSYUTS-EXT.
           EXIT.
      ******************************************************************
      *        日数換算処理
      ******************************************************************
       NISSU-KANSAN-RTN    SECTION.
       NISSU-KANSAN-000.
           IF  WK-NISU  <  ZERO
               COMPUTE  KIJYUN-A  =  WK-NISU  /  365
               COMPUTE  KIJYUN-YY =  WK-SYY   +  KIJYUN-A  -  1
           ELSE
               MOVE    WK-SYY          TO    KIJYUN-YY
           END-IF.
           MOVE    WK-YYMMDD           TO  SYURYO-YMD.
           MOVE    KIJYUN-YY           TO  KISAN-YY.
           MOVE    1                   TO  KISAN-MM.
           MOVE    ZERO                TO  KISAN-DD.
           PERFORM  NISSU-SANSYUTS-RTN.
           COMPUTE  WK-SANSYUTS  =  WK-SANSYUTS  +  WK-NISU.
      ******************
      *NISSU-KANSAN-100.
      ******************
           MOVE    ZERO                TO  FLG-SYORI-END
           PERFORM
                   UNTIL   FLG-SYORI-END   =   1
               MOVE    KIJYUN-YY       TO  WK-SYY
               PERFORM URUDOSHI-CHK-RTN
               IF      WK-UERR         =   1
                   MOVE    366             TO    SYU-W
               ELSE
                   MOVE    365             TO    SYU-W
               END-IF
               IF      WK-SANSYUTS     >   SYU-W
                   COMPUTE WK-SANSYUTS     =   WK-SANSYUTS  -  SYU-W
                   COMPUTE KIJYUN-YY       =   KIJYUN-YY    +  1
               ELSE
                   MOVE    1              TO  FLG-SYORI-END
               END-IF
           END-PERFORM
           IF  WK-UERR  =  1
               MOVE    29              TO    CAL-T (2)
           ELSE
               MOVE    28              TO    CAL-T (2)
           END-IF.
           MOVE    1                   TO  WK-MM.
      *
           MOVE    ZERO                TO  FLG-SYORI-END
           PERFORM
                   UNTIL   FLG-SYORI-END   =   1
               IF      CAL-T (WK-MM)  <  WK-SANSYUTS
                   COMPUTE WK-SANSYUTS     =   WK-SANSYUTS
                                           -   CAL-T (WK-MM)
                   COMPUTE WK-MM           =   WK-MM   +  1
                   IF      WK-MM       >   12
                       MOVE    1           TO   WK-MM
                       MOVE    1           TO   WK-DD
                   END-IF
               ELSE
                   MOVE    WK-SANSYUTS     TO  WK-DD
                   MOVE    1               TO  FLG-SYORI-END
               END-IF
           END-PERFORM
           .
       NISSU-KANSAN-EXT.
           EXIT.
      ******************************************************************
      *    春分の日・秋分の日を算出する                                *
      ******************************************************************
       SKJT-KEISAN1-RTN  SECTION.
       SKJT-KEISAN1-000.
           IF  WK-MM  <  3
               COMPUTE  WK-YSK-YYYY  =  WK-SYY   -  1
               COMPUTE  WK-YSK-MM    =  WK-MM  +  10
           ELSE
               MOVE    WK-SYY          TO    WK-YSK-YYYY
               COMPUTE  WK-YSK-MM    =  WK-MM  -  2
           END-IF.
           COMPUTE  WK-SZ       =  2.6  *  WK-YSK-MM  -  0.2.
           COMPUTE  WK-SY       =  (WK-YSK-YYYY  -  1900)  *  0.25.
           COMPUTE  WK-SY       =  WK-DD  +  WK-SZ  +  WK-SY  +
                                   (WK-YSK-YYYY  -  1934).
      *    COMPUTE  YSK-YOUBI   =  WK-SY  -  WK-SY  /  7  *  7.
           DIVIDE  7  INTO  WK-SY  GIVING  WK-DUMMYN
                      REMAINDER  YSK-YOUBI.
           COMPUTE  WK-WORKA    =  WK-SYY  -  1960.
           COMPUTE  WK-SSYB     =  WK-WORKA  *  CST1  +  CST2.
           COMPUTE  WK-SY       =  WK-WORKA  /  4.
      *     COMPUTE  WK-SZ       =  (WK-WORKA  -  WK-WORKA  /  4  *  4).
           DIVIDE  4  INTO  WK-WORKA  GIVING  WK-DUMMYN
                      REMAINDER  WK-SZ.
           COMPUTE  WK-SSYB     =  WK-SSYB  -  (WK-SY  *  1461)  -
                                   (WK-SZ  *  365).
           SET     IX3           TO    1.
           MOVE    3             TO    SKJT-M (IX3).
           MOVE    WK-SSYB       TO    SKJT-D (IX3)
                                       SKJT-KNRDD (3, 1).
      *
           SET     IX3           TO    2.
           MOVE    9             TO    SKJT-M (IX3).
           COMPUTE  WK-SSYB     =  WK-SSYB  +  CST3.
           IF  WK-SYY  =  2012 
               MOVE  22  TO  WK-SSYB
           END-IF.
           MOVE    WK-SSYB       TO    SKJT-D (IX3)
                                       SKJT-KNRDD (9, 2).
       SKJT-KEISAN1-EXT.
           EXIT.
      ******************************************************************
      *    成人の日  体育の日を算出する                                *
      ******************************************************************
       SKJT-KEISAN2-RTN  SECTION.
       SKJT-KEISAN2-000.
           SET     IX5              TO  1.
           MOVE    WK-SYY           TO  WK-SKJTYY.
           MOVE    1                TO  WK-SKJTDD.
       SKJT-KEISAN2-100.
           IF      IX5          =   1
                MOVE    1           TO  WK-SKJTMM
           ELSE
                MOVE    10          TO  WK-SKJTMM
           END-IF.
      *
           IF    WK-SKJTMM      <   3
               COMPUTE  WK-YSK-YYYY     =   WK-SKJTYY   -   1
               COMPUTE  WK-YSK-MM       =   WK-SKJTMM   +   10
           ELSE
               MOVE    WK-SKJTYY            TO  WK-YSK-YYYY
               COMPUTE WK-YSK-MM        =   WK-SKJTMM   -   2
           END-IF.
           COMPUTE WK-SZ   =  2.6 * WK-YSK-MM  -  0.2.
           COMPUTE WK-SY   =  (WK-YSK-YYYY -  1900)  *  0.25.
           COMPUTE WK-SY   =  WK-SKJTDD  +  WK-SZ  +  WK-SY  +
                              (WK-YSK-YYYY    -   1934).
      *     COMPUTE YSK-YOUBI2 =  WK-SY   -  WK-SY  /   7  *  7.
           DIVIDE  7  INTO  WK-SY  GIVING  WK-DUMMYN
                      REMAINDER  YSK-YOUBI2.
      *
           MOVE    WK-SKJTMM          TO  SKJT-M3(IX5).
           EVALUATE     YSK-YOUBI2
             WHEN       0   MOVE  9   TO  SKJT-D3(IX5)
             WHEN       1   MOVE  8   TO  SKJT-D3(IX5)
             WHEN       2   MOVE 14   TO  SKJT-D3(IX5)
             WHEN       3   MOVE 13   TO  SKJT-D3(IX5)
             WHEN       4   MOVE 12   TO  SKJT-D3(IX5)
             WHEN       5   MOVE 11   TO  SKJT-D3(IX5)
             WHEN       6   MOVE 10   TO  SKJT-D3(IX5)
           END-EVALUATE.
      *
           IF    WK-SKJTMM  =   1
                 MOVE  SKJT-D3(IX5)   TO  SKJT-KNRDD (1, 2)
           END-IF.
           IF    WK-SKJTMM  =   10
                 MOVE  SKJT-D3(IX5)   TO  SKJT-KNRDD (10, 1)
           END-IF.
      *
           IF    IX5        =   2
               GO  TO  SKJT-KEISAN2-EXT
           ELSE
               SET     IX5      TO  2
               GO  TO  SKJT-KEISAN2-100
           END-IF.
       SKJT-KEISAN2-EXT.
           EXIT.
      ******************************************************************
      *    海の日  敬老の日を算出する                                *
      ******************************************************************
       SKJT-KEISAN3-SEC  SECTION.
           SET     IX8              TO  1.
           MOVE    WK-SYY           TO  WK-SKJTYY
           MOVE    1                TO  WK-SKJTDD
           PERFORM VARYING  IDX     FROM    1   BY  1
                   UNTIL    IDX     >   2
               IF      IDX          =   1
                   MOVE    7            TO  WK-SKJTMM
               ELSE
                   MOVE    9            TO  WK-SKJTMM
               END-IF
      *
               SET     IX8              TO  IDX
               IF    WK-SKJTMM      <   3
                   COMPUTE  WK-YSK-YYYY     =   WK-SKJTYY   -   1
                   COMPUTE  WK-YSK-MM       =   WK-SKJTMM   +   10
               ELSE
                   MOVE    WK-SKJTYY            TO  WK-YSK-YYYY
                   COMPUTE WK-YSK-MM        =   WK-SKJTMM   -   2
               END-IF
               COMPUTE WK-SZ   =  2.6 * WK-YSK-MM  -  0.2
               COMPUTE WK-SY   =  (WK-YSK-YYYY -  1900)  *  0.25
               COMPUTE WK-SY   =  WK-SKJTDD  +  WK-SZ  +  WK-SY  +
                                  (WK-YSK-YYYY    -   1934)
               DIVIDE  7  INTO  WK-SY  GIVING  WK-DUMMYN
                          REMAINDER  YSK-YOUBI2
      *
               MOVE    WK-SKJTMM          TO  SKJT-M4(IX8)
               EVALUATE    YSK-YOUBI2
                   WHEN    0       MOVE  16   TO  SKJT-D4(IX8)
                   WHEN    1       MOVE  15   TO  SKJT-D4(IX8)
                   WHEN    2       MOVE  21   TO  SKJT-D4(IX8)
                   WHEN    3       MOVE  20   TO  SKJT-D4(IX8)
                   WHEN    4       MOVE  19   TO  SKJT-D4(IX8)
                   WHEN    5       MOVE  18   TO  SKJT-D4(IX8)
                   WHEN    6       MOVE  17   TO  SKJT-D4(IX8)
               END-EVALUATE
      *
               IF      WK-SKJTMM  =   7
                   MOVE    SKJT-D4(IX8)    TO  SKJT-KNRDD (7, 1)
               END-IF
               IF      WK-SKJTMM  =   9
                   MOVE    SKJT-D4(IX8)    TO  SKJT-KNRDD (9, 1)
               END-IF
           END-PERFORM
           .
       SKJT-KEISAN3-EXT.
           EXIT.
      *******************************************************************
      *        数字変換処理
      ******************************************************************
       SUUJI-EDT-RTN    SECTION.
       SUUJI-EDT-000.
           MOVE    WK-YYMMDD       TO      EDT-GYMD.
           MOVE    EDT-GYMD-Y      TO      EDT-GYMDZ-Y.
           MOVE    EDT-GYMD-M      TO      EDT-GYMDZ-M.
           MOVE    EDT-GYMD-D      TO      EDT-GYMDZ-D.
           MOVE    EDT-GYMDZ       TO      EDT-GYMDX.
      *
           MOVE    SPACE           TO      EDT-YMD.
           PERFORM     VARYING     I       FROM  1  BY  1
                         UNTIL     I       >     6
               EVALUATE  EDT-GYMDX-OC ( I )
                   WHEN  " "   MOVE  SUUJIN(1)   TO  EDT-YMD-OC (I)
                   WHEN  "0"   MOVE  SUUJIN(2)   TO  EDT-YMD-OC (I)
                   WHEN  "1"   MOVE  SUUJIN(3)   TO  EDT-YMD-OC (I)
                   WHEN  "2"   MOVE  SUUJIN(4)   TO  EDT-YMD-OC (I)
                   WHEN  "3"   MOVE  SUUJIN(5)   TO  EDT-YMD-OC (I)
                   WHEN  "4"   MOVE  SUUJIN(6)   TO  EDT-YMD-OC (I)
                   WHEN  "5"   MOVE  SUUJIN(7)   TO  EDT-YMD-OC (I)
                   WHEN  "6"   MOVE  SUUJIN(8)   TO  EDT-YMD-OC (I)
                   WHEN  "7"   MOVE  SUUJIN(9)   TO  EDT-YMD-OC (I)
                   WHEN  "8"   MOVE  SUUJIN(10)  TO  EDT-YMD-OC (I)
                   WHEN  "9"   MOVE  SUUJIN(11)  TO  EDT-YMD-OC (I)
               END-EVALUATE
           END-PERFORM.
       SUUJI-EDT-EXT.
           EXIT.
      ******************************************************************
      *        曜日変換処理
      ******************************************************************
       YOUBI-SET-RTN    SECTION.
       YOUBI-SET-000.
           SET   IX-YOUBI       TO  1.
           EVALUATE    YSK-YOUBI
               WHEN    1        SET   IX-YOUBI   TO   1
               WHEN    2        SET   IX-YOUBI   TO   2
               WHEN    3        SET   IX-YOUBI   TO   3
               WHEN    4        SET   IX-YOUBI   TO   4
               WHEN    5        SET   IX-YOUBI   TO   5
               WHEN    6        SET   IX-YOUBI   TO   6
               WHEN    0        SET   IX-YOUBI   TO   7
           END-EVALUATE.
       YOUBI-SET-EXT.
           EXIT.
