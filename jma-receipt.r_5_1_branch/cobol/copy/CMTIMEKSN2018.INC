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
      *----------------------------------------------------------------*
      *    時間外加算コードテーブル　平成３０年４月改正から            *
      *                         COPY (CMTIMEKSN2018.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *    時間外加算テーブル
       01  TBL-TIMEKSN18-AREA.
      *初診
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "11   111000570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "12   111000670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "13   111000770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "14   111000870".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "18   111012470".
      *（乳幼児） 時間外
           03  FILLER              PIC X(14)   VALUE   "1101 111011970".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "1201 111012070".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "1301 111012170".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "1401 111012270".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "1501 111011570".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "1601 111011670".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "1701 111011770".
      *再診料加算
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "21   112001110".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "22   112001210".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "23   112001310".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "24   112001410".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "28   112015570".
      *（乳幼児）    時間外
           03  FILLER              PIC X(14)   VALUE   "2102 112014770".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2202 112014870".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2302 112014970".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2402 112015070".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2502 112014170".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2602 112014270".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2702 112014370".
      *外来診察料加算
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "21  G112006470".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "22  G112006570".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "23  G112006670".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "24  G112006770".
      *（乳幼児）    時間外
           03  FILLER              PIC X(14)   VALUE   "2103 112015170".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2203 112015270".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2303 112015370".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2403 112015470".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2503 112014470".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2603 112014570".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2703 112014670".
      *小児科外来診療科（初診）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "1104 113009670".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "1204 113007170".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "1304 113007270".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "1404 111010770".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "1504 113007070".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "1604 113007170".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "1704 113007270".
      *小児科外来診療科（再診）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2105 113009770".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2205 113007470".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2305 113007570".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2405 112006070".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2505 113007370".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2605 113007470".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2705 113007570".
      *小児科外来診療科（外来診療科）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2106 113009870".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2206 113007770".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2306 113007870".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2406 113005570".
      *    夜間（時間外）加算
           03  FILLER              PIC X(14)   VALUE   "2506 113007670".
      *    休日加算
           03  FILLER              PIC X(14)   VALUE   "2606 113007770".
      *    深夜加算
           03  FILLER              PIC X(14)   VALUE   "2706 113007870".
      *地域包括診療料
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "31   113016270".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "32   113016370".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "33   113016470".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "34   113016870".
      *    乳幼児時間外加算
           03  FILLER              PIC X(14)   VALUE   "3107 113016570".
      *    乳幼児休日加算
           03  FILLER              PIC X(14)   VALUE   "3207 113016670".
      *    乳幼児深夜加算
           03  FILLER              PIC X(14)   VALUE   "3307 113016770".
      *    乳幼児時間外特例加算
           03  FILLER              PIC X(14)   VALUE   "3407 113016870".
      *    小児科 乳幼児夜間
           03  FILLER              PIC X(14)   VALUE   "3507 113017070".
      *    小児科 乳幼児休日
           03  FILLER              PIC X(14)   VALUE   "3607 113017170".
      *    小児科 乳幼児深夜
           03  FILLER              PIC X(14)   VALUE   "3707 113017270".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "38   113017370".
      *
      *認知症地域包括診療料
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "41   113018570".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "42   113018670".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "43   113018770".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "44   113019170".
      *    乳幼児時間外加算
           03  FILLER              PIC X(14)   VALUE   "4108 113018870".
      *    乳幼児休日加算
           03  FILLER              PIC X(14)   VALUE   "4208 113018970".
      *    乳幼児深夜加算
           03  FILLER              PIC X(14)   VALUE   "4308 113019070".
      *    乳幼児時間外特例加算
           03  FILLER              PIC X(14)   VALUE   "4408 113019270".
      *    小児科 乳幼児夜間
           03  FILLER              PIC X(14)   VALUE   "4508 113019370".
      *    小児科 乳幼児休日
           03  FILLER              PIC X(14)   VALUE   "4608 113019470".
      *    小児科 乳幼児深夜
           03  FILLER              PIC X(14)   VALUE   "4708 113019570".
      *    早朝加算
           03  FILLER              PIC X(14)   VALUE   "48   113019670".
      *
      *小児かかりつけ診療科（初診）（乳幼児）
      *    乳幼児　時間外
           03  FILLER              PIC X(14)   VALUE   "1109 113020170".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "1209 113020670".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "1309 113020770".
      *    乳幼児　時間外特例（小児科）
           03  FILLER              PIC X(14)   VALUE   "1409 113020470".
      *    乳幼児　夜間（小児科）
           03  FILLER              PIC X(14)   VALUE   "1509 113020570".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "1609 113020670".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "1709 113020770".
      *小児かかりつけ診療科（再診）（乳幼児）
      *    乳幼児　時間外
           03  FILLER              PIC X(14)   VALUE   "2110 113020870".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2210 113021370".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2310 113021470".
      *    乳幼児　時間外特例
           03  FILLER              PIC X(14)   VALUE   "2410 113021170".
      *    乳幼児　夜間（小児科）
           03  FILLER              PIC X(14)   VALUE   "2510 113021270".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2610 113021370".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2710 113021470".
      *小児かかりつけ診療科(外来診療科）（乳幼児）
      *    乳幼児　時間外
           03  FILLER              PIC X(14)   VALUE   "2111 113021570".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2211 113022070".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2311 113022170".
      *    乳幼児　時間外特例
           03  FILLER              PIC X(14)   VALUE   "2411 113021870".
      *    乳幼児　夜間（小児科）
           03  FILLER              PIC X(14)   VALUE   "2511 113021970".
      *    乳幼児　休日（小児科）
           03  FILLER              PIC X(14)   VALUE   "2611 113022070".
      *    乳幼児　深夜（小児科）
           03  FILLER              PIC X(14)   VALUE   "2711 113022170".
      *
      *小児かかりつけ診療科（初診）（７歳まで）
      *   時間外
           03  FILLER              PIC X(14)   VALUE   "1112 113026670".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "1212 113026770".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "1312 113026870".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "1412 113026970".
      *小児かかりつけ診療科（再診）（７歳まで）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2113 113027070".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2213 113027170".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2313 113027270".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2413 113027370".
      *小児かかりつけ診療科(外来診療科）（７歳まで）
      *    時間外
           03  FILLER              PIC X(14)   VALUE   "2114 113027470".
      *    休日
           03  FILLER              PIC X(14)   VALUE   "2214 113027570".
      *    深夜
           03  FILLER              PIC X(14)   VALUE   "2314 113027670".
      *    時間外特例
           03  FILLER              PIC X(14)   VALUE   "2414 113027770".
      *
       01  TBL-TIMEKSN18-AREA-R      REDEFINES   TBL-TIMEKSN18-AREA.
           03  TBL-TIMEKSN18-OCC     OCCURS   113
                                     INDEXED  TBL-TIDX18.
      *        1:初診、2：再診、3:地域包括診療料、4：認知症地域
               05  TBL-TM18-KBN1         PIC X(01).
      *        1:時間外、2：休日、3:深夜、4:特例時間外、5:夜間、
      *        6:小児科特例、7:小児科特例、8:夜間・早朝
               05  TBL-TM18-TIMKBN       PIC X(01).
      *        01:乳幼児（初診）、02:乳幼児（再診）、03:乳幼児（外来診療料）
      *        04:小児科（初診）、05:小児科（再診）、06:小児科（外来診療料）
      *        07:乳幼児（地域包括）、08:乳幼児（認知症）
      *        09:小児かかり（初診）、10:小児かかり（再診）
      *        11:小児かかり（外来診療料）
      *        12:小児かかり（７歳）（初診）
      *        13:小児かかり（７歳）（再診）
      *        14:小児かかり（７歳）（外来診療料）
               05  TBL-TM18-KBN2         PIC X(02).
      *        G:外来診療料
               05  TBL-TM18-KBN3         PIC X(01).
      *
               05  TBL-TM18-SRYCD        PIC X(09).
      *
      *H30.4 から
      *    妊婦加算テーブル
       01  TBL-NINPUKSN18-AREA.
      *    初診(妊婦加算)
           03  FILLER              PIC X(14)   VALUE   "10   111013370".
      *    再診(妊婦加算)
           03  FILLER              PIC X(14)   VALUE   "20   112022070".
      *    外来診療料(妊婦加算)
           03  FILLER              PIC X(14)   VALUE   "20  G112022870".
      *妊婦時間外加算
      *  初診
      *    時間外（初診）
           03  FILLER              PIC X(14)   VALUE   "1120 111012970".
      *    休日（初診）
           03  FILLER              PIC X(14)   VALUE   "1220 111013070".
      *    深夜（初診）
           03  FILLER              PIC X(14)   VALUE   "1320 111013170".
      *    時間外特例（初診）
           03  FILLER              PIC X(14)   VALUE   "1420 111013270".
      *    夜間（産科　初診）
           03  FILLER              PIC X(14)   VALUE   "1520 111013470".
      *    休日（産科　初診）
           03  FILLER              PIC X(14)   VALUE   "1620 111013570".
      *    深夜（産科　初診）
           03  FILLER              PIC X(14)   VALUE   "1720 111013670".
      *  再診
      *    時間外（再診）
           03  FILLER              PIC X(14)   VALUE   "2120 112021370".
      *    休日（再診）
           03  FILLER              PIC X(14)   VALUE   "2220 112021470".
      *    深夜（再診）
           03  FILLER              PIC X(14)   VALUE   "2320 112021570".
      *    時間外特例（再診）
           03  FILLER              PIC X(14)   VALUE   "2420 112021670".
      *    夜間（産科　再診）
           03  FILLER              PIC X(14)   VALUE   "2520 112022170".
      *    休日（産科　再診）
           03  FILLER              PIC X(14)   VALUE   "2620 112022270".
      *    深夜（産科　再診）
           03  FILLER              PIC X(14)   VALUE   "2720 112022370".
      *  外来診療料
      *    時間外（外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2120G112022470".
      *    休日　（外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2220G112022570".
      *    深夜（外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2320G112022670".
      *    時間外特例（外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2420G112022770".
      *    夜間（産科　外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2520G112022970".
      *    休日（産科　外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2620G112023070".
      *    深夜（産科　外来診療料）
           03  FILLER              PIC X(14)   VALUE   "2720G112023170".
      *  地域包括診療料
      *    時間外（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3120 113024970".
      *    休日（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3220 113025070".
      *    深夜（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3320 113025170".
      *    時間外特例（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3420 113025270".
      *    夜間（産科　（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3520 113025370".
      *    休日（産科　（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3620 113025470".
      *    深夜（産科　（再診）（地域包括）
           03  FILLER              PIC X(14)   VALUE   "3720 113025570".
      *  認知症地域包括診療料
      *    時間外（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4120 113025870".
      *    休日（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4220 113025970".
      *    深夜（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4320 113026070".
      *    時間外特例（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4420 113026170".
      *    夜間（産科　（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4520 113026270".
      *    休日（産科　（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4620 113026370".
      *    深夜（産科　（再診）（認知症地域包括）
           03  FILLER              PIC X(14)   VALUE   "4720 113026470".
      *
       01  TBL-NINPUKSN18-AREA-R   REDEFINES   TBL-NINPUKSN18-AREA.
           03  TBL-NINPUKSN18-OCC  OCCURS   38
                                   INDEXED  TBL-NPIDX18.
      *        1:初診、2：再診、3:地域包括診療料、4：認知症地域
               05  TBL-NP18-KBN1         PIC X(01).
      *        1:時間外、2：休日、3:深夜、4:特例時間外、5:夜間、
      *        6:特例休日、7:特例深夜
      *        0:妊婦加算
               05  TBL-NP18-TIMKBN       PIC X(01).
      *        20:妊婦加算
               05  TBL-NP18-KBN2         PIC X(02).
      *        G:外来診療料
               05  TBL-NP18-KBN3         PIC X(01).
      *
               05  TBL-NP18-SRYCD        PIC X(09).
      *
      *H30.4 から
      *    妊婦加算（入院）テーブル
       01  TBL-NYU-NINPUKSN18-AREA.
      *  再診
      *    時間外（再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2120 112019990".
      *    休日（再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2220 112020090".
      *    深夜（再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2320 112020190".
      *    時間外特例（再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2420 112020290".
      *    夜間（産科　再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2520 112020390".
      *    休日（産科　再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2620 112020490".
      *    深夜（産科　再診）（入院）
           03  FILLER              PIC X(14)   VALUE   "2720 112020590".
      *  外来診療料
      *    時間外（外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2120G112020690".
      *    休日　（外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2220G112020790".
      *    深夜（外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2320G112020890".
      *    時間外特例（外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2420G112020990".
      *    夜間（産科　外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2520G112021090".
      *    休日（産科　外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2620G112021190".
      *    深夜（産科　外来診療料）（入院）
           03  FILLER              PIC X(14)   VALUE   "2720G112021290".
      *
       01  TBL-NYU-NINPUKSN18-AREA-R
                               REDEFINES   TBL-NYU-NINPUKSN18-AREA.
           03  TBL-NYU-NINPUKSN18-OCC  OCCURS   14
                                   INDEXED  TBL-NYUIDX18.
      *        2：再診
               05  TBL-NYU-NP18-KBN1         PIC X(01).
      *        1:時間外、2：休日、3:深夜、4:特例時間外、5:夜間、
      *        6:特例休日、7:特例深夜
               05  TBL-NYU-NP18-TIMKBN       PIC X(01).
      *
               05  TBL-NYU-NP18-KBN2         PIC X(02).
      *        G:外来診療料
               05  TBL-NYU-NP18-KBN3         PIC X(01).
      *
               05  TBL-NYU-NP18-SRYCD        PIC X(09).
