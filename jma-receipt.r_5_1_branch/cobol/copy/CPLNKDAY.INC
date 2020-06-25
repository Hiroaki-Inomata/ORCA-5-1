      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
000100******************************************************
000200* ���Ѵ������ΰ�                                     *
000300*        ���ꥳ����   11 ������ ��������                *
000400*                   12 ������ ��������                *
000500******************************************************
000600 01  LNK-DAY1-AREA.
000700     03  LNK-DAY1-IRAI               PIC X(02).
000800     03  LNK-DAY1-YMD                PIC 9(08).
000900     03  LNK-DAY1-YMD-X   REDEFINES  LNK-DAY1-YMD.
001000       05  LNK-DAY1-XXXX             PIC 9(01).
001100       05  LNK-DAY1-WYMD             PIC 9(07).
001200     03  LNK-DAY1-GENGO              PIC X(01).
001300     03  LNK-DAY1-GENKIGO            PIC X(01).
001400     03  LNK-DAY1-GENMEI             PIC N(02).
001500     03  LNK-DAY1-FILLER             PIC X(64).
001600******************************************************
001700* �����Խ������ΰ�                                   *
001800*        ���ꥳ����   21 ������ ����ˤ�����            *
001900*                   22 ������ ����ˤ�����            *
002000******************************************************
002100 01  LNK-DAY2-AREA.
002200     03  LNK-DAY2-IRAI               PIC X(02).
002300     03  LNK-DAY2-YMD                PIC 9(08).
002400     03  LNK-DAY2-EDTYMD1.
002500       05  LNK-DAY2-EDTYMD1-G        PIC X(01).
002600       05  LNK-DAY2-EDTYMD1-Y        PIC Z9.
002700       05  LNK-DAY2-EDTYMD1-P1       PIC X(01).
002800       05  LNK-DAY2-EDTYMD1-M        PIC Z9.
002900       05  LNK-DAY2-EDTYMD1-P2       PIC X(01).
003000       05  LNK-DAY2-EDTYMD1-D        PIC Z9.
003100     03  LNK-DAY2-EDTYMD2.
003200       05  LNK-DAY2-EDTYMD2-G        PIC N(02).
003300       05  LNK-DAY2-EDTYMD2-Y        PIC N(02).
003400       05  LNK-DAY2-EDTYMD2-P1       PIC N(01).
003500       05  LNK-DAY2-EDTYMD2-M        PIC N(02).
003600       05  LNK-DAY2-EDTYMD2-P2       PIC N(01).
003700       05  LNK-DAY2-EDTYMD2-D        PIC N(02).
003800     03  LNK-DAY2-EDTYMD3.
003900       05  LNK-DAY2-EDTYMD3-G        PIC N(02).
004000       05  LNK-DAY2-EDTYMD3-Y        PIC N(02).
004100       05  LNK-DAY2-EDTYMD3-NEN      PIC N(01).
004200       05  LNK-DAY2-EDTYMD3-M        PIC N(02).
004300       05  LNK-DAY2-EDTYMD3-TUKI     PIC N(01).
004400       05  LNK-DAY2-EDTYMD3-D        PIC N(02).
004500       05  LNK-DAY2-EDTYMD3-HI       PIC N(01).
004600     03  LNK-DAY2-FILLER             PIC X(19).
004700******************************************************
004800* �������а����ΰ�                                   *
004900*        ���ꥳ����   31 ������ ����ˤ�����            *
005000*                   32 ������ ����ˤ�����            *
005100******************************************************
005200 01  LNK-DAY3-AREA.
005300     03  LNK-DAY3-IRAI               PIC X(02).
005400     03  LNK-DAY3-YMD                PIC 9(08).
005500     03  LNK-DAY3-YOBI               PIC 9(01).
005600     03  LNK-DAY3-YOBIKIGO           PIC X(03).
005700     03  LNK-DAY3-YOBIMEI            PIC N(01).
005800     03  LNK-DAY3-SYUKU              PIC 9(01).
005900     03  LNK-DAY3-YASUMI             PIC 9(01).
006000     03  LNK-DAY3-FILLER             PIC X(62).
006100******************************************************
006200* �������а����ΰ�                                   *
006300*        ���ꥳ����   41 ������ ����ˤ�����            *
006400*                   42 ������ ����ˤ�����            *
006500******************************************************
006600 01  LNK-DAY4-AREA.
006700     03  LNK-DAY4-IRAI               PIC X(02).
006800     03  LNK-DAY4-YM                 PIC 9(06).
006900     03  LNK-DAY4-COUNT              PIC 9(02).
007000     03  LNK-DAY4-SKJTDD-R.
007100       05  LNK-DAY4-SKJTDD           PIC 9(02)  OCCURS  5.
007200     03  LNK-DAY4-FILLER             PIC X(60).
007300******************************************************
007400* ���������׻������ΰ�                               *
007500*        ���ꥳ����   51 ������ ����ˤ�����            *
007600*                   52 ������ ����ˤ�����            *
007700******************************************************
007800 01  LNK-DAY5-AREA.
007900     03  LNK-DAY5-IRAI               PIC X(02).
008000     03  LNK-DAY5-START              PIC 9(08).
008100     03  LNK-DAY5-END                PIC 9(08).
008200     03  LNK-DAY5-NISSU1             PIC 9(05).
008300     03  LNK-DAY5-NISSU2             PIC 9(05).
008400     03  LNK-DAY5-FILLER             PIC X(52).
008500******************************************************
008600* ���������а����ΰ�                                 *
008700*        ���ꥳ����   61 ������ ����ˤ�����            *
008800*                   62 ������ ����ˤ�����            *
008900******************************************************
009000 01  LNK-DAY6-AREA.
009100     03  LNK-DAY6-IRAI               PIC X(02).
009200     03  LNK-DAY6-KIJUN              PIC 9(08).
009300     03  LNK-DAY6-ZOGENPTN           PIC X(01).
009400     03  LNK-DAY6-ZOGEN              PIC S9(05)   PACKED-DECIMAL.
009500     03  LNK-DAY6-KOYOMI             PIC 9(08).
009600     03  LNK-DAY6-KOYOMIZEN          PIC 9(08).
009700     03  LNK-DAY6-KOYOMIGO           PIC 9(08).
009800     03  LNK-DAY6-KEISAN             PIC 9(08).
009900     03  LNK-DAY6-KEISANZEN          PIC 9(08).
010000     03  LNK-DAY6-KEISANGO           PIC 9(08).
010100     03  LNK-DAY6-FILLER             PIC X(18).
010200******************************************************
010300* ���������а����ΰ�                                 *
010400*        ���ꥳ����   71 ������ ����ˤ�����            *
010500*                   72 ������ ����ˤ�����            *
010600******************************************************
010700 01  LNK-DAY7-AREA.
010800     03  LNK-DAY7-IRAI               PIC X(02).
010900     03  LNK-DAY7-YM                 PIC 9(06).
011000     03  LNK-DAY7-KOYOMI             PIC 9(08).
011100     03  LNK-DAY7-EIGYO              PIC 9(08).
011200     03  LNK-DAY7-FILLER             PIC X(56).