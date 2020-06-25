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
      *         �����å��ޥ�����     (CPCK1201.INC)              *
      *         ���顼����ܺ�                                   *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CK4-1201-REC.
           03  CK4-1201-KEY.
               05  CK4-1201-HOSPNUM     PIC 9(2).
      *����������
               05  CK4-1201-KANRICD     PIC  X(04).
      *��ʬ������(����������1200�ζ�ʬ�����ɤ��б���
               05  CK4-1201-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  CK4-1201-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  CK4-1201-EDYUKYMD    PIC  X(08).
      *���ԣɣ�
           03  CK4-1201-PTID            PIC  9(10).
      *�����ơ��֥�
           03  CK4-1201-TBL.
               05  CK4-1201-DAY-G.
                   07  CK4-1201-DAY     PIC  9(01)
                                        OCCURS   31.
               05  CK4-1201-LASTYMD     PIC  X(08).
               05  CK4-1201-SRYCD       PIC  X(09).
               05  CK4-1201-SRYKA       PIC  X(02).
               05  CK4-1201-CHKKBN      PIC  X(01).
               05  CK4-1201-BYOMEI      PIC  X(80).
               05  CK4-1201-COMPKBN     PIC  X(1).
               05  CK4-1201-TEKCD       PIC  X(09).
               05  CK4-1201-TEKBYOMEI   PIC  X(80).
               05  CK4-1201-TEKCOMPKBN  PIC  X(1).
               05  CK4-1201-TANICD      PIC  9(3).
               05  CK4-1201-TANINAME    PIC  X(12).
               05  CK4-1201-SHONUM      PIC  9(3).
               05  CK4-1201-SRYSTYMD    PIC  X(08).
               05  CK4-1201-SRYEDYMD    PIC  X(08).
               05  CK4-1201-LIMIT       PIC  9(5).
               05  CK4-1201-TSUKISU     PIC  9(2).
               05  CK4-1201-FILLER      PIC  X(237).
      *ü���ɣ�
           03  CK4-1201-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CK4-1201-OPID            PIC  X(16).
      *����ǯ����
           03  CK4-1201-CREYMD          PIC  X(08).
      *����ǯ����
           03  CK4-1201-UPYMD           PIC  X(08).
      *��������
           03  CK4-1201-UPHMS           PIC  X(06).