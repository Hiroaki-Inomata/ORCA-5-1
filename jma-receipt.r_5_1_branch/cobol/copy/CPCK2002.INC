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
      *         �����å��ޥ�����     (CPCK2002.INC)              *
      *         �ʣ���������                                     *
      *         �����ǧ����                                     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CK4-2002-REC.
           03  CK4-2002-KEY.
               05  CK4-2002-HOSPNUM     PIC 9(2).
      *����������
               05  CK4-2002-KANRICD     PIC  X(04).
      *��ʬ������
               05  CK4-2002-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  CK4-2002-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  CK4-2002-EDYUKYMD    PIC  X(08).
      *���ԣɣ�
           03  CK4-2002-PTID            PIC  9(10).
      *�����ơ��֥�
           03  CK4-2002-TBL.
               05  CK4-2002-NAME        PIC  X(30).
               05  CK4-2002-CHKKBN-G.
                   07  CK4-2002-CHKKBN  PIC  X(01)  OCCURS 50.
               05  CK4-2002-INGAIPROCKBN
                                        PIC  X(01).
               05  CK4-2002-FILLER      PIC  X(419).
      *ü���ɣ�
           03  CK4-2002-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CK4-2002-OPID            PIC  X(16).
      *����ǯ����
           03  CK4-2002-CREYMD          PIC  X(08).
      *����ǯ����
           03  CK4-2002-UPYMD           PIC  X(08).
      *��������
           03  CK4-2002-UPHMS           PIC  X(06).