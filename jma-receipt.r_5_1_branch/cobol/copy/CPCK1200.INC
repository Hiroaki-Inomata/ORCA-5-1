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
      *         �����å��ޥ�����     (CPCK1200.INC)              *
      *         �ʣ���������                                     *
      *         ���󥨥顼����                                   *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CK4-1200-REC.
           03  CK4-1200-KEY.
               05  CK4-1200-HOSPNUM     PIC 9(2).
      *����������
               05  CK4-1200-KANRICD     PIC  X(04).
      *��ʬ������(1-6���ܡ�Ϣ�֡�7-8���ܡ���ǧ���ܤ��ֹ��
               05  CK4-1200-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  CK4-1200-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  CK4-1200-EDYUKYMD    PIC  X(08).
      *���ԣɣ�
           03  CK4-1200-PTID            PIC  9(10).
      *�����ơ��֥�
           03  CK4-1200-TBL.
               05  CK4-1200-HKNKBN      PIC  X(01).
               05  CK4-1200-ERRCD       PIC  X(06).
               05  CK4-1200-FIL1        PIC  X(193).
               05  CK4-1200-ERRMSG      PIC  X(220).
               05  CK4-1200-FIL2        PIC  X(80).
      *ü���ɣ�
           03  CK4-1200-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CK4-1200-OPID            PIC  X(16).
      *����ǯ����
           03  CK4-1200-CREYMD          PIC  X(08).
      *����ǯ����
           03  CK4-1200-UPYMD           PIC  X(08).
      *��������
           03  CK4-1200-UPHMS           PIC  X(06).