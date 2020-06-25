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
      *         �����ƥ�����ޥ���     (CPSK8000.INC)            *
      *         �ʣ���������                                     *
      *         �����ǡ���������������                         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-8000-REC.
           03  SYS-8000-KEY.
               05  SYS-8000-HOSPNUM    PIC 9(2).
      *����������
               05  SYS-8000-KANRICD    PIC  X(04).
      *��ʬ������
               05  SYS-8000-KBNCD      PIC  X(08).
      *ͭ������ǯ����
               05  SYS-8000-STYUKYMD   PIC  X(08).
      *ͭ����λǯ����
               05  SYS-8000-EDYUKYMD   PIC  X(08).
      *�����ơ��֥�
           03  SYS-8000-TBL.
      *��������
               05  SYS-8000-DAILY      PIC  9(03).
      *
               05  SYS-8000-DAILY-YM   PIC  X(06).
      *
               05  SYS-8000-DAILY-UPYMD
                                       PIC  X(08).
      *�����
               05  SYS-8000-MONTHLY    PIC  9(03).
      *
               05  SYS-8000-MONTHLY-YM PIC  X(06).
      *
               05  SYS-8000-MONTHLY-UPYMD
                                       PIC  X(08).
      *������������
               05  SYS-8000-NORDER     PIC  9(03).
      *
               05  SYS-8000-NORDER-YM  PIC  X(06).
      *
               05  SYS-8000-NORDER-UPYMD
                                       PIC  X(08).
      *ͽ��
           03  SYS-8000-YOBI           PIC  X(449).
      *ü���ɣ�
           03  SYS-8000-TERMID         PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-8000-OPID           PIC  X(16).
      *����ǯ����
           03  SYS-8000-CREYMD         PIC  X(08).
      *����ǯ����
           03  SYS-8000-UPYMD          PIC  X(08).
      *��������
           03  SYS-8000-UPHMS          PIC  X(06).
      *�祭�������ѹ���
           03  SYS-8000-UPD-KEY.
               05  SYS-8000-UPD-HOSPNUM     PIC 9(2).
               05  SYS-8000-UPD-KANRICD     PIC X(04).
               05  SYS-8000-UPD-KBNCD       PIC X(08).
               05  SYS-8000-UPD-STYUKYMD    PIC X(08).
               05  SYS-8000-UPD-EDYUKYMD    PIC X(08).