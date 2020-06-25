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
      *         �����ƥ�����ޥ���     (CPSK9104.INC)            *
      *         �ʣ���������                                     *
      *         ���ž����������ؾ���                             *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-9104-REC.
           03  SYS-9104-KEY.
               05  SYS-9104-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9104-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-9104-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9104-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9104-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-9104-KANRITBL.
      *�Żҥ쥻�ץȥǡ���
               05  SYS-9104-RECEDEN     PIC X(01).
      *������ž���ǡ���
               05  SYS-9104-JMAMR       PIC X(01).
      *ͽ��
               05  SYS-9104-YOBI        PIC X(498).
      *ü���ɣ�
           03  SYS-9104-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9104-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-9104-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-9104-UPYMD           PIC  X(08).
      *��������
           03  SYS-9104-UPHMS           PIC  X(06).