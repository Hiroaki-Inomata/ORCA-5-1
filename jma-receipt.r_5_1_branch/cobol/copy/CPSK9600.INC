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
      *         �����ƥ�����ޥ���     (CPSK9600.INC)            *
      *         �ʣ���������                                     *
      *         �ã����������                                 *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-9600-REC.
           03  SYS-9600-KEY.
               05  SYS-9600-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-9600-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-9600-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-9600-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-9600-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-9600-TBL.
      *ͽ��
               05  SYS-9600-YOBI        PIC  X(500).
      *ü���ɣ�
           03  SYS-9600-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-9600-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-9600-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-9600-UPYMD           PIC  X(08).
      *��������
           03  SYS-9600-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-9600-UPD-KEY.
               05  SYS-9600-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9600-UPD-KANRICD     PIC X(04).
               05  SYS-9600-UPD-KBNCD       PIC X(08).
               05  SYS-9600-UPD-STYUKYMD    PIC X(08).
               05  SYS-9600-UPD-EDYUKYMD    PIC X(08).