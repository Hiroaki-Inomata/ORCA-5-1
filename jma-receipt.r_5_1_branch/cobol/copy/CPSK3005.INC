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
      *         �����ƥ�����ޥ���     (CPSK3005.INC)   *
      *         �ʣ���������                            *
      *         ����������                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-3005-REC.
           03  SYS-3005-KEY.
               05  SYS-3005-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-3005-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-3005-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-3005-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-3005-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-3005-TBL.
      *������ʬ
               05  SYS-3005-PROCKBN     PIC  X(01).
      *��������
               05  SYS-3005-STSRYYMD    PIC  X(08).
      *���Ｋ
               05  SYS-3005-STTIME      PIC  X(04).
      *��������
               05  SYS-3005-EDSRYYMD    PIC  X(08).
      *�����
               05  SYS-3005-EDTIME      PIC  X(04).
      *ͽ��
               05  SYS-3005-YOBI        PIC  X(475).
      *ü���ɣ�
           03  SYS-3005-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-3005-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-3005-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-3005-UPYMD           PIC  X(08).
      *��������
           03  SYS-3005-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-3005-UPD-KEY.
               05  SYS-3005-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3005-UPD-KANRICD     PIC X(04).
               05  SYS-3005-UPD-KBNCD       PIC X(08).
               05  SYS-3005-UPD-STYUKYMD    PIC X(08).
               05  SYS-3005-UPD-EDYUKYMD    PIC X(08).