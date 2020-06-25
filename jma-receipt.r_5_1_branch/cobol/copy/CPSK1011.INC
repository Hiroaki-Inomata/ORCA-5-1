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
      *         �����ƥ�����ޥ���     (CPSK1011.INC)   *
      *         �ʣ���������                            *
      *         ô��������                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1011-REC.
           03  SYS-1011-KEY.
               05  SYS-1011-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1011-KANRICD     PIC  X(04).
      *��ʬ�����ɡʿ��Ųʡ�
               05  SYS-1011-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1011-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1011-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1011-TBL.
               05  SYS-1011-TBL2        OCCURS   20.
      *�ֹ�
                   07  SYS-1011-RENNUM      PIC  X(04).
      *�ɥ�����
                   07  SYS-1011-DRCD        PIC  X(05).
      *ͽ��
                   07  SYS-1011-YOBI1       PIC  X(02).
      *ͽ��
           03  SYS-1011-YOBI2           PIC  X(280).
      *ü���ɣ�
           03  SYS-1011-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1011-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1011-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1011-UPYMD           PIC  X(08).
      *��������
           03  SYS-1011-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1011-UPD-KEY.
               05  SYS-1011-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1011-UPD-KANRICD     PIC X(04).
               05  SYS-1011-UPD-KBNCD       PIC X(08).
               05  SYS-1011-UPD-STYUKYMD    PIC X(08).
               05  SYS-1011-UPD-EDYUKYMD    PIC X(08).