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
      *         �����ƥ�����ޥ���     (CPSK5007.INC)   *
      *         �ʣ���������                            *
      *         �����ؼ������������                    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-5007-REC.
           03  SYS-5007-KEY.
               05  SYS-5007-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5007-KANRICD     PIC  X(04).
      *��ʬ�����ɡʣ���������������������������������
      *            ���������ͽ���������������������
      *            �������ؼ�䵡�
               05  SYS-5007-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-5007-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-5007-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-5007-TBL.
      *�����ؼ������ʬ
               05  SYS-5007-SIJISENKBN  PIC  X(01).
      *�ؼ��
               05  SYS-5007-SIJISEN-AREA   OCCURS   4.
      *        ���Ф�
                   07  SYS-5007-SIJISEN-HEAD   PIC X(30).
      *        ��������
                   07  SYS-5007-SIJISEN-PRT    PIC 9(02).
      *ͽ��
               05  SYS-5007-YOBI             PIC  X(371).
      *ü���ɣ�
           03  SYS-5007-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5007-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-5007-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-5007-UPYMD           PIC  X(08).
      *��������
           03  SYS-5007-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5007-UPD-KEY.
               05  SYS-5007-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5007-UPD-KANRICD     PIC X(04).
               05  SYS-5007-UPD-KBNCD       PIC X(08).
               05  SYS-5007-UPD-STYUKYMD    PIC X(08).
               05  SYS-5007-UPD-EDYUKYMD    PIC X(08).