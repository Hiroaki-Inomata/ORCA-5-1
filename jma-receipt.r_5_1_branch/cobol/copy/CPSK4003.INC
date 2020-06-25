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
      *-------------------------------------------------*
      *         �����ƥ�����ޥ���     (CPSK4003.INC)   *
      *         �ʣ���������                            *
      *         �������ݸ������������������оݳ�������*
      *-------------------------------------------------*
       01  SYS-4003-REC.
           03  SYS-4003-KEY.
               05  SYS-4003-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-4003-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-4003-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-4003-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-4003-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-4003-TBL.
      *�оݳ����ť�����
               05  SYS-4003-SRYCD       PIC  X(09)
                                        OCCURS  50.
      *ͽ��
           03  SYS-4003-YOBI            PIC  X(50).
      *ü���ɣ�
           03  SYS-4003-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-4003-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-4003-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-4003-UPYMD           PIC  X(08).
      *��������
           03  SYS-4003-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-4003-UPD-KEY.
               05  SYS-4003-UPD-HOSPNUM     PIC 9(2).
               05  SYS-4003-UPD-KANRICD     PIC X(04).
               05  SYS-4003-UPD-KBNCD       PIC X(08).
               05  SYS-4003-UPD-STYUKYMD    PIC X(08).
               05  SYS-4003-UPD-EDYUKYMD    PIC X(08).