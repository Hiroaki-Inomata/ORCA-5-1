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
      *         �����ƥ�����ޥ���     (CPSK1006.INC)   *
      *         �ʣ���������                            *
      *         ���ߴ����󥳡���                      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1006-REC.
           03  SYS-1006-KEY.
               05  SYS-1006-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1006-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1006-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1006-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1006-EDYUKYMD    PIC  X(08).
      *���ߴ�ॳ���ɥե饰
           03  SYS-1006-TBL.
               05  SYS-1006-SSTKIJUN    PIC  9(01)
                                        OCCURS   500.
      *ü���ɣ�
           03  SYS-1006-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1006-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1006-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1006-UPYMD           PIC  X(08).
      *��������
           03  SYS-1006-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1006-UPD-KEY.
               05  SYS-1006-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1006-UPD-KANRICD     PIC X(04).
               05  SYS-1006-UPD-KBNCD       PIC X(08).
               05  SYS-1006-UPD-STYUKYMD    PIC X(08).
               05  SYS-1006-UPD-EDYUKYMD    PIC X(08).