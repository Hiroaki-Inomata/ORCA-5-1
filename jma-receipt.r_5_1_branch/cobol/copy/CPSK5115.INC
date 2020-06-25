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
      *         �����ƥ�����ޥ���     (CPSK5115.INC)   *
      *         �ʣ���������                            *
      *         �Ǹ��������                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-5115-REC.
           03  SYS-5115-KEY.
               05  SYS-5115-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-5115-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-5115-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-5115-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-5115-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-5115-TBL.
      *�Ǹ��������̾��
               05  SYS-5115-BTU-KNGHJOHAI-NM
                                        PIC  X(12).
      *ͽ��
           03  SYS-5115-YOBI            PIC  X(488).
      *ü���ɣ�
           03  SYS-5115-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-5115-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-5115-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-5115-UPYMD           PIC  X(08).
      *��������
           03  SYS-5115-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-5115-UPD-KEY.
               05  SYS-5115-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5115-UPD-KANRICD     PIC X(04).
               05  SYS-5115-UPD-KBNCD       PIC X(08).
               05  SYS-5115-UPD-STYUKYMD    PIC X(08).
               05  SYS-5115-UPD-EDYUKYMD    PIC X(08).