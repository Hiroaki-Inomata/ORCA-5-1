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
      *         �����ƥ�����ޥ���     (CPSK2002.INC)   *
      *         �ʣ���������                            *
      *         �쥻�ץȺ����ؼ��ʸ��̺����˥�����      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01   MCC-ƣ��      02.06.13  �쥻�ץȼ����ʬ���ɲ�
      ************************************************************
       01  SYS-2002-REC.
           03  SYS-2002-KEY.
               05  SYS-2002-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2002-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2002-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2002-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2002-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-2002-TBL.
      *���ֹ�
               05  SYS-2002-PTNUM       PIC  X(20).
      *����ɽ��ʬ
               05  SYS-2002-PTID        PIC  X(10).
      *���ŵ��إ�����
               05  SYS-2002-SRYYM       PIC  X(06).
      *���ŵ��ؼ���
               05  SYS-2002-NYUGAIKBN   PIC  X(01).
      *�쥻�ץȼ����ʬ
               05  SYS-2002-RECEKBN     PIC  X(01).
      *ͽ��
           03  SYS-2002-YOBI            PIC  X(462).
      *ü���ɣ�
           03  SYS-2002-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-2002-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2002-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2002-UPYMD           PIC  X(08).
      *��������
           03  SYS-2002-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2002-UPD-KEY.
               05  SYS-2002-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2002-UPD-KANRICD     PIC X(04).
               05  SYS-2002-UPD-KBNCD       PIC X(08).
               05  SYS-2002-UPD-STYUKYMD    PIC X(08).
               05  SYS-2002-UPD-EDYUKYMD    PIC X(08).