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
      *         �����ƥ�����ޥ���     (CPSK2001.INC)            *
      *         �ʣ���������                                     *
      *         �쥻�ץȺ����ؼ�������                           *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01   MCC-ƣ��      02.06.13  �쥻�ץȼ����ʬ���ɲ�
      *  01.00.02   NACL-ƣ��     02.12.11  ������ʬ���ɲ�
      *
      *  03.04.01    NACL-ƣ��    06/11/01  �Ȳ�ǽϢ���б�
      *                                     ������ʬ���ɲ�
      *
      *  04.03.01    NACL-ƣ��    08/07/09  ���쥻�����ʱ��������ޤ���б�
      *                                     �쥻�ץȺ�����ʬ�ɲ� 
      ************************************************************
       01  SYS-2001-REC.
           03  SYS-2001-KEY.
               05  SYS-2001-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2001-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2001-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2001-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2001-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-2001-TBL.
      *������ʬ
               05  SYS-2001-SYOKBN      PIC  X(01).
      *����ǯ��
               05  SYS-2001-SRYYM       PIC  X(06).
      *�쥻�ץȼ����ʬ
               05  SYS-2001-RECEKBN     PIC  X(01).
      *������ʬ
               05  SYS-2001-NYUGAIKBN   PIC  X(01).
      *������ʬ
               05  SYS-2001-SYOSBT      PIC  X(01).
      *�쥻�ץȺ�����ʬ�ʣ��������ѥ쥻�������ţƥե����롢����Ϣ�������ѡ�
               05  SYS-2001-PRTKBN      PIC  X(01).
      *ͽ��
           03  SYS-2001-YOBI            PIC  X(489).
      *ü���ɣ�
           03  SYS-2001-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-2001-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2001-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2001-UPYMD           PIC  X(08).
      *��������
           03  SYS-2001-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2001-UPD-KEY.
               05  SYS-2001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2001-UPD-KANRICD     PIC X(04).
               05  SYS-2001-UPD-KBNCD       PIC X(08).
               05  SYS-2001-UPD-STYUKYMD    PIC X(08).
               05  SYS-2001-UPD-EDYUKYMD    PIC X(08).