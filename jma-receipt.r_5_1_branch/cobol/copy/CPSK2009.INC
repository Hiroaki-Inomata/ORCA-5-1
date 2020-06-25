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
      *         �����ƥ�����ޥ���     (CPSK2009.INC)            *
      *         �ʣ���������                                     *
      *         ����������                                     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *
      *  03.02.01    NACL-ƣ��    06/08/01  ���ݼ���б�
      *
      *  04.02.01    NACL-ƣ��    08/03/25  ʿ������ǯ�������
      *                                     ����Ϣ�����б�
      ************************************************************
       01  SYS-2009-REC.
           03  SYS-2009-KEY.
               05  SYS-2009-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2009-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2009-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2009-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2009-EDYUKYMD    PIC  X(08).
      *�������ե饰
           03  SYS-2009-SYUKA-KBN       PIC  X(01).
      *�����������������
           03  SYS-2009-SYUKA-NYUIN     PIC  X(01).
      *�������������������
           03  SYS-2009-SYUKA-GAIRAI    PIC  X(01).
      *��ʾ�������ե饰
           03  SYS-2009-SYUKA-MODE      PIC  X(01).
      *ʣ����Ʊ������
           03  SYS-2009-SYUKA-SELECT    PIC  X(01).
      *��ʼ�������ե饰
           03  SYS-2009-SYUKA-SYAHO     PIC  X(01).
      *��ʹ�������ե饰
           03  SYS-2009-SYUKA-KOKUHO    PIC  X(01).
      *��ʹ���Ϣ������ե饰
           03  SYS-2009-SYUKA-KOUIKI    PIC  X(01).
      *ͽ��
           03  SYS-2009-YOBI            PIC  X(492).
      *ü���ɣ�
           03  SYS-2009-TERMID          PIC  X(16).
      *���ڥ졼���ɣ�
           03  SYS-2009-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2009-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2009-UPYMD           PIC  X(08).
      *��������
           03  SYS-2009-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2009-UPD-KEY.
               05  SYS-2009-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2009-UPD-KANRICD     PIC X(04).
               05  SYS-2009-UPD-KBNCD       PIC X(08).
               05  SYS-2009-UPD-STYUKYMD    PIC X(08).
               05  SYS-2009-UPD-EDYUKYMD    PIC X(08).