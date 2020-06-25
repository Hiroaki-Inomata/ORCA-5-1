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
      *         �����ƥ�����ޥ���     (CPSK3103.INC)   *
      *         �ʣ���������                            *
      *         �ʥ�ݥ��ȥꡢ�ץ�������              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-3103-REC.
           03  SYS-3103-KEY.
               05  SYS-3103-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-3103-KANRICD     PIC  X(04).
      *��ʬ������(���Υ����ƥ�����ֹ�+Ϣ��3���
      *�㡧������� 5010001(5010 + 001)
               05  SYS-3103-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-3103-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-3103-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-3103-TBL.
               05  SYS-3103-REPOS-NAME  PIC  X(256).
               05  SYS-3103-PROGRAM-NAME-G.
                   07   SYS-3103-PROGRAM-NAME
                                        PIC X(20)
                                        OCCURS 5.
               05  SYS-3103-YOBI        PIC  X(144).
      *ü���ɣ�
           03  SYS-3103-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-3103-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-3103-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-3103-UPYMD           PIC  X(08).
      *��������
           03  SYS-3103-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-3103-UPD-KEY.
               05  SYS-3103-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3103-UPD-KANRICD     PIC X(04).
               05  SYS-3103-UPD-KBNCD       PIC X(08).
               05  SYS-3103-UPD-STYUKYMD    PIC X(08).
               05  SYS-3103-UPD-EDYUKYMD    PIC X(08).