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
      *         �����ƥ�����ޥ���     (CPSK1902.INC)            *
      *         �ʣ���������                                     *
      *         ���ŵ����Խ�����ݽ���Ͼ���                     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1902-REC.
           03  SYS-1902-KEY.
               05  SYS-1902-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1902-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1902-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1902-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1902-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1902-TBL.
      *͹���ֹ�
               05  SYS-1902-POST        PIC  X(07).
      *����ϡʣ����ܡ�
               05  SYS-1902-ADRS1       PIC  X(152).
      *����ϡʣ����ܡ�
               05  SYS-1902-ADRS2       PIC  X(152).
      *����ϡʣ����ܡ�
               05  SYS-1902-ADRS3       PIC  X(152).
      *�����ֹ�
               05  SYS-1902-TEL         PIC  X(15).
      *�ƣ����ֹ�
               05  SYS-1902-FAX         PIC  X(15).
      *����ϡʣ����ܡ�ʸ����
               05  SYS-1902-ADRS1MOJI   PIC  9(02).
      *ͽ��
           03  SYS-1902-YOBI            PIC  X(05).
      *ü���ɣ�
           03  SYS-1902-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1902-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1902-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1902-UPYMD           PIC  X(08).
      *��������
           03  SYS-1902-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1902-UPD-KEY.
               05  SYS-1902-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1902-UPD-KANRICD     PIC X(04).
               05  SYS-1902-UPD-KBNCD       PIC X(08).
               05  SYS-1902-UPD-STYUKYMD    PIC X(08).
               05  SYS-1902-UPD-EDYUKYMD    PIC X(08).