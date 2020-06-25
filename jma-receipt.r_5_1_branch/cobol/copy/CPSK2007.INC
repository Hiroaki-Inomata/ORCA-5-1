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
      *         �����ƥ�����ޥ���     (CPSK2007.INC)            *
      *         �ʣ���������                                     *
      *         �����ֹ�ޤȤ����                               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    16/09/13  �����ֹ���ɲ�
      ************************************************************
       01  SYS-2007-REC.
           03  SYS-2007-KEY.
               05  SYS-2007-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2007-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2007-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2007-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2007-EDYUKYMD    PIC  X(08).
      *�������
           03  SYS-2007-KOH-AREA.
      *�����ֹ�
               05  SYS-2007-KOHNUM      PIC  X(03)   OCCURS  10.
      *̾��
           03  SYS-2007-MEISYO-OCC      OCCURS  2.
      *
               05  SYS-2007-MEISYO      PIC  X(44).
      *�������
           03  SYS-2007-KOH1-AREA.
      *�����ֹ�
               05  SYS-2007-KOHNUM1     PIC  X(03)   OCCURS  20.
      *ͽ��
           03  SYS-2007-YOBI            PIC  X(322).
      *ü���ɣ�
           03  SYS-2007-TERMID          PIC  X(16).
      *���ڥ졼���ɣ�
           03  SYS-2007-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2007-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2007-UPYMD           PIC  X(08).
      *��������
           03  SYS-2007-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2007-UPD-KEY.
               05  SYS-2007-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2007-UPD-KANRICD     PIC X(04).
               05  SYS-2007-UPD-KBNCD       PIC X(08).
               05  SYS-2007-UPD-STYUKYMD    PIC X(08).
               05  SYS-2007-UPD-EDYUKYMD    PIC X(08).