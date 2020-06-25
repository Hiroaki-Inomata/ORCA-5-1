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
      *         �����ƥ����    COPY   (CPSYSKANRI.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYSKANRI-REC.
      *����
           03  SYS-KEY.
               05  SYS-HOSPNUM         PIC 9(2).
      *����������
               05  SYS-KANRICD          PIC  X(04).
      *��ʬ������
               05  SYS-KBNCD            PIC  X(08).
      *����ͭ��ǯ����
               05  SYS-STYUKYMD         PIC  X(08).
      *��λͭ��ǯ����
               05  SYS-EDYUKYMD         PIC  X(08).
      *�����ơ��֥�
           03  SYS-KANRITBL             PIC  X(500).
      *ü���ɣ�
           03  SYS-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-OPID                 PIC  X(16).
      *����ǯ����
           03  SYS-CREYMD               PIC  X(08).
      *����ǯ����
           03  SYS-UPYMD                PIC  X(08).
      *��������
           03  SYS-UPHMS                PIC  X(06).
      *�����祭�������ѹ���
           03  SYSUPD-KEY.
               05  SYSUP-HOSPNUM          PIC 9(2).
               05  SYSUP-KANRICD          PIC X(04).
               05  SYSUP-KBNCD            PIC X(08).
               05  SYSUP-STYUKYMD         PIC X(08).
               05  SYSUP-EDYUKYMD         PIC X(08).