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
      *****************************************************************
      *    �������û������å��ޥ���   COPY (CPNYUKSNCHK.INC)          *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  NYUKSNCHK-REC.
           03  NYUKSNCHK-KEY.
      *��������ʬ
               05  NYUKSNCHK-NYUINKBN  PIC  X(03).
      *�û���ʬ
               05  NYUKSNCHK-KSNKBN    PIC  X(03).
      *ͭ������ǯ����
               05  NYUKSNCHK-YUKOSTYMD PIC  X(08).
      *ͭ����λǯ����
               05  NYUKSNCHK-YUKOEDYMD PIC  X(08).
      *�����å���ʬ
           03  NYUKSNCHK-CHKKBN        PIC  X(01).
      *ü���ɣ�
           03  NYUKSNCHK-TERMID        PIC  X(16).
      *���ڡ��졼���ɣ�
           03  NYUKSNCHK-OPID          PIC  X(16).
      *����ǯ����
           03  NYUKSNCHK-CREYMD        PIC  X(08).
      *����ǯ����
           03  NYUKSNCHK-UPYMD         PIC  X(08).
      *�������� 
           03  NYUKSNCHK-UPHMS         PIC  X(06).
      *�祭�������ѹ���
           03  NYUKSNCHKUP-KEY.
               05  NYUKSNCHKUP-NYUINKBN      PIC  X(03).
               05  NYUKSNCHKUP-KSNKBN        PIC  X(03).
               05  NYUKSNCHKUP-YUKOSTYMD     PIC  X(08).
               05  NYUKSNCHKUP-YUKOEDYMD     PIC  X(08).
      *