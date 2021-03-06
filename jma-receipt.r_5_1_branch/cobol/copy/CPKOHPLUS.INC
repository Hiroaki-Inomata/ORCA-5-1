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
      *    �����ղþ���      �� COPY (CPKOHPLUS.INC)             *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  KOHPLUS-REC.
      *����
           03 KOHPLUS-KEY.
      *���ŵ��أɣ�
               05 KOHPLUS-HOSPNUM        PIC 9(2).
      *���ֹ�
               05 KOHPLUS-PREFNUM       PIC  X(02).
      *��Į¼�ֹ�
               05 KOHPLUS-CITYNUM       PIC  X(03).
      *�����ֹ�
               05 KOHPLUS-KOHNUM        PIC  X(03).
      *���Զ�ʬ
               05 KOHPLUS-PTKBN         PIC  X(01).
      *ͭ������ǯ����
               05 KOHPLUS-YUKOSTYMD     PIC  X(08).
      *ͭ����λǯ����
           03 KOHPLUS-YUKOEDYMD         PIC  X(08).
      *��ô��ʬ
           03 KOHPLUS-FTNKBN            PIC  X(01).
      *�����ݿ������ܶ�ʬ
           03 KOHPLUS-NYUSKJRYYKBN      PIC  X(01).
      *�쥻�ץ������ʬ
           03 KOHPLUS-RECESKYKBN        PIC  X(01).
      *����̾
           03 KOHPLUS-SEIDONAME         PIC  X(100).
      *ü���ɣ�
           03 KOHPLUS-TERMID            PIC  X(16).
      *���ڡ��졼���ɣ�
           03 KOHPLUS-OPID              PIC  X(16).
      *����ǯ����
           03 KOHPLUS-CREYMD            PIC  X(08).
      *����ǯ����
           03 KOHPLUS-UPYMD             PIC  X(08).
      *��������
           03 KOHPLUS-UPHMS             PIC  X(06).
      *�����ѥ���
           03 KOHPLUSUP-KEY.
      *���ŵ��أɣ�
               05 KOHPLUSUP-HOSPNUM      PIC 9(2).
      *���ֹ�
               05 KOHPLUSUP-PREFNUM     PIC  X(02).
      *��Į¼�ֹ�
               05 KOHPLUSUP-CITYNUM     PIC  X(03).
      *�����ֹ�
               05 KOHPLUSUP-KOHNUM      PIC  X(03).
      *���Զ�ʬ
               05 KOHPLUSUP-PTKBN       PIC  X(01).
      *ͭ������ǯ����
               05 KOHPLUSUP-YUKOSTYMD   PIC  X(08).
