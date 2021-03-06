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
      *    �����������    �� COPY  (CPTSYRRK.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TSYRRK-REC.
      *����
           03  TSYRRK-KEY.
      *���ŵ��أɣ�
               05  TSYRRK-HOSPNUM           PIC 9(2).
      *���ԣɣ�
               05  TSYRRK-PTID             PIC  9(10).
      *ǧ�곫��ǯ����
               05  TSYRRK-NINSTYMD         PIC  X(08).
      *ǧ�꽪λǯ����
           03  TSYRRK-NINEDYMD             PIC  X(08).
      *����ɸ����ô�۸��۳�����
           03  TSYRRK-SKJNINSTYMD          PIC  X(08).
      *Ĺ�������������۳�����
           03  TSYRRK-SKJGNSTYMD           PIC  X(08).
      *�ݸ�Ŭ�Ѷ�ʬ�ʣ��������оݡ���������������оݳ�
      *              ������������Τ��оݡ�
           03  TSYRRK-HKNTEKKBN            PIC  X(01).
      *�����س�����ʬ�ʶ������ϣ������������������������س���
           03  TSYRRK-KYOUKAISOUKBN        PIC  X(01).
      *ü���ɣ�
           03  TSYRRK-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  TSYRRK-OPID                 PIC  X(16).
      *����ǯ����
           03  TSYRRK-CREYMD               PIC  X(08).
      *����ǯ����
           03  TSYRRK-UPYMD                PIC  X(08).
      *��������
           03  TSYRRK-UPHMS                PIC  X(06).
      *�����祭�������ѹ���
           03  TSYRRK-KEY-O.
               05  TSYRRK-HOSPNUM-O           PIC 9(2).
               05  TSYRRK-PTID-O             PIC  9(10).
               05  TSYRRK-NINSTYMD-O         PIC  X(08).
