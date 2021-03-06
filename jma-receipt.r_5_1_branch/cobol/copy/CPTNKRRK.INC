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
      *    ǯ������        �� COPY (CPTNKRRK.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TNKRRK-REC.
      *����
           03  TNKRRK-KEY.
      *���ŵ��أɣ�
               05  TNKRRK-HOSPNUM           PIC 9(2).
      *���ԣɣ�
               05  TNKRRK-PTID             PIC  9(10).
      *����ǯ����
               05  TNKRRK-STYMD            PIC  X(08).
      *��λǯ����
           03  TNKRRK-EDYMD                PIC  X(08).
      *����ɸ����ô�۸��۳�����
           03  TNKRRK-SKJNINSTYMD          PIC  X(08).
      *Ϸ��ʡ��ǯ�����Ծڶ�ʬ�ʣ����ʤ������������
           03  TNKRRK-RRI-FUKUSHI-KBN      PIC  X(01).
      *�ݸ�Ŭ�Ѷ�ʬ�ʣ��������оݡ���������������оݳ�
      *              ������������Τ��оݡ�
           03  TNKRRK-HKNTEKKBN            PIC  X(01).
      *�����س�����ʬ�ʶ������ϣ������������������������س���
           03  TNKRRK-KYOUKAISOUKBN        PIC  X(01).
      *ü���ɣ�
           03  TNKRRK-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  TNKRRK-OPID                 PIC  X(16).
      *����ǯ����
           03  TNKRRK-CREYMD               PIC  X(08).
      *����ǯ����
           03  TNKRRK-UPYMD                PIC  X(08).
      *��������
           03  TNKRRK-UPHMS                PIC  X(06).
      *�����祭�������ѹ���
           03  TNKRRK-KEY-O.
               05  TNKRRK-HOSPNUM-O           PIC 9(2).
               05  TNKRRK-PTID-O             PIC  9(10).
               05  TNKRRK-STYMD-O            PIC  X(08).
