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
      *    ���ԸĿ��ֹ�            COPY  (CPPTMYNUMBER.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTMYNUMBER-REC.
      *����
           03  PTMYNUM-KEY.
      *���ŵ��أɣ�
               05  PTMYNUM-HOSPNUM         PIC  9(2).
      *���ԣɣ�
               05  PTMYNUM-PTID            PIC  9(10).
      *�ɣļ���
               05  PTMYNUM-ID-KEY          PIC  X(10).
      *�ɣ��ֹ�
           03  PTMYNUM-MY-NUMBER           PIC  X(20).
      *����
           03  PTMYNUM-DESCRIPTION         PIC  X(100).
      *ü���ɣ�
           03  PTMYNUM-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTMYNUM-OPID                PIC  X(16).
      *����ǯ����
           03  PTMYNUM-CREYMD              PIC  X(08).
      *����ǯ����
           03  PTMYNUM-UPYMD               PIC  X(08).
      *��������
           03  PTMYNUM-UPHMS               PIC  X(06).
      *
