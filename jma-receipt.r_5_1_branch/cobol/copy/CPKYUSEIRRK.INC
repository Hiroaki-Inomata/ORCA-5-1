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
      *    ��������    ������ COPY  (CPKYUSEIRRK.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01/07/23  �祭�������ѹ��� �ɲ�
      ************************************************************
      *01  KYUSEIRRK-REC.
      *��������
           03  KYUSEI-KEY.
      *���ŵ��أɣ�
               05  KYUSEI-HOSPNUM           PIC 9(2).
      *���ԣɣ�
               05  KYUSEI-PTID             PIC  9(10).
      *�ѹ�ǯ����
               05  KYUSEI-CHGYMD           PIC  X(08).
      *���ʻ�̾
           03  KYUSEI-KANANAME             PIC  X(100).
      *������̾
           03  KYUSEI-NAME                 PIC  X(100).
      *�̾�
           03  KYUSEI-NICKNAME             PIC  X(100).
      *ü���ɣ�
           03  KYUSEI-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  KYUSEI-OPID                 PIC  X(16).
      *����ǯ����
           03  KYUSEI-CREYMD               PIC  X(08).
      *����ǯ����
           03  KYUSEI-UPYMD                PIC  X(08).
      *��������
           03  KYUSEI-UPHMS                PIC  X(06).
      *�����祭�������ѹ���
           03  KYUSEIUP-KEY.
               05  KYUSEIUP-HOSPNUM         PIC 9(2).
               05  KYUSEIUP-PTID           PIC 9(10).
               05  KYUSEIUP-CHGYMD         PIC X(08).
