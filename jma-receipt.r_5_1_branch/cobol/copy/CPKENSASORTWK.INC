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
      ************************************************************
      ************************************************************
      *    ����ʬ�ࡡ        �� COPY (CPKENSASORTWK.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  KENSASORTWK-REC.
      *������ʬ
           03  KENSASORTWK-SYORIKBN        PIC  X(01).
      *����
           03  KENSASORTWK-KEY.
      *����ʬ�ॳ����
               05  KENSASORTWK-KNSBUNRUI   PIC  9(02).
      *���ť�����
               05  KENSASORTWK-SRYCD       PIC  X(09).
      *ɽ��Ϣ��
           03  KENSASORTWK-DSPSEQ          PIC  9(04).
      *ü���ɣ�
           03  KENSASORTWK-TERMID          PIC  X(32).
      *���ڥ졼���ɣ�
           03  KENSASORTWK-OPID            PIC  X(16).
      *����ǯ����
           03  KENSASORTWK-CREYMD          PIC  X(08).
      *����ǯ����
           03  KENSASORTWK-UPYMD           PIC  X(08).
      *��������
           03  KENSASORTWK-UPHMS           PIC  X(06).