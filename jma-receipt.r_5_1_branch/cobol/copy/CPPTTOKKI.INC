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
      *  �����õ�����ơ��֥�       (CPPTTOKKI.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  PTTOKKI-REC.
           03  PTTOKKI-KEY.
      *���ŵ��أɣ�
               05  PTTOKKI-HOSPNUM     PIC 9(2).
      *������ʬ
               05  PTTOKKI-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
               05  PTTOKKI-PTID        PIC 9(10).
      *Ϣ�ֹ�
               05  PTTOKKI-RENNUM      PIC 9(02).
      *�õ����ॳ����
           03  PTTOKKI-CD              PIC X(02).
      *�õ���������
           03  PTTOKKI-NAIYO           PIC X(12).
      *����ǯ��
           03  PTTOKKI-STYM            PIC X(06).
      *��λǯ��
           03  PTTOKKI-EDYM            PIC X(06).
      *ü���ɣ�
           03  PTTOKKI-TERMID          PIC X(16).
      *���ڡ��졼���ɣ�
           03  PTTOKKI-OPID            PIC X(16).
      *����ǯ����
           03  PTTOKKI-CREYMD          PIC X(08).
      *����ǯ����
           03  PTTOKKI-UPYMD           PIC X(08).
      *��������
           03  PTTOKKI-UPHMS           PIC X(06).