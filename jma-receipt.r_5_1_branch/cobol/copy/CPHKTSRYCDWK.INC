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
      *    �����ť����ɥơ��֥�        COPY (CPHKTSRYCDWK.INC)
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.08.00     NACL-��ƣ    16/03/13  Ϣ�֣��墪����
      ************************************************************
      *01  HKTSRYCDWK-REC.
           03  HKTSRYCDWK-SYORIKBN            PIC  X(01).
           03  HKTSRYCDWK-KEY.
      *�������ֹ�
               05  HKTSRYCDWK-KANRICD         PIC  9(03).
      *�������ֹ�Ϣ��
               05  HKTSRYCDWK-RENNUM          PIC  9(03).
      *ͭ��������
               05  HKTSRYCDWK-YUKOSTYMD       PIC  X(08).
      *ͭ����λ��
               05  HKTSRYCDWK-YUKOEDYMD       PIC  X(08).
      *����оݿ��Ź԰٥�����
           03  HKTSRYCDWK-SRYCD               PIC  X(09).
      *ü���ɣ�
           03  HKTSRYCDWK-TERMID              PIC  X(16).
      *���ڥ졼���ɣ�
           03  HKTSRYCDWK-OPID                PIC  X(16).
      *����ǯ����
           03  HKTSRYCDWK-CREYMD              PIC  X(08).
      *����ǯ����
           03  HKTSRYCDWK-UPYMD               PIC  X(08).
      *��������
           03  HKTSRYCDWK-UPHMS               PIC  X(06).
