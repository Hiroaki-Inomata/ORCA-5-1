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
      *    �����ť����ɥơ��֥�        COPY (CPHKTSRYCD.INC)
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.08.00     NACL-��ƣ    16/03/13  Ϣ�֣��墪����
      ************************************************************
      *01  HKTSRYCD-REC.
           03  HKTSRYCD-KEY.
      *�������ֹ�
               05  HKTSRYCD-KANRICD         PIC  9(03).
      *�������ֹ�Ϣ��
               05  HKTSRYCD-RENNUM          PIC  9(03).
      *ͭ��������
               05  HKTSRYCD-YUKOSTYMD       PIC  X(08).
      *ͭ����λ��
               05  HKTSRYCD-YUKOEDYMD       PIC  X(08).
      *����оݿ��Ź԰٥�����
           03  HKTSRYCD-SRYCD               PIC  X(09).
      *ü���ɣ�
           03  HKTSRYCD-TERMID              PIC  X(16).
      *���ڥ졼���ɣ�
           03  HKTSRYCD-OPID                PIC  X(16).
      *����ǯ����
           03  HKTSRYCD-CREYMD              PIC  X(08).
      *����ǯ����
           03  HKTSRYCD-UPYMD               PIC  X(08).
      *��������
           03  HKTSRYCD-UPHMS               PIC  X(06).
