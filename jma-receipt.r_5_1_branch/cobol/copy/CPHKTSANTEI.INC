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
      *    �������å��ޥ���              COPY (CPHKTSANTEI.INC)
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HKTSANTEI-REC.
           03  HKTSANTEI-KEY.
      *�������ֹ�
               05  HKTSANTEI-KANRICD         PIC  9(03).
      *ͭ��������
               05  HKTSANTEI-YUKOSTYMD       PIC  X(08).
      *ͭ����λ��
               05  HKTSANTEI-YUKOEDYMD       PIC  X(08).
      *����оݿ��Ź԰٥�����
               05  HKTSANTEI-SRYCD           PIC  X(09).
      *��绻���ʬ
           03  HKTSANTEI-SANTEIKBN           PIC  X(01).
      *����оݸ�����Ǣ��
           03  HKTSANTEI-KENTAIKBN           PIC  X(01).
      *ü���ɣ�
           03  HKTSANTEI-TERMID              PIC  X(16).
      *���ڥ졼���ɣ�
           03  HKTSANTEI-OPID                PIC  X(16).
      *����ǯ����
           03  HKTSANTEI-CREYMD              PIC  X(08).
      *����ǯ����
           03  HKTSANTEI-UPYMD               PIC  X(08).
      *��������
           03  HKTSANTEI-UPHMS               PIC  X(06).
