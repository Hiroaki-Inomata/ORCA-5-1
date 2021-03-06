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
      *    ��������          COPY (CPSANTEI.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SANTEI-REC.
           03  SANTEI-KEY.
      *���ŵ��أɣ�
               05  SANTEI-HOSPNUM          PIC 9(2).
      *���ԣɣ�
               05  SANTEI-PTID            PIC  9(10).
      *����ǯ��
               05  SANTEI-SRYYM           PIC  X(06).
      *���ť�����
               05  SANTEI-SRYCD           PIC  X(09).
      *������ʬ
               05  SANTEI-NYUGAIKBN       PIC  X(01).
      *���Ų�
               05  SANTEI-SRYKA           PIC  X(02).
      *�ݸ��ȹ��ֹ�
               05  SANTEI-HKNCOMBINUM     PIC  9(04).
      *�������
           03  SANTEI-FSANTEIYMD       PIC  X(08).
      *����������
           03  SANTEI-MSANTEID         PIC  X(02).
      *�������
           03  SANTEI-MSANTEICNT       PIC  9(03).
      *���Ų���ơ��֥�ʣ������飳������
           03  SANTEI-DAY-AREA.
               05  SANTEI-DAY          PIC  9(03)
                                       OCCURS   31.
      *����������оݥե饰
           03  SANTEI-MSANTEIFLG       PIC  9(01).
      *���������
           03  SANTEI-MSANTEITEN       PIC  9(08).
      *�����������ʬ
           03  SANTEI-MSANTEITENKBN    PIC  9(01).
      *ü���ɣ�
           03  SANTEI-TERMID           PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SANTEI-OPID             PIC  X(16).
      *����ǯ����
           03  SANTEI-CREYMD           PIC  X(08).
      *����ǯ����
           03  SANTEI-UPYMD            PIC  X(08).
      *��������
           03  SANTEI-UPHMS            PIC  X(06).
      *
