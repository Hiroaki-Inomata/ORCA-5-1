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
      *  ���޾���       (CPYAKUJYO.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  YAKUJYO-REC.
           03  YAKUJYO-KEY.
      *���ŵ��أɣ�
               05  YAKUJYO-HOSPNUM          PIC 9(2).
      *�����ʥ�����
               05  YAKUJYO-SRYCD           PIC X(09).
      *Ŭ���ֹ�
               05  YAKUJYO-TEKNUM          PIC 9(02).
      *������̾��
           03  YAKUJYO-NAME                PIC X(170).
      *��ǽ����
           03  YAKUJYO-EFFECT              PIC X(2000).
      *���ջ���
           03  YAKUJYO-CAUTION             PIC X(4000).
      *�����ե�����̾
           03  YAKUJYO-PHOTO               PIC X(256).
      *��
           03  YAKUJYO-COLOR               PIC X(360).
      *��
           03  YAKUJYO-SHAPE               PIC X(360).
      *����
           03  YAKUJYO-MARK                PIC X(360).
      *ü���ɣ�
           03  YAKUJYO-TERMID              PIC  X(32).
      *���ڡ��졼���ɣ�
           03  YAKUJYO-OPID                PIC  X(16).
      *����ǯ����
           03  YAKUJYO-CREYMD              PIC  X(08).
      *����ǯ����
           03  YAKUJYO-UPYMD               PIC  X(08).
      *��������
           03  YAKUJYO-UPHMS               PIC  X(06).