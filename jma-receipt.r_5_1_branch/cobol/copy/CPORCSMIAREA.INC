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
      *    ���޾�������Խ��ѥ�᥿����   COPY (CPORCSMIAREA.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSMIAREA.
      *���ť�����
           03  ORCSMI-SRYCD        PIC X(09).
      *������ʬ��1:���Ρ�2:�Уԣ�������3:����¾��
           03  ORCSMI-IMAGEKBN     PIC 9(01).
      *�����ե��������ΰ�Υ�����
           03  ORCSMI-TEMPDIR      PIC X(1024).
      *�����ե�����̾
           03  ORCSMI-FILENAME     PIC X(128).
      *��̥����ɡ�0:������0�ʳ�:���顼��
           03  ORCSMI-RETCD        PIC 9(01).
      *