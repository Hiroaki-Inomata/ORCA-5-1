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
      *----------------------------------------------------------*
      *    ���ϥѥ�᥿              COPY (CPORCHCM31.INC)
      *                                   (ORCHCM31��ORCHCM32�ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHCM31AREA.
      *
      *    ���Ͽ�����
           03  ORCHCM31-STRSRYYMD       PIC  X(08).
      *    ��λ������
           03  ORCHCM31-ENDSRYYMD       PIC  X(08).
      *    ���ֻ����ʬ
           03  ORCHCM31-KIKANKBN        PIC X(01).
      *    ���ֽ�λ�����λ�ʧ����ʬ
           03  ORCHCM31-SUMKBN          PIC X(01).
      *    ���ֽ�λ����λ�ʧ����ʬ
           03  ORCHCM31-SYORIKBN        PIC X(01).
      *    �������Ƥΰ�����ʬ
           03  ORCHCM31-DSPKBN          PIC X(01).
      *    �����ֹ�
           03  ORCHCM31-PTID            PIC  9(10).
      *    ����
           03  ORCHCM31-NYUGAI          PIC  X(01).
      *
           03  ORCHCM31-OT-AREA.
      *        ���Ϸ��
               05  ORCHCM31-OT-CNT      PIC  9(05).
      *
      *        �꥿���󥳡���
               05  ORCHCM31-RC          PIC S9(03).
      *