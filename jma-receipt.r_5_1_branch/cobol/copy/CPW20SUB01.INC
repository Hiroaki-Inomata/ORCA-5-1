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
      *    ORCGW20���֥ץ������������������     COPY (CPW20SUB01.INC)       *
      ************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  W20SUB01-LNK.
           03  W20SUB01-IN.
      *
      *        ������ʬ
               05  W20SUB01-I-KBN              PIC X(01).
      *        �����
               05  W20SUB01-I-KJNYMD           PIC X(08).
      *
      *        ����������
               05  W20SUB01-I-KEY1.
      *
      *            ���ܿ��Ź԰٥�����
                   07  W20SUB01-I-KHNSRYCD     PIC X(09).
      *
           03  W20SUB01-OT.
      *
      *        �꥿���󥳡��ɡʣ���������������ʤ�����
               05  W20SUB01-O-RC               PIC 9(01).
      *        ���Ź԰٥�����
               05  W20SUB01-O-KHNSRYCD         PIC  X(09).
      *        ���Ź԰�̾��
               05  W20SUB01-O-NAME             PIC X(200).
      *        ��������
               05  W20SUB01-O-KHNTEN           PIC  9(06).
      *        ���ߴ���ʬ