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
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         12/07/31  ���ض�ʬ���ɲ�
      *  01.00.02    ����         18/10/10  �������򥳥��Ⱥ��
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
               05  MF100-CITYNUM         PIC  X(03).
               05  MF100-SYUGAKU         PIC  X(01).
      *
           03  MF100-SEIKYU-TBL.
               05  MF100-KENSU           PIC  9(06).
               05  MF100-TENSU           PIC  9(09).
               05  MF100-SEIKYUGK        PIC  9(07).
               05  MF100-FTNGK           PIC  9(08).
