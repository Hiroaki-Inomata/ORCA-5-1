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
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *�ݸ���ʬ
               05  MF100-KENTAN          PIC  9(01).
      *��Į¼�ֹ�
               05  MF100-CITYNUM         PIC  X(03).
      *���ճ��
               05  MF100-KYURATE1        PIC  9(01).
               05  MF100-KYURATE2        PIC  9(03).
      *���ᥨ�ꥢ
           03  MF100-SEIKYU-AREA.
      *������ʬ
               05  MF100-NYUGAIKBN OCCURS 2.
                   07  MF100-KENSU         PIC  9(04).
                   07  MF100-NISSU         PIC  9(04).
                   07  MF100-TENSU         PIC  9(07).
                   07  MF100-TAHO          PIC  9(07).
